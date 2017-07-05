-- Acorn Electron ULA
-- Ferranti 12C021 Custom
--
-- Copyright Gary Preston 2017
-- All Rights Reserved
--
-- Temporary implementation until reverse engineering of the 12C021 is complete
--
-- Note: This implementation is a compromise between matching the external interface
-- of the real ULA and providing the replay framework with the data it needs to
-- function. A fully pin accurate mapping would likely entail so much extra logic
-- outside the ULA to rebuild data the ULA could just provide. For example
-- outputting one bit RGB vs the full value the framework takes. Likewise for
-- trying to derive the active video signal from just csync and hsync.
--
-- Ram access didn't need that much more logic to remain pin accurate however.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity ULA_12C021 is
  port (
    --
    -- Additional framework signals to ease usage
    --
    o_n_vsync     : out bit1;                  -- 
    o_de          : out bit1;                  --

    o_rgb         : out word(23 downto 0);

    --
    -- ULA
    -- 
    
    -- Cassette I/O (not yet supported)
    i_cas         : in bit1; 
    o_cas         : out bit1;      
    b_cas_rc      : inout bit1;                -- RC high tone detection
    o_cas_mo      : out bit1;                  -- Motor relay
           
    -- Audio
    o_sound_op    : out bit1;            
       
    -- Reset             
    i_n_por       : in bit1;                   -- /Power on reset

    -- Video
    o_n_csync     : out bit1;                  -- h/v sync  (low during horizontal or vertical synchronisation)
    o_n_hsync     : out bit1;                  -- h sync    
    --o_red         : out bit1;            
    --o_green       : out bit1;            
    --o_blue        : out bit1; 
       
    -- Clock             
    i_clk         : in bit1;                    
    i_div_13      : in bit1;                   -- clk div 13
       
    -- RAM (4x64k 1 bit)       
    b_ram0        : inout bit1;                -- RAM Data ic 0
    b_ram1        : inout bit1;                -- RAM Data ic 1
    b_ram2        : inout bit1;                -- RAM Data ic 2
    b_ram3        : inout bit1;                -- RAM Data ic 3
       
    o_n_we        : out bit1;                  -- /write, read
    o_n_ras       : out bit1;                  -- row address strobe  -ve edge
    o_n_cas       : out bit1;                  -- col address strobe  -ve edge

    o_ra          : out word( 7 downto 0 );    -- ram address

    -- Keyboard
    i_kbd         : in word( 3 downto 0 ); 
    o_caps_lock   : out bit1;
    i_n_reset     : in bit1;

    -- ROM/CPU addressing
    o_rom         : out bit1;                  -- rom select enable   
    i_addr        : in word( 15 downto 0 );
    b_pd          : inout word( 7 downto 0 );  -- CPU/ROM data

    -- CPU
    i_n_nmi       : inout bit1;                -- 1MHz RAM access detection
    o_phi_out     : out bit1;                  -- CPU clk, 2MHz, 1MHz or stopped
    o_n_irq       : out bit1;
    i_n_w         : in bit1                    -- Data direction, /write, read
  );
end;

architecture RTL of ULA_12C021 is
  -- Debug
  signal cur_nibble_signal : word(3 downto 0);

  -- Framework Video
  signal ana_hsync, ana_vsync, ana_de : bit1;
  signal dig_hsync, dig_vsync, dig_de : bit1;

  signal vpix, hpix, vtotal : word(13 downto 0);
  
  signal display_period : boolean;

  signal rst : bit1;
  signal nmi : bit1;

  signal ula_ram_data : word(7 downto 0);
  signal ula_ram_addr : word(14 downto 0);

  signal cpu_ram_data : word(7 downto 0);

  -- Timing
  signal clk_2MHz, clk_1MHz  : bit1;
  signal clk_phase : unsigned(3 downto 0);

  signal rtc_count : unsigned(18 downto 0);

  -- 
  -- Registers (AUG p206)
  --
  -- Interrupt status and control (AUG p135)
  -- TX/RX swapped based on:-
  -- https://web.archive.org/web/20060206155259/http://electrem.emuunlim.com/techinfo.htm
  constant ISR_MASTER_IRQ     : integer := 0;
  constant ISR_POWER_ON_RESET : integer := 1;
  constant ISR_FRAME_END      : integer := 2;
  constant ISR_RTC            : integer := 3;
  constant ISR_RX_FULL        : integer := 4;
  constant ISR_TX_EMPTY       : integer := 5;
  constant ISR_HIGH_TONE      : integer := 6;
  signal isr_en               : word(6 downto 2);
  signal isr_status           : word(6 downto 0);
  
  signal screen_start_addr    : word(14 downto 6);
  signal cassette_data_shift  : word(7 downto 0);
  
  -- Interrupt clear & ROM Paging
  subtype ISRC_ROM_PAGE is integer range 2 downto 0;
  constant ISRC_ROM_PAGE_ENABLE  : integer := 3;
  constant ISRC_FRAME_END        : integer := 4;       
  constant ISRC_RTC              : integer := 5; 
  constant ISRC_HIGH_TONE        : integer := 6;       
  constant ISRC_NMI              : integer := 7;
  signal isrc_paging             : word(3 downto 0);

  -- Multipurpose Counter
  signal multi_counter           : word(7 downto 0);

  -- Misc control
  subtype MISC_COMM_MODE is integer range 2 downto 1;
  subtype MISC_DISPLAY_MODE is integer range 5 downto 3;
  constant MISC_CASSETTE_MOTOR   : integer := 6;
  constant MISC_CAPS_LOCK        : integer := 7;
  signal misc_control            : word(7 downto 1);

  -- Colour palettes
  subtype t_colour_palette is word( 7 downto 0);
  type t_colour_palettes is array(15 downto 8) of t_colour_palette;
  signal colour_palettes : t_colour_palettes;
   
begin

  -- Power up, perform reset
  -- TODO: Should ULA drive i_n_reset too to cause cpu reset? Is this
  --       pin tri-state?
  rst <= not i_n_por;
  
  -- Internal weak pull-up
  i_n_nmi <= 'H';
  
  -- ====================================================================
  -- Master Timing
  -- ====================================================================
  -- 16MHz clock down to 2MHz & 1MHz generator
  p_clk_gen : process(i_clk, rst)
  begin
    if (rst = '1') then
      clk_phase <= (others => '0');
      clk_1MHz <= '0';
      clk_2MHz <= '0';
    elsif rising_edge(i_clk) then
      clk_1MHz <= '0';
      clk_2MHz <= '0';
      
      if (clk_phase = "1111") then
        clk_2MHz <= '1';
        clk_1MHz <= '1';
      elsif (clk_phase = "0111") then
        clk_2MHz <= '1';
      end if;
      clk_phase <= clk_phase + 1;
    end if;
  end process;

  -- ====================================================================
  -- Video
  -- ====================================================================
  -- Modes:
  --   0 - 640x256 two colour gfx, 80x32 text (20K)
  --   1 - 320x256 four colour gfx, 40x32 text (20K)
  --   2 - 160x256 sixteen colour gfx, 20x32 text (20K)
  --   3 - 80x25 two colour text gfx
  --   4 - 320x256 two colour gfx, 40x32 text (10K)
  --   5 - 160x256 four colour gfx, 20x32 text (10K)
  --   6 - 40x25 two colour text (8K)
  
  -- TODO: Did Electron generate offset/interlaced display or use matching fields for 256p?
  u_VideoTiming : entity work.Replay_VideoTiming
    generic map (
      g_enabledynamic       => '0',
      g_param               => c_Vidparam_832x287p_50_16MHz
      )
    port map (
      i_clk                 => i_clk,
      i_ena                 => '1',
      i_rst                 => rst,
      --
      i_param               => c_Vidparam_832x287p_50_16MHz,
      i_sof                 => '0',
      i_f2_flip             => '0',
      --
      o_hactive             => open,
      o_hrep                => open,
      o_vactive             => vtotal,
      --
      o_dig_hs              => dig_hsync,
      o_dig_vs              => dig_vsync,
      o_dig_de              => dig_de,
      o_dig_ha              => open,
      o_dig_va              => open,
      o_dig_sof             => open,
      o_dig_sol             => open,
      o_ana_hs              => ana_hsync,
      o_ana_vs              => ana_vsync,
      o_ana_de              => ana_de,
      --
      o_hpix                => hpix,
      o_vpix                => vpix,
      --
      o_f2                  => open,
      o_voddline            => open,
      o_stdprog             => open
      );

  -- TODO: [Gary] Mixing of dig/ana here :( Analog in PAL 576i returns csync as
  -- hsync and '1' for vsync. However, OSD in Syscon uses vsync to determine display
  -- location so digital h/v passed out for now. This is a cludge as the
  -- timing of dig h/v may not match that of analog h/v (or combined csync). This will
  -- be fixed once ULA switched to its own analog timing, or VideoTiming adjusted to
  -- expose analog h/v.
  o_n_hsync <= dig_hsync;
  o_n_vsync <= dig_vsync;
  o_n_csync <= ana_hsync;
  o_de      <= ana_de;

  u_vid_rgb : process(i_clk)
    variable cur_pix, next_pix : word(7 downto 0);
    variable double : boolean := false;
    variable count : integer range 0 to 8;
    -- TODO: [Gary] switch ula_read_addr to 15 bits? even though cur_addr ignores top bit
    variable read_addr : word(15 downto 0);
    variable hardcoded_screen_start : word(15 downto 0);
  begin
    if rising_edge(i_clk) then
      o_rgb <= x"000000";

      display_period <= false;
      
      -- TODO: Mode changes can occur mid scanline. Treat mode 7 as mode 4.
      -- TODO: How to handle different modes?
            
      -- 832 active "pixels" in the 51.95us display area. The 640
      -- display should use the central 40us giving a cycle timing of 62.5ns.
      -- Needs a 96 border both sides for centering or a start cycle of 288.
      -- ULA doc suggests off-center with start on 256 boundary, border of 64 and 128.
      --
      
      -- TODO: [Gary] how to sync the setup and read in time for next byte of data?
      if (vpix = 0) then   
        -- mdfs.net notes that if addr 0 is loaded, it will be replaced by a
        -- hardcoded per mode base address. Also used if address overflows back to 0.
        -- 3000 for 0,1,2; 4000 for 3; 5800 for 4,5; 6000 for 6.
        read_addr := (others => '0');

        if screen_start_addr = x"00" & '0' then
          case misc_control(MISC_DISPLAY_MODE) is
            when "000" | "001" | "010" => hardcoded_screen_start := x"3000";
            when "011" => hardcoded_screen_start := x"4000";
            when "100" | "101" | "111" => hardcoded_screen_start := x"5800";
            when "110" => hardcoded_screen_start := x"6000";
            when others =>
          end case;
          read_addr := hardcoded_screen_start;
        else
          hardcoded_screen_start := '0' & screen_start_addr & "000000";
          read_addr(14 downto 6) := screen_start_addr;
        end if;
      end if;

      if (clk_phase(3) = '0') then
        ula_ram_addr <= read_addr(14 downto 0);
      elsif (clk_phase = "1111") then
        next_pix := ula_ram_data;
      end if;

      if (vpix < 16 or vpix >= 16+256) then
        -- overscan
        o_rgb <= x"FF0000";
      elsif (vpix >= 16 and vpix < 16+256) then

        -- TODO: [Gary] Need mode 6 going first for bootup.
        if (hpix < 64 or hpix >= 704) then
          o_rgb <= x"FF0000";
          cur_pix := next_pix;
          count := 0;
          double := false;
        else
          display_period <= true;

          -- Active Region 640x256
          -- For 320 and 160 modes, repeat pixels.        
          o_rgb <= x"0000FF";
          if (cur_pix(7-count) = '1') then
            o_rgb <= x"FFFFFF";
          end if;

          if (double) then
            count := count + 1;
          end if;
          double := not double;
          
          if (count = 8) then
            count := 0;
            cur_pix := next_pix;
            -- next byte
            read_addr := read_addr + '1';
            if (read_addr(15) = '1') then
              -- wrap around. Not sure where this should start again though?
              read_addr := (others => '0');
              read_addr := hardcoded_screen_start;
            end if;
          end if;

          -- TODO: Use data currently on ram bus? RAM access handled
          -- in another process that skips trying to setup addr if nmi active.
          -- whatever happens to be on the ram data bus will be output as video data.

          -- Debug: using rgb out for a few quick sanity checks
          --o_rgb <= x"0000FF";
          if (isr_status(ISR_POWER_ON_RESET) = '0') and
             (misc_control(MISC_DISPLAY_MODE) = "110") and
             (screen_start_addr = (x"0" & '0')) then
            o_rgb <= x"00FF00";
          end if;
          
        end if;        
      end if;

    end if;
  end process;


  -- ====================================================================
  -- RAM
  -- ====================================================================
  -- Memory Layout (AUG p183-200)
  -- 0000-7FFF RAM    - Shared between system/user and video
  -- 8000-BFFF ROM    - Paged (initially basic)
  -- C000-FBFF ROM    - OS
  -- FC00-FCFF Fred   - Memory Mapped I/O (Expansions)
  -- FD00-FDFF Jim    - Memory Mapped I/O (??)
  -- FE00-FEFF Sheila - Memory Mapped I/O (ULA)
  -- FF00-FFFF ROM    - OS

 

  --
  -- Ram Interface & Timing
  --
  -- 4164 ram is async, however this implementation uses synchronous ram. 
  -- For a ULA replacement, exact timing requirements of 4164 would need to be
  -- checked and implemented. This is a pseudo ras/cas implementation only.
  --
  -- RAM access occurs at 16MHz, however it takes 4 cycles to perform a 4bit
  -- read, 8 cycles to get a full byte. This is effectively 1 byte per 1MHz clk.
  -- The ULA time shares ram access (1MHz period each) with the CPU except:
  --  1. During the display_period of modes 0..3 where ULA needs 2MHz access and cpu stopped
  --  2. When nmi is signalled during which time the ULA suspends its ram access.
  -- CPU Gets first 8 cycles, ULA second 8.

  
  -- TODO: [Gary] with dram usage, 2 ula_clk cycles need to be allowed between
  -- setting read addr and data available, rather than the 1 that a sys_clk/4
  -- enable gate would have allowed.
  p_ram_access : process(i_clk, rst)
    variable ram_even_tmp  : word(3 downto 0);
  begin
    cur_nibble_signal <= ram_even_tmp;

    if (rst = '1') then
      o_n_we <= '1';
      o_n_cas <= '1';
      o_n_ras <= '1';
      ula_ram_data <= (others => '0');
      b_pd <= (others => 'Z');
    elsif rising_edge(i_clk) then

      -- ULA reads ram during its time slot
      --if (clk_phase >= 1000 and clk_phase <= "1111") then
          --b_ram0 <= 'Z'; b_ram1 <= 'Z'; b_ram2 <= 'Z'; b_ram3 <= 'Z';
      --end if;

      -- Cpu accessing ram during its slot, or, ula slot.
      if (i_addr(15) = '0') then
        if (i_n_w = '1') then 
          b_pd <= cpu_ram_data;
          b_ram0 <= 'Z'; b_ram1 <= 'Z'; b_ram2 <= 'Z'; b_ram3 <= 'Z';
        else
          b_pd <= (others => 'Z');
        end if;

        -- TODO: [Gary] When nmi is active should cpu get both slots for 2MHz ram access?
        --              or does it still access at 1MHz but overrides the usual ULA block 
        --              for active display during modes 0..3?
        -- TODO: [Gary] Is the 1 to 2 MHz clock transition in sync with this? as cpu needs to remain on the 0
        --       cycle access after transitioning back, not end up expecting to use 8+

        -- TODO: [Gary] Adjust this to use READ/WRITE states rather than 16 cycle based states    

        -- CPU Address and data available on falling edge of cycle 0?
        -- Read/write of byte split into two 4 cycle stages handling 4 bits each.       
        case clk_phase is
          -- CPU Slot
          when "0000" =>
            -- row latch
            o_ra <= i_addr(14 downto 7);
            o_n_ras <= '0';

            o_n_cas <= '1';
            o_n_we <= '1';
          when "0001" =>
            -- unused row delay

          when "0010" =>
            -- col latch
            o_ra <= i_addr(6 downto 0) & '0';
            o_n_cas <= '0';
            o_n_we <= i_n_w;
            if (i_n_w = '0') then
              b_ram0 <= b_pd(0);
              b_ram1 <= b_pd(2);
              b_ram2 <= b_pd(4);
              b_ram3 <= b_pd(6);
            end if;
          when "0011" =>
            o_n_we <= '1';
            if (i_n_w = '1') then
              ram_even_tmp(0) := b_ram0;              
              ram_even_tmp(1) := b_ram1;
              ram_even_tmp(2) := b_ram2;
              ram_even_tmp(3) := b_ram3;
            end if;
          when "0100" =>
            o_n_cas <= '1';          
          when "0101" =>
            -- second nibble cycle
            o_ra <= i_addr(6 downto 0) & '1';
            o_n_cas <= '0';
            o_n_we <= i_n_w;
            if (i_n_w = '0') then
              b_ram0 <= b_pd(1);
              b_ram1 <= b_pd(3);
              b_ram2 <= b_pd(5);
              b_ram3 <= b_pd(7);
            end if;
          when "0110" =>
            o_n_we <= '1';
            if (i_n_w = '1') then
              cpu_ram_data(0) <= ram_even_tmp(0);
              cpu_ram_data(1) <= b_ram0;
              cpu_ram_data(2) <= ram_even_tmp(1);
              cpu_ram_data(3) <= b_ram1;
              cpu_ram_data(4) <= ram_even_tmp(2);
              cpu_ram_data(5) <= b_ram2;
              cpu_ram_data(6) <= ram_even_tmp(3);
              cpu_ram_data(7) <= b_ram3;
            end if;
          when "0111" => 
            -- TODO: [Gary] could this be moved elsewhere for reads? like the main b_pd assign?
            b_pd <= cpu_ram_data; 
            o_n_ras <= '1';
            o_n_cas <= '1';  
            o_n_we <= '1';

          when others =>

        end case;
      end if;

      -- ULA Slot
      if (clk_phase(3) = '1') then
        -- ULA reads internally only.
        b_ram0 <= 'Z'; b_ram1 <= 'Z'; b_ram2 <= 'Z'; b_ram3 <= 'Z';
        -- TODO: [Gary] not needed for ula? Let cpu slot handle that?
        b_pd <= (others => 'Z');

        case clk_phase is
          -- ULA Slot
          when "1000" =>
            -- row latch
            o_ra <= ula_ram_addr(14 downto 7);
            o_n_ras <= '0';

            o_n_cas <= '1';
            o_n_we <= '1';
          when "1001" =>
            -- unused
          when "1010" =>
            -- col latch
            o_ra <= ula_ram_addr(6 downto 0) & '0';
            o_n_cas <= '0';
          when "1011" =>
            ula_ram_data(0) <= b_ram0;
            ula_ram_data(2) <= b_ram1;
            ula_ram_data(4) <= b_ram2;
            ula_ram_data(6) <= b_ram3;
          when "1100" =>
            o_n_cas <= '1';          
          when "1101" =>
            -- second nibble cycle
            o_ra <= ula_ram_addr(6 downto 0) & '1';
            o_n_cas <= '0';
          when "1110" =>
            -- TODO: [Gary] with this separate var ula during nmi wouldn't
            -- display the cpu's ram reading as though it was ula data. No snow!
            -- Need a more accurate representation of what the ULA may have done.            
            ula_ram_data(1) <= b_ram0;
            ula_ram_data(3) <= b_ram1;
            ula_ram_data(5) <= b_ram2;
            ula_ram_data(7) <= b_ram3;
          when "1111" => 
            o_n_ras <= '1';
            o_n_cas <= '1';  
            o_n_we <= '1';
          when others =>
        end case;

      end if;

    end if;
  end process;

  -- ====================================================================
  -- ROM
  -- ====================================================================
  -- Enable main board rom for OS access or BASIC rom if page enable
  -- TODO: [Gary] reading any register other than 0 or 4 should read from os/basic rom.
  o_rom <= '1' when (i_addr >= x"8000" and i_addr <= x"BFFF" and        -- ROM page 10 or 11
                     isrc_paging(ISRC_ROM_PAGE_ENABLE) = '1' and
                     isrc_paging(ISRC_ROM_PAGE'left downto ISRC_ROM_PAGE'right+1) = "01" ) else
           '1' when (i_addr >= x"C000" and i_addr <= x"FBFF") else      -- ROM OS
           '1' when (i_addr >= x"FF00" and i_addr <= x"FFFF") else      -- ROM OS
           '0';

  -- CPU Variable clocking
  -- TODO: [Gary] AN015 p5 notes 2->1MHz transition is based on phase of 2MHz clock, handle this.
  --              Without this ram access timing slot may end up conflicting with the ULAs slot?
  -- TODO: [Gary] Its suggested that the RAM access can be at 2MHz outside of the display period
  --              in mode 0..3?? Investigate.
  --o_phi_out <= clk_1MHz when i_addr(15 downto 7) = "111110" else  -- ROM Fred/Jim
  --             clk_2MHz when i_addr(15) = '1' else                -- Any other ROM access
  --             clk_1MHz when nmi = '1' else                       -- TODO: [Gary] Double check this
  --             clk_1MHz when misc_control(MISC_DISPLAY_MODE) >= "100" else -- RAM access mode 4,5,6
  --             '0' when misc_control(MISC_DISPLAY_MODE) <= "011" and display_period  else -- RAM access mode 0,1,2,3
  --             clk_1MHz;                                          -- RAM access, mode 0,1,2,3 outside active display
  o_phi_out <= clk_1MHz;

  --
  --  Memory Mapped Registers (AUG p206)
  --
  -- FEX0 - Interrupt status and control register
  -- FEX2 - Video display start address (low byte)
  -- FEX3 - Video display start address (high byte)
  -- FEX4 - Cassette data register
  -- FEX5 - Paged ROM control and interrupt control
  -- FEX6 - Counter plus cassette control
  -- FEX7 - Controls screen, sound, cassette and CAPS LED
  -- FEX8-XF - Palette registers
  -- 
  -- Addressed via page 0xFExx. 16 byte aliasing, ie 0xFE00 and 0xFE10 both refer to register 0.
  
  -- Flag master irq for enabled and active interrupts only.
  isr_status(ISR_MASTER_IRQ) <= (isr_status(ISR_FRAME_END) and isr_en(ISR_FRAME_END)) or
                                (isr_status(ISR_RTC) and isr_en(ISR_RTC)) or
                                (isr_status(ISR_TX_EMPTY) and isr_en(ISR_TX_EMPTY)) or 
                                (isr_status(ISR_RX_FULL) and isr_en(ISR_RX_FULL)) or
                                (isr_status(ISR_HIGH_TONE) and isr_en(ISR_HIGH_TONE));
  o_n_irq <= not isr_status(ISR_MASTER_IRQ);

  -- Register data out
  -- TODO: [Gary] Is it just 0 and 4 that are readable?
  b_pd <= (others => 'Z')          when i_n_w = '0' or i_addr(15 downto 8) /= x"FE" else
          '0' & isr_status                        when i_addr( 3 downto 0) = x"0" else
          cassette_data_shift                     when i_addr( 3 downto 0) = x"4" else
          (others => 'Z');

  p_registers : process(i_clk, rst)
    -- delay POR reset until next CPU clock
    variable delayed_por_reset : bit1 := '0';
  begin
    if (rst = '1') then
      isr_en <= (others => '0');
      isr_status(6 downto 1) <= (ISR_POWER_ON_RESET => '1', others => '0');
      isrc_paging(ISRC_ROM_PAGE) <= "000";
      isrc_paging(ISRC_ROM_PAGE_ENABLE) <= '0';
      screen_start_addr <= (others => '0');
      multi_counter <= (others => '0');
      misc_control <= (others => '0');
      colour_palettes <= (others => (others => '0'));
      rtc_count <= (others => '0');
    elsif rising_edge(i_clk) then

      -- Delayed POR reset pending?
      -- TODO: [Gary] This needs to account for 1 or 2MHz cpu?
      if (delayed_por_reset = '1' and clk_1MHz = '1') then
        delayed_por_reset := '0';
        isr_status(ISR_POWER_ON_RESET) <= '0';
      end if;

      if (i_n_nmi = '0') then
        nmi <= '1';
      end if;
      
      -- Register access
      if (i_addr(15 downto 8) = x"FE") then
      
        if (i_n_w = '1') then

          if (i_addr(3 downto 0) = x"0") then
            -- CPU needs to be able to see the POR flag was active at the start
            -- of the next clock edge when it reads this register. Without the
            -- delay the next ULA clock will clear it long before CPU read occurs.
            -- TODO: [Gary] Could this process be clocked off current CPU clock instead?
            -- TODO: [Gary] Should this reset only occur on the first read?
            delayed_por_reset := '1';
          elsif (i_addr(3 downto 0) = x"4") then
            isr_status(ISR_RX_FULL) <= '0';
          end if;

        else
          case i_addr(3 downto 0) is
            -- Interrupt status and control register
            when x"0" => isr_en <= b_pd(6 downto 2);

            -- do nothing
            when x"1" => 

            -- Video status address low
            when x"2" => screen_start_addr(8 downto 6) <= b_pd(7 downto 5);
            -- Video status address high
            when x"3" => screen_start_addr(14 downto 9) <= b_pd(5 downto 0);
            
            -- Cassette
            when x"4" => -- TODO: [Gary] not yet implemented
              isr_status(ISR_TX_EMPTY) <= '0';

            -- Paged ROM/Interrupt clear
            when x"5" =>
              if (isrc_paging(ISRC_ROM_PAGE_ENABLE) = '1' and isrc_paging(ISRC_ROM_PAGE'LEFT) = '0') then
                -- Only 8-15 allowed when page 8-11 is active (ie kbd/basic rom pages AUG p211)
                if (b_pd(3) = '1') then
                  isrc_paging(ISRC_ROM_PAGE_ENABLE) <= b_pd(3); 
                  isrc_paging(ISRC_ROM_PAGE) <= b_pd(2 downto 0);
                end if;
              else
                isrc_paging(ISRC_ROM_PAGE_ENABLE) <= b_pd(3); 
                isrc_paging(ISRC_ROM_PAGE) <= b_pd(2 downto 0);
              end if;
              
              -- Clear requested interrupts
              nmi                       <= nmi and not b_pd(ISRC_NMI);
              isr_status(ISR_HIGH_TONE) <= isr_status(ISR_HIGH_TONE) and not b_pd(ISRC_HIGH_TONE);
              isr_status(ISR_RTC)       <= isr_status(ISR_RTC) and not b_pd(ISRC_RTC);
              isr_status(ISR_FRAME_END) <= isr_status(ISR_FRAME_END) and not b_pd(ISRC_FRAME_END);

            -- Counter/Cassette control (write only)
            when x"6" => multi_counter <= b_pd;

            -- Controls
            when x"7" => misc_control <= b_pd(7 downto 1);
            
            -- Palette 
            when others => colour_palettes(to_integer(unsigned(i_addr(3 downto 0)))) <= b_pd;            

          end case;
        end if;

      end if;

      -- Interrupt Generation
      -- TODO: [Gary] check -1, may be off by 1 depending on when vtotal inc occurs
      if (vpix = vtotal-1) then
        isr_status(ISR_FRAME_END) <= '1';        
      end if;

      -- 50Hz RTC interrupt every 320000 clocks      
      if (rtc_count = 320000-1) then
        rtc_count <= (others => '0');
        isr_status(ISR_RTC) <= '1';
      else
        rtc_count <= rtc_count + 1;
      end if;
    end if;
  end process;

  -- ====================================================================
  -- Interfacing
  -- ====================================================================
  
  -- 
  -- Keyboard Interface
  --
  
  -- TODO: [Gary] Normal keyboard reading handled via address lines 0..13 
  -- and read when ROM 8 or 9 is paged in.
  -- TODO: [Gary] Also possible to read when paged in via mem mapping (AUG p216)
  -- Keyboard rom active
  b_pd <= x"0" & i_kbd when (i_addr >= x"8000" and i_addr <= x"BFFF" and
                             isrc_paging(ISRC_ROM_PAGE_ENABLE) = '1' and
                             isrc_paging(ISRC_ROM_PAGE'left downto ISRC_ROM_PAGE'right+1) = "00" ) else
                             (others => 'Z');
  o_caps_lock <= misc_control(MISC_CAPS_LOCK);

  -- 
  -- Sound Interface
  --

  --
  -- Cassette Interface
  --
  -- TODO: mdfs.net notes bit take order is opposite to what the AUG states. Investigate.

  -- TODO: [Gary] shift in new bit of data from cassette every ~2ms
  -- read full interrupt every 8 bits
  -- write empty interrupt after 8 bits output
  -- High tone interrupt

end;

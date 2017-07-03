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
  -- Framework Video
  signal ana_hsync, ana_vsync, ana_de : bit1;
  signal dig_hsync, dig_vsync, dig_de : bit1;

  signal vpix, hpix, vtotal : word(13 downto 0);
  
  signal display_period : boolean;

  signal rst : bit1;
  signal rom_en : bit1;
  signal nmi : bit1;

  -- Timing
  signal clk_2MHz, clk_1MHz  : bit1;

  -- 
  -- Registers (AUG p206)
  --
  -- Interrupt status and control (AUG p135)
  constant ISR_MASTER_IRQ     : integer := 0;
  constant ISR_POWER_ON_RESET : integer := 1;
  constant ISR_FRAME_END      : integer := 2;
  constant ISR_RTC            : integer := 3;
  constant ISR_TX_EMPTY       : integer := 4;
  constant ISR_RX_FULL        : integer := 5;
  constant ISR_HIGH_TONE      : integer := 6;
  signal isr_en               : word(6 downto 2);
  signal isr_status           : word(6 downto 0);
  
  signal screen_start_addr    : word(14 downto 6);
  signal cassette_data_shift  : word(7 downto 0);
  
  -- Interrupt clear & ROM Paging
  subtype ISRC_ROM_PAGE is integer range 2 downto 0;
  subtype ISRC_INTERRUPTS is integer range 7 downto 4;
  constant ISRC_ROM_PAGE_ENABLE  : integer := 3;
  constant ISRC_FRAME_END        : integer := 4;       
  constant ISRC_RTC              : integer := 5; 
  constant ISRC_HIGH_TONE        : integer := 6;       
  constant ISRC_NMI              : integer := 7;
  signal isrc_paging             : word(7 downto 0);

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
    variable count : unsigned(3 downto 0) := (others => '0');
  begin
    if (rst = '1') then
      count := (others => '0');
      clk_1MHz <= '0';
      clk_2MHz <= '0';
    elsif rising_edge(i_clk) then
      clk_1MHz <= '0';
      clk_2MHz <= '0';
      
      if (count = "0000") then
        clk_2MHz <= '1';
        clk_1MHz <= '1';
      elsif (count = "1000") then
        clk_2MHz <= '1';
      end if;
      count := count + 1;
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
  begin      
    if rising_edge(i_clk) then
      o_rgb <= x"000000";

      display_period <= false;
      
      -- 832 active "pixels" in the 51.95us display area. The 640
      -- display should use the central 40us giving a cycle timing of 62.5ns.
      -- Needs a 96 border both sides for centering or a start cycle of 288.
      -- ULA doc suggests off-center with start on 256 boundary, border of 64 and 128.
      --
      -- In 320 mode use 2 cycles per pixel? or use middle 320 pixels?
      if (vpix < 16 or vpix >= 16+256) then
        -- overscan
        o_rgb <= x"FFFFFF";
      elsif (vpix >= 16 and vpix < 16+256) then

        -- TODO: [Gary] Need mode 6 going first for bootup.

        if (hpix <= 64) then
          o_rgb <= x"FF0000";
        elsif (hpix >= 832-96) then
          o_rgb <= x"00FF00";
        else
          display_period <= true;

          -- Test, Blue default and white for mode 6
          o_rgb <= x"0000FF";
          if (misc_control(MISC_DISPLAY_MODE) = "110") then
            o_rgb <= x"FFFFFF";
          end if;
          
        end if;        
      end if;


      -- TODO: [Gary] Use screen_addr_start etc

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
  -- 4164 ram is async, however this implementation is synchronous
  -- An actual ULA would need this interface logic rewriting to match timing requirements

  -- Display modes 4,5,6 only need 1MHz ram access
  -- Modes 0..3 need 2MHz?
  -- if nmi = '1', ULA cannot access ram.

  -- ram enable/read/write


  -- rom enable
  -- TODO: Rom page 8 or 9 for keyboard reading
  -- TODO: 8000-BFFF only used when rom paged in and enabled?
  rom_en <= '0' when i_addr(15) = '0' else
            '1' when (i_addr >= x"C000" and i_addr <= x"FBFF") else
            '1' when (i_addr >= x"FF00" and i_addr <= x"FFFF") else
            '1' when (i_addr >= x"8000" and i_addr <= x"BFFF") else
            '0';            
  o_rom <= rom_en;

  -- 2MHz rom, 1MHz RAM and stopped during rendering in mode 0..3
  -- i_n_nmi gives priority to cpu, ULA must block.
  -- REVIEW: should i_n_nmi be held once triggered until cleared by cpu clear register ?
  -- TODO: [Gary] should 2MHz or 1MHz be the default?
  -- TODO: [Gary] AN015 p5 notes 2->1MHz transition is based on phase of 2MHz clock, handle this.
  o_phi_out <= clk_1MHz when nmi = '1' else
               clk_2MHz when rom_en = '1' else 
               '0' when misc_control(MISC_DISPLAY_MODE) <= "011" and display_period  else
               clk_1MHz;
  
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
  o_n_irq <= isr_status(ISR_MASTER_IRQ);

  -- Register data out
  -- TODO: [Gary] Rom page 8 or 9 for keyboard reading
  b_pd <= (others => 'Z')          when i_n_w = '0' or i_addr(15 downto 8) /= x"FE" else
          '0' & isr_status                        when i_addr( 3 downto 0) = x"0" else
          screen_start_addr(8 downto 6) & "00000" when i_addr( 3 downto 0) = x"2" else
          "00" & screen_start_addr(14 downto 9)   when i_addr( 3 downto 0) = x"3" else
          cassette_data_shift                     when i_addr( 3 downto 0) = x"4" else
          isrc_paging                             when i_addr( 3 downto 0) = x"5" else
          misc_control & '0'                      when i_addr( 3 downto 0) = x"7" else
          (others => 'Z');

  -- FEX0
  p_registers : process(i_clk, rst)
  begin
    if (rst = '1') then
      isr_en <= (others => '0');
      isr_status(6 downto 1) <= (ISR_POWER_ON_RESET => '1', others => '0');
      isrc_paging <= (others => '0');
      screen_start_addr <= (others => '0');
      multi_counter <= (others => '0');
      misc_control <= (others => '0');
      colour_palettes <= (others => (others => '0'));
    elsif rising_edge(i_clk) then
      -- TODO: [Gary] Interrupt servicing priority?
      -- TODO: [Gary] Clear ISR_POWER_ON_RESET once read

      if (i_n_nmi = '0') then
        nmi <= '1';
      end if;
      
      -- Register write      
      if (i_addr(15 downto 8) = x"FE" and i_n_w = '0') then
        case i_addr(3 downto 0) is
          -- Interrupt status and control register
          when x"0" => isr_en <= b_pd(6 downto 2);

          -- do nothing
          when x"1" => 

          -- Video status address low
          when x"2" => screen_start_addr(8 downto 6) <= b_pd(7 downto 5);
          -- Video status address low
          when x"3" => screen_start_addr(14 downto 9) <= b_pd(5 downto 0);
          
          -- Cassette
          when x"4" => -- TODO: [Gary] not yet implemented
            -- TODO: [Gary] Clear ISR_RX_FULL on read and ISR_TX_EMPTY on write

          -- Paged ROM/Interrupt clear
          when x"5" =>
            -- TODO: [Gary] Rom Page handling not yet implemented (AUG p143)
            -- Keyboard slot 8 & 9, Basic rom 10 & 11
            --isrc_paging(IRSC_ROM_PAGE_ENABLE)
            --isrc_paging(ISRC_ROM_PAGE)

            -- Clear requested interrupts
            nmi                       <= nmi and not isrc_paging(ISRC_NMI);
            isr_status(ISR_HIGH_TONE) <= isr_status(ISR_HIGH_TONE) and not isrc_paging(ISRC_HIGH_TONE);
            isr_status(ISR_RTC)       <= isr_status(ISR_RTC) and not isrc_paging(ISRC_RTC);
            isr_status(ISR_FRAME_END) <= isr_status(ISR_FRAME_END) and not isrc_paging(ISRC_FRAME_END);
            isrc_paging(ISRC_INTERRUPTS) <= (others => '0');

          -- Counter/Cassette control (write only)
          when x"6" => multi_counter <= b_pd;

          -- Controls
          when x"7" => misc_control <= b_pd(7 downto 1);
          -- TODO: What to do about mode change?
          -- Palette 
          when others => colour_palettes(to_integer(unsigned(i_addr(3 downto 0)))) <= b_pd;            

        end case;        
      end if;

      -- Interrupt Generation
      if (vpix = vtotal) then
        isr_status(ISR_FRAME_END) <= '1';
      end if;

      -- TODO: [Gary] Counter to generate 50Hz RTC interrupt every 320000 clocks
    end if;
  end process;

  -- ====================================================================
  -- Interfacing
  -- ====================================================================

  -- 
  -- Keyboard Interface
  --
  
  -- TODO: Keyboard reading
  -- b_pd <= .... else (others => 'Z');

  o_caps_lock <= misc_control(MISC_CAPS_LOCK);

  -- 
  -- Sound Interface
  --

  --
  -- Cassette Interface
  --

  -- TODO: [Gary] shift in new bit of data from cassette every ~2ms
  -- read full interrupt every 8 bits
  -- write empty interrupt after 8 bits output
  -- High tone interrupt

end;

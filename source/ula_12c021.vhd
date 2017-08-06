--
-- Copyright 2017 Gary Preston <gary@mups.co.uk>
-- All rights reserved
--
-- Redistribution and use in source and synthesized forms, with or without
-- modification, are permitted provided that the following conditions are met:
--
-- Redistributions of source code must retain the above copyright notice,
-- this list of conditions and the following disclaimer.
--
-- Redistributions in synthesized form must reproduce the above copyright
-- notice, this list of conditions and the following disclaimer in the
-- documentation and/or other materials provided with the distribution.
--
-- Neither the name of the author nor the names of other contributors may
-- be used to endorse or promote products derived from this software without
-- specific prior written permission.
--
-- License is granted for non-commercial use only.  A fee may not be charged
-- for redistributions as source code or in synthesized/hardware form without
-- specific prior written permission.
--
-- THIS CODE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
-- AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
-- THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
-- PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE
-- LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
-- CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
-- SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
-- INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
-- CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
-- ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.
--
-- You are responsible for any legal issues arising from your use of this code.
--

-- Acorn Electron ULA
-- Ferranti 12C021 Custom
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

    -- ULA is clock enabled on clk_sys rather than a new clock domain
    i_clk_sys     : in bit1;
    i_cph_sys     : in word(3 downto 0);

    i_ena_ula     : in bit1;                    
    i_div13_ena   : in bit1;

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
    -- i_clk      : in bit1;
    -- i_div13    : in bit1;
       
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
    o_n_reset     : out bit1;

    -- ROM/CPU addressing
    o_rom         : out bit1;                  -- rom select enable   
    i_addr        : in word( 15 downto 0 );
    b_pd          : inout word( 7 downto 0 );  -- CPU/ROM data

    -- CPU
    i_n_nmi       : inout bit1;                -- 1MHz RAM access detection
    o_ena_phi_out : out bit1;                  -- CPU clk enable, 2MHz, 1MHz or stopped
    o_n_irq       : out bit1;
    i_n_w         : in bit1                    -- Data direction, /write, read

  );
end;

architecture RTL of ULA_12C021 is

  -- Framework Video
  signal ana_hsync, ana_vsync, ana_de : bit1;
  signal dig_hsync, dig_vsync, dig_de : bit1;

  signal vid_rst : bit1;
  signal vpix, hpix, vtotal : word(13 downto 0);
  signal vid_sol : bit1;  
  signal vid_text_mode, vid_v_blank, vid_h_blank : boolean;
  signal vid_row_count : integer range 0 to 10;

  signal ram_contention : boolean;

  -- Adjusted screen base and wrap addr for current mode
  signal mode_base_addr : unsigned(14 downto 6);
  signal mode_wrap_addr : unsigned(14 downto 6);

  signal rst : bit1;
  signal nmi : bit1;

  signal ula_ram_addr : word(14 downto 0);

  signal cpu_ram_data : word(7 downto 0);

  signal ram_cpu_slot : bit1 := '0';
  signal ram_data     : word(7 downto 0);
  signal ram_addr     : word(14 downto 0);
  signal ram_n_w      : bit1;

  -- CPU Timing
  type t_cpu_clk is (CPU_1MHz, CPU_2MHz, CPU_STOPPED);
  signal cpu_target_clk : t_cpu_clk;
  type t_clk_state is (CLK_1MHz, CLK_2MHz, CLK_TRANSITION);
  signal cpu_clk_state : t_clk_state; 

  signal phi_out   : bit1;
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

  -- Hard/Soft Reset
  rst <= not i_n_reset or not i_n_por;  
  o_n_reset <= i_n_por and i_n_reset;
  
  -- Internal weak pull-up
  i_n_nmi <= 'H';
  
  -- ====================================================================
  -- Master Timing
  -- ====================================================================
  -- 2MHz & 1MHz generator based on 16MHz clock
  -- ULA ticks 0..15 with 1MHz active on phase 0, 2MHz on 0 and 8
  -- State transition sampling on phase 1 and 9 (reacting to cpu changes).
  --
  -- Transitions from 2MHz to 1MHz depend on 2MHz phase:
  --   Phase 0: Switch to 1MHz, nothing else required 
  --   Phase 8: Stretched pulse, in effect skip the next 1MHz tick
  --            then resume clocking on the following 1MHz tick
  -- Phase 8 transition ensures that the RAM read that was setup on the
  -- 2MHz tick (phase 8) and will not be serviced until phase 0, is not
  -- lost due to a 1MHz tick occuring (that is why the 1MHz tick is skipped). 
  --
  -- Final quirk is that in any state, entire CPU clocking may be
  -- stopped. State transitions will not occur. It is assumed clocking
  -- will resume on the same clk_phase it was stopped on.
  --
  -- Note: Even ula ticks are aligned with sys_cph(3) for DRAM access
  p_clk_gen : process(i_clk_sys, rst)
  begin
    if (rst = '1') then
      vid_rst <= '1';    
      clk_phase <= "0000";
      cpu_clk_state <= CLK_1MHz;
    elsif rising_edge(i_clk_sys) then
      if i_cph_sys(1) = '1' or i_cph_sys(3) = '1' then
        clk_phase <= clk_phase + 1;

      end if;

      phi_out <= '0';

      -- bring video out of reset to align hpix 0 with phase 0
      if (vid_rst = '1' and clk_phase = "1111") then
        vid_rst <= '0';
      end if;

      -- TODO: [Gary] Whilst handling of STOPPED cpu should be accounted for, the whole logic is untested 
      -- and may turn out to be horribly broken. Extensive testing required when modes 0..3 are implemented.

      -- CPU_STOPPED will be asserted after cph(2) has created a pulse, but before cph(3) has changed
      -- states. Likewise /CPU_STOPPED will occur on cph(3) allowing state change to occur without
      -- generating an extra clock pulse that would otherwise cause the delayed RAM access to be lost. 
      -- NOTE: This relies on ram_contention changing only on phase 0000.
      if (cpu_target_clk /= CPU_STOPPED) then
        -- Clock gen on cph(2) to edge align with cph(3)
        if (i_cph_sys(2) = '1') then          
          if (cpu_clk_state = CLK_1MHz and cpu_target_clk = CPU_1MHz and clk_phase = "0000") then          
            -- 1MHz pulse
            phi_out <= '1';
          elsif ( (cpu_clk_state = CLK_2MHz or cpu_target_clk = CPU_2MHz) and clk_phase(2 downto 0) = "000") then
            -- 2MHz pulse or transition to 2MHz pulsing
            phi_out <= '1';          
          end if;
        end if;

        -- State transitions checked on cph(3)
        if (i_cph_sys(3) = '1') then
          case cpu_clk_state is
            when CLK_1MHz =>
              if (cpu_target_clk = CPU_2MHz) then
                -- 2MHz transition safe to occur at any time
                cpu_clk_state <= CLK_2MHz;
              end if;
            when CLK_2MHz => 
              if (cpu_target_clk = CPU_1MHz) and (clk_phase(3) = '1') then
                -- ram access attempt during 2MHz only phase, transition to 1MHz
                cpu_clk_state <= CLK_TRANSITION;
              elsif (cpu_target_clk = CPU_1MHz) and (clk_phase(3) = '0') then
                -- ram access during 1MHz aligned phase, no transition required
                cpu_clk_state <= CLK_1MHz;
              end if;
            when CLK_TRANSITION => -- 2MHz -> 1MHz
              -- Transition on 1000 to ensure a full 1MHz RAM cycle has
              -- occured whilst waiting in this state
              if (clk_phase = "1000") then
                cpu_clk_state <= CLK_1MHz;
              end if;
          end case;
        end if;
      end if;

    end if;
  end process;

  -- CPU clocking based on access type
  cpu_target_clk <= CPU_1MHz when nmi = '1' else                       
                    CPU_1MHz when i_addr(15 downto 9) = "1111110" else  -- ROM Fred/Jim
                    CPU_2MHz when i_addr(15) = '1' else                 -- Any other ROM access
                    CPU_STOPPED when misc_control(MISC_DISPLAY_MODE'LEFT) = '0' and ram_contention else -- RAM access mode 0..3
                    CPU_1MHz;                                           -- Ram access  

  o_ena_phi_out <= phi_out and i_cph_sys(3);

  -- ====================================================================
  -- Video
  -- ====================================================================

  -- Mode reads required on phase 0 or 8 ready for following phase as follows:
  -- Mode | Res                  | Read Phase  | cnt
  --  0   | 640x256 1bpp         | 1000, 0000  | 1   
  --  1   | 320x256 2bpp         | 1000, 0000  | 2   
  --  2   | 160x256 4bpp         | 1000, 0000  | 4   
  --  3   | 640x250 1bpp (text)  | 1000, 0000  | 1   
  --  4   | 320x256 1bpp         | 1000        | 2   
  --  5   | 160x256 2bpp         | 1000        | 4   
  --  6   | 320x250 1bpp (text)  | 1000        | 2   
  --
  -- E.g mode 6 needs 1 byte read during phase 8, ready for output on phase 0
  -- Due to 1bpp and repeated pixels, one byte covers phase 0-7 & 8-15.
  -- Mode 3 lacks repeated pixels and needs a new byte every 0 & 8 and only
  -- one byte covers at most either 0-7 or 8-15.
  --
  -- See AUG:
  --   p214 Colour palette
  --   p234 Graphics Modes (Appendix C)
  --   

  -- TODO: [Gary] Did Electron generate offset/interlaced display or use matching fields for 256p?
  u_VideoTiming : entity work.Replay_VideoTiming
    generic map (
      g_enabledynamic       => '0',
      g_param               => c_Vidparam_832x287p_50_16MHz
      )
    port map (
      i_clk                 => i_clk_sys,
      i_ena                 => i_ena_ula,
      i_rst                 => vid_rst,
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
      o_dig_sol             => vid_sol,
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
  -- location so digital h/v passed out for now. This is a bit of a kludge as the
  -- timing of dig h/v may not match that of analog h/v (or combined csync)
  o_n_hsync <= dig_hsync;
  o_n_vsync <= dig_vsync;
  o_n_csync <= ana_hsync;
  o_de      <= ana_de;
  
  vid_text_mode <= misc_control(MISC_DISPLAY_MODE) = "110" or
                   misc_control(MISC_DISPLAY_MODE) = "011";
  vid_v_blank <= (unsigned(vpix) < 16) or
                 (unsigned(vpix) >= 16+256) or
                 (unsigned(vpix) >= 16+250 and vid_text_mode);
  vid_h_blank <= unsigned(hpix) < 64 or unsigned(hpix) >= 704;
  
  p_vid_out : process(rst, vid_rst, i_clk_sys)
    variable pix_idx : integer range 0 to 7;
    variable pixel_data : word(7 downto 0);
    variable repeat_count : integer range 0 to 4;
    variable repeat_count_reg : integer range 0 to 4;
  begin
    if (rst = '1') or (vid_rst = '1') then
      pixel_data := (others => '0');    
      pix_idx := 7;
      repeat_count := 0;
    elsif rising_edge(i_clk_sys) then
      if (i_ena_ula = '1') then

        -- overscan
        o_rgb <= x"000000";

        -- Byte read on phase 8 only when mode 0..3
        if ((clk_phase = "0000") or 
            (clk_phase = "1000" and misc_control(MISC_DISPLAY_MODE'LEFT) = '0')) then
          pixel_data := ram_data;
                    
          case misc_control(MISC_DISPLAY_MODE) is
            when "000" | "011" => repeat_count_reg := 0;
            when "001" | "100" | "110" | "111" => repeat_count_reg := 1;
            when "010" | "101" => repeat_count_reg := 3;
            when others => -- usused          
          end case;        

          pix_idx := 7;
          repeat_count := repeat_count_reg;
        end if;

        if (not vid_v_blank and vid_row_count < 8) then
          
          if (not vid_h_blank) then
            case misc_control(MISC_DISPLAY_MODE) is
              -- TODO: [Gary] sort modes out for pixel interpretation
              when "000" | "011" | "100" | "110" =>
                -- Mode 0,3,4,6 : 1bpp 7,6,5,4,3,2,1,0
                if (pixel_data(pix_idx) = '1') then
                  -- TODO: [Gary] should this be paletted for the two colours?
                  o_rgb <= x"FFFFFF";
                end if;
              when "001" | "101" =>
                -- Mode 1,5     : 2bpp 7&3, 6&2, 5&1, 4&0
                o_rgb <= x"FF0000";
              when "010" =>        
                -- Mode 2       : 4bpp 7&5&3&1, 6&4&2&0
                o_rgb <= x"FF0000";
              when others =>
                -- TODO: [Gary] Mode 7 should be treated as 4?
                o_rgb <= x"FF0000";
            end case;

            if repeat_count = 0 then
              pix_idx := pix_idx - 1;
              repeat_count := repeat_count_reg;
            else
              repeat_count := repeat_count - 1;
            end if;

          end if;
        end if;

      end if;
    end if;
  end process;

  p_vid_addr : process(i_clk_sys, rst, vid_rst, mode_base_addr)
    -- start address of current row
    variable row_addr  : unsigned(15 downto 0);
    -- address of byte to fetch from RAM
    variable read_addr : unsigned(15 downto 0);

  begin
    if (rst = '1' or vid_rst = '1') then
      row_addr := '0' & mode_base_addr & "000000";
      read_addr := row_addr;
      vid_row_count <= 0;
      ram_contention <= false;
    elsif rising_edge(i_clk_sys) then      
      if (i_ena_ula = '1') then

        -- Check for CPU RAM contention change only on phase 0
        if (clk_phase = "0000") then
          -- TODO: [Gary] 2 blanking lines in mode 3 are contention free or not?
          -- TODO: [Gary] if using 704 for contention, may end up doing  byte more than
          --              needed? On pixel 696 data for that pixel should already be available
          --              and no more bytes follow it, just h_sync/border. render process needs 704 
          --              check though. switch to hpix here instead? Otherwise 1 more RAM cycle
          --              under contention than needed.
          ram_contention <= (not (vid_v_blank or vid_h_blank)) and vid_row_count < 8 and
                             misc_control(MISC_DISPLAY_MODE'LEFT) = '0';
        end if;

        if (unsigned(vpix) = 0) then
          -- Latch mode adjusted screen start. Wrap is not latched and may
          -- change mid frame depending on mode.
          row_addr := '0' & mode_base_addr & "000000";
          read_addr := row_addr;
          vid_row_count <= 0;
        end if;

      
        if (not vid_v_blank) then
          -- end of active line
          if (unsigned(hpix) = 704) then  -- switch to 696? ie last block of pixels? to save 1 redundant read/contention cycle
            if ( (vid_row_count = 9) or (vid_row_count = 7 and not vid_text_mode) ) then
              vid_row_count <= 0;
              if (misc_control(MISC_DISPLAY_MODE'LEFT) = '0') then
                row_addr := row_addr + 640;
              else
                row_addr := row_addr + 320;
              end if;
              read_addr := row_addr;
            else
              vid_row_count <= vid_row_count + 1;
              read_addr := row_addr + vid_row_count + 1;
            end if;
          end if;
        end if;

        -- Every 8 or 16 pixels depending on mode/repeats
        if (clk_phase = "1000" or (clk_phase = "0000" and misc_control(MISC_DISPLAY_MODE'LEFT) = '0')) then 
          if (not (vid_v_blank or vid_h_blank) and vid_row_count < 8 ) then          
              read_addr := read_addr + 8;
          end if;
        end if;  

        -- Frame read_addr overflowed into ROM? Wrap around until reset next frame
        ula_ram_addr <= std_logic_vector(read_addr(14 downto 0));
        if (read_addr(15) = '1') then
          ula_ram_addr <= std_logic_vector(read_addr(14 downto 0) + (mode_wrap_addr & "000000"));
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
  -- 4164 ram is async, however this implementation uses synchronous ram. 
  -- For a ULA replacement, exact timing requirements of 4164 would need to be
  -- checked and implemented. This is a pseudo ras/cas implementation only.
  --
  
  -- RAM access occurs at 16MHz, however it takes 4 cycles to perform a 4bit
  -- read, 8 cycles to get a full byte. This is effectively 1 byte per 1MHz clk.
  -- The ULA time shares ram access (1MHz period each) with the CPU as:
  -- Cycle 0:
  --   * cpu gets the slot by default
  --   * ULA overrides cpu if mode 0,1,2,3 during ram_contention
  --   * CPU overrides ULA if nmi set
  -- Cycle 8:
  --   * ula always gets this slot
  --
  -- Ram slot check based on clk_phase 0001 but will be stable before the ula clock occurs on that phase
  
  -- TODO: [Gary] this is ending up as a latch. fix.
  p_ram_access_sel : process(clk_phase, i_addr, rst, nmi, ram_contention)
  begin
    if (rst = '1') then
      ram_cpu_slot <= '0';
    elsif (clk_phase(2 downto 0) = "001") then
      -- ula always has phase 8 slot
      ram_cpu_slot <= '0';

      -- ula/cpu contention over phase 0 slot
      if (clk_phase(3) = '0') and (i_addr(15) = '0') then
        ram_cpu_slot <= '1';
        if (nmi = '0') and ram_contention then
          ram_cpu_slot <= '0';
        end if;
      end if;    
    end if;
  end process;

  -- Ram access on behalf of CPU or ULA
  p_ram_access : process(i_clk_sys, rst)
    variable ram_even_tmp  : word(3 downto 0);
  begin
    if (rst = '1') then
      o_n_we <= '1';
      o_n_cas <= '1';
      o_n_ras <= '1';
      ram_data <= (others => '0');
    elsif rising_edge(i_clk_sys) then
      if (i_ena_ula = '1') then
        if (ram_n_w = '1') then 
          b_ram0 <= 'Z'; b_ram1 <= 'Z'; b_ram2 <= 'Z'; b_ram3 <= 'Z';
        end if;

        -- Read/write of byte split into two 4 cycle stages handling 4 bits each.        
        case clk_phase(2 downto 0) is
          when "000" =>
            -- CPU clocked on 1 / 2MHz bounds here when enabled.
            -- addr/data lines set by CPU or ULA depending on slot/priority.
          when "001" =>
            -- row latch
            o_ra <= ram_addr(14 downto 7);
            o_n_ras <= '0';
            o_n_cas <= '1';
            o_n_we <= '1';
          when "010" =>
            -- col latch
            o_ra <= ram_addr(6 downto 0) & '0';
            o_n_cas <= '0';
            o_n_we <= ram_n_w;
            if (ram_n_w = '0') then
              b_ram0 <= b_pd(0);
              b_ram1 <= b_pd(2);
              b_ram2 <= b_pd(4);
              b_ram3 <= b_pd(6);
            end if;
          when "011" =>
            -- Unused, future DRAM delay
            -- Might require two spare cycles with current DRAM setup?
          when "100" =>
            if (ram_n_w = '1') then
              ram_even_tmp(0) := b_ram0;              
              ram_even_tmp(1) := b_ram1;
              ram_even_tmp(2) := b_ram2;
              ram_even_tmp(3) := b_ram3;
            end if;
            o_n_we <= '1';
            o_n_cas <= '1';            
          when "101" =>            
            -- second nibble cycle setup
            o_ra <= ram_addr(6 downto 0) & '1';
            o_n_cas <= '0';
            o_n_we <= ram_n_w;
            if (ram_n_w = '0') then
              b_ram0 <= b_pd(1);
              b_ram1 <= b_pd(3);
              b_ram2 <= b_pd(5);
              b_ram3 <= b_pd(7);
            end if;
          when "110" =>
            -- Unused, future DRAM delay
          when "111" => 
            if (ram_n_w = '1') then
              ram_data <= b_ram3 & ram_even_tmp(3) & b_ram2 & ram_even_tmp(2) &
                          b_ram1 & ram_even_tmp(1) & b_ram0 & ram_even_tmp(0);

              -- Don't allow ULA to clobber CPUs last read data
              if (ram_cpu_slot = '1') then
                cpu_ram_data <= b_ram3 & ram_even_tmp(3) & b_ram2 & ram_even_tmp(2) &
                                b_ram1 & ram_even_tmp(1) & b_ram0 & ram_even_tmp(0);
              end if;
            end if;
            o_n_ras <= '1';
            o_n_cas <= '1';  
            o_n_we <= '1';
          when others =>
            -- unused
        end case;

      end if;
    end if;
  end process;                

  -- ram access
  ram_addr <= i_addr(14 downto 0) when ram_cpu_slot = '1' else ula_ram_addr;
  -- cpu r/w, ula always reads
  ram_n_w <= i_n_w when ram_cpu_slot = '1' else '1';

  -- reg'd cpu data from before ULA read
  b_pd <= cpu_ram_data when i_addr(15) = '0' and i_n_w = '1' else (others => 'Z');

  -- ====================================================================
  -- ROM
  -- ====================================================================
  -- Enable main board rom for OS access or BASIC rom if page enable
  -- TODO: [Gary] reading any register other than 0 or 4 should read from os/basic rom.
  
  -- ROM enabled for 0x8000 - 0xBFFF when page 10 or 11 active, or for
  -- 0xC000 - 0xFFFF except for the memory mapped i/o in 0xFCXX, 0xFDXX, 0xFEXX
  o_rom <= '1' when (i_addr(15) = '1' and i_addr(14) = '0' and        
                     isrc_paging(ISRC_ROM_PAGE_ENABLE) = '1' and        -- ROM page 10 or 11
                     isrc_paging(ISRC_ROM_PAGE'left downto ISRC_ROM_PAGE'right+1) = "01" ) else
           --'1' when (i_addr >= x"C000" and i_addr <= x"FBFF") else      -- ROM OS
           --'1' when (i_addr >= x"FF00" and i_addr <= x"FFFF") else      -- ROM OS
           '1' when (i_addr(15) = '1' and i_addr(14) = '1') and         -- ROM OS except mem mapped i/o
                    (i_addr(15 downto 8) /= x"FC") and
                    (i_addr(15 downto 8) /= x"FD") and
                    (i_addr(15 downto 8) /= x"FE") else
           '0';

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

  p_registers : process(i_clk_sys, i_n_reset, i_n_por)
    -- delay POR reset until next CPU clock
    variable delayed_por_reset : bit1 := '0';
  begin
    if (i_n_reset = '0') or (i_n_por = '0') then
      isr_en <= (others => '0');
      isr_status(6 downto 1) <= (others => '0');
      isrc_paging(ISRC_ROM_PAGE) <= "000";
      isrc_paging(ISRC_ROM_PAGE_ENABLE) <= '0';
      screen_start_addr <= (others => '0');
      multi_counter <= (others => '0');
      misc_control <= (others => '0');
      -- TODO: [Gary] is this a correct default? starting in mode 0 would cause cpu to never execute
      -- during "ram_contention", although boot should still work, just take longer.
      misc_control(MISC_DISPLAY_MODE) <= "110";
      colour_palettes <= (others => (others => '0'));
      rtc_count <= (others => '0');

      if (i_n_por = '0') then
        isr_status(ISR_POWER_ON_RESET) <= '1';
      end if;
    elsif rising_edge(i_clk_sys) then
      if (i_ena_ula = '1') then
        -- Delayed POR reset pending?
        if (delayed_por_reset = '1' and phi_out = '1') then
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
        if (unsigned(vpix) = unsigned(vtotal)-1) then
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

    end if;
  end process;

  p_screen_addr : process(screen_start_addr, misc_control)
    variable base_addr : word(15 downto 6);
  begin    
    -- mdfs.net notes that if addr 0 is loaded, it will be replaced by a
    -- hardcoded per mode base address. Also used if address overflows back to 0.
    -- 3000 for 0,1,2; 4000 for 3; 5800 for 4,5; 6000 for 6.
    case misc_control(MISC_DISPLAY_MODE) is
      when "000" | "001" | "010" => base_addr := x"30" & "00";
      when "011" => base_addr := x"40" & "00";
      when "100" | "101" | "111" => base_addr := x"58" & "00";
      when "110" => base_addr := x"60" & "00";
      when others =>
    end case;
    
    -- TODO: [Gary] May be more to it than this, pastraiser suggests anything
    -- below 800H caused base_addr to be used (firmware skips clearing this region
    -- of ram too on startup) as well as other variations/skips. This needs further
    -- research.
    if screen_start_addr = x"00" & '0' then
      mode_base_addr <= unsigned(base_addr(14 downto 6));
    else
      mode_base_addr <= unsigned(screen_start_addr);
    end if;

    -- Wrapping always starts from the hardcoded address regardless
    -- of screen_start_addr.
    mode_wrap_addr <= unsigned(base_addr(14 downto 6));
  end process;

  -- ====================================================================
  -- Interfacing
  -- ====================================================================
  
  -- 
  -- Keyboard Interface
  --

  -- Keyboard rom active
  -- Invert key state to give 1 for pressed
  b_pd <= (x"0" & not i_kbd) when  (i_addr(15) = '1' and i_addr(14) = '0' and  
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

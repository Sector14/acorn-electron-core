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
-- Implementation based on AUG and the reverse engineering of others. Improvements
-- are likely once the the 12C021 is fully analysed.
--
-- Note: This implementation is a compromise between matching the external interface
-- of the real ULA and providing the replay framework with the data it needs to
-- function.

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
    o_n_vsync     : out bit1;
    o_de          : out bit1;

    -- ULA is clock enabled on clk_sys
    i_clk_sys     : in bit1;
    i_cph_sys     : in word(3 downto 0);
   
    --
    -- ULA
    -- 
    
    -- Cassette I/O (not yet supported)
    i_cas         : in bit1; 
    o_cas         : out bit1;                  -- pseudo sine-wave
    b_cas_rc      : inout bit1;                -- Not used, see hightone process.
    o_cas_mo      : out bit1;                  -- Motor relay
           
    -- Audio
    o_sound_op    : out bit1;            
       
    -- Reset             
    i_n_por       : in bit1;                   -- /Power on reset

    -- Video
    o_n_csync     : out bit1;                  -- h/v sync  (low during horizontal or vertical synchronisation)
    o_n_hsync     : out bit1;                  -- h sync    
    o_red         : out bit1;            
    o_green       : out bit1;            
    o_blue        : out bit1; 
  
    -- Clock (used via enables rather than direct clock and must be aligned)
    i_ena_ula     : in bit1;                   
    i_ena_div13   : in bit1;
       
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
    i_n_w         : in bit1;                   -- Data direction, /write, read

    o_debug       : out word(7 downto 0)
  );
end;

architecture RTL of ULA_12C021 is
  constant c_vidparam : r_Vidparam_int := c_Vidparam_832x287p_50_16MHz;

  -- Framework Video
  signal ana_hsync, ana_vsync, ana_de : bit1;
  signal dig_hsync, dig_vsync, dig_de : bit1;

  signal vid_rst : bit1;
  signal vpix, hpix, vtotal : word(13 downto 0);
  signal vid_sol : bit1;  
  signal vid_text_mode, vid_v_blank, vid_h_blank : boolean;
  signal vid_row_count : integer range 0 to 10;

  signal ram_contention : boolean;

  -- Audio
  signal snd_data       : bit1;

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

  -- Cassette
  signal cas_i_delay1  : bit1;
  signal cas_i_delay2  : bit1;
  signal cas_i_edge    : boolean;
  signal cas_i_bit     : bit1;
  signal cas_hightone  : boolean;

  -- CPU Timing
  type t_cpu_clk is (CPU_1MHz, CPU_2MHz, CPU_STOPPED);
  signal cpu_target_clk : t_cpu_clk;
  type t_clk_state is (CLK_1MHz, CLK_2MHz, CLK_TRANSITION);
  signal cpu_clk_state : t_clk_state; 

  signal phi_out   : bit1;
  signal clk_phase : unsigned(3 downto 0);

  signal rtc_count : unsigned(18 downto 0);

  -- Cassette Timing enables
  -- These are internally generated and thus stretched across multiple sys clocks
  -- ensure they're only ever used within ena_ula.
  signal ck_div128 : bit1;
  signal ck_div26  : bit1;
  signal ck_div52  : bit1;
  signal ck_cas    : bit1;
  signal ck_freqx  : bit1;

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
  signal cas_o_data_shift     : word(9 downto 0);
  signal cas_i_data_shift     : word(7 downto 0);
  
  -- Interrupt clear & ROM Paging
  subtype ISRC_ROM_PAGE is integer range 2 downto 0;
  constant ISRC_ROM_PAGE_ENABLE  : integer := 3;
  constant ISRC_FRAME_END        : integer := 4;       
  constant ISRC_RTC              : integer := 5; 
  constant ISRC_HIGH_TONE        : integer := 6;       
  constant ISRC_NMI              : integer := 7;
  signal isrc_paging             : word(3 downto 0);

  -- Multipurpose Counter
  signal multi_counter           : unsigned(8 downto 0);
  signal multi_cnt_reg           : unsigned(7 downto 0);

  -- Misc control
  subtype MISC_COMM_MODE is integer range 2 downto 1;
  subtype MISC_DISPLAY_MODE is integer range 5 downto 3;
  constant MISC_CASSETTE_MOTOR   : integer := 6;
  constant MISC_CAPS_LOCK        : integer := 7;
  signal misc_control            : word(7 downto 1);

  constant MISC_COMM_MODE_INPUT  : word(1 downto 0) := "00";
  constant MISC_COMM_MODE_OUTPUT : word(1 downto 0) := "10";
  constant MISC_COMM_MODE_SOUND  : word(1 downto 0) := "01";

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
      phi_out <= '0';
    elsif rising_edge(i_clk_sys) then

      phi_out <= '0';

      -- Bring video out of reset to align hpix 0 with phase 0
      if (i_cph_sys(0) = '1' and vid_rst = '1' and clk_phase = "1111") then
        vid_rst <= '0';
      end if;
      
      if i_cph_sys(1) = '1' or i_cph_sys(3) = '1' then
        clk_phase <= clk_phase + 1;
      end if;

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
  
  --
  -- Additional Timing Frequencies
  -- Used by Cassette I/O and Sound.
  p_clk_divider : process(i_clk_sys, rst)
    variable div128 : integer range 127 downto 0;
    variable div26  : bit1;
    variable div52  : bit1;
  begin
    if (rst = '1') then
      ck_div128 <= '0';
      ck_div26 <= '0';
      ck_div52 <= '0';
      div128 := 0;
      div26 := '0';
      div52 := '0';
    elsif rising_edge(i_clk_sys) then
      -- NOTE: These must be used within i_ena_ula as they stretch multiple sys clocks.
      -- NOTE: div13 is must be aligned with ula_ena and used within ena_ula.
      --       Without this assumption FREQX/multi counter will need reviewing as
      --       it uses enables derived from both.
      
      if i_ena_ula = '1' then
        ck_div128 <= '0';

        -- (M/8): 16MHz/128 = 0.125MHz for SOUND
        if (div128 = 127) then
          div128 := 0;
          ck_div128 <= '1';
        else
          div128 := div128 + 1;
        end if;

        -- (S8M13 and S8M13/2): ~615kHz and ~307kHz for cassette I/O
        ck_div26 <= '0';
        ck_div52 <= '0';
        if i_ena_div13 = '1' then
          div26 := not div26;

          if div26 = '1' then
            div52 := not div52;
          end if;

          ck_div26 <= div26;
          ck_div52 <= div26 and div52;
        end if;

      end if;
    end if;
  end process;

  -- Mode based frequency selection
  ck_freqx <= ck_div128 when misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_SOUND else
              ck_cas    when misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_INPUT else
              ck_div26  when misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_OUTPUT else
              '0';

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
      g_param               => c_vidparam
      )
    port map (
      i_clk                 => i_clk_sys,
      i_ena                 => i_ena_ula,
      i_rst                 => vid_rst,
      --
      i_param               => c_vidparam,
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
  -- timing of dig h/v may not match that of analog h/v (or csync)
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
    variable pixel_data : word(7 downto 0);
    variable pix_idx : integer range 0 to 7;

    variable logical_colour : unsigned(3 downto 0);
    variable rgb : word(2 downto 0);
    
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
        o_red <= '0';
        o_green <= '0';
        o_blue <= '0';

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

            logical_colour := (others => '0');

            -- Decode pixel to logical colour
            case misc_control(MISC_DISPLAY_MODE) is
              when "000" | "011" | "100" | "110" | "111" =>
                -- Mode 0,3,4,6 : 1bpp 7,6,5,4,3,2,1,0                    
                logical_colour := "000" & pixel_data(pix_idx);
              when "001" | "101" =>
                -- Mode 1,5     : 2bpp 7&3, 6&2, 5&1, 4&0
                logical_colour := "00" & pixel_data(pix_idx) & pixel_data(pix_idx-4);
              when "010" =>
                -- Mode 2       : 4bpp 7&5&3&1, 6&4&2&0
                 logical_colour := pixel_data(pix_idx) & pixel_data(pix_idx-2) & pixel_data(pix_idx-4) & pixel_data(pix_idx-6);                 
              when others =>
            end case;

            -- Palette Lookup (AUG p215)
            -- TODO: [Gary] There has to be some logic to the palette register format that will
            --              avoid this big case but I'm not seeing it.
            case misc_control(MISC_DISPLAY_MODE) is
              when "000" | "011" | "100" | "110" | "111" =>
                -- 2 colour
                case to_integer(logical_colour) is 
                  when 0 => 
                    rgb := colour_palettes(9)(0) & colour_palettes(8)(4) & colour_palettes(8)(4);
                  when others => -- 1
                    rgb := colour_palettes(9)(2) & colour_palettes(8)(2) & colour_palettes(8)(6);
                end case;
                
              when "001" | "101" =>
                case to_integer(logical_colour) is 
                  -- 4 colour
                  when 0 => 
                    rgb := colour_palettes(9)(0) & colour_palettes(9)(4) & colour_palettes(8)(4);
                  when 1 =>
                    rgb := colour_palettes(9)(1) & colour_palettes(9)(5) & colour_palettes(8)(5);
                  when 2 => 
                    rgb := colour_palettes(9)(2) & colour_palettes(8)(2) & colour_palettes(8)(6);
                  when others => -- 3
                    rgb := colour_palettes(9)(3) & colour_palettes(8)(3) & colour_palettes(8)(7);
                end case;

              when "010" =>
                case to_integer(logical_colour) is 
                  -- 16 colour
                  when 0 => 
                    rgb := colour_palettes(9)(0) & colour_palettes(9)(4) & colour_palettes(8)(4);
                  when 1 =>
                    rgb := colour_palettes(15)(0) & colour_palettes(15)(4) & colour_palettes(14)(4);
                  when 2 => 
                    rgb := colour_palettes(9)(1) & colour_palettes(9)(5) & colour_palettes(8)(5);
                  when 3 => 
                    rgb := colour_palettes(15)(1) & colour_palettes(15)(5) & colour_palettes(14)(5);
                  when 4 =>
                    rgb := colour_palettes(11)(0) & colour_palettes(11)(4) & colour_palettes(10)(4);
                  when 5 => 
                    rgb := colour_palettes(13)(0) & colour_palettes(13)(4) & colour_palettes(12)(4);
                  when 6 => 
                    rgb := colour_palettes(11)(1) & colour_palettes(11)(5) & colour_palettes(10)(5);
                  when 7 =>
                    rgb := colour_palettes(13)(1) & colour_palettes(13)(5) & colour_palettes(12)(5);
                  when 8 => 
                    rgb := colour_palettes(9)(2) & colour_palettes(8)(2) & colour_palettes(8)(6);
                  when 9 => 
                    rgb := colour_palettes(15)(2) & colour_palettes(14)(2) & colour_palettes(14)(6);
                  when 10 =>
                    rgb := colour_palettes(9)(3) & colour_palettes(8)(3) & colour_palettes(8)(7);
                  when 11 => 
                    rgb := colour_palettes(15)(3) & colour_palettes(14)(3) & colour_palettes(14)(7);
                  when 12 => 
                    rgb := colour_palettes(11)(2) & colour_palettes(10)(2) & colour_palettes(10)(6);
                  when 13 =>
                    rgb := colour_palettes(13)(2) & colour_palettes(12)(2) & colour_palettes(12)(6);
                  when 14 => 
                    rgb := colour_palettes(11)(3) & colour_palettes(10)(3) & colour_palettes(10)(7);
                  when others => -- 15
                    rgb := colour_palettes(13)(3) & colour_palettes(12)(3) & colour_palettes(12)(7);
                end case;

              when others => -- unused
                rgb := "111";
            end case;

            -- Palette uses '1' to turn off that colour
            o_red <= not rgb(2);
            o_green <= not rgb(1);
            o_blue <= not rgb(0);
            
            -- Handle repeated pixel modes
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
  
  -- TODO: [Gary] ram_cpu_slot ends up as a latch, needs resolving. 
  -- Note: ram_cpu_slot needs to be stable before rising edge of "0001" and "1001" as ram_addr 
  --       will be reg'd however value of ram_cpu_slot depeneds on
  --       values set on clock phase "0000" and "1000".
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

  -- ====================================================================
  -- Registers
  -- ====================================================================
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

  p_registers : process(i_clk_sys, rst, i_n_por)
    -- delay POR reset until next CPU clock
    variable delayed_por_reset : bit1;

    variable in_reset : boolean;
    variable frameck_ena : boolean;
    variable frameck_cnt : integer range 0 to 3;
    variable cas_i_bits : integer range 0 to 9;
    
    variable cas_o_bits : integer range 0 to 10;
    variable cas_o_data : bit1;
    variable cas_o_halt : boolean;
    variable cas_o_init : boolean;
  begin
    if rst = '1' then      
      isr_en <= (others => '0');
      isr_status(6 downto 1) <= (others => '0');
      isrc_paging(ISRC_ROM_PAGE) <= "000";
      isrc_paging(ISRC_ROM_PAGE_ENABLE) <= '0';
      screen_start_addr <= (others => '0');
      multi_cnt_reg <= (others => '0');
      misc_control <= (others => '0');
      misc_control(MISC_DISPLAY_MODE) <= "110";
      misc_control(MISC_COMM_MODE) <= MISC_COMM_MODE_INPUT;
      colour_palettes <= (others => (others => '0'));
      rtc_count <= (others => '0');
      
      cas_o_data_shift <= (others => '0');

      delayed_por_reset := '0';

      in_reset := false;
      frameck_ena := false;
      frameck_cnt := 0;
      cas_i_bits := 0;

      cas_o_data := '0';
      cas_o_bits := 0;
      cas_o_init := true;
      cas_o_halt := false;      
      
      o_cas <= '0';

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
              when x"4" =>
                cas_o_data_shift(9 downto 2) <= b_pd;
                cas_o_data_shift(1) <= '0';
                cas_o_init := false;

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
              when x"6" =>
                multi_cnt_reg <= unsigned(b_pd);              

              -- Controls
              when x"7" =>                 
                misc_control <= b_pd(7 downto 1);

              -- Palette 
              when others => colour_palettes(to_integer(unsigned(i_addr(3 downto 0)))) <= b_pd;            

            end case;
          end if;

        end if;
                 
        -- Interrupt Generation
        -- Aligned to end of hsync
        if unsigned(hpix) = c_vidparam.syncp_h + c_vidparam.syncw_h - 1 then
          -- Display, generate on the line after last active display line
          if (unsigned(vpix) = 16+256 and not vid_text_mode) or
             (unsigned(vpix) = 16+250 and vid_text_mode) then
              isr_status(ISR_FRAME_END) <= '1';
          end if;

          -- RTC 10ms before FRAME END interrupt, roughly display line 100
          if unsigned(vpix) = 16+100 then
            isr_status(ISR_RTC) <= '1';
          end if;
        end if;
     
        --
        -- Cassette Registers
        --
        -- TODO: Read and write can be split out to separate processes with
        --       ISR for RX and TX based on external signals just as hightone
        --       set and clear is.
        if ck_freqx = '1' then

          -- 
          -- Reading
          --
          if cas_hightone or cas_i_bits = 8 then
            if cas_hightone then
              isr_status(ISR_HIGH_TONE) <= '1';
              isr_status(ISR_RX_FULL) <= '0';
            end if;

            cas_i_bits := 0;
            frameck_cnt := 0;
            in_reset := true;
          elsif (multi_counter(7 downto 0) = 250 or    -- S1
                 multi_counter(7 downto 0) = 170) then -- S11                 
            if frameck_cnt = 3 then
              frameck_cnt := 0;
              if not in_reset then
                frameck_ena := true;
              end if;
              in_reset := false;
            else
              frameck_cnt := frameck_cnt + 1;
            end if;
          end if;
          
          if in_reset then
            cas_i_bits := 0;
            if cas_i_bit = '1' then
              -- 1/2 of 1200Hz pulse will have already happened by the time start
              -- bit is detected, pre-sync counter.
              frameck_cnt := 2;
            end if;
          end if;
          
          if frameck_ena then
            cas_i_bits := cas_i_bits + 1;
            cas_i_data_shift <= cas_i_bit & cas_i_data_shift(7 downto 1);
            if cas_i_bits = 8 then 
              isr_status(ISR_RX_FULL) <= '1';
            end if;
          end if;

          frameck_ena := false;

        end if;

        --
        -- Cassette Writing
        --
        o_debug(2) <= '0';
        -- Reset counter after register write ends
        if i_n_w = '1' or i_addr(3 downto 0) /= x"4" then
          -- writes made during a transfer do not reset counter
          if not cas_o_init then
            cas_o_bits := 0;
          end if;
        end if;

        if ck_freqx = '1' then     

          if multi_counter = '1' & x"FF" then
            -- no clocking shift reg/counter during halt
            if not cas_o_halt then
              cas_o_data_shift <= '1' & cas_o_data_shift(9 downto 1);
              cas_o_bits := cas_o_bits + 1;
            end if;
            o_debug(2) <= '1';
          end if;

        end if;

        if cas_o_bits = 10 then
          -- start of transfering 10th bit (stop bit) notify cpu
          isr_status(ISR_TX_EMPTY) <= '1';
          cas_o_halt := true;
        else
          isr_status(ISR_TX_EMPTY) <= '0';
          cas_o_halt := false;
          cas_o_init := true;
        end if;

        if cas_o_halt then
          cas_o_data_shift(0) <= '1';
        end if;

        
        -- TODO: This is a square wave based o_cas for use with virtual cassette interface
        --       need to also generate a pseudo sine wave to output to aux pins for real cassette.
        --       It bears little resemblence to the ULA CASA0..2 interface
        o_cas <= '0';
      
        if misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_OUTPUT then 
          if multi_counter = '1' & x"FF" then -- ???
            cas_o_data := cas_o_data_shift(0); 
          end if;
          -- 9 bit counter clocked at 615kHz
          -- 255 multi_counter: high cycles 128 = 1200Hz, 64 = 2400Hz 
          if cas_o_data = '0' then  
            -- ~1200Hz '0'
            if multi_counter < 256 then 
              o_cas <= '1'; 
            end if; 
          else 
            -- ~2400Hz '1'
            if multi_counter < 128 or (multi_counter >= 256 and multi_counter < 384) then 
              o_cas <= '1'; 
            end if; 
          end if; 
        end if; 
   
      end if; -- end_ula      
    end if;
  
    
  end process;

  o_debug(0) <= cas_o_data_shift(1);
  o_debug(1) <= cas_o_data_shift(0);
  o_debug(3) <= isr_status(ISR_TX_EMPTY);

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
          '1' & isr_status         when i_addr( 3 downto 0) = x"0" else
          cas_i_data_shift         when i_addr( 3 downto 0) = x"4" else
          (others => 'Z');

  o_cas_mo <= misc_control(MISC_CASSETTE_MOTOR);

  --
  -- Video Address
  -- 

  p_screen_addr : process(screen_start_addr, misc_control)
    variable base_addr : word(15 downto 6);
  begin    
    -- mdfs.net notes that if addr 0 is loaded, it will be replaced by a
    -- hardcoded per mode base address. Also used if address overflows back to 0.
    -- 3000 for 0,1,2; 4000 for 3; 5800 for 4,5; 6000 for 6.
    -- NOTE: 3000 used for mode 7 despite treated same as mode 4 everywhere else. Verify.
    case misc_control(MISC_DISPLAY_MODE) is
      when "000" | "001" | "010" | "111" => base_addr := x"30" & "00";
      when "011" => base_addr := x"40" & "00";
      when "100" | "101" => base_addr := x"58" & "00";
      when "110" => base_addr := x"60" & "00";
      when others =>
    end case;
    
    -- TODO: [Gary] May be more to it than this, pastraiser suggests anything
    --       below 800H caused base_addr to be used (firmware skips clearing this region
    --       of ram too on startup) as well as other variations/skips. This needs further
    --       research.
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
  -- Cassette/sound multi counter
  -- 
  p_multi_counter : process(i_clk_sys, rst)
  begin
    if (rst = '1') then
      multi_counter <= (others => '0');
    elsif rising_edge(i_clk_sys) then
      if i_ena_ula = '1' then

        if ck_freqx = '1' then           
          multi_counter <= multi_counter - 1;

          -- MSB indicates a wrap around from 0. Also used as an enable
          -- by cassette data shift and sound output. Only sound resets on wrap.
          if misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_SOUND then
            if multi_counter(8) = '1' then -- = 255 then
              multi_counter <= '0' & multi_cnt_reg;
            end if;
          end if;
        end if;


        if ck_div52 = '1' then
          -- TODO: Reset on pulse edges in input mode also depend upon DATACNT?
          if cas_i_edge and misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_INPUT  then
            multi_counter <= '0' & multi_cnt_reg;
          end if;
        end if;

      end if;
    end if;
  end process;
 
  -- 
  -- Cassette Interface
  --
  -- Based on sheet 4 & 10 of the Synertek ULA schematics with noted exceptions.
  -- NOTE: S* signals are ranged based on the ULA but only a single value has been
  --       used in this implementation currently.

  -- Edge detection
  p_cas_edge : process(i_clk_sys, rst)
  begin
    if rst = '1' then
      cas_i_delay1 <= i_cas;
      cas_i_delay2 <= i_cas;
    elsif rising_edge(i_clk_sys) then
      if i_ena_ula = '1' then
        
        if ck_div52 = '1' then
          cas_i_delay1 <= i_cas;    
          cas_i_delay2 <= cas_i_delay1;
        end if;
      
      end if;
    end if;
  end process;

  cas_i_edge <= true when (cas_i_delay1 xor cas_i_delay2) = '1' else false;

  -- Stop clocking cas counter to resync if S15 reached with no detected edge  
  ck_cas <= '0' when multi_counter(7 downto 0) = 138 else ck_div52;

  -- Frequency detection
  p_cas_freq_decode : process(i_clk_sys, rst)
    variable candidate : bit1;
  begin
    if rst = '1' then
      cas_i_bit <= '0';
      candidate := '0';
    elsif rising_edge(i_clk_sys) then
      if i_ena_ula = '1' then
        if ck_div52 = '1' then
          
          -- candidate 1/0 decode
          if multi_counter(7 downto 0) = 242 then     -- S2
            candidate := '1';
          elsif multi_counter(7 downto 0) = 170 then  -- S11
            candidate := '0';
          end if;

          if cas_i_edge then
            cas_i_bit <= candidate;
          end if;
        end if;
      end if;
    end if;    
  end process;

  -- Hightone detection
  -- NOTE: This is not based on actual ULA which uses external CAS RC
  p_cas_hightone : process(i_clk_sys, rst)
    variable hightone_cnt : integer range 0 to 40;    
  begin
    if rst = '1' then
      cas_hightone <= false;
      hightone_cnt := 0;
    elsif rising_edge(i_clk_sys) then
      if i_ena_ula = '1' then          
        if ck_div52 = '1' then

          if misc_control(MISC_CASSETTE_MOTOR) = '0' or
             multi_counter(7 downto 0) = 138 then  -- S15
            hightone_cnt := 0;
            cas_hightone <= false;
          elsif cas_i_edge then
            cas_hightone <= false;

            if cas_i_bit = '0' then
              hightone_cnt := 0;
            elsif hightone_cnt /= 40 then
              -- 40 edges = 20x2400Hz = 10 '1' bits
              hightone_cnt := hightone_cnt + 1;
            else
              cas_hightone <= true;
            end if;      
          end if;

        end if;
      end if;
    end if;  
  end process;

  -- 
  -- Sound Interface
  --

  p_sound : process(i_clk_sys, rst)
    variable snd_cnt9 : bit1;
    variable snd_src  : bit1;
  begin
    if (rst = '1') then
      o_sound_op <= '0';
      snd_cnt9 := '0';
      snd_src := '0';
    elsif rising_edge(i_clk_sys) then
      if i_ena_ula = '1' then
        if ck_freqx = '1' then           

          if misc_control(MISC_COMM_MODE) = MISC_COMM_MODE_SOUND then
            if multi_counter(8) = '1' then
              snd_cnt9 := not snd_cnt9;
              if snd_cnt9 = '0' then
                snd_src := not snd_src;
              end if;
            end if;

            o_sound_op <= snd_src;
          else
            o_sound_op <= '0';
          end if;

        end if;
      end if;
    end if;
  end process;

end;

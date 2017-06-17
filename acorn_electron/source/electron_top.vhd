--
-- WWW.FPGAArcade.COM
--
-- REPLAY Retro Gaming Platform
-- No Emulation No Compromise
--
-- All rights reserved
-- Mike Johnson
--
-- Redistribution and use in source and synthezised forms, with or without
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
-- The latest version of this file can be found at: www.FPGAArcade.com
--
-- Email support@fpgaarcade.com
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity Electron_Top is
  port (
    ------------------------------------------------------
    -- To Lib
    ------------------------------------------------------

    -- System clock, enable and reset, generated from Clk A
    i_clk_sys             : in  bit1;
    i_ena_sys             : in  bit1;
    i_cph_sys             : in  word( 3 downto 0); -- four phased enables. (3) = ena_sys
    i_rst_sys             : in  bit1;

    i_clk_ram             : in  bit1;
    i_rst_ram             : in  bit1;

    -- Video clock, generated from Clk C. Used for all OSD Video/PHY data path.
    i_clk_vid             : in  bit1;
    i_rst_vid             : in  bit1;
    --
    -- Config/Control
    o_cfg_status          : out word(15 downto 0); -- status feedback to ARM
    i_cfg_static          : in  word(31 downto 0);
    i_cfg_dynamic         : in  word(31 downto 0);

    i_tick_1us            : in  bit1; -- on clk_sys with ena
    i_tick_100us          : in  bit1; -- on clk_sys with ena
    i_halt                : in  bit1;
    i_dram_ref_panic      : in  bit1;
    o_rst_soft            : out bit1 := '0';

    -- Joystick
    i_joy_a_l             : in  word( 5 downto 0);
    i_joy_b_l             : in  word( 5 downto 0);

    -- Keyboard
    o_kb_ps2_leds         : out word( 2 downto 0);
    i_kb_ps2_we           : in  bit1;
    i_kb_ps2_data         : in  word( 7 downto 0);
    i_kb_inhibit          : in  bit1; -- OSD active

    -- Fileio A
    i_fcha_cfg            : in  r_Cfg_fileio;
    i_fcha_to_core        : in  r_Fileio_to_core;
    o_fcha_fm_core        : out r_Fileio_fm_core;  -- connect to z_Fileio_fm_core if not used

    -- Fileio B
    i_fchb_cfg            : in  r_Cfg_fileio;
    i_fchb_to_core        : in  r_Fileio_to_core;
    o_fchb_fm_core        : out r_Fileio_fm_core;  -- connect to z_Fileio_fm_core if not used

    -- Fileio Mem
    i_memio_to_core       : in  r_Memio_to_core;
    o_memio_fm_core       : out r_Memio_fm_core;   -- connect to z_Memio_fm_core if not used

    -- Video (clk_vid)
    o_vid_rgb             : out word(23 downto 0);
    o_vid_sync            : out r_Vidsync;

    -- Audio (clk_aud)
    o_audio_l             : out word(23 downto 0); -- left  sample
    o_audio_r             : out word(23 downto 0); -- right sample
    i_audio_taken         : in  bit1;  -- sample ack

    ------------------------------------------------------
    -- Other IO
    ------------------------------------------------------
    o_disk_led            : out bit1;
    o_pwr_led             : out bit1  -- note these are active high outputs
    );
end;

architecture RTL of Electron_Top is

  signal led                    : bit1;
  signal tick_pre1              : bit1;
  signal tick                   : bit1;

begin

  o_cfg_status(15 downto  0) <= (others => '0');

  o_rst_soft            <= '0';
  o_kb_ps2_leds         <= "000";

  o_memio_fm_core       <= Z_Memio_fm_core;

  o_fcha_fm_core        <= z_Fileio_fm_core;
  o_fchb_fm_core        <= z_Fileio_fm_core;

  o_audio_l             <= (others => '0');
  o_audio_r             <= (others => '0');

  o_vid_rgb             <= (others => '0');

  u_VideoTiming : entity work.Replay_VideoTiming
    generic map (
      g_enabledynamic       => '0',
      g_param               => c_Vidparam_720x480p_60
      )
    port map (
      i_clk                 => i_clk_vid,
      i_ena                 => '1',
      i_rst                 => i_rst_vid,
      --
      i_param               => c_Vidparam_720x480p_60,
      i_sof                 => '0',
      i_f2_flip             => '0',
      --
      o_hactive             => open,
      o_hrep                => open,
      o_vactive             => open,
      --
      o_dig_hs              => o_vid_sync.dig_hs,
      o_dig_vs              => o_vid_sync.dig_vs,
      o_dig_de              => o_vid_sync.dig_de,
      o_dig_ha              => open,
      o_dig_va              => open,
      o_dig_sof             => open,
      o_dig_sol             => open,
      o_ana_hs              => o_vid_sync.ana_hs,
      o_ana_vs              => o_vid_sync.ana_vs,
      o_ana_de              => o_vid_sync.ana_de,
      --
      o_hpix                => open,
      o_vpix                => open,
      --
      o_f2                  => open,
      o_voddline            => open,
      o_stdprog             => open
      );

  -- some blinking LED thingy...
  b_tick : block
    signal precounter1 : word(15 downto 0);
    signal precounter2 : word(11 downto 0);
  begin

    p_count : process(i_clk_sys, i_rst_sys)
    begin
      if (i_rst_sys = '1') then
        precounter1 <= (others => '0');
        precounter2 <= (others => '0');
        tick_pre1   <= '0';
        tick        <= '0';
      elsif rising_edge(i_clk_sys) then

        if (i_ena_sys = '1') then
          precounter1 <= precounter1 - "1";

          tick_pre1 <= '0';
          if (precounter1 = x"0000") then
            tick_pre1 <= '1';
          end if;
          -- synopsys translate_off
          tick_pre1 <= '1';
          -- synopsys translate_on

          tick <= '0';
          if (tick_pre1 = '1') then
            if (precounter2 = x"000") then
              precounter2 <= x"19B";
              tick <= '1';
            else
              precounter2 <= precounter2 - "1";
            end if;
          end if;
        end if;
      end if;
    end process;
  end block;

  p_flash : process(i_clk_sys, i_rst_sys)
  begin
    if (i_rst_sys = '1') then
      led <= '0';
    elsif rising_edge(i_clk_sys) then
      if (i_ena_sys = '1') then
        if (tick = '1') then
          led  <= not led;
        end if;
      end if;
    end if;
  end process;

  o_disk_led        <=     led;
  o_pwr_led         <= not led;


  --
  -- Acorn Electron Specific
  --

  -- 32kB ROM
    
  -- 4x8K RAM

  -- T65 (6502-A)
    
  -- Keyboad

  -- ULA (Uncommitted Logic Array)
  -- Handles RAM, Video, Cassette and sound

  -- Cassette i/o wrapper

end RTL;

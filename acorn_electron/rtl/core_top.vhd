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
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_Core_Wrap_Pack.all;
  use work.Replay_Target_Pack.all; -- only if you need to know which target

--
-- This is an interface layer between the board support library, and the "core"
--
entity Core_Top is
  port (
    -- system
    i_si         : in  r_to_core := z_to_core;
    o_so         : out r_fm_core;
    -- external
    i_ei         : in  r_Pins_to_core := z_Pins_to_core;
    o_eo         : out r_Pins_fm_core
  );
end;

architecture RTL of Core_Top is
  -- this faff is so we can assign defaults to the complete output records
  signal si : r_to_core;
  signal so : r_fm_core := z_fm_core;

  signal ei : r_Pins_to_core;
  signal eo : r_Pins_fm_core := z_Pins_fm_core;
  --
  signal clk_sys                : bit1;
  signal ena_sys                : bit1;
  signal cph_sys                : word(3 downto 0);
  signal rst_sys                : bit1;

  signal pwr_led                : bit1;
  signal disk_led               : bit1;

  signal debug                  : word(15 downto 0);

begin
  si <= i_si; -- save typing
  ei <= i_ei;

  -- Sys clock used for system, audio and video
  clk_sys <= si.ctrl.clk_sys;
  ena_sys <= si.ctrl.ena_sys;
  cph_sys <= si.ctrl.cph_sys;
  rst_sys <= si.ctrl.rst_sys;

  so.ctrl.clk_aud <= clk_sys;
  so.ctrl.ena_aud <= ena_sys;
  so.ctrl.rst_aud <= rst_sys;

  so.ctrl.ena_vid <= '1';

  --
  -- CONFIG
  --
  so.ctrl.rst_soft   <= '0';

  --
  -- The Core
  --
  u_Core : entity work.Electron_Top
  port map (
    --
    i_clk_sys             => clk_sys,
    i_ena_sys             => ena_sys,
    i_cph_sys             => cph_sys,
    i_rst_sys             => rst_sys,

    --
    o_cfg_status          => so.cfg.cfg_status,
    i_cfg_static          => si.cfg.cfg_static,
    i_cfg_dynamic         => si.cfg.cfg_dynamic,

    i_halt                => si.ctrl.halt,

    --
    i_joy_a_l             => si.kbmsjoy.joy_a_l,
    i_joy_b_l             => si.kbmsjoy.joy_b_l,

    --
    o_kb_ps2_leds         => so.kbmsjoy.kb_ps2_leds,
    i_kb_ps2_we           => si.kbmsjoy.kb_ps2_we,
    i_kb_ps2_data         => si.kbmsjoy.kb_ps2_data,
    i_kb_inhibit          => si.kbmsjoy.kb_inhibit,

    --
    i_memio_to_core       => si.mem_a,
    o_memio_fm_core       => so.mem_a,

    --
    i_fcha_to_core        => si.io.fcha_to_core,
    o_fcha_fm_core        => so.io.fcha_fm_core,

    i_fchb_to_core        => si.io.fchb_to_core,
    o_fchb_fm_core        => so.io.fchb_fm_core,

    --
    i_cfgio_to_core       => si.io.cfg_to_core,
    o_cfgio_fm_core       => so.io.cfg_fm_core,

    --
    o_vid_rgb             => so.av.vid_rgb,
    o_vid_sync            => so.av.vid_sync,

    --
    o_audio_l             => so.av.audio_l,
    o_audio_r             => so.av.audio_r,
    i_audio_taken         => si.av.audio_taken,

    ------------------------------------------------------
    -- Other IO
    ------------------------------------------------------
    o_disk_led            => disk_led,
    o_pwr_led             => pwr_led,

    o_sound_op            => open, -- b_aux_io(6),
    o_debug               => debug(15 downto 0)
  );

  o_so <= so;

  g_R1 : if (c_Target = R1) generate
    eo.disk_led <= disk_led;
    eo.pwr_led  <= pwr_led;
  end generate;
  o_eo <= eo;

end RTL;

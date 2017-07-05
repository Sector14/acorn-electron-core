--
-- WWW.FPGAArcade.COM
--
-- REPLAY Retro Gaming Platform
-- No Emulation No Compromise
--
-- All rights reserved
-- Mike Johnson 2015
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
-- This file is licensed for use ONLY with Replay hardware from FPGAArcade.com
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity Replay_OSD is
  port (
    i_clk                 : in    bit1; -- video clock
    i_ena                 : in    bit1;
    i_rst                 : in    bit1;
    --
    i_clk_ctl             : in    bit1; -- used for i_ctl_*
    i_ctl_din             : in    word( 7 downto 0);
    i_ctl_char_addr       : in    word(10 downto 0);
    i_ctl_char_wena       : in    bit1;
    i_ctl_hoff_wena_1     : in    bit1;
    i_ctl_hoff_wena_2     : in    bit1;
    i_ctl_voff_wena       : in    bit1;
    --
    i_hstart              : in    bit1;
    i_vstart              : in    bit1;
    i_hexp                : in    bit1; -- expand x2
    i_vexp                : in    bit1;
    --
    i_stdprog             : in    bit1; -- low for interlaced, in which case V counts by 2
    i_voddline            : in    bit1; -- starting at 0 or 1 (VOddLine high)
    --
    i_osd_display         : in    bit1; -- turn on OSD
    i_osd_dim             : in    bit1; -- leave some background
    i_vid_rgb             : in    word(23 downto 0); -- r 23..16 g 15..8 b 7..0
    -- 1 clock delay vid in to vid out
    o_vid_rgb             : out   word(23 downto 0)
    );
end;

architecture RTL of Replay_OSD is

begin
  o_vid_rgb <= i_vid_rgb;
end RTL;


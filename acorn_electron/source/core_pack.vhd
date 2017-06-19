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

Package Core_Pack is
  constant c_clk_sys_divider      : natural := 28;   -- divide clk_sys down to 1MHz
  constant c_clk_ctl_for_video    : boolean := false;
  constant c_use_dram             : boolean := true; -- enabled DRAM block
  constant c_use_fileio           : boolean := true; -- enabled FILEIO block

  constant c_cfg_fileio_cha_ena   : word( 3 downto 0) := "0000"; -- FD
  constant c_cfg_fileio_cha_drv   : word( 3 downto 0) := "0000"; -- driver x
  constant c_cfg_fileio_chb_ena   : word( 3 downto 0) := "0000"; -- HD
  constant c_cfg_fileio_chb_drv   : word( 3 downto 0) := "0000"; -- driver x

  constant c_cfg_static           : word(31 downto 0) := x"00000000";
  constant c_cfg_dynamic          : word(31 downto 0) := x"00000000";
  constant c_version              : word(15 downto 0) := x"8000"; -- top bit must be set to enable DRAM

end;

package body Core_Pack is

end;

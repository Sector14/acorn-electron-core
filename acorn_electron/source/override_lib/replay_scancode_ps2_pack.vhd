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
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;

Package Replay_ScanCode_PS2_Pack is
                                                           -- extended
  -- keypad                                               |
  constant c_PS2_KP_0             : word (8 downto 0) := '0' & X"70";
  constant c_PS2_KP_1             : word (8 downto 0) := '0' & X"69";
  constant c_PS2_KP_2             : word (8 downto 0) := '0' & X"72";
  constant c_PS2_KP_3             : word (8 downto 0) := '0' & X"7A";
  constant c_PS2_KP_4             : word (8 downto 0) := '0' & X"6B";
  constant c_PS2_KP_5             : word (8 downto 0) := '0' & X"73";
  constant c_PS2_KP_6             : word (8 downto 0) := '0' & X"74";
  constant c_PS2_KP_7             : word (8 downto 0) := '0' & X"6C";
  constant c_PS2_KP_8             : word (8 downto 0) := '0' & X"75";
  constant c_PS2_KP_9             : word (8 downto 0) := '0' & X"7D";
  constant c_PS2_KP_DOT           : word (8 downto 0) := '0' & X"71";
  constant c_PS2_KP_ENTER         : word (8 downto 0) := '1' & X"5A";
  constant c_PS2_KP_SLASH         : word (8 downto 0) := '1' & X"4A";
  constant c_PS2_KP_STAR          : word (8 downto 0) := '0' & X"7C";
  constant c_PS2_KP_PLUS          : word (8 downto 0) := '0' & X"79";
  constant c_PS2_KP_MINUS         : word (8 downto 0) := '0' & X"7B";
  --
  constant c_PS2_LEFT_ALT         : word (8 downto 0) := '0' & X"11";
  constant c_PS2_RIGHT_ALT        : word (8 downto 0) := '1' & X"11";
  constant c_PS2_LEFT_CTRL        : word (8 downto 0) := '0' & X"14";
  constant c_PS2_RIGHT_CTRL       : word (8 downto 0) := '1' & X"14";
  constant c_PS2_LEFT_WIN         : word (8 downto 0) := '1' & X"1F";
  constant c_PS2_RIGHT_WIN        : word (8 downto 0) := '1' & X"27";
  constant c_PS2_LEFT_SHIFT       : word (8 downto 0) := '0' & X"12";
  constant c_PS2_RIGHT_SHIFT      : word (8 downto 0) := '0' & X"59";
  constant c_PS2_MENU             : word (8 downto 0) := '1' & X"2F";
  constant c_PS2_ENTER            : word (8 downto 0) := '0' & X"5A";
  constant c_PS2_ESC              : word (8 downto 0) := '0' & X"76";
  constant c_PS2_SPACE            : word (8 downto 0) := '0' & X"29";

  constant c_PS2_CAPS_LOCK        : word (8 downto 0) := '0' & X"58";
  constant c_PS2_NUM_LOCK         : word (8 downto 0) := '0' & X"77";
  constant c_PS2_SCROLL_LOCK      : word (8 downto 0) := '0' & X"7E";
  constant c_PS2_BREAK            : word (8 downto 0) := '1' & X"14";
  --
  constant c_PS2_A                : word (8 downto 0) := '0' & X"1C";
  constant c_PS2_B                : word (8 downto 0) := '0' & X"32";
  constant c_PS2_C                : word (8 downto 0) := '0' & X"21";
  constant c_PS2_D                : word (8 downto 0) := '0' & X"23";
  constant c_PS2_E                : word (8 downto 0) := '0' & X"24";
  constant c_PS2_F                : word (8 downto 0) := '0' & X"2B";
  constant c_PS2_G                : word (8 downto 0) := '0' & X"34";
  constant c_PS2_H                : word (8 downto 0) := '0' & X"33";
  constant c_PS2_I                : word (8 downto 0) := '0' & X"43";
  constant c_PS2_J                : word (8 downto 0) := '0' & X"3B";
  constant c_PS2_K                : word (8 downto 0) := '0' & X"42";
  constant c_PS2_L                : word (8 downto 0) := '0' & X"4B";
  constant c_PS2_M                : word (8 downto 0) := '0' & X"3A";
  constant c_PS2_N                : word (8 downto 0) := '0' & X"31";
  constant c_PS2_O                : word (8 downto 0) := '0' & X"44";
  constant c_PS2_P                : word (8 downto 0) := '0' & X"4D";
  constant c_PS2_Q                : word (8 downto 0) := '0' & X"15";
  constant c_PS2_R                : word (8 downto 0) := '0' & X"2D";
  constant c_PS2_S                : word (8 downto 0) := '0' & X"1B";
  constant c_PS2_T                : word (8 downto 0) := '0' & X"2C";
  constant c_PS2_U                : word (8 downto 0) := '0' & X"3C";
  constant c_PS2_V                : word (8 downto 0) := '0' & X"2A";
  constant c_PS2_W                : word (8 downto 0) := '0' & X"1D";
  constant c_PS2_X                : word (8 downto 0) := '0' & X"22";
  constant c_PS2_Y                : word (8 downto 0) := '0' & X"35";
  constant c_PS2_Z                : word (8 downto 0) := '0' & X"1A";

  constant c_PS2_0                : word (8 downto 0) := '0' & X"45";
  constant c_PS2_1                : word (8 downto 0) := '0' & X"16";
  constant c_PS2_2                : word (8 downto 0) := '0' & X"1E";
  constant c_PS2_3                : word (8 downto 0) := '0' & X"26";
  constant c_PS2_4                : word (8 downto 0) := '0' & X"25";
  constant c_PS2_5                : word (8 downto 0) := '0' & X"2E";
  constant c_PS2_6                : word (8 downto 0) := '0' & X"36";
  constant c_PS2_7                : word (8 downto 0) := '0' & X"3D";
  constant c_PS2_8                : word (8 downto 0) := '0' & X"3E";
  constant c_PS2_9                : word (8 downto 0) := '0' & X"46";

  constant c_PS2_FN_1             : word (8 downto 0) := '0' & X"05";
  constant c_PS2_FN_2             : word (8 downto 0) := '0' & X"06";
  constant c_PS2_FN_3             : word (8 downto 0) := '0' & X"04";
  constant c_PS2_FN_4             : word (8 downto 0) := '0' & X"0C";
  constant c_PS2_FN_5             : word (8 downto 0) := '0' & X"03";
  constant c_PS2_FN_6             : word (8 downto 0) := '0' & X"0B";
  constant c_PS2_FN_7             : word (8 downto 0) := '0' & X"83";
  constant c_PS2_FN_8             : word (8 downto 0) := '0' & X"0A";
  constant c_PS2_FN_9             : word (8 downto 0) := '0' & X"01";
  constant c_PS2_FN_10            : word (8 downto 0) := '0' & X"09";
  constant c_PS2_FN_11            : word (8 downto 0) := '0' & X"78";
  constant c_PS2_FN_12            : word (8 downto 0) := '0' & X"07";

  constant c_PS2_UP               : word (8 downto 0) := '1' & X"75";
  constant c_PS2_DOWN             : word (8 downto 0) := '1' & X"72";
  constant c_PS2_LEFT             : word (8 downto 0) := '1' & X"6B";
  constant c_PS2_RIGHT            : word (8 downto 0) := '1' & X"74";

end;

package body Replay_ScanCode_PS2_Pack is
end;


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

-- NOTE: This is a temporary file. 
-- As lib copies happen before core source copies during build, this
-- file will replace the library version allowing testing of a
-- new mode without risking breaking everything in svn.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;

Package Replay_VideoTiming_Pack is

  subtype int_14 is integer range 0 to 16383;

  -- note h constants are /2
  type r_Vidparam_int is record
    rep_h           : bit1;
    total_h         : int_14;
    active_h        : int_14;
    syncp_h         : int_14;
    syncw_h         : int_14;
    --
    total_v         : int_14;
    active_v        : int_14;
    --
    fline_f1_v      : int_14; -- first line in field 1 -1
    lline_f1_v      : int_14; -- last line in field 1
    fline_f2_v      : int_14; -- first line in field 2 -1
    lline_f2_v      : int_14; -- last line in field 2
    --
    start_f1_v      : int_14; -- first line in field 1 -1
    start_f2_v      : int_14; -- first line if field 2 -1
    --
    fsync_f1_v      : int_14; -- first vsync line -1
    lsync_f1_v      : int_14; -- last vsync line
    fsync_f2_v      : int_14; -- first vsync line -1
    lsync_f2_v      : int_14; -- last vsync line
    --
    syncpol_h       : bit1;   -- active low
    syncpol_v       : bit1;   -- active low
    progressive     : bit1;
  end record;

  -- PAL_P is a fake progressive resolution of 256 but
  -- still using two fields @50Hz.
  -- REVIEW: Better place to identify this?
  type t_Standard_ana is (PAL, PAL_P, NTSC, NONE);

  type r_Vidsync is record
    dig_de : bit1;
    dig_vs : bit1;
    dig_hs : bit1;
    ana_de : bit1;
    ana_vs : bit1;
    ana_hs : bit1;
  end record;

  constant z_Vidsync : r_Vidsync := (
    dig_de => '0',
    dig_vs => '0',
    dig_hs => '0',
    ana_de => '0',
    ana_vs => '0',
    ana_hs => '0'
  );

  type r_Vidtiming is record -- dynamic
    -- start of frame/line timing
    dig_act_h            : bit1;
    dig_act_v            : bit1;
    dig_sof              : bit1; -- just before active video
    dig_sol              : bit1; -- every line, hpix = 0
    -- current position
    h_pix                : word(13 downto 0);
    v_pix                : word(13 downto 0); -- frame line correct for i standards
    f2                   : bit1;              -- high for field 2 if not progressive
    v_oddline            : bit1;              -- same as vpix(0) for interlanced standards. High in F2 except for NTSC where high in F1
  end record;

  type r_Vidstd is record -- static
    -- current standard h/v size
    h_active         : word(13 downto 0); -- in clocks
    h_rep            : bit1;
    v_active         : word(13 downto 0);
    prog             : bit1;              -- (field & frame line number same if progressive)
  end record;

  -- note, in these numbers the front porch is at the start of the line, and
  -- the active video finishes at the end of the line
  -- e.g fline_f1_v 22 means first active video line for field one starts
  -- at the end of line 22.
  -- for SD standards 1 count = 37.037ns (using 27MHZ pixel clock)
  -- Timing Links:
  --   * http://martin.hinner.info/vga/pal.html
  --   * http://www.batsocks.co.uk/readme/video_timing.htm
  --{{{
  constant c_Vidparam_832x287p_50_16MHz : r_Vidparam_int := (
    -- 832x256p @ 50Hz 16MHz pclk
    rep_h           => '0',
    total_h         => 1024,
    active_h        => 832,
    syncp_h         => 26,
    syncw_h         => 75,
    --
    total_v         => 312,
    active_v        => 287,  -- two lines dropped per field
    --
    fline_f1_v      => 22,
    lline_f1_v      => 309,  -- one line lost
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 312,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 312,
    lsync_f1_v      => 3,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0', -- active low
    syncpol_v       => '0', -- active low
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_720x287p_50 : r_Vidparam_int := (
    -- 720(1440)x256p @ 50Hz 27MHz pclk
    rep_h           => '1',
    total_h         => 1728,
    active_h        => 1440,
    syncp_h         => 24,
    syncw_h         => 126,
    --
    total_v         => 312,
    active_v        => 287,  -- two lines dropped per field
    --
    fline_f1_v      => 22,
    lline_f1_v      => 309,  -- one line lost
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 312,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 312,
    lsync_f1_v      => 3,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0', -- active low
    syncpol_v       => '0', -- active low
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_720x576i_50 : r_Vidparam_int := (
    -- 720(1440)x576i @ 50Hz 27MHz pclk
    rep_h           => '1',
    total_h         => 1728,
    active_h        => 1440,
    syncp_h         => 24,  -- start of sync
    syncw_h         => 126, -- sync width
    --
    total_v         => 625,
    active_v        => 576,
    --
    fline_f1_v      => 22,
    lline_f1_v      => 310,
    fline_f2_v      => 335,
    lline_f2_v      => 623,
    --
    start_f1_v      => 623,
    start_f2_v      => 310,
    --
    fsync_f1_v      => 625,
    lsync_f1_v      => 3,
    fsync_f2_v      => 312,
    lsync_f2_v      => 315,
    --
    syncpol_h       => '0', -- active low
    syncpol_v       => '0', -- active low
    progressive     => '0'
  );
  --}}}
  --{{{
  constant c_Vidparam_720x576p_50 : r_Vidparam_int := (
    -- 720x576p @ 50Hz 27MHz pclk
    rep_h           => '0',
    total_h         => 864,
    active_h        => 720,
    syncp_h         => 12,
    syncw_h         => 64,
    --
    total_v         => 625,
    active_v        => 576,
    --
    fline_f1_v      => 44,
    lline_f1_v      => 620,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 625,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 625,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0', -- active low
    syncpol_v       => '0', -- active low
    progressive     => '1'
  );
  --}}}

  --{{{
  constant c_Vidparam_720x480i_60 : r_Vidparam_int := (
    -- 720(1440)x480i @ 59.94Hz 27MHz pclk
    rep_h           => '1',
    total_h         => 1716,
    active_h        => 1440,
    syncp_h         => 38,
    syncw_h         => 124,
    --
    total_v         => 525,
    active_v        => 480,
    --
    fline_f1_v      => 21,
    lline_f1_v      => 261,
    fline_f2_v      => 284,
    lline_f2_v      => 524,
    --
    start_f1_v      => 524,
    start_f2_v      => 261,
    --
    fsync_f1_v      => 3,
    lsync_f1_v      => 6,
    fsync_f2_v      => 265,
    lsync_f2_v      => 268,
    --
    syncpol_h       => '0',
    syncpol_v       => '0',
    progressive     => '0'
  );
  --}}}
  --{{{
  constant c_Vidparam_720x480p_60 : r_Vidparam_int := (
    -- 720x480p @ 59.94Hz 27MHz pclk
    rep_h           => '0',
    total_h         => 858,
    active_h        => 720,
    syncp_h         => 16,
    syncw_h         => 62,
    --
    total_v         => 525,
    active_v        => 480,
    --
    fline_f1_v      => 42,
    lline_f1_v      => 522,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 525,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 6,
    lsync_f1_v      => 12,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0',
    syncpol_v       => '0',
    progressive     => '1'
  );
  --}}}

  --{{{
  constant c_Vidparam_640x480p_60 : r_Vidparam_int := (
    -- 640x480p @ 60Hz 25.2MHz pclk
    rep_h           => '0',
    total_h         => 800,
    active_h        => 640,
    syncp_h         => 16,
    syncw_h         => 96,
    --
    total_v         => 525,
    active_v        => 480,
    --
    fline_f1_v      => 42,
    lline_f1_v      => 522,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 525,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 7,
    lsync_f1_v      => 9,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0',
    syncpol_v       => '0',
    progressive     => '1'
  );
  --}}}

  -- hidef
  --{{{
  constant c_Vidparam_1280x720p_50 : r_Vidparam_int := (
    -- 1280x720p @ 50Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 1980,
    active_h        => 1280,
    syncp_h         => 440,
    syncw_h         => 40,
    --
    total_v         => 750,
    active_v        => 720,
    --
    fline_f1_v      => 25,
    lline_f1_v      => 745,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 750,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 750,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_1280x720p_60 : r_Vidparam_int := (
    -- 1280x720p @ 60Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 1650,
    active_h        => 1280,
    syncp_h         => 110,
    syncw_h         => 40,
    --
    total_v         => 750,
    active_v        => 720,
    --
    fline_f1_v      => 25,
    lline_f1_v      => 745,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 750,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 750,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}

  --{{{
  constant c_Vidparam_1920x1080i_50 : r_Vidparam_int := (
    -- 1920x1080i @ 50Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 2640,
    active_h        => 1920,
    syncp_h         => 528,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 20,
    lline_f1_v      => 560,
    fline_f2_v      => 583,
    lline_f2_v      => 1123,
    --
    start_f1_v      => 1125,
    start_f2_v      => 560,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 562,
    lsync_f2_v      => 567,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '0'
  );
  --}}}
  --{{{
  constant c_Vidparam_1920x1080i_60 : r_Vidparam_int := (
    -- 1920x1080i @ 60Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 2200,
    active_h        => 1920,
    syncp_h         => 88,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 20,
    lline_f1_v      => 560,
    fline_f2_v      => 583,
    lline_f2_v      => 1123,
    --
    start_f1_v      => 1125,
    start_f2_v      => 560,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 562,
    lsync_f2_v      => 567,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '0'
  );
  --}}}

  --{{{
  constant c_Vidparam_1920x1080p_24 : r_Vidparam_int := (
    -- 1920x1080p @ 24Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 2750,
    active_h        => 1920,
    syncp_h         => 638,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 41,
    lline_f1_v      => 1121,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1125,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_1920x1080p_25 : r_Vidparam_int := (
    -- 1920x1080p @ 25Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 2640,
    active_h        => 1920,
    syncp_h         => 528,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 41,
    lline_f1_v      => 1121,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1125,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_1920x1080p_30 : r_Vidparam_int := (
    -- 1920x1080p @ 30Hz 74.25MHz pclk
    rep_h           => '0',
    total_h         => 2640,
    active_h        => 1920,
    syncp_h         => 528,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 41,
    lline_f1_v      => 1121,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1125,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}

  --{{{
  constant c_Vidparam_1920x1080p_50 : r_Vidparam_int := (
    -- 1920x1080p @ 50Hz 148.5MHz pclk
    rep_h           => '0',
    total_h         => 2640,
    active_h        => 1920,
    syncp_h         => 528,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 41,
    lline_f1_v      => 1121,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1125,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_1920x1080p_60 : r_Vidparam_int := (
    -- 1920x1080p @ 60Hz 148.5MHz pclk
    rep_h           => '0',
    total_h         => 2200,
    active_h        => 1920,
    syncp_h         => 88,
    syncw_h         => 44,
    --
    total_v         => 1125,
    active_v        => 1080,
    --
    fline_f1_v      => 41,
    lline_f1_v      => 1121,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1125,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1125,
    lsync_f1_v      => 5,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}

  -- vesa
  --
  --{{{
  constant c_Vidparam_1024x768_60 : r_Vidparam_int := (
    -- 1024x768  @ 60Hz 65MHz pclk
    -- says 766 on the screen!!! note
    rep_h           => '0',
    total_h         => 1344,
    active_h        => 1024,
    syncp_h         => 24,
    syncw_h         => 136,
    --
    total_v         => 806,
    active_v        => 768,
    --
    fline_f1_v      => 39,
    lline_f1_v      => 806,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 806,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 2,
    lsync_f1_v      => 9,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0',
    syncpol_v       => '0',
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_1280x1024_60 : r_Vidparam_int := (
    -- 1280x1024 @ 60Hz 108MHz pclk
    rep_h           => '0',
    total_h         => 1688,
    active_h        => 1280,
    syncp_h         => 48,
    syncw_h         => 112,
    --
    total_v         => 1066,
    active_v        => 1024,
    --
    fline_f1_v      => 42,
    lline_f1_v      => 1066,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1066,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1,
    lsync_f1_v      => 4,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '1',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}
  --{{{
  constant c_Vidparam_1680x1050_60 : r_Vidparam_int := (
    -- 1680x1050 @ 60Hz 147.14MHz pclk
    rep_h           => '0',
    total_h         => 2256,
    active_h        => 1680,
    syncp_h         => 104,
    syncw_h         => 184,
    --
    total_v         => 1087,
    active_v        => 1050,
    --
    fline_f1_v      => 37,
    lline_f1_v      => 1087,
    fline_f2_v      => 0,
    lline_f2_v      => 0,
    --
    start_f1_v      => 1087,
    start_f2_v      => 0,
    --
    fsync_f1_v      => 1,
    lsync_f1_v      => 4,
    fsync_f2_v      => 0,
    lsync_f2_v      => 0,
    --
    syncpol_h       => '0',
    syncpol_v       => '1',
    progressive     => '1'
  );
  --}}}

end;

package body Replay_VideoTiming_Pack is
end;


-- Disabled OSD to save a little BRAM until core ROM/RAM moved to DDR
-- If this needs re-enabling for any reason, the RAM in tm4164ea3_64k_w4
-- can be reduced to a few k. Whilst the core will no longer be functional
-- the rom upload/verify should still be testable.

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


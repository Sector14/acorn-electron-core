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

-- TODO: Electron uses interlaced 312.5 fields but there may be a quirk
--       due to the appearance of four partial pulses that add up to two
--       full scanlines and are split either side of vsync. This _might_
--       be another way to do a pseudo progressive display whilst retaining
--       all 625 lines.

-- Horizontal:
--   Standard is hs=4.7us, fp=1.65us, bp=5.7us
--   Measurements indicate border+fp = 8us, hs=4us and bp+border=12us
--   Assuming a border of 6us gives the following:
--     hs=4us (64px), fp=2us (32px), bp=6us (96px)
--     borders=6us (96px), active=40us (640px)
--   Note: hs+fp+bs must be >= 12us
--
-- Vertical: 
--   Standard is 625 lines, two fields 312.5 lines each with 2nd field 1/2 scanline offset.  
--   Measurements indicate 1/2 scanline offset is not used. Vsync instead occurs within
--   hsync pulses with a partial offset per field.
--   Example mode 6 (sl = 64us scanlines)
--     Fn   = vysnc 2.5sl, partial 11us, 28sl, 250sl active, 31sl, partial 17us
--     Fn+1 = vsync 2.5sl, partial 43us, 28sl, 250sl active, 31sl, partial 49us 
--   Partials either side of vsync when added = 60us making a full scanline once 4us hs accounted for.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity ULA_DISPLAY_LOGIC is
  port (      
      i_clk                   : in bit1;
      i_ena                   : in bit1;
      i_rst                   : in bit1;

      i_ck_s1m                : in bit1; -- 1MHz enable
      i_ck_s1m2               : in bit1; -- 0.5MHz enable

      -- Graphics mode (0,1,2,4,5)
      i_gmode                 : in bit1;

      -- Sync
      o_hsync                 : out bit1;
      o_vsync                 : out bit1;
      o_csync                 : out bit1

      -- Interrupts
      --o_rtc                 : out bit1;
      --o_dispend             : out bit1;

      --o_pause_cpu           : out bit1;
      
      -- o_bline
      -- o_blank
      -- o_cntwh

      -- TODO: Electron display logic didn't output hpix/vpix, not sure where that
      --       was tracked. Handle with separate process that gens same as replay did
      --       with 0..1023 h range and 0..625 v?
  );
end;

architecture RTL of ULA_DISPLAY_LOGIC is
  signal hsync : bit1;
  signal vsync, vsync_l : bit1;

  -- hsync [2-6]
  signal hsync_cnt : unsigned(4 downto 0);
  signal vsync_cnt : unsigned(9 downto 0);
begin

  o_csync <= hsync or vsync_l;
  o_hsync <= hsync;
  o_vsync <= vsync_l;

  p_hsync : process(i_clk, i_ena, i_rst)
  begin
    if i_rst = '1' then
      hsync <= '0';
      hsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_ena = '1' and i_ck_s1m2 = '1' then
        hsync <= '0';

        -- 4us pulse
        if hsync_cnt = 26 or hsync_cnt = 27 then
          hsync <= '1';
        end if;

        hsync_cnt <= hsync_cnt + 1;

      end if;
    end if;
  end process;


  p_vsync : process(i_clk, i_ena, i_rst)
  begin
    if i_rst = '1' then
      vsync <= '0';
      vsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      -- TODO: [Gary] Review schematics, what causes the 1us extra delay on Electron?  
      --       Propagation delay from ripple counter and vsync reset/ff?
      --       The 1us early triggering causes partials to be 42,50 & 10,18us rather
      --       than 43,49 & 11,17us. Difference is unlikely to matter.
      -- if i_ena = '1' and i_ck_s1m = '1' then
      --   vsync_l <= vsync;
      -- end if;

      if i_ena = '1' and i_ck_s1m2 = '1' then       
        vsync <= '0';

        if vsync_cnt >= 620 and vsync_cnt <= 624 then
          -- TODO: [Gary] really VReset occurs and generates 2.5 or 3 lines? of vsync
          vsync <= '1';
        end if;

        -- 31.25kHZ enable from hsync counter (falling edge of hsync_cnt(3))
        if hsync_cnt = 0 or hsync_cnt = 16 then
          if vsync_cnt = 624 then
            vsync_cnt <= (others => '0');
          else
            vsync_cnt <= vsync_cnt + 1; 
          end if;
        end if;

      end if;
    end if;
  end process;

end;

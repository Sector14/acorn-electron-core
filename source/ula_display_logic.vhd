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

-- Horizontal:
--   Standard is hs=4.7us, fp=1.65us, bp=5.7us
--   Electron uses:
--     hs=4us (64px), fp=2us (32px), bp=6us (96px)
--     borders=6us (96px), active=40us (640px)
--   Note: hs+fp+bs must be >= 12us
--
-- Vertical: 
--   Standard is 625 lines, two fields 312.5 lines each with 2nd field 1/2 scanline offset.  
--   Measurements indicate 1/2 scanline offset is not used. Vsync instead occurs within
--   regular timed hsync pulses causing a partial offset per field.
--   Example mode 6 (sl = 64us scanlines)
--     Fn   = vysnc 2.5sl, partial 11us, 28sl, 250sl active, 31sl, partial 17us
--     Fn+1 = vsync 2.5sl, partial 43us, 28sl, 250sl active, 31sl, partial 49us 
--   Partials either side of vsync when added = 60us making a full scanline once 4us hs accounted for.
--
-- Width of partials in this implementation differs slightly from the Electron.
-- 12us & 16us for one field and 44us & 48us for the next.

-- Compatible Mode
--
-- As the "authentic" PAL signal used by the Electron is:
--   H 15.625kHz, 50 Hz, 832x576 as two 312.5 line fields.
-- Monitors with a HDMI connection will detect this and try to display it as
--   720x576 @25Hz with a 13.4MHz pixel clock
-- which results in a flickering mess of a display. With scan doubler, no signal is
-- detected at all.
--
-- Compatible mode drops 1/2 a scanline from the end of each field resulting in a
-- pseudo progressive display when coupled with scan doubler of:
--   H 31.25kHz, V 50Hz, 720x576 with two 312 line fields.
-- HDMI/VGA will display this as a flicker free
--   720x576 @ 50.2Hz with a 27MHz pixel clock.
-- Downside of this is the core operates every so slightly faster as RTC/DispEnd
-- interrupt timing is based on scanlines and 1 line is now lost per frame. This 
-- results in the core running about 1 second faster per 5 minutes run time.
-- Timing difference: 100.197Hz compatible vs 100.038Hz in authentic mode.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity ULA_DISPLAY_LOGIC is
  port (      
      i_clk                   : in bit1;
      i_ena                   : in bit1;
      i_rst                   : in bit1;

      i_ck_s1m                : in bit1; -- 1MHz enable
      i_ck_s1m2               : in bit1; -- 0.5MHz enable

      i_compatible            : in boolean;

      -- Graphics mode 0,1,2,4,5
      i_gmode                 : in boolean;

      -- Sync
      o_hsync                 : out bit1;
      o_vsync                 : out bit1;
      o_csync                 : out bit1;

      -- Interrupts
      o_rtc                 : out boolean;
      o_dispend             : out boolean;
      
      o_bline               : out boolean;  -- end of 8/10 block of lines based on gfx mode
      o_addint              : out boolean;  -- start of new fields active data
      
      o_n_pcpu              : out boolean;  -- Low when cpu can process regardless of mode contention
      o_n_blank             : out boolean;  -- n_pcpu sync'd to 1MHz
      o_cntinh              : out boolean;  -- high during sync or border regions of scanline

      -- represents VA1,VA2,VA3
      o_rowcount            : out integer range 0 to 10;

      o_de                  : out bit1
  );
end;

architecture RTL of ULA_DISPLAY_LOGIC is
  signal hsync : bit1;
  signal vsync : bit1;

  -- hsync [2-6]
  signal hsync_cnt : unsigned(4 downto 0);
  -- vsync [6-15] in half scanlines
  signal vsync_cnt : unsigned(9 downto 0);

  signal vid_row_count : integer range 0 to 10;

  signal lsff2 : boolean;

  signal cntinh  : boolean;
  signal dispend : boolean;
  signal n_pcpu  : boolean;
begin
  o_csync <= hsync or vsync;
  o_hsync <= hsync;
  o_vsync <= vsync;

  o_de <= '1' when vsync_cnt < 576 and not cntinh and not dispend else '0';

  o_dispend <= dispend;
  o_n_pcpu <= n_pcpu;
  o_cntinh <= cntinh;

  o_rowcount <= vid_row_count;

  p_hsync : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      hsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_ena = '1' and i_ck_s1m2 = '1' then        
        hsync_cnt <= hsync_cnt + 1;
      end if;
    end if;
  end process;

  hsync <= '1' when hsync_cnt = 24 or hsync_cnt = 25 else '0';
  vsync <= '1' when vsync_cnt >= 564 and vsync_cnt <= 568 else '0';

  p_vsync : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      vsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_ena = '1' then

        if i_ck_s1m2 = '1' then       
          
          -- 31.25kHZ enable from hsync counter (falling edge of hsync_cnt(3))
          if hsync_cnt = 31 or hsync_cnt = 15 then
            if (i_compatible and vsync_cnt = 623) or (vsync_cnt = 624) then
              vsync_cnt <= (others => '0');
              lsff2 <= false;
            else
              vsync_cnt <= vsync_cnt + 1; 
            end if;
          end if;

          -- Falling edge of hsync
          if hsync_cnt = 25 then
            lsff2 <= true;
          end if;
        end if;

      end if;
    end if;
  end process;


  -- vid row count
  p_vid_row : process(i_clk, i_rst) 
  begin
    if i_rst = '1' then
      vid_row_count <= 0;
    elsif rising_edge(i_clk) then
      if i_ena = '1' and i_ck_s1m2 = '1' then

        -- row count "clocked" by LS falling edge i.e end of hsync
        if hsync_cnt = 25 then
          if (vid_row_count = 9) or (vid_row_count = 7 and i_gmode) then
            vid_row_count <= 0;
          else
            vid_row_count <= vid_row_count + 1;
          end if;
        end if;

        -- forced reset on start of new field
        if not lsff2 then
          vid_row_count <= 0;
        end if;

      end if;
    end if;
  end process;

  o_bline <= (vid_row_count = 9) or (vid_row_count = 7 and i_gmode);

  -- Low when cpu can process regardless of mode contention
  n_pcpu <= vid_row_count < 8 and not cntinh and not dispend;

  -- TODO: [Gary] Schematics show this as sync'd to 1MHz clock but that
  --       offsets rgb by 1us later than it should be?
  cntinh <= hsync_cnt >= 20;

  p_inactive_video : process(i_clk, i_rst) 
  begin
    if i_rst = '1' then
      o_n_blank <= false;
    elsif rising_edge(i_clk) then
      if i_ena = '1' then
        if i_ck_s1m = '1' then
          -- pcpu syncrhonised to 1MHz
          o_n_blank <= n_pcpu;
        end if;
      end if;
    end if;
  end process;


  p_display_interrupts : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      o_rtc <= false;
      dispend <= false;
      o_addint <= false;
    elsif rising_edge(i_clk) then
      if i_ena = '1' then

        if i_ck_s1m2 = '1' then          
          o_rtc <= false;

          -- LSFF2 ensures reset occurs once after vcnt reset only during none active video LSN2
          if not lsff2 and hsync_cnt >= 20 then
            dispend <= false;
          end if;

          -- Dispend aligned to hsync leading edge
          if hsync_cnt = 23 or hsync_cnt = 24 then
            -- DISPg0 range [500,503]
            if not i_gmode and (vsync_cnt >= 500 and vsync_cnt <= 503) then
              dispend <= true;
            end if;

            -- DISPg1 range [512,625)
            if i_gmode and vsync_cnt >= 512 then
              dispend <= true;
            end if;
          end if;

          -- RTC [200,207]          
          if vsync_cnt >= 200 and vsync_cnt <= 207 then
            o_rtc <= true;
          end if;

          -- Start of new active display vcnt 0
          if (hsync_cnt >= 7 and hsync_cnt <= 14) or (hsync_cnt >= 23) then
            o_addint <= false;
          end if;

          if (hsync_cnt = 31 or hsync_cnt = 15) then
            if (i_compatible and vsync_cnt = 623) or (vsync_cnt = 624) then
              o_addint <= true;
            end if;
          end if;

        end if;
      end if;
    end if;
  end process;

end;

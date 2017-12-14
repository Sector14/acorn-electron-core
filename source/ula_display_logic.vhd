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

      -- Graphics mode 0,1,2,4,5
      i_gmode                 : boolean;

      -- Sync
      o_hsync                 : out bit1;
      o_vsync                 : out bit1;
      o_csync                 : out bit1;

      -- Interrupts
      o_rtc                 : out boolean;
      o_dispend             : out boolean;

      -- TODO: Generate following signals and adjust ULA to use these instead of the
      --       existing timing which was based on hpix/vpix.
      --o_pause_cpu           : out bit1;
      
      -- o_bline  -- 2 lines of blanking every 8?
      -- o_blank  -- border blanking?
      -- o_cntwh

      -- TODO: Electron display logic didn't output hpix/vpix, not sure where that
      --       was tracked. Handle with separate process that gens same as replay did
      --       with 0..1023 h range and 0..625 v? and temp export from here for now?
      -- These are temporary.
      -- hpix is range 0..831, allows 96px left border, 640px display and 96px right border.
      -- vpix is range 0..312 where 312 will only be active for 1/2 a scanline. Two fields
      -- of 312.5 lines.
      o_hpix                : out unsigned(13 downto 0);
      o_vpix                : out unsigned(13 downto 0);
      o_de                  : out bit1
  );
end;

architecture RTL of ULA_DISPLAY_LOGIC is
  signal hsync : bit1;
  signal vsync, vsync_l : bit1;

  -- hsync [2-6]
  signal hsync_cnt : unsigned(4 downto 0);
  -- vsync [6-15] in half scanlines
  signal vsync_cnt : unsigned(9 downto 0);

  -- Temp counts
  signal hpix_total, vpix_total, hpix, vpix : unsigned(13 downto 0);
begin

  o_csync <= hsync or vsync_l;
  o_hsync <= hsync;
  o_vsync <= vsync_l;

  o_hpix <= hpix;
  o_vpix <= vpix;

  p_hsync : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      hsync <= '0';
      hsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_ena = '1' and i_ck_s1m2 = '1' then

        hsync <= '0';

        -- 4us pulse (covers hsync active during 26 & 27)
        if hsync_cnt = 25 or hsync_cnt = 26 then
          hsync <= '1';
        end if;

        hsync_cnt <= hsync_cnt + 1;

      end if;
    end if;
  end process;

  -- TODO: Pipeline hsync so it can be used in other processes whilst retaining alignment?

  p_vsync : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      vsync <= '0';
      vsync_l <= '0';
      vsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_ena = '1' then

        if i_ck_s1m = '1' then
          -- delay by 1us to match Electron's partial scanlines either side of vsync
          vsync_l <= vsync;
        end if;

        if i_ck_s1m2 = '1' then       
          vsync <= '0';         

          -- TODO: VReset occurs during 564-567 which holds a 2 bit counter in reset.
          -- This causes vsync to occuring during 2bit counts "00", "10" and "01" where
          -- the state 00 lasts 2 lines. Although seems to amount to 3.5 rather than 2.5
          -- scanlines? due to [564,567] range? Using 564-568 = 5 count = 2.5 lines instead.
          -- [562,566] gives a 31+p, vs, p+28 lines after/before active video for each field.
          -- Expected to use 563 with changing occuring following tick?
          if vsync_cnt >= 563 and vsync_cnt <= 567 then
            vsync <= '1';
          end if;

          -- 31.25kHZ enable from hsync counter (falling edge of hsync_cnt(3))
          -- TODO: Should vsync line counts be done separate to this for display purposes
          --       so the reset can be done in sync with hsync despite this being offset by
          --       partials? ATM getting partial end line for one frame and partial start
          --       line for next?
          -- TODO: Should be 31 and 15? but doing so aligns inc of vcnt to 224 instead of
          --       192 (ie 32fp, 64hs, 96bp = 192)
          if hsync_cnt = 31 or hsync_cnt = 15 then
            if vsync_cnt = 624 then
              vsync_cnt <= (others => '0');              
            else
              vsync_cnt <= vsync_cnt + 1; 
            end if;
          end if;
        end if;

      end if;
    end if;
  end process;

  p_display_interrupts : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      o_rtc <= false;
      o_dispend <= false;
    elsif rising_edge(i_clk) then
      if i_ena = '1' then
        if i_ck_s1m2 = '1' then       

          o_rtc <= false;

          -- TODO: [Gary] Reset for this occurred on vcnt 0 with LSN1 & LSN2
          --       as it's used for pausing cpu too. Handle that as separate signal instead?
          o_dispend <= false;
                  
          -- DISPg0 range [500,503], DISPg1 range [512,625) isr clocked by falling edge.
          -- Aligned to hsync leading edge.
          if hsync_cnt = 24 then
            if not i_gmode and (vsync_cnt >= 500 and vsync_cnt <= 503) then
              o_dispend <= true;
            end if;

            -- TODO: [Gary] Check duration of dispend for graphics modes.
            -- > 512 may be usable once ula switched to leading edge test.
            if i_gmode and vsync_cnt >= 512 and vsync_cnt <= 515 then
              o_dispend <= true;
            end if;
          end if;

          -- Real RTC is high for range [200,207], isr clocked by falling edge
          -- TODO: [Gary] adjust to 200-207?
          if vsync_cnt = 200 then
            o_rtc <= true;
          end if;

        end if;
      end if;
    end if;
  end process;

  -- TEMPORARY: will be replaced once further ULA logic has been deciphered
  --            from the schematics. This section bears little resemblance to the real ULA.
  p_active_pixels : process(i_clk, i_rst)
    variable hack_waithsync : boolean;
  begin
    if i_rst = '1' then
      vpix <= (others => '0');
      hpix <= (others => '0');
      hpix_total <= (others => '0');      
      o_de <= '0';

      hack_waithsync := false;
    elsif rising_edge(i_clk) then
      if i_ena = '1' then

        o_de <= '0';

        -- vpix 0 is first _active_ line of 288. 
        -- 250 (500) to 256 (512) lines (1/2 lines) active.
        -- Due to 312.5 lines, the reset at the end of each field from 624->0
        -- will mean video can render on 0-1 on one field but needs delaying to be
        -- 1-2 on the next, otherwise a partial line of data will be output then
        -- interrupted by hsync.
        if i_ck_s1m2 = '1' and vsync_cnt = 624 and hsync_cnt = 15 then
          hack_waithsync := true;
        end if;

        -- TODO: [Gary] numbers will be wrong with this with 0 being skipped every
        --       other field as far as data output goes, but ula shouldn't really use
        --       vpix directly although it is currently.
        if hack_waithsync then
          -- Delay switching to line 0 until next hsync to bring both fields into alignment
          -- for vsync->partial pulse->28 blank lines->line 0 of data.
          vpix <= to_unsigned(312, 14);
          if (hsync_cnt = 24 and i_ck_s1m2 = '1') then
            hack_waithsync := false;
            vpix <= (others => '0');
          end if;
        else
          vpix <= "00000" & vsync_cnt(9 downto 1);
        end if;
        
        -- reset on end of hsync
        if (hsync_cnt = 24 and i_ck_s1m2 = '1') then
          hpix_total <= (others => '0');
          hpix <= (others => '0');
        elsif (hsync_cnt = 14 or hsync_cnt = 30) and vsync_cnt = 624 and i_ck_s1m2 = '1' then
          -- Just done line 312.5 eg the 1/2 scanline. Adjust hpix counters for next field due to 1/2 offset
          hpix_total <= to_unsigned(192,14);
        else
          hpix_total <= hpix_total + 1;
        end if;

        -- hsync 4us (64px), 
        -- bp 6us (96px), border 6us (96px)
        -- active 40us (640px)
        -- border 6us (96px), fp 2us (32px)
        -- hpix_total with fp+hs+bp = 192.
        if (hsync_cnt = 14 or hsync_cnt = 30) and vsync_cnt = 624 and i_ck_s1m2 = '1' then
          hpix <= (others => '0');
        elsif hpix_total >= 192 and hpix_total < 1023 then
          hpix <= hpix + 1;
        else
          hpix <= (others => '0');
        end if;

        -- Line 0 is first visible display line.
        if vsync_cnt < 576 and hpix_total >= 192 and hpix_total < 1023 then
          o_de <= '1';
        end if;

      end if;
    end if;
  end process;

end;

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

      
      o_bline               : out boolean;  -- end of 8/10 block of lines based on gfx mode
      o_addint              : out boolean;  -- start of new fields active data
      
      -- o_pcpu             : out boolean;
      o_blank               : out boolean;
      o_cntwh               : out boolean;  -- high during sync or border regions of scanline

      -- represents VA1,VA2,VA3
      o_rowcount            : out integer range 0 to 10;

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

  signal vid_row_count : integer range 0 to 10;

  signal lsff2 : boolean;

  signal cntwh : boolean;
  signal dispend : boolean;
  signal pcpu : boolean;

  -- Temp counts
  signal hpix_total, vpix_total, hpix, vpix : unsigned(13 downto 0);
begin
  -- TODO: [Gary] Will need to latch vsync to delay by 1us to better match Electron's
  --       partial pulses either side of vsync. 
  o_csync <= hsync or vsync;
  o_hsync <= hsync;
  o_vsync <= vsync;

  o_dispend <= dispend;
  --o_pcpu <= pcpu;
  o_cntwh <= cntwh;

  o_rowcount <= vid_row_count;

  p_hsync : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      hsync <= '0';
      hsync_cnt <= (others => '0');
    elsif rising_edge(i_clk) then
      if i_ena = '1' and i_ck_s1m2 = '1' then
        
        hsync <= '0';
        -- Hsync appears to need to occur during 24 and 25 rather than 26-27 as schematics
        -- appear to suggest? If interpret schematics to use 25, it doesn't look like 
        -- hsync could be 4us, instead it's one clock cycle or 2us which is certainly wrong.
        -- There doesn't look to be a way for 24-25 to be used via current schematics unlike
        -- 26-27. But that would throw otu rest of sync pulses if active 640 pixels are meant
        -- to be from hcnt 0??

        -- 4us pulse (covers hsync active during 25 & 26)
        if hsync_cnt = 23 or hsync_cnt = 24 then
          hsync <= '1';
        end if;

        hsync_cnt <= hsync_cnt + 1;

      end if;
    end if;
  end process;

  -- TODO: VReset occurs during 564-567 which holds a 2 bit counter in reset.
  -- This causes vsync to occuring during 2bit counts "00", "10" and "01" where
  -- the state 00 lasts 2 lines. Although seems to amount to 3.5 rather than 2.5
  -- scanlines? due to [564,567] range? Using 564-568 = 5 count = 2.5 lines instead.
  -- [562,566] gives a 31+p, vs, p+28 lines after/before active video for each field.
  -- Expected to use 563 with changing occuring following tick?
  vsync <= '1' when vsync_cnt >= 564 and vsync_cnt <= 568 else '0';

  p_vsync : process(i_clk, i_rst)
  begin
    if i_rst = '1' then
      vsync_l <= '0';
      vsync_cnt <= (others => '0');
      
    elsif rising_edge(i_clk) then
      if i_ena = '1' then

        if i_ck_s1m = '1' then
          -- delay by 1us to match Electron's partial scanlines either side of vsync
          vsync_l <= vsync;
        end if;

        if i_ck_s1m2 = '1' then       
          
          -- 31.25kHZ enable from hsync counter (falling edge of hsync_cnt(3))
          if hsync_cnt = 31 or hsync_cnt = 15 then
            if vsync_cnt = 624 then
              vsync_cnt <= (others => '0');
              lsff2 <= false;
            else
              vsync_cnt <= vsync_cnt + 1; 
            end if;
          end if;

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
          -- Real ULA
          --   resets to 0 on vid cnt = 9 if not i_gmode
          --   resets to 8 on vid cnt = 15
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

  -- TODO: [Gary] What does this actually represent. Originally thought it would be used
  --       as part of the contention logic to pause the cpu, however 
  --       due to inclusion of gmode (0,1,2,4,5) when contention is only considered in modes
  --       0-3, this can't be the case. blankb and pcpub would be always '0' during
  --       graphics modes whilst in modes 3 & 6 they'd go high during the 2 blanking lines?
  --       unless it's used as an extra signal to re-enable processing when contention was
  --       otherwise considered to be active?
  pcpu <= vid_row_count >= 8 or cntwh or dispend or (not i_gmode);

  -- TODO: Schematics show this as been sync'd to 1MHz clock but that
  --       offsets rgb by 1us later than it should be?
  cntwh <= hsync_cnt >= 20;

  p_inactive_video : process(i_clk, i_rst) 
  begin
    if i_rst = '1' then
      --cntwh <= false;
    elsif rising_edge(i_clk) then
      if i_ena = '1' then
        if i_ck_s1m = '1' then
          
          -- cntwh <= false;
          
          -- -- 20-22 = border, 23 = front porch, 24-25 = hsync, 26-28 = bp, 29-31 = border
          -- -- LSN2 >= 20 i.e 12 cycles (24us) sync+border duration of a single scanline
          -- if hsync_cnt >= 20 then
          --   cntwh <= true;
          -- end if;

          -- TODO: [Gary] Investigate where the non synchronised pcpu is used and why.
          --              looks to be on the master timing sheet, for allowing processing when true??
          --              schematic had pcpub (active low)
          --              where does blank then fit in? Doesn't seem to be used anywhere?

          -- pcpu syncrhonised to 1MHz
          o_blank <= pcpu;
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
            if not i_gmode and (vsync_cnt >= 499 and vsync_cnt <= 502) then
              dispend <= true;
            end if;

            -- DISPg1 range [512,625)
            if i_gmode and vsync_cnt >= 511 then
              dispend <= true;
            end if;
          end if;

          -- RTC
          if vsync_cnt >= 199 and vsync_cnt <= 206 then
            o_rtc <= true;
          end if;

          -- Start of new active display vcnt 0
          o_addint <= false;
          if (hsync_cnt = 31 or hsync_cnt = 15) and vsync_cnt = 624 then
            o_addint <= true;             
          end if;

        end if;
      end if;
    end if;
  end process;



  -- TEMPORARY: will be replaced once further ULA logic has been deciphered
  --            from the schematics. This section bears little resemblance to how the
  --            real ULA tracks data for video addressing.
  p_active_pixels : process(i_clk, i_rst)
    variable hack_waithsync : boolean;
  begin
    if i_rst = '1' then
      vpix <= (others => '0');
      hpix <= (others => '0');
      hpix_total <= (others => '0');      

      hack_waithsync := false;
    elsif rising_edge(i_clk) then
      if i_ena = '1' then

        -- vpix 0 is first _active_ line of 288. 
        -- 250 (500) to 256 (512) lines (1/2 lines) active.
        -- Due to 312.5 lines, the reset at the end of each field from 624->0
        -- will mean video can render on 0-1 on one field but needs delaying to be
        -- 1-2 on the next, otherwise a partial line of data will be output then
        -- interrupted by hsync.
        if i_ck_s1m2 = '1' and vsync_cnt = 624 and hsync_cnt = 15 then
          hack_waithsync := true;
        end if;

        if hack_waithsync then
          -- Delay switching to line 0 until next hsync to bring both fields into alignment
          -- for vsync->partial pulse->28 blank lines->line 0 of data.
          -- vpix will only be "accurate" for the start of each scanline and ULA really
          -- shouldn't be using this value directly, but using blanking signals.
          vpix <= to_unsigned(312, 14);
          if (hsync_cnt = 23 and i_ck_s1m2 = '1') then
            hack_waithsync := false;
            vpix <= (others => '0');
          end if;
        else
          vpix <= "00000" & vsync_cnt(9 downto 1);
        end if;
        
        -- reset after back porch and border, just as active video begins
        --if (hsync_cnt = 24 and i_ck_s1m2 = '1') then
        if (hsync_cnt = 0 and i_ck_s1m = '1') and cntwh then
          hpix_total <= (others => '0');
          hpix <= (others => '0');
        else
          hpix_total <= hpix_total + 1;
        
          -- TODO: [Gary] is hpix 0 needs to be first active pixel. 
          -- So 640 then border then sync then border, only really needs to count
          -- 0..639 then? and tbh hpix may not be required at all anymore.

          -- hsync 4us (64px), 
          -- bp 6us (96px), border 6us (96px)
          -- active 40us (640px)
          -- border 6us (96px), fp 2us (32px)
          -- hpix_total with fp+hs+bp = 192.
          if hpix_total >= 192 and hpix_total < 1023 then
            hpix <= hpix + 1;
          else
            hpix <= (others => '0');
          end if;
        end if;

      end if;
    end if;
  end process;

  -- TODO: [Gary] May want this to use hsync_cnt earlier than 20 so that de also
  --       is high during 96 left/right border?
  o_de <= '1' when vsync_cnt < 576 and not cntwh else '0';
end;

-- TODO: Getting no display output in text modes. 
-- TODO: Graphics modes show ">" repeated for entire line. There's no row increment occuring?
-- TODO: In some modes cpu processing might be totally stopping? like mode 4 & 5?
-- TODO: is disp_bline and hsync edge detection correct for ula line 733?
--       could address never get incremented? or is it the read_addr + 8 part?
--       check if disp_addint is firing for too long?
-- Back to one extra line at bottom of display every other field. Same as the issue
-- that needed hackwhsync

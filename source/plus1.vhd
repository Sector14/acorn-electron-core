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

-- Plus 1 Expansion
--
-- The Electron's Plus1 provides 2 cart slots, a 15 pin analogue port (2 joysticks,
-- 4 paddles) and a printer port.
--
-- Current recreation supports only the onboard ROM (page 12) as well as up to
-- 4 additional 16KB ROMs, two per cart (pages 0-3 or 13).
--
-- Joystick support may be added later but only as a digital version.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

use work.Replay_Pack.all;

entity Expansion_Plus1 is
  port (
    -- Framework interfacing
    i_clk_sys       : in bit1;
    o_n_oe          : out bit1;  -- onboard plus 1 ROM

    i_joy_a         : in  word( 5 downto 0);
    i_joy_b         : in  word( 5 downto 0);

    -- Expansion port I/O
    i_16m_ena       : in bit1;

    i_n_rst         : in bit1;
    i_n_w           : in bit1;
    i_phiout        : in bit1;

    i_addr          : in word( 15 downto 0 );
    b_data          : inout word( 7 downto 0 );

    
    -- Unused expansion signals
    -- o_rdy          => cpu_rdy,
    -- i_sound_out    => ula_sound_o,
    -- o_n_irq        => ula_n_irq,
    -- o_n_nmi        => n_nmi,

    -- Cart Slots (not supported beyond chip enables for ROM paging)
    -- Two sockets available with signals other than oe2/oe4 shared between.
    o_n_oe4         : out bit1;     -- SK 1 (far) page 0 or 1
    o_n_oe2         : out bit1;     -- SK 2 (near) page 2 or 3
    o_n_oe3         : out bit1;     -- SK1&2 page 13

    o_rom_qa        : out bit1;      -- LSB of xFE05 (distinguish between two possible pages)

    -- Unused cart socket signals
    -- o_addr 
    -- o_data 
    -- o_n_rst

    -- o_r_n_w
    -- o_phi 

    -- o_n_romstb                   -- Paging reg select xFC73 (how was this used?)

    -- i_n_rdy     -- suspend cpu
    -- i_n_nmi     -- non maskable interrupt (let it snow)
    -- i_n_irq     -- maskable interrupt

    -- o_16m_ena   -- 16MHz clock enable

    -- o_sound_out
    -- io_adin      -- linked between both carts

    o_debug         : out word(15 downto 0)
  );
end;

architecture RTL of Expansion_Plus1 is
  -- ROMs
  signal rom_page_enable : bit1;
  signal rom_page        : word(2 downto 0);

  -- ADC
  signal n_fc70, n_fc71, n_fc72, n_fc73 : bit1;
  signal adc_n_cs, adc_n_r, adc_n_w : bit1;
  signal adc_n_w_l : bit1;
  signal adc_n_intr : bit1;
  signal adc_data : word(7 downto 0);

  signal stat_reg : word(7 downto 4);
begin
  -- page 12 on board rom
  o_n_oe <= '0' when i_n_w = '1' and i_addr(15) = '1' and i_addr(14) = '0' and
                     rom_page_enable = '1' and rom_page = "100" else '1';

  -- page 0 or 1
  o_n_oe4 <= '0' when i_addr(15) = '1' and i_addr(14) = '0' and  
                      rom_page_enable = '0' and rom_page(2 downto 1) = "00" else '1';
  -- page 2 or 3
  o_n_oe2 <= '0' when i_addr(15) = '1' and i_addr(14) = '0' and  
                      rom_page_enable = '0' and rom_page(2 downto 1) = "01" else '1';
  -- page 13
  o_n_oe3 <= '0' when i_addr(15) = '1' and i_addr(14) = '0' and  
                      rom_page_enable = '1' and rom_page = "101" else '1';
  -- LSB FE05
  o_rom_qa <= rom_page(0);

  -- Rom Page decoding
  p_page_decode : process(i_clk_sys, i_n_rst)
  begin
    if i_n_rst = '0' then
      rom_page_enable <= '0';
      rom_page <= (others => '0');
    elsif rising_edge(i_clk_sys) then
      if i_phiout = '1' then
    
        -- Register access rom paging
        if i_addr(15 downto 0) = x"FE05" and i_n_w = '0' then
          if rom_page_enable = '1' and rom_page(2) = '0' then
            -- Only 8-15 allowed when page 8-11 is active (ie kbd/basic rom pages AUG p211)
            if (b_data(3) = '1') then
              rom_page_enable <= b_data(3); 
              rom_page <= b_data(2 downto 0);              
            end if;
          else
            rom_page_enable <= b_data(3); 
            rom_page <= b_data(2 downto 0);
          end if;
        end if;

      end if;
    end if;                
  end process;

  --
  -- 15 Pin ADC0844
  --   
  n_fc70 <= '0' when i_addr = x"FC70" else '1';
  n_fc71 <= '0' when i_addr = x"FC71" else '1';
  n_fc72 <= '0' when i_addr = x"FC72" else '1';
  n_fc73 <= '0' when i_addr = x"FC73" else '1';
  
  adc_n_cs <= n_fc70;

  -- These were AND'd with phiout however due to using clock enable, logic has to be adjusted
  adc_n_r <= '0' when i_n_w = '1' else '1';
  adc_n_w <= '0' when i_n_w = '0' else '1';

  -- n_romstb <= n_fc73 = '1' and i_n_w = '0';

  -- Only single ended mux supported. Differential and pseudo differential
  -- not implemented. Single ended is "faked" in that it's a min/mid/max result only
  -- due to the translation of digital joystick inputs rather than real actual ADC.
  p_adc : process (i_clk_sys, i_n_rst)    
  begin
    if i_n_rst = '0' then
      adc_n_intr <= '1';
      adc_data <= x"80";
    elsif rising_edge(i_clk_sys) then   
      if i_phiout = '1' then

        if adc_n_cs = '0' then
          if adc_n_r = '0' then
            adc_n_intr <= '1';
          end if;

          if adc_n_w = '0' then
            -- No input, mid value.
            adc_data <= x"80";

            -- TODO: [Gary] should really wait 40us after conversion started before joystick values latched.
            -- TODO: [Gary] ADC would have not provided a nice 0, 128, 255 response to the three joystick
            --              positions. Perhaps add a little variable noise onto this.
            case b_data(3 downto 0) is 
              -- joy1
              when "0100" => -- ch1 p15 X
                if i_joy_a(2) = '0' then
                  adc_data <= x"00";
                elsif i_joy_a(3) = '0' then
                  adc_data <= x"FF";
                end if;
              when "0101" => -- ch2 p7  Y            
                if i_joy_a(0) = '0' then
                  adc_data <= x"00";
                elsif i_joy_a(1) = '0' then
                  adc_data <= x"FF";
                end if;
              -- joy2
              when "0110" => -- ch3 p12 X
                if i_joy_b(2) = '0' then
                  adc_data <= x"00";
                elsif i_joy_b(3) = '0' then
                  adc_data <= x"FF";
                end if;
              when "0111" => -- ch4 p4  Y
                if i_joy_b(0) = '0' then
                  adc_data <= x"00";
                elsif i_joy_b(1) = '0' then
                  adc_data <= x"FF";
                end if;
              when others => 
                -- No other mode supported
                adc_data <= x"80"; 
            end case;

            adc_n_intr <= '0';
          end if;

        end if;

      end if;
    end if;    
  end process;

  -- d7 = set/latched 0 when /RST or printer busy (DB7 = 0?), reset = 1 after /CEN? which is a /wr to fc71
  -- d7-d4: printer /busy, /intr, /button1 (p10), /button0 (p13)
  stat_reg <= '1' & adc_n_intr & i_joy_b(4) & i_joy_a(4);

  b_data <= adc_data          when adc_n_cs = '0' and adc_n_r = '0' else   -- adc read FC70
            stat_reg & "0000" when n_fc72 = '0' and i_n_w = '1' else       -- FC72 status reg
            (others => 'Z');
  
  o_debug(0) <= n_fc70;
  o_debug(1) <= n_fc72;
  o_debug(2) <= adc_n_r;
  o_debug(3) <= adc_n_w;
  o_debug(4) <= adc_n_w_l;
  o_debug(5) <= i_n_w;
  o_debug(6) <= i_phiout;
  o_debug(7) <= adc_n_intr;
  o_debug(8) <= adc_n_cs;
end RTL;
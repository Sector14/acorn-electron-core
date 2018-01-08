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

    -- Expansion port I/O
    i_n_rst         : in bit1;
    i_n_w           : in bit1;
    i_phiout        : in bit1;

    i_addr          : in word( 15 downto 0 );
    b_data          : inout word( 7 downto 0 );

    i_16m_ena       : in bit1;
    
    -- Unused expansion signals
    -- o_rdy          => cpu_rdy,
    -- i_sound_out    => ula_sound_o,
    -- o_n_irq        => ula_n_irq,
    -- o_n_nmi        => n_nmi,

    -- Cart Slots (not supported beyond chip enables for ROM paging)
    -- Two sockets available with signals other than oe2/oe4 shared between.
    o_n_oe4         : out bit1;     -- SK 1 (far) page 0 or 1
    o_n_oe2         : out bit1;     -- SK 2 (near, higher priority) page 2 or 3
    o_n_oe3         : out bit1;     -- SK1&2 page 13

    o_rom_qa        : out bit1      -- LSB of xFE05

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
  );
end;

architecture RTL of Expansion_Plus1 is

begin
  b_data <= 'Z';
  
  o_n_oe2 <= '1';
  o_n_oe3 <= '1';
  o_n_oe4 <= '1';

  o_rom_qa <= '0';



  -- TODO: [Gary] FE05 decoding to track which page is currently enabled.
  -- TODO: [Gary] DDR check if chip enables and select correct ROM address
  -- TODO: [Gary] ROMQA to determine between the two current banks the OE pin is active for.

end RTL;
-- WWW.FPGAArcade.COM
--
-- REPLAY Retro Gaming Platform
-- No Emulation No Compromise
--
-- All rights reserved
-- Mike Johnson 2015
--
-- Modified 2017 Gary Preston
-- Coverted from D4096_W8 to D32k_W8 with bank selection.
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

-- TODO: Needs test bench and validation

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

--
-- A RAM/ROM which is loadable/readable from the ARM MemIO bus
--
entity RAM_D32K_W8 is -- 32k 0x000 - 0x7FFF
  generic (
    g_addr                      : in  word(31 downto 0); -- 31 and 14..0 not used (15bits req)
    g_mask                      : in  word(31 downto 0) := x"FFFFFFFF" -- use to mask off any other bits in the compare
    );
  port (
    -- ARM interface
    i_memio_to_core             : in  r_Memio_to_core := z_Memio_to_core;
    --
    i_memio_fm_core             : in  r_Memio_fm_core := z_Memio_fm_core; -- cascade input. Must be z_Memio_fm_core on first memory
    o_memio_fm_core             : out r_Memio_fm_core; -- output back to LIB, or cascade into i_memio_fm_core on next memory
    --
    i_clk_sys                   : in  bit1 := '0';
    i_ena_sys                   : in  bit1 := '0';
    --
    i_addr                      : in  word(14 downto 0);
    i_data                      : in  word( 7 downto 0);
    i_ena                       : in  std_logic;          -- read/write clock enable
    i_wen                       : in  bit1;               -- high for a write
    o_data                      : out word( 7 downto 0);
    --
    i_clk                       : in  std_logic
    );
end;

architecture RTL of RAM_D32K_W8 is
  signal memio_match            : bit1;
  signal memio_cycle            : bit1 := '0';
  signal memio_ena              : bit1;
  signal memio_we               : bit1;
  signal memio_re               : bit1;
  signal memio_dout             : word(7 downto 0);
  signal memio_dout_g           : word(7 downto 0);

  -- bank selection
  signal memio_ena0             : bit1;
  signal memio_ena1             : bit1;
  signal memio_dout0            : word(7 downto 0);
  signal memio_dout1            : word(7 downto 0);
  signal i_ena0                 : bit1;
  signal i_ena1                 : bit1;
  signal o_data0                : word(7 downto 0);
  signal o_data1                : word(7 downto 0);

begin
  p_sel : process(i_memio_to_core)
  begin
    memio_match <= '0';
    if (i_memio_to_core.addr(30 downto 15) and g_mask(30 downto 15)) = (g_addr(30 downto 15) and g_mask(30 downto 15)) then
      memio_match <= '1';
    end if;
  end process;

  p_mio : process
  begin
    wait until rising_edge(i_clk_sys);
    if (i_ena_sys = '1') then
      memio_cycle <= '0';
      memio_we    <= '0';

      if (memio_match = '1') and (i_memio_to_core.valid = '1') and (memio_cycle = '0') then
        memio_cycle <= '1';
        memio_we    <= not i_memio_to_core.rw_l;
      end if;
    end if;
  end process;

  memio_ena <= i_ena_sys and memio_cycle;

  -- 32k 8bit comprised of two banks of eight 16k x 1bit BRAM
  -- A ARM port, B CORE port
  Ram : for i in 0 to 7 generate
    u_Ram_inst0 : RAMB16_S1_S1
    port map (
      DOA   => memio_dout0(i downto i),
      DOB   => o_data0(i downto i),
      ADDRA => i_memio_to_core.addr(13 downto 0),
      ADDRB => i_addr(13 downto 0),
      CLKA  => i_clk_sys,
      CLKB  => i_clk,
      DIA   => i_memio_to_core.w_data(i downto i),
      DIB   => i_data(i downto i),
      ENA   => memio_ena0,
      ENB   => i_ena0,
      SSRA  => '0',
      SSRB  => '0',
      WEA   => memio_we,
      WEB   => i_wen
      );

    u_Ram_inst1 : RAMB16_S1_S1
    port map (
      DOA   => memio_dout1(i downto i),
      DOB   => o_data1(i downto i),
      ADDRA => i_memio_to_core.addr(13 downto 0),
      ADDRB => i_addr(13 downto 0),
      CLKA  => i_clk_sys,
      CLKB  => i_clk,
      DIA   => i_memio_to_core.w_data(i downto i),
      DIB   => i_data(i downto i),
      ENA   => memio_ena1,
      ENB   => i_ena1,
      SSRA  => '0',
      SSRB  => '0',
      WEA   => memio_we,
      WEB   => i_wen
      );
  end generate;

  -- bank enable 
  -- data in, addr, write enable ignored whilst disabled.
  memio_ena0 <= memio_ena when i_memio_to_core.addr(14) = '0' else '0';
  memio_ena1 <= memio_ena when i_memio_to_core.addr(14) = '1' else '0';
  i_ena0 <= i_ena when i_addr(14) = '0' else '0';
  i_ena1 <= i_ena when i_addr(14) = '1' else '0';

  -- 2:1 mux for data out
  memio_dout <= memio_dout0 when i_memio_to_core.addr(14) = '0' else memio_dout1;
  o_data <= o_data0 when i_addr(14) = '0' else o_data1;

  p_out : process
  begin
    wait until rising_edge(i_clk_sys);
    if (i_ena_sys = '1') then
      memio_re <= memio_cycle and not memio_we;
    end if;
  end process;

  memio_dout_g <= memio_dout when (memio_re = '1') else (others => '0');

  o_memio_fm_core.taken  <= memio_cycle  or i_memio_fm_core.taken;
  o_memio_fm_core.r_we   <= memio_re     or i_memio_fm_core.r_we;
  o_memio_fm_core.r_data <= memio_dout_g or i_memio_fm_core.r_data;
end;

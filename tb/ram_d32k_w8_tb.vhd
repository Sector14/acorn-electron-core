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

--
-- 32k 8bit RAM Test Bench
--
-- Tests read/write of ram from the FPGA side
--

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_textio.all;

  use std.textio.ALL;
  use work.Replay_Pack.all;
  use work.Replay_Tb_Pack.all;

entity a_RAM_D32K_W8_tb is
end;

architecture rtl of a_RAM_D32K_W8_tb is

  constant c_clk_sys_period : time := 1 us / 128.000; -- MHz

  -- memio_to_core : r_Memio_to_core;
  -- memio_fm_core : r_Memio_fm_core;
  signal o_memio_fm_core : r_Memio_fm_core;

  signal clk_sys : bit1 := '0';
  signal ena_sys : bit1 := '0';

  signal addr        : word(14 downto 0) := (others => '0');
  signal data_write  : word( 7 downto 0) := (others => '0');
  signal ena         : std_logic := '1';
  signal wen         : bit1 := '0';
  signal data_read   : word( 7 downto 0) := (others => '0');

begin

  -- sys clock with 1:4 sys_ena
  p_clk_gen_sys : process
  begin
    ena_sys <= '0';
  	clk_sys <= '1';
  	wait for c_clk_sys_period/2;

  	clk_sys <= '0';
    wait for c_clk_sys_period/2;

    clk_sys <= '1';
  	wait for c_clk_sys_period/2;

  	clk_sys <= '0';
    wait for c_clk_sys_period/2;

    clk_sys <= '1';
  	wait for c_clk_sys_period/2;

  	clk_sys <= '0';
    wait for c_clk_sys_period/2;

    ena_sys <= '1';
    clk_sys <= '1';
  	wait for c_clk_sys_period/2;

    ena_sys <= '0';
  	clk_sys <= '0';
    wait for c_clk_sys_period/2;

  end process;

  uut_ram : entity work.RAM_D32K_W8
  -- 32k 0x000 - 0x7FFF
  generic map (
    g_addr => x"00000000",
    g_mask => x"00008000"
  )
  port map (
    -- ARM interface
    i_memio_to_core  => z_Memio_to_core,  -- not used
    --
    i_memio_fm_core  => z_Memio_fm_core,  -- first module
    o_memio_fm_core  => o_memio_fm_core,
    --
    i_clk_sys  => clk_sys,                -- ARM clock
    i_ena_sys  => ena_sys,
    --
    i_addr  => addr,
    i_data  => data_write,
    i_ena   => ena,
    i_wen   => wen,
    o_data  => data_read,
    --
    i_clk   => clk_sys                    -- FPGA clock
  );

  rw_process : process
  begin
    --
    -- Bank 0 address 0 & 1 write/read
    --
    wait until falling_edge(clk_sys);

    -- Test FPGA write/read
    addr <= x"00" & "0000000";
    data_write <= "00011011";
    wen <= '1';

    wait until falling_edge(clk_sys);
    addr <= x"00" & "0000001";
    data_write <= "01010101";
    wen <= '1';

    wait until falling_edge(clk_sys);
    addr <= x"00" & "0000000";
    data_write <= "11111111";
    wen <= '0';

    wait until falling_edge(clk_sys);

    assert (data_read = "00011011")
      report "Unexpected data read from address 0x0"
      severity FAILURE;

    --
    -- Bank 1 address 0 & 1 write/read
    --

    -- Test FPGA write/read
    addr <= x"80" & "0000000";
    data_write <= "11011011";
    wen <= '1';

    wait until falling_edge(clk_sys);
    addr <= x"80" & "0000001";
    data_write <= "11010101";
    wen <= '1';

    wait until falling_edge(clk_sys);
    addr <= x"80" & "0000000";
    data_write <= "11111111";
    wen <= '0';

    wait until falling_edge(clk_sys);

    assert (data_read = "11011011")
      report "Unexpected data read from address 0x4000"
      severity FAILURE;

    -- Check bank 0 address 0 still correct
    addr <= x"00" & "0000000";
    wen <= '0';

    wait until falling_edge(clk_sys);

    assert (data_read = "00011011")
      report "Unexpected data read from address 0x0"
      severity FAILURE;

    --
    -- Test ARM write/read, both banks
    --

    --
    -- Test ARM port write then FPGA read, both banks
    --

  end process;

end;

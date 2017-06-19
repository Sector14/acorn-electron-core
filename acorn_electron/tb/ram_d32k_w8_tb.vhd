-- 32k 8bit RAM Test Bench
--
-- Tests read/write of ram from the FPGA side
--
-- TODO: Simulate the ARM side and write from ARM read from FPGA too

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
  
  -- TODO: Adjust to match core
  constant c_clk_sys_period : time := 1 us / 114.545; -- MHz
  
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
    -- TODO: Sort out a more realistic test case using similar clock setup as the core.
    
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
    -- TODO: Test ARM write/read, both banks
    --

    --
    -- TODO: Test ARM port write then FPGA read, both banks
    --
  end process;

end;

-- 32k 8bit RAM Test Bench
--
-- Tests read/write of ram from the FPGA side
--
-- TODO: Simulate the ARM side and write from ARM read from FPGA too

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_textio.all;

  use std.textio.ALL;
  use work.Replay_Pack.all;
  use work.Replay_Tb_Pack.all;

entity a_RAM_D32K_W8_tb is
end;

architecture rtl of a_RAM_D32K_W8_tb is 
  -- memio_to_core : r_Memio_to_core;
  -- memio_fm_core : r_Memio_fm_core;
  signal o_memio_fm_core : r_Memio_fm_core;
  
  signal clk_sys : bit1 := '0';
  signal ena_sys : bit1 := '0';

  signal clk_ram : bit1 := '0';

  signal addr        : word(14 downto 0) := (others => '0');
  signal data_write  : word( 7 downto 0) := (others => '0');
  signal ena         : std_logic := '0';
  signal wen         : bit1 := '0';
  signal data_read   : word( 7 downto 0) := (others => '0');

begin

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

  clk_sys <= 'X', '0' after 10 NS, '1' after 20 NS;
end;

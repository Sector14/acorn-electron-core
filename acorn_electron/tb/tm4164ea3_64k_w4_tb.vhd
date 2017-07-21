-- TM4164EA3 Test Bench
--
-- Tests read/write of ram via /cas,/ras,/we signals

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_textio.all;

  use std.textio.ALL;
  use work.Replay_Pack.all;
  use work.Replay_Tb_Pack.all;

entity a_TM4164EA3_64k_W4_tb is
end;

architecture rtl of a_TM4164EA3_64k_W4_tb is
  
  constant c_clk_period : time := 1 us / 16.0; -- MHz
  
  signal clk : bit1 := '0';

  signal data : word(3 downto 0);
  signal o_data : word(3 downto 0);
  signal addr : word(7 downto 0);
  signal n_we : bit1;
  signal n_ras : bit1;
  signal n_cas : bit1;

  function vec2str(vec: std_logic_vector) return string is
  variable result: string(vec'left + 1 downto 1);
  begin
    for i in vec'reverse_range loop
      if (vec(i) = '1') then
        result(i + 1) := '1';
      elsif (vec(i) = '0') then
        result(i + 1) := '0';
      else
        result(i + 1) := 'X';
      end if;
    end loop;
  return result;
  end; 
begin


  p_clk_gen : process
  begin
  	clk <= '1';
  	wait for c_clk_period/2;

  	clk <= '0';
    wait for c_clk_period/2;
  end process;

  uut_ram : entity work.TM4164EA3_64k_W4
  port map (
    -- clock for sync bram 
    i_clk     => clk,

    i_addr    => addr,

    -- tied i/o bus
    i_data    => data,
    o_data    => o_data,
  
    i_n_we    => n_we,
    i_n_ras   => n_ras,
    i_n_cas   => n_cas
  );


  rw_process : process
  begin    
    wait until falling_edge(clk); 
    --
    -- Write "0101" to addr 0
    --
    n_we  <= '1';
    n_ras <= '1';
    n_cas <= '1';

    wait until falling_edge(clk);    
    -- row
    addr <= x"00";
    n_ras <= '0';  

    wait until falling_edge(clk);
    -- col
    addr <= x"00";
    data <= "0101";
    n_we <= '0';    
    n_cas <= '0';
    
    wait until falling_edge(clk);
    n_we <= '1';
    n_ras <= '1';
    n_cas <= '1';
    
    --
    -- Write "1111" to address 19,000 (0x4A38)
    --
    wait until falling_edge(clk);    
    -- row
    addr <= x"4A";
    n_ras <= '0';  

    wait until falling_edge(clk);
    -- col
    addr <= x"38";
    data <= "1111";
    n_we <= '0';    
    n_cas <= '0';
    
    wait until falling_edge(clk);
    n_we <= '1';
    n_ras <= '1';
    n_cas <= '1';

    --
    -- Read back address 0
    -- 
    wait until falling_edge(clk);   
    -- Read addr 0
    data <= (others => 'Z');
    addr <= x"00";
    n_ras <= '0';

    wait until falling_edge(clk);   
    -- col
    addr <= x"00";
    n_cas <= '0';

    wait until falling_edge(clk);   
    n_ras <= '1';
    n_cas <= '1';

    assert (o_data = "0101")
      report "Unexpected data read from address 0x0 "& vec2str(o_data)
      severity ERROR;

    --
    -- Read back address 0x4A38
    -- 
    wait until falling_edge(clk);   
    -- Read addr 4a38
    data <= (others => 'Z');
    addr <= x"4A";
    n_ras <= '0';

    wait until falling_edge(clk);   
    -- col
    addr <= x"38";
    n_cas <= '0';
    
    -- Test output remaining active over multiple cycles
    wait until falling_edge(clk);   
    --n_ras <= '1';
    --n_cas <= '1';

    assert (o_data = "1111")
      report "Unexpected data read from address 0x4a38"& vec2str(o_data)
      severity ERROR;

    wait until falling_edge(clk);   
    --n_ras <= '1';
    --n_cas <= '1';

    assert (o_data = "1111")
      report "Unexpected data read from address 0x4a38"& vec2str(o_data)
      severity ERROR;

    wait until falling_edge(clk);   
    n_ras <= '1';
    n_cas <= '1';

    assert (o_data = "1111")
      report "Unexpected data read from address 0x4a38"& vec2str(o_data)
      severity ERROR;

  end process;

end;

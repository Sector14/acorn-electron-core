-- Acorn Electron Test Bench

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;
  use ieee.std_logic_textio.all;

  use std.textio.ALL;
  use work.Replay_Pack.all;
  use work.Replay_Tb_Pack.all;

entity a_Replay_tb is
end;

architecture rtl of a_Replay_tb is

  constant c_clk_a_period : time := 1 us / 128.032; -- MHz
  constant c_clk_b_period : time := 1 us / 27.000;  -- MHz
  constant c_clk_c_period : time := 1 us / 27.000;   -- MHz

  signal scl                    : bit1;
  signal sda                    : bit1;

  signal fpga_ctrl              : word(1 downto 0);
  signal fpga_ctrl_direct_l     : bit1;
  signal fpga_spi_clk           : bit1;
  signal fpga_spi_mosi          : bit1;
  signal fpga_spi_miso          : bit1;
  signal spi_tb_miso            : bit1;

  signal ps2_clk                : word(1 downto 0);
  signal ps2_data               : word(1 downto 0);

begin

  u_Replay_Tb_Wrap : entity work.a_Replay_Tb_Wrap
  generic map (
    g_clk_a_period => c_clk_a_period,
    g_clk_b_period => c_clk_b_period,
    g_clk_c_period => c_clk_c_period
  )
  port map (
    b_scl                => scl,
    b_sda                => sda,
    --
    i_fpga_ctrl          => fpga_ctrl,
    i_fpga_spi_clk       => fpga_spi_clk,
    b_fpga_spi_mosi      => fpga_spi_mosi,
    b_fpga_spi_miso      => fpga_spi_miso,
    i_fpga_ctrl_direct_l => fpga_ctrl_direct_l,
    --
    b_ps2_clk            => ps2_clk,
    b_ps2_data           => ps2_data
  );

  scl       <= 'H';
  sda       <= 'H';

  ps2_clk  <= "HH";
  ps2_data <= "HH";

  --
  -- SPI
  --
  fpga_spi_mosi <= spi_tb_miso when (fpga_ctrl_direct_l = '1') else 'H';
  fpga_spi_miso <= spi_tb_miso when (fpga_ctrl_direct_l = '0') else 'H';

  p_spi : process
    variable spi_read_data : word(7 downto 0);

    procedure spi (data:word(7 downto 0); disp:bit1:='0') is
    begin
      spi(fpga_spi_clk, spi_tb_miso, fpga_ctrl_direct_l, fpga_spi_mosi, fpga_spi_miso, spi_read_data, data, disp);
    end procedure;

    procedure ena(sel:integer) is
    begin
      spi_ena(fpga_ctrl, sel);
    end procedure;

    procedure dis is
    begin
      spi_dis(fpga_ctrl);
    end procedure;

    procedure spi_readhex(filename:string) is
    begin
      spi_readhex(fpga_ctrl,fpga_spi_clk, spi_tb_miso, fpga_ctrl_direct_l, fpga_spi_mosi, fpga_spi_miso,filename);
    end procedure;

    procedure spi_readbin(filename:string;addr:word(31 downto 0);size:word(15 downto 0):=x"0000") is
    begin
      spi_readbin(fpga_ctrl,fpga_spi_clk, spi_tb_miso, fpga_ctrl_direct_l, fpga_spi_mosi, fpga_spi_miso,filename,addr,size);
    end procedure;
    
  begin
    spi_tb_miso   <= '1';
    fpga_spi_clk  <= '1';
    fpga_ctrl_direct_l <= '1';

    dis;

    -- see other testbenches for SPI examples
    wait for 1 us;
    
    ena(2);
    spi(x"23"); -- set phase
    spi(x"68");
    spi(x"02");
    dis;
    
    wait for 5 us;

    -- send config bits to core
    ena(2);
    spi(x"20"); -- static config
    spi(x"00");
    spi(x"00");
    spi(x"00");
    spi(x"00");
    dis;

    ena(2);
    spi(x"21"); -- dynamic config
    spi(x"00");
    spi(x"00");
    spi(x"00");
    spi(x"00");
    dis;

    ena(2);
    spi(x"22"); --global config
    spi(x"01");
    dis;

    -- DDR
    spi_readbin("../sdcard/os_basic.rom", x"00008000", x"8000");

    -- SRAM
    -- NOTE: SRAM has a separate ROM module so starts at addr 0x0 unlike
    -- DDR where room is left for RAM in the first 0x8000 bytes.
    --spi_readbin("../sdcard/os_basic.rom", x"80000000");

    ena(2);
    spi(x"11"); -- soft reset, remove halt
    dis;

    wait for 1 us;

    ena(2);
    spi(x"30"); -- ps/2 write
    spi(x"01");
    dis;

    wait;
  end process;

end;

-- TM4164EA4 - 4 x 4164 64k 1bit RAM 
--
-- Copyright Gary Preston 2017
-- All Rights Reserved

-- Original 4164 RAM was async however this module assumes all signals
-- are clock synchronised and implements the read/write protocol that the
-- temporary ULA is using. This will be revisited once more is understood about
-- the real workings of the ULA.
--
-- Read
--  1 - we, /ras = addr latched for row
--  2 - /cas = addr used for col and read occurs
--  3 - data out valid, /ras & /cas may return high
--  4 - data out goes Z
--
-- Write
--  1 - ras low = addr latched for row
--  2 - data in, cas low, we low = addr used for col, data written to row/col
--  3 - we high, cas & ras high
--
-- Not implemented: any of the other cycles including paged reads.
-- This implementation is a little more convoluted than it perhaps needed to be
-- due to sticking with a pin accurate ULA mapping.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity TM4164EA3_64k_W4 is 
  -- 64k 0x000 - 0x3FFFF
  port (
    -- clock for sync bram 
    i_clk                       : bit1;

    i_addr                      : in word(7 downto 0);

    i_data                      : in word(3 downto 0);
    o_data                      : out word(3 downto 0);
  
    i_n_we                      : in bit1;
    i_n_ras                     : in bit1;
    i_n_cas                     : in bit1
    );
end;

architecture RTL of TM4164EA3_64k_W4 is
  -- multiplexed addressing
  signal row_addr               : word(7 downto 0);

  -- active read/write address
  signal addr                   : word(15 downto 0);

  -- edge detection
  signal n_cas_edge, n_cas_l    : bit1;
  signal n_ras_edge, n_ras_l    : bit1;

  signal read_data              : word(3 downto 0);

  -- Ran out of available BRAM. Allowing use of distributed ram until this can
  -- be moved over to using the boards DRAM.
  type ram_type is array (65534 downto 0) of word(3 downto 0);
  shared variable RAM : ram_type;

begin 
  o_data <= read_data when (i_n_ras = '0' and i_n_cas = '0' and i_n_we = '1') else (others => 'Z');
  
  -- edge detection of ras and cas signals
  p_edge_detect : process (i_clk)
  begin
    if rising_edge(i_clk) then
      n_cas_l <= i_n_cas;
      n_ras_l <= i_n_ras;
    end if;
  end process;
  -- rising edge detect
  -- n_cas <= not n_cas_l and i_n_cas;
  -- n_ras <= not n_ras_l and i_n_ras;

  -- falling edge detect
  n_cas_edge <= (not i_n_cas) and n_cas_l;
  n_ras_edge <= (not i_n_ras) and n_ras_l;

  p_ras_cas : process(i_clk)
  begin
    if rising_edge(i_clk) then
      -- multiplexed address decoding
      if (n_ras_edge = '1') then
        row_addr <= i_addr;
      end if;

      if (n_cas_edge = '1') then
        -- Full nibble address
        addr <= row_addr & i_addr;

        if (i_n_we = '0') then
          RAM(to_integer(unsigned(i_addr))) := i_data;
        end if;
        read_data <= RAM(to_integer(unsigned(i_addr)));
      end if;
      
    end if;

    -- TODO: [Gary] Other modes that may be needed if used by ULA
    --   * read-write/read-modify-write cycles
    --   * page mode read cycle
    --   * page mode write cycle

    -- TODO: [Gary] Implementation not needed just ensure it doesn't break anything if used:
    --   * ras only refresh cycle 
    --   * hidden refresh cycle

  end process;


end;

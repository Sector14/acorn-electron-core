-- Acorn Electron ULA
-- Ferranti 12C021 Custom
--
-- Copyright Gary Preston 2017
-- All Rights Reserved
--
-- Temporary implementation until reverse engineering of the 12C021 is complete

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity ULA_12C021 is
  port (
    -- Cassette I/O (not yet supported)
    i_cas         : in bit1; 
    o_cas         : out bit1;      
    b_cas_rc      : inout bit1;                -- RC high tone detection
    o_cas_mo      : out bit1;                  -- Motor relay
       
    -- Audio             
    o_sound_op    : out bit1;            
       
    -- Reset             
    i_n_por       : in bit1;                   -- /Power on reset
       
    -- Video             
    o_n_csync     : out bit1;                  -- h/v sync
    o_hsync       : out bit1;                  -- h sync
    o_red         : out bit1;            
    o_green       : out bit1;            
    o_blue        : out bit1;            
       
    -- Clock             
    i_clk         : in bit1;                    
    i_div_13      : in bit1;                   -- clk div 13
       
    -- RAM (4x64k 1 bit)       
    b_ram0        : inout bit1;                -- RAM Data ic 0
    b_ram1        : inout bit1;                -- RAM Data ic 1
    b_ram2        : inout bit1;                -- RAM Data ic 2
    b_ram3        : inout bit1;                -- RAM Data ic 3
       
    o_n_we        : out bit1;                  -- /write, read
    o_n_ras       : out bit1;                  -- row address strobe  -ve edge
    o_n_cas       : out bit1;                  -- col address strobe  -ve edge

    o_ra          : out word( 7 downto 0 );    -- ram address

    -- Keyboard
    i_kbd         : in word( 3 downto 0 ); 
    i_caps_lock   : in bit1;
    i_n_reset     : in bit1;

    -- ROM/CPU addressing
    o_rom         : out bit1;                  -- rom select enable   
    i_addr        : in word( 15 downto 0 );
    b_pd          : inout word( 7 downto 0 );  -- CPU/ROM data

    -- CPU
    i_nmi         : in bit1;                   -- 1MHz RAM access detection
    o_phi_out     : out bit1;                  -- CPU clk, 2MHz, 1MHz or stopped
    o_n_irq       : out bit1;
    i_n_w         : in bit1                    -- Data direction, /write, read
  );
end;

architecture RTL of ULA_12C021 is
  -- TODO: Consts for bit access?
  
  -- 
  -- Registers (AUG p206)
  --

  -- Addressed via page 0xFExx. 16 byte aliasing, ie 0xFE00 and 0xFE10 both refer to register 0.
  --
  -- 0xFE00 - Interrupt status and control (enable interrupts)
  --     0=master irq, 1=por, 2=disp interrupt, 3=rtc, 4=transmit empty, 5=receive full, 6=high tone, 7=N/A
  signal master_irq     : bit1;
  signal power_on_reset : bit1; 
  signal isr_en         : word( 6 downto 2 ) := (others => '0');

  -- 0xFE02 & 0xFE03 - screen start addr control

  -- 0xFE04 - Cassette data shift

  -- 0xFE05 - Interrupt clear & ROM Paging

  -- 0xFE06 - Multipurpose Counter

  -- 0xFE07 - Misc control

  -- 0xFE08 to 0xFE0F - Colour palette


begin

  -- bi-dir 'Z' state
  b_pd <= (others => 'Z');

-- Power up, perform reset

-- Clock Divider

-- Modes

-- RAM Access
-- 4164 ram is async, however this implementation is synchronous
-- An actual ULA would need this interface logic rewriting to match timing requirements

end;

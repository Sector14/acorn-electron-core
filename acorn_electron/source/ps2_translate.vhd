-- Translate between Framework PS2 scancodes and Electron ULA
--
-- ULA expects keyboard to send 4 data bits representing the state of
-- four keys each. Set of keys selected by active low bits on addr bus.
-- See AUG p216 or Keyboard matrix in service manual.
--
-- Copyright Gary Preston 2017
-- All Rights Reserved

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

use work.Replay_Pack.all;
use work.Replay_ScanCode_PS2_Pack.all;

entity PS2_Translate is
  port (
    -- NO ENABLE for ps2 usage in the framework
    i_clk_sys        : in bit1;
    i_rst_sys        : in bit1;

    -- Keyboard framework interface
    i_kb_ps2_we           : in  bit1;
    i_kb_ps2_data         : in  word( 7 downto 0);
    i_kb_inhibit          : in  bit1;           -- OSD active

    -- Electron keyboard interface
    i_addr            : in word(13 downto 0);   -- active low
    o_data            : out word(3 downto 0);   -- active low

    o_n_break         : out bit1
  );
end;

architecture RTL of PS2_Translate is

  constant c_KEY_EXTENDED  : word(7 downto 0) := x"E0";
  constant c_KEY_PAUSE     : word(7 downto 0) := x"E1";
  constant c_KEY_RELEASE   : word(7 downto 0) := x"F0";

  -- key state by addr line
  subtype t_key_row_state is word(3 downto 0);
  type t_key_state is array (13 downto 0) of t_key_row_state;

  signal key_state    : t_key_state := (others => (others => '1'));
  signal key_release  : bit1;
  signal key_extended : bit1;
  
  signal key_n_pausebreak : bit1;

begin

  -- Keys and address are active low
  -- See AUG p216 or Keyboard matrix in service manual.
  -- Grid is (a0 || k0) && (a1 + k1) && (a2 || k2)... for each bit
  p_kbd_scan : process(i_clk_sys, i_rst_sys, i_kb_inhibit)
  begin
    if (i_rst_sys = '1' or i_kb_inhibit = '1') then
      key_extended <= '0';
      key_release <= '0';
      key_state <= (others => (others => '1'));
      key_n_pausebreak <= '1';
    elsif rising_edge(i_clk_sys) then
      -- No release code, active for one clock pulse only
      key_n_pausebreak <= '1';

      -- Framework strobes only on activity
      if (i_kb_ps2_we = '1' and i_kb_inhibit = '0') then

        if (i_kb_ps2_data = c_KEY_PAUSE) then
          key_n_pausebreak <= '0';
        elsif (i_kb_ps2_data = c_KEY_RELEASE) then
          key_release <= '1'; 
        elsif (i_kb_ps2_data = c_KEY_EXTENDED) then
          key_extended <= '1';
        else

          -- keys in scancode_ps2 are ext bit & scancode
          case key_extended & i_kb_ps2_data is
            when c_PS2_ESC         => key_state(13)(0) <= key_release;
            when c_PS2_CAPS_LOCK   => key_state(13)(1) <= key_release;
            when c_PS2_LEFT_CTRL | c_PS2_RIGHT_CTRL   => key_state(13)(2) <= key_release;
            when c_PS2_LEFT_SHIFT | c_PS2_RIGHT_SHIFT => key_state(13)(3) <= key_release;
           
            when c_PS2_1 => key_state(12)(0) <= key_release;
            when c_PS2_Q => key_state(12)(1) <= key_release;
            when c_PS2_A => key_state(12)(2) <= key_release;
            when c_PS2_Z => key_state(12)(3) <= key_release;

            when c_PS2_2 => key_state(11)(0) <= key_release;
            when c_PS2_W => key_state(11)(1) <= key_release;
            when c_PS2_S => key_state(11)(2) <= key_release;
            when c_PS2_X => key_state(11)(3) <= key_release;

            when c_PS2_3 => key_state(10)(0) <= key_release;
            when c_PS2_E => key_state(10)(1) <= key_release;
            when c_PS2_D => key_state(10)(2) <= key_release;
            when c_PS2_C => key_state(10)(3) <= key_release;

            when c_PS2_4 => key_state(9)(0) <= key_release;
            when c_PS2_R => key_state(9)(1) <= key_release;
            when c_PS2_F => key_state(9)(2) <= key_release;
            when c_PS2_V => key_state(9)(3) <= key_release;

            when c_PS2_5 => key_state(8)(0) <= key_release;
            when c_PS2_T => key_state(8)(1) <= key_release;
            when c_PS2_G => key_state(8)(2) <= key_release;
            when c_PS2_B => key_state(8)(3) <= key_release;

            when c_PS2_6 => key_state(7)(0) <= key_release;
            when c_PS2_Y => key_state(7)(1) <= key_release;
            when c_PS2_H => key_state(7)(2) <= key_release;
            when c_PS2_N => key_state(7)(3) <= key_release;

            when c_PS2_7 => key_state(6)(0) <= key_release;
            when c_PS2_U => key_state(6)(1) <= key_release;
            when c_PS2_J => key_state(6)(2) <= key_release;
            when c_PS2_M => key_state(6)(3) <= key_release;

            when c_PS2_8 => key_state(5)(0) <= key_release;
            when c_PS2_I => key_state(5)(1) <= key_release;
            when c_PS2_K => key_state(5)(2) <= key_release;
            when c_PS2_COMMA => key_state(5)(3) <= key_release;

            when c_PS2_9 => key_state(4)(0) <= key_release;
            when c_PS2_O => key_state(4)(1) <= key_release;
            when c_PS2_L => key_state(4)(2) <= key_release;
            when c_PS2_DOT => key_state(4)(3) <= key_release;

            when c_PS2_0 => key_state(3)(0) <= key_release;
            when c_PS2_P => key_state(3)(1) <= key_release;
            when c_PS2_SEMICOLON => key_state(3)(2) <= key_release;
            when c_PS2_FWDSLASH  => key_state(3)(3) <= key_release;

            when c_PS2_MINUS => key_state(2)(0) <= key_release;
            when c_PS2_UP    => key_state(2)(1) <= key_release;
            when c_PS2_EQUALS => key_state(2)(2) <= key_release; -- : *
            -- when UNUSED => key_state(2)(3) <= key_release;

            when c_PS2_LEFT => key_state(1)(0) <= key_release;
            when c_PS2_DOWN => key_state(1)(1) <= key_release;
            when c_PS2_ENTER => key_state(1)(2) <= key_release;
            when c_PS2_DELETE | c_PS2_BACKSPACE => key_state(1)(3) <= key_release;

            when c_PS2_RIGHT => key_state(0)(0) <= key_release;            
            when c_PS2_LEFT_BRACKET => key_state(0)(1) <= key_release; -- COPY
            -- when UNUSED => key_state(0)(2) <= key_release;
            when c_PS2_SPACE => key_state(0)(3) <= key_release;

            when others =>
          end case;

          key_release <= '0';
          key_extended <= '0';
        end if;

      end if;
    end if;
  end process;

  p_addr_scan : process(i_addr)
      variable result : t_key_row_state := (others => '1');
  begin
    result := (others => '1');
    -- Addr and key state are active low.
    for I in i_addr'range loop
      for J in result'range loop
        result(J) := result(J) and (i_addr(I) or key_state(I)(J));
      end loop;
    end loop;

    o_data <= result;
  end process;

  o_n_break <= key_n_pausebreak;

end RTL;

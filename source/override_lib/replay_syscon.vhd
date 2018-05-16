--
-- WWW.FPGAArcade.COM
--
-- REPLAY Retro Gaming Platform
-- No Emulation No Compromise
--
-- All rights reserved
-- Mike Johnson 2015
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
-- This file is licensed for use ONLY with Replay hardware from FPGAArcade.com
--
library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity Replay_Syscon is
  generic (
    -- initial config
    g_cfg_static          : in   word(31 downto 0);
    g_cfg_dynamic         : in   word(31 downto 0);
    --
    g_cfg_fileio_cha_ena  : in   word( 3 downto 0) := "0000"; -- MASK for what the core supports. Read by ARM
    g_cfg_fileio_cha_drv  : in   word( 3 downto 0) := "0000"; -- ARM Driver type
    g_cfg_fileio_chb_ena  : in   word( 3 downto 0) := "0000"; -- MASK for what the core supports. Read by ARM
    g_cfg_fileio_chb_drv  : in   word( 3 downto 0) := "0000"; -- ARM Driver type
    --
    g_cfg_ctrl            : in   word(15 downto 0); --
    g_version             : in   word(15 downto 0)  -- 15..8 core ID 7..0 version , bit 15 high DRAM not used
    );
  port (
    i_clk_vid             : in   bit1;
    i_ena_vid             : in   bit1;
    i_rst_vid             : in   bit1;
    --
    i_clk_ctl             : in   bit1; --~50 MHz
    i_rst_ctl             : in   bit1;
    i_rst_ctl_hard        : in   bit1;
    --
    o_rst_soft            : out  bit1;
    o_rst_vid             : out  bit1;
    o_halt                : out  bit1;
    --
    i_spi_cs_l            : in   bit1;
    i_spi_clk             : in   bit1;
    i_spi_d               : in   bit1;
    o_spi_d               : out  bit1;
    --
    o_vbl                 : out  bit1; -- to ARM
    --
    -- all on clk_Ctrl
    --
    -- Config
    i_cfg_status          : in   word(15 downto 0);
    --
    o_cfg_static          : out  word(31 downto 0);
    o_cfg_dynamic         : out  word(31 downto 0);
    o_cfg_global          : out  word( 7 downto 0);
    o_cfg_fileio_cha      : out  r_Cfg_fileio;
    o_cfg_fileio_chb      : out  r_Cfg_fileio;
    o_cfg_ctrl            : out  word(15 downto 0);
    --
    -- Keyboard
    --
    i_kb_ps2_we           : in   bit1;
    i_kb_ps2_data         : in   word( 7 downto 0);
    o_kb_ps2_we           : out  bit1;              -- ps2 from ARM to core
    o_kb_ps2_data         : out  word( 7 downto 0);
    o_kb_inhibit          : out  bit1;
    --
    -- Video on clk_video
    --
    i_vid_rgb             : in   word(23 downto 0); -- r 23..16 g 15..8 b 7..0
    i_vid_sync            : in   r_Vidsync;         -- syncs co-timed with input video
    --
    o_vid_rgb             : out  word(23 downto 0);
    o_vid_sync            : out  r_Vidsync
    --
    );
end;

architecture RTL of Replay_Syscon is

  -- SPI --
  signal spi_rx_rdy             : bit1;
  signal spi_rx_rdy_meta_1      : bit1;
  signal spi_rx_rdy_meta_2      : bit1;
  signal spi_stb                : bit1;
  --
  signal spi_rx_sof             : bit1;
  signal spi_rx_sof_meta_1      : bit1;
  signal spi_rx_sof_meta_2      : bit1;
  signal spi_sof                : bit1;
  --
  signal spi_rx_data            : word( 7 downto 0);
  signal spi_data               : word( 7 downto 0);
  --
  signal spi_rx_byte_sel        : word( 7 downto 0);
  signal spi_byte_sel           : word( 7 downto 0);
  signal spi_rx_cmd             : word( 7 downto 0);
  --
  signal spi_tx_data            : word( 7 downto 0);

  signal spi_tx_fifo_data       : word( 7 downto 0);
  signal spi_tx_fifo_valid      : bit1;
  signal spi_tx_fifo_rd         : bit1;
  --
  signal spi_tx_status          : word( 7 downto 0);
  signal spi_tx_kbd             : word( 7 downto 0);
  signal spi_tx_cfg_status      : word(15 downto 0);

  signal spi_cmd_high_comb      : word(15 downto 0);
  signal spi_cmd_high           : word(15 downto 0);
  signal spi_cmd_low_comb       : word(15 downto 0);
  signal spi_cmd_low            : word(15 downto 0);
  --
  signal spi_rx_ps2_we          : bit1;
  signal spi_rx_ps2_data        : word( 7 downto 0);

  signal char_addr              : word(10 downto 0);
  signal char_wena              : bit1;
  signal hoff_wena_1            : bit1;
  signal hoff_wena_2            : bit1;
  signal voff_wena              : bit1;

  signal osd_ena                : bit1;
  signal osd_hstart             : bit1;
  signal osd_vstart             : bit1;
  signal osd_hexp               : bit1;
  signal osd_vexp               : bit1;
  
  signal osd_vid_in             : word(23 downto 0);
  signal osd_vid_out            : word(23 downto 0);

  signal reset_vid_t            : word( 5 downto 0);

  type r_SpiCtrl is record
    keyboard_dis    : bit1;
    osd_ena         : bit1;
    reset           : bit1;
    halt            : bit1;
    reset_vid       : bit1;
    --
    cfg_static      : word(31 downto 0);
    cfg_dynamic     : word(31 downto 0);
    cfg_global      : word( 7 downto 0);
    cfg_fileio_cha  : r_Cfg_fileio;
    cfg_fileio_chb  : r_Cfg_fileio;
    cfg_ctrl        : word(15 downto 0);
  end record;

  signal spi_ctrl               : r_SpiCtrl;
  --{{{  local CS
  --component icon
    --PORT (
      --CONTROL0 : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0)
      --);
  --end component;

  --component ila_1024_63
    --PORT (
      --CONTROL : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0);
      --CLK     : IN STD_LOGIC;
      --TRIG0   : IN STD_LOGIC_VECTOR(62 DOWNTO 0)
      --);
  --end component;

  --signal cs_control0   : std_logic_vector(35 downto 0);
  --signal cs_clk        : std_logic;
  --signal cs_trig       : std_logic_vector(62 downto 0);
  --}}}

begin
  -- OSD commands
  -- C = CMD W0 = byte following command, W1 = next byte etc
  -- (no W means command word only)

  --   Read (aliased, only bottom 4 bits are check on mux)
  -- 0x00 read syscon status (W0)
  -- 0x01 read magic         (W0) xA0 Syscon protocol version
  -- 0x02 read core verion   (W0) g_Version ( 7..0)
  -- 0x03 read core verion   (W0) g_Version (15..8)
  -- 0x04 read fileio        (W0) g_cfg_fileio_chb_ena, g_cfg_fileio_cha_ena
  -- 0x05 read fileio        (W0) g_cfg_fileio_chb_drv, g_cfg_fileio_cha_drv
  -- 0x06 read status        (W0) i_Cfg_Status( 7..0)
  -- 0x07 read status        (W0) i_Cfg_Status(15..8)

  -- 0x08 read keyboard fifo (W0)

  --   Write
  -- 0x1x control                     bit 0 = reset, bit 1 = halt, bit 2 = reset_vid

  -- 0x20 write config (core static)  W0-W3   reset required to update
  -- 0x21 write config (core dynamic) W0-W3
  -- 0x22 write config (global)       W0
  -- 0x23 write config (ctrl)         W0-W1   used for DRAM and internal config

  -- 0x28 write fileio cha            W0 7..4 hd_wen, 3..0 hd_ins DYNAMIC
  -- 0x29 write fileio chb            W0 7..4 fd_wen, 3..0 fd_ins DYNAMIC

  -- 0x3x PS2 from ARM                W0 7..0 PS2 data
  -- 0x4x OSD control                 bit 0 = ena,   bit 1 = disable keyboard
  -- 0x50 OSD set Hoffset             W1 = offset 5..0, pixel 3..2. Use OSD buffer write to set row address
  --                                  W2 = pixel 1..0
  -- 0x51 OSD set Voffset             W1 = offset 3..0

  -- 0xC0-0xFF OSD buffer write       bits 5..0 set row
  --                                  W1 6..0 set col
  --                                  W2- write char
  -- OSD address 10..7 row,6..1 col, 0 char/attrib

  -- Note - cfg_ctrl
  --

  --
  -- SPI interface to ARM
  --
  u_spi : entity work.Replay_SPI8
  port map(
    i_spi_direct_l => '1',
    i_spi_cs_l     => i_spi_cs_l,
    i_spi_clk      => i_spi_clk,
    i_spi_d        => i_spi_d,
    o_spi_d        => o_spi_d,
    --
    o_rxdata       => spi_rx_data,
    o_rxbytesel    => spi_rx_byte_sel,
    o_rxbytecnt    => open,
    o_rxcmd        => spi_rx_cmd,
    --
    i_txdata       => spi_tx_data, -- reclocked internally
    --
    o_sof          => spi_rx_sof,
    i_acksof       => spi_rx_sof_meta_2,
    o_first        => open,
    --
    o_rdy          => spi_rx_rdy,
    i_ack          => spi_rx_rdy_meta_2
    );

  -- inputs should be clocked on rising_edge i_spi_clk then muxed using spi_rx_cmd.
  -- registered on falling edge inside spi8. Note that if the first read word is used,
  -- the data will be registered here on the last rising edge from the previous SPI frame,
  -- and could be quite stale.
  p_spi_tx_reg : process
  begin
    wait until rising_edge(i_spi_clk);
    -- NO ENABLE
    spi_tx_status     <= "0000000" & spi_tx_fifo_valid;
    spi_tx_kbd        <= spi_tx_fifo_data; -- for timing

  end process;

  p_spi_tx_mux : process(spi_rx_cmd,
                         spi_tx_status, spi_tx_cfg_status, spi_tx_kbd)
  begin
    -- all inputs to this must either be clocks on spi_clk or latched on sof
    spi_tx_data <= spi_tx_status; -- default
    case spi_rx_cmd(3 downto 0) is
      when x"0" => spi_tx_data <= spi_tx_status;
      when x"1" => spi_tx_data <= x"A5";
      when x"2" => spi_tx_data <= g_version( 7 downto 0);
      when x"3" => spi_tx_data <= g_version(15 downto 8);
      when x"4" => spi_tx_data <= g_cfg_fileio_chb_ena & g_cfg_fileio_cha_ena; -- static
      when x"5" => spi_tx_data <= g_cfg_fileio_chb_drv & g_cfg_fileio_cha_drv; -- static
      when x"6" => spi_tx_data <= spi_tx_cfg_status( 7 downto 0);
      when x"7" => spi_tx_data <= spi_tx_cfg_status(15 downto 8);
      when x"8" => spi_tx_data <= spi_tx_kbd;
      when others => null;
    end case;

  end process;
  --
  -- END of SPI_CLK DOMAIN
  --
  p_spi_meta : process
  begin
    wait until rising_edge(i_clk_ctl);
    -- CLOCK CHANGE
    spi_rx_rdy_meta_1 <= spi_rx_rdy;
    spi_rx_rdy_meta_2 <= spi_rx_rdy_meta_1;

    spi_rx_sof_meta_1 <= spi_rx_sof;
    spi_rx_sof_meta_2 <= spi_rx_sof_meta_1;

    spi_data     <= spi_rx_data;
    spi_byte_sel <= spi_rx_byte_sel;
    --
    spi_stb      <= spi_rx_rdy_meta_2 and not spi_stb;
    spi_sof      <= spi_rx_sof_meta_2 and not spi_sof;
  end process;

  p_spi_status : process
  begin
    wait until rising_edge(i_clk_ctl);
    if (spi_sof = '1') then
      spi_tx_cfg_status <= i_cfg_status;
    end if;
  end process;

  ps2kb_fifo : entity work.FIFO_SyncSRL_D16
  generic map (
    g_width => 8)
  port map (
    i_w_data      => i_kb_ps2_data,
    i_w_ena       => i_kb_ps2_we,
    o_w_full      => open,
    o_w_hfull     => open,
    -- note fall through
    o_r_data      => spi_tx_fifo_data,
    i_r_taken     => spi_tx_fifo_rd,
    o_r_valid     => spi_tx_fifo_valid,
    i_rst         => i_rst_ctl,
    i_clk         => i_clk_ctl
    );

  o_kb_ps2_we   <= spi_rx_ps2_we;
  o_kb_ps2_data <= spi_rx_ps2_data;

  --
  -- SPI ctrl
  --
  p_spi_decode_cmd : process(spi_data)
  begin
     -- decode top nibble
     spi_cmd_high_comb <= (others => '0');
     spi_cmd_high_comb(conv_integer(spi_data(7 downto 4))) <= '1';
     -- decode bottom nibble
     spi_cmd_low_comb  <= (others => '0');
     spi_cmd_low_comb (conv_integer(spi_data(3 downto 0))) <= '1';
  end process;

  p_spi_decode : process(i_clk_ctl, i_rst_ctl_hard)
  begin
    if (i_rst_ctl_hard = '1') then -- note startup reset
      spi_ctrl.keyboard_dis <= '0';
      spi_ctrl.osd_ena      <= '0';
      spi_ctrl.reset        <= '0';
      spi_ctrl.reset_vid    <= '0';
      spi_ctrl.halt         <= '1';
      --
      spi_ctrl.cfg_static     <= g_cfg_static;
      spi_ctrl.cfg_dynamic    <= g_cfg_dynamic;
      spi_ctrl.cfg_global     <= (others => '0');
      spi_ctrl.cfg_fileio_cha <= z_Cfg_fileio;
      spi_ctrl.cfg_fileio_chb <= z_Cfg_fileio;
      spi_ctrl.cfg_ctrl       <= g_cfg_ctrl;
      --
      spi_cmd_high    <= (others => '0');
      spi_cmd_low     <= (others => '0');
      --
      char_addr       <= (others => '0');
      spi_rx_ps2_data <= (others => '0');
      spi_rx_ps2_we   <= '0';
      --
    elsif rising_edge(i_clk_ctl) then
      spi_ctrl.reset     <= '0';
      spi_ctrl.reset_vid <= '0';

      if (spi_stb = '1') then
        if (spi_byte_sel(0) = '1') then -- command
          spi_cmd_high <= spi_cmd_high_comb;
          spi_cmd_low  <= spi_cmd_low_comb;

          if (spi_cmd_high_comb(1) = '1') then -- 0x1x
            spi_ctrl.reset     <= spi_data(0);
            spi_ctrl.halt      <= spi_data(1);
            spi_ctrl.reset_vid <= spi_data(2);
          end if;

          if (spi_cmd_high_comb(4) = '1') then -- 0x4x
            spi_ctrl.osd_ena      <= spi_data(0);
            spi_ctrl.keyboard_dis <= spi_data(1);
          end if;
        end if;
      end if;

      -- OSD Char address load/count
      if (spi_stb = '1') then
        if (spi_byte_sel(0) = '1') then -- command
          if (spi_cmd_high_comb(12) = '1') then
            -- set row
            char_addr(10 downto 7) <= spi_data(3 downto 0); -- 5..0 possible
            char_addr(0)           <= '0';
          end if;
        elsif (spi_cmd_high(12) = '1') then
          if (spi_byte_sel(1) = '1') then -- W0
            char_addr( 6 downto 0) <= spi_data(5 downto 0) & '0';
          else
            char_addr <= char_addr + "1";
          end if;
        end if;
      end if;

      -- config write
      if (spi_stb = '1') then -- not command
        if (spi_cmd_high(2) = '1') then -- 0x2x
          --
          if (spi_cmd_low(0) = '1') then
            for i in 0 to 3 loop if (spi_byte_sel(i+1) = '1') then spi_ctrl.cfg_static (i*8+7 downto i*8) <= spi_data; end if; end loop;
          end if;

          if (spi_cmd_low(1) = '1') then
            for i in 0 to 3 loop if (spi_byte_sel(i+1) = '1') then spi_ctrl.cfg_dynamic(i*8+7 downto i*8) <= spi_data; end if; end loop;
          end if;

          if (spi_cmd_low(2) = '1') then
            for i in 0 to 0 loop if (spi_byte_sel(i+1) = '1') then spi_ctrl.cfg_global (i*8+7 downto i*8)  <= spi_data; end if; end loop;
          end if;

          if (spi_cmd_low(3) = '1') then
            for i in 0 to 1 loop if (spi_byte_sel(i+1) = '1') then spi_ctrl.cfg_ctrl   (i*8+7 downto i*8) <= spi_data; end if; end loop;
          end if;

          if (spi_cmd_low(8) = '1') then
            if (spi_byte_sel(1) = '1') then
              spi_ctrl.cfg_fileio_cha.writable <= spi_data(7 downto 4);
              spi_ctrl.cfg_fileio_cha.inserted <= spi_data(3 downto 0);
            end if;
          end if;

          if (spi_cmd_low(9) = '1') then
            if (spi_byte_sel(1) = '1') then
              spi_ctrl.cfg_fileio_chb.writable <= spi_data(7 downto 4);
              spi_ctrl.cfg_fileio_chb.inserted <= spi_data(3 downto 0);
            end if;
          end if;
          --
        end if;
      end if;

      -- PS/2 from ARM
      spi_rx_ps2_we      <= '0';
      if (spi_stb = '1') then -- not command
        if (spi_cmd_high(3) = '1') then -- 0x3x
          if (spi_byte_sel(1) = '1') then
            spi_rx_ps2_data <= spi_data(7 downto 0);
            spi_rx_ps2_we   <= '1';
          end if;
        end if;
      end if;

    end if;
  end process;

  o_kb_inhibit      <= spi_ctrl.keyboard_dis;
  --
  o_cfg_dynamic     <= spi_ctrl.cfg_dynamic;
  o_cfg_global      <= spi_ctrl.cfg_global;
  o_cfg_fileio_cha  <= spi_ctrl.cfg_fileio_cha;
  o_cfg_fileio_chb  <= spi_ctrl.cfg_fileio_chb;
  o_cfg_ctrl        <= spi_ctrl.cfg_ctrl;
  --
  o_rst_soft        <= spi_ctrl.reset; -- single clock
  o_halt            <= spi_ctrl.halt; -- single clock

  p_rst_vid : process
  begin
    wait until rising_edge(i_clk_ctl);
    reset_vid_t <= reset_vid_t(4 downto 0) & spi_ctrl.reset_vid;
    o_rst_vid   <= red_or(reset_vid_t);
  end process;

  p_config_static : process(i_clk_ctl, i_rst_ctl_hard)
  begin
    if (i_rst_ctl_hard = '1') then -- note startup reset
      o_cfg_static  <= g_cfg_static;
    elsif rising_edge(i_clk_ctl) then
      if (i_rst_ctl = '1') then -- deassert is sync to clock
        o_cfg_static  <= spi_ctrl.cfg_static;
      end if;
    end if;
  end process;
  --
  char_wena <= '1' when (spi_stb = '1') and (spi_cmd_high(12) = '1') and (spi_byte_sel(1 downto 0) = "00") else '0'; -- not command / W0

  -- 1 lut
  hoff_wena_1    <= '1' when (spi_stb = '1') and (spi_cmd_high(5) = '1') and (spi_cmd_low(0) = '1') and (spi_byte_sel(1) = '1') else '0';
  hoff_wena_2    <= '1' when (spi_stb = '1') and (spi_cmd_high(5) = '1') and (spi_cmd_low(0) = '1') and (spi_byte_sel(2) = '1') else '0';
  voff_wena      <= '1' when (spi_stb = '1') and (spi_cmd_high(5) = '1') and (spi_cmd_low(1) = '1') and (spi_byte_sel(1) = '1') else '0';
  -- only one word is read, update once 1st word read
  spi_tx_fifo_rd <= '1' when (spi_stb = '1') and (spi_cmd_high(0) = '1') and (spi_cmd_low(8) = '1') and (spi_byte_sel(1) = '1') else '0';

  --
  -- OSD
  --

  osd_vid_in <= i_vid_rgb;
  --
  -- auto calc OSD position
  --
  b_osd_pos : block
    signal hs_t           : word(1 downto 0);
    signal vs_t           : word(1 downto 0);
    signal hs             : bit1;
    signal vs             : bit1;

    signal h_count        : word(11 downto 0) := x"000"; -- for sim only
    signal h_size         : word(11 downto 0) := x"359";
    signal v_count        : word(11 downto 0) := x"0C0";
    signal v_size         : word(11 downto 0) := x"0C6";
    signal h_start_pos    : word(11 downto 0);
    signal v_start_pos    : word(11 downto 0);

  begin
    --
    -- osd is 32 cols by 16 rows. char is 16x16
    --
    p_sync : process
    begin
      wait until rising_edge(i_clk_vid);
      if (i_ena_vid = '1') then
        hs_t <= hs_t(0) & i_vid_sync.dig_hs;
        vs_t <= vs_t(0) & i_vid_sync.dig_vs;
      end if;
    end process;

    hs <= '1' when (hs_t = "01") else '0'; -- hsync rising edge detection
    vs <= '1' when (vs_t = "01") else '0'; -- vsync rising edge detection
    
    p_hv_count : process
      variable v_short : bit1; -- set if <320 lines per field
    begin
      wait until rising_edge(i_clk_vid);
      if (i_ena_vid = '1') then

        osd_vexp <= '0';

        if (hs = '1') then
          h_size   <= h_count;
          osd_hexp <= h_count(11);

          h_count <= (others => '0');
        else
          h_count <= h_count + "1";
        end if;

        v_short := '1';
        if (red_or(v_size(11 downto 9)) = '1') or
          (v_size(8 downto 7) = "11") then
          v_short := '0';
        end if;

        if (vs = '1') then
          v_count <= (others => '0');
          v_size  <= v_count;
        elsif (hs = '1') then
          v_count <= v_count + "1";
        end if;
      end if;
    end process;

    -- to do, add auto scale for hi-res
    p_osd_start_calc : process(h_size, v_size, osd_hexp, i_vid_sync.progressive)
    begin
      if (osd_hexp = '1') then
        h_start_pos <= ('0' & h_size(11 downto 1)) - 512 - 4;
      else
        h_start_pos <= ('0' & h_size(11 downto 1)) - 256 - 4;
      end if;

      if (i_vid_sync.progressive = '1') then
        v_start_pos <= ('0' & v_size(11 downto 1)) - 128;
      else
        v_start_pos <= ('0' & v_size(11 downto 1)) - 64;
      end if;

    end process;

    p_osd_start : process
    begin
      wait until rising_edge(i_clk_vid);
      if (i_ena_vid = '1') then
        osd_hstart <= '0';
        if (h_count = h_start_pos) then
          osd_hstart <= '1';
        end if;

        osd_vstart <= '0';
        if (v_count = v_start_pos) then -- even lines only for i to work
          -- osd will ignore a second vstart once it is running
          osd_vstart <= '1';
        end if;
      end if;
    end process;


  end block;
   --{{{
   --cs_trig(62 downto 57) <= (others => '0');
   --cs_trig(56) <= i_vid_sync.oddline;
   --cs_trig(55) <= i_ena_vid;
   --cs_trig(54) <= i_vid_sync.dig_hs;
   --cs_trig(53) <= vs;
   --cs_trig(52) <= hs;
   --cs_trig(51) <= osd_ena;
   --cs_trig(50) <= i_vid_sync.progressive;
   --cs_trig(49) <= osd_vstart;
   --cs_trig(48) <= osd_hstart;
   --cs_trig(47 downto 36) <= v_count;
   --cs_trig(35 downto 24) <= h_count;
   --cs_trig(23 downto 12) <= v_size;
   --cs_trig(11 downto  0) <= h_size;
   --}}}
   --{{{
   --i_icon : icon
      --port map (
        --CONTROL0 => cs_control0
    --);
    --i_ila : ila_1024_63
      --port map (
        --CONTROL => cs_control0,
        --CLK     => cs_clk,
        --TRIG0   => cs_trig);

    ----
    --cs_clk       <= i_clk_vid;
   --}}}

  p_osd_meta : process
  begin
    -- OSD control needs to be on clk_video
    wait until rising_edge(i_clk_vid);
    if (i_ena_vid = '1') then
      osd_ena <= spi_ctrl.osd_ena;
    end if;
  end process;

  u_OSD : entity work.Replay_OSD
    port map (
      i_clk                 => i_clk_vid,
      i_ena                 => i_ena_vid,
      i_rst                 => i_rst_vid,
      --
      i_clk_ctl             => i_clk_ctl,
      i_ctl_din             => spi_data,
      i_ctl_char_addr       => char_addr,
      i_ctl_char_wena       => char_wena,
      i_ctl_hoff_wena_1     => hoff_wena_1,
      i_ctl_hoff_wena_2     => hoff_wena_2,
      i_ctl_voff_wena       => voff_wena,
      --
      i_hstart              => osd_hstart,
      i_vstart              => osd_vstart,
      i_hexp                => osd_hexp,
      i_vexp                => osd_vexp,
      i_stdprog             => i_vid_sync.progressive, -- low for interlaced, in which case V counts by 2
      i_voddline            => i_vid_sync.oddline,     -- starting at 0 or 1 (VOddLine high) - only for osd_prog low
      --
      i_osd_display         => osd_ena,
      i_osd_dim             => '1',
      i_vid_rgb             => osd_vid_in,
      o_vid_rgb             => osd_vid_out
      );

  o_vid_rgb <= osd_vid_out;

  p_video_delay : process
  begin
    wait until rising_edge(i_clk_vid);
    if (i_ena_vid = '1') then
      -- match one clock video delay through OSD
      o_vid_sync <= i_vid_sync;
      o_vbl      <= i_vid_sync.dig_vs; -- active low
    end if;
  end process;

end RTL;

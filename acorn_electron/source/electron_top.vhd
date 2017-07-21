--
-- Copyright Gary Preston 2017
-- All Rights Reserved

--
-- DDR Layout
--
-- 0x0000 - 0x7FFF  RAM Not currently used
-- 0x8000 - 0xFFFF  ROM (some mem mapped i/o interleaved too, see ULA notes)
-- Rest of DDR space unused

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;
entity Electron_Top is
  port (
    ------------------------------------------------------
    -- To Lib
    ------------------------------------------------------

    -- System clock, enable and reset, generated from Clk A
    i_clk_sys             : in  bit1;
    i_ena_sys             : in  bit1;
    i_cph_sys             : in  word( 3 downto 0); -- four phased enables. (3) = ena_sys
    i_rst_sys             : in  bit1;

    i_clk_ram             : in  bit1;
    i_rst_ram             : in  bit1;

    --
    -- Config/Control
    o_cfg_status          : out word(15 downto 0); -- status feedback to ARM
    i_cfg_static          : in  word(31 downto 0);
    i_cfg_dynamic         : in  word(31 downto 0);

    i_tick_1us            : in  bit1; -- on clk_sys with ena
    i_tick_100us          : in  bit1; -- on clk_sys with ena
    i_halt                : in  bit1;
    i_dram_ref_panic      : in  bit1;
    o_rst_soft            : out bit1 := '0';

    -- Joystick
    i_joy_a_l             : in  word( 5 downto 0);
    i_joy_b_l             : in  word( 5 downto 0);

    -- Keyboard
    o_kb_ps2_leds         : out word( 2 downto 0);
    i_kb_ps2_we           : in  bit1;
    i_kb_ps2_data         : in  word( 7 downto 0);
    i_kb_inhibit          : in  bit1; -- OSD active

    -- Mem interface high prio
    o_ddr_hp_fm_core      : out   r_DDR_hp_fm_core;  -- connect to z_DDR_hp_fm_core if not used
    i_ddr_hp_to_core      : in    r_DDR_hp_to_core;

    -- Fileio A
    i_fcha_cfg            : in  r_Cfg_fileio;
    i_fcha_to_core        : in  r_Fileio_to_core;
    o_fcha_fm_core        : out r_Fileio_fm_core;  -- connect to z_Fileio_fm_core if not used

    -- Fileio B
    i_fchb_cfg            : in  r_Cfg_fileio;
    i_fchb_to_core        : in  r_Fileio_to_core;
    o_fchb_fm_core        : out r_Fileio_fm_core;  -- connect to z_Fileio_fm_core if not used

    -- Fileio Mem
    i_memio_to_core       : in  r_Memio_to_core;
    o_memio_fm_core       : out r_Memio_fm_core;   -- connect to z_Memio_fm_core if not used

    -- Video
    o_vid_rgb             : out word(23 downto 0);
    o_vid_sync            : out r_Vidsync;

    -- Audio
    o_audio_l             : out word(23 downto 0); -- left  sample
    o_audio_r             : out word(23 downto 0); -- right sample
    i_audio_taken         : in  bit1;  -- sample ack

    ------------------------------------------------------
    -- Other IO
    ------------------------------------------------------
    o_disk_led            : out bit1;
    o_pwr_led             : out bit1  -- note these are active high outputs
    );
end;

architecture RTL of Electron_Top is

  constant electrontop_cs_enable : boolean := true;
  -- TODO: [Gary] This should come from config.
  --constant cfg_dblscan : bit1 := '1';

  signal led                    : bit1;
  signal tick_pre1              : bit1;
  signal tick                   : bit1;

  -- Scanline doubler
  --signal dbl_hsync_l, dbl_vsync_l, dbl_csync_l, dbl_blank : bit1;
  --signal dbl_rgb   : word(23 downto 0);

  -- addr/data bus shared by ROM, CPU and ULA
  signal addr_bus    : word(15 downto 0);
  signal data_bus    : word( 7 downto 0);

  -- ROM
  signal rom_data    : word( 7 downto 0);
  signal ddr_valid   : bit1;

  -- RAM  
  signal ram_addr    : word( 7 downto 0);
  signal ram_data    : word( 3 downto 0);
  signal ram_n_we    : bit1;
  signal ram_n_ras   : bit1;
  signal ram_n_cas   : bit1;

  -- CPU
  signal cpu_n_w     : bit1;
  signal cpu_data_in  : word(7 downto 0);
  signal cpu_data_out : word(7 downto 0);
  signal cpu_addr     : word(23 downto 0);

  -- ULA
  signal ula_clk     : bit1;
  signal ula_rom_ena : bit1;

  signal ula_phi_out : bit1;
  signal ula_n_irq   : bit1;

  signal ula_caps_lock : bit1;

  -- ULA/Framework extras
  signal ula_n_hsync, ula_n_vsync, ula_n_csync, ula_de : bit1;
  signal ula_rgb     : word(23 downto 0);

  -- ULA Glue
  signal div13       : bit1;
  signal n_por       : bit1;
  signal n_reset     : bit1;

  -- CPU/ULA Glue
  signal n_nmi        : bit1;
  
  -- Keyboard
  signal kbd_n_break  : bit1;
  signal kbd_data     : word(3 downto 0);

begin
  
  o_cfg_status(15 downto  0) <= (others => '0');

  o_rst_soft            <= '0';

  o_fcha_fm_core        <= z_Fileio_fm_core;
  o_fchb_fm_core        <= z_Fileio_fm_core;

  o_memio_fm_core       <= z_Memio_fm_core;

  o_audio_l             <= (others => '0');
  o_audio_r             <= (others => '0');

  -- ====================================================================
  -- Misc
  -- ====================================================================

  -- TODO: [Gary] Should sys_clk be passed into the ULA to allow
  -- div13 and the ula_clk to be used as clock enables rather than new
  -- clocks? Are there timing issues with how it is setup now?

  -- IC9 clock div 13 (74LS163)
  b_clk_div : block
    signal cnt : unsigned( 3 downto 0 ) := (others => '0');
  begin

    p_ic9_div13 : process(i_clk_sys)
    begin
      if rising_edge(i_clk_sys) then
        div13 <= '0';

        cnt <= cnt + 1;        
        if (cnt = 12) then
          cnt <= (others => '0');
          div13 <= '1';
        end if;
      end if;
    end process;

  end block;
  
  -- ====================================================================
  -- RAM
  -- ====================================================================

  -- IC20 RAM 4x64K 1bit
  -- 64k 0x000 - 0x3FFFF
  ram_ic20 : entity work.TM4164EA3_64k_W4
  port map (
    -- clock for sync bram 
    i_clk    => i_clk_sys,

    i_addr   => ram_addr,

    i_data   => ram_data,
    o_data   => ram_data,
  
    i_n_we   => ram_n_we,
    i_n_ras  => ram_n_ras,
    i_n_cas  => ram_n_cas
  );

  -- ====================================================================
  -- CPU
  -- ====================================================================

  -- IC3 T65 (6502-A)
  ic3_6502 : entity work.T65
  port map (
    Mode    => "00",               -- 6502
    Res_n   => n_reset,
    Enable  => '1',
    Clk     => ula_phi_out,
    Rdy     => '1',
    Abort_n => '1',
    IRQ_n   => ula_n_irq,
    NMI_n   => n_nmi,
    SO_n    => '1',
    R_W_n   => cpu_n_w,
    Sync    => open,
    EF      => open,
    MF      => open,
    XF      => open,
    ML_n    => open,
    VP_n    => open,
    VDA     => open,
    VPA     => open,
    A       => cpu_addr,
    DI      => cpu_data_in,
    DO      => cpu_data_out,
    DEBUG   => open
  );

  addr_bus <= cpu_addr(15 downto 0);
  -- multiplex di/do to bi-dir data_bus
  cpu_data_in <= data_bus;
  data_bus <= cpu_data_out when cpu_n_w = '0' else (others => 'Z');
  
  -- ====================================================================
  -- ULA
  -- ====================================================================

  -- TODO: [Gary] Cassette and audio to sort. May be simpler to add extra
  --       signals and alternative path to save converting data
  --       to intermediate form only to then try to convert back again.
  -- IC1 ULA (Uncommitted Logic Array)
  -- Managed RAM, Video, Cassette and Sound
  ula_ic1 : entity work.ULA_12C021
  port map (
    --
    -- Framework extras
    --
    o_n_vsync     => ula_n_vsync,
    o_de          => ula_de,               
    o_rgb         => ula_rgb,

    --
    -- ULA
    --

    -- Cassette I/O (not yet supported)
    i_cas         => '0',
    o_cas         => open,
    b_cas_rc      => open,                      -- RC high tone detection
    o_cas_mo      => open,                      -- Motor relay
       
    -- Audio      (not yet supported)       
    o_sound_op    => open,            
       
    -- Reset             
    i_n_por       => n_por,                     -- /Power on reset
       
    -- Video             
    o_n_csync     => ula_n_csync,               -- h/v sync
    o_n_hsync     => ula_n_hsync,               -- h sync
     
    -- Clock   
    i_clk         => ula_clk,                   -- 16MHz
    i_div_13      => div13,                     -- ula_clk div 13
       
    -- RAM (4x64k 1 bit)       
    b_ram0        => ram_data(0),
    b_ram1        => ram_data(1),
    b_ram2        => ram_data(2),
    b_ram3        => ram_data(3),
       
    o_n_we        => ram_n_we,                  -- /write, read
    o_n_ras       => ram_n_ras,                 -- row address strobe  -ve edge
    o_n_cas       => ram_n_cas,                 -- col address strobe  -ve edge

    o_ra          => ram_addr,                  -- ram address

    -- Keyboard
    i_kbd         => kbd_data,
    o_caps_lock   => ula_caps_lock,
    i_n_reset     => n_reset,

    -- ROM/CPU addressing
    o_rom         => ula_rom_ena,               -- rom select enable   
    i_addr        => addr_bus,
    b_pd          => data_bus,                  -- CPU/ROM data

    -- CPU
    i_n_nmi       => n_nmi,                     -- 1MHz RAM access detection
    o_phi_out     => ula_phi_out,               -- CPU clk, 2MHz, 1MHz or stopped
    o_n_irq       => ula_n_irq,
    i_n_w         => cpu_n_w                    -- Data direction, /write, read
  );

  -- TODO: [Gary] If DDR is to be shared between ROM and RAM more thought will need to be given
  -- to timing as ULA assumes the two are on separate buses. Access to RAM is
  -- at 2MHz max so there is ample time to interleave the two.
  p_por : process(i_clk_sys, i_rst_sys, i_halt)
  begin
    if (i_rst_sys = '1' or i_halt = '1') then
      n_por <= '0';
    elsif rising_edge(i_clk_sys) then
      -- Delay bringing out of reset until after cph(1) to align even ula_clk
      -- cycles with cph(3). DDR can then be accessed every other even ula_clk cycle
      if (i_cph_sys(2) = '1') then
        n_por <= '1';
      end if;
    end if;
  end process;

  -- ULA is supposed to drive n_reset for the POR case but unless
  -- the ULA needs to drive n_reset in any other case just handle it here
  n_reset <= n_por and kbd_n_break;

  -- TODO: [Gary] not keen on using POR in this way as ula_clk should
  -- be running all the time. Switch to a one off flag set during
  -- sys reset instead?
  -- 16MHz from sys_clk / 2. ula_clk sync'd to cph(3) via por
  ula_clk <= i_cph_sys(1) or i_cph_sys(3) when n_por = '1' else '0';
  
  o_vid_rgb <= ula_rgb;

  o_vid_sync.dig_de <= ula_de;
  o_vid_sync.dig_hs <= ula_n_hsync;
  o_vid_sync.dig_vs <= ula_n_vsync;

  -- Analog
  -- TODO: [Gary] When doubling output separate h & v sync. Otherwise use csync
  o_vid_sync.ana_de <= ula_de;
  o_vid_sync.ana_hs <= ula_n_csync;
  o_vid_sync.ana_vs <= '1';

  -- ====================================================================
  -- Input
  -- ====================================================================
  
  -- Framework PS2 to ULA format.
  u_ps2_translate : entity work.PS2_Translate
    port map (
      i_clk_sys         => i_clk_sys,
      i_rst_sys         => i_rst_sys,

      -- Keyboard framework interface
      i_kb_ps2_we       => i_kb_ps2_we,
      i_kb_ps2_data     => i_kb_ps2_data,
      i_kb_inhibit      => i_kb_inhibit,

      -- Electron keyboard interface
      i_addr            => addr_bus(13 downto 0),
      o_data            => kbd_data,
      o_n_break         => kbd_n_break
    );
  
  -- caps, num, scroll lock
  o_kb_ps2_leds <= ula_caps_lock & "00";

  -- ====================================================================
  -- Framwork Interfacing
  -- ====================================================================
  
  --
  -- DDR 
  --
  -- IC2 ROM 32kB (addressable via ARM bus)
  -- Original ROM, Hitatchi HN613256 with tri-state output buffer
  -- DDR ROM addressable 0x8000 - 0xFFFF
  -- 
  -- NOTE: DDR samples address after sys_ena and will present result in time
  -- for following sys_ena 4 sys_clk ticks later. 
  -- i.e samples on i_cph_sys(0), result by i_cph_sys(3)
  -- ULA controller ticks twice per ena_sys and must thus ensure at least
  --
  -- 32kB of ROM, 4 bytes returned per read (only 1 used each access)
  o_ddr_hp_fm_core.valid  <= ddr_valid;
  o_ddr_hp_fm_core.burst  <= "00";
  o_ddr_hp_fm_core.addr(25 downto 16) <= (others => '0');
  o_ddr_hp_fm_core.addr(15 downto  2) <= addr_bus(15 downto 2); 

  -- read only
  o_ddr_hp_fm_core.rw_l   <= '1';
  o_ddr_hp_fm_core.w_be   <= "1111";
  o_ddr_hp_fm_core.w_data <= (others => '0');

  p_program_rom : process(i_clk_sys, i_rst_sys, i_halt)
  begin
    if (i_rst_sys = '1' or i_halt = '1') then
      ddr_valid <= '0';
      rom_data <= (others => '0');
    elsif rising_edge(i_clk_sys) then
      -- DDR access takes 4 clk_sys cycles. ula runs at clk_sys/2 (16MHz) and generates
      -- ula_phi_out at 0, 1 or 2MHz aligned to cph_sys(3). Room for 4 full DDR accesses    
      -- @ 2MHz or 8 @ 1MHz. Ample spare time to interleave ULA RAM access.
      if (i_ena_sys = '1') then
        if (ddr_valid = '1') then
          ddr_valid <= '0';
          -- note, default is big endian in the DRAM controller
          rom_data <= i_ddr_hp_to_core.r_data(31 downto 24);
          case addr_bus(1 downto 0) is
            when "00" => rom_data <= i_ddr_hp_to_core.r_data(31 downto 24);
            when "01" => rom_data <= i_ddr_hp_to_core.r_data(23 downto 16);
            when "10" => rom_data <= i_ddr_hp_to_core.r_data(15 downto  8);
            when "11" => rom_data <= i_ddr_hp_to_core.r_data( 7 downto  0);
            when others => null;
          end case;
        end if;

        if (ula_phi_out = '1' and ula_rom_ena = '1') then
          ddr_valid <= '1';
        end if;
      end if;
    end if;
  end process;

  -- rom data tri-state via OE
  data_bus <= rom_data when ula_rom_ena = '1' else (others => 'Z');

  -- Cassette i/o adapter
  -- Support loading via SD card rather than physical cassette.
  -- Should be optional and route additional signals to allow a physical
  -- cassette to be interfaced via expansion port.

  -- Expansion roms

  -- Audio adapter

  
  --
  -- Scanline Doubling
  --
  -- TODO: [Gary] Sort high BRAM usage due to RAM/ROM before enabling this.
  --  u_DblScan : entity work.Replay_DblScan
  --  port map (
  --    -- TODO: [Gary] clk_sys is 2x video generation, sufficient for doubler?
  --    -- clocks
  --    --i_clk                 => i_clk_sys,
  --    --i_ena                 => '1', 
  --    --i_rst                 => i_rst_sys,
  --
  --    --
  --    i_bypass              => '1',
  --    i_dblscan             => cfg_dblscan,
  --    --
  --    i_hsync_l             => dig_hsync,
  --    i_vsync_l             => dig_vsync,
  --    i_csync_l             => csync_l,  -- passed through
  --    i_blank               => dig_de,   -- passed through
  --    i_vid_rgb             => x"FF0000", -- r 23..16 g 15..8 b 7..0
  --    --
  --    o_hsync_l             => dbl_hsync_l,
  --    o_vsync_l             => dbl_vsync_l,
  --    o_csync_l             => dbl_csync_l,
  --    o_blank               => dbl_blank,
  --    o_vid_rgb             => dbl_rgb
  --  );

  --
  -- Activity LEDs
  --
  b_tick : block
    signal precounter1 : word(15 downto 0);
    signal precounter2 : word(11 downto 0);
  begin

    p_count : process(i_clk_sys, i_rst_sys)
    begin
      if (i_rst_sys = '1') then
        precounter1 <= (others => '0');
        precounter2 <= (others => '0');
        tick_pre1   <= '0';
        tick        <= '0';
      elsif rising_edge(i_clk_sys) then

        if (i_ena_sys = '1') then
          precounter1 <= precounter1 - "1";

          tick_pre1 <= '0';
          if (precounter1 = x"0000") then
            tick_pre1 <= '1';
          end if;
          -- synopsys translate_off
          tick_pre1 <= '1';
          -- synopsys translate_on

          tick <= '0';
          if (tick_pre1 = '1') then
            if (precounter2 = x"000") then
              precounter2 <= x"19B";
              tick <= '1';
            else
              precounter2 <= precounter2 - "1";
            end if;
          end if;
        end if;
      end if;
    end process;
  end block;

  p_flash : process(i_clk_sys, i_rst_sys)
  begin
    if (i_rst_sys = '1') then
      led <= '0';
    elsif rising_edge(i_clk_sys) then
      if (i_ena_sys = '1') then
        if (tick = '1') then
          led  <= not led;
        end if;
      end if;
    end if;
  end process;

  o_disk_led        <= led;
  o_pwr_led         <= not led;

  -- ====================================================================
  -- Chipscope
  -- ====================================================================

  cs_debug : block
    component icon
      PORT (
        CONTROL0 : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0)
        );
      end component;

    component ila_1024_63
      PORT (
        CONTROL : INOUT STD_LOGIC_VECTOR(35 DOWNTO 0);
        CLK : IN STD_LOGIC;
        TRIG0 : IN STD_LOGIC_VECTOR(62 DOWNTO 0)
        );
    end component;

    signal cs_clk  : bit1;
    signal cs_ctrl : word(35 downto 0);
    signal cs_trig : word(62 downto 0);

  begin -- cs_debug

    electrontop_cs : if electrontop_cs_enable=true generate

      cs_icon : icon
      port map (
        CONTROL0 => cs_ctrl
        );

      cs_ila : ila_1024_63
      port map (
        CONTROL => cs_ctrl,
        CLK     => cs_clk,
        TRIG0   => cs_trig
        );

      cs_clk  <= i_clk_ram;
      cs_trig(62) <= i_clk_sys;
      cs_trig(61) <= i_ena_sys;
      cs_trig(60) <= ula_clk;
      cs_trig(59) <= ula_phi_out;
      cs_trig(58) <= ula_rom_ena;
      cs_trig(57 downto 42) <= addr_bus;
      cs_trig(41 downto 38) <= "0000"; -- kbd_data;
      cs_trig(37 downto 34) <= (others => '0');
      cs_trig(33 downto 26) <= data_bus;

      -- RAM
      cs_trig(25 downto 18) <= ram_addr;
      cs_trig(17 downto 14) <= ram_data;
      cs_trig(13) <= ram_n_we;
      cs_trig(12) <= ram_n_ras;
      cs_trig(11) <= ram_n_cas;
      -- ROM
      cs_trig(10 downto 3) <= rom_data;
      cs_trig(2) <= cpu_n_w;
      cs_trig(1 downto 0) <= (others => '0');
    end generate electrontop_cs;

  end block cs_debug;
end RTL;
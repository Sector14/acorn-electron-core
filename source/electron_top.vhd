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
    o_pwr_led             : out bit1;  -- note these are active high outputs

    o_sound_op            : out bit1;
    o_debug               : out word(15 downto 0)
    );
end;

architecture RTL of Electron_Top is

  constant electrontop_cs_enable : boolean := true;
  
  -- Config
  signal cfg_dblscan : bit1;
  signal cfg_vid_compatible : boolean;
  signal cfg_plus1_attached : boolean;
  signal cfg_cas_play, cfg_cas_rec, cfg_cas_ffwd, cfg_cas_rwnd : bit1;

  -- LED Blink
  signal led         : bit1;
  signal tick_pre1   : bit1;
  signal tick        : bit1;

  -- Scanline doubler
  signal dbl_hsync_l, dbl_vsync_l, dbl_csync_l, dbl_blank : bit1;
  signal dbl_rgb   : word(23 downto 0);

  -- addr/data bus shared by ROM, CPU and ULA
  signal addr_bus    : word(15 downto 0);
  signal data_bus    : word( 7 downto 0);

  -- ROM
  signal rom_data     : word( 7 downto 0);
  signal ddr_valid    : bit1;
  signal ddr_rom_page : word(3 downto 0);

  -- RAM  
  signal ram_addr    : word( 7 downto 0);
  signal ram_data    : word( 3 downto 0);
  signal ram_n_we    : bit1;
  signal ram_n_ras   : bit1;
  signal ram_n_cas   : bit1;

  -- CPU
  signal cpu_n_w      : bit1;
  signal cpu_data_in  : word(7 downto 0);
  signal cpu_data_out : word(7 downto 0);
  signal cpu_addr     : word(23 downto 0);

  -- ULA
  signal ena_ula     : bit1;
  signal ula_rom_ena : bit1;

  signal ula_ena_phi_out : bit1;
  signal ula_n_irq   : bit1;

  signal ula_n_reset_in   : bit1;
  signal ula_n_reset_out  : bit1;

  signal ula_caps_lock : bit1;

  signal ula_r, ula_b, ula_g : bit1;

  signal ula_cas_i, ula_cas_o, ula_cas_mo : bit1;

  signal ula_sound_o : bit1;

  -- ULA/Framework extras
  signal ula_n_hsync, ula_n_vsync, ula_n_csync, ula_de : bit1;   
  signal ula_rgb : word(23 downto 0);

  -- ULA Glue
  signal div13       : bit1;
  signal n_por       : bit1;
  signal audio_filter_in_s  : signed(15 downto 0);
  signal audio_filter_lpf_s : signed(15 downto 0);
  signal audio_filter_out_s : signed(15 downto 0);

  -- CPU/ULA Glue
  signal n_nmi        : bit1;
  
  -- Keyboard
  signal kbd_n_break  : bit1;
  signal kbd_data     : word(3 downto 0);

  --
  -- Expansion HW
  -- 

  -- Plus1
  signal plus1_n_oe, plus1_n_oe2, plus1_n_oe3, plus1_n_oe4 : bit1;
  signal plus1_rom_qa : bit1;

  -- Debug
  signal debug        : word(15 downto 0);
begin
    
  o_cfg_status(15 downto  0) <= (others => '0');

  o_rst_soft            <= '0';
  
  o_fchb_fm_core        <= z_Fileio_fm_core;

  o_memio_fm_core       <= z_Memio_fm_core;

  -- Config
  cfg_dblscan           <= i_cfg_dynamic(0);
  cfg_vid_compatible    <= i_cfg_dynamic(5) = '1';
  cfg_plus1_attached    <= i_cfg_dynamic(6) = '1';

  cfg_cas_play          <= i_cfg_dynamic(1);
  cfg_cas_rec           <= i_cfg_dynamic(2);
  cfg_cas_ffwd          <= i_cfg_dynamic(3);
  cfg_cas_rwnd          <= i_cfg_dynamic(4);

  -- ====================================================================
  -- Misc
  -- ====================================================================

  -- IC9 clock div 13 (74LS163)
  p_ic9_div13 : process(i_clk_sys, i_rst_sys)
    variable cnt : integer range 0 to 13;
  begin
    if (i_rst_sys = '1') then
      cnt := 0;
    elsif rising_edge(i_clk_sys) then
      div13 <= '0';
      
      -- cph 0 & 2 to align div13 with ena_ula cph 1 & 3
      if (i_cph_sys(0) = '1' or i_cph_sys(2) = '1') then
        cnt := cnt + 1;        
        if (cnt = 13) then
          cnt := 0;
          div13 <= '1';
        end if;
      end if;
    end if;
  end process;
  
  -- ====================================================================
  -- RAM
  -- ====================================================================

  -- IC20 RAM 4x64K 1bit (0x0 - 0x3FFFF)
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
    Res_n   => ula_n_reset_out,
    Enable  => ula_ena_phi_out,
    Clk     => i_clk_sys,
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

  -- IC1 ULA (Uncommitted Logic Array)
  -- Managed RAM, Video, Cassette and Sound
  ula_ic1 : entity work.ULA_12C021
  port map (
    --
    -- Framework extras
    --
    o_n_vsync     => ula_n_vsync,
    o_de          => ula_de,               
    
    i_compatible  => cfg_vid_compatible,

    -- Clock   
    i_clk_sys     => i_clk_sys,
    i_cph_sys     => i_cph_sys,
    i_ena_ula     => ena_ula,                   -- 1 in 2, 16MHz
    i_ena_div13   => div13,                     -- ena_ula div 13

    --
    -- ULA
    --

    -- Cassette I/O
    i_cas         => ula_cas_i,
    o_cas         => ula_cas_o,
    b_cas_rc      => open,                      -- Not implemented
    o_cas_mo      => ula_cas_mo,                -- Motor relay
       
    -- Audio
    o_sound_op    => ula_sound_o,            
       
    -- Reset             
    i_n_por       => n_por,                     -- /Power on reset
       
    -- Video             
    o_n_csync     => ula_n_csync,               -- h/v sync
    o_n_hsync     => ula_n_hsync,               -- h sync
    o_red         => ula_r,
    o_green       => ula_g,
    o_blue        => ula_b,
            
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
    i_n_reset     => ula_n_reset_in,
    o_n_reset     => ula_n_reset_out,

    -- ROM/CPU addressing
    o_rom         => ula_rom_ena,               -- rom select enable   
    i_addr        => addr_bus,
    b_pd          => data_bus,                  -- CPU/ROM data

    -- CPU
    i_n_nmi       => n_nmi,                     -- 1MHz RAM access detection
    o_ena_phi_out => ula_ena_phi_out,           -- CPU clk enable, 2MHz, 1MHz or stopped
    o_n_irq       => ula_n_irq,
    i_n_w         => cpu_n_w,                   -- Data direction, /write, read

    o_debug       => open
  );

  -- 1 bit r,g,b to 24 bit
  ula_rgb <= (23 downto 16 => ula_r) &
             (15 downto 8  => ula_g) &
             (7  downto 0  => ula_b);

  p_por : process(i_clk_sys, i_rst_sys, i_halt, kbd_n_break)
  begin
    if (i_rst_sys = '1' or i_halt = '1') then
      n_por <= '0';
      ula_n_reset_in <= '0';
    elsif (kbd_n_break = '0') then
      ula_n_reset_in <= '0';
    elsif rising_edge(i_clk_sys) then
      -- soft or hard reset
      -- Ensure first ULA clock tick occurs on cph(3) to align even ena_ula
      -- cycles with cph(3). DDR can then be accessed every other even ena_ula cycle
      if (i_cph_sys(2) = '1') then
        n_por <= '1';
        if (kbd_n_break = '1') then
          ula_n_reset_in <= '1';
        end if;
      end if;

    end if;
  end process;

  -- Orig ULA would clock regardless of por, this is a compromise for DDR access
  -- 16MHz from sys_clk / 2
  ena_ula <= i_cph_sys(1) or i_cph_sys(3);


  -- ====================================================================
  -- Plus 1 Expansion
  -- ====================================================================

  expansion_plus1 : entity work.expansion_plus1
  port map (
    -- Framework interfacing
    i_clk_sys      => i_clk_sys,
    o_n_oe         => plus1_n_oe,
    i_joy_a        => i_joy_a_l,
    i_joy_b        => i_joy_b_l,

    -- Expansion port I/O
    i_addr         => addr_bus,
    b_data         => data_bus,

    i_n_rst        => ula_n_reset_out,
    i_n_w          => cpu_n_w,
    i_phiout       => ula_ena_phi_out,

    i_16m_ena      => ena_ula,

    -- ROM Enables
    o_n_oe4       => plus1_n_oe4,     -- SK 1 (far) page 0 or 1
    o_n_oe2       => plus1_n_oe2,     -- SK 2 (near, higher priority) page 2 or 3
    o_n_oe3       => plus1_n_oe3,     -- SK1&2 page 13

    o_rom_qa      => plus1_rom_qa,    -- LSB of xFE05

    o_debug       => debug
  );
  
  -- ====================================================================
  -- Framework Interfacing
  -- ====================================================================
  
  --
  -- Keyboard Input
  --
    
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

  --
  -- DDR 
  --

  -- Electron Memory Map:
  --   0x0000 - 0x7FFF  RAM
  --   0x8000 - 0xBFFF  BASIC/Paged ROM
  --   0xC000 - 0xFFFF  OS ROM (some mem mapped i/o interleaved too, see ULA notes)
  --
  -- IC2 ROM 32kB 0x8000 - 0xFFFF, original ROM, Hitatchi HN613256 with tri-state output buffer
  --
  -- Page register (FE05) 0-15 
  -- Slots 8-11 are occupied by Keyboard/BASIC. Remaining slots may be claimed by
  -- add-on hardware such as carts in the Plus1 using chip enables based on
  -- decoding FE05 address writes.
  --
  -- Chip select is simulated by addressing higher areas of DDR based on current
  -- ROM number paged in. Note, original hw could possibly glitch and multiple
  -- ROMs use the data bus at the same time. This is not replicated (yet).
  -- 
  -- DDR Usage for 0x0000-0xFFFF matches the Electron's 0-15 bit address layout
  -- although ironically, RAM is not yet stored in DDR.
  --
  -- Additional paged ROMs
  -- 0x40000 - 0x7C000   Up to 12 x 16KB (12 x 0x4000) Paged ROMs 
  -- slots 8-11 unused although DDR memory allocated for simplicity of accessing 12-15.
  --
  --
  -- Rest of DDR space unused
  --
  -- NOTE: Paging bits are ignored when ula_rom is active. This is the case for
  -- OS ROM access and anytime BASIC ROM is paged in and accessed. A15-0 can be
  -- used directly as 0x0000-0xFFFF DDR mirrors Electron's 32K RAM + 32K ROM base setup.
  --
  -- NOTE 2: DDR samples address after sys_ena and will present result in time
  -- for following sys_ena 4 sys_clk ticks later. 
  -- i.e samples on i_cph_sys(0), result by i_cph_sys(3)
  -- ULA controller ticks twice per ena_sys and must thus ensure at least
  -- two ula ticks have occurred between the request and result. Whilst
  -- data is available in the process below on the 2nd ULA tick, it will
  -- take one more sys clk to become visible to the ULA, in effect requests by the
  -- ULA on phase 0 will have data visible to the ULA (via rom_data) by phase 3.
  --
  -- Timing allows 1 DDR access for CPU or ULA and room for 3 further accesses
  -- before ULA would require another access followed again by 3 spare accesses.
  --

  -- Plus 1 chip select decoding to ROM page
  ddr_rom_page <= "0000" when  plus1_n_oe4 = '0' and plus1_rom_qa = '0' else -- page 0
                  "0001" when  plus1_n_oe4 = '0' and plus1_rom_qa = '1' else -- page 1
                  "0010" when  plus1_n_oe2 = '0' and plus1_rom_qa = '0' else -- page 2
                  "0011" when  plus1_n_oe2 = '0' and plus1_rom_qa = '1' else -- page 3
                  "1100" when  plus1_n_oe = '0' else                         -- page 12
                  "1101" when  plus1_n_oe3 = '0';                            -- page 13

  -- 32kB of ROM, 4 bytes returned per read (only 1 used each access)
  o_ddr_hp_fm_core.valid  <= ddr_valid;
  o_ddr_hp_fm_core.burst  <= "00";
  o_ddr_hp_fm_core.addr(25 downto 19) <= (others => '0');
  o_ddr_hp_fm_core.addr(18 downto  2) <= "000" & addr_bus(15 downto 2) when ula_rom_ena = '1' else -- OS/BASIC
                                         '1' & ddr_rom_page & addr_bus(13 downto 2);               -- Paged ROM
  -- read only
  o_ddr_hp_fm_core.rw_l   <= '1';
  o_ddr_hp_fm_core.w_be   <= "1111";
  o_ddr_hp_fm_core.w_data <= (others => '0');

  p_program_rom : process(i_clk_sys, i_rst_sys, i_halt)
    variable rom_page : word(3 downto 0);
  begin
    if (i_rst_sys = '1' or i_halt = '1') then
      rom_data <= (others => '0');
      rom_page := (others => '0');
    elsif rising_edge(i_clk_sys) then
      -- DDR access takes 4 clk_sys cycles. ula runs at clk_sys/2 (16MHz) and generates
      -- ula_ena_phi_out at 0, 1 or 2MHz aligned to cph_sys(3). Room for 4 full DDR accesses    
      -- @ 2MHz or 8 @ 1MHz. Ample spare time to interleave ULA RAM access.
      if (i_ena_sys = '1') then
      
        if (ddr_valid = '1') then
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
      
      end if;
    end if;
  end process;

  -- 4 DDR reads possible when running @2MHz. 3 of which are redundant reads atm
  ddr_valid <= '1' when ula_rom_ena = '1' else
               '1' when cfg_plus1_attached and ((plus1_n_oe and plus1_n_oe2 and plus1_n_oe3 and plus1_n_oe4) = '0') else
               '0';

  -- rom data tri-state when any enabled rom is read
  data_bus <= rom_data when ula_rom_ena = '1' else
              rom_data when cfg_plus1_attached and ((plus1_n_oe and plus1_n_oe2 and plus1_n_oe3 and plus1_n_oe4) = '0') else
              (others => 'Z');

  --
  -- Cassette
  --

  -- Read(and write) bytes from SD using file i/o and serialise/frequency encode
  u_virtual_cassette : entity work.Virtual_Cassette_FileIO
  port map (
    -- Clocks
    i_clk          => i_clk_sys,
    i_ena          => i_ena_sys,
    i_rst          => i_rst_sys,

    -- -- FileIO / Syscon interface
    i_fch_cfg      => i_fcha_cfg,
    i_fch_to_core  => i_fcha_to_core,
    o_fch_fm_core  => o_fcha_fm_core,

    -- Controls
    i_motor        => ula_cas_mo,
    i_play         => cfg_cas_play,
    i_rec          => cfg_cas_rec,
    i_ffwd         => cfg_cas_ffwd,
    i_rwnd         => cfg_cas_rwnd,

    i_cas_to_fch   => ula_cas_o,
    o_cas_fm_fch   => ula_cas_i,

    o_debug        => open
  );

  -- TODO: [Gary] Multiplex i_cas/o_cas aux pins and i_cas_virt/o_cas_virt with ula_cas_i/o

  --
  -- Sound
  --
  p_audio_out : process(i_clk_sys, i_rst_sys)
  begin 
    if i_rst_sys = '1' then
      o_audio_l <= (others => '0');
      o_audio_r <= (others => '0');
    elsif rising_edge(i_clk_sys) then
      if (ena_ula = '1') then
        -- ULA @16MHz supports from 122Hz to 31.25kHz
        -- hpf = sig - lpf(sig)
        o_audio_l <= std_logic_vector(unsigned(audio_filter_in_s - audio_filter_out_s)) & x"00";
        o_audio_r <= std_logic_vector(unsigned(audio_filter_in_s - audio_filter_out_s)) & x"00";
      end if;
    end if;
  end process;

  audio_filter_in_s <= "00" & (13 downto 0 => ula_sound_o);

  -- R61 47ohm lpf
	p_audio_lpf_filter : entity work.rc_bypass
  generic map (
    -- sampling frequency given by clk_i or clk_i/fen_i
    fsamp_MHz => 16.0,
    -- RC configuration in kOhms and uF
    R_kO => 0.047,
    C_uF => 10.0,
    width_ext => 16,
    accurate => true
  )
  port map (
    -- system clock
    clk_i    => i_clk_sys,
    clk_en_i => ena_ula,
    fen_i    => '1',
    byp_i    => '0',
    vbyp_i   => x"0000",
    vin_i    => audio_filter_in_s,
    vout_o   => audio_filter_lpf_s
  );

  -- Really a lpf but used to derive a hpf that simulates sound circuit C11 and 16ohm speaker
	p_audio_hpf_filter : entity work.rc_bypass
  generic map (
    -- sampling frequency given by clk_i or clk_i/fen_i
    fsamp_MHz => 16.0,
    -- RC configuration in kOhms and uF
    R_kO => 0.0160,
    C_uF => 10.0,
    width_ext => 16,
    accurate => true
  )
  port map (
    -- system clock
    clk_i    => i_clk_sys,
    clk_en_i => ena_ula,
    fen_i    => '1',
    byp_i    => '0',
    vbyp_i   => x"0000",
    vin_i    => audio_filter_lpf_s,
    vout_o   => audio_filter_out_s
  );
  
  -- Unfiltered audio available on external aux i/o pin
  o_sound_op <= ula_sound_o;

  --
  -- Scanline Doubling
  --
  u_DblScan : entity work.Replay_DblScan
  port map (
    i_clk                 => i_clk_sys,
    i_ena                 => i_ena_sys,   -- not used 
    i_rst                 => i_rst_sys,

    --
    i_bypass              => '0',
    i_dblscan             => cfg_dblscan,
    --
    i_hsync_l             => not ula_n_hsync,
    i_vsync_l             => not ula_n_vsync,
    i_csync_l             => not ula_n_csync,  -- passed through
    i_blank               => not ula_de,       -- passed through
    i_vid_rgb             => ula_rgb,
    --
    o_hsync_l             => dbl_hsync_l,
    o_vsync_l             => dbl_vsync_l,
    o_csync_l             => dbl_csync_l,
    o_blank               => dbl_blank,
    o_vid_rgb             => dbl_rgb
  );

  -- Digital (using analog timings but with separate syncs)
  o_vid_sync.dig_de <= not dbl_blank;
  o_vid_sync.dig_hs <= dbl_hsync_l;
  o_vid_sync.dig_vs <= dbl_vsync_l;

  -- Analog
  o_vid_sync.ana_de <= not dbl_blank;
  o_vid_sync.ana_hs <= dbl_hsync_l when cfg_dblscan = '1' else dbl_csync_l;
  o_vid_sync.ana_vs <= dbl_vsync_l when cfg_dblscan = '1' else '1';

  o_vid_rgb <= dbl_rgb;

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
              precounter2 <= x"00B"; -- x"19B";
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
          led  <= ula_cas_i or ula_cas_o;
        end if;
      end if;
    end if;
  end process;

  o_disk_led        <= led;
  o_pwr_led         <= ula_n_reset_out;

  o_debug <= debug;

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

      cs_clk  <= i_clk_sys;

      cs_trig(62 downto 40) <= (others => '0');
      cs_trig(39 downto 32) <= data_bus;
      cs_trig(31 downto 16) <= addr_bus;
      cs_trig(15 downto 0) <= debug(15 downto 0);
    end generate electrontop_cs;

  end block cs_debug;
end RTL;
--
-- WWW.FPGAArcade.COM
--
-- REPLAY Retro Gaming Platform
-- No Emulation No Compromise
--
-- All rights reserved
-- Mike Johnson
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

--
-- RAM and ROM  could be moved to DRAM but using BRAM for simplicity. 
-- Keep eye on timing if moved as ULA runs at 16MHz which is sys_clk / 2
-- whilst DRAM is set for a sys_clk / 4 read cycle. May not be an issue
-- however as internally ULA will be generating a 2MHz and 1MHz clock.

library ieee;
  use ieee.std_logic_1164.all;
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

    -- Video clock, generated from Clk C. Used for all OSD Video/PHY data path.
    i_clk_vid             : in  bit1;
    i_rst_vid             : in  bit1;
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

    -- Video (clk_vid)
    o_vid_rgb             : out word(23 downto 0);
    o_vid_sync            : out r_Vidsync;

    -- Audio (clk_aud)
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

  signal led                    : bit1;
  signal tick_pre1              : bit1;
  signal tick                   : bit1;


  -- addr/data bus shared by ROM, CPU and ULA
  signal addr_bus  : word(15 downto 0);
  signal data_bus  : word( 7 downto 0);

  -- ROM
  signal rom_data  : word( 7 downto 0);

  --
  -- ULA
  --
  signal ula_clk   : bit1;
  signal div13     : bit1;
  signal n_por     : bit1;
  signal n_reset   : bit1;
  signal rom_ena   : bit1;
begin
  
  --
  -- Replay Lib
  -- 

  o_cfg_status(15 downto  0) <= (others => '0');

  o_rst_soft            <= '0';
  o_kb_ps2_leds         <= "000";

  o_fcha_fm_core        <= z_Fileio_fm_core;
  o_fchb_fm_core        <= z_Fileio_fm_core;

  o_audio_l             <= (others => '0');
  o_audio_r             <= (others => '0');

  o_vid_rgb             <= (others => '0');

  -- TODO: How to reconcile this and ULA h/v sync?
  u_VideoTiming : entity work.Replay_VideoTiming
    generic map (
      g_enabledynamic       => '0',
      g_param               => c_Vidparam_720x480p_60
      )
    port map (
      i_clk                 => i_clk_vid,
      i_ena                 => '1',
      i_rst                 => i_rst_vid,
      --
      i_param               => c_Vidparam_720x480p_60,
      i_sof                 => '0',
      i_f2_flip             => '0',
      --
      o_hactive             => open,
      o_hrep                => open,
      o_vactive             => open,
      --
      o_dig_hs              => o_vid_sync.dig_hs,
      o_dig_vs              => o_vid_sync.dig_vs,
      o_dig_de              => o_vid_sync.dig_de,
      o_dig_ha              => open,
      o_dig_va              => open,
      o_dig_sof             => open,
      o_dig_sol             => open,
      o_ana_hs              => o_vid_sync.ana_hs,
      o_ana_vs              => o_vid_sync.ana_vs,
      o_ana_de              => o_vid_sync.ana_de,
      --
      o_hpix                => open,
      o_vpix                => open,
      --
      o_f2                  => open,
      o_voddline            => open,
      o_stdprog             => open
      );

  -- some blinking LED thingy...
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

  o_disk_led        <=     led;
  o_pwr_led         <= not led;


  --
  -- Acorn Electron Specific
  --

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
  
  -- ROM 32kB (addressable via ARM bus)
  -- 0x000 - 0x7FFF
  -- Hitatchi HN613256 with tri-state output buffer
  -- Ignored /CS tied to gnd.
  rom_ic2 : entity work.RAM_D32K_W8
  generic map (
    g_addr => x"00000000",
    g_mask => x"00008000"
  )
  port map (
    -- ARM interface
    i_memio_to_core  => i_memio_to_core,  -- not used
    i_memio_fm_core  => z_Memio_fm_core,  -- first module
    o_memio_fm_core  => o_memio_fm_core,  

    i_clk_sys  => i_clk_sys,              -- ARM clock
    i_ena_sys  => i_ena_sys,

    -- Core interface
    i_addr  => addr_bus(14 downto 0),
    i_data  => x"00",                     -- ROM unused
    i_wen   => '0',                       -- ROM unused
    o_data  => rom_data,

    -- TODO: should this really be CPU out clock?
    i_ena   => i_ena_sys,
    i_clk   => i_clk_sys                  -- Core clock
  );
  -- rom data tri-state via OE
  data_bus <= rom_data when rom_ena = '1' else (others => 'Z');

  -- RAM 4x64K 1bit

  -- T65 (6502-A)
    
  -- Keyboard

  -- ULA (Uncommitted Logic Array)
  -- Handles RAM, Video, Cassette and sound
  -- TODO: Finish wiring
  -- ULA uses 16MHz clock, which is sys_clk / 2, bear in mind if ROM/RAM
  -- is moved to DRAM as access time is sys_clk / 4. May not be an issue
  -- however as internally ULA runs at 2MHz and 1MHz.
  ula_ic1 : entity work.ULA_12C021
  port map (
    -- Cassette I/O (not yet supported)
    i_cas         => '0',
    o_cas         => open,
    b_cas_rc      => open,                      -- RC high tone detection
    o_cas_mo      => open,                      -- Motor relay
       
    -- Audio             
    o_sound_op    => open,            
       
    -- Reset             
    i_n_por       => n_por,                     -- /Power on reset
       
    -- Video             
    o_n_csync     => open,                      -- h/v sync
    o_hsync       => open,                      -- h sync
    o_red         => open,            
    o_green       => open,            
    o_blue        => open,            
       
    -- Clock   
    i_clk         => ula_clk,
    i_div_13      => div13,                     -- clk div 13
       
    -- RAM (4x64k 1 bit)       
    b_ram0        => open,                      -- RAM Data ic 0
    b_ram1        => open,                      -- RAM Data ic 1
    b_ram2        => open,                      -- RAM Data ic 2
    b_ram3        => open,                      -- RAM Data ic 3
       
    o_n_we        => open,                      -- /write, read
    o_n_ras       => open,                      -- row address strobe  -ve edge
    o_n_cas       => open,                      -- col address strobe  -ve edge

    o_ra          => open,                      -- ram address

    -- Keyboard
    i_kbd         => "0000",
    i_caps_lock   => '0',
    i_n_reset     => n_reset,

    -- ROM/CPU addressing
    o_rom         => rom_ena,                   -- rom select enable   
    i_addr        => addr_bus,
    b_pd          => data_bus,                  -- CPU/ROM data

    -- CPU
    i_nmi         => '0',                       -- 1MHz RAM access detection
    o_phi_out     => open,                      -- CPU clk, 2MHz, 1MHz or stopped
    o_n_irq       => open,
    i_n_w         => '0'                        -- Data direction, /write, read
  );

  n_por <= not i_halt;
  n_reset <= not i_halt; -- TODO: incl keyboard "break"
  -- 16MHz from sys_clk / 2
  ula_clk <= i_cph_sys(1) or i_cph_sys(3); 

  --
  -- Electron to Lib Adapters
  -- 

  -- Cassette i/o adapter
  -- Audio adapter

  -- TODO: Add Chipscope
  
end RTL;

-- Acorn Electron ULA
-- Ferranti 12C021 Custom
--
-- Copyright Gary Preston 2017
-- All Rights Reserved
--
-- Temporary implementation until reverse engineering of the 12C021 is complete
--
-- Note: This implementation is a compromise between matching the external interface
-- of the real ULA and providing the replay framework with the data it needs to
-- function. A fully pin accurate mapping would likely entail so much extra logic
-- outside the ULA to rebuild data the ULA could just provide. For example
-- outputting one bit RGB vs the full value the framework takes. Likewise for
-- trying to derive the active video signal from just csync and hsync.
--
-- Ram access didn't need that much more logic to remain pin accurate however.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity ULA_12C021 is
  port (

    -- TEMP: Needed by VideoTiming atm. Assume always enabled.
    i_clk_vid     : in bit1;
    i_rst_vid     : in bit1;

    --
    -- Additional framework signals to ease usage
    --
    -- TODO: [Gary] Current setup is PAL 576i analog, so vsync is always 1 and hsync 
    --       is actually csync. Need to either modify replay_videotiming to provide
    --       split h/v even in PAL mode or add custom sig gen
    o_n_vsync     : out bit1;                  -- 
    o_de          : out bit1;                  --

    o_rgb         : out word(23 downto 0);

    --
    -- ULA
    -- 
    
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
    o_n_csync     : out bit1;                  -- h/v sync  (low during horizontal or vertical synchronisation)
    o_n_hsync     : out bit1;                  -- h sync    
    --o_red         : out bit1;            
    --o_green       : out bit1;            
    --o_blue        : out bit1; 
       
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
  -- Framework Video
  signal ana_hsync, ana_vsync, ana_de : bit1;
  signal dig_hsync, dig_vsync, dig_de : bit1;

  signal vpix, hpix : word(13 downto 0);

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


  --
  -- Master Timing
  --
  -- 4MHz, 2MHz generator


  --
  -- Video Timing
  --
  -- Modes:
  --   0 - 640x256 two colour gfx, 80x32 text (20K)
  --   1 - 320x256 four colour gfx, 40x32 text (20K)
  --   2 - 160x256 sixteen colour gfx, 20x32 text (20K)
  --   3 - 80x25 two colour text gfx
  --   4 - 320x256 two colour gfx, 40x32 text (10K)
  --   5 - 160x256 four colour gfx, 20x32 text (10K)
  --   6 - 40x25 two colour text (8K)

  -- TODO: [Gary] Switch to using sys clock instead of separate video clock.
  --       timing changes?

  -- Temp use of VideoTiming to generate a 576i.
  u_VideoTiming : entity work.Replay_VideoTiming
    generic map (
      g_enabledynamic       => '0',
      g_param               => c_Vidparam_720x287p_50
      )
    port map (
      i_clk                 => i_clk_vid,
      i_ena                 => '1',
      i_rst                 => i_rst_vid,
      --i_clk => ula_clk,
      --i_ena => '1',
      --i_rst => i_rst_sys,
      --
      i_param               => c_Vidparam_720x287p_50,
      i_sof                 => '0',
      i_f2_flip             => '0',
      --
      o_hactive             => open,
      o_hrep                => open,
      o_vactive             => open,
      --
      o_dig_hs              => dig_hsync,
      o_dig_vs              => dig_vsync,
      o_dig_de              => dig_de,
      o_dig_ha              => open,
      o_dig_va              => open,
      o_dig_sof             => open,
      o_dig_sol             => open,
      o_ana_hs              => ana_hsync,
      o_ana_vs              => ana_vsync,
      o_ana_de              => ana_de,
      --
      o_hpix                => hpix,
      o_vpix                => vpix,
      --
      o_f2                  => open,
      o_voddline            => open,
      o_stdprog             => open
      );

  -- TODO: [Gary] Mixing of dig/ana here :( Analog in PAL 576i returns csync as
  -- hsycn and '1' for vsync. However, OSD in Syscon uses vsync to determine display
  -- location so digital h/v passed out for now. This is a cludge for now as the
  -- timing of dig h/v does not match that of analog h/v (or combined csync). This will
  -- be fixed once ULA switched to its own analog timing, or VideoTiming adjusted to
  -- expose analog h/v.
  o_n_hsync <= dig_hsync;
  o_n_vsync <= dig_vsync;
  o_n_csync <= ana_hsync;
  o_de      <= ana_de;

  u_vid_rgb : process(i_clk_vid)
  begin
    if rising_edge(i_clk_vid) then
      o_rgb <= x"000000";
      
      -- 287 visible lines. Using center 256 for output.
      -- 832 visible horizontal, using center 640 with 96 border either side.
      -- In 320 mode use 2 cycles per pixel.
      -- TODO: [Gary] Confirm actual electron output. 
      -- TODO: [Gary] TV sometimes locks onto the display at a slight vertical offset. 
      --       Need to double check timings. Full top overscan + blanking visible whilst
      --       at bottom just the tip of the bottom overscan visible.
      
      if (vpix < 16 or vpix >= 16+256) then
        -- overscan
        o_rgb <= x"FFFFFF";
      elsif (vpix >= 16 and vpix < 16+256) then

        if (hpix >= 96 and hpix < 96+640) then
          o_rgb <= x"FF0000";
        end if;

      end if;

    end if;
  end process;

  --
  -- Ram Interface & Timing
  --
  -- 4164 ram is async, however this implementation is synchronous
  -- An actual ULA would need this interface logic rewriting to match timing requirements

  --
  -- Registers
  --

  -- 
  -- Keyboard Interface
  --

  -- 
  -- Sound Interface
  --

  --
  -- Cassette Interface
  --

end;

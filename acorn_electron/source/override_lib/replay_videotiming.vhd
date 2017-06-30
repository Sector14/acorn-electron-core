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

-- NOTE: This is a temporary file. Included as a replacement for the
-- Replay videotiming to provide a new mode. As lib copies happen before
-- core source copies during build, this will replace the library version
-- allowing testing of a new mode without risking breaking everything in svn.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.std_logic_unsigned.all;
  use ieee.numeric_std.all;

  use work.Replay_Pack.all;
  use work.Replay_VideoTiming_Pack.all;

library UNISIM;
  use UNISIM.Vcomponents.all;

entity Replay_VideoTiming is
  generic (
    g_enabledynamic       : in    bit1; -- '1' uses i_Param, otherwide g_Param is used
    g_param               : in    r_Vidparam_int -- used when dynamic standard selection
    );
  port (
    i_clk                 : in    bit1;
    i_ena                 : in    bit1;
    i_rst                 : in    bit1;
    --
    i_param               : in    r_Vidparam_int; -- used when dynamic standard selection
    i_sof                 : in    bit1; -- reset when changing standards
    i_f2_flip             : in    bit1 := '0'; -- flip f2 above/below f1 for interlaced standards
    --
    o_hactive             : out   word(13 downto 0); -- in clocks
    o_hrep                : out   bit1;
    o_vactive             : out   word(13 downto 0);
    --
    o_dig_hs              : out   bit1; -- h sync
    o_dig_vs              : out   bit1; -- v sync
    o_dig_de              : out   bit1; -- display enable (active video)
    o_dig_ha              : out   bit1; -- h active
    o_dig_va              : out   bit1; -- v active
    o_dig_sof             : out   bit1; -- just before active video
    o_dig_sol             : out   bit1; -- every line, hpix = 0
    o_ana_hs              : out   bit1;
    o_ana_vs              : out   bit1;
    o_ana_de              : out   bit1;
    --
    o_hpix                : out   word(13 downto 0);
    o_vpix                : out   word(13 downto 0); -- frame line correct for i standards
    --
    o_f2                  : out   bit1;              -- high for field 2 if not progressive
    o_voddline            : out   bit1;              -- same as vpix(0) for interlanced standards. High in F2 except for NTSC where high in F1
    o_stdprog             : out   bit1               -- (field & frame line number same if progressive)
    );
end;

architecture RTL of Replay_VideoTiming is

  type r_Vidparam_c_int is record
    h_len         : int_14;
    h_start_act   : int_14;
    h_start_ana   : int_14;
    h_stop_ana    : int_14;
    h_start_sync  : int_14;
    h_stop_sync   : int_14;
    h_start_2nd   : int_14;
    -- analog only
    h_stop_eq1    : int_14;
    h_stop_eq2    : int_14;
    h_stop_bd1    : int_14;
    h_stop_bd2    : int_14;
  end record;

  signal param                  : r_Vidparam_int;
  signal param_c                : r_Vidparam_c_int;
  signal standard_ana           : t_Standard_ana := NONE;
  --
  signal h_cnt                  : word(13 downto 0);
  signal h_sol                  : bit1;
  signal h_eol                  : bit1;
  signal h_act                  : bit1;
  signal h_ana_act              : bit1;
  signal h_vgate_f1             : bit1;
  signal h_vgate_f2             : bit1;

  signal h_sync                 : bit1;
  signal h_sync_l               : bit1;
  signal h_ana_n1               : bit1;
  signal h_ana_n2               : bit1;
  signal h_ana_b1               : bit1;
  signal h_ana_b2               : bit1;

  signal v_cnt                  : word(13 downto 0);
  signal v_sof                  : bit1;
  signal v_act                  : bit1;
  signal v_f2                   : bit1;
  signal v_sync                 : bit1;
  signal v_sync_l               : bit1;
  signal cs_gate                : word(4 downto 0);

  signal act_h_cnt              : word(13 downto 0);
  signal act_v_cnt              : word(13 downto 0);
  signal h_eol_t1               : bit1;

  signal dig_vs                 : bit1;
  signal dig_hs                 : bit1;
  signal dig_de                 : bit1;
  signal dig_ha                 : bit1;
  signal dig_va                 : bit1;
  signal dig_sof                : bit1;
  signal dig_sol                : bit1;
  signal ana_vs                 : bit1;
  signal ana_hs                 : bit1;
  signal ana_cs                 : bit1;
  signal ana_de                 : bit1;

begin

  p_standard_mux : process(i_param)
  begin
    if (g_enabledynamic = '1') then
      param <= i_param;
    else
      param <= g_param;
    end if;
  end process;

  p_standard : process
    variable param_int : r_Vidparam_int;
    variable start_2nd : int_14;
  begin
    wait until rising_edge(i_clk);
    if (i_ena = '1') then
      param_int := param;

      standard_ana <= NONE;
      if    (param_int = c_Vidparam_720x480i_60) then
        standard_ana <= NTSC;
      elsif (param_int = c_Vidparam_720x576i_50) then
        standard_ana <= PAL;
      elsif (param_int = c_Vidparam_720x256p_50) then
        standard_ana <= PAL_P;
      end if;
      --
      -- calc constants here so they are static, this saves a lot of space.
      -- It also lets us use "standard" numbers in the definitions, independent of
      -- our implementation here.
      --
      param_c.h_len        <= param_int.total_h - 2;
      param_c.h_start_act  <= param_int.total_h - param_int.active_h - 1;

      -- pal/ntsc blanking giving 702 active
      param_c.h_start_ana  <= param_int.total_h - param_int.active_h - 1 + 18;
      param_c.h_stop_ana   <= param_int.total_h - 1 - 18;

      param_c.h_start_sync <= param_int.syncp_h - 1;
      param_c.h_stop_sync  <= param_int.syncp_h + param.syncw_h -1;

      -- pal/ntsc numbers are close enough
      -- sd analog only
      start_2nd            := param_int.syncp_h + (param_int.total_h / 2 ) - 1;
      param_c.h_start_2nd  <= start_2nd;

      param_c.h_stop_eq1   <= param_int.syncp_h - 1 + 62;
      param_c.h_stop_eq2   <= start_2nd  + 62;
      param_c.h_stop_bd1   <= param_int.syncp_h - 1 + 736;
      param_c.h_stop_bd2   <= start_2nd  + 736;
    end if;
  end process;

  o_hactive <= std_logic_vector(to_unsigned(param.active_h, 14));
  o_hrep    <= param.rep_h;
  o_vactive <= std_logic_vector(to_unsigned(param.active_v, 14));


  -- SD @ 27MHz
  -- line   1728 = 64uS

  -- active 1440 = 53.3uS (total)
  -- really 702 = 1404 = 52uS non blank
  -- safe is 640/720 x 512/576 lines

  -- EIA/CEA-861 / SMPTE standads
  -- First line is line 1
  -- blanking is at start of line
  p_h_cnt : process(i_clk, i_rst)
  begin
    if (i_rst = '1') then
      h_cnt     <= (others => '0');
      h_eol     <= '0';
      h_sol     <= '0'; -- start of active line
      h_act     <= '0';
      h_sync    <= '1';
      h_sync_l    <= '1';
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        if (i_sof = '1') then
          h_cnt    <= (others => '0');
          h_eol    <= '0';
          h_sol    <= '0';
          h_act    <= '0';
          h_sync   <= not param.syncpol_h;
          h_sync_l <= '1';
        else
          h_eol <= '0';
          if (h_cnt = param_c.h_len) then h_eol <= '1'; end if;

          if (h_eol = '1') then
            h_cnt <= (others => '0');
          else
            h_cnt <= h_cnt + "1";
          end if;

          h_sol <= '0';
          if (h_cnt = param_c.h_start_act) then
            h_act <= '1'; h_sol <= '1';
          elsif (h_eol = '1') then
            h_act <= '0';
          end if;

          if (h_cnt = param_c.h_start_sync) then
            h_sync   <= param.syncpol_h; -- assert
            h_sync_l <= '0';
          elsif (h_cnt = param_c.h_stop_sync) then
            h_sync   <= not param.syncpol_h;
            h_sync_l <= '1';
          end if;
        end if;
      end if;
    end if;
  end process;

  -- only needed for sd analog standards
  p_h_ana : process(i_clk, i_rst)
  begin
    if (i_rst = '1') then
       h_ana_act <= '0';

       h_ana_n1  <= '1';
       h_ana_n2  <= '1';
       h_ana_b1  <= '1';
       h_ana_b2  <= '1';
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        -- for analog output the non-blanked line is often shorter than the digital line
        if (h_cnt = param_c.h_start_ana) then
          h_ana_act <= '1';
        elsif (h_cnt = param_c.h_stop_ana) then
          h_ana_act <= '0';
        end if;
        -- narrow
        if (h_cnt = param_c.h_start_sync) then
          h_ana_n1 <= param.syncpol_h;
        elsif (h_cnt = param_c.h_stop_eq1) then
          h_ana_n1 <= not param.syncpol_h;
        end if;

        if (h_cnt = param_c.h_start_2nd) then
          h_ana_n2 <= param.syncpol_h;
        elsif (h_cnt = param_c.h_stop_eq2) then
          h_ana_n2 <= not param.syncpol_h;
        end if;
        -- broad
        if (h_cnt = param_c.h_start_sync) then
          h_ana_b1 <= param.syncpol_h;
        elsif (h_cnt = param_c.h_stop_bd1) then
          h_ana_b1 <= not param.syncpol_h;
        end if;

        if (h_cnt = param_c.h_start_2nd) then
          h_ana_b2 <= param.syncpol_h;
        elsif (h_cnt = param_c.h_stop_bd2) then
          h_ana_b2 <= not param.syncpol_h;
        end if;
      end if;
    end if;
  end process;

  p_h_comp : process(i_clk, i_rst)
  begin
    if (i_rst = '1') then
      h_vgate_f1 <= '0';
      h_vgate_f2 <= '0';
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        h_vgate_f1 <= '0';
        if (h_cnt = param_c.h_start_sync) then
          h_vgate_f1 <= '1';
        end if;

        h_vgate_f2 <= '0';
        if (h_cnt = param_c.h_start_2nd) then -- check this is always true for interlaced standards
          h_vgate_f2 <= '1';
        end if;
      end if;
    end if;
  end process;

  p_v_cnt : process(i_clk, i_rst)
  begin
    if (i_rst = '1') then
      v_cnt <= "00000000000001";
      -- synopsys translate_off
      v_cnt <= std_logic_vector(to_unsigned(param.total_v - 1, 14));
      -- synopsys translate_on

    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        if (i_sof = '1') then
            v_cnt <= "00000000000001";
        elsif (h_eol = '1') then
          if (v_cnt = param.total_v) then
            v_cnt <= "00000000000001";
          else
            v_cnt <= v_cnt + "1";
          end if;
        end if;
      end if;
    end if;
  end process;

  p_v_comp : process(i_clk, i_rst)
  begin
    if (i_rst = '1') then
      v_sof    <= '0';
      v_act    <= '0';
      v_f2     <= '0';
      v_sync   <= '1';
      v_sync_l <= '1';
      cs_gate  <= "00000";
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        v_sof <= '0';
        if (h_eol = '1') then
          if    (v_cnt = param.fline_f1_v) then
            v_act <= '1'; v_sof <= '1';
          elsif (v_cnt = param.lline_f1_v) then
            v_act <= '0';
          elsif (v_cnt = param.fline_f2_v) and (param.progressive = '0') then
            v_act <= '1'; v_sof <= '1';
          elsif (v_cnt = param.lline_f2_v) and (param.progressive = '0') then
            v_act <= '0';
          end if;

          if    (v_cnt = param.start_f2_v) and (param.progressive = '0') then
            v_f2 <= '1';
          elsif (v_cnt = param.start_f1_v) then
            v_f2 <= '0';
          end if;

          if    (v_cnt = param.fsync_f1_v) then
            v_sync   <= param.syncpol_v; -- assert
            v_sync_l <= '0';
          elsif (v_cnt = param.lsync_f1_v) then
            v_sync   <= not param.syncpol_v;
            v_sync_l <= '1';
          elsif (v_cnt = param.fsync_f2_v) and (param.progressive = '0') then
            v_sync   <= param.syncpol_v; -- assert
            v_sync_l <= '0';
          elsif (v_cnt = param.lsync_f2_v) and (param.progressive = '0') then
            v_sync   <= not param.syncpol_v;
            v_sync_l <= '1';
          end if;

          cs_gate <= "10000"; -- normal,n1,n2,b1,b2
          -- minus one as sampled at h_eol
          if (standard_ana = PAL) then
            if (v_cnt =   625) then cs_gate <= "00011"; end if;  -- actually line 1
            if (v_cnt =   2-1) then cs_gate <= "00011"; end if;
            if (v_cnt =   3-1) then cs_gate <= "00110"; end if;
            if (v_cnt =   4-1) then cs_gate <= "01100"; end if;
            if (v_cnt =   5-1) then cs_gate <= "01100"; end if;

            if (v_cnt = 311-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 312-1) then cs_gate <= "01100"; end if;

            if (v_cnt = 313-1) then cs_gate <= "01001"; end if;
            if (v_cnt = 314-1) then cs_gate <= "00011"; end if;
            if (v_cnt = 315-1) then cs_gate <= "00011"; end if;
            if (v_cnt = 316-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 317-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 318-1) then cs_gate <= "01000"; end if;
            --
            if (v_cnt = 623-1) then cs_gate <= "10100"; end if; -- half line
            if (v_cnt = 624-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 625-1) then cs_gate <= "01100"; end if;
          elsif (standard_ana = PAL_P) then
            if (v_cnt =   312) then cs_gate <= "00011"; end if;  -- actually line 1
            if (v_cnt =   2-1) then cs_gate <= "00011"; end if;
            if (v_cnt =   3-1) then cs_gate <= "00110"; end if;
            if (v_cnt =   4-1) then cs_gate <= "01100"; end if;
            if (v_cnt =   5-1) then cs_gate <= "01100"; end if;

            if (v_cnt = 310-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 311-1) then cs_gate <= "01100"; end if;            
            if (v_cnt = 312-1) then cs_gate <= "01100"; end if; -- extended to full line
          else
            -- NTSC
            if (v_cnt =   525) then cs_gate <= "01100"; end if;  -- line1
            if (v_cnt =   2-1) then cs_gate <= "01100"; end if;
            if (v_cnt =   3-1) then cs_gate <= "01100"; end if;
            if (v_cnt =   4-1) then cs_gate <= "00011"; end if;
            if (v_cnt =   5-1) then cs_gate <= "00011"; end if;
            if (v_cnt =   6-1) then cs_gate <= "00011"; end if;
            if (v_cnt =   7-1) then cs_gate <= "01100"; end if;
            if (v_cnt =   8-1) then cs_gate <= "01100"; end if;
            if (v_cnt =   9-1) then cs_gate <= "01100"; end if;
            --
            if (v_cnt = 263-1) then cs_gate <= "10100"; end if; -- half line
            if (v_cnt = 264-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 265-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 266-1) then cs_gate <= "01001"; end if;
            if (v_cnt = 267-1) then cs_gate <= "00011"; end if;
            if (v_cnt = 268-1) then cs_gate <= "00011"; end if;
            if (v_cnt = 269-1) then cs_gate <= "00110"; end if;
            if (v_cnt = 270-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 271-1) then cs_gate <= "01100"; end if;
            if (v_cnt = 272-1) then cs_gate <= "01000"; end if;
          end if;
        end if;
      end if;
    end if;
  end process;
  -- next clock
  p_sync_gate : process(i_clk, i_rst)
    variable cs : bit1;
  begin
    if (i_rst = '1') then
      dig_vs <= '1';
      dig_hs <= '1';

      dig_de <= '0';
      dig_ha <= '0';
      dig_va <= '0';

      ana_vs <= '1';
      ana_hs <= '1';
      ana_cs <= '1'; -- composite sync
      ana_de <= '0';

      h_eol_t1   <= '0';
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        -- digital syncs (to DAC) always active low
        if (v_f2 = '0') then -- field 1
          if (h_vgate_f1 = '1') then
            ana_vs <= v_sync;
            dig_vs <= v_sync_l;
          end if;
        else
          if (h_vgate_f2 = '1') then
            ana_vs <= v_sync;
            dig_vs <= v_sync_l;
          end if;
        end if;

        ana_hs <= h_sync;
        dig_hs <= h_sync_l;

        dig_de <= h_act and v_act;
        dig_ha <= h_act;
        dig_va <= v_act;

        cs := (h_sync   or (not cs_gate(4)) ) and
              (h_ana_n1 or (not cs_gate(3)) ) and
              (h_ana_n2 or (not cs_gate(2)) ) and
              (h_ana_b1 or (not cs_gate(1)) ) and
              (h_ana_b2 or (not cs_gate(0)) );

        if (standard_ana = NONE) then
          ana_cs <= '0';
          ana_de <= h_act and v_act;
        else
          ana_cs <= cs;
          ana_de <= h_ana_act and v_act;
        end if;

        h_eol_t1 <= h_eol;
      end if;
    end if;
  end process;

  -- active pixel counters
  p_act_cnt : process(i_clk, i_rst)
  begin
    if (i_rst = '1') then
      act_h_cnt <= (others => '0');
      act_v_cnt <= (others => '0');

      dig_sol <= '0';
      dig_sof <= '0';

    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        if (i_sof = '1') then
          act_h_cnt <= (others => '0');
          act_v_cnt <= (others => '0');
          dig_sol <= '0';
          dig_sof <= '0';
        else
          dig_sol <= '0';
          if (h_sol = '1') then
            act_h_cnt <= (others => '0');
            dig_sol <= '1';
          elsif (h_act = '1') then
            act_h_cnt <= act_h_cnt + "1";
          end if;

          dig_sof <= '0';
          if (v_sof = '1') then
            act_v_cnt <= (others => '0');
            dig_sof <= '1';
          elsif (dig_de = '1') and (h_eol_t1 = '1') then
            act_v_cnt <= act_v_cnt + "1";
          end if;
        end if;

      end if;
    end if;
  end process;

  -- all outputs co-timed
  o_dig_hs   <= dig_hs;
  o_dig_vs   <= dig_vs;
  o_dig_de   <= dig_de;
  o_dig_ha   <= dig_ha;
  o_dig_va   <= dig_va;
  o_dig_sof  <= dig_sof;
  o_dig_sol  <= dig_sol;

  -- for CODER -- output registers in phy

  p_ana_sync : process(standard_ana, ana_hs, ana_vs, ana_cs)
  begin
    -- the Ana syncs go out on the VGA connector and to the SD coder
    if (standard_ana = NONE) then
      o_ana_hs <= ana_hs;
      o_ana_vs <= ana_vs;
    else
      o_ana_hs <= ana_cs;
      o_ana_vs <= '1';
    end if;
  end process;

  o_ana_de <= ana_de;
  o_hpix   <= act_h_cnt;
  o_f2     <= v_f2;

  o_stdprog  <= param.progressive;

  p_cur_line : process(act_v_cnt, v_f2, param, i_f2_flip)
  begin
    -- fix up for interlaced standards
    o_voddline  <= '0';

    if (param.progressive = '0') then
      o_vpix(13 downto 1) <= act_v_cnt(12 downto 0);
      o_vpix(0)  <= v_f2;
      o_voddline <= v_f2;
      -- looks like DVI (analog) out needs the flip, but composite/hdmi do not.
      if (i_f2_flip = '1') then
        -- f2 above f1
        o_vpix(0)  <= not v_f2;
        o_voddline <= not v_f2;
      end if;
    else
      o_vpix      <= act_v_cnt;
    end if;
  end process;
end RTL;

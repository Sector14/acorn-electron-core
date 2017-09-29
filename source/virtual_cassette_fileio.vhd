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

-- Interface Frequency based ULA cassette pins with SD Card based 
-- files using Generic FileIO.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

use work.Replay_Pack.all;

entity Virtual_Cassette_FileIO is
  port (
    -- Clocks
    i_clk                : in bit1;
    i_ena                : in bit1;
    i_rst                : in bit1;

    -- FileIO channel
    i_fch_cfg            : in  r_Cfg_fileio;
    i_fch_to_core        : in  r_Fileio_to_core;
    o_fch_fm_core        : out r_Fileio_fm_core;

    -- Tape Controls
    i_motor              : in bit1;
    i_play               : in bit1;
    i_rec                : in bit1;
    i_ffwd               : in bit1;
    i_rwnd               : in bit1
  );
end;

architecture RTL of Virtual_Cassette_FileIO is

begin

  -- Temp disabled until file io hooked up
  o_fch_fm_core <= Z_Fileio_fm_core;

  --
  -- FILEIO
  --
  -- u_FileIO_FCh : entity work.Replay_FileIO_FCh_Generic
  -- port map (
  --   -- clocks
  --   i_clk                 => i_clk_sys,
  --   i_ena                 => i_ena_sys,
  --   i_rst                 => i_rst_sys,

  --   -- FileIO / Syscon interface
  --   i_fch_to_core         => i_fch_to_core,
  --   o_fch_fm_core         => o_fch_fm_core,

  --   -- to user space
  --   i_req                 => fileio_req,
  --   o_ack_req             => fileio_ack_req,
  --   o_ack_trans           => fileio_ack_trans, -- transfer done
  --   o_trans_err           => fileio_trans_err, -- aborted, truncated, seek error

  --   -- below latched on ack
  --   i_dir                 => '0', -- read only
  --   i_chan                => "00",
  --   i_addr                => fileio_addr,
  --   i_size                => fileio_size,
  --   o_size0               => fileio_src_size,

  --   -- Reading
  --   i_fifo_to_core_flush  => '0',
  --   o_fifo_to_core_data   => fileio_data,
  --   i_fifo_to_core_taken  => fileio_taken,
  --   o_fifo_to_core_valid  => fileio_valid,
  --   o_fifo_to_core_level  => fileio_rx_level,
  --   o_fifo_to_core_overfl => fileio_rx_overfl,

  --   -- Writing
  --   i_fifo_fm_core_flush  => '0',
  --   i_fifo_fm_core_data   => (others => '0'),
  --   i_fifo_fm_core_we     => '0',
  --   o_fifo_fm_core_level  => open
  --   );
  
  -- -- request size is 512 16 bits words
  -- fileio_size <= x"0400";

  -- -- Enable/pause data request via the Generic FileIO entity
  -- p_fileio_req : process(i_clk_sys, i_rst_sys)
  -- begin   
  --   if (i_rst_sys = '1') then
  --     fileio_addr <= (others => '0');
  --     fileio_req  <= '0';
  --     fileio_req_state <= S_IDLE;
  --   elsif rising_edge(i_clk_sys) then
  --     if (i_ena_sys = '1') then
  --       fileio_req  <= '0';

  --       if (i_fcha_cfg.inserted(0) = '0') then
  --         fileio_addr <= (others => '0');
  --         fileio_req_state <= S_IDLE;
  --       else
  --         case fileio_req_state is
  --           when S_IDLE =>
  --             if (fileio_rx_level(9) = '0') then -- <hf
  --               fileio_req  <= '1'; -- note, request only sent when ack received
  --             end if;
  --             if (fileio_ack_req = '1') then
  --               fileio_req_state <= S_WAIT;
  --             end if;

  --           when S_WAIT =>
  --             if (fileio_ack_trans = '1') then
  --               if (red_or(fileio_trans_err) = '1') then
  --                 fileio_addr <= (others => '0');
  --               else
  --                 fileio_addr <= fileio_addr + fileio_size;
  --               end if;
  --               fileio_req_state <= S_IDLE;
  --             end if;

  --           when others => null;
  --         end case;
  --       end if;
  --     end if;
  --   end if;
  -- end process;

  -- -- Transfer available sample data from FIFO out to audio subsystem  
  -- p_fileio_reader : process(i_clk_sys, i_rst_sys)
  -- begin
  --   if (i_rst_sys = '1') then
  --           fileio_sample_cnt <= "00";
  --           fileio_taken      <= '0';
  --           sample_audio_l   <= (others => '0');
  --           sample_audio_r   <= (others => '0');
  --   elsif rising_edge(i_clk_sys) then


  --     if (i_ena_sys = '1') then
        
  --       sys_audio_taken_sync_old <= sys_audio_taken_sync;
  --       sys_audio_taken_sync <= audio_taken_sync;
        
  --       fileio_taken <= '0';
  --       case fileio_sample_cnt is
  --         when "00" =>
  --           if (fileio_valid = '1') then
  --             fileio_taken <= '1';
  --             sample_audio_l   <= fileio_data( 7 downto 0) & fileio_data(15 downto 8) & x"00";
  --             --o_audio_l  <= fileio_data(15 downto 0)  & x"00";
  --             fileio_sample_cnt <= "01";
  --           end if;
  --         when "01" => -- wait for taken to update valid
  --             fileio_sample_cnt <= "10";

  --         when "10" =>
  --           if (fileio_valid = '1') then
  --             fileio_taken <= '1';
  --             sample_audio_r   <= fileio_data( 7 downto 0) & fileio_data(15 downto 8) & x"00";
  --             --o_audio_r   <= fileio_data(15 downto 0) & x"00";
  --             fileio_sample_cnt <= "11";
  --           end if;
  --         when "11" => -- ready
  --           if (sys_audio_taken_sync /= sys_audio_taken_sync_old) then              
              
  --             fileio_sample_cnt <= "00";
  --           end if;
  --         when others => null;
  --       end case;
  --     end if;
  --   end if;

  -- end process;
    
  -- p_audio_out : process
  -- begin    
  --   wait until rising_edge(i_clk_aud);

  --   if (i_ena_aud = '1') then
  --     o_audio_l <= sample_audio_l;
  --     o_audio_r <= sample_audio_r;

  --     -- Stretch taken pulse to sync with reader process
  --     if (i_audio_taken = '1') then
  --       audio_taken_sync <= not audio_taken_sync;
  --     end if;
  --   end if;

  -- end process;


  -- TODO: output a start/stop bit around each byte on dig, analog will have that already
  -- 1200 bps using 1200Hz and 2400Hz pulses

  
end RTL;
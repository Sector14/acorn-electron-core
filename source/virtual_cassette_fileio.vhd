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

-- Interface the frequency based ULA cassette i/o pins with SD Card based 
-- files using Generic FileIO.
--
-- File IO only handles 0 or 1 states where as tape has pulses of 0's,
-- 1's and gaps with level 0. Gaps will end up generating
-- pulses of 0's with the current setup. Although this should hopefully
-- not cause too big a problem as the first gap/run of 0's will cause the
-- stop bit check to fail and a return to looking for a high tone.
-- There will be a single byte that generates a RX full interrupt however.

library ieee;
  use ieee.std_logic_1164.all;
  use ieee.numeric_std.all;

use work.Replay_Pack.all;

entity Virtual_Cassette_FileIO is
  port (
    -- Clocks (Assumes 32MHz sys with 1:4 enable)
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
    i_rwnd               : in bit1;

    -- Pulse based cassette i/o (0 = 1200Hz and 1 = 2400Hz)
    i_cas_to_fch         : in bit1;
    o_cas_fm_fch         : out bit1;

    -- When true, requests switch of o_cas_fm_fch from pulse to avail/taken protocol
    i_cas_turbo          : in boolean;
    i_cas_taken          : in boolean;

    o_cas_turbo          : out boolean;
    o_cas_avail          : out boolean;

    o_debug              : out word(15 downto 0)
  );
end;

architecture RTL of Virtual_Cassette_FileIO is

  -- fileio
  signal fileio_addr            : word(31 downto 0);
  signal fileio_size            : word(15 downto 0);
  signal fileio_src_size        : word(31 downto 0);

  signal fileio_dir             : bit1;
  signal fileio_req             : bit1;
  signal fileio_ack_req         : bit1;
  signal fileio_ack_trans       : bit1;
  signal fileio_trans_err       : word( 2 downto 0);

  signal fileio_data            : word(17 downto 0);

  signal fileio_taken           : bit1;
  signal fileio_valid           : bit1;
  signal fileio_rx_level        : word(10 downto 0);
  signal fileio_rx_overfl       : bit1;
  signal fileio_rx_flush        : bit1;

  signal fileio_w_data          : word(15 downto 0);
  signal fileio_we              : bit1;
  signal fileio_tx_flush        : bit1;
  signal fileio_tx_level        : word(10 downto 0);
  signal fileio_w_addr          : word(31 downto 0);

  -- fileio req
  type t_fileio_req_state is (S_IDLE, S_W_IDLE, S_R_IDLE, S_W_WAIT, S_R_WAIT, S_HALT);
  signal fileio_req_state       : t_fileio_req_state;
  
  -- tape
  signal cas_to_fch_t1           : bit1;
  signal cas_to_fch_negedge     : boolean;
  signal bit_taken_r            : boolean;
  signal bit_valid_w            : boolean;
  
  signal ula_to_fileio          : bit1; 
  signal freq_encoded_bit       : bit1;

  -- Doubtful anyone will want a 500MB tape but why not :)
  signal tape_position          : unsigned(31 downto 0);  -- in bits
  signal freq_cnt               : integer range 6666 downto 0;

  -- current 16 bit data buffer for read / write of tape position
  signal cur_data               : word(15 downto 0);

  signal cas_turbo_latch        : boolean;
  
begin
  -- TODO: [Gary] Adapt uef2raw to emit a small header to start of virtual tape.
  --       tape read/write should skip this. On eject, write to this location
  --       the current tape position. On insert, read it and set tape_position
  --       accordingly.

  o_debug(15 downto 0) <= (others => '0');

  p_turbo_latch : process(i_clk, i_rst, i_cas_turbo)
  begin
    if (i_rst = '1') then
      cas_turbo_latch <= i_cas_turbo;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        -- Leaving turbo mode is restricted to when motor is not active due to
        -- tricky timing issues.
        if i_motor = '0' or freq_cnt = 0 then
          cas_turbo_latch <= i_cas_turbo;
        end if;
      end if;
    end if;
  end process;

  o_cas_turbo <= cas_turbo_latch;

  -- 1/8MHz = 125ns. 1/1200Hz = 833.333us
  -- 833.33us/125ns = 6666 cycles
  -- 1200Hz with 50% duty cycle = 3333 cycles high.
  p_freq_cnt : process(i_clk, i_rst, cas_turbo_latch)
  begin
    if (i_rst = '1' or cas_turbo_latch) then
      freq_cnt <= 6666;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        
        if i_fch_cfg.inserted(0) = '0' or i_play = '0' or i_motor = '0' then
          freq_cnt <= 6666;
        else
          freq_cnt <= freq_cnt - 1;    

          if freq_cnt = 0 then
            freq_cnt <= 6666;
          end if;
        end if;

      end if;
    end if;
  end process;

  -- Frequency encode current bit and output to o_cas
  p_read_encode : process(i_clk, i_ena, i_rst, cas_turbo_latch)
    variable cur_bit : integer range 15 downto 0;
  begin
    if (i_rst = '1' or cas_turbo_latch) then
      bit_taken_r <= false;
      freq_encoded_bit <= '0';      
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then

        freq_encoded_bit <= '0';
        bit_taken_r <= false;
    
        -- Read active
        if i_fch_cfg.inserted(0) = '1' and i_play = '1' and i_motor = '1' and i_rec = '0' then
          cur_bit := to_integer(unsigned(tape_position(3 downto 0)));

          if (freq_cnt = 0) then
            bit_taken_r <= true;
          end if;

          -- Pulse generation cnt 0..6666: 2400Hz = 0, 2x1200Hz = 1
          if (cur_data(15-cur_bit) = '1' and freq_cnt > 1666 and freq_cnt < 3333) or
              (cur_data(15-cur_bit) = '1' and freq_cnt > 4999) or
              (cur_data(15-cur_bit) = '0' and freq_cnt > 3333) then
            freq_encoded_bit <= '1';
          else
            freq_encoded_bit <= '0';
          end if;          
        end if;

      end if;
    end if;
  end process;

  -- Frequency encoding (authentic mode)
  -- Direct bit transfer (turbo mode) 
  -- Constant 0 when inactive in authentic mode due to no edges. Turbo mode relies
  -- on receiver checking avail as a 0 here would otherwise be taken as a full 0 bit
  -- due to lack of pulse encoding in turbo mode. The 0 here is purely to match authentic
  -- mode for LED usage by the core, as long as avail is checked last bit could be sent forever.
  o_cas_fm_fch <= freq_encoded_bit when not cas_turbo_latch else 
                  '0' when (i_fch_cfg.inserted(0) = '0' or i_play = '0' or i_motor = '0') and i_rec = '0' else
                  cur_data(15 - to_integer(unsigned(tape_position(3 downto 0))));

  -- Decode ULA FSK output back to 0/1 and write to current word
  p_write_decode : process(i_clk, i_rst, i_ena)
    variable cur_bit : integer range 15 downto 0;
    variable high_cnt : integer;
    variable skip_next_pulse : boolean;
  begin
    if (i_rst = '1') then
      bit_valid_w  <= false;
      high_cnt := 0;
      cas_to_fch_t1 <= '0';
      ula_to_fileio <= '0';
      skip_next_pulse := false;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        bit_valid_w  <= false;
        cas_to_fch_t1 <= i_cas_to_fch;

        if i_fch_cfg.inserted(0) = '0' or
           i_play = '0' or i_rec = '0' or i_motor = '0' then
          high_cnt := 0;
          ula_to_fileio <= '0';
          skip_next_pulse := false;
        else
          
          if i_cas_to_fch = '1' then
            high_cnt := high_cnt + 1;
          end if;
          
          -- TODO: [Gary] Additional latches to avoid noise triggering negedge?
          if cas_to_fch_negedge then

            if skip_next_pulse then
              skip_next_pulse := false;
            else
              -- 2400Hz = 3333, 1200Hz = 6666 cycles high
              -- 2500 cycles threshold in middle
              if high_cnt > 2500 then
                ula_to_fileio <= '0';
                bit_valid_w <= true;
              elsif high_cnt > 1000 then
                ula_to_fileio <= '1';
                skip_next_pulse := true;
                bit_valid_w <= true;
              end if;              
            end if;

            high_cnt := 0;
          end if;
        end if;

      end if;
    end if;
  end process;

  cas_to_fch_negedge <= true when (not i_cas_to_fch and cas_to_fch_t1) = '1' else false;

  -- Authentic Mode:
  --   Tape is one way sync'd with read/write process and one way 
  --   sync'd to FileIO request process. No sync back from FIFO
  --   as ULA will not yield even if ARM transfer stalled.  
  -- Turbo Mode: 
  --   Tape is read as fast as ULA requests and may stall if FIFO
  --   queue runs out.
  p_tape_readwrite : process(i_clk, i_rst, i_ena)
    variable cur_bit : integer range 15 downto 0;
  begin
    if (i_rst = '1') then
      tape_position <= (others => '0');
      cur_data <= (others => '0');
      fileio_w_data <= (others => '0');
      fileio_taken <= '0';
      fileio_we <= '0';
      o_cas_avail <= false;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        fileio_taken <= '0';
        fileio_we <= '0';
        o_cas_avail <= false;

        if (i_fch_cfg.inserted(0) = '0') then
          tape_position <= (others => '0');
          cur_data <= (others => '0');
        else
          -- TODO: [Gary] Handle reaching limit of tape_position
          
          cur_bit := to_integer(unsigned(tape_position(3 downto 0)));

          if cas_turbo_latch and i_fch_cfg.inserted(0) = '1' and i_play = '1' and i_motor = '1' and i_rec = '0' then 
            -- Turbo mode runs a slightly higher risk of exhausting the queue if the arm stalls.
            -- This pause relies on the arm still sending data after EOF is reached as up to 16 bits
            -- of valid data may reside in cur_data and will not process until queue no longer empty.
            if fileio_valid = '1' then
              o_cas_avail <= true;
            end if;
          end if;

          -- Reading
          if bit_taken_r or i_cas_taken then

            if cur_bit = 15 then
              -- TODO: [Gary] cur_data isn't set until tape position has done one 
              --       16 bit cycle! really first fileio_data byte should be valid
              --       before allowing tape to start ticking away? Once move to FSM
              --       to handle loading tape header this problem will go away.

              -- spi transfers in big endian and uef2raw writes big endian
              cur_data <= fileio_data(15 downto 0);
              fileio_taken <= '1';
            end if;

          -- Writing (Authentic)
          elsif bit_valid_w then
            -- TODO: [Gary] if i_rec is disabled before bit 15, the 16bit word is never
            --       passed to fileio for storage. So far this is not an issue as there's
            --       ample high tone that losing a bit (or 15) of it doesn't change anything. Needs
            --       fixing though.
            if cur_bit = 15 then
              fileio_w_data <= cur_data(15 downto 1) & ula_to_fileio;
              fileio_we <= '1';
              cur_data <= (others => '0');
            else            
              -- ULA shifts out to right. LSB in ULA will end up stored in MSB on tape.
              cur_data(15-cur_bit) <= ula_to_fileio;
            end if;

          end if;

          if (bit_valid_w or bit_taken_r or i_cas_taken) then
            tape_position <= tape_position + 1;
          end if;          

          -- TODO: [Gary] Handle ffwd/rwnd. Will need to cause fileio read and write address changes.
        end if;

      end if;
    end if;
  end process;

  --
  -- FILEIO
  --
  u_FileIO_FCh : entity work.Replay_FileIO_FCh_Generic
  port map (
    -- clocks
    i_clk                 => i_clk,
    i_ena                 => i_ena,
    i_rst                 => i_rst,

    -- FileIO / Syscon interface
    i_fch_to_core         => i_fch_to_core,
    o_fch_fm_core         => o_fch_fm_core,

    -- to user space
    i_req                 => fileio_req,
    o_ack_req             => fileio_ack_req,
    o_ack_trans           => fileio_ack_trans, -- transfer done
    o_trans_err           => fileio_trans_err, -- aborted, truncated, seek error

    -- below latched on ack
    i_dir                 => fileio_dir,  -- 1 write, 0 read
    i_chan                => "00",
    i_addr                => fileio_addr,
    i_size                => fileio_size,
    o_size0               => fileio_src_size,

    -- Reading
    i_fifo_to_core_flush  => fileio_rx_flush,
    o_fifo_to_core_data   => fileio_data,
    i_fifo_to_core_taken  => fileio_taken,
    o_fifo_to_core_valid  => fileio_valid,
    o_fifo_to_core_level  => fileio_rx_level,
    o_fifo_to_core_overfl => fileio_rx_overfl,

    -- Writing
    i_fifo_fm_core_flush  => fileio_tx_flush,
    i_fifo_fm_core_data   => fileio_w_data,
    i_fifo_fm_core_we     => fileio_we,
    o_fifo_fm_core_level  => fileio_tx_level
    );


  -- Enable/pause data request via the Generic FileIO entity
  p_fileio_req : process(i_clk, i_rst)
  begin   
    if (i_rst = '1') then
      fileio_addr <= (others => '0');
      fileio_req  <= '0';
      fileio_req_state <= S_IDLE;
      fileio_rx_flush <= '1';
      fileio_tx_flush <= '1';
    elsif rising_edge(i_clk) then

      if (i_ena = '1') then
        
        fileio_req  <= '0';
        fileio_rx_flush <= '0';
        fileio_tx_flush <= '0';

        if i_fch_cfg.inserted(0) = '0' then     
          fileio_addr <= (others => '0');
          fileio_req_state <= S_IDLE;
          fileio_rx_flush <= '1';
          fileio_tx_flush <= '1';
        else
          case fileio_req_state is
            when S_IDLE =>
              -- Sync next r/w transfer with current tape position (16 bit aligned)
              fileio_addr <= "000" & word(tape_position(31 downto 4)) & "0";

              if (i_play = '1' and i_rec = '1') then
                fileio_req_state <= S_W_IDLE;
                fileio_dir <= '1';
              elsif (i_play = '1') then
                fileio_req_state <= S_R_IDLE;
                fileio_dir <= '0';
              end if;

            -- 
            -- Write states
            --
            when S_W_IDLE =>
              fileio_dir <= '1';              
              
              -- User attempting to leave record mode or in case of motor stop, writing
              -- has finished and remainder of FIFO queue needs transfering asap.
              if (i_play = '0' or i_rec = '0' or i_motor = '0') then
                -- NOTE: transfer may still be split into two requests by half full test below
                if unsigned(fileio_tx_level) /= 0 then
                  -- size is in bytes, level in words
                  fileio_size <= "0000" & fileio_tx_level & "0";
                  fileio_req <= '1';
                else
                  fileio_req_state <= S_IDLE;
                end if;
              end if;

              -- Start transmit as soon as 64 (0x40) bytes available (buffer >= 32 words)
              if (fileio_tx_level(5) = '1') then
                fileio_req  <= '1';
                fileio_size <= x"0040";
              end if;

              if (fileio_ack_req = '1') then
                fileio_req_state <= S_W_WAIT;
              end if;

            when S_W_WAIT =>
              if (fileio_ack_trans = '1') then
                fileio_req_state <= S_W_IDLE;
                if (red_or(fileio_trans_err) = '1') then
                  -- unhandled - write failed.
                  fileio_req_state <= S_HALT;
                else       
                  fileio_addr <= word(unsigned(fileio_addr) + unsigned(fileio_size));
                end if;
              end if;
        
            --
            -- Read states
            --
            when S_R_IDLE =>
              fileio_dir <= '0';
              fileio_size <= x"0040";

              -- Stopping playback or switching to record?
              if (i_play = '0' or i_rec = '1') then
                fileio_rx_flush <= '1';
                fileio_req_state <= S_IDLE;
              else            
                -- bit 9 = buffer < half full (512 words)
                -- Results in at most one request over half full (1024 + 64 bytes) buffered
                if fileio_rx_level(9) = '0' then
                  fileio_req  <= '1'; -- note, request only sent when ack received
                end if;
                if (fileio_ack_req = '1') then
                  fileio_req_state <= S_R_WAIT;
                end if;
              end if;

            when S_R_WAIT =>
              if (fileio_ack_trans = '1') then
                fileio_req_state <= S_R_IDLE;
                if (red_or(fileio_trans_err) = '1') then
                  -- TODO: [Gary] Check truncated for end of tape. How to handle? Same way
                  --       as tape_position reaching max val? Report error status to OSD via flags?
                  fileio_req_state <= S_HALT;
                else
                  fileio_addr <= word(unsigned(fileio_addr) + unsigned(fileio_size));
                end if;
              end if;

            when others => null;
          end case;

        end if;
      end if;

    end if;
  end process;

end RTL;
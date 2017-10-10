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
--
-- Assumes start/stop bits are baked into the bitstream already.
--
-- File IO only handles 0 or 1 states where as tape has pulses of 0's,
-- 1's and gaps with level 0. Gaps will end up generating
-- pulses of 0's with the current setup. Although this should hopefully
-- not cause too big a problem as the first gap/run of 0's will cause the
-- stop bit check to fail and a return to looking for a high tone.
-- There will be a single byte that generates a RX full interrupt however.


-- TODO: This is quite brittle at the moment. Switch between read<->write
-- is likely not handled safely. 
-- If i_rec is disabled before 16 bits have been written, the cur_data may not
-- be pushed to the write FIFO. If the data is always pushed on negative
-- edge of i_rec, play may kick in too early and write may also not occur.
-- Really it should be considered out of spec for a switch to occur whilst
-- the motor is active. I don't believe the electron ever needs this although
-- uses could obviously do so with the tape recorder but only by stopping
-- play at the same time.

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

    o_debug              : out word(7 downto 0)
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
  signal cas_to_fch_l           : bit1;
  signal cas_to_fch_negedge     : boolean;
  signal bit_taken_r            : boolean;
  signal bit_valid_w            : boolean;
  
  signal ula_to_fileio          : bit1; 

  -- Doubtful anyone will want a 500MB tape but why not :)
  signal tape_position          : unsigned(31 downto 0);  -- in bits
  signal freq_cnt               : integer range 6666 downto 0;

  -- current 16 bit data buffer for read / write of tape position
  signal cur_data               : word(15 downto 0);

begin

  -- TODO: Adapt uef2raw to emit a small header to start of virtual tape.
  -- tape read/write should skip this. On eject, write to this location
  -- the current tape position. On insert, read it and set tape_position
  -- accordingly.

  -- 1/8MHz = 125ns. 1/1200Hz = 833.333us
  -- 833.33us/125ns = 6666 cycles
  -- 1200Hz with 50% duty cycle = 3333 cycles high.
  p_freq_cnt : process(i_clk, i_ena, i_rst)
  begin
    if (i_rst = '1') then
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
  p_read_encode : process(i_clk, i_ena, i_rst)
    variable cur_bit : integer range 15 downto 0;
  begin
    if (i_rst = '1') then
      bit_taken_r <= false;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        o_cas_fm_fch <= '0';
        bit_taken_r <= false;

        -- Read active
        if i_fch_cfg.inserted(0) = '1' and i_play = '1' and i_motor = '1' and i_rec = '0' then
          if (freq_cnt = 0) then
            bit_taken_r <= true;
          end if;

          cur_bit := to_integer(unsigned(tape_position(3 downto 0)));
  
          -- Pulse generation cnt 0..6666: 2400Hz = 0, 2x1200Hz = 1
          if (cur_data(15-cur_bit) = '1' and freq_cnt > 1666 and freq_cnt < 3333) or
             (cur_data(15-cur_bit) = '1' and freq_cnt > 4999) or
             (cur_data(15-cur_bit) = '0' and freq_cnt > 3333) then
             o_cas_fm_fch <= '1';
          end if;
          
        end if;

      end if;
    end if;
  end process;

  -- Decode ULA FSK output back to 0/1 and write to current word
  p_write_decode : process(i_clk, i_rst, i_ena)
    variable cur_bit : integer range 15 downto 0;
    variable high_cnt : integer;
    variable skip_next_pulse : boolean;
  begin
    if (i_rst = '1') then
      bit_valid_w  <= false;
      high_cnt := 0;
      cas_to_fch_l <= '0';
      ula_to_fileio <= '0';
      skip_next_pulse := false;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        bit_valid_w  <= false;
        cas_to_fch_l <= i_cas_to_fch;

        if i_fch_cfg.inserted(0) = '0' or
           i_play = '0' or i_rec = '0' or i_motor = '0' then
          high_cnt := 0;
          ula_to_fileio <= '0';
          skip_next_pulse := false;
        else
          
          if i_cas_to_fch = '1' then
            high_cnt := high_cnt + 1;
          end if;
          
          -- TODO: Additional latches to avoid noise triggering negedge?
          if cas_to_fch_negedge then

            if skip_next_pulse then
              skip_next_pulse := false;
            else
              -- 2400Hz = 3333, 1200Hz = 6666 cycles high
              -- 2500 cycles threshold in middle
              -- 1000 is overkill but handles partial high pulse that might occur whilst
              -- waiting to generate START bit.
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

  cas_to_fch_negedge <= true when (not i_cas_to_fch and cas_to_fch_l) = '1' else false;

  -- Tape is one way sync'd with read/write process and one way 
  -- sync'd to FileIO request process. No sync back from FIFO
  -- as ULA will not yield even if ARM transfer stalled.  
  p_tape_readwrite : process(i_clk, i_rst, i_ena)
    variable cur_bit : integer range 15 downto 0;
  begin
    if (i_rst = '1') then
      tape_position <= (others => '0');
      cur_data <= (others => '0');
      fileio_w_data <= (others => '0');
      fileio_taken <= '0';
      fileio_we <= '0';
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        fileio_taken <= '0';
        fileio_we <= '0';

        if (i_fch_cfg.inserted(0) = '0') then
          tape_position <= (others => '0');
          cur_data <= (others => '0');
        -- elsif i_motor = '0' and was on before...
          -- TODO: write out current tape position to addr 0
          --       need to adjust start read/write address to be after header
        else
          -- TODO: Handle reaching limit of tape_position, currently wraps which is bad!
          
          cur_bit := to_integer(unsigned(tape_position(3 downto 0)));

          -- Reading
          if bit_taken_r then
            
            if cur_bit = 15 then
              -- TODO: cur_data isn't set until tape position has done one 
              -- 16 bit cycle! really first fileio_data byte should be valid
              -- before allowing tape to start ticking away? Once move to FSM
              -- to handle loading tape header this problem will go away.

              -- spi transfers in big endian and uef2raw writes big endian
              cur_data <= fileio_data(15 downto 0);
              fileio_taken <= '1';
            end if;

          -- Writing
          elsif bit_valid_w then
            
            if cur_bit = 15 then
              fileio_w_data <= cur_data(15 downto 1) & ula_to_fileio;
              fileio_we <= '1';
              cur_data <= (others => '0');
            else            
              -- ULA shifts out to right. LSB in ULA will end up stored in MSB on tape.
              cur_data(15-cur_bit) <= ula_to_fileio;
            end if;

          end if;

          if (bit_valid_w or bit_taken_r) then
            tape_position <= tape_position + 1;
          end if;          

          -- TODO: Handle ffwd/rwnd. Will need to cause fileio read and write address changes.
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

  -- TODO: Current setup requires REC be ON before PLAY is turned on
  -- and for disabling, PLAY must be disabled then REC.

  o_debug(0) <= '0';
  o_debug(1) <= i_cas_to_fch;
  o_debug(2) <= '1' when bit_valid_w else '0';
  o_debug(3) <= i_ena;

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

        -- TODO: Handle ffwd/rwnd. fileio addr for r/w needs to track tape_position

        if i_fch_cfg.inserted(0) = '0' then     
          fileio_addr <= (others => '0');
          fileio_req_state <= S_IDLE;
          fileio_rx_flush <= '1';
          fileio_tx_flush <= '1';
        else
          -- TODO: Change i_rec to mean play+rec whilst play is play only and 
          --       make the two mutually exclusive.
          case fileio_req_state is
            when S_IDLE =>
              -- TODO: Account for any external changes to tape position      

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
              
              -- Attempting to leave record mode
              if (i_play = '0' or i_rec = '0') then
                -- transfer any remaining data in buffer
                -- NOTE: transfer may still be split into two requests by half full test below
                if unsigned(fileio_tx_level) /= 0 then
                  fileio_size <= "00000" & fileio_tx_level;
                  fileio_req <= '1';
                else
                  fileio_req_state <= S_IDLE;
                end if;
              end if;
        
              -- buffer >= half full, start transfer
              if (fileio_tx_level(9) = '1') then
                fileio_req  <= '1';
                fileio_size <= x"0400";
              end if;

              if (fileio_ack_req = '1') then
                fileio_req_state <= S_W_WAIT;
              end if;

            when S_W_WAIT =>
              if (fileio_ack_trans = '1') then
                if (red_or(fileio_trans_err) = '1') then
                  -- unhandled - write failed.
                  fileio_req_state <= S_HALT;
                else       
                  fileio_addr <= fileio_addr + fileio_size;
                end if;
                fileio_req_state <= S_W_IDLE;
              end if;
        
            --
            -- Read states
            --
            when S_R_IDLE =>
              fileio_dir <= '0';
              fileio_size <= x"0400";

              -- Stopping playback or switching to record?
              if (i_play = '0' or i_rec = '1') then
                fileio_rx_flush <= '1';
                fileio_req_state <= S_IDLE;
              else            
                -- buffer < half full pre-fetch more data
                if fileio_rx_level(9) = '0' then
                  fileio_req  <= '1'; -- note, request only sent when ack received
                end if;
                if (fileio_ack_req = '1') then
                  fileio_req_state <= S_R_WAIT;
                end if;
              end if;

            when S_R_WAIT =>
              if (fileio_ack_trans = '1') then
                if (red_or(fileio_trans_err) = '1') then
                  -- TODO: Check truncated for end of tape. How to handle? Same way
                  -- as tape_position reaching max val? Report error status to OSD via flags?
                  fileio_req_state <= S_HALT;
                else
                  fileio_addr <= fileio_addr + fileio_size;
                end if;
                fileio_req_state <= S_R_IDLE;
              end if;

            when others => null;
          end case;

        end if;
      end if;

    end if;
  end process;

end RTL;
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

  signal fileio_req             : bit1;
  signal fileio_ack_req         : bit1;
  signal fileio_ack_trans       : bit1;
  signal fileio_trans_err       : word( 2 downto 0);

  signal fifoio_tx_flush        : bit1;
  signal fileio_data            : word(17 downto 0);
  signal fileio_taken           : bit1;
  signal fileio_valid           : bit1;
  signal fileio_rx_level        : word(10 downto 0);
  signal fileio_rx_overfl       : bit1;

  signal fileio_rx_flush        : bit1;

  -- fileio req
  type t_fileio_req_state is (S_IDLE, S_WAIT);
  signal fileio_req_state       : t_fileio_req_state;

  -- tape
  signal bit_taken_r            : boolean;
  -- Doubtful anyone will want a 500MB tape but why not :)
  signal tape_position          : unsigned(31 downto 0);  -- in bits
  signal freq_cnt               : integer range 6666 downto 0;

  -- prefetched 16 bit data during read, or, write buffer
  signal cur_data               : word(15 downto 0);
begin

  -- TODO: Adapt uef2raw to emit a small header to start of virtual tape.
  -- tape read/write should skip this. On eject, write to this location
  -- the current tape position. On insert, read it and set tape_position
  -- accordingly.

  -- TODO: This feels a little brittle at the moment. Not convinced
  -- switching between read<->write is handled correctly either. If i_rec
  -- is disabled before 16 bits have been written, the cur_data may not
  -- be pushed to the write FIFO. If the data is always pushed on negative
  -- edge of i_rec, play may kick in too early and write may also not occur.
  -- Really it should be considered out of spec for a switch to occur whilst
  -- the motor is active. I don't believe the electron ever needs this although
  -- uses could obviously do so with the tape recorder but only by stopping
  -- play at the same time.

  -- set signals at 1200 and 2400Hz?
  -- other processes can avoid basing off the exact cnt value then
  -- and it'll be easier to change the freq gen for other systems
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
  p_tape_read : process(i_clk, i_ena, i_rst)
    variable cur_bit : integer range 15 downto 0;
  begin
    if (i_rst = '1') then
      bit_taken_r <= false;
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        o_cas_fm_fch <= '0';
        bit_taken_r <= false;

        if i_fch_cfg.inserted(0) = '1' and i_play = '1' and i_motor = '1' then
          if (freq_cnt = 0) then
            bit_taken_r <= true;
          end if;

          cur_bit := to_integer(unsigned(tape_position(3 downto 0)));
  
          -- Pulse generation: 2400Hz = 0, 2x1200Hz = 1
          if (cur_data(15-cur_bit) = '1' and freq_cnt > 1666 and freq_cnt < 3333) or
             (cur_data(15-cur_bit) = '1' and freq_cnt > 4999) or
             (cur_data(15-cur_bit) = '0' and freq_cnt > 3333) then
             o_cas_fm_fch <= '1';
          end if;

        end if;

      end if;
    end if;
  end process;

  -- p_tape_write : process(i_clk, i_rst, i_ena)
  -- begin
  --   if (i_rst = '1') then
  --     -- bit_transmit_w  <= false; ??
  --   elsif rising_edge(i_clk) then
  --     if (i_ena = '1') then

  --       -- TODO: Decide how to handle write
  --       if pos = "1111"
  --         copy byte to data_write
  --         signal to bit_take_w
  --         can't clear cur_data here, tape_position would need to do it

  --     end if;
  --   end if;
  -- end;

  -- Decouple tape position changes from read and write processes
  -- Tape runs on bit position, FIFO runs on 16 bit aligned word addr.  
  p_tape_position : process(i_clk, i_rst, i_ena)
  begin
    if (i_rst = '1') then
      tape_position <= (others => '0');
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        fileio_taken <= '0';

        if (i_fch_cfg.inserted(0) = '0') then
          tape_position <= (others => '0');
        -- elsif i_motor = '0' and was on before...
          -- TODO: write out current tape position to addr 0
          --       need to adjust start read/write address to be after header
        else      
          -- TODO: Handle reaching limit of tape_position. Everything should stop.
          -- TODO: Is it possible (or even wanted) to drop the taken/transmit and 
          -- just have data expected at a fixed rate like a normal tape in motion would?

          if bit_taken_r then
            if tape_position(3 downto 0) = "1111" then
              -- TODO: cur_data isn't set until tape position has done one 
              -- 16 bit cycle! really first fileio_data byte should be valid
              -- before allowing tape to start ticking away?
              -- spi transfers in big endian and uef2raw writes big endian
              cur_data <= fileio_data(15 downto 0);
              -- fifo can pop data now
              fileio_taken <= '1';                      
            end if;

            tape_position <= tape_position + 1;
          end if;

          -- TODO: If writing, submit byte to fifo. Clear out data_buf?
          --       read should continue from where write left off due to
          --       sharing cur_data and tape_position

          -- TODO: Handle ffwd/rwnd
        end if;

      end if;
    end if;
  end process;

  o_debug(0) <= '1' when bit_taken_r else '0';
  o_debug(1) <= fileio_taken;
  o_debug(2) <= '1' when freq_cnt = 0 else '0';

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
    i_dir                 => i_rec,  -- 1 write, 0 read
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

    -- TODO: Writing
    i_fifo_fm_core_flush  => '0',
    i_fifo_fm_core_data   => (others => '0'),
    i_fifo_fm_core_we     => '0',
    o_fifo_fm_core_level  => open
    );
  
  -- Filesize in bytes, for 16bit requests must be multiple of 2. 
  fileio_size <= x"0400";

  -- Enable/pause data request via the Generic FileIO entity
  p_fileio_req : process(i_clk, i_rst)
    variable cur_dir : bit1;
  begin   
    if (i_rst = '1') then
      fileio_addr <= (others => '0');
      fileio_req  <= '0';
      fileio_req_state <= S_IDLE;
      fileio_rx_flush <= '0';
      cur_dir := '0';
    elsif rising_edge(i_clk) then

      if (i_ena = '1') then
        fileio_req  <= '0';
        fileio_rx_flush <= '0';

        -- fileio_req assumes sequential read with flush/reset occuring
        -- anytime ffwd/rwnd occurs
        if (i_fch_cfg.inserted(0) = '0') then -- or (cur_dir /= i_rec) then
          -- TODO: Switching from write to read, if bit position is mid-word 
          -- does a refetch really need to be made? Would p_read end up
          -- using wrong data bits? as write won't have been padded out and
          -- pushed? May not be an issue if motor stops before dir switch?
          fileio_addr <= "0000" & word(tape_position(31 downto 4)); 
          fileio_req_state <= S_IDLE;
          -- direction switch, flush buffers (both or just one switching to?)
          fileio_rx_flush <= '1';
          cur_dir := i_rec;
        else
          case fileio_req_state is
            when S_IDLE =>            
              if fileio_rx_level(9) = '0' then -- bit 9 would be < half full
                fileio_req  <= '1'; -- note, request only sent when ack received
              end if;
              if (fileio_ack_req = '1') then
                fileio_req_state <= S_WAIT;
              end if;

            when S_WAIT =>
              if (fileio_ack_trans = '1') then
                if (red_or(fileio_trans_err) = '1') then
                  -- TODO: Check truncated for end of tape. How to handle? Same way
                  -- as tape_position reaching max val?
                  -- TODO: Should at least stop making new requests until tape rewound or ejected?
                else
                  -- Only prefetch one word
                  fileio_addr <= fileio_addr + fileio_size;
                end if;
                fileio_req_state <= S_IDLE;
              end if;

            when others => null;
          end case;
        end if;
      end if;

    end if;
  end process;

end RTL;
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
    o_cas_fm_fch         : out bit1
  );
end;

architecture RTL of Virtual_Cassette_FileIO is

  signal cur_data : word(15 downto 0);

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


begin
  -- File IO only handles 0 or 1 states where as tape had pulses of 0's
  -- pulses of 1's and then gaps with level 0. Gaps will end up generating
  -- pulses of 0's with the current setup. Although this should hopefully
  -- not cause too big a problem as the first run of 0's will cause the
  -- stop bit check to fail and a return to looking for a high tone.
  -- There will be a single byte that generates a RX full interrupt however.

  -- output a byte of 0's before first real data. Is cur_bit/cur_data wrong
  -- and not catching first byte?
  p_dummy_read : process(i_clk, i_ena, i_rst)
    variable cnt : integer := 0;
    -- Operates 2 bytes at a time due to fileio data size
    variable cur_bit : integer range 15 downto 0;    
  begin
    if (i_rst = '1') then
      cnt := 0;
      cur_bit := 15;
      fileio_taken <= '0';
      cur_data <= (others => '0');
    elsif rising_edge(i_clk) then
      if (i_ena = '1') then
        fileio_taken <= '0';

        if (i_fch_cfg.inserted(0) = '0') then
          -- fileio resets addr on eject
          cur_bit := 15;
          cnt := 0;
        elsif (i_play = '1') and (i_motor = '1') then
          if cnt = 0 then
            if cur_bit = 15 then
              cur_bit := 0;
            else            
              cur_bit := cur_bit + 1;
            end if;

            cnt := 6666;
          end if;

          cnt := cnt - 1;

          -- ready next byte 
          if cnt = 0 and cur_bit = 15 then
            -- spi transfers in big endian and uef2raw writes big endian
            cur_data <= fileio_data(15 downto 0);
            fileio_taken <= '1';            
          end if;          
        end if;    

      end if;  

      -- Pulse generation: 2400Hz = 0, 2x1200Hz = 1
      o_cas_fm_fch <= '0';
      if (cur_data(15-cur_bit) = '1' and cnt > 1666 and cnt < 3333) or
         (cur_data(15-cur_bit) = '1' and cnt > 4999) or
         (cur_data(15-cur_bit) = '0' and cnt > 3333) then
        o_cas_fm_fch <= '1';
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
    i_dir                 => '0', -- read only
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
  
  -- request 512 16 bits words at a time (half of fifo size per request)
  fileio_size <= x"0400";

  -- Enable/pause data request via the Generic FileIO entity
  -- Keep FIFO buffer full via new requests as long as a tape inserted
  p_fileio_req : process(i_clk, i_rst)
  begin   
    if (i_rst = '1') then
      fileio_addr <= (others => '0');
      fileio_req  <= '0';
      fileio_req_state <= S_IDLE;
      fileio_rx_flush <= '0';
    elsif rising_edge(i_clk) then

      if (i_ena = '1') then
        fileio_req  <= '0';
        fileio_rx_flush <= '0';

        -- Unlike a real tape, virtual tapes rewind upon eject ;) This also
        -- makes up for the fact that the RWND button doesn't work.
        if (i_fch_cfg.inserted(0) = '0') then
          fileio_addr <= (others => '0');
          fileio_req_state <= S_IDLE;
          fileio_rx_flush <= '1';
        else
          case fileio_req_state is
            when S_IDLE =>
              if (fileio_rx_level(9) = '0') then -- <hf
                fileio_req  <= '1'; -- note, request only sent when ack received
              end if;
              if (fileio_ack_req = '1') then
                fileio_req_state <= S_WAIT;
              end if;

            when S_WAIT =>
              if (fileio_ack_trans = '1') then
                if (red_or(fileio_trans_err) = '1') then
                  -- TODO: Check truncated for end of tape. How to handle?
                  -- TODO: Should at least stop making new requests until tape rewound or ejected?
                  -- fileio_addr <= (others => '0');
                else
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
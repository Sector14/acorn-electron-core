# Electron Core ROMS

In order to use the Electron core you will need the OS and Basic ROMs
or a combined os_basic ROM.

Rom sha1sum

  os.rom       a48b8fa0cfb09140e808ac8a187316c605a0b32e
  basic.rom    4a7393f3a45ea309f744441c16723e2ef447a281
  os_basic.rom bad51a4666ff9e9eed19811a1eb9d4cda10e69a3


# Key Binds

Most non-shifted keys on the Electron have been mapped to the same non-shifted 
key on a regular keyboard, however, the shifted and ctrl states will be the
Electron symbol not the symbol shown on a regular keyboard.

Exceptions to this are:

  - : and the shifted * are moved to the key for "'" and "@" key
  - COPY key is bound to "["

For example, the "shift+8" on a normal keyboard outputs "*" whilst on the 
Electron you get a "(". The "*" symbol on the Electron keyboard is shown
instead on the "shift+:" key. However, as noted above, the core uses the
"shift+'" key for a "*" due to the ":" key conflicting with the ";" key
and its shifted key "+".
  
As with the original electron, caps-lock is toggled via shift+capslock, whilst 
holding capslock will result in the function key, i.e capslock+e will output
"ELSE"


# Video Output

By default the core outputs a PAL analog signal with CSync @ 15.652kHz. 

TVs will accept this signal via scart sockets. A DVI/VGA adapter plus
VGA to Scart cable will be needed. Note most VGA/Scart cables will not
have a compatible pin-out but scart cables can easily have the pins 
switched.

See http://www.fpgaarcade.com/punbb/viewtopic.php?id=1211 for instructions
on how to modify a regular VGA/Scart cable to work with the replay.

Compatible cables can be bought, they'll be sold as "minimig" such as:
http://amigakit.leamancomputing.com/catalog/product_info.php?products_id=919

Digital outputs (DVI or HDMI) may also be used however most monitors will
require a 31kHZ signal. Enable the "Double scan" option in the replay.ini

Note: Whilst a DVI/VGA adapter can be used to connect to monitors/TVs 
that provide a VGA connection, even if they support 15.652kHz VGA it
is likely to expect separate H & V Sync signals rather than CSync.
The only way to achieve that currently is to enable "Double scan"
and operate at 32kHz.


# Core Status

The core boots to the Basic prompt in mode 6 with keyboard support.

Entering of Basic programs should work. If you find any exceptions to
that, please send me a minimal program example that illustrates the issue.

Loading of games is also supported although see the Virtual Cassette
section for file format/usage.

The notable missing features are:

  - Save support.
  - FFwd, Rwnd, current tape position counter.
  - Sound.
  - Any kind of expansion (plus 1 etc)


## Virtual Cassette Interface

The current cassette interface is a very limited and temporary method
to allow loading of a raw file off of an SD card. There is no support
for save/ffwd/rwnd or feedback on the current tape position. You can
"reset" the tape to the beginning by ejecting the tape and inserting
again via the menu.

A "raw" file can be obtained by extracting the tape data from a UEF
file including start/stop bits. A python uef2raw.py script will do this
for you and can be found in the SVN sw/tools/acorn folder.

Usage:

  python uef2raw.py <input_file.uef> <output_file.raw>

Once the "raw" file is inserted via the OSD, it is safe to set PLAY to ON.
The "tape" will not actually start playing as the core supports motor
control and will wait until the Electron enables the motor in response
to a "*CAT" or "*LOAD" action.

Save support, counter feedback, fast forward and rewind will arrive in
a future update. Direct UEF support is also a future possibility.


## Physical Cassette Interface

A physical cassette recorder cannot yet be attached to the Replay Board.
However the core should support loading once a pin is routed to it.

You will need to replicate the original Acorn cassette hardware interface
for CAS IN, CAS OUT and CAS MO. CAS RC is not used currently. 
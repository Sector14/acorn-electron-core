# Electron Core ROMS

In order to use the Electron core you will need the OS and Basic ROMs
or a combined os_basic ROM.

Rom sha1sum

  os.rom       a48b8fa0cfb09140e808ac8a187316c605a0b32e
  basic.rom    4a7393f3a45ea309f744441c16723e2ef447a281
  os_basic.rom bad51a4666ff9e9eed19811a1eb9d4cda10e69a3


# Core Status

The core boots to the Basic prompt in mode 6 with keyboard support.

Entering of Basic programs should work. If you find any exceptions to
that, please send me a minimal program example that illustrates the issue.

Loading of games and saving programs is also supported. See the Virtual 
Cassette section for file format/usage.

The notable missing features are:
  
  - Fast Forward / Rewind
  - Tape position counter
  - Any kind of expansions (plus 1 etc)


# Key Binds

Most non-shifted keys on the Electron have been mapped to the same non-shifted 
key on a regular keyboard, however, the shifted and ctrl states will be the
Electron symbol not the symbol shown on a regular keyboard.

Exceptions to this are:

  - : and the shifted * are moved to the key for "'" and "@" key
  - COPY key is bound to "["
  - BREAK key bound to SCROLL LOCK. Use SCROLL LOCK for soft reset or
    CTRL+SCROLL LOCK for hard reset.

For example, the "shift+8" on a normal keyboard outputs "*" whilst on the 
Electron you get a "(". The "*" symbol on the Electron keyboard is shown
instead on the "shift+:" key. However, as noted above, the core uses the
"shift+'" key for a "*" as the ":" key was mapped to "'" due to conflicting with
the ";" key and its shifted key "+" (clear as mud?)
  
As with the original electron, caps-lock is toggled via shift+capslock, whilst 
holding capslock will result in the function key, i.e capslock+e will output
"ELSE"


# Video Output

Two video modes are available. "Authentic" and "Compatible". By default the
core operates in "compatible" mode. Monitors should be set to a 4:3 aspect
ratio if available.

To change the default mode, edit the replay.ini file and move the ",default"
from the compatible option to authentic (or vice-versa). E.G to default to
"Authentic" mode

  item = "Accuracy", 0x00000021,dynamic
  option = "Authentic",  0x00000000,default
  option = "Compatible", 0x00000021

## Authentic Mode

Authentic mode is a more faithful recreation of the Electron's PAL signal
running at 15.625kHz line frequency and 50Hz vertical refresh using
CSync. The timing derived from the RTC and Display interrupts is also
more accurate.

TVs will accept this signal via scart sockets. (see below for cabling
information).

Note: Due to the use of CSync, even if your monitor supports a
15.625kHz line frequency over VGA, it is still unlikely to work as many
expect separate H & V Sync signals rather than CSync.

Also be aware that some monitors when connected via HDMI will mis-identify
authentic mode as a 720x576 @25Hz signal. This can cause excess flickering.

## Compatible Mode

Compatible mode uses a tweaked PAL signal (two fields of 312 lines rather than
312.5 lines) and scan line doubler to increase the chance of the Electron's
non-standard signal working with VGA/DVI/HDMI connected monitors.

Whilst supporting a wider range of displays, the dropping of one line per
frame means the core will run slightly faster than normal. Roughly 1 second
faster per 5 minutes of runtime or 99.9844Hz vs 100.128Hz


## Cables

For Scart connections a DVI/VGA adapter plus VGA to Scart cable will be needed.
Be aware that most VGA/Scart cables will not have a compatible pin-out but most 
scart cables can easily have the pins switched around.

See http://www.fpgaarcade.com/punbb/viewtopic.php?id=1211 for instructions
on how to modify a regular VGA/Scart cable to work with the replay.

Alternatively, compatible cables can be bought, they'll be sold as suitable for
the "minimig" such as:

http://amigakit.leamancomputing.com/catalog/product_info.php?products_id=919


# Virtual Cassette Interface

## File format

The current cassette interface is a temporary method to allow loading
of a raw file off of an SD card. There is no support for ffwd/rwnd or
feedback on the current tape position. You can "reset" the tape to
the beginning by ejecting the tape and inserting again via the menu.
Further improvements are planned once the core is fully functional.

The only supported tape file format at this time is "raw". Such a file
can be created by extracting the tape data from a UEF file including 
start/stop bits. A python uef2raw.py script will do this for you and
can be found in the SVN sw/tools/acorn folder.

Usage:

  python uef2raw.py <input_file.uef> <output_file.raw>

Note: It's advisable to mark these raw files as READ ONLY once transferred
to the SD card.


## Loading

From the "Virtual Tape" OSD menu, select tape "1 (raw)" and choose a 
raw file (prepared as above). Switch to the "Cassette Player" menu
and switch "Play" to ON. The core implements motor control so the
tape will not being playing until a the Electron enables the motor.

Load the program/app/game as normal by issuing

  CHAIN"" 

There is no need to toggle play to OFF, the Electron will pause playback
as and when needed.

Rewind/Fast forward is not yet supported. You can work around this by
ejecting and re-inserting the tape which will reset the position to
the beginning of the tape. Position will be preserved in a future update
once FFwd/Rwnd are supported.


## Saving 

In order to save, you need to mount a raw file with plenty of space. You
can create blank 1 megabyte tape on Linux using:-

  dd if=/dev/zero of=tape_1.raw bs=1M count=1

Tapes up to around 400MB in size should be usable although ill advised until
there's a way to jump to specific counter locations.

Before mounting a tape, ensure PLAY and REC are OFF. Insert the tape then
switch REC to ON and then PLAY to ON. Save as normal e.g

  SAVE "TESTING"

and press return. 


## Physical Cassette Interface

A physical cassette recorder cannot yet be attached to the Replay Board.
However the core should support loading once suitable pins are routed to it.

You will need to replicate the original Acorn cassette hardware interface
for CAS IN, CAS OUT and optionally CAS MO. CAS RC is not used currently. 
In addition be careful to adjust voltage levels to be within spec for the FPGA. 


# Resources

The "Acorn Electron User Guide" is a good starting point. An on-line version
can be found at:

http://www.acornelectron.co.uk/ugs/acorn/ug-english/contents_eng.html

Following on from that is the "Acorn Electron - Advanced User Guide"

It's worth downloading the "Introductory Cassette" to really follow along
with the User Guide and when you're bored of that, hop over to
http://www.elitehomepage.org/c64/index.htm and download the Electron
version of Elite. Perhaps the only game you'll ever need ;)

So far only a handful of games have been tested with this core and not
extensively. They include, Elite, Hopper, Sphinx Adventures, Jet Set Willy,
Monsters, Repton and the Introductory Cassette.

Please report any issues with the core and any games on the fpgaarcade
forum http://www.fpgaarcade.com/punbb/viewforum.php?id=20


# Change Log

* 4/Jan/2017 - V1.2
  - More accurate video address logic
  - Fixes issue with occasional glitch in top quarter of Firetrack
    
* 28/Dec/2017 - V1.1
  - Generate a more accurate Electron PAL signal
  - Improve timing of RTC/DispEnd interrupts based on new PAL signal
  - Add authentic/compatibility video mode OSD option

* 28/Nov/2017 - V1.0
  - Improve accuracy of audio waveform with low+high pass filter
  - Active video region offset to match Electron.

* 16/Nov/2017
  - Adjust audio tone frequency with extra div2.

* 04/Nov/2017
  - Initial release, feature complete.

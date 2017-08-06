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

  - : and the shifted * are moved to the equals key "=" and "shift+="
  - COPY key is bound to "["

For example, the "shift+8" on a normal keyboard outputs "*" whilst on the 
Electron you get a "(". The "*" symbol on the Electron keyboard is shown
instead on the "shift+:" key. However, as noted above, the core uses the
"shift+=" key for a "*" due to the ":" key conflicting with the ";" key
and its shifted key "+".
  
As with the original electron, caps-lock is toggled via shift+capslock, whilst 
holding capslock will result in the function key, i.e capslock+e will output
"ELSE"

# Core Status

The core boots to the Basic prompt in mode 6 with keyboard support.

Entering of Basic programs should work. If you find any exceptions to
that, please send me a minimal program example that illustrates the issue.

This is a work in progress and missing a few key features:

  - Cassette/data loading
  - Sound

Only PAL analog output is supported @ 15.652kHz. A scan doubler will be
added once the core is more functional.
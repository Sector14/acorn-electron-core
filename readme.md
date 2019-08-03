# About

Acorn Electon core for the [FPGA Replay](http://www.fpgaarcade.com/)

Please refer to [acorn_electron/sdcard/readme.md](acorn_electron/sdcard/readme.md) for usage/status information.

A series of blog posts covering this core are available on
[my website](https://www.mups.co.uk/post/2017/07/acorn-electron-fpga-project/)

# Building

Building requires a checkout of the [Replay Common](https://github.com/Takasa/replay_common)
repository.

The build scripts expect both this repository and the replay\_common repository 
to be cloned into the same parent directory.

For a list of build options:

  python rmake\_all.py --help

To build all supported platforms run:

  python rmake\_all.py 

To build a specific target

  python rmake\_all.py --targets R1

or

  cd acorn_electron
  python rmake.py infer --target R1

Where target can be R1, V4 or any new Replay platform target.

# License

Most code is provided under the FPGA Arcade License. Some files include
a non-commericial clause.

Please refer to the header comments of individual vhd files.

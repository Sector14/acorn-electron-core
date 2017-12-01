# About

Acorn Electon core for the [FPGA Replay](http://www.fpgaarcade.com/)

Please refer to sdcard/readme.txt for usage/status information.

A series of blog posts covering this core are available on
[my website](https://www.mups.co.uk/post/2017/07/acorn-electron-fpga-project/)

# Building

If building from a checkout of the Replay SVN, run build.bat or build.sh
as with any other core.

When building from a standalone git clone of the acorn electron repo
you'll first need a copy of (or ideally sym links to) the replay\_lib/ and lib/
directories from a checkout of the [Replay Library SVN](http://svn.fpgaarcade.com/).

The build scripts expect those directories (or sym links) to be present in
the parent of the directory containing the build scripts/this readme.

# License

Most code is provided under the FPGA Arcade License. Some files include
a non-commericial clause.

Please refer to the header comments of individual vhd files.

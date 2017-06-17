
echo off

rem set paths

set REPLAYLIBPATH=..\..\replay_lib\rtl
set COMPATH=      ..\..\replay_lib\common
set LIBPATH=      ..\..\lib
set RTLPATH=      ..\source

echo Compiling core
vcom -quiet -nologo %COMPATH%\replay_lib_wrap_pack.vhd
vcom -quiet -nologo %RTLPATH%\replay_example_core_pack.vhd

vcom -quiet -nologo %RTLPATH%\replay_example_top.vhd
vcom -quiet -nologo %RTLPATH%\replay_example_core_top.vhd
vcom -quiet -nologo %COMPATH%\replay_lib_wrap.vhd
vcom -quiet -nologo %COMPATH%\replay_top_entity.vhd
vcom -quiet -nologo %COMPATH%\replay_top.vhd

echo compiling Test Bench

vcom -quiet -nologo %COMPATH%\replay_tb_pack.vhd
vcom -quiet -nologo %COMPATH%\replay_tb_wrap.vhd
vcom -quiet -nologo replay_example_tb.vhd



@echo off
set name=replay
set core=acorn_electron

set REPLAYLIBCOMMONPATH=..\..\replay_lib\common
set REPLAYLIBPATH=      ..\..\replay_lib\rtl
set LIBPATH=            ..\..\lib

rem SYNTHESIS SCRIPT USING XST (WEBPACK)

echo ---------------------------------------------------------
echo Use 'build /xil' to skip synthesis stage.
echo ---------------------------------------------------------

rem ---------------------------------------------------------
rem ---------------------------------------------------------
rem We check first for an existing tooling or we try to set up one...
if not "%XILINX%"=="" echo Using %XILINX% & goto isefound

rem check for latest version in the default location and set the pathes
set xpath=c:\xilinx
if not exist %xpath% echo No default Xilinx directory to be used & goto error
for /f "tokens=*" %%a in ('dir /AD /B /ON %xpath%\??.?') do (
  set xversion=%%a
)

if exist %xpath%\%xversion%\ISE set XILINX=%xpath%\%xversion%\ISE
if exist %xpath%\%xversion%\ISE_DS set XILINX=%xpath%\%xversion%\ISE_DS\ISE
PATH=%XILINX%\lib\nt;%XILINX%\bin\nt;%PATH%

:isefound

echo Using %XILINX%
rem ---------------------------------------------------------
rem ---------------------------------------------------------

if not exist build mkdir build
if not exist build echo Could not create directory & goto :eof
pushd build

xcopy /y %REPLAYLIBPATH%\*.vhd
xcopy /y %REPLAYLIBCOMMONPATH%\*.vhd
xcopy /y %REPLAYLIBCOMMONPATH%\*.ut
xcopy /y %REPLAYLIBCOMMONPATH%\replay_common.ucf


xcopy /y ..\source\*.vhd
xcopy /y ..\source\*.edf
xcopy /y ..\cs\*.edn
xcopy /y ..\cs\*.ngc

copy ..\%name%.ucf %name%.ucf
copy ..\%name%.scr
copy ..\%name%.prj

if "%1"=="/xil" goto xilinx

xst -ifn %name%.scr -ofn %name%.srp
if errorlevel 1 goto error

:xilinx
ngdbuild -nt on -uc replay_common.ucf -uc %name%.ucf %name%.ngc %name%.ngd
if errorlevel 1 goto error
map -pr b -timing %name%.ngd -o %name%.ncd %name%.pcf
if errorlevel 1 goto error
par -w -ol high %name%.ncd %name%.ncd %name%.pcf
if errorlevel 1 goto error
bitgen %name%.ncd %core%.bit -w -f %name%.ut
if errorlevel 1 goto error
trce -v 10 -o %name%.twr %name%.ncd %name%.pcf
if errorlevel 1 goto error

copy %core%.bin ..\sdcard

popd
echo Done
goto :eof
:error
popd
echo Error!
pause

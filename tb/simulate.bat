@echo off
rem SIMULATION COMPILE SCRIPT USING ISIM (WEBPACK)

echo ---------------------------------------------------------
echo Use 'simulate /run' to skip compilation stage.
echo Use 'simulate /view' to show previous simulation results.
echo ---------------------------------------------------------

rem We check first for an existing tooling or we try to set up one...
if not "%XILINX%"=="" goto isefound

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

set name=Replay

if not exist sim mkdir sim
if not exist sim echo Could not create directory & goto :eof
pushd sim

rem if we have a WDB file, we can view it if requested (otherwise we remove it)
if not exist %name%.wdb echo No previous WDB found & goto nowdb
if "%1"=="/view" goto view

del %name%.wdb

:nowdb

rem if we have a EXE, we can run it if requested (otherwise we remove it)
if not exist %name%.exe echo No previous executable found & goto noexe
if "%1"=="/run" goto simulate

del %name%.exe

:noexe

rem we do a fresh compile, thus we collect all sources from the project
xcopy /y ..\..\replay_lib\rtl\*.vhd .
xcopy /y ..\..\replay_lib\tb\*.vh .
xcopy /y ..\..\replay_lib\tb\*.v .
xcopy /y ..\..\replay_lib\tb\*.vhd .

xcopy /y ..\source\*.vhd .
xcopy /y ..\tb\*.vhd .

xcopy /y ..\%name%.prj %name%.prj
type ..\%name%_tb.prj >> %name%.prj

rem verbose & no multthreading - fallback in case of problems
rem fuse -v 1 -mt off -incremental -prj %name%.prj -o %name%.exe -t %name%

fuse -incremental -prj %name%.prj -o %name%.exe -t a_%name%_tb
rem fuse --mt off -prj %name%.prj -o %name%.exe -t a_%name%_tb
if errorlevel 1 goto error

rem Check for the EXE again, independent of the errorlevel of fuse...
if not exist %name%.exe echo No simulation executable created & goto error

:simulate

rem Open the iSIM GUI and run the simulation

%name%.exe -gui -f ..\%name%.cmd -wdb %name%.wdb -log %name%.log -view ..\%name%.wcfg
if errorlevel 1 goto error

popd
echo Done
goto :eof

:view

rem Only start the viewer on an existing wave configuration (from an old simulation)

isimgui.exe -view ..\%name%.wcfg
if errorlevel 1 goto error

popd
echo Done
goto :eof

:error
popd
echo Error!
pause

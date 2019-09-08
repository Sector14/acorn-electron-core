# Impact command line batch file
# Usage: impact -batch impact.cmd

setmode -bscan
setcable -p auto
addDevice -p 1 -file "build_ise/acorn_electron.bit"
program -p 1 
quit


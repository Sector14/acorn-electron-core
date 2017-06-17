#!/bin/bash

if [ -d "/opt/Xilinx" ]; then
	XILINX_VERSIONS=(/opt/Xilinx/*)
	XILINX_PATH="${XILINX_VERSIONS[0]}"
	echo "Found $XILINX_PATH ..."
fi

if [ -z "$ISE_PATH" ]; then
	ISE_PATH="$XILINX_PATH/ISE_DS/" 
fi

if [ ! -d "$ISE_PATH" ]; then
  echo "ISE is not under $ISE_PATH ! Please adjust path in script or use ISE_PATH env!"
  exit 
fi

MACHINE_TYPE=`uname -m`
if [ ${MACHINE_TYPE} == 'x86_64' ]; then
  source "${ISE_PATH}settings64.sh"
else
  source "${ISE_PATH}settings32.sh"
fi

NAME="replay"
CORE="acorn_electron"
REPLAY_LIB_COMMON_PATH="./../../replay_lib/common"
REPLAY_LIB_PATH="./../../replay_lib/rtl"
LIB_PATH="./../../lib"

echo "---------------------------------------------------------"
echo "create build dir and copy files                          "
echo "---------------------------------------------------------"

mkdir -p build
cd build
pwd

cp $REPLAY_LIB_PATH/*.vhd ./

cp ./../source/*.vhd ./
#cp ./../source/*.edf ./

cp $REPLAY_LIB_COMMON_PATH/*.vhd ./
cp $REPLAY_LIB_COMMON_PATH/*.ut ./
cp $REPLAY_LIB_COMMON_PATH/replay_common.ucf ./

# chip scope
cp ./../cs/*.edn ./
cp ./../cs/*.ngc ./

# project files
cp ./../$NAME.ucf ./
cp ./../$NAME.scr ./
cp ./../$NAME.prj ./

echo "---------------------------------------------------------"
echo "xst                                                      "
echo "---------------------------------------------------------"

xst -ifn $NAME.scr -ofn $NAME.srp || exit $?

echo "---------------------------------------------------------"
echo "start ngdbuild                                           "
echo "---------------------------------------------------------"

ngdbuild -nt on -uc replay_common.ucf -uc $NAME.ucf $NAME.ngc $NAME.ngd || exit $?

echo "---------------------------------------------------------"
echo "start map                                                "
echo "---------------------------------------------------------"

map -pr b $NAME.ngd -o $NAME.ncd $NAME.pcf || exit $?

echo "---------------------------------------------------------"
echo "start par                                                "
echo "---------------------------------------------------------"

par -w -ol high $NAME.ncd $NAME.ncd $NAME.pcf || exit $?

echo "---------------------------------------------------------"
echo "start trce                                               "
echo "---------------------------------------------------------"

trce -v 10 -o $NAME.twr $NAME.ncd $NAME.pcf || exit $?

echo "---------------------------------------------------------"
echo "start bitgen                                             "
echo "---------------------------------------------------------"

bitgen $NAME.ncd $CORE.bit -w -f $NAME.ut || exit $?

cp $CORE.bin ../sdcard || exit $?

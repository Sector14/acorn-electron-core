#!/bin/bash
# SIMULATION COMPILE SCRIPT USING ISIM (WEBPACK)
#
# TODO: -view and -run switches not implemented
# TODO: Remove hardcoded path for fuse

NAME="replay"
REPLAY_LIB_BASE_PATH="../replay_lib"
LIB_PATH="../lib"

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
  FUSE="${ISE_PATH}ISE/bin/lin64/fuse"
else
  source "${ISE_PATH}settings32.sh"
  FUSE="${ISE_PATH}ISE/bin/lin/fuse"
fi

# ----------------------------------------------------------------------

mkdir -p sim

# Compile project for simulation
cp ${REPLAY_LIB_BASE_PATH}/rtl/*.vhd ./sim
cp ${REPLAY_LIB_BASE_PATH}/common/*.vhd ./sim
cp ${REPLAY_LIB_BASE_PATH}/tb/*.vh ./sim
cp ${REPLAY_LIB_BASE_PATH}/tb/*.v ./sim
cp ${REPLAY_LIB_BASE_PATH}/tb/*.vhd ./sim

cp ./source/*.vhd ./sim
cp ./tb/*.vhd ./sim

cp ${NAME}.prj sim/
cat tb/${NAME}_tb.prj >> sim/${NAME}.prj

pushd sim

${FUSE} -incremental -prj ${NAME}.prj -o ${NAME}.bin -t a_${NAME}_tb || exit $?

echo "Running Simulation in iSIM gui"

./${NAME}.bin -gui -f ../tb/${NAME}.cmd -wdb ${NAME}.wdb -log ${NAME}.log -view ../tb/${NAME}.wcfg || exit $?

popd


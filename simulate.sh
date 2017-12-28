#!/bin/bash
# SIMULATION COMPILE SCRIPT USING ISIM (WEBPACK)
#
# Usage:
#   ./simulate.sh [test_bench_name]
#
# test_bench_name if not provided will default to "replay_tb" resulting in the
# "tb/replay_tb.prj" being included and the test bench entity "a_[test_bench_name]_tb"
#
# Example:
#   To run the Acorn Electron ram only test bench.
#
#   ./simulate.sh ram_d32k_w8_tb

NAME="replay"
TB_NAME="${1:-${NAME}_tb}"
REPLAY_LIB_BASE_PATH="../replay_lib"
LIB_PATH="../lib"

# Clear out cmd line args as xilinx settings tries to use them
args=$@
shift

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

# cpu
cp ${LIB_PATH}/cpu/t65/T65*.vhd ./sim
# rc filter
cp ${LIB_PATH}/generic/filters/rc_butter_1O/*.vhd ./sim

cp ./source/*.vhd ./sim
cp ./source/override_lib/*.vhd ./sim
cp ./tb/*.vhd ./sim

cp ${NAME}.prj sim/${TB_NAME}.prj
cat tb/${TB_NAME}.prj >> sim/${TB_NAME}.prj

pushd sim

${FUSE} -incremental -prj ${TB_NAME}.prj -o ${TB_NAME}.bin -t a_${TB_NAME} || exit $?

echo "Running Simulation in iSIM gui"

./${TB_NAME}.bin -gui -f ../tb/${TB_NAME}.cmd -wdb ${TB_NAME}.wdb -log ${TB_NAME}.log -view ../tb/${TB_NAME}.wcfg || exit $?

popd


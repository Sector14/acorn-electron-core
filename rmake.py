#!/usr/bin/env python
import runpy, sys
sys.argv.append('--root=../../') # default is ../../
sys.argv.append('--core=acorn_electron') # default is ../../
runpy.run_path('../../replay_common/scripts/common.py')


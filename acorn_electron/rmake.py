#!/usr/bin/env python
import runpy, sys
sys.argv.append('--group=computer')
sys.argv.append('--core=acorn_electron')
runpy.run_path('../../replay_common/scripts/common.py')

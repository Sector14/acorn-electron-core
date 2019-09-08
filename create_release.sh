#!/bin/bash
# Crude packaging script for releases
hash zip 2>/dev/null || { echo >&2 "zip required but not found.  Aborting."; exit 1; }
hash git 2>/dev/null || { echo >&2 "git required but not found.  Aborting."; exit 1; }

VERSION=`git describe --tags`
DATE=`date -u '+%Y%m%d%H%M%S'`
RELEASE_ZIP="AcornElectron_${VERSION}_${DATE}.zip"

######################################################################

echo "Creating release zip in ${RELEASE_ZIP}"
pushd acorn_electron/sdcard > /dev/null

zip ../../"${RELEASE_ZIP}" acorn_electron.bit changelog.md empty.rom readme.md replay.ini

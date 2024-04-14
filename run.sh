#!/usr/bin/env bash

export AMPII_DB_PATH=database
export AMPII_SCALE_PATH=/dev/hidraw2
export LINES
export COLUMNS

pack run ampii.ipkg "$@"

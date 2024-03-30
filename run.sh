#!/usr/bin/env bash

export AMPII_DB_PATH=database
export AMPII_SCALE_PATH=/dev/hidraw2

pack run ampii.ipkg "$@"

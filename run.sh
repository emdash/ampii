#!/usr/bin/env bash

shopt -s checkwinsize ; (:;:)

export LINES
export COLUMNS
export AMPII_DB_PATH=database
export AMPII_SCALE_PATH=/dev/hidraw0
export IDRIS_TUI_MAINLOOP=input-shim

function localURL {
    echo "http://$(hostname -I | cut -d ' ' -f 1):8000"
}

function hostQR {
    qrencode -o "upload/qrcode.png" "$(localURL)"
}

function shim {
    # generate a QR code for this host
    hostQR 2>&1 > /dev/null

    # tell the app to load the image
    echo '{"tag": "Image", "contents": ["upload/qrcode.png"]}'

    # now actually start the shim
    if test -e "${AMPII_SCALE_PATH}"
    then
	python input-shim.py \
	   "${AMPII_SCALE_PATH}" \
	   2>shim_log
    else
	echo "Scale path does not exist"
	python input-shim.py 2>shim_log
    fi
}

shim | ./build/exec/ampii "$@" 2> debug_log

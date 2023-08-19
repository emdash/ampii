#!/usr/bin/env bash

# Use `fzf` and `jq` to navigate recipe databases exported from the
# recipecleaner browser plugin on the command line.

CACHE_DIR=".image_cache"

# hack for launching locally-installed copy of img2sixel
function sixel {
    export LD_LIBRARY_PATH="${HOME}/.local/lib64/:${LD_LIBRARY_PATH}"
    ~/.local/bin/img2sixel "$@"
}

# print all recipes to stdout
function all {
    jq '. | delpaths([["selection"], ["options"], ["filters"]]) | to_entries' < recipe_db_2.json
}

# print the specific recipe id to stdout
function get {
    jq ".\"$1\"" < recipe_db_2.json
}

# get the cached image for the given id
function image {
    local -r id="$1"
    mkdir -p "${CACHE_DIR}"
    local -r cache_file="${CACHE_DIR}/${id}"
    if test -e "${cache_file}"
    then
	cat "${cache_file}"
    else
	local -r url="$(complete "${id}" | jq -r .image)"
	curl -s "${url}" | tee "${cache_file}"
    fi
}

# present an FZF menu
function menu {
    fzf \
	-n 2,3 \
	--with-nth=2 \
	-d '\t' \
	--ansi \
	--preview="${self} preview {1}" \
	--preview-window=wrap
}

# prompt user to choose an item and print its id to stdout
function choose {
    summary | menu | cut -f 1
}

# return a summary of the database
function summary {
    all | jq -r '.[] | [.key, .value.name, .value.description, .value.image] | @tsv'
}

# browse the recipe database
function browse {
    choose | show
}

# print a complete recipe to stdout
function complete {
    get "$1" | jq '{name, description, ingredients, instructionList, image}'
}

# show visual summary of recipe for FZF preview
function preview {
    if test -n "$1"
    then
	id="$1"
    else
	read id
    fi
    echo "${id}"
    local data="$(complete "${id}")"
    local name="$(echo "${data}" | jq -r '.name')"
    local desc="$(echo "${data}" | jq -r '.description')"
    image "${id}" | chafa -s 50x40 --format=symbols
    echo
    echo "${name}"
    echo
    echo "${desc}"
}

# insert extra blank after newline
function extblank {
    while read line
    do
	echo "${line}"
	echo
    done
}

# show high-quality, full-version of recipe on command-line
function show {
    if test -n "$1"
    then
	id="$1"
    else
	read id
    fi
    echo "${id}"
    local data="$(complete "${id}")"
    local name="$(echo "${data}" | jq -r '.name')"
    local desc="$(echo "${data}" | jq -r '.description')"
    image "${id}" | chafa -s 50x40

    tabs 1,4,16,24,36

    echo
    echo "${name}" | fold -s -w 80
    echo
    echo "${desc}" | fold -s -w 80
    echo

    echo
    echo "**** Ingredients ****"
    echo
    echo "${data}" | jq -r '.ingredients | .[] | [.quantity, .unit, .ingredient] | @tsv'
    echo

    echo "**** Instructions ****"
    echo
    echo "${data}" | jq -r '.instructionList | .[]' | extblank | fold -s -w 80

    tabs -8
}

self="$0"

"$@"

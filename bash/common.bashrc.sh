#!/usr/bin/env bash

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

# Set my local ~/bin dir to override the PATH
export PATH="~/bin:${PATH}"

# BASH history
# https://unix.stackexchange.com/a/26253/55912
# don't put duplicate lines in the history
# don't save commands which start with a space
HISTCONTROL=ignoredups:erasedups:ignorespace
# append to the history file, don't overwrite it
shopt -s histappend

function load_devtools {
     TOOLS_DIR="$1"
    if [ -d $TOOLS_DIR ]
    then
        for f in $(ls $TOOLS_DIR/*.sh)
        do
            source $f || echo "Failed to load ${f}"
        done
    fi
}

function load_alias {
    alias_script="$1"
    test -f "${alias_script}" && source "${alias_script}"
}

BASE_DIR="${HOME}/dotfiles/bash"

load_alias "${BASE_DIR}/common.alias.sh"
load_devtools "${BASE_DIR}/common.devtools"


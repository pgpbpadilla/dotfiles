#!/usr/bin/env bash

BASE_DIR="${HOME}/dotfiles/bash"

source "${BASE_DIR}/common.bashrc.sh"
load_alias "${BASE_DIR}/macos.alias.sh"
load_devtools "${BASE_DIR}/macos.devtools"

# Export user binaries
export PATH="/usr/local/bin:$PATH"


# User-specific configuration files (a.k.a. /dotfiles/)
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# This is used for example by:
# - mpd: https://mpd.readthedocs.io/en/latest/user.html#configuration
export XDG_CONFIG_HOME="${HOME}/.config"

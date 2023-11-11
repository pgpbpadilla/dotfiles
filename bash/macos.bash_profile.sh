#!/usr/bin/env bash

BASE_DIR="${HOME}/dotfiles/bash"

source "${BASE_DIR}/common.bashrc.sh"
load_alias "${BASE_DIR}/macos.alias.sh"
load_devtools "${BASE_DIR}/macos.devtools"

# Export user binaries
export PATH="~/bin:/usr/local/bin:$PATH"


# User-specific configuration files (a.k.a. /dotfiles/)
# https://specifications.freedesktop.org/basedir-spec/basedir-spec-latest.html
# This is used for example by:
# - mpd: https://mpd.readthedocs.io/en/latest/user.html#configuration
export XDG_CONFIG_HOME="${HOME}/.config"

# pin gnupg: 2.4+ have a bug: https://stackoverflow.com/q/76388376/400544
# fix: https://stackoverflow.com/a/76404609/400544
export PATH="/usr/local/Cellar/gnupg@2.2/2.2.41/bin:$PATH"

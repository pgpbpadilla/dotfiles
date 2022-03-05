#!/usr/bin/env bash

BASE_DIR="${HOME}/dotfiles/bash"

source "${BASE_DIR}/common.bashrc.sh"
load_alias "${BASE_DIR}/macos.alias.sh"
load_devtools "${BASE_DIR}/macos.devtools"

# Export user binaries
export PATH="/usr/local/bin:$PATH"



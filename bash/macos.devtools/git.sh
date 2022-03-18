#!/usr/bin/env bash

# Git: Fixes crashing gitk due to geometry bug
# in config file, see: https://stackoverflow.com/q/65938739/400544
alias gitk="sed -i .bak 's/zoomed/normal/g' ~/.config/git/gitk && /usr/local/bin/gitk"

# Git
export PATH="/usr/local/git/bin:$PATH"


#!/usr/bin/env bash

# 
eval "$(/opt/homebrew/bin/brew shellenv)"

# Enable bash-completion
# brew install bash-completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"

# Fixes warning:
# >> Homebrew's "sbin" was not found in your PATH but you have installed
# >> formulae that put executables in /usr/local/sbin.
export PATH="/usr/local/sbin:$PATH"




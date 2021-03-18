#!/bin/bash


export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

export PATH="/usr/local/bin:$PATH"
export PATH="~/bin:$PATH"

export PATH="/Library/TeX/Distributions/Programs/texbin:$PATH"
export PATH="/usr/local/git/bin:$PATH"

export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

# BASH history
# https://unix.stackexchange.com/a/26253/55912
# don't put duplicate lines in the history
# don't save commands which start with a space
HISTCONTROL=ignoredups:erasedups:ignorespace
# append to the history file, don't overwrite it
shopt -s histappend

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Git prompt
if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
__GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
GIT_PROMPT_ONLY_IN_REPO=1
source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi

# Go stuff
export GOPATH=$HOME/code/go
export GOROOT=/usr/local/opt/go/libexec
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:$GOROOT/bin

# Python stuff
eval "$(pyenv init -)"
# Auto-activate virtual environment
eval "$(pyenv virtualenv-init -)"


# Enable bash-completion
# brew install bash-completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"


# Basher: https://github.com/basherpm/basher
export PATH="$HOME/.basher/bin:$PATH"
eval "$(basher init - bash)"

# Load extra DevTools

TOOLS_DIR=~/.bash-devtools
if [ -d $TOOLS_DIR ]
then
    for f in $(ls $TOOLS_DIR)
    do
        source ${TOOLS_DIR}/$f || echo "Failed to load ${f}"
    done
fi


# Load personal aliases

test -f ~/.alias.sh && source ~/.alias.sh


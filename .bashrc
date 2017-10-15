#!/bin/bash

export CLICOLOR=1
export LSCOLORS=GxFxCxDxBxegedabagaced

alias cdapps='cd ~/code'
alias cdtmp="cd ~/tmp"

alias la='ls -la'
alias ll='ls -l'
alias lt='ls -lt'
alias ..='cd ..'
alias c++='clang++ -std=c++11 -stdlib=libc++'
alias lat='ls -lat'
# alias startHTTP='python -m SimpleHTTPServer'
alias emcs='open -a Emacs.app'
alias docker-qst='eval $(docker-machine env default)'


export PATH="/usr/local/bin:$PATH"
export PATH="~/bin:$PATH"

alias chrome-unsec='open -a Google\ Chrome --disable-web-security'

### Added by the Heroku Toolbelt
export PATH="/usr/local/heroku/bin:$PATH"

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

# source /usr/local/bin/git-completion.bash


#!/usr/bin/env bash

# https://github.com/pyenv/pyenv

export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
eval "$(pyenv init -)"

# https://github.com/pyenv/pyenv-virtualenv
eval "$(pyenv virtualenv-init -)"

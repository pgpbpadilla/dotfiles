#!/usr/bin/env bash

## Git prompt
# https://github.com/magicmonty/bash-git-prompt
# *WARN*: The following snippet only becomes os-agnostic if 
# bash-git-prompt is installed via =git clone=
if [ -f "$HOME/.bash-git-prompt/gitprompt.sh" ]; then
    GIT_PROMPT_ONLY_IN_REPO=1
    source $HOME/.bash-git-prompt/gitprompt.sh
fi

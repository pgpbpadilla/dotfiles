#!/bin/bash

export PATH="/usr/local/bin:$PATH"
export PATH="/Library/TeX/Distributions/Programs/texbin:$PATH"
export PATH="/usr/local/git/bin:$PATH"
export PATH="$PATH:$HOME/.rvm/bin" # Add RVM to PATH for scripting

if [ -f $(brew --prefix)/etc/bash_completion ]; then
  . $(brew --prefix)/etc/bash_completion
fi

# Git prompt
if [ -f "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh" ]; then
__GIT_PROMPT_DIR=$(brew --prefix)/opt/bash-git-prompt/share
GIT_PROMPT_ONLY_IN_REPO=1
source "$(brew --prefix)/opt/bash-git-prompt/share/gitprompt.sh"
fi

# Enable bash-completion
# brew install bash-completions
[[ -r "/usr/local/etc/profile.d/bash_completion.sh" ]] && . "/usr/local/etc/profile.d/bash_completion.sh"



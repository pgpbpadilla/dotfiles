# Wow, nothing in here really belongs in here...

# TODO: move to dev-tools
# SSH Agent autstart
SSH_ENV="$HOME/.ssh/agent-environment"

function start_agent {
    echo "Initialising new SSH agent..."
    /usr/bin/ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    /usr/bin/ssh-add;
}

# Source SSH settings, if applicable

if [ -f "${SSH_ENV}" ]; then
    . "${SSH_ENV}" > /dev/null
    #ps ${SSH_AGENT_PID} doesn't work under cywgin
    ps -ef | grep ${SSH_AGENT_PID} | grep ssh-agent$ > /dev/null || {
        start_agent;
    }
else
    start_agent;
fi


# TODO: devtools
export PATH="$HOME/.jenv/bin:$PATH"
eval "$(jenv init -)"


complete -C /usr/bin/terraform terraform

# https://github.com/pyenv/pyenv#basic-github-checkout
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init --path)"
# https://github.com/pyenv/pyenv-virtualenv
eval "$(pyenv virtualenv-init -)"


# export NVM_DIR="$HOME/.nvm"
# [ -s "$NVM_DIR/nvm.sh" ] && \. "$NVM_DIR/nvm.sh"  # This loads nvm
# [ -s "$NVM_DIR/bash_completion" ] && \. "$NVM_DIR/bash_completion"  # This loads nvm bash_completion
# 
# export PATH="$HOME/.jenv/bin:$PATH"
# eval "$(jenv init -)"
# 
# export NODE_PATH=/usr/lib/nodejs:/usr/share/nodejs
# 
# if [ -f "$HOME/.bash-git-prompt/gitprompt.sh" ]; then
#     GIT_PROMPT_ONLY_IN_REPO=1
#     source $HOME/.bash-git-prompt/gitprompt.sh
# fi

#GDK_DPI_SCALE=0.5

# Open Zoom with big fonts
# alias zzz='QT_SCALE_FACTOR=1.8 zoom &'

############################################################################
# ***WARN*** DO NOT PUSH THIS PUBLICLY, IT SHOULD ONLY BE WITHIN THE KN REPO
# KN Proxy
export HTTP_PROXY="http://zscaler.proxy.int.kn:80"
export HTTPS_PROXY="http://zscaler.proxy.int.kn:80"
export NO_PROXY="127.0.0.1,localhost,.int.kn"
  
export http_proxy=$HTTP_PROXY
export https_proxy=$HTTPS_PROXY
export no_proxy=$NO_PROXY

export SSO_USERNAME=pablo.padillabeltran # in case the Linux user you created does not match your SSO name

# ***END OF WARNING***
#############################################################################


# TODO: move to alias file
# some more ls aliases
alias ll='ls -l'
alias la='ls -lat'
alias l='ls -CF'
alias ..='cd ..'
alias cdcode='cd ~/code'

# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi


alias cdcode='cd ~/code'
alias cdtmp="cd ~/tmp"

alias la='ls -la'
alias ll='ls -l'
alias lt='ls -lt'
alias ..='cd ..'
alias lat='ls -lat'

# Git: Fixes crashing gitk due to geometry bug
# in config file, see: https://stackoverflow.com/q/65938739/400544
alias gitk="sed -i .bak 's/zoomed/normal/g' ~/.config/git/gitk && /usr/local/bin/gitk"

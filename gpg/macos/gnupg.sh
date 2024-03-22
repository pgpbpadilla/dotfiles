#!/usr/bin/env bash
### WARN: THIS FILE IS AUTO-GENERATED FROM AN Org FILE VIA org-babel-tangle
### DO NOT MODIFY BY HAND

CONF_DIR="$(pwd)/macos/$(uname -m)"
if ! mkdir -p $CONF_DIR;
then
    echo "Failed to create $CONF_DIR"
    exit -1
fi
echo "Location for configuration: $CONF_DIR"

GPG_AGENT_CONF=$CONF_DIR/gpg-agent.conf

cat <<EOF > $GPG_AGENT_CONF
allow-emacs-pinentry
pinentry-program $(brew --prefix)/bin/pinentry-mac
EOF

echo "Created config file $GPG_AGENT_CONF"

ln -svf $(pwd)/macos/*.conf ${HOME}/.gnupg

ln -svf $(pwd)/macos/$(uname -m)/*.conf ${HOME}/.gnupg

echo "Reloading gpg-agent ..."
gpgconf --kill gpg-agent
gpgconf --list-components

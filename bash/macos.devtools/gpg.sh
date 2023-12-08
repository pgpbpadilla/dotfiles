#!/usr/bin/env bash

# pin gnupg: 2.4+ have a bug: https://stackoverflow.com/q/76388376/400544
# fix: https://stackoverflow.com/a/76404609/400544
export PATH="$(brew --prefix)/opt/gnupg@2.2/bin:$PATH"

#!/usr/bin/env bash

# uses my private Homebrew tap formula:
# https://github.com/pgpbpadilla/homebrew-pgpb
# $ brew info gnupg@2.2.41
# ==> pgpbpadilla/pgpb/gnupg@2.2.41: stable 2.2.41
# fixes: Gnupg team issue tracker: https://dev.gnupg.org/T6481
export PATH="$(brew --prefix)/Cellar/gnupg@2.2.41/2.2.41/bin:$PATH"

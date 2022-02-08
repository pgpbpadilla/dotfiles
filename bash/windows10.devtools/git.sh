#!/usr/bin/env bash

### Fixes =git gui= launch crash
### https://github.com/msys2/MSYS2-packages/issues/801

# Fix 1: set missing env-var 
export GIT_GUI_LIB_DIR=/c/msys64/usr/share/git-gui/lib

# Fix 2: use Git binary from GitBash (https://gitforwindows.org/)
alias git='/c/Users/pablo.padillabeltran/AppData/Local/Programs/Git/mingw64/bin/git.exe'


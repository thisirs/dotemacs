#! /usr/bin/env bash

# Full path of script
SCRIPT_PATH=$(readlink -f "$(dirname "$0")")

# Init and update submodules
cd $SCRIPT_PATH
git submodule init
git submodule update

# Backup existing .emacs and .emacs.d
if [ -e "$HOME/.emacs" ]; then
    echo "Moving existing ~/.emacs to ~/.emacs-$(date +%s).old"
    mv $HOME/.emacs{,-$(date +%s).old}
fi

if [ -e "$HOME/.emacs.d" ]; then
    echo "Moving existing ~/.emacs.d to ~/.emacs.d-$(date +%s).old"
    mv $HOME/.emacs.d{,-$(date +%s).old}
fi

# Create soft link
ln -sv $SCRIPT_PATH ~/.emacs.d


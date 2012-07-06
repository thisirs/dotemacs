#! /usr/bin/env bash

# Full path of script
SCRIPT_PATH=$(readlink -f "$(dirname "$0")")

# Init and update submodules
cd $SCRIPT_PATH
git submodule init
git submodule update

# Backup existing .emacs and .emacs.d
[ -e "$HOME/.emacs" ] && { echo "Moving existing ~/.emacs to ~/.emacs-$(date +%s).old"; mv $HOME/.emacs{,-$(date +%s).old}; }
[ -e "$HOME/.emacs.d" ] && { echo "Moving existing ~/.emacs.d to ~/.emacs.d-$(date +%s).old"; mv $HOME/.emacs.d{,-$(date +%s).old}; }

# Create soft link
ln -s -t ~/ $SCRIPT_PATH


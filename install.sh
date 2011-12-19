#! /usr/bin/env bash

# Full path of script
SCRIPT_PATH=$(readlink -f "$(dirname "$0")")

# Where to copy dotemacs repo
INSTALL_PATH=${1:-"$HOME/git-repos"}

mkdir -p $INSTALL_PATH
cd $INSTALL_PATH
cp -r $SCRIPT_PATH $INSTALL_PATH

cd dotemacs

git submodule init
git submodule update

#Sauvegarde de la configuration existante
[ -e "$HOME/.emacs" ] && { echo "Moving existing ~/.emacs to ~/.emacs-$(date +%s).old"; mv $HOME/.emacs{,-$(date +%s).old}; }
[ -e "$HOME/.emacs.d" ] && { echo "Moving existing ~/.emacs.d to ~/.emacs.d-$(date +%s).old"; mv $HOME/.emacs.d{,-$(date +%s).old}; }

#Création des liens
cd $HOME
ln -s "$INSTALL_PATH/dotemacs/.emacs.d"


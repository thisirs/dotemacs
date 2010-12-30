#! /usr/bin/env bash

INSTALL_PATH=${1:-"/$HOME/git-repos"}

cd $INSTALL_PATH
git clone git@github.com:thisirs/dotemacs.git
git submodule init
git submodule update

#Sauvegarde de la configuration existante
[ -e "$HOME/.emacs" ] && { echo "Moving existing ~/.emacs to ~/.emacs.old"; mv $HOME/.emacs{,.old}; }
[ -e "$HOME/.emacs.d" ] && { echo "Moving existing ~/.emacs.d to ~/.emacs.d.old"; mv $HOME/.emacs.d{,.old}; }

#Création des liens
cd $HOME
ln -s "$INSTALL_PATH/dotemacs/.emacs"
ln -s "$INSTALL_PATH/dotemacs/.emacs.d"


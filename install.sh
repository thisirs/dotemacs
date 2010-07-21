#! /usr/bin/env bash

INSTALL_PATH=~/dotemacs

#Création de la structure
[ -e "$INSTALL_PATH" ] && { echo "Install path $INSTALL_PATH already exists, aborting..."; exit 1; }
mkdir -p "$INSTALL_PATH"
cp -r ../dotemacs "$INSTALL_PATH"

#Téléchargement des derniers paquets
cd "$INSTALL_PATH"
git clone git://repo.or.cz/anything-config.git
git clone git://repo.or.cz/org-mode.git

#Sauvegarde de la configuration existante
[ -e "$HOME/.emacs" ] && { echo "Moving existing ~/.emacs to ~/.emacs.old"; mv $HOME/.emacs{,.old}; }
[ -e "$HOME/.emacs.d" ] && { echo "Moving existing ~/.emacs.d to ~/.emacs.d.old"; mv $HOME/.emacs.d{,.old}; }

#Création des liens
cd $HOME
ln -s "$INSTALL_PATH/dotemacs/.emacs"
ln -s "$INSTALL_PATH/dotemacs/.emacs.d"

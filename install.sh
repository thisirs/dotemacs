#! /usr/bin/env bash

INSTALL_PATH=~/git

#Création de la structure
mkdir -p "$INSTALL_PATH/dotemacs"
mv .emacs.d "$INSTALL_PATH/dotemacs"
mv .emacs "$INSTALL_PATH/dotemacs"

#Téléchargement des derniers paquets
cd "$INSTALL_PATH"
git clone git://repo.or.cz/anything-config.git
git clone git://repo.or.cz/org-mode.git

#Sauvegarde de la configuration existante
[ -e "$HOME/.emacs" ] && {echo "Moving existing ~/.emacs to ~/.emacs.old"; mv $HOME/.emacs{,.old}}
[ -e "$HOME/.emacs.d" ] && {echo "Moving existing ~/.emacs.d to ~/.emacs.d.old"; mv $HOME/.emacs.d{,.old}}

#Création des liens
cd $HOME
ln -s "$INSTALL_PATH/dotemacs/.emacs"
ln -s "$INSTALL_PATH/dotemacs/.emacs.d"

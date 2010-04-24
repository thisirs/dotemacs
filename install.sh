#! /usr/bin/env bash

[ -e "$HOME/.emacs" ] && mv $HOME/.emacs{,.old}
[ -e "$HOME/.emacs.d" ] && mv $HOME/.emacs.d{,.old}
GIT_PATH=$(pwd)
cd $HOME
ln -s $GIT_PATH/.emacs
ln -s $GIT_PATH/.emacs.d

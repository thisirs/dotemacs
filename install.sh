#! /usr/bin/env bash

[ -e ~/.emacs ] && mv ~/.emacs{,.old}
[ -e ~/.emacs.d ] && mv ~/.emacs.d{,.old}
GIT_PATH=$(pwd)
cd
ln -s $GIT_PATH/.emacs
ln -s $GIT_PATH/.emacs.d

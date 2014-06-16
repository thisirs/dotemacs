#! /bin/bash -

# Minimum emacs version and repo url
EMACS_VERSION=24.3
DOTEMACS_REPO=https://github.com/thisirs/dotemacs.git

# Do some preliminary checks
hash git > /dev/null 2>&1 \
    || { echo >&2 "git not installed. Aborting."; exit 1; }

hash emacs > /dev/null 2>&1 \
    || { echo >&2 "emacs not installed. Aborting."; exit 1; }

emacs -Q --batch --eval "(kill-emacs (if (version< emacs-version \"$EMACS_VERSION\") 1 0))" > /dev/null 2>&1 \
    || { echo >&2 "emacs > $EMACS_VERSION required."; exit 1; }

# Backup existing .emacs and .emacs.d
if [ -e "$HOME/.emacs" ]; then
    echo "Moving existing ~/.emacs to ~/.emacs-$(date +%s).old"
    mv $HOME/.emacs{,-$(date +%s).old}
fi

if [ -e "$HOME/.emacs.d" ]; then
    echo "Moving existing ~/.emacs.d to ~/.emacs.d-$(date +%s).old"
    mv $HOME/.emacs.d{,-$(date +%s).old}
fi

git clone $DOTEMACS_REPO $HOME/.emacs.d

cd $HOME/.emacs.d
git submodule init
git submodule update

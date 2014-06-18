#! /bin/bash -

# Updates all submodules. Submodules are mainly clones of github
# repos. Some of them are clones of repos i forked in github to be
# able to modify them easily. Therefore for each submodule i have a
# origin repo and a upstream repo for those that are clones of forks
# in github.

TMPFILE=$(mktemp)
trap 'rm -f $TMPFILE' EXIT

cat << 'EOF' > $TMPFILE
echo -e "\e[31mRepository:\e[0m $(basename $(pwd))"

BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch is $BRANCH"

if [ ! "xmaster" == "x$BRANCH" ]; then
  git checkout master
fi

BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch is now $BRANCH"

echo "Fetching origin"
git fetch origin > /dev/null 2>&1

if $(git fetch upstream > /dev/null 2>&1); then
  echo "Upstream branch detected"

  # Make origin and local in sync
  git rebase origin/$BRANCH
  git push origin $BRANCH

  # Save commits id before destroying them with a rebase
  git branch --force $BRANCH-before-rebase

  # Rebase local changes onto up-to-date upstream
  if ! git rebase upstream/master; then
    git rebase --abort
  fi

  # Push
  git push -f origin $BRANCH
  git push -f origin $BRANCH-before-rebase
else
  echo "No upstream branch"
  git rebase origin/$BRANCH
fi

# For git submodule foreach not to fail
exit
EOF

git submodule foreach "bash $TMPFILE"


#! /bin/bash -

# Updates all submodules. Submodules are mainly clones of github
# repos. Some of them are clones of repos i forked in github to be
# able to modify them easily. Therefore for each submodule i have a
# origin repo and a upstream repo for those that are clones of forks
# in github.

TMPFILE=$(mktemp)
trap 'rm -f $TMPFILE' EXIT

cat << 'EOF' > $TMPFILE
echo "Repository: $(basename $(pwd))"

BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch is $BRANCH"

if [ "xHEAD" == "x$BRANCH" ]; then
  git checkout master
fi

BRANCH=$(git rev-parse --abbrev-ref HEAD)
echo "Current branch is now $BRANCH"

echo "Fetching origin"
git fetch origin > /dev/null 2>&1

if $(git fetch upstream > /dev/null 2>&1); then
  echo "Upstream branch detected"
  git rebase origin/$BRANCH
  git push origin $BRANCH

  git merge upstream/master
  git push origin $BRANCH
else
  echo "No upstream branch"
  git rebase origin/$BRANCH
fi
EOF

git submodule foreach "bash $TMPFILE"


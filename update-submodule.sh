#! /bin/bash -

# Checking out master in all submodule
git submodule foreach git checkout master

# Fetching and upstream if it exists
git submodule foreach git fetch
git submodule foreach "git fetch upstream || true"

# Merging from upstream/master if it exists and then pushing to
# origin. Otherwise rebasing from origin/master
git submodule foreach "git merge upstream/master && git push origin || git rebase origin/master"

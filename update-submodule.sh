#! /bin/bash -

# Updates all submodules. Submodules are mainly clones of github
# repos. Some of them are clones of repos i forked in github to be
# able to modify them easily. Therefore for each submodule i have a
# origin repo and a upstream repo for those that are clones of forks
# in github.

# Checking out master in all submodule
git submodule foreach git checkout master

# Fetching all remote repos and upstream if it exists
git submodule foreach git fetch
git submodule foreach "git fetch upstream || true"

# Merging from upstream/master if it exists and then pushing to
# origin. Otherwise rebasing from origin/master
git submodule foreach "git merge upstream/master && git push origin || git rebase origin/master"

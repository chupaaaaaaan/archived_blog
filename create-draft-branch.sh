#!/bin/bash

set -eu

git stash

git checkout source

UNTRACKED_LIST=$(git status -s | grep "^?? posts/" | cut -d' ' -f2)

for file in $UNTRACKED_LIST
do
    git checkout -b ${file/posts/draft}
    git add ${file}
    git commit -m "add draft"
    git push -u origin ${file/posts/draft}
    git checkout source
done

git stash pop || :

git branch -vva

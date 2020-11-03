#!/bin/bash

set -eu

TARGET_BRANCH="$(git branch | grep "draft/" | cut -c3- | peco)"

git checkout $TARGET_BRANCH

git add $(git status -s | peco | cut -c4-)

[ -z "$(git status -s | cut -d' ' -f2 | grep '^posts/')" ] && git commit -m "add draft"

git checkout source

git pull

git checkout $TARGET_BRANCH

git merge source

git checkout source

git merge $TARGET_BRANCH

git push origin :$TARGET_BRANCH

git branch -d $TARGET_BRANCH

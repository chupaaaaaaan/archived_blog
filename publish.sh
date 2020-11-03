#!/bin/bash

set -eu

cd $(dirname $0)

git checkout source

git stash

git pull

./build.sh no-watch

git checkout master

git pull

./deploy.sh

git add . && git commit -m "publish" && git push

git checkout source

git stash pop

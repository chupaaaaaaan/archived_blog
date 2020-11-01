#!/bin/bash

set -eu

cd $(dirname $0)

git checkout source

git pull

./build.sh no-watch

git add . && git commit -m "publish" && git push

git checkout master

git pull

./deploy.sh

git add . && git commit -m "publish" && git push

git checkout source

#!/bin/bash

set -eu

cd $(dirname $0)

git checkout source

./build.sh

git add . && git commit -m "publish" && git push

git checkout master

./deploy.sh

git add . && git commit -m "publish" && git push

git checkout source

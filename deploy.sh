#!/bin/bash

cd $(dirname $0)

rsync -a --delete-excluded \
      --filter='P _site/' \
      --filter='P _cache/' \
      --filter='P .git/' \
      --filter='P .stack-work' \
      --filter='P .gitignore' \
      _site/ .

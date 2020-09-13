#!/bin/bash

cd $(dirname $0)

stack build --fast

stack exec site rebuild

[ "$1" != "no-watch" ] && stack exec site watch

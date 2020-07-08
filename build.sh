#!/bin/bash

cd $(dirname $0)

stack build --fast

stack exec site rebuild

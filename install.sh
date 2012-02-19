#!/bin/sh
set -e -x

/usr/bin/ghc -O -Wall git-prompt.hs -o /usr/local/bin/git-prompt


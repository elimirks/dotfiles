#!/bin/bash

# Taken from https://ddrscott.github.io/blog/2017/fzf-dictionary/

# Default `fold` to screen width and break at spaces
function fold {
    /usr/bin/fold $*
    # if [ $# -eq 0 ]; then
    #     #/usr/bin/fold -w $COLUMNS -s
    # else
    #     #/usr/bin/fold $*
    # fi
}

# Use `fzf` against system dictionary
function spell {
    cat /usr/share/dict/words | fzf --preview 'wn {} -over | fold' --preview-window=up:60%
}

# Lookup definition of word using `wn $1 -over`.
# If $1 is not provided, we'll use the `spell` command to pick a word.
#
# Requires:
#   brew install wordnet
#   brew install fzf

if [ $# -eq 0 ]; then
    wn `spell` -over | fold
else
    wn $1 -over | fold
fi

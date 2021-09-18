#!/bin/sh

FILE="$1"
CMD=${2/"{}"/$FILE}

$CMD
while inotifywait -e close_write $FILE; do $CMD; done

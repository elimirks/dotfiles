#!/bin/sh
# Used to strip coloured output from the shell
exec sed 's/\x1b\[[0-9;]*m//g'

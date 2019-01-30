#!/bin/sh

# We expect the dotfiles directory as the first argument
DOTFILES="$1/"

cd $DOTFILES/packages/PinkyCtrls/

# If we're on the US layout, switch to PL DVP, and vice versa
if [ "$(setxkbmap -query | \grep layout | sed 's/layout:[ ]*//')" = "us" ]; then
    ./s2cctl restart
else
    ./s2cctl stop
    setxkbmap us
fi

# Regenerate i3config
$DOTFILES/i3/generateConfig.sh
i3-msg restart

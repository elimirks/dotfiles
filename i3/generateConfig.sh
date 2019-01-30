#!/bin/sh

# This script generates Eli's i3 config!

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
CONF=$DIR/config

echo -e "# WARNING: This was auto generated via generateConfig.sh!\n" > $CONF

cat $DIR/config.d/vars.conf >> $CONF
echo >> $CONF
cat $DIR/config.d/main.conf >> $CONF
echo >> $CONF

if [ "$(setxkbmap -query | \grep layout | sed 's/layout:[ ]*//')" = "us" ]; then
    cat $DIR/config.d/qwerty.conf >> $CONF
    echo >> $CONF
else
    # Polish Programmers Dvorak
    cat $DIR/config.d/pldvp.conf >> $CONF
    echo >> $CONF
fi

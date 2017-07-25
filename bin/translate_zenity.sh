#!/bin/sh

TRANSLATE_SHELL="$DOTFILES_DIR/packages/translate-shell/translate"
TRANSLATION="de:en"

# Allow the first arg to change the translation language
if [ $# -ge 1 ]
then
	TRANSLATION=$1
fi

# Previous message
prevmes=
# Previous translation
prevtra=

getmsg() {
	zenity --entry --title="Translating $TRANSLATION" --text="$prevmes\n$prevtra"
}

msg=`getmsg`
while [ ! -z "$msg" ]; do
	prevmes=$msg
	prevtra=$($TRANSLATE_SHELL $TRANSLATION --brief "$msg")
	msg=`getmsg`
done


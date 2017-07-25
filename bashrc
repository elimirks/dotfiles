source $HOME/.profile

# Colors!
txtgrn='\e[0;32m' # Green
txtblu='\e[0;34m' # Blue
bldgrn='\e[1;32m' # Green
bldblu='\e[1;34m' # Blue
txtrst='\e[0m'    # Text Reset

#Console display
PS1="${bldgrn}\u ${txtgrn}\h ${txtblu}\t ${bldblu}\w ${txtrst}\n$ "

# Bash completion.. of course!
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

set -o vi


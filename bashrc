# Colors!
txtgrn='\e[0;32m' # Green
txtblu='\e[0;34m' # Blue
bldgrn='\e[1;32m' # Green
bldblu='\e[1;34m' # Blue
txtrst='\e[0m'    # Text Reset

# Set some aliases that spice up programs
alias ls='ls --color=auto -F'
alias grep='grep --color=auto'

#Console display
PS1="${bldgrn}\u ${txtgrn}\h ${txtblu}\t ${bldblu}\w ${txtrst}\n$ "

# Bash completion O.O
if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi

set -o vi


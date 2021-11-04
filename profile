# An RC file common to any standard shell

# Environment
export PATH="$PATH:$HOME/bin:$DOTFILES_DIR/bin"
export EDITOR='vim'

# This will break on unsupported systems... but who the fuck doesn't have 256?
export TERM="xterm-256color"

# Crazy, short, and cool util additions

if [ `uname` = 'Linux' ]; then
    alias ls='ls --color=always'
    alias o='xdg-open'
else  # Proboblay a Mac
    alias ls='ls -G'
    alias o='open'
fi

alias ll='ls -l'
alias la='ls -la'

alias ..="cd .. && ls"
alias ...="cd ../.. && ls"
alias ....="cd ../../.. && ls"
alias .....="cd ../../../.. && ls"

# Nice colors
alias grep='grep -n --color=always'

if [ `command -v grc` ]; then
	# grc colors.. this is a very neat program
	alias ping='grc ping'
	alias traceroute='grc traceroute'
	alias gcc='grc gcc'
	alias make='grc make'
	alias netstat='grc netstat'
	alias diff='grc diff'
	alias last='grc last'
	alias ldap='grc ldap'
	alias cvs='grc cvs'
	alias configure='grc ./configure'
fi

if [ `command -v emacs` ]; then
    alias ed="emacsclient -nw"
else
    alias ed="vim"
fi

alias trim="sed 's/^ *//;s/ *$//'"
alias wsdelimit="sed -r 's/\s+/ /g'"
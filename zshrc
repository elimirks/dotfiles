# Fix for editing remote files with Emacs Tramp
[[ $TERM == "dumb" ]] && unsetopt zle && PS1='$ ' && return

source $HOME/.profile

# COLURZ!!!

txtblk='\e[0;30m' # Black - Regular
txtred='\e[0;31m' # Red
txtgrn='\e[0;32m' # Green
txtylw='\e[0;33m' # Yellow
txtblu='\e[0;34m' # Blue
txtpur='\e[0;35m' # Purple
txtcyn='\e[0;36m' # Cyan
txtwht='\e[0;37m' # White
bldblk='\e[1;30m' # Black - Bold
bldred='\e[1;31m' # Red
bldgrn='\e[1;32m' # Green
bldylw='\e[1;33m' # Yellow
bldblu='\e[1;34m' # Blue
bldpur='\e[1;35m' # Purple
bldcyn='\e[1;36m' # Cyan
bldwht='\e[1;37m' # White
unkblk='\e[4;30m' # Black - Underline
undred='\e[4;31m' # Red
undgrn='\e[4;32m' # Green
undylw='\e[4;33m' # Yellow
undblu='\e[4;34m' # Blue
undpur='\e[4;35m' # Purple
undcyn='\e[4;36m' # Cyan
undwht='\e[4;37m' # White
bakblk='\e[40m'   # Black - Background
bakred='\e[41m'   # Red
badgrn='\e[42m'   # Green
bakylw='\e[43m'   # Yellow
bakblu='\e[44m'   # Blue
bakpur='\e[45m'   # Purple
bakcyn='\e[46m'   # Cyan
bakwht='\e[47m'   # White
txtrst='\e[0m'    # Text Reset

# Set up the prompt

setopt histignorealldups sharehistory

# What does this do?
local -A pc

# Just some variable to keep the PROMPT evironment var clean
pc['user']=${1:-'red'}
pc['host']=${1:-'green'}
pc['time']=${2:-'white'}
pc['pwd']=${3:-'blue'}
pc['prom']=${4:-'magenta'}
pc['com']=${4:-'white'}
pc['smile']=${1:-'green'}
pc['frown']=${1:-'red'}

SMILE="if [ \$? = 0 ]; then echo \"%F{$pc['smile']}:)\"; else; echo \"%F{$pc['frown']}:(\"; fi"
PROMPT_STATUS="\`$SMILE\`"
PROMPT_USER_HOST_TIME="%F{$pc['user']}%n %F{$pc['host']}%m%F{$pc['time']} %T%f"
PROMPT="$PROMPT_USER_HOST_TIME $PROMPT_STATUS %F{$pc['pwd']}%~
%F{$pc['prom']}%# %F{$pc['com']}"
setopt promptsubst

# Vim style
bindkey -v

# Use %_ to show why you are in a continued prompt
PROMPT2="%F{$pc['pwd']}%_> %F{$pc['com']}"

# Set the key mapping style to 'emacs' or 'vi'.
zstyle ':omz:editor' keymap 'vi'

# Nice auto correct prompt
setopt correct
autoload -U colors && colors
export SPROMPT="Correct $fg[red]%R$reset_color to $fg[green]%r?$reset_color (Yes, No, Abort, Edit) "

# Keep 1000 lines of history within the shell and save it to ~/.zsh_history:
HISTSIZE=1000
SAVEHIST=1000
HISTFILE=~/.zsh_history

# Use modern completion system
autoload -Uz compinit
compinit

zstyle ':completion:*' auto-description 'specify: %d'
zstyle ':completion:*' completer _expand _complete _correct _approximate
zstyle ':completion:*' format 'Completing %d'
zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
#`command -v dircolors` && eval "$(dircolors -b)"
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}
zstyle ':completion:*' list-colors ''
zstyle ':completion:*' list-prompt %SAt %p: Hit TAB for more, or the character to insert%s
zstyle ':completion:*' matcher-list '' 'm:{a-z}={A-Z}' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=* l:|=*'
zstyle ':completion:*' menu select=long
zstyle ':completion:*' select-prompt %SScrolling active: current selection at %p%s
zstyle ':completion:*' use-compctl false
zstyle ':completion:*' verbose true

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=0=01;31'
zstyle ':completion:*:kill:*' command 'ps -u $USER -o pid,%cpu,tty,cputime,cmd'

# Get ZSH to play nicely with Emacs multi-term
if [ -n "$INSIDE_EMACS" ]; then
  chpwd() { print -P "\033AnSiTc %d" }
  print -P "\033AnSiTu %n"
  print -P "\033AnSiTc %d"
fi

# Fancy prompt stuff

# Colorify man
function man() {
	env \
		LESS_TERMCAP_mb=$(printf "\e[1;31m") \
		LESS_TERMCAP_md=$(printf "\e[1;31m") \
		LESS_TERMCAP_me=$(printf "\e[0m") \
		LESS_TERMCAP_se=$(printf "\e[0m") \
		LESS_TERMCAP_so=$(printf "\e[1;44;33m") \
		LESS_TERMCAP_ue=$(printf "\e[0m") \
		LESS_TERMCAP_us=$(printf "\e[1;32m") \
			man "$@"
}

function cl() { cd $@ && ls }
function mkc() { mkdir $@ && cd $1 }

function preexec() {
	echo
}
function precmd() {
	echo -e "$txtpur"
	jobs
	echo -ne "$txtrst"
}

function source_if_exists() {
  if [ -e "$1" ]; then
    source "$1"
		return 0
  fi
	return 1
}

source_if_exists /usr/share/zsh/plugins/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh || \
    source_if_exists /usr/local/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

# Computer specific scripts
if [ -d $HOME/.zsh ]; then
	source $HOME/.zsh/*
fi

# Enable the home/end/delete keys
bindkey "\e[7~" beginning-of-line
bindkey "\e[8~" end-of-line
bindkey "\e[3~" delete-char

# VI MODE KEYBINDINGS (ins mode)

bindkey -M viins '^a'    beginning-of-line
bindkey -M viins '^e'    end-of-line
bindkey -M viins '^k'    kill-line
bindkey -M viins '^w'    backward-kill-word
bindkey -M viins '^u'    backward-kill-line

autoload -U edit-command-line
zle -N edit-command-line
bindkey -M viins '^xe'  edit-command-line
bindkey -M viins '^x^e'  edit-command-line
bindkey "^R" history-incremental-search-backward

# source_if_exists /usr/share/fzf/completion.zsh
# source_if_exists /usr/share/fzf/key-bindings.zsh
# source_if_exists /usr/share/fzf/key-bindings.zsh

# TODO: Try this out again
# source_if_exists ./zsh/zsh-github-copilot.plugin.zsh
# bindkey '^ge' zsh_gh_copilot_explain  # bind Alt+shift+\ to explain
# bindkey '^gs' zsh_gh_copilot_suggest  # bind Alt+\ to suggest

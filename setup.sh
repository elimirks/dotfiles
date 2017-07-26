#!/bin/bash

# Pull various dotfile submodules
git submodule update --init --recursive
cd packages/PinkyCtrls; make; cd -; echo

DEPENDENCIES="zsh bash emacs tmux vim git"
DEPENDENCIES+=" gawk" # For translate-shell

echo "Installing $DEPENDENCIES"
if uname -v | grep -qE "(Ubuntu|Debian)"; then
	sudo apt-get install $DEPENDENCIES
elif uname -v | grep -qE "Arch"; then
	sudo pacman -S $DEPENDENCIES
else
	echo "Unrecognized distro. Please install dependencies manually."
fi

# If the current shell is not set to zsh
if [[ "$(getent passwd $USER | cut -d: -f7)" = *"zsh"* ]]
then
	echo "zsh is already set as default shell... skipping"
else
	if command -v zsh >/dev/null 2>&1
	then
		read -p "Set ZSH as default shell? (y/n) " yesorno
		case "$yesorno" in
			y) 
				ZSH_PATH=`which zsh`
				echo "Setting $ZSH_PATH as the default shell"
				chsh $USER -s $ZSH_PATH ;;
			*) echo "zsh was not set to the default shell" ;;
		esac
	else
		echo "Warning: zsh is not installed - skipping chsh"
	fi
fi

# Actual dotfile linking

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo -e Linking dotfiles to \"$DIR\" "\n"

read -p "Set up shells? (y/n) " yesorno
if [ "x$yesorno" = "xy" ]; then
	echo "Setting up $HOME/.profile"
	sed -i -- "/^.*#ELIDOTFILES$/d" $HOME/.profile
	SOURCE_CMD="export DOTFILES_DIR=$DIR; source $DIR/profile"
	echo "$SOURCE_CMD #ELIDOTFILES" >> "$HOME/.profile"

	echo "Setting up $HOME/.bashrc"
	sed -i -- "/^.*#ELIDOTFILES$/d" $HOME/.bashrc
	SOURCE_CMD="source $DIR/bashrc"
	echo "$SOURCE_CMD #ELIDOTFILES" >> "$HOME/.bashrc"

	echo "Setting up $HOME/.zshrc"
	sed -i -- "/^.*#ELIDOTFILES$/d" $HOME/.zshrc
	SOURCE_CMD="source $DIR/zshrc"
	echo "$SOURCE_CMD #ELIDOTFILES" >> "$HOME/.zshrc"
fi

if [ -e "$HOME/.emacs.d" ]
then
	echo $HOME/.emacs.d already exists... skipping
else
	read -p "Set up $HOME/.emacs.d? (y/n) " yesorno
	case "$yesorno" in
		y) ln -ds "$DIR/emacs.d" "$HOME/.emacs.d" ;;
		*) echo "Skipping $HOME/.emacs.d" ;;
	esac
fi

if [ -e "$HOME/.tmux.conf" ]
then
	echo $HOME/.tmux.conf already exists... skipping
else
	read -p "Set up $HOME/.tmux.conf? (y/n) " yesorno
	case "$yesorno" in
		y) ln -s "$DIR/tmux.conf" "$HOME/.tmux.conf" ;;
		*) echo "Skipping $HOME/.tmux.conf" ;;
	esac
fi

if [ -e "$HOME/.vimrc" ]
then
	echo $HOME/.vimrc already exists... skipping
else
	read -p "Set up $HOME/.vimrc? (y/n) " yesorno
	case "$yesorno" in
		y) echo "source $DIR/vimrc" > "$HOME/.vimrc" ;;
		*) echo "Skipping $HOME/.vimrc" ;;
	esac
fi

if [ -e "$HOME/.config/git/config" ]
then
	echo $HOME/.config/git/config already exists... skipping
else
	read -p "Set up $HOME/.config/git/config? (y/n) " yesorno
	case "$yesorno" in
		y) mkdir -p "$HOME/.config/git/" && \
		   ln -s "$DIR/gitconfig" "$HOME/.config/git/config" ;;
		*) echo "Skipping $HOME/.config/git/config" ;;
	esac
fi


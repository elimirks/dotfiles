#!/bin/bash

# Pull various dotfile submodules
git submodule update --init --recursive

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
echo -e Linking dotfiles at \"$DIR\" "\n"

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

if [ -e "$HOME/.bashrc" ]
then
	echo $HOME/.bashrc already exists... skipping
else
	read -p "Set up $HOME/.bashrc? (y/n) " yesorno
	case "$yesorno" in
		y) echo "source $DIR/bashrc" > "$HOME/.bashrc" ;;
		*) echo "Skipping $HOME/.bashrc" ;;
	esac
fi

if [ -e "$HOME/.zshrc" ]
then
	echo $HOME/.zshrc already exists... skipping
else
	read -p "Set up $HOME/.zshrc? (y/n) " yesorno
	case "$yesorno" in
		y) echo "source $DIR/zshrc" > "$HOME/.zshrc" ;;
		*) echo "Skipping $HOME/.zshrc" ;;
	esac
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


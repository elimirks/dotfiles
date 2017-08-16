#!/bin/bash

# Set to 1 to set up GUI stuff (like i3)
HAS_GUI=1

DEPENDENCIES=
UBUNTU_DEPENDENCIES=
ARCH_DEPENDENCIES=

DEPENDENCIES+="zsh zsh-syntax-highlighting bash emacs tmux vim git"

if [ HAS_GUI -eq 1 ]; then
 	# For PinkCtrls
	UBUNTU_DEPENDENCIES+=" libxtst-dev"
	ARCH_DEPENDENCIES+=" libxtst"

	DEPENDENCIES+=" gawk" # For translate-shell
	DEPENDENCIES+=" i3-wm"
fi

echo "Installing $DEPENDENCIES"
if cat /etc/os-release | grep -qE "(Ubuntu|Debian)"; then
	sudo apt-get install $DEPENDENCIES $UBUNTU_DEPENDENCIES
elif cat /etc/os-release | grep -qE "Arch"; then
	sudo pacman -S $DEPENDENCIES $ARCH_DEPENDENCIES
else
	echo "Unrecognized distro. Please install dependencies manually."
fi

echo "Pulling and building git based dependencies"
git submodule update --init --recursive

if [ HAS_GUI -eq 1 ]; then
	cd packages/PinkyCtrls; make; cd -; echo
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


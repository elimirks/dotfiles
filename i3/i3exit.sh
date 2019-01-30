#!/bin/sh
lock() {
    $DOTFILES_DIR/packages/i3lock-fancy/lock
}

case "$1" in
    lock)
        lock
        ;;
    logout)
        i3-msg exit
        ;;
    suspend)
				lock&
				sleep 1
				systemctl suspend
				sleep 10
				xmodmap /home/elijah/.Xmodmap
        ;;
    hibernate)
        lock && systemctl hibernate
        ;;
    reboot)
        systemctl reboot
        ;;
    shutdown)
        systemctl poweroff
        ;;
    *)
        echo "Usage: $0 {lock|logout|suspend|hibernate|reboot|shutdown}"
        exit 2
esac

exit 0


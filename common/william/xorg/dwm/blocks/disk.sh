case $BUTTON in
        1)
		dunstify "Disks" "$(df -h | awk 'BEGIN {print "Used % Filesystem"} /^\/dev/ {print $3 "/" $2, $5, $NF }' | column -t)" &
                ;;
        2)
		{
			mountPoint="/run/media/$USER/$(lsblk | grep /run/media/$USER/ | awk '{split($0, sp, "/"); print sp[length(sp)]}' | dmenu -p 'Umount :')"
			[ -n "$mountPoint" ] && udiskie-umount "$mountPoint"
		} > /dev/null 2>&1 &
                ;;
        3)
		eject > /dev/null 2>&1 &
                ;;
	4)
		alacritty -e "vifmrun" &
		;;
	5)
		alacritty -e "vifmrun" &
		;;
esac
disown -a

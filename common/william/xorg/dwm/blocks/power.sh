function rebootto {
			entry="$(printf "%d" "$(efibootmgr | awk -F "*" '/^Boot[0-9]{4}/ {sub(/^Boot[0]*/, "", $1);print}' | dmenu -i -p "Reboot :")")"
			[ "$entry" -eq 0 ] && exit
			sudo -A efibootmgr -n $entry	
			[ $? -eq 0 ] && reboot
}

case $BUTTON in
        1)
		{
			action="$(echo -e "Shutdown\nReboot\nReboot to\nSleep\nLock" | ~/.config/dmenu/dmenuw.sh)"
			case $action in
				"Shutdown")
					poweroff
					;;
				"Reboot")
					reboot
					;;
				"Sleep")
					systemctl suspend
					;;
				"Lock")
					bash -c "sleep 1; light-locker-command -l"
					;;
				"Reboot to")
					rebootto
					;;
			esac
		} > /dev/null 2>&1 &
                ;;
        2)
		{
			sleep 1; light-locker-command -a
		} > /dev/null 2>&1 &
                ;;
        3)
		rebootto > /dev/null 2>&1 &
                ;;
	4)
		;;
	5)
		;;
esac
disown -a

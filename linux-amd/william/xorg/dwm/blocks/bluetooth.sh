case $BUTTON in
        1)
		{
			dev="$(bluetoothctl devices | awk '{sub(/^Device .{17} /, "", $0); print}' | dmenu -p "Connect :")"
			if [ -n "$dev" ]; then
				bluetoothctl power on
				notify-send "Bluetooth" "$(bluetoothctl connect "$(bluetoothctl devices | grep "$dev" | cut -d " " -f 2)")" 
			fi
		} > /dev/null 2>&1 &
                ;;
        2)
		notify-send "Connected bluetooth devices" "$(bluetoothctl info | awk '/Name/ {sub(/^.*: /, "", $0); print}')"
                ;;
        3)
		alacritty -e "bluetoothctl" &
                ;;
	4)
		bluetoothctl power on > /dev/null 2>&1 &
		;;
	5)
		bluetoothctl power off > /dev/null 2>&1 &
		;;
esac
disown -a

if [ "$(bluetoothctl show | awk '/Powered/ {printf $2}')" = "no" ]; then
	echo "off"
	exit
fi

bluetoothctl info | grep Missing > /dev/null 2>&1

if [ $? -eq 0 ]; then
	echo "none"
	exit
fi

bluetoothctl info | awk '/Alias/ {sub(/^.*: /, "", $0); print; exit}'

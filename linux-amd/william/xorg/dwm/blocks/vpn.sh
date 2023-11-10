status="$(expressvpn status)"

case $BUTTON in
        1)
		if [[ "$status" == *"Not connected"* ]]; then
			expressvpn connect > /dev/null 2>&1 &
		else
			status="Not connected"
			expressvpn disconnect > /dev/null 2>&1 &
		fi
                ;;
        2)
		{
			location="$(expressvpn ls all | awk '(NR == 2) {iLoc = index($0, $3); lenLoc = length($3)} (NR > 2) {f3 = substr($0, iLoc, lenLoc); sub(/ *$/, "", f3); print(f3)}' | dmenu -i)"
			if [ -n "$location" ]; then
				expressvpn disconnect
				sleep 3
				expressvpn connect "$location"
			fi
		} > /dev/null 2>&1 &
                ;;
        3)
		{
			expressvpn disconnect
			sleep 3
			expressvpn connect smart
		} > /dev/null 2>&1 &
                ;;
	4)
		;;
	5)
		;;
esac
disown -a

if [[ "$status" == *"Not connected"* ]]; then
	echo "x"
elif [[ "$status" == *"Connecting"* ]]; then
	echo "c"
else
	echo "$status" | awk '/Connected to / {sub(/.*Connected to /, "", $0); split($0,a," - "); if(length(a) == 2) {print substr(a[1],1,3) "-" substr(a[2],1,3)} else {print substr($0,1,7)}}'
fi

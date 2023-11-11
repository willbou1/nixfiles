case $BUTTON in
        1)
		notify-send "Running players" "$(~/.config/dwm/statscripts/mprisinfo.sh)" &
                ;;
        2)
		playerctl -p spotify play-pause &
                ;;
        3)
		playerctl -a pause &
                ;;
	4)
		{
			playerctld unshift
			curr="$(playerctl metadata | awk '{print $1; exit}')"
			notify-send "Selected player" "$(playerctl -l | awk -v curr=$curr '{sub(/\..*$/, "", $0); if ($0 == curr) {print "\\-> " $0} else {print "   " $0}}')"
		} > /dev/null 2>&1 &
		;;
	5)
		{
			playerctld unshift
			curr="$(playerctl metadata | awk '{print $1; exit}')"
			notify-send "Selected player" "$(playerctl -l | awk -v curr=$curr '{sub(/\..*$/, "", $0); if ($0 == curr) {print "\\-> " $0} else {print "   " $0}}')"
		} > /dev/null 2>&1 &
		playerctld shift &
		;;
esac
disown -a

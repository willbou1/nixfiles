brightness="$(xrandr --verbose --current | awk '/Brightness/ {print $2; exit}')"
gamma="$(cat ~/.config/dwmblocks/gamma)"

gamma1="1:1:1"
gamma2="1:1:0.8"
gamma3="1:1:0.6"

if [[ -z "$gamma" ]]; then
    gamma="$gamma1"
fi

case $BUTTON in
	1)
		case $gamma in
			"$gamma1")
				gamma="$gamma2"
				;;
			"$gamma2")
				gamma="$gamma3"
				;;
			"$gamma3")
				gamma="$gamma1"
				;;
		esac
		xrandr $(xrandr --listmonitors  | awk -v bri="$brightness" -v gam="$gamma" '!(/Monitors/) {printf "--output %s --gamma %s --brightness %f ", $NF, gam, bri}') &
		;;
	2)
		brightness="0.7"
		gamma="$gamma1"
		xrandr $(xrandr --listmonitors  | awk -v bri="$brightness" -v gam="$gamma" '!(/Monitors/) {printf "--output %s --gamma %s --brightness %f ", $NF, gam, bri}') &
		;;
        4)
		brightness="$(echo $brightness | awk '{if($1 < 1.0) {print $1 + 0.05} else {print $1} exit}')"
		xrandr $(xrandr --listmonitors  | awk -v bri="$brightness" -v gam="$gamma" '!(/Monitors/) {printf "--output %s --gamma %s --brightness %f ", $NF, gam, bri}') &
                ;;
        5)
		brightness="$(echo $brightness | awk '{if($1 > 0.2) {print $1 - 0.05} else {print $1} exit}')"
		xrandr $(xrandr --listmonitors  | awk -v bri="$brightness" -v gam="$gamma" '!(/Monitors/) {printf "--output %s --gamma %s --brightness %f ", $NF, gam, bri}') &
                ;;
        3)
		flameshot gui &
                ;;
esac
disown -a

echo $gamma > ~/.config/dwmblocks/gamma
echo $brightness > ~/.config/dwmblocks/brightness
echo "$brightness" | awk '{print ($0 * 100) "%"}'

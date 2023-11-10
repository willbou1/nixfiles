players="$(playerctl -l)"

for p in $players; do
	name="$(echo $p | cut -d '.' -f 1)"
	echo "<b>$name</b>"
	case $name in
		"chromium")
			echo -e "Artist : $(playerctl -p $p metadata xesam:artist)\nTitle : $(playerctl -p $p metadata xesam:title)" | column -t -s ":"
			;;
		"mpv")
			echo -e "File : $(playerctl -p $p metadata xesam:url | sed "s/^file:\/\///")\nTitle : $(playerctl -p $p metadata xesam:title)" | column -t -s ":"
			;;
		"spotify")
			echo -e "Artist : $(playerctl -p $p metadata xesam:artist)\nTitle : $(playerctl -p $p metadata xesam:title)" | column -t -s ":"
			;;
	esac
	echo ""
done

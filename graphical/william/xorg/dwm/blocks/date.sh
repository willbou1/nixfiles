case $BUTTON in
	1)
		$terminal -e "neomutt" &
		;;
	2)
		$terminal -e "calcurse" &
		;;
	3)
		notify-send "$(date)" &
		;;
	4)
		echo "12" > ~/.config/dwmblocks/dateformat
		;;
	5)
		echo "24" > ~/.config/dwmblocks/dateformat
		;;
esac
disown -a

dateformat="$(cat ~/.config/dwmblocks/dateformat)"

if [[ -z "$dateformat" ]]; then
    dateformat="12";
fi

echo "$dateformat" > ~/.config/dwmblocks//dateformat

case "$dateformat" in
	"24")
		date "+%I:%M"
		;;
	"12")
		date "+%H:%M"
		;;
esac

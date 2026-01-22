case $BUTTON in
	1)
		$terminal -e btop &
		;;
	2)
		{
			pid="$(ps -U william -o pid,cmd | awk -f "$HOME/.config/dwm/statscripts/pu.awk"  | dmenu -p "Kill :" | cut -d " " -f 1)"
			[ -n "$pid" ] && kill -9 $pid
		} > /dev/null 2>&1 &
		;;
	3)
		{
			pid="$(ps -U root -o pid,cmd | awk -f "$HOME/.config/dwm/statscripts/pu.awk"  | dmenu -p "Kill :" | cut -d " " -f 1)"
			[ -n "$pid" ] && sudo -A kill -9 $pid
		} > /dev/null 2>&1 &
		;;
	4)
		;;
	5)
		;;
esac

disown -a 

mpstat | awk '/all/ {printf "%d%\n", (100 - $NF); exit}'

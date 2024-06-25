stats="$(nmcli con show --active)"

conName="$(echo "$stats" | awk '!/NAME/ && !/bridge/ {sub(/_*[0-9].*$/, "", $1); print substr(tolower($1),1,5); exit}')"

conType="$(echo "$stats" | awk '!/NAME/ && !/bridge/ {print $(NF - 1); exit}')"

conDev="$(echo "$stats" | awk '!/NAME/ && !/bridge/ {print $NF; exit}')"

case $BUTTON in
        1)
                alacritty -e nmtui &
                ;;
        2)
		notify-send  "Network" "$(nmcli dev show $conDev | grep -E "(ADDRESS)|(GATEWAY)|(DNS)|(DEVICE)" | sed "s/:\s\+/\t/g")" &
                ;;
        3)
		{
			stats="$(~/.config/dwm/statscripts/net_stats.sh $conDev | column -t -R 2)"
			notify-send "Etc" "Public IP : $(curl -s ipinfo.io/ip)<br><b>Statistics</b><br>$stats"
		} > /dev/null 2>&1 &
                ;;
	4)
		;;
	5)
		;;
esac
disown -a

nmcli general | grep disconnected > /dev/null 2>&1

if [ $? -eq 0 ]; then
	echo "Disconnected"
	exit
fi

echo -n "$conName"

[ "$conType" == "wifi" ] && iwconfig wlp5s0 | awk '/Link Quality/ {sub(/^.*=/, "", $2); split($2, sp, "/"); printf " %2d\n", sp[1] / sp[2] * 100; exit}' && exit
echo ""

case $1 in
	"--ip")
		nmcli dev show wlp6s0 | awk '/IP4.ADDRESS/ {print $2}'
		;;
	"--strength")
		nmcli -f IN-USE,SIGNAL device wifi | awk '/\*/ {print $2}'
		;;
	"--vpn")
        systemctl list-units --type=service --state=running | awk '/openvpn/ { rc = 1; split($1, DASH, "-"); split(DASH[2], LOC, "."); print LOC[1]; next } END { if (rc != 1) { print "Disconnected"} }'
		;;
	"--name")
		nmcli dev show wlp6s0 | awk '/GENERAL.CONNECTION/ {print substr($2, 0, 6)}'
		;;
	"--bandwidth")
		f="$(awk '/wlp5s0/ {print $2}'  /proc/net/dev)"
		sleep 1
		s="$(awk '/wlp5s0/ {print $2}'  /proc/net/dev)"
		b="$((s - f))"
		echo "$b" | awk '{print $0 / 1024 / 1024}'
		;;
	*)
		true
		;;
esac


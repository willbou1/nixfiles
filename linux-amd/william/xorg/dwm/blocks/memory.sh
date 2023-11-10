function clear_memory {
	sync
	echo 3 > /proc/sys/vm/drop_caches
	swapoff -a && swapon -a
}

case $BUTTON in
	1)
		notify-send "Memory" "$(ps -e -o pid,cmd,vsz --sort vsz | tail -n 10 | awk '{print $1 "\t" $2}')" &
		;;
	2)
		notify-send "Swap memory usage" "$(free -h | awk '/Swap/ {print $3, "/", $2}')" &
		;;
	3)
		sudo -A clear_memory > /dev/null 2>&1 &
		;;
	4)
		;;
	5)
		;;
esac
disown -a

free -h | awk '/^Mem/ { sub(/i/, "", $3);print $3}'

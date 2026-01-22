case $1 in
	"-1")
		ddcutil -t -b 5 getvcp 10 | tail -n 1 | cut -d ' ' -f 4
		;;
	"-2")
		ddcutil -t -b 7 getvcp 10 | tail -n 1| cut -d ' ' -f 4
		;;
	*)
		true
		;;
esac


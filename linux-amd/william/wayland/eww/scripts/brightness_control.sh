case $1 in
	"-1")
		ddcutil -b 5 setvcp 10 "$2"
		;;
	"-2")
		ddcutil -b 6 setvcp 10 "$2"
		;;
	"--synced")
		ddcutil -b 5 setvcp 10 "$2"
        ddcutil -b 6 setvcp 10 "$2"
		;;
	"--toggle")
		s="$(eww get brightness_reveal)"
		[[ "$s" == *true* ]] && eww update brightness_reveal=false || eww update brightness_reveal=true
		b1="$(ddcutil -t -b 5 getvcp 10 | tail -1 | cut -d ' ' -f 4)"
		b2="$(ddcutil -t -b 6 getvcp 10 | tail -1 | cut -d ' ' -f 4)"
        if [ "$b1" -ge "$b2" ]; then
		    ddcutil -b 6 setvcp 10 "$b1"
            eww update b2="$b1"
        else
            ddcutil -b 5 setvcp 10 "$b2"
            eww update b1="$b2"
        fi
		;;
	*)
		true
		;;
esac


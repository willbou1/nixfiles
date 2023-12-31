case $1 in
	"--canada")
		case $2 in
		    "--hour")
			date "+%I"
			;;
		    "--minutes")
			date "+%M"
			;;
		    "--type")
			date "+%p"
			;;
		    "--date")
			date "+ %a, %b %d"
			;;
		    *)
			true
			;;
		esac
		;;
	"--sk")
		case $2 in
		    "--hour")
			TZ="Asia/Seoul" date "+%I"
			;;
		    "--minutes")
			TZ="Asia/Seoul" date "+%M"
			;;
		    "--type")
			TZ="Asia/Seoul" date "+%p"
			;;
		    "--date")
			TZ="Asia/Seoul" date "+ %a, %b %d"
			;;
		    *)
			true
			;;
		esac
		;;
esac

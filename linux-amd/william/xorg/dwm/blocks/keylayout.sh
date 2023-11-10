case $BUTTON in
        1)
                ibus engine "xkb:ca:multix:fra"
		;;
        2)
		ibus engine "xkb:ca:fr-dvorak:fra"
                ;;
        3)
		ibus restart &
                ;;
	4)
		{
			layout="$(ibus list-engine | awk -F " - " '!/^language:/ {if (length($2) > 40) {print substr($1,3) "~" substr($2,1,40) "..."} else {print substr($1,3) "~" $2}}' | column -t -s "~" | dmenu -i | cut -d " " -f 1)"
			[ -n "$layout" ] && ibus engine "$layout"
		}  > /dev/null 2>&1 &
		;;
	5)
		{
			layout="$(ibus list-engine | awk -F " - " '!/^language:/ {if (length($2) > 40) {print substr($1,3) "~" substr($2,1,40) "..."} else {print substr($1,3) "~" $2}}' | column -t -s "~" | dmenu -i | cut -d " " -f 1)"
			[ -n "$layout" ] && ibus engine "$layout"
		}  > /dev/null 2>&1 &
		;;
esac
disown -a

layout="$(ibus engine)"

case "$layout" in
	"xkb:ca:multix:fra")
		echo "qw"
		;;
	"xkb:ca:fr-dvorak:fra")
		echo "dv"
		;;
	"hangul")
		echo "ha"
		;;
	"mozc-jp")
		echo "jp"
		;;
	"libpinyin")
		echo "pi"
		;;
	"libbopomofo")
		echo "bo"
		;;
	*)
		echo "$layout" | awk -F ":" '{print substr($NF,1,2)}'
		;;
esac

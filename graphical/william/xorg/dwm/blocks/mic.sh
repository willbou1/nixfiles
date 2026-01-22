case $BUTTON in
    1)
        $terminal -t "ncpamixer" -e "ncpamixer" &
        ;;
    2)
        catia &
        ;;
    3)
        pactl set-source-mute @DEFAULT_SOURCE@ toggle
        ;;
    4)
        pactl set-source-volume @DEFAULT_SOURCE@ +5%
        pactl set-source-mute @DEFAULT_SOURCE@ 0
        ;;
    5)
        pactl set-source-volume @DEFAULT_SOURCE@ -5%
        ;;
esac
disown -a

pactl info | awk '/Default Source:/ {sub(/_.*/,"", $3);printf substr($3, 1, 1)}'
pactl get-source-volume @DEFAULT_SOURCE@ | awk '/Volume/ { print $5 }'

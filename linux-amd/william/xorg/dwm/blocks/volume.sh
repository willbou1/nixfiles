case $BUTTON in
    1)
        $terminal -t "ncpamixer" ncpamixer &
        ;;
    2)
        catia &
        ;;
    3)
        pactl set-sink-mute @DEFAULT_SINK@ toggle
        ;;
    4)
        pactl set-sink-volume @DEFAULT_SINK@ +5%
        pactl set-sink-mute @DEFAULT_SINK@ 0
        ;;
    5)
        pactl set-sink-volume @DEFAULT_SINK@ -5%
        ;;
esac
disown -a

pactl info | awk '/Default Sink:/ {sub(/_.*/,"", $3);printf("%s", substr($3, 1, 1))}'
pactl get-sink-volume @DEFAULT_SINK@ | awk '/front-left/ { print $5 }'

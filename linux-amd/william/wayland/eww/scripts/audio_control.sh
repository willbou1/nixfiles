bi="$(wpctl status | awk 'BEGIN {f = 0} /bluez5/ {bn = $3; f = 1} (f == 1 && match($0, bn ".*vol")) {gsub(/\./, "", $0); if ($2 == "*") {print $3} else {print $2}}')"

case $1 in
	"--toggle")
		s="$(eww get audio_sink_reveal)"
		[[ "$s" == *true* ]] && eww update audio_sink_reveal=false || eww update audio_sink_reveal=true
		;;
	"--source")
		case $2 in
			"--volume")
				eww update audio_source_volume="$3"
				wpctl set-volume @DEFAULT_AUDIO_SOURCE@ "$3%"
				;;
		esac
		;;
	"--sink")
		case $2 in
			"--volume")
				eww update audio_sink_volume="$3"
				wpctl set-volume @DEFAULT_AUDIO_SINK@ "$3%"
				;;
			"--monitor")
				wpctl set-default 52
				eww update audio_sink_choice_card_class="choice_icons"
				eww update audio_sink_choice_monitor_class="selected_choice_icons"
				eww update audio_sink_choice_bluetooth_class="choice_icons"
				v="$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{printf "%d", ($2 * 100)}')"
				eww update audio_sink_volume="$v"
				;;
			"--card")
				wpctl set-default 54
				eww update audio_sink_choice_card_class="selected_choice_icons"
				eww update audio_sink_choice_monitor_class="choice_icons"
				eww update audio_sink_choice_bluetooth_class="choice_icons"
				v="$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{printf "%d", ($2 * 100)}')"
				eww update audio_sink_volume="$v"
				;;
			"--bluetooth")
				wpctl set-default "$bi"
				eww update audio_sink_choice_card_class="choice_icons"
				eww update audio_sink_choice_monitor_class="choice_icons"
				eww update audio_sink_choice_bluetooth_class="selected_choice_icons"
				v="$(wpctl get-volume @DEFAULT_AUDIO_SINK@ | awk '{printf "%d", ($2 * 100)}')"
				eww update audio_sink_volume="$v"
				;;
			"--bluetooth")
				wpctl set-default 0
				;;
		esac
		;;
	*)
		true
		;;
esac


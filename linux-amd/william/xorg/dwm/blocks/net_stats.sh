start_tx="$(cat /sys/class/net/$1/statistics/tx_bytes)"
start_rx="$(cat /sys/class/net/$1/statistics/rx_bytes)"

sleep 1

end_tx="$(cat /sys/class/net/$1/statistics/tx_bytes)"
end_rx="$(cat /sys/class/net/$1/statistics/rx_bytes)"

echo "$start_tx $end_tx" | awk '{kb = ($2 - $1) * 1 / 1024; printf "Transmitting  "; if (kb > 1023) {print kb / 1024 " MB/s"} else {print kb " KB/s"}}'
echo "$start_rx $end_rx" | awk '{kb = ($2 - $1) * 1 / 1024; printf "Receiving     "; if (kb > 1023) {print kb / 1024 " MB/s"} else {print kb " KB/s"}}'

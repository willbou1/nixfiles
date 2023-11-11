ml_sensors="$(sensors)"

case $BUTTON in
	1)
		dunstify "CPU temps" "$(sensors k10temp-pci-00c3 | awk '/^T/')" &
		;;
	2)
		dunstify "GPU temps" "$(sensors amdgpu-pci-0d00 | awk '/°C/ && !/^ / {sub(/  \(.*$/, "", $0); print}')" &
		;;
	3)
		dunstify "NVMe temps" "$(sensors nvme-pci-0100 nvme-pci-0400 | awk '!/^Adapter/ && !/^ / {sub(/  \(.*$/, "", $0); print}')" &
		;;
	4)
		;;
	5)
		;;
esac

disown -a 

echo "$ml_sensors" | awk '/Tctl:/ {printf "%d°C\n", $2; exit}'

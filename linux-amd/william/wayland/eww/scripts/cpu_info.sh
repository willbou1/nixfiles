case $1 in
    "-1")
        mpstat -o JSON -P ALL 1 1 | jq -a '.sysstat.hosts[0].statistics[0][\"cpu-load\"] | map(100 - .idle | floor) | .[1:9]'
        ;;
    "-2")
        mpstat -o JSON -P ALL 1 1 | jq -a '.sysstat.hosts[0].statistics[0][\"cpu-load\"] | map(100 - .idle | floor) | .[9:17]'
        ;;
    "-3")
        mpstat -o JSON -P ALL 1 1 | jq -a '.sysstat.hosts[0].statistics[0][\"cpu-load\"] | map(100 - .idle | floor) | .[17:25]'
        ;;
    "--usage")
        mpstat -o JSON 1 1 | jq '100 - .sysstat.hosts[0].statistics[0][\"cpu-load\"][0].idle | floor'
        ;;
    *)
        true
        ;;
esac

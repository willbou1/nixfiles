case $1 in
    "--used")
        free -m | grep Mem | awk '{printf $3/100}'
        ;;
    "--all")
        free -m | grep Mem | awk '{printf $2/100}'
        ;;
    "--parsed")
        free -h | grep Mem | awk '{printf $3 "/" $2}'
        ;;
    *)
        true
        ;;
esac

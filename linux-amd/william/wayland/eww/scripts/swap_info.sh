case $1 in
    "--used")
        free -m | grep Swap | awk '{printf $3/100}'
        ;;
    "--all")
        free -m | grep Swap | awk '{printf $2/100}'
        ;;
    "--parsed")
        free -h | grep Swap | awk '{printf $3 "/" $2}'
        ;;
    *)
        true
        ;;
esac

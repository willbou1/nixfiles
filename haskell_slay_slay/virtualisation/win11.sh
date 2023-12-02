if [[ $1 == "win11" ]]; then
  if [[ $2 == "prepare" ]]; then
        systemctl set-property --runtime -- user.slice AllowedCPUs=6-11,16-19
        systemctl set-property --runtime -- system.slice AllowedCPUs=6-11,16-19
        systemctl set-property --runtime -- init.scope AllowedCPUs=6-11,16-19
  fi
  if [[ $2 == "release" ]]; then
    systemctl set-property --runtime -- user.slice AllowedCPUs=0-19
    systemctl set-property --runtime -- system.slice AllowedCPUs=0-19
    systemctl set-property --runtime -- init.scope AllowedCPUs=0-19
  fi
fi

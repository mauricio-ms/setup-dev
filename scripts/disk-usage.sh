df -h /home --output=avail | sed -n '2p' | awk '{printf "\uf0c7  %s", $1}'

IP="185.165.171.79"
SSH_COMMAND="ssh -o StrictHostKeyChecking=no"
eval $(ssh-agent)
sed -i "/^$IP/d" ~/.ssh/known_hosts
$SSH_COMMAND root@$IP "bash -c 'mount -t tmpfs tmpfs /mnt; pacman -S --noconfirm kexec-tools'"
rsync -aL -e "$SSH_COMMAND" result/ root@$IP:/mnt/
$SSH_COMMAND root@$IP /ram/kexec-installer
sed -i "/^$IP/d" ~/.ssh/known_hosts
sleep 30
$SSH_COMMAND root@$IP

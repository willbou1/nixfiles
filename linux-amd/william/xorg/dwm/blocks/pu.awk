!/PID/ {
	cmd = $0;
	sub(/^\s*[0-9]* /, "", cmd);
	len = length(cmd);
	printf "%d  ", $1;
	if (len > 70) {
		print substr(cmd, 1, 70) "...";
	} else {
		print cmd;
	}
}

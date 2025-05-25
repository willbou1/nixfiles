# online
for input in nur nix-alien hosts zen-browser; do
  nix flake update "$input"
done


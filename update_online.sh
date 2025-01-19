# online
for input in nur nix-alien hosts; do
  nix flake update "$input"
done


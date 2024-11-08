import os
import subprocess as sp

os.chdir("/etc/nixos/devel");

class Repo:
    def __init__(self, name, upstream_url, branch_to_sync):
        self.name = name
        self.upstream_url = upstream_url
        self.branches_to_sync = branches_to_sync
    def sync(self):
        os.chdir(f"/etc/nixos/devel/{self.name}");
        sp.run(['git', 'remote', "add", "upstream", upstream_url], capture_output=True, text=True)
        sp.run(['git', 'checkout', branch_to_sync], capture_output=True, text=True)
        sp.run(['git', 'pull', "upstream", branch_to_sync], capture_output=True, text=True)
        sp.run(['git', 'push'], capture_output=True, text=True)

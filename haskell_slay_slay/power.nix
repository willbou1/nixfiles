{
  powerManagement.enable = true;
  services = {
    logind = {
      settings.Login = {
        IdleActionSec = 1200;
        IdleAction = "ignore";
        HandlePowerKey = "suspend";
      };
    };
    #thermald.enable = true;
    tlp = {
      enable = true;
      settings = {
        DISK_IDLE_SECS_ON_AC = 2;
        DISK_IDLE_SECS_ON_BAT = 2;

        CPU_SCALING_GOVERNOR_ON_AC = "powersave";
        CPU_SCALING_GOVERNOR_ON_BAT = "powersave";

        CPU_ENERGY_PERF_POLICY_ON_BAT = "balance_power";
        CPU_ENERGY_PERF_POLICY_ON_AC = "balance_performance";

        CPU_MIN_PERF_ON_AC = 0;
        CPU_MAX_PERF_ON_AC = 80;
        CPU_MIN_PERF_ON_BAT = 0;
        CPU_MAX_PERF_ON_BAT = 30;

        CPU_BOOST_ON_AC = 1;
        CPU_BOOST_ON_BAT = 0;

        SCHED_POWERSAVE_ON_AC = 1;
        SCHED_POWERSAVE_ON_BAT = 1;

        ENERGY_PERF_POLICY_ON_AC = "balance-performance";
        ENERGY_PERF_POLICY_ON_BAT = "power";
      };
    };
  };
}

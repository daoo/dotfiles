#include "settings.h"
#include "power.h"

void settings_check() {
  // bus
  //for i in /sys/bus/*/devices/*/power/control; do opt $i auto; done

  // usb autosuspend
  //for i in /sys/bus/usb/devices/*/power/autosuspend; do opt $i 10; done

  // nmi_watchdog
  check("/proc/sys/kernel/nmi_watchdog");

  // cpu
  check("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor");
  check("/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor");
  check("/sys/devices/system/cpu/cpu2/cpufreq/scaling_governor");
  check("/sys/devices/system/cpu/cpu3/cpufreq/scaling_governor");

  // aspm
  check("/sys/module/pcie_aspm/parameters/policy");

  // kernel write mode
  check("/proc/sys/vm/laptop_mode");
  check("/proc/sys/vm/dirty_ratio");
  check("/proc/sys/vm/dirty_background_ratio");
  check("/proc/sys/vm/dirty_expire_centisecs");
  check("/proc/sys/vm/dirty_writeback_centisecs");

  // disks
  //for dev in $(awk '/^\/dev\/sd/ {print $1}' /etc/mtab); do run hdparm -B 1 -S 12 $dev; done
  //for i in /sys/class/scsi_host/host*/link_power_management_policy; do opt $i min_power; done

  // sound card
  check("/sys/module/snd_hda_intel/parameters/power_save");
  check("/sys/module/snd_hda_intel/parameters/power_save_controller");
  //check("/sys/module/snd_ac97_codec/parameters/power_save");

  // wlan0/eth0 powersave
  //run("iwconfig wlan0 power on");

  // screen powersave
  //for i in /sys/class/backlight/acpi_video*/brightness; do opt $i 0; done
  //opt_("/sys/module/i915/parameters/i915_enable_rc6", "1");

  // disable webcam
  //unload("uvcvideo");

  // Disable bluetooth
  //unload("btusb");
  //unload("bluetooth");
}

void settings_save() {
  // bus
  //for i in /sys/bus/*/devices/*/power/control; do opt $i auto; done

  // usb autosuspend
  //for i in /sys/bus/usb/devices/*/power/autosuspend; do opt $i 10; done

  // nmi_watchdog
  opt_("/proc/sys/kernel/nmi_watchdog", "0");

  // cpu
  opt_("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor", "powersave");
  opt_("/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor", "powersave");
  opt_("/sys/devices/system/cpu/cpu2/cpufreq/scaling_governor", "powersave");
  opt_("/sys/devices/system/cpu/cpu3/cpufreq/scaling_governor", "powersave");

  // aspm
  opt_("/sys/module/pcie_aspm/parameters/policy", "powersave");

  // kernel write mode
  opt_("/proc/sys/vm/laptop_mode", "5");
  opt_("/proc/sys/vm/dirty_ratio", "90");
  opt_("/proc/sys/vm/dirty_background_ratio", "1");
  opt_("/proc/sys/vm/dirty_expire_centisecs", "60000");
  opt_("/proc/sys/vm/dirty_writeback_centisecs", "60000");

  // disks
  //for dev in $(awk '/^\/dev\/sd/ {print $1}' /etc/mtab); do run hdparm -B 1 -S 12 $dev; done
  //for i in /sys/class/scsi_host/host*/link_power_management_policy; do opt $i min_power; done

  // sound card
  opt_("/sys/module/snd_hda_intel/parameters/power_save", "1");
  opt_("/sys/module/snd_hda_intel/parameters/power_save_controller", "Y");
  //opt_("/sys/module/snd_ac97_codec/parameters/power_save", "1");

  // wlan0/eth0 powersave
  //run("iwconfig wlan0 power on");

  // screen powersave
  //for i in /sys/class/backlight/acpi_video*/brightness; do opt $i 0; done
  //opt_("/sys/module/i915/parameters/i915_enable_rc6", "1");

  // disable webcam
  //unload("uvcvideo");

  // Disable bluetooth
  //unload("btusb");
  //unload("bluetooth");
}

void settings_full() {
}

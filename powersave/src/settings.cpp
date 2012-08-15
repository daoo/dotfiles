#include "settings.hpp"

#include <cstdio>
#include "power.hpp"

void settings_check() {
  // {{{ bus
  // files: /sys/bus/*/devices/*/power/control

  check("/sys/bus/acpi/devices/ACPI0003:00/power/control");
  check("/sys/bus/acpi/devices/device:00/power/control");
  check("/sys/bus/acpi/devices/device:01/power/control");
  check("/sys/bus/acpi/devices/device:02/power/control");
  check("/sys/bus/acpi/devices/device:03/power/control");
  check("/sys/bus/acpi/devices/device:04/power/control");
  check("/sys/bus/acpi/devices/device:05/power/control");
  check("/sys/bus/acpi/devices/device:06/power/control");
  check("/sys/bus/acpi/devices/device:07/power/control");
  check("/sys/bus/acpi/devices/device:08/power/control");
  check("/sys/bus/acpi/devices/device:09/power/control");
  check("/sys/bus/acpi/devices/device:0a/power/control");
  check("/sys/bus/acpi/devices/device:0b/power/control");
  check("/sys/bus/acpi/devices/device:0c/power/control");
  check("/sys/bus/acpi/devices/device:0d/power/control");
  check("/sys/bus/acpi/devices/device:0e/power/control");
  check("/sys/bus/acpi/devices/device:0f/power/control");
  check("/sys/bus/acpi/devices/device:10/power/control");
  check("/sys/bus/acpi/devices/device:11/power/control");
  check("/sys/bus/acpi/devices/device:12/power/control");
  check("/sys/bus/acpi/devices/device:13/power/control");
  check("/sys/bus/acpi/devices/device:14/power/control");
  check("/sys/bus/acpi/devices/device:15/power/control");
  check("/sys/bus/acpi/devices/device:16/power/control");
  check("/sys/bus/acpi/devices/device:17/power/control");
  check("/sys/bus/acpi/devices/device:18/power/control");
  check("/sys/bus/acpi/devices/device:19/power/control");
  check("/sys/bus/acpi/devices/device:1a/power/control");
  check("/sys/bus/acpi/devices/device:1b/power/control");
  check("/sys/bus/acpi/devices/device:1c/power/control");
  check("/sys/bus/acpi/devices/device:1d/power/control");
  check("/sys/bus/acpi/devices/device:1e/power/control");
  check("/sys/bus/acpi/devices/device:1f/power/control");
  check("/sys/bus/acpi/devices/device:20/power/control");
  check("/sys/bus/acpi/devices/device:21/power/control");
  check("/sys/bus/acpi/devices/device:22/power/control");
  check("/sys/bus/acpi/devices/device:23/power/control");
  check("/sys/bus/acpi/devices/device:24/power/control");
  check("/sys/bus/acpi/devices/device:25/power/control");
  check("/sys/bus/acpi/devices/device:26/power/control");
  check("/sys/bus/acpi/devices/device:27/power/control");
  check("/sys/bus/acpi/devices/device:28/power/control");
  check("/sys/bus/acpi/devices/device:29/power/control");
  check("/sys/bus/acpi/devices/device:2a/power/control");
  check("/sys/bus/acpi/devices/device:2b/power/control");
  check("/sys/bus/acpi/devices/device:2c/power/control");
  check("/sys/bus/acpi/devices/device:2d/power/control");
  check("/sys/bus/acpi/devices/device:2e/power/control");
  check("/sys/bus/acpi/devices/device:2f/power/control");
  check("/sys/bus/acpi/devices/device:30/power/control");
  check("/sys/bus/acpi/devices/device:31/power/control");
  check("/sys/bus/acpi/devices/device:32/power/control");
  check("/sys/bus/acpi/devices/device:33/power/control");
  check("/sys/bus/acpi/devices/device:34/power/control");
  check("/sys/bus/acpi/devices/device:35/power/control");
  check("/sys/bus/acpi/devices/device:36/power/control");
  check("/sys/bus/acpi/devices/device:37/power/control");
  check("/sys/bus/acpi/devices/device:38/power/control");
  check("/sys/bus/acpi/devices/device:39/power/control");
  check("/sys/bus/acpi/devices/device:3a/power/control");
  check("/sys/bus/acpi/devices/device:3b/power/control");
  check("/sys/bus/acpi/devices/device:3c/power/control");
  check("/sys/bus/acpi/devices/device:3d/power/control");
  check("/sys/bus/acpi/devices/device:3e/power/control");
  check("/sys/bus/acpi/devices/device:3f/power/control");
  check("/sys/bus/acpi/devices/device:40/power/control");
  check("/sys/bus/acpi/devices/device:41/power/control");
  check("/sys/bus/acpi/devices/device:42/power/control");
  check("/sys/bus/acpi/devices/device:43/power/control");
  check("/sys/bus/acpi/devices/device:44/power/control");
  check("/sys/bus/acpi/devices/device:45/power/control");
  check("/sys/bus/acpi/devices/device:46/power/control");
  check("/sys/bus/acpi/devices/device:47/power/control");
  check("/sys/bus/acpi/devices/device:48/power/control");
  check("/sys/bus/acpi/devices/device:49/power/control");
  check("/sys/bus/acpi/devices/device:4a/power/control");
  check("/sys/bus/acpi/devices/device:4b/power/control");
  check("/sys/bus/acpi/devices/device:4c/power/control");
  check("/sys/bus/acpi/devices/device:4d/power/control");
  check("/sys/bus/acpi/devices/device:4e/power/control");
  check("/sys/bus/acpi/devices/device:4f/power/control");
  check("/sys/bus/acpi/devices/device:50/power/control");
  check("/sys/bus/acpi/devices/device:51/power/control");
  check("/sys/bus/acpi/devices/device:52/power/control");
  check("/sys/bus/acpi/devices/device:53/power/control");
  check("/sys/bus/acpi/devices/device:54/power/control");
  check("/sys/bus/acpi/devices/device:55/power/control");
  check("/sys/bus/acpi/devices/device:56/power/control");
  check("/sys/bus/acpi/devices/device:57/power/control");
  check("/sys/bus/acpi/devices/device:58/power/control");
  check("/sys/bus/acpi/devices/device:59/power/control");
  check("/sys/bus/acpi/devices/device:5a/power/control");
  check("/sys/bus/acpi/devices/device:5b/power/control");
  check("/sys/bus/acpi/devices/device:5c/power/control");
  check("/sys/bus/acpi/devices/device:5d/power/control");
  check("/sys/bus/acpi/devices/device:5e/power/control");
  check("/sys/bus/acpi/devices/device:5f/power/control");
  check("/sys/bus/acpi/devices/device:60/power/control");
  check("/sys/bus/acpi/devices/device:61/power/control");
  check("/sys/bus/acpi/devices/device:62/power/control");
  check("/sys/bus/acpi/devices/ETD0101:00/power/control");
  check("/sys/bus/acpi/devices/INT0800:00/power/control");
  check("/sys/bus/acpi/devices/INT340E:00/power/control");
  check("/sys/bus/acpi/devices/INT3F0D:00/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:00/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:01/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:02/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:03/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:04/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:05/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:06/power/control");
  check("/sys/bus/acpi/devices/LNXCPU:07/power/control");
  check("/sys/bus/acpi/devices/LNXPWRBN:00/power/control");
  check("/sys/bus/acpi/devices/LNXSYSTM:00/power/control");
  check("/sys/bus/acpi/devices/LNXTHERM:00/power/control");
  check("/sys/bus/acpi/devices/LNXVIDEO:00/power/control");
  check("/sys/bus/acpi/devices/LNXVIDEO:01/power/control");
  check("/sys/bus/acpi/devices/PNP0000:00/power/control");
  check("/sys/bus/acpi/devices/PNP0100:00/power/control");
  check("/sys/bus/acpi/devices/PNP0103:00/power/control");
  check("/sys/bus/acpi/devices/PNP0200:00/power/control");
  check("/sys/bus/acpi/devices/PNP0303:00/power/control");
  check("/sys/bus/acpi/devices/PNP0A08:00/power/control");
  check("/sys/bus/acpi/devices/PNP0B00:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C01:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C02:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C02:01/power/control");
  check("/sys/bus/acpi/devices/PNP0C02:02/power/control");
  check("/sys/bus/acpi/devices/PNP0C04:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C09:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C0A:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C0D:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C0E:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:01/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:02/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:03/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:04/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:05/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:06/power/control");
  check("/sys/bus/acpi/devices/PNP0C0F:07/power/control");
  check("/sys/bus/acpi/devices/PNP0C14:00/power/control");
  check("/sys/bus/acpi/devices/PNP0C14:01/power/control");
  check("/sys/bus/clocksource/devices/clocksource0/power/control");
  check("/sys/bus/cpu/devices/cpu0/power/control");
  check("/sys/bus/cpu/devices/cpu1/power/control");
  check("/sys/bus/cpu/devices/cpu2/power/control");
  check("/sys/bus/cpu/devices/cpu3/power/control");
  check("/sys/bus/event_source/devices/breakpoint/power/control");
  check("/sys/bus/event_source/devices/cpu/power/control");
  check("/sys/bus/event_source/devices/software/power/control");
  check("/sys/bus/event_source/devices/tracepoint/power/control");
  check("/sys/bus/i2c/devices/i2c-0/power/control");
  check("/sys/bus/i2c/devices/i2c-1/power/control");
  check("/sys/bus/i2c/devices/i2c-2/power/control");
  check("/sys/bus/i2c/devices/i2c-3/power/control");
  check("/sys/bus/i2c/devices/i2c-4/power/control");
  check("/sys/bus/i2c/devices/i2c-5/power/control");
  check("/sys/bus/i2c/devices/i2c-6/power/control");
  check("/sys/bus/i2c/devices/i2c-7/power/control");
  check("/sys/bus/i2c/devices/i2c-8/power/control");
  check("/sys/bus/machinecheck/devices/machinecheck0/power/control");
  check("/sys/bus/machinecheck/devices/machinecheck1/power/control");
  check("/sys/bus/machinecheck/devices/machinecheck2/power/control");
  check("/sys/bus/machinecheck/devices/machinecheck3/power/control");
  check("/sys/bus/media/devices/media0/power/control");
  check("/sys/bus/memory/devices/memory0/power/control");
  check("/sys/bus/memory/devices/memory10/power/control");
  check("/sys/bus/memory/devices/memory11/power/control");
  check("/sys/bus/memory/devices/memory12/power/control");
  check("/sys/bus/memory/devices/memory13/power/control");
  check("/sys/bus/memory/devices/memory14/power/control");
  check("/sys/bus/memory/devices/memory15/power/control");
  check("/sys/bus/memory/devices/memory16/power/control");
  check("/sys/bus/memory/devices/memory17/power/control");
  check("/sys/bus/memory/devices/memory18/power/control");
  check("/sys/bus/memory/devices/memory19/power/control");
  check("/sys/bus/memory/devices/memory1/power/control");
  check("/sys/bus/memory/devices/memory20/power/control");
  check("/sys/bus/memory/devices/memory21/power/control");
  check("/sys/bus/memory/devices/memory2/power/control");
  check("/sys/bus/memory/devices/memory32/power/control");
  check("/sys/bus/memory/devices/memory33/power/control");
  check("/sys/bus/memory/devices/memory34/power/control");
  check("/sys/bus/memory/devices/memory35/power/control");
  check("/sys/bus/memory/devices/memory36/power/control");
  check("/sys/bus/memory/devices/memory37/power/control");
  check("/sys/bus/memory/devices/memory38/power/control");
  check("/sys/bus/memory/devices/memory39/power/control");
  check("/sys/bus/memory/devices/memory3/power/control");
  check("/sys/bus/memory/devices/memory40/power/control");
  check("/sys/bus/memory/devices/memory41/power/control");
  check("/sys/bus/memory/devices/memory4/power/control");
  check("/sys/bus/memory/devices/memory5/power/control");
  check("/sys/bus/memory/devices/memory6/power/control");
  check("/sys/bus/memory/devices/memory7/power/control");
  check("/sys/bus/memory/devices/memory8/power/control");
  check("/sys/bus/memory/devices/memory9/power/control");
  check("/sys/bus/node/devices/node0/power/control");
  check("/sys/bus/pci/devices/0000:00:00.0/power/control");
  check("/sys/bus/pci/devices/0000:00:01.0/power/control");
  check("/sys/bus/pci/devices/0000:00:02.0/power/control");
  check("/sys/bus/pci/devices/0000:00:16.0/power/control");
  check("/sys/bus/pci/devices/0000:00:1a.0/power/control");
  check("/sys/bus/pci/devices/0000:00:1b.0/power/control");
  check("/sys/bus/pci/devices/0000:00:1c.0/power/control");
  check("/sys/bus/pci/devices/0000:00:1c.1/power/control");
  check("/sys/bus/pci/devices/0000:00:1c.3/power/control");
  check("/sys/bus/pci/devices/0000:00:1c.5/power/control");
  check("/sys/bus/pci/devices/0000:00:1d.0/power/control");
  check("/sys/bus/pci/devices/0000:00:1f.0/power/control");
  check("/sys/bus/pci/devices/0000:00:1f.2/power/control");
  check("/sys/bus/pci/devices/0000:00:1f.3/power/control");
  check("/sys/bus/pci/devices/0000:01:00.0/power/control");
  check("/sys/bus/pci/devices/0000:03:00.0/power/control");
  check("/sys/bus/pci/devices/0000:04:00.0/power/control");
  check("/sys/bus/pci/devices/0000:05:00.0/power/control");
  check("/sys/bus/pci_express/devices/0000:00:01.0:pcie08/power/control");
  check("/sys/bus/platform/devices/alarmtimer/power/control");
  check("/sys/bus/platform/devices/asus-nb-wmi/power/control");
  check("/sys/bus/platform/devices/coretemp.0/power/control");
  check("/sys/bus/platform/devices/i8042/power/control");
  check("/sys/bus/platform/devices/iTCO_wdt/power/control");
  check("/sys/bus/platform/devices/microcode/power/control");
  check("/sys/bus/platform/devices/pcspkr/power/control");
  check("/sys/bus/platform/devices/regulatory.0/power/control");
  check("/sys/bus/platform/devices/serial8250/power/control");
  check("/sys/bus/pnp/devices/00:00/power/control");
  check("/sys/bus/pnp/devices/00:01/power/control");
  check("/sys/bus/pnp/devices/00:02/power/control");
  check("/sys/bus/pnp/devices/00:03/power/control");
  check("/sys/bus/pnp/devices/00:04/power/control");
  check("/sys/bus/pnp/devices/00:05/power/control");
  check("/sys/bus/pnp/devices/00:06/power/control");
  check("/sys/bus/pnp/devices/00:07/power/control");
  check("/sys/bus/pnp/devices/00:08/power/control");
  check("/sys/bus/pnp/devices/00:09/power/control");
  check("/sys/bus/pnp/devices/00:0a/power/control");
  check("/sys/bus/pnp/devices/00:0b/power/control");
  check("/sys/bus/pnp/devices/00:0c/power/control");
  check("/sys/bus/scsi/devices/0:0:0:0/power/control");
  check("/sys/bus/scsi/devices/2:0:0:0/power/control");
  check("/sys/bus/scsi/devices/host0/power/control");
  check("/sys/bus/scsi/devices/host1/power/control");
  check("/sys/bus/scsi/devices/host2/power/control");
  check("/sys/bus/scsi/devices/host3/power/control");
  check("/sys/bus/scsi/devices/host4/power/control");
  check("/sys/bus/scsi/devices/host5/power/control");
  check("/sys/bus/scsi/devices/target0:0:0/power/control");
  check("/sys/bus/scsi/devices/target2:0:0/power/control");
  check("/sys/bus/serio/devices/serio0/power/control");
  check("/sys/bus/serio/devices/serio1/power/control");
  check("/sys/bus/serio/devices/serio2/power/control");
  check("/sys/bus/serio/devices/serio3/power/control");
  check("/sys/bus/serio/devices/serio4/power/control");
  check("/sys/bus/usb/devices/1-1.1/power/control");
  check("/sys/bus/usb/devices/1-1.2/power/control");
  check("/sys/bus/usb/devices/1-1/power/control");
  check("/sys/bus/usb/devices/3-1/power/control");
  check("/sys/bus/usb/devices/usb1/power/control");
  check("/sys/bus/usb/devices/usb2/power/control");
  check("/sys/bus/usb/devices/usb3/power/control");
  check("/sys/bus/usb/devices/usb4/power/control");
  // }}}
  // {{{ usb autosuspend
  // files: /sys/bus/usb/devices/*/power/autosuspend

  check("/sys/bus/usb/devices/1-1.1/power/autosuspend");
  check("/sys/bus/usb/devices/1-1.2/power/autosuspend");
  check("/sys/bus/usb/devices/1-1/power/autosuspend");
  check("/sys/bus/usb/devices/3-1/power/autosuspend");
  check("/sys/bus/usb/devices/usb1/power/autosuspend");
  check("/sys/bus/usb/devices/usb2/power/autosuspend");
  check("/sys/bus/usb/devices/usb3/power/autosuspend");
  check("/sys/bus/usb/devices/usb4/power/autosuspend");
  // }}}

  // nmi_watchdog
  check("/proc/sys/kernel/nmi_watchdog");

  // cpu
  check("/sys/devices/system/cpu/sched_mc_power_savings");
  check("/sys/devices/system/cpu/sched_smt_power_savings");
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
  // files: /dev/sd* from /etc/mtab
  printf("/dev/sda3 readahead: "); fflush(stdout); run("blockdev --getra /dev/sda3");
  printf("/dev/sda4 readahead: "); fflush(stdout); run("blockdev --getra /dev/sda4");
  // files: /sys/class/scsi_host/host*/link_power_management_policy; do opt $i min_power; done

  // sound card
  check("/sys/module/snd_hda_intel/parameters/power_save");
  check("/sys/module/snd_hda_intel/parameters/power_save_controller");

  // display
  check("/sys/class/backlight/acip_video0/brightness");

  is_loaded("btusb");
}

void settings_save() {
  // {{{ bus
  // files: /sys/bus/*/devices/*/power/control

  opt("/sys/bus/acpi/devices/ACPI0003:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:01/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:02/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:03/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:04/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:05/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:06/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:07/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:08/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:09/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:0a/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:0b/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:0c/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:0d/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:0e/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:0f/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:10/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:11/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:12/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:13/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:14/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:15/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:16/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:17/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:18/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:19/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:1a/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:1b/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:1c/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:1d/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:1e/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:1f/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:20/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:21/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:22/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:23/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:24/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:25/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:26/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:27/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:28/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:29/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:2a/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:2b/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:2c/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:2d/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:2e/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:2f/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:30/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:31/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:32/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:33/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:34/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:35/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:36/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:37/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:38/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:39/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:3a/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:3b/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:3c/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:3d/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:3e/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:3f/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:40/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:41/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:42/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:43/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:44/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:45/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:46/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:47/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:48/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:49/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:4a/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:4b/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:4c/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:4d/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:4e/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:4f/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:50/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:51/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:52/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:53/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:54/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:55/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:56/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:57/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:58/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:59/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:5a/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:5b/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:5c/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:5d/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:5e/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:5f/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:60/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:61/power/control", "auto");
  opt("/sys/bus/acpi/devices/device:62/power/control", "auto");
  opt("/sys/bus/acpi/devices/ETD0101:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/INT0800:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/INT340E:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/INT3F0D:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:01/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:02/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:03/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:04/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:05/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:06/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXCPU:07/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXPWRBN:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXSYSTM:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXTHERM:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXVIDEO:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/LNXVIDEO:01/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0000:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0100:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0103:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0200:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0303:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0A08:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0B00:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C01:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C02:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C02:01/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C02:02/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C04:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C09:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0A:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0D:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0E:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:01/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:02/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:03/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:04/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:05/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:06/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C0F:07/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C14:00/power/control", "auto");
  opt("/sys/bus/acpi/devices/PNP0C14:01/power/control", "auto");
  opt("/sys/bus/clocksource/devices/clocksource0/power/control", "auto");
  opt("/sys/bus/cpu/devices/cpu0/power/control", "auto");
  opt("/sys/bus/cpu/devices/cpu1/power/control", "auto");
  opt("/sys/bus/cpu/devices/cpu2/power/control", "auto");
  opt("/sys/bus/cpu/devices/cpu3/power/control", "auto");
  opt("/sys/bus/event_source/devices/breakpoint/power/control", "auto");
  opt("/sys/bus/event_source/devices/cpu/power/control", "auto");
  opt("/sys/bus/event_source/devices/software/power/control", "auto");
  opt("/sys/bus/event_source/devices/tracepoint/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-0/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-1/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-2/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-3/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-4/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-5/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-6/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-7/power/control", "auto");
  opt("/sys/bus/i2c/devices/i2c-8/power/control", "auto");
  opt("/sys/bus/machinecheck/devices/machinecheck0/power/control", "auto");
  opt("/sys/bus/machinecheck/devices/machinecheck1/power/control", "auto");
  opt("/sys/bus/machinecheck/devices/machinecheck2/power/control", "auto");
  opt("/sys/bus/machinecheck/devices/machinecheck3/power/control", "auto");
  opt("/sys/bus/media/devices/media0/power/control", "auto");
  opt("/sys/bus/memory/devices/memory0/power/control", "auto");
  opt("/sys/bus/memory/devices/memory10/power/control", "auto");
  opt("/sys/bus/memory/devices/memory11/power/control", "auto");
  opt("/sys/bus/memory/devices/memory12/power/control", "auto");
  opt("/sys/bus/memory/devices/memory13/power/control", "auto");
  opt("/sys/bus/memory/devices/memory14/power/control", "auto");
  opt("/sys/bus/memory/devices/memory15/power/control", "auto");
  opt("/sys/bus/memory/devices/memory16/power/control", "auto");
  opt("/sys/bus/memory/devices/memory17/power/control", "auto");
  opt("/sys/bus/memory/devices/memory18/power/control", "auto");
  opt("/sys/bus/memory/devices/memory19/power/control", "auto");
  opt("/sys/bus/memory/devices/memory1/power/control", "auto");
  opt("/sys/bus/memory/devices/memory20/power/control", "auto");
  opt("/sys/bus/memory/devices/memory21/power/control", "auto");
  opt("/sys/bus/memory/devices/memory2/power/control", "auto");
  opt("/sys/bus/memory/devices/memory32/power/control", "auto");
  opt("/sys/bus/memory/devices/memory33/power/control", "auto");
  opt("/sys/bus/memory/devices/memory34/power/control", "auto");
  opt("/sys/bus/memory/devices/memory35/power/control", "auto");
  opt("/sys/bus/memory/devices/memory36/power/control", "auto");
  opt("/sys/bus/memory/devices/memory37/power/control", "auto");
  opt("/sys/bus/memory/devices/memory38/power/control", "auto");
  opt("/sys/bus/memory/devices/memory39/power/control", "auto");
  opt("/sys/bus/memory/devices/memory3/power/control", "auto");
  opt("/sys/bus/memory/devices/memory40/power/control", "auto");
  opt("/sys/bus/memory/devices/memory41/power/control", "auto");
  opt("/sys/bus/memory/devices/memory4/power/control", "auto");
  opt("/sys/bus/memory/devices/memory5/power/control", "auto");
  opt("/sys/bus/memory/devices/memory6/power/control", "auto");
  opt("/sys/bus/memory/devices/memory7/power/control", "auto");
  opt("/sys/bus/memory/devices/memory8/power/control", "auto");
  opt("/sys/bus/memory/devices/memory9/power/control", "auto");
  opt("/sys/bus/node/devices/node0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:00.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:01.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:02.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:16.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1a.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1b.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1c.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1c.1/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1c.3/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1c.5/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1d.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1f.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1f.2/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:00:1f.3/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:01:00.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:03:00.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:04:00.0/power/control", "auto");
  opt("/sys/bus/pci/devices/0000:05:00.0/power/control", "auto");
  opt("/sys/bus/pci_express/devices/0000:00:01.0:pcie08/power/control", "auto");
  opt("/sys/bus/platform/devices/alarmtimer/power/control", "auto");
  opt("/sys/bus/platform/devices/asus-nb-wmi/power/control", "auto");
  opt("/sys/bus/platform/devices/coretemp.0/power/control", "auto");
  opt("/sys/bus/platform/devices/i8042/power/control", "auto");
  opt("/sys/bus/platform/devices/iTCO_wdt/power/control", "auto");
  opt("/sys/bus/platform/devices/microcode/power/control", "auto");
  opt("/sys/bus/platform/devices/pcspkr/power/control", "auto");
  opt("/sys/bus/platform/devices/regulatory.0/power/control", "auto");
  opt("/sys/bus/platform/devices/serial8250/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:00/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:01/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:02/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:03/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:04/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:05/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:06/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:07/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:08/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:09/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:0a/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:0b/power/control", "auto");
  opt("/sys/bus/pnp/devices/00:0c/power/control", "auto");
  opt("/sys/bus/scsi/devices/0:0:0:0/power/control", "auto");
  opt("/sys/bus/scsi/devices/2:0:0:0/power/control", "auto");
  opt("/sys/bus/scsi/devices/host0/power/control", "auto");
  opt("/sys/bus/scsi/devices/host1/power/control", "auto");
  opt("/sys/bus/scsi/devices/host2/power/control", "auto");
  opt("/sys/bus/scsi/devices/host3/power/control", "auto");
  opt("/sys/bus/scsi/devices/host4/power/control", "auto");
  opt("/sys/bus/scsi/devices/host5/power/control", "auto");
  opt("/sys/bus/scsi/devices/target0:0:0/power/control", "auto");
  opt("/sys/bus/scsi/devices/target2:0:0/power/control", "auto");
  opt("/sys/bus/serio/devices/serio0/power/control", "auto");
  opt("/sys/bus/serio/devices/serio1/power/control", "auto");
  opt("/sys/bus/serio/devices/serio2/power/control", "auto");
  opt("/sys/bus/serio/devices/serio3/power/control", "auto");
  opt("/sys/bus/serio/devices/serio4/power/control", "auto");
  opt("/sys/bus/usb/devices/1-1.1/power/control", "auto");
  opt("/sys/bus/usb/devices/1-1.2/power/control", "auto");
  opt("/sys/bus/usb/devices/1-1/power/control", "auto");
  opt("/sys/bus/usb/devices/3-1/power/control", "auto");
  opt("/sys/bus/usb/devices/usb1/power/control", "auto");
  opt("/sys/bus/usb/devices/usb2/power/control", "auto");
  opt("/sys/bus/usb/devices/usb3/power/control", "auto");
  opt("/sys/bus/usb/devices/usb4/power/control", "auto");
  // }}}
  // {{{ usb autosuspend
  // files: /sys/bus/usb/devices/*/power/autosuspend

  opt("/sys/bus/usb/devices/1-1.1/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/1-1.2/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/1-1/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/3-1/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/usb1/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/usb2/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/usb3/power/autosuspend", "2");
  opt("/sys/bus/usb/devices/usb4/power/autosuspend", "2");
  // }}}

  // nmi_watchdog
  opt("/proc/sys/kernel/nmi_watchdog", "0");

  // cpu
  //opt("/sys/devices/system/cpu/cpu0/cpufreq/scaling_governor", "powersave");
  //opt("/sys/devices/system/cpu/cpu1/cpufreq/scaling_governor", "powersave");
  //opt("/sys/devices/system/cpu/cpu2/cpufreq/scaling_governor", "powersave");
  //opt("/sys/devices/system/cpu/cpu3/cpufreq/scaling_governor", "powersave");

  // aspm
  opt("/sys/module/pcie_aspm/parameters/policy", "powersave");

  // kernel write mode
  opt("/proc/sys/vm/laptop_mode", "5");
  opt("/proc/sys/vm/dirty_ratio", "90");
  opt("/proc/sys/vm/dirty_background_ratio", "1");
  opt("/proc/sys/vm/dirty_expire_centisecs", "60000");
  opt("/proc/sys/vm/dirty_writeback_centisecs", "60000");

  // sound card
  opt("/sys/module/snd_hda_intel/parameters/power_save", "1");
  opt("/sys/module/snd_hda_intel/parameters/power_save_controller", "Y");
}

void settings_full() {
}

// vim: set fdm=marker :

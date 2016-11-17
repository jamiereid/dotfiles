# your_user your_machine = NOPASSWD: /path/to/script/bright_up
# your_user your_machine = NOPASSWD: /path/to/script/bright_down

max_brightness=$(cat /sys/class/backlight/intel_backlight/max_brightness)
brightness=$(cat /sys/class/backlight/intel_backlight/brightness)

if (($brightness < $max_brightness)); then
  let brightness=$brightness+100
  echo "echo $brightness > /sys/class/backlight/intel_backlight/brightness" | sudo zsh #or bash
  notify-send Brightness "${brightness}/${max_brightness}" -t 200
fi

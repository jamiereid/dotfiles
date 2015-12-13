MAX0="$(cat /sys/class/power_supply/BAT0/charge_full)"
CAP0="$(cat /sys/class/power_supply/BAT0/charge_now)"

BAT="$(echo "100*(${CAP0})/(${MAX0})" | bc)"

echo $BAT%

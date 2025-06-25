#!/bin/bash

compositor=""

if [[ "$XDG_CURRENT_DESKTOP" == "Hyprland" ]] || pgrep -x "Hyprland" >/dev/null; then
	compositor="hyprland"
elif [[ "$XDG_CURRENT_DESKTOP" == "sway" ]] || pgrep -x "sway" >/dev/null; then
	compositor="sway"
fi

SELECTION="$(printf "1 - Lock\n2 - Suspend\n3 - Log out\n4 - Reboot\n5 - Reboot to UEFI\n6 - Hard reboot\n7 - Shutdown" | fuzzel --dmenu -l 7 -p "Power Menu: ")"

case $SELECTION in
*"Lock")
	if [[ $compositor == "hyprland" ]]; then
		hyprlock
	elif [[ $compositor == "sway" ]]; then
		"$HOME"/.local/bin/lock
	fi
	;;
*"Suspend")
	systemctl suspend
	;;
*"Log out")
	if [[ $compositor == "hyprland" ]]; then
		hyprctl dispatch exit
	elif [[ $compositor == "sway" ]]; then
		swaymsg exit
	fi
	;;
*"Reboot")
	systemctl reboot
	;;
*"Reboot to UEFI")
	systemctl reboot --firmware-setup
	;;
*"Hard reboot")
	pkexec "echo b > /proc/sysrq-trigger"
	;;
*"Shutdown")
	systemctl poweroff
	;;
esac

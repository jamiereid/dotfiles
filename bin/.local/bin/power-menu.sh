#!/bin/bash

compositor=""

if [[ "$XDG_CURRENT_DESKTOP" == "Hyprland" ]] || pgrep -x "Hyprland" >/dev/null; then
  compositor="hyprland"
elif [[ "$XDG_CURRENT_DESKTOP" == "sway" ]] || pgrep -x "sway" >/dev/null; then
  compositor="sway"
fi

# check if windows entry exists in GRUB
has_windows=false
if doas grep -q "Windows 11" /boot/grub/grub.cfg 2>/dev/null; then
  has_windows=true
fi

if $has_windows; then
  menu="1 - Lock\n2 - Suspend\n3 - Log out\n4 - Reboot\n5 - Reboot to Windows\n6 - Reboot to UEFI\n7 - Hard reboot\n8 - Shutdown"
  menu_lines=8
else
  menu="1 - Lock\n2 - Suspend\n3 - Log out\n4 - Reboot\n5 - Reboot to UEFI\n6 - Hard reboot\n7 - Shutdown"
  menu_lines=7
fi

SELECTION="$(echo -e "$menu" | fuzzel --dmenu -l $menu_lines -p "Power Menu: ")"

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
  *"Reboot to Windows")
    doas grub-reboot "Windows 11 (VR Gaming)"
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

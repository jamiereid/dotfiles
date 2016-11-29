#!/bin/bash

COLGOOD="#2E9D54"
COLBAD="#cc6666"
COLDEGRADED="#81a2be"

IWPRESENT="$(ip link | grep wlp)"

if [[ -n "$IWPRESENT" ]]; then
    INT="$(ip link | grep wlp | awk -F: '{print $2}' | tr -d ' ')"
    SSID="$(/sbin/iwgetid -r)"
    if [[ -z "$SSID" ]]; then
        RET=" down"
		RETCOLOR=$COLBAD
    else
        SIGNAL="$(/sbin/iwconfig $INT | sed -n 's/.*Link Quality=\([0-9][0-9]*\/[0-9][0-9]*\).*/\1/p')"
        IP="$(ip addr show $INT | grep "inet " | awk '{print $2}')"
        RET=" $SIGNAL at $SSID ($IP)"
		RETCOLOR=$COLGOOD
    fi 
else
    RET=" Disabled"
    RETCOLOR=$COLBAD
fi

echo $RET
echo
echo $RETCOLOR

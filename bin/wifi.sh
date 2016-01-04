IWPRESENT="$(ifconfig | grep wlp)"

if [[ -n "$IWPRESENT" ]]; then
    SSID="$(iwgetid -r)"
    if [[ -z "$SSID" ]]; then
        RET=""
    else
        RET=" $SSID"
    fi 
else
    RET=""
fi

echo $RET

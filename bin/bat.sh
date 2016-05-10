COLGOOD="#2E9D54"
COLBAD="#cc6666"
COLDEGRADED="#81a2be"

BAT=$(upower -e | grep BAT)
STATE=$(upower -i $BAT | grep state | awk '{print $2}')
CURRENT_PERCENT=$(upower -i $BAT | grep percentage | awk '{print $2}')

case "$CURRENT_PERCENT" in
    9[0-9]*|100*)
        BATICON="  "
        RETCOLOR=$COLGOOD
        ;;
    5[0-9]*|6[0-9]*|7[0-9]*|8[0-9]*)
        BATICON="  "
        RETCOLOR=$COLGOOD
        ;;
    3[0-9]*|4[0-9]*)
        BATICON="  "
        RETCOLOR=$COLDEGRADED
        ;;
    1[1-9]*|2[0-9]*)
        BATICON="  "
        RETCOLOR=$COLDEGRADED
        ;;
    *)
        BATICON="  "
        RETCOLOR=$COLBAD
        ;;
esac

if [[ "$STATE" = "discharging" ]]; then
    REMAINING=$(upower -i $BAT | grep "time to empty" | awk '{print $4" "$5}')
    RET="$BATICON $CURRENT_PERCENT ($REMAINING)"
elif [[ "$STATE" = "charging" ]]; then
    REMAINING=$(upower -i $BAT | grep "time to full" | awk '{print $4" "$5}')
    RET=" $CURRENT_PERCENT ($REMAINING)"
    RETCOLOR=$COLGOOD
else
    RET=" $CURRENT_PERCENT"
    RETCOLOR=$COLGOOD
fi

echo $RET
echo
echo $RETCOLOR

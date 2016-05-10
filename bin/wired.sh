COLGOOD="#2E9D54"
COLBAD="#cc6666"
COLDEGRADED="#81a2be"

INTPRESENT="$(ifconfig | grep enp)"

if [[ -n "$INTPRESENT" ]]; then
    INT="$(ifconfig | grep enp | awk -F: '{print $1}')"
    CARRIER="$(cat /sys/class/net/$INT/carrier)"
    if [[ "$CARRIER" -eq 0 ]]; then
        RET=" down"
		RETCOLOR=$COLBAD
    else
        IP="$(ip addr show $INT | grep "inet " | awk '{print $2}')"
        RET=" Connected ($IP)"
		RETCOLOR=$COLGOOD
    fi 
else
    RET=" no interface"
    RETCOLOR=$COLBAD
fi

echo $RET
echo
echo $RETCOLOR

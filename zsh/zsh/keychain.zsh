if [ -x /usr/bin/keychain ]; then
    keychain --timeout 15 $HOME/.ssh/id_rsa
    source $HOME/.keychain/$HOSTNAME-sh
fi

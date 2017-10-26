if [[ "$OSTYPE" == linux* ]]; then
  if (( $+commands['systemctl'] )) ; then
    # systemctl --user env imports
    systemctl --user import-environment PATH SSH_AUTH_SOCK GTAGSCONF GTAGSLABEL VM
  fi

  if [[ "$TERM" == xterm ]]; then
    export TERM=xterm-256color
  fi
fi


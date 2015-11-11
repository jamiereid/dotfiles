if [[ "$OSTYPE" == linux* ]]; then
  # systemctl --user env imports
  systemctl --user import-environment PATH SSH_AUTH_SOCK GTAGSCONF GTAGSLABEL VM

  if [[ "$TERM" == xterm ]]; then
    export TERM=xterm-256color
  fi
fi


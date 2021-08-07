# hack for wsl?
cd $HOME

abbr -a t env DISPLAY=(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0 LIBGL_ALWAYS_INDIRECT=1 setsid xfce4-terminal
set __fish_prompt_lastchar '::'

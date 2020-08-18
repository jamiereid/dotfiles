#set -gx DOCKER_HOST tcp://localhost:2375

# hack for wsl?
cd $HOME

abbr -a doxfx mono /opt/docfx-git/docfx.exe
#abbr -a e env DISPLAY=(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0 setsid emacs
abbr -a t env DISPLAY=(cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0 LIBGL_ALWAYS_INDIRECT=1 setsid xfce4-terminal

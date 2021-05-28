#set -gx DOCKER_HOST tcp://localhost:2375
set -gx DISPLAY (cat /etc/resolv.conf | grep nameserver | awk '{print $2; exit;}'):0.0
set -gx LIBGL_ALWAYS_INDIRECT 1

# hack for wsl?
cd $HOME

abbr -a doxfx mono /opt/docfx-git/docfx.exe
abbr -a t setsid xfce4-terminal

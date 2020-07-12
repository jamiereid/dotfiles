#set -gx DOCKER_HOST tcp://localhost:2375

# hack for wsl?
cd $HOME

abbr -a doxfx mono /opt/docfx-git/docfx.exe
abbr -a e env DISPLAY=:0 setsid emacs

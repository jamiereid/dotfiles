# tmux
alias tmux="tmux -2"
alias t="tmux"
alias tn="tmux new -s"
alias ta="tmux a -t"
alias taro="tmux a -rt"

# ag
alias agq="ag -Q"

alias ls="ls -lh --color=auto"

# ;)
alias ":q"="exit"
alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'

alias tsup="sudo ntpd -qg"

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

alias svim='sudoedit'

# prompt if deleting more than 3 files
alias rm='rm -I'

if (( $+commands[xsel] )); then
  alias cbc='xsel -i -b'
  alias cbp='xsel -o -b'
elif (( $+commands[pbcopy] )); then
  alias cbc='pbcopy'
  alias cbp='pbpaste'
fi

if (( $+commands[gist] )); then
  alias gist='gist -c -o'
fi

if (( $+commands[systemctl] )); then
  alias sc="systemctl"
  alias scu="systemctl --user"
  alias jc="journalctl"
  alias jcu="journalctl --user-unit"
fi

# docker
if (( $+commands[docker] )); then
	alias dockerip='docker ps | tail -n +2 | while read cid b; do echo -n "$cid\t"; docker inspect $cid | grep IPAddress | cut -d \" -f 4; done'
fi

if (( $+commands[thefuck] )); then
	eval $(thefuck --alias)
fi

alias t=todo.sh

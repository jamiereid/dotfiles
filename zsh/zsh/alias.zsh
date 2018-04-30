# tmux
alias tmux="tm"

# ag
alias agq="ag -Q"

# ;)
alias ":q"="exit"
alias ":qa"='[[ -n $TMUX ]] && tmux confirm-before kill-session'

# fixes weird problem in tmux and ssh with zsh-syntax-highlighting
alias sudo='sudo '

# prompt if deleting more than 3 files
alias rm='rm -I'

# clipboard
if (( $+commands[xsel] )); then
  alias cbc='xsel -i -b'
  alias cbp='xsel -o -b'
elif (( $+commands[pbcopy] )); then
  alias cbc='pbcopy'
  alias cbp='pbpaste'
fi

# gist
if (( $+commands[gist] )); then
  alias gist='gist -c -o'
fi

# systemctl
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

# the fuck
if (( $+commands[thefuck] )); then
	eval $(thefuck --alias)
fi

alias vi="vim"
alias ls="ls -lh --color=auto"
alias ll="ls -la"
alias grep="grep --color=always"
alias cp="cp -Ra"
alias mv="mv -i"
alias rm="rm -i"
alias shred="shred -uz"
alias psg="ps auxw | grep -i "
alias ping="ping -c 5"
alias more="less"
alias calc="bc -l <<<"
alias spell="aspell -a <<< "

alias ppxml="xmllint --format -"
alias ppjson=""

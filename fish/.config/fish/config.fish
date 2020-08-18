# fix rustup not adding path for fish (https://github.com/rust-lang/rustup.rs/issues/478)

set -gx EDITOR vim
set -gx LANG en_US.UTF-8

set -gx GOPATH ~/.local/share/go/
set -gx PATH ~/.local/bin ~/.cargo/bin $GOPATH/bin $PATH

set -gx FZF_DEFAULT_COMMAND 'fd --type file --follow'
set -gx FZF_CTRL_T_COMMAND 'fd --type file --follow'
set -gx FZF_DEFAULT_OPTS '--height 20%'

set fish_greeting

# auto load tmux unless we're already in tmux, or on tty1
if status --is-interactive; \
and not string match -q "tmux*" $TERM; \
and not string match -eq "/dev/tty1" (tty); \
or set -q WSLENV
    tmux new-session -A -s main 2> /dev/null; and exec true
end

abbr -a gs 'git status'
abbr -a gc 'git commit -m'
abbr -a ga 'git add -p'
abbr -a httpserver 'python -m http.server 8000'

if command -v exa > /dev/null
    abbr -a ls 'exa'
    abbr -a ll 'exa -la'
else
    abbr -a ll 'ls -la'
end

if command -v nvim > /dev/null
    abbr -a vi 'nvim'
    abbr -a vim 'nvim'
else
    abbr -a vi 'vim'
end

set -g fish_prompt_pwd_dir_length 3

function fish_prompt
  printf '%s %s $ ' (uname -n) (prompt_pwd)
end

if test -e ~/.config/fish/(uname -n).fish
    source ~/.config/fish/(uname -n).fish
end

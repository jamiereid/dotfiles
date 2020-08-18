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

abbr -a c cargo
abbr -a e nvim
abbr -a m make
abbr -a g git
abbr -a gs 'git status'
abbr -a gc 'git commit -m'
abbr -a gco 'git checkout'
abbr -a ga 'git add -p'
abbr -a httpserver 'python -m http.server 8000'

if command -v yay > /dev/null
    abbr -a p 'yay'
    abbr -a pup 'yay -Syu'
else
    abbr -a p 'sudo pacman'
    abbr -a pup 'sudo pacman -Syu'
end

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

# Fish git prompt
set __fish_git_prompt_showuntrackedfiles 'yes'
set __fish_git_prompt_showdirtystate 'yes'
set __fish_git_prompt_showstashstate 'yes'
set __fish_git_prompt_showupstream 'verbose name'
set __fish_git_prompt_describe_style 'branch'
set __fish_git_prompt_color_upstream_ahead green
set __fish_git_prompt_color_downstream_behind red

set __fish_git_prompt_char_dirtystate '+'
set __fish_git_prompt_char_stagedstate '●'
set __fish_git_prompt_char_untrackedfiles '…'
set __fish_git_prompt_char_stashstate '$'
set __fish_git_prompt_char_upstream_ahead '↑'
set __fish_git_prompt_char_upstream_behind '↓'
set __fish_git_prompt_char_upstream_equal ''
set __fish_git_prompt_char_cleanstate ''
set __fish_git_prompt_char_invalidstate 'X'

set -g fish_prompt_pwd_dir_length 3

function is_status_okay
    [ $status = 0 ]
end

## ACME theme
function fish_prompt
  if set -q SSH_TTY
    set_color purple
    echo -n "@"(hostname)":"
  end

  set_color black
  echo -n (prompt_pwd)

  set_color blue
  printf '%s' (__fish_git_prompt ':%s')

  if set -q VIRTUAL_ENV
    set_color green
    echo -n ":"(basename "$VIRTUAL_ENV")
    set_color normal
  end

  set_color --bold black
  echo -n " » "
end

## dark theme
#function fish_prompt
#  if set -q SSH_TTY
#    set_color yellow
#    echo -n "@"(hostname)":"
#  end
#
#  set_color white
#  echo -n (prompt_pwd)
#
#  set_color brown
#  printf '%s' (__fish_git_prompt ':%s')
#
#  if set -q VIRTUAL_ENV
#    set_color blue
#    echo -n ":"(basename "$VIRTUAL_ENV")
#    set_color normal
#  end
#
#  if is_status_okay
#      set_color green
#  else
#      set_color red
#  end
#  echo -n " » "
#end

if test -e ~/.config/fish/(uname -n).fish
    source ~/.config/fish/(uname -n).fish
end

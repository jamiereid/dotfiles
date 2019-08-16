# fix rustup not adding path for fish (https://github.com/rust-lang/rustup.rs/issues/478)
set PATH $HOME/.cargo/bin $HOME/bin $PATH

abbr -a g git
abbr -a gco 'git checkout'
abbr -a gc 'git commit -m "'
abbr -a ga 'git add'
abbr -a gs 'git status'
complete --command g --wraps git

# auto load tmux
if status --is-interactive
    tmux ^ /dev/null; and exec true
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

if command -v neomutt > /dev/null
    abbr -a mutt neomutt
end

# this is nice, and will come in handy, just need to fill out cases
#function ssh
#    switch $argv[1]
#    case "*.amazonaws.com"
#        env TERM=xterm /usr/bin/ssh $argv
#    case "ubuntu@"
#        env TERM=xterm /usr/bin/ssh $argv
#    case "*"
#        /usr/bin/ssh $argv
#    end
#end

#if command -v ct > /dev/null
#    function ssh
#        /usr/bin/ssh $argv | ct
#    end
#end

function remote_alacritty
    # https://gist.github.com/costis/5135502
    set fn (mktemp)
    infocmp alacritty > $fn
    scp $fn $argv[1]":alacritty.ti"
    ssh $argv[1] tic "alacritty.ti"
    ssh $argv[1] rm "alacritty.ti"
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

# colored man output
# from http://linuxtidbits.wordpress.com/2009/03/23/less-colors-for-man-pages/
setenv LESS_TERMCAP_mb \e'[01;31m'       # begin blinking
setenv LESS_TERMCAP_md \e'[01;38;5;74m'  # begin bold
setenv LESS_TERMCAP_me \e'[0m'           # end mode
setenv LESS_TERMCAP_se \e'[0m'           # end standout-mode
setenv LESS_TERMCAP_so \e'[38;5;246m'    # begin standout-mode - info box
setenv LESS_TERMCAP_ue \e'[0m'           # end underline
setenv LESS_TERMCAP_us \e'[04;38;5;146m' # begin underline

function sudo-commandline -d "Insert sudo at the beginning of the command line"
    set -l cmdline (commandline)
    commandline -r -- "sudo $cmdline"
end

# Thanks https://swsnr.de/blog/2018/10/05/sudo-fish/
function fish_user_key_bindings
    fish_default_key_bindings

    # Prepend sudo to the command line with M-s
    bind \es sudo-commandline
end

eval (python -m virtualfish)

function is_status_okay
    [ $status = 0 ]
end

# https://fishshell.com/docs/current/index.html#variables-color
#set fish_color_command purple
#set fish_color_param blue

function fish_prompt
  if set -q SSH_TTY
    set_color yellow
    echo -n "@"(hostname)":"
  end

  set_color white
  echo -n (prompt_pwd)

  set_color brown
  printf '%s' (__fish_git_prompt ':%s')

  if set -q VIRTUAL_ENV
    set_color blue
    echo -n ":"(basename "$VIRTUAL_ENV")
    set_color normal
  end

  if is_status_okay
      set_color green
  else
      set_color red
  end
  echo -n " » "
  #
end

function fish_right_prompt
end

function fish_greeting
  echo
  echo -e (uname -ro | awk '{print " \\\\e[1mOS: \\\\e[0;32m"$0"\\\\e[0m"}')
  echo -e (uptime -p | sed 's/^up //' | awk '{print " \\\\e[1mUptime: \\\\e[0;32m"$0"\\\\e[0m"}')
  echo -e (uname -n | awk '{print " \\\\e[1mHostname: \\\\e[0;32m"$0"\\\\e[0m"}')
  echo -e " \\e[1mNetwork:\\e[0m"
  echo
  # http://tdt.rocks/linux_network_interface_naming.html
  echo -ne (\
    ip addr show up scope global | \
      grep -E ': <|inet' | \
      sed \
        -e 's/^[[:digit:]]\+: //' \
        -e 's/: <.*//' \
        -e 's/.*inet[[:digit:]]* //' \
        -e 's/\/.*//'| \
      awk 'BEGIN {i=""} /\.|:/ {print i" "$0"\\\n"; next} // {i = $0}' | \
      sort | \
      column -t -R1 | \
      # public addresses are underlined for visibility \
      sed 's/ \([^ ]\+\)$/ \\\e[4m\1/' | \
      # private addresses are not \
      sed 's/m\(\(10\.\|172\.\(1[6-9]\|2[0-9]\|3[01]\)\|192\.168\.\).*\)/m\\\e[24m\1/' | \
      # unknown interfaces are cyan \
      sed 's/^\( *[^ ]\+\)/\\\e[36m\1/' | \
      # ethernet interfaces are normal \
      sed 's/\(\(en\|em\|eth\)[^ ]* .*\)/\\\e[39m\1/' | \
      # wireless interfaces are purple \
      sed 's/\(wl[^ ]* .*\)/\\\e[35m\1/' | \
      # wwan interfaces are yellow \
      sed 's/\(ww[^ ]* .*\).*/\\\e[33m\1/' | \
      sed 's/$/\\\e[0m/' | \
      sed 's/^/\t/' \
    )
  echo

  set_color normal
end

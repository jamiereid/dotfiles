# fix rustup not adding path for fish (https://github.com/rust-lang/rustup.rs/issues/478)

# check if brew is installed, if so, we need to do some extra things before we continue
if test -e /opt/homebrew/bin/brew
  eval (/opt/homebrew/bin/brew shellenv)
end

set -gx EDITOR vim
set -gx LANG en_US.UTF-8

set -gx GOPATH ~/.local/share/go/
set -gx GOBIN ~/.local/share/go/bin
set -gx PATH ~/.local/bin ~/.cargo/bin $GOPATH/bin $PATH


if command -v bob > /dev/null
    set -gx PATH ~/.local/share/bob/nvim-bin $PATH
end

set -gx NOTES_DIR ~/n

set -gx FZF_DEFAULT_COMMAND 'fd --type file --follow'
set -gx FZF_CTRL_T_COMMAND 'fd --type file --follow'
set -gx FZF_DEFAULT_OPTS '--height 20%'

set fish_greeting

# auto load tmux unless we're already in tmux, on tty1, or in vscode
if status --is-interactive; \
and not string match -q "tmux*" $TERM; \
and not string match -eq "/dev/tty1" (tty); \
and not string length -q $VSCODE_GIT_ASKPASS_NODE; \
and not string match -eq "sway" $XDG_SESSION_DESKTOP
	tmux new-session -A -s main 2> /dev/null; and exec true
end

abbr -a gs 'git status'
abbr -a gc 'git commit -m'
abbr -a ga 'git add -p'
abbr -a gl 'git lg'
abbr -a gd 'git diff'
abbr -a gw 'git worktree'
abbr -a gwa 'git worktree add'
abbr -a gwl 'git worktree list'
abbr -a gwr 'git worktree remove'
abbr -a httpserver 'python -m http.server 8000'

if status --is-interactive
	if command -v exa > /dev/null
		printf "%s\n" "exa is installed; you should switch to eza"
	end

	if command -v eza > /dev/null
		abbr -a ls 'eza'
		abbr -a ll 'eza -la'
	else
		abbr -a ll 'ls -la'
		printf "%s\n" "exa not installed"
	end

	if command -v zoxide > /dev/null
		zoxide init fish | source
		abbr -a cd 'z'
	else
		printf "%s\n" "zoxide not installed"
	end

	if command -v nvim > /dev/null
		abbr -a vi 'nvim'
		abbr -a vim 'nvim'
	else
		abbr -a vi 'vim'
		printf "%s\n" "vim not installed"
	end

	if command -v drill > /dev/null
		abbr -a dig 'drill'
	else
		printf "%s\n" "drill not installed"
	end

	# Debian packages bat with the binary and manpages renamed to batcat
	if command -v batcat > /dev/null
		abbr -a cat 'batcat'
	else if command -v bat > /dev/null
		abbr -a cat 'bat'
	else
		printf "%s\n" "bat not installed"
	end

	if command -v rg > /dev/null
		abbr -a grep 'rg'
	else
		printf "%s\n" "rg not installed"
	end

	if command -v mise > /dev/null
		mise activate fish | source
	else
		printf "%s\n" "mise not installed"
	end
end

set -g fish_prompt_pwd_dir_length 3
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

source ~/.config/fish/naysayer-theme.fish

function bind_bang
    switch (commandline -t)[-1]
        case "!"
            commandline -t -- $history[1]
            commandline -f repaint
        case "*"
            commandline -i !
    end
end

function bind_dollar
    switch (commandline -t)[-1]
        case "!"
            commandline -f backward-delete-char history-token-search-backward
        case "*"
            commandline -i '$'
    end
end

function fish_user_key_bindings
    bind ! bind_bang
    bind '$' bind_dollar
end

set __fish_prompt_lastchar '$'

if test -e ~/.config/fish/(uname -n|cut -d. -f1).fish
    source ~/.config/fish/(uname -n|cut -d. -f1).fish
end

if test -e ~/.config/fish/local.fish
    source ~/.config/fish/local.fish
end



function fish_prompt
    printf '%s %s %s%s ' (uname -n|cut -d. -f1) (prompt_pwd) (__fish_git_prompt '[%s] ') "$__fish_prompt_lastchar"
end

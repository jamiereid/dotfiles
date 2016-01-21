# fix for OS X - make sure to brew install coreutils
PATH="/usr/local/opt/coreutils/libexec/gnubin:$PATH"
# determine path to dots dir
DOTSPATH="$(cd $(dirname $(dirname $(readlink -f ${(%):-%N}))); pwd)"

fpath=(
  "$DOTSPATH/zsh/zsh/comp"
  "${fpath[@]}"
)

autoload -U compinit promptinit colors select-word-style
select-word-style bash
compinit -i
promptinit
colors

# history
setopt hist_ignore_space
setopt append_history
setopt hist_ignore_dups
setopt share_history
setopt extendedglob

# env vars
export EDITOR=vim
export VISUAL=vim
set -o emacs

# bundles
if [[ ! -d $DOTSPATH/zsh/zsh/antigen ]]; then
  git clone https://github.com/zsh-users/antigen.git $DOTSPATH/zsh/zsh/antigen
fi

source $DOTSPATH/zsh/zsh/antigen/antigen.zsh

# antigen
antigen bundles <<EOBUNDLES
  zsh-users/zsh-syntax-highlighting
  zsh-users/zsh-history-substring-search
  zsh-users/zsh-completions src
EOBUNDLES

antigen apply

# strict control over source order
sources=(
  'hub'
  'path'
  'base16-shell'
  'vcsinfo'
  'prompt'
  'completions'
  'highlight'
  'history'
  'functions'
  'alias'
  'linux'
  'gtags'
)

for src in $sources; do
  source $DOTSPATH/zsh/zsh/$src.zsh
done

if [[ -e $DOTSPATH/zsh/zsh/custom.zsh ]]; then
  source $DOTSPATH/zsh/zsh/custom.zsh
fi

if [[ -e ~/.zsh.local ]]; then
    source ~/.zsh.local
fi

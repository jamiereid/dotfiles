typeset -U path

# prepends
#path=(~/.rbenv/bin "$path[@]")

# appends
path+=(~/bin)
path+=($DOTSPATH/bin)

# prune paths that don't exist
path=($^path(N))

export GOPATH="~/Code/go"

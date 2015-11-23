typeset -U path

# prepends
#path=(~/.rbenv/bin "$path[@]")

# appends
path+=(~/bin)
path+=($DOTSPATH/bin)

# prune paths that don't exist
path=($^path(N))

if [[ $OSTYPE == 'cygwin' ]]; then
	export GOPATH="/cygdrive/c/Go/"
else
	export GOPATH="~/Code/go"
fi

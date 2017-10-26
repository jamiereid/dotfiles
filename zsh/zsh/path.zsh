typeset -U path

# prepends
#path=(~/.rbenv/bin "$path[@]")

if [[ $OSTYPE == 'cygwin' ]]; then
	export GOPATH="/cygdrive/c/Go/"
else
	export GOPATH="$HOME/Code/go"
fi

# appends
path+=(~/bin)
path+=($DOTSPATH/bin)
path+=($GOPATH/bin)

# prune paths that don't exist
path=($^path(N))

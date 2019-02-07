# open man page and jump to specific option
# $ manf ls -l
function manf() {
  man -P "less -p \"^ +$2\"" $1
}

# open man page and jump to examples section
function eg(){
  man -P "less -p \"^EXAMPLES?\"" $1
}

# find the zsh file that backs a command
# $ funcpath ls
# /usr/share/zsh/functions/Completion/Unix/_ls
function funcpath() {
  echo ${^fpath}/_${1}(N)
}

# label the current window/tab
function label() {
  print -Pn "\e]2;$1\a"
}

# print numerical permissions before each item in ls
function lsp() {
  command ls -lh --time-style '+%m/%d/%y %I:%M %p' --color=always $@ |\
    awk '{k=0;for(i=0;i<=8;i++)k+=((substr($1,i+2,1)~/[rwx]/)\
         *2^(8-i));if(k)printf("%0o ",k);print}'
}

# move back arbitrary number of directories
# $ cd b...
# $ cd ../../../
function cd() {
  emulate -LR zsh

  if [[ $1 == 'b.'* ]]; then
    builtin cd ${${1/"b"}//"."/"../"}
  else
    builtin cd $*
  fi
}

# what is my ip?
# $ ip get
#   copied <ip> to clipboard
function ip() {
  emulate -LR zsh

  if [[ $1 == 'get' ]]; then
    res=$(curl -s ipinfo.io/ip)
    echo -n $res | xsel --clipboard
    echo "copied $res to clipboard"
  # only run ip if it exists
  elif (( $+commands[ip] )); then
    command ip $*
  fi
}

# go to the dotfiles directory
function go_dots() {
  emulate -LR zsh
  cd $DOTSPATH
}

# deploy the dotfiles
function put_dots() {
  emulate -LR zsh

  msg_info "deploying dots from $DOTSPATH"
  msg_info "help: "\
"$(tput bold)b$(tput sgr0)ackup, "\
"$(tput bold)o$(tput sgr0)verwrite, "\
"$(tput bold)r$(tput sgr0)emove, "\
"$(tput bold)s$(tput sgr0)kip\n"\
"          capitalize to apply to all remaining\n"

  overwrite_all=false
  backup_all=false
  skip_all=false
  remove_all=false

  for src in `find "$DOTSPATH" -mindepth 2 -maxdepth 2  -name .\* ! -path "$DOTSPATH/.git*"`; do
    dest="$HOME/`basename \"$src\"`"

    if [[ -e $dest ]] || [[ -L $dest ]]; then
      overwrite=false
      backup=false
      skip=false
      remove=false
      fname="$(tput bold)`basename $dest`$(tput sgr0)"

      if [[ "$overwrite_all" == "false" ]] &&\
         [[ "$backup_all" == "false" ]] &&\
         [[ "$remove_all" == "false" ]] &&\
         [[ "$skip_all" == "false" ]]; then
        if [[ ! -L $dest ]]; then
          msg_user "$fname exists non-linked:"
        else
          link=`readlink -mn "$dest"`
          msg_user "$fname is already linked to $link:"
        fi

        read -k 1 action

        case "$action" in
          o )
            overwrite=true;;
          O )
            overwrite_all=true;;
          b )
            backup=true;;
          B )
            backup_all=true;;
          s )
            skip=true;;
          S )
            skip_all=true;;
          r )
            remove=true;;
          R )
            remove_all=true;;
          * )
            ;;
        esac
      fi

      if [[ "$skip" == "false" ]] && [[ "$skip_all" == "false" ]]; then
        if [[ "$overwrite" == "true" ]] || [[ "$overwrite_all" == "true" ]] ||\
           [[ "$remove" == "true" ]] || [[ "$remove_all" == "true" ]]; then
          rm -rf $dest
          msg_fail "removed $fname"
        fi

        if [[ "$backup" == "true" ]] || [[ "$backup_all" == "true" ]]; then
          mv $dest{,.bak}
          msg_success "moved $fname to $fname.bak"
        fi

        if [[ "$overwrite" == "true" ]] || [[ "$overwrite_all" == "true" ]] ||\
           [[ "$backup" == "true" ]] || [[ "$backup_all" == "true" ]]; then
          link_files $src $dest
        fi
      else
        msg_info "skipped $fname"
      fi

    else
      link_files $src $dest
    fi
  done
}

# message functions
function tput_msg() {
  printf "\r$(tput el)  $(tput setaf $1)$2$(tput sgr0) $3\n"
}

function msg_info() {
  printf "\r$(tput el)  $(tput setaf 4)·$(tput sgr0) $1\n"
  # tput_msg "4" "·" $1
}

function msg_success() {
  printf "\r$(tput el)  $(tput setaf 2)+$(tput sgr0) $1\n"
  # tput_msg "2" "+" $1
}

function msg_fail() {
  printf "\r$(tput el)  $(tput setaf 1)-$(tput sgr0) $1\n"
  # tput_msg "1" "-" $1
}

function msg_user() {
  printf "\r  $(tput setaf 5)?$(tput sgr0) $1 "
}

function link_files() {
  ln -s $1 $2
  msg_success "linked $1 $(tput setaf 2)→$(tput sgr0) $2"
}

# update and deploy dots
function dots() {
  emulate -LR zsh

  echo ''

  case "$1" in
    put )
      put_dots;;
    go )
      go_dots;;
    * )
      msg_user "use the 'put' or 'go' commands"
      echo ''
      ;;
  esac
}


# helper function to extract any file type (http://git.sysphere.org/dotfiles/tree/zshrc)
function extract () {
    if [[ -f "$1" ]]; then
        case "$1" in
            *.tbz2 | *.tar.bz2) tar -xvjf  "$1"     ;;
            *.txz | *.tar.xz)   tar -xvJf  "$1"     ;;
            *.tgz | *.tar.gz)   tar -xvzf  "$1"     ;;
            *.tar | *.cbt)      tar -xvf   "$1"     ;;
            *.zip | *.cbz)      unzip      "$1"     ;;
            *.rar | *.cbr)      unrar x    "$1"     ;;
            *.arj)              unarj x    "$1"     ;;
            *.ace)              unace x    "$1"     ;;
            *.bz2)              bunzip2    "$1"     ;;
            *.xz)               unxz       "$1"     ;;
            *.gz)               gunzip     "$1"     ;;
            *.7z)               7z x       "$1"     ;;
            *.Z)                uncompress "$1"     ;;
            *.gpg)       gpg2 -d "$1" | tar -xvzf - ;;
            *) echo "Error: failed to extract $1" ;;
        esac
    else
        echo "Error: $1 is not a valid file for extraction"
    fi
    
}

_has() {
    return $( whence $1 >/dev/null )
}

# get windows remote hostname by reading the cert
function rdphostname () {
    echo '\n' | openssl s_client -host $1 -port 3389 2> /dev/null | grep 'subject=' | sed s/subject=\\/CN\=//g
}

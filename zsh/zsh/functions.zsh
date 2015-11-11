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

# print colors
# $ clist 16
function clist(){
  x=`tput op`
  y=`printf %76s`
  for i in {0..$1}
  do
    o=00$i
    echo -e ${o:${#o}-3:3} `tput setaf $i;tput setab $i`${y// /=}$x
  done
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

# open alias for xdg-open
# it ignores stdout and stderr
# pass-through for os x

function open() {
  emulate -LR zsh

  # linux
  if (( $+commands[xdg-open] )); then
    xdg-open $* > /dev/null 2>&1
  # mac
  elif (( $+commands[open] )); then
    open $*
  fi
}

# go to the dotfiles directory

function go_dots() {
  emulate -LR zsh
  cd $DOTSPATH
}

  emulate -LR zsh

  pushd $DOTSPATH > /dev/null

  pre=$(git log -1 HEAD --pretty=format:%h)

  msg_info "checking for updates since $pre"

  if git pull > /dev/null 2>&1; then
    post=$(git log -1 HEAD --pretty=format:%h)

    if [[ "$pre" == "$post" ]]; then
      msg_info "no updates available"
    else
      msg_info "updated to $post\n"
      git log --oneline --format='  %C(green)+%Creset %C(bold)%h%Creset %s' $pre..HEAD
    fi
  else
    msg_fail "there was an error with updating"
  fi

  # msg_info "updating vim plugins"
  # vim +PluginInstall +qall

  popd > /dev/null

  msg_info "reloading zsh"
  exec zsh
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
    get )
      get_dots;;
    go )
      go_dots;;
    * )
      msg_user "use the 'get' or 'put' commands"
      echo ''
      ;;
  esac
}

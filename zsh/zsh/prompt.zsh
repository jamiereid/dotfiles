# prompt
setopt prompt_subst

# colored path

function p_colored_path {
  local slash="%F{cyan}/%f"
  echo "${${PWD/#$HOME/~}//\//$slash}"
}

# git info
zstyle ':vcs_info:*' stagedstr '%F{green}●'
zstyle ':vcs_info:*' unstagedstr '%F{yellow}●'
zstyle ':vcs_info:*' check-for-changes true
zstyle ':vcs_info:(sv[nk]|bzr):*' branchformat '%b%F{1}:%F{11}%r'
zstyle ':vcs_info:*' enable git svn

function p_vcs {
    if [[ -z $(git ls-files --other --exclude-standard 2> /dev/null) ]] {
        zstyle ':vcs_info:*' formats ' %F{cyan}(%b%c%u%F{cyan})%f'
    } else {
        zstyle ':vcs_info:*' formats ' %F{cyan}(%b%c%u%F{red}●%F{green}%F{cyan})%f'
    }
    vcs_info
    echo $vcs_info_msg_0_
}

# environments:
#  - ssh
#  - virtualenv

export VIRTUAL_ENV_DISABLE_PROMPT=1

function p_envs {
  local envs
  [[ -n $VIRTUAL_ENV ]] && envs+="P"
  [[ -n $envs ]] && echo " %F{blue}[%f$envs%F{blue}]%f"
}

function p_remote {
  [[ -n $SSH_CLIENT ]] && echo "%F{8}@%m%f"
}

PROMPT='
%F{cyan}%n%f$(p_remote) $(p_colored_path)$(p_envs)$(p_vcs)
%(?.%F{cyan}.%F{red})»%f '

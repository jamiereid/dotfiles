# prompt
setopt prompt_subst

# colored path

function p_colored_path {
  local slash="%F{cyan}/%f"
  echo "${${PWD/#$HOME/~}//\//$slash}"
}

# git info

function p_vcs {
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
  [[ -n $SSH_CLIENT ]] && echo "%F{grey}@%m%f"
}

PROMPT='
%(?.%F{green}.%F{red})λ%f %F{cyan}%n%f$(p_remote):$(p_colored_path)$(p_envs)$(p_vcs)
%(?.%F{cyan}.%F{red})»%f '

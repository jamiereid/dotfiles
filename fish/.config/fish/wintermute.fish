if status --is-interactive
    set __fish_prompt_lastchar '::'
    set -g -x JRR_THEME naysayer
end

set -gx PATH ~/jai/bin $PATH
abbr -a jai 'jai-linux'

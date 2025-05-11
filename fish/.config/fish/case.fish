if status --is-interactive
	set __fish_prompt_lastchar '::'
	set -g -x JRR_WEZ_RESIZE false
	set -g -x JRR_THEME acme
end
cd $HOME

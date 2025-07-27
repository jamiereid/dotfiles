#!/usr/bin/env bash
set -euo pipefail

STATUS_KEY="BackendState"
RUNNING="Running"

tailscale_status() {
	local status
	status="$(tailscale status --json | jq -r ".${STATUS_KEY}")"
	[[ "$status" == "$RUNNING" ]]
}

toggle_status() {
	if tailscale_status; then
		tailscale down
	else
		tailscale up
	fi
	sleep 5
}

case "${1:-}" in
--status)
	if tailscale_status; then
		T=${2:-"green"}
		F=${3:-"red"}

		# Build tooltip HTML and escape it for JSON
		raw_peers=$(tailscale status --json | jq -r \
			--arg T "$T" \
			--arg F "$F" \
			'.Peer[] | "<span color=\"" + (if .Online then $T else $F end) + "\">" + (.DNSName | split(".")[0]) + "</span>"')

		peers=$(echo "$raw_peers")

		# Get optional exit node
		exitnode=$(tailscale status --json | jq -r \
			'.Peer[] | select(.ExitNode == true).DNSName | split(".")[0]' || true)
		status_text="${exitnode:-Tailscale}"

		# Safely emit valid JSON using jq
		jq -n -c \
			--arg text "$status_text" \
			--arg tooltip "$peers" \
			'{text: $text, class: "connected", alt: "connected", tooltip: $tooltip}'
	else
		jq -n -c \
			'{"text": "", "class": "stopped", "alt": "stopped", "tooltip": "The VPN is not active."}'
	fi
	;;
--toggle)
	toggle_status
	;;
*)
	echo "Usage: $0 --status [on-color] [off-color] | --toggle"
	exit 1
	;;
esac

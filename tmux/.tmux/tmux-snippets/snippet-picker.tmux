#!/usr/bin/env bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

# Get the configured keybinding (default: prefix + s)
snippet_key=$(tmux show-option -gqv "@snippet-picker-key")
snippet_key=${snippet_key:-s}

# Bind the key to open the snippet picker popup
tmux bind-key "$snippet_key" run-shell "tmux display-popup -E -w 90% -h 40% \"$CURRENT_DIR/scripts/snippet-picker.sh '#{pane_id}'\""

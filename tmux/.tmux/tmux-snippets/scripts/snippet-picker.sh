#!/usr/bin/env bash

CURRENT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SNIPPETS_FILE="${SNIPPETS_FILE:-$HOME/.tmux-snippets.yaml}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[0;33m'
BLUE='\033[0;34m'
NC='\033[0m'

# Parse YAML and format for fzf (name⟪DELIM⟫description⟪DELIM⟫category⟪DELIM⟫tags⟪DELIM⟫language⟪DELIM⟫content)
# Using ⟪DELIM⟫ as delimiter to avoid conflicts with pipe characters in content
DELIM="⟪DELIM⟫"

parse_snippets() {
  awk -v delim="$DELIM" '
    BEGIN { 
        name=""; description=""; category=""; tags=""; language="bash"; content=""; in_content=0 
    }
    
    /^[[:space:]]*-[[:space:]]*name:/ {
        # Print previous snippet if exists
        if (name != "") {
            print name delim description delim category delim tags delim language delim content
        }
        # Reset for new snippet
        name=""; description=""; category=""; tags=""; language="bash"; content=""; in_content=0
        # Extract name
        gsub(/^[[:space:]]*-[[:space:]]*name:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        name = $0
        next
    }
    
    /^[[:space:]]*description:/ {
        gsub(/^[[:space:]]*description:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        description = $0
        next
    }
    
    /^[[:space:]]*category:/ {
        gsub(/^[[:space:]]*category:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        category = $0
        next
    }
    
    /^[[:space:]]*tags:/ {
        gsub(/^[[:space:]]*tags:[[:space:]]*/, "")
        gsub(/^\[|\]$/, "")
        gsub(/[[:space:]]*,[[:space:]]*/, ",")
        tags = $0
        next
    }
    
    /^[[:space:]]*language:/ {
        gsub(/^[[:space:]]*language:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        language = $0
        next
    }
    
    /^[[:space:]]*content:[[:space:]]*[|>]/ {
        in_content = 1
        next
    }
    
    in_content {
        # Check if we hit the next snippet or end of content
        if (/^[[:space:]]*-[[:space:]]*name:/ || (/^[[:space:]]*$/ && length($0) < 2)) {
            in_content = 0
            # Print current snippet
            if (name != "") {
                print name delim description delim category delim tags delim language delim content
            }
            # If new snippet starting, process it
            if (/^[[:space:]]*-[[:space:]]*name:/) {
                name=""; description=""; category=""; tags=""; language="bash"; content=""
                gsub(/^[[:space:]]*-[[:space:]]*name:[[:space:]]*/, "")
                gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
                name = $0
            }
        } else {
            # Add line to content
            line = $0
            gsub(/^[[:space:]]*/, "", line)
            if (content != "") content = content "\n"
            content = content line
        }
    }
    
    END {
        # Print last snippet
        if (name != "") {
            print name delim description delim category delim tags delim language delim content
        }
    }
    ' "$SNIPPETS_FILE"
}

# Format snippet for display in fzf
format_for_display() {
  local snippets="$1"
  echo "$snippets" | awk -F"$DELIM" '{
        name = $1
        desc = $2
        category = $3
        tags = $4
        language = $5
        content = $6
        
        # Format category
        cat_display = ""
        if (category != "") {
            cat_display = "[" category "] "
        }
        
        # Format tags
        tag_display = ""
        if (tags != "") {
            tag_display = " {" tags "}"
        }
        
        # Create display string
        display = cat_display name " - " desc tag_display
        
        # Output: display⟪DELIM⟫name⟪DELIM⟫desc⟪DELIM⟫category⟪DELIM⟫tags⟪DELIM⟫language⟪DELIM⟫content
        print display "⟪DELIM⟫" name "⟪DELIM⟫" desc "⟪DELIM⟫" category "⟪DELIM⟫" tags "⟪DELIM⟫" language "⟪DELIM⟫" content
    }'
}

# Get unique categories from snippets
get_categories() {
  echo "$1" | awk -F"$DELIM" '{if ($3 != "") print $3}' | sort -u
}

# Get unique tags from snippets
get_tags() {
  echo "$1" | awk -F"$DELIM" '{
        if ($4 != "") {
            split($4, arr, ",")
            for (i in arr) {
                gsub(/^[[:space:]]+|[[:space:]]+$/, "", arr[i])
                print arr[i]
            }
        }
    }' | sort -u
}

# Filter snippets by category
filter_by_category() {
  local snippets="$1"
  local category="$2"
  echo "$snippets" | awk -F"$DELIM" -v cat="$category" '{if ($3 == cat || cat == "all") print}'
}

# Filter snippets by tag
filter_by_tag() {
  local snippets="$1"
  local tag="$2"
  echo "$snippets" | awk -F"$DELIM" -v tag="$tag" '{if (index($4, tag) > 0 || tag == "all") print}'
}

# Main function
main() {
  # Check dependencies
  for cmd in fzf bat; do
    if ! command -v "$cmd" &>/dev/null; then
      echo -e "${RED}Error: $cmd is not installed${NC}"
      echo "Install with: brew install $cmd  (or your package manager)"
      sleep 2
      exit 1
    fi
  done

  if [[ ! -f "$SNIPPETS_FILE" ]]; then
    echo -e "${RED}Snippets file not found: $SNIPPETS_FILE${NC}"
    echo "Create a snippets file at $SNIPPETS_FILE"
    sleep 2
    exit 1
  fi

  # Get the target pane (passed from tmux)
  TARGET_PANE="$1"

  # Parse snippets
  SNIPPETS=$(parse_snippets)

  if [[ -z "$SNIPPETS" ]]; then
    echo -e "${RED}No snippets found in $SNIPPETS_FILE${NC}"
    sleep 2
    exit 1
  fi

  # Get categories and tags for header
  CATEGORIES=$(get_categories "$SNIPPETS")
  TAGS=$(get_tags "$SNIPPETS")

  # Build header with available filters
  HEADER="↑↓: Navigate | ENTER: Insert | ESC: Cancel"
  HEADER="$HEADER\nCtrl-C: Filter by category | Ctrl-T: Filter by tag"

  # Format snippets for display
  FORMATTED=$(format_for_display "$SNIPPETS")

  # Create temporary directory for filter state
  TMP_DIR=$(mktemp -d)
  trap "rm -rf $TMP_DIR" EXIT

  # Create a helper script for reloading (re-parses YAML each time)
  RELOAD_SCRIPT="$TMP_DIR/reload.sh"
  cat >"$RELOAD_SCRIPT" <<'RELOAD_EOF'
#!/bin/bash
SNIPPETS_FILE="$1"
FILTER_TYPE="$2"
FILTER_VALUE="$3"
DELIM="⟪DELIM⟫"

parse_snippets() {
    awk -v delim="$DELIM" '
    BEGIN { 
        name=""; description=""; category=""; tags=""; language="bash"; content=""; in_content=0 
    }
    /^[[:space:]]*-[[:space:]]*name:/ {
        if (name != "") {
            print name delim description delim category delim tags delim language delim content
        }
        name=""; description=""; category=""; tags=""; language="bash"; content=""; in_content=0
        gsub(/^[[:space:]]*-[[:space:]]*name:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        name = $0
        next
    }
    /^[[:space:]]*description:/ {
        gsub(/^[[:space:]]*description:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        description = $0
        next
    }
    /^[[:space:]]*category:/ {
        gsub(/^[[:space:]]*category:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        category = $0
        next
    }
    /^[[:space:]]*tags:/ {
        gsub(/^[[:space:]]*tags:[[:space:]]*/, "")
        gsub(/^\[|\]$/, "")
        gsub(/[[:space:]]*,[[:space:]]*/, ",")
        tags = $0
        next
    }
    /^[[:space:]]*language:/ {
        gsub(/^[[:space:]]*language:[[:space:]]*/, "")
        gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
        language = $0
        next
    }
    /^[[:space:]]*content:[[:space:]]*[|>]/ {
        in_content = 1
        next
    }
    in_content {
        if (/^[[:space:]]*-[[:space:]]*name:/ || (/^[[:space:]]*$/ && length($0) < 2)) {
            in_content = 0
            if (name != "") {
                print name delim description delim category delim tags delim language delim content
            }
            if (/^[[:space:]]*-[[:space:]]*name:/) {
                name=""; description=""; category=""; tags=""; language="bash"; content=""
                gsub(/^[[:space:]]*-[[:space:]]*name:[[:space:]]*/, "")
                gsub(/^["'\''"]|["'\''"[[:space:]]*$/, "")
                name = $0
            }
        } else {
            line = $0
            gsub(/^[[:space:]]*/, "", line)
            if (content != "") content = content "\n"
            content = content line
        }
    }
    END {
        if (name != "") {
            print name delim description delim category delim tags delim language delim content
        }
    }
    ' "$SNIPPETS_FILE"
}

PARSED=$(parse_snippets)

# Apply filter
if [ "$FILTER_TYPE" = "category" ] && [ "$FILTER_VALUE" != "all" ]; then
    PARSED=$(echo "$PARSED" | awk -F"$DELIM" -v cat="$FILTER_VALUE" '{if ($3 == cat) print}')
elif [ "$FILTER_TYPE" = "tag" ] && [ "$FILTER_VALUE" != "all" ]; then
    PARSED=$(echo "$PARSED" | awk -F"$DELIM" -v tag="$FILTER_VALUE" '{if (index($4, tag) > 0) print}')
fi

# Format for display
echo "$PARSED" | awk -F"$DELIM" '{
    name = $1; desc = $2; category = $3; tags = $4; language = $5; content = $6
    cat_display = ""; if(category!=""){cat_display="["category"] "}
    tag_display = ""; if(tags!=""){tag_display=" {"tags"}"}
    display = cat_display name " - " desc tag_display
    print display "⟪DELIM⟫" name "⟪DELIM⟫" desc "⟪DELIM⟫" category "⟪DELIM⟫" tags "⟪DELIM⟫" language "⟪DELIM⟫" content
}'
RELOAD_EOF
  chmod +x "$RELOAD_SCRIPT"

  # Select snippet with fzf
  SELECTED=$(echo "$FORMATTED" \
    | fzf --delimiter="$DELIM" \
      --with-nth=1 \
      --preview='echo {7} | bat --style=numbers --color=always --language={6} --plain' \
      --preview-window='right:60%:wrap' \
      --prompt='Snippet: ' \
      --header="$HEADER" \
      --border=rounded \
      --height=100% \
      --ansi \
      --bind "ctrl-c:execute-silent(echo {} | cut -d'⟪' -f4 | cut -d'⟫' -f2 | sed 's/^$/all/' > $TMP_DIR/cat)+reload($RELOAD_SCRIPT '$SNIPPETS_FILE' category \$(cat $TMP_DIR/cat 2>/dev/null || echo all))+change-prompt(Category filter> )" \
      --bind "ctrl-t:execute-silent(echo {} | cut -d'⟪' -f5 | cut -d'⟫' -f2 | tr ',' '\n' | head -1 | sed 's/^$/all/' > $TMP_DIR/tag)+reload($RELOAD_SCRIPT '$SNIPPETS_FILE' tag \$(cat $TMP_DIR/tag 2>/dev/null || echo all))+change-prompt(Tag filter> )")

  if [[ -n "$SELECTED" ]]; then
    # Extract content (field 7) - need to handle the delimiter properly
    CONTENT=$(echo "$SELECTED" | awk -F"$DELIM" '{print $7}')

    # Substitute variables
    CONTENT=$(substitute_variables "$CONTENT")

    # Send to target pane
    tmux send-keys -t "$TARGET_PANE" "$CONTENT"
  fi
}

# Substitute variables in snippet content
substitute_variables() {
  local content="$1"

  # Get current date and time
  local date=$(date +%Y-%m-%d)
  local time=$(date +%H:%M:%S)
  local datetime=$(date +"%Y-%m-%d %H:%M:%S")
  local timestamp=$(date +%s)
  local year=$(date +%Y)
  local month=$(date +%m)
  local day=$(date +%d)

  # Get clipboard content (try multiple clipboard commands)
  local clipboard=""
  if command -v xclip &>/dev/null; then
    clipboard=$(xclip -selection clipboard -o 2>/dev/null || echo "")
  elif command -v pbpaste &>/dev/null; then
    clipboard=$(pbpaste 2>/dev/null || echo "")
  elif command -v wl-paste &>/dev/null; then
    clipboard=$(wl-paste 2>/dev/null || echo "")
  elif command -v tmux &>/dev/null; then
    clipboard=$(tmux show-buffer 2>/dev/null || echo "")
  fi

  # Get username and hostname
  local username=$(whoami)
  local hostname=$(hostname)

  # Perform substitutions
  content="${content//\{DATE\}/$date}"
  content="${content//\{TIME\}/$time}"
  content="${content//\{DATETIME\}/$datetime}"
  content="${content//\{TIMESTAMP\}/$timestamp}"
  content="${content//\{YEAR\}/$year}"
  content="${content//\{MONTH\}/$month}"
  content="${content//\{DAY\}/$day}"
  content="${content//\{CLIPBOARD\}/$clipboard}"
  content="${content//\{USER\}/$username}"
  content="${content//\{HOSTNAME\}/$hostname}"

  echo "$content"
}

main "$@"

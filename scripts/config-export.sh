#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Export cloud-sync-tuner configuration to portable format

set -euo pipefail

CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/cloud-sync-tuner"
OUTPUT="${1:-cloud-sync-config-$(date +%Y%m%d).tar.gz}"

# Files to export
FILES=(
    "config.toml"
    "wg0.conf"
    "cicada-identity.pub"
)

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Copy existing files
for file in "${FILES[@]}"; do
    if [[ -f "$CONFIG_DIR/$file" ]]; then
        cp "$CONFIG_DIR/$file" "$TEMP_DIR/"
        echo "Exported: $file"
    fi
done

# Export rclone config (sanitized - no tokens)
if command -v rclone &>/dev/null; then
    rclone config dump | jq 'del(.[].token)' > "$TEMP_DIR/rclone-remotes.json"
    echo "Exported: rclone-remotes.json (tokens removed)"
fi

# Export systemd service files
if [[ -d "$CONFIG_DIR/../systemd/user" ]]; then
    mkdir -p "$TEMP_DIR/systemd"
    cp "$CONFIG_DIR/../systemd/user/rclone-"*.service "$TEMP_DIR/systemd/" 2>/dev/null || true
    echo "Exported: systemd service files"
fi

# Create archive
tar -czf "$OUTPUT" -C "$TEMP_DIR" .
echo ""
echo "Configuration exported to: $OUTPUT"
echo ""
echo "Contents:"
tar -tzf "$OUTPUT" | sed 's/^/  /'
echo ""
echo "WARNING: Re-authenticate remotes after import (tokens not exported)"

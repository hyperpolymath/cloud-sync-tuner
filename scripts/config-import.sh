#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Import cloud-sync-tuner configuration from exported archive

set -euo pipefail

ARCHIVE="${1:-}"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/cloud-sync-tuner"
SYSTEMD_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user"

if [[ -z "$ARCHIVE" ]]; then
    echo "Usage: config-import.sh <archive.tar.gz>"
    exit 1
fi

if [[ ! -f "$ARCHIVE" ]]; then
    echo "Error: Archive not found: $ARCHIVE"
    exit 1
fi

# Create directories
mkdir -p "$CONFIG_DIR"
mkdir -p "$SYSTEMD_DIR"

# Create temp directory
TEMP_DIR=$(mktemp -d)
trap "rm -rf $TEMP_DIR" EXIT

# Extract archive
tar -xzf "$ARCHIVE" -C "$TEMP_DIR"

echo "Importing configuration from: $ARCHIVE"
echo ""

# Import main config
if [[ -f "$TEMP_DIR/config.toml" ]]; then
    cp "$TEMP_DIR/config.toml" "$CONFIG_DIR/"
    echo "Imported: config.toml"
fi

# Import WireGuard config
if [[ -f "$TEMP_DIR/wg0.conf" ]]; then
    cp "$TEMP_DIR/wg0.conf" "$CONFIG_DIR/"
    chmod 600 "$CONFIG_DIR/wg0.conf"
    echo "Imported: wg0.conf (permissions: 600)"
fi

# Import cicada public key
if [[ -f "$TEMP_DIR/cicada-identity.pub" ]]; then
    cp "$TEMP_DIR/cicada-identity.pub" "$CONFIG_DIR/"
    echo "Imported: cicada-identity.pub"
fi

# Import systemd services
if [[ -d "$TEMP_DIR/systemd" ]]; then
    cp "$TEMP_DIR/systemd/"*.service "$SYSTEMD_DIR/" 2>/dev/null || true
    echo "Imported: systemd service files"
    systemctl --user daemon-reload
    echo "Reloaded systemd user daemon"
fi

# Show rclone remotes that need re-auth
if [[ -f "$TEMP_DIR/rclone-remotes.json" ]]; then
    echo ""
    echo "Remotes that need re-authentication:"
    jq -r 'keys[]' "$TEMP_DIR/rclone-remotes.json" | sed 's/^/  - /'
    echo ""
    echo "Run 'rclone config reconnect <remote>:' for each"
fi

echo ""
echo "Import complete. Verify configuration:"
echo "  cat $CONFIG_DIR/config.toml"

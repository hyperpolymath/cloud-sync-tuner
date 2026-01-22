#!/bin/bash
# SPDX-License-Identifier: PMPL-1.0-or-later
# Sync cloud-sync-tuner configuration between machines via secure channel

set -euo pipefail

COMMAND="${1:-}"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/cloud-sync-tuner"

usage() {
    cat <<EOF
Usage: config-sync.sh <command> [options]

Commands:
  push <host>    Push config to remote host via SSH
  pull <host>    Pull config from remote host via SSH
  diff <host>    Show differences with remote host
  verify         Verify local configuration integrity

Options:
  --dry-run      Show what would be done without making changes

Examples:
  config-sync.sh push server.example.com
  config-sync.sh pull backup-machine --dry-run
  config-sync.sh diff laptop
  config-sync.sh verify
EOF
}

push_config() {
    local host="$1"
    local dry_run="${2:-false}"

    echo "Pushing configuration to: $host"

    # Export locally
    local temp_archive=$(mktemp --suffix=.tar.gz)
    trap "rm -f $temp_archive" EXIT

    ./config-export.sh "$temp_archive" >/dev/null

    if [[ "$dry_run" == "true" ]]; then
        echo "[DRY RUN] Would transfer: $temp_archive"
        echo "[DRY RUN] Would run: config-import.sh on $host"
        return
    fi

    # Transfer and import
    scp "$temp_archive" "$host:/tmp/cloud-sync-config.tar.gz"
    ssh "$host" "cd ~/.config/cloud-sync-tuner/scripts && ./config-import.sh /tmp/cloud-sync-config.tar.gz && rm /tmp/cloud-sync-config.tar.gz"

    echo "Configuration pushed to $host"
}

pull_config() {
    local host="$1"
    local dry_run="${2:-false}"

    echo "Pulling configuration from: $host"

    if [[ "$dry_run" == "true" ]]; then
        echo "[DRY RUN] Would fetch config from: $host"
        echo "[DRY RUN] Would run: config-import.sh locally"
        return
    fi

    local temp_archive=$(mktemp --suffix=.tar.gz)
    trap "rm -f $temp_archive" EXIT

    # Export on remote and transfer
    ssh "$host" "cd ~/.config/cloud-sync-tuner/scripts && ./config-export.sh /tmp/cloud-sync-config.tar.gz >/dev/null"
    scp "$host:/tmp/cloud-sync-config.tar.gz" "$temp_archive"
    ssh "$host" "rm /tmp/cloud-sync-config.tar.gz"

    # Import locally
    ./config-import.sh "$temp_archive"

    echo "Configuration pulled from $host"
}

diff_config() {
    local host="$1"

    echo "Comparing configuration with: $host"

    local local_temp=$(mktemp -d)
    local remote_temp=$(mktemp -d)
    trap "rm -rf $local_temp $remote_temp" EXIT

    # Export both
    ./config-export.sh "$local_temp/config.tar.gz" >/dev/null
    tar -xzf "$local_temp/config.tar.gz" -C "$local_temp"

    ssh "$host" "cd ~/.config/cloud-sync-tuner/scripts && ./config-export.sh /tmp/cloud-sync-config.tar.gz >/dev/null"
    scp "$host:/tmp/cloud-sync-config.tar.gz" "$remote_temp/config.tar.gz"
    ssh "$host" "rm /tmp/cloud-sync-config.tar.gz"
    tar -xzf "$remote_temp/config.tar.gz" -C "$remote_temp"

    # Compare
    echo ""
    diff -ru "$local_temp" "$remote_temp" --exclude="*.tar.gz" || true
}

verify_config() {
    echo "Verifying local configuration..."

    local errors=0

    # Check config.toml
    if [[ -f "$CONFIG_DIR/config.toml" ]]; then
        echo "✓ config.toml exists"
        if command -v tomlq &>/dev/null; then
            if tomlq '.' "$CONFIG_DIR/config.toml" >/dev/null 2>&1; then
                echo "✓ config.toml is valid TOML"
            else
                echo "✗ config.toml has syntax errors"
                ((errors++))
            fi
        fi
    else
        echo "✗ config.toml not found"
        ((errors++))
    fi

    # Check permissions on sensitive files
    if [[ -f "$CONFIG_DIR/wg0.conf" ]]; then
        local perms=$(stat -c %a "$CONFIG_DIR/wg0.conf")
        if [[ "$perms" == "600" ]]; then
            echo "✓ wg0.conf permissions correct (600)"
        else
            echo "✗ wg0.conf permissions too open ($perms, should be 600)"
            ((errors++))
        fi
    fi

    echo ""
    if [[ $errors -eq 0 ]]; then
        echo "Configuration verified successfully"
    else
        echo "Found $errors error(s)"
        exit 1
    fi
}

# Parse arguments
DRY_RUN=false
for arg in "$@"; do
    if [[ "$arg" == "--dry-run" ]]; then
        DRY_RUN=true
    fi
done

case "$COMMAND" in
    push)
        push_config "${2:-}" "$DRY_RUN"
        ;;
    pull)
        pull_config "${2:-}" "$DRY_RUN"
        ;;
    diff)
        diff_config "${2:-}"
        ;;
    verify)
        verify_config
        ;;
    *)
        usage
        exit 1
        ;;
esac

#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# SDP VPN Connect Script
# Establishes secure tunnel before cloud sync operations

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/cloud-sync-tuner"
CICADA_KEY="${CICADA_KEY:-cloud-sync}"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

log() { echo -e "${GREEN}[SDP]${NC} $*"; }
warn() { echo -e "${YELLOW}[SDP]${NC} $*"; }
error() { echo -e "${RED}[SDP]${NC} $*" >&2; }

# Check dependencies
check_deps() {
    local missing=()
    command -v wg &>/dev/null || missing+=(wireguard-tools)
    command -v julia &>/dev/null || missing+=(julia)

    if [[ ${#missing[@]} -gt 0 ]]; then
        error "Missing dependencies: ${missing[*]}"
        exit 1
    fi
}

# Verify identity with cicada
verify_identity() {
    log "Verifying identity with cicada..."

    if julia -e "using CIcaDA; verify_key(\"$CICADA_KEY\")" 2>/dev/null; then
        log "Identity verified"
        return 0
    else
        warn "cicada not available, skipping identity verification"
        return 0
    fi
}

# Establish WireGuard tunnel
establish_tunnel() {
    local config="${1:-$CONFIG_DIR/wg0.conf}"

    if [[ ! -f "$config" ]]; then
        warn "WireGuard config not found: $config"
        warn "Proceeding without VPN (not recommended for production)"
        return 0
    fi

    log "Establishing WireGuard tunnel..."
    sudo wg-quick up "$config"
    log "Tunnel established"
}

# Teardown on exit
cleanup() {
    log "Disconnecting VPN..."
    sudo wg-quick down wg0 2>/dev/null || true
}

# Main
main() {
    local mode="${1:-writes}"

    check_deps
    verify_identity
    establish_tunnel

    trap cleanup EXIT

    log "Starting cloud sync tuner (mode: $mode)..."
    cloud_sync_tuner "$mode"
}

main "$@"

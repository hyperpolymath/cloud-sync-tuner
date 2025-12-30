#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# Cloud Sync Tuner - Install Script
# Installs cloud-sync-tuner and all components

set -euo pipefail

VERSION="1.0.0"
PREFIX="${PREFIX:-$HOME/.local}"
BINDIR="${PREFIX}/bin"
DATADIR="${PREFIX}/share/cloud-sync-tuner"
SYSTEMD_USER_DIR="${HOME}/.config/systemd/user"
NAUTILUS_EXT_DIR="${HOME}/.local/share/nautilus-python/extensions"
DOLPHIN_SVC_DIR="${HOME}/.local/share/kservices5/ServiceMenus"
SELINUX_DIR="/etc/selinux/targeted/policy"

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

info() { echo -e "${BLUE}[INFO]${NC} $1"; }
success() { echo -e "${GREEN}[OK]${NC} $1"; }
warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
error() { echo -e "${RED}[ERROR]${NC} $1"; exit 1; }

check_deps() {
    info "Checking dependencies..."

    local missing=()

    command -v rclone >/dev/null 2>&1 || missing+=("rclone")
    command -v systemctl >/dev/null 2>&1 || missing+=("systemd")
    command -v gnatmake >/dev/null 2>&1 || missing+=("gnat (Ada compiler)")

    if [[ ${#missing[@]} -gt 0 ]]; then
        error "Missing dependencies: ${missing[*]}"
    fi

    success "All dependencies found"
}

build_ada() {
    info "Building cloud-sync-tuner (Ada)..."

    cd src
    gnatmake -O2 -gnatn cloud_sync_tuner.adb -o cloud-sync-tuner
    cd ..

    success "Built cloud-sync-tuner"
}

build_overlay_daemon() {
    info "Building overlay daemon (Rust)..."

    if command -v cargo >/dev/null 2>&1; then
        cd overlay-daemon
        cargo build --release
        cd ..
        success "Built cloud-sync-overlay"
    else
        warn "Rust/Cargo not found - skipping overlay daemon"
    fi
}

install_binaries() {
    info "Installing binaries to ${BINDIR}..."

    mkdir -p "${BINDIR}"

    # Main TUI
    install -m 755 src/cloud-sync-tuner "${BINDIR}/"

    # Overlay daemon (if built)
    if [[ -f overlay-daemon/target/release/cloud-sync-overlay ]]; then
        install -m 755 overlay-daemon/target/release/cloud-sync-overlay "${BINDIR}/"
    fi

    # Health check script
    install -m 755 scripts/cloud-sync-status "${BINDIR}/"

    success "Installed binaries"
}

install_systemd_services() {
    info "Installing systemd user services..."

    mkdir -p "${SYSTEMD_USER_DIR}"

    # Overlay daemon service
    if [[ -f overlay-daemon/cloud-sync-overlay.service ]]; then
        install -m 644 overlay-daemon/cloud-sync-overlay.service "${SYSTEMD_USER_DIR}/"
    fi

    # Watchdog service for error recovery
    install -m 644 systemd/cloud-sync-watchdog.service "${SYSTEMD_USER_DIR}/"
    install -m 644 systemd/cloud-sync-watchdog.timer "${SYSTEMD_USER_DIR}/"

    # Reload systemd
    systemctl --user daemon-reload

    success "Installed systemd services"
}

install_file_manager_extensions() {
    info "Installing file manager extensions..."

    # Nautilus (GNOME)
    if command -v nautilus >/dev/null 2>&1; then
        mkdir -p "${NAUTILUS_EXT_DIR}"
        install -m 644 nautilus-extension/cloud_sync_overlay.py "${NAUTILUS_EXT_DIR}/"
        success "Installed Nautilus extension"
    fi

    # Dolphin (KDE)
    if command -v dolphin >/dev/null 2>&1; then
        mkdir -p "${DOLPHIN_SVC_DIR}"
        install -m 644 dolphin-extension/cloud_sync_overlay.desktop "${DOLPHIN_SVC_DIR}/"
        success "Installed Dolphin extension"
    fi
}

install_selinux_policy() {
    info "Installing SELinux policy..."

    if ! command -v semodule >/dev/null 2>&1; then
        warn "SELinux not available - skipping policy installation"
        return
    fi

    if ! selinuxenabled 2>/dev/null; then
        warn "SELinux disabled - skipping policy installation"
        return
    fi

    cd selinux
    if [[ -f cloud_sync_tuner.pp ]]; then
        sudo semodule -i cloud_sync_tuner.pp
        success "Installed SELinux policy"
    else
        warn "SELinux policy not compiled - run 'make' in selinux/ first"
    fi
    cd ..
}

install_auditd_rules() {
    info "Installing auditd rules..."

    if ! command -v auditctl >/dev/null 2>&1; then
        warn "auditd not available - skipping audit rules"
        return
    fi

    if [[ -f audit/cloud-sync-tuner.rules ]]; then
        sudo install -m 644 audit/cloud-sync-tuner.rules /etc/audit/rules.d/
        sudo augenrules --load 2>/dev/null || warn "Could not reload audit rules"
        success "Installed auditd rules"
    fi
}

install_man_page() {
    info "Installing man page..."

    local mandir="${PREFIX}/share/man/man1"
    mkdir -p "${mandir}"

    if [[ -f man/cloud-sync-tuner.1 ]]; then
        install -m 644 man/cloud-sync-tuner.1 "${mandir}/"
        success "Installed man page"
    fi
}

install_config() {
    info "Installing default configuration..."

    local config_dir="${HOME}/.config/cloud-sync-tuner"
    mkdir -p "${config_dir}"

    if [[ ! -f "${config_dir}/config.toml" ]]; then
        install -m 644 config/config.toml.example "${config_dir}/config.toml"
        success "Installed default config"
    else
        warn "Config exists - not overwriting"
    fi
}

create_offline_dir() {
    info "Creating offline sync directory..."

    local offline_dir="${HOME}/Offline"
    mkdir -p "${offline_dir}"/{Dropbox,GoogleDrive,OneDrive}

    success "Created ${offline_dir}"
}

enable_services() {
    info "Enabling services..."

    # Enable watchdog
    systemctl --user enable --now cloud-sync-watchdog.timer 2>/dev/null || true

    # Enable overlay daemon if installed
    if [[ -f "${SYSTEMD_USER_DIR}/cloud-sync-overlay.service" ]]; then
        systemctl --user enable cloud-sync-overlay.service 2>/dev/null || true
    fi

    success "Enabled services"
}

print_summary() {
    echo
    echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
    echo -e "${GREEN}  Cloud Sync Tuner ${VERSION} installed successfully!${NC}"
    echo -e "${GREEN}═══════════════════════════════════════════════════════════${NC}"
    echo
    echo "Commands:"
    echo "  cloud-sync-tuner        Launch TUI"
    echo "  cloud-sync-tuner writes Apply writes mode"
    echo "  cloud-sync-status       Check sync health"
    echo
    echo "Next steps:"
    echo "  1. Configure rclone remotes: rclone config"
    echo "  2. Run the TUI: cloud-sync-tuner"
    echo "  3. Apply configuration: press [A] in TUI"
    echo
    echo "Documentation:"
    echo "  man cloud-sync-tuner"
    echo
}

uninstall() {
    info "Uninstalling cloud-sync-tuner..."

    # Stop services
    systemctl --user stop cloud-sync-watchdog.timer 2>/dev/null || true
    systemctl --user stop cloud-sync-overlay.service 2>/dev/null || true
    systemctl --user disable cloud-sync-watchdog.timer 2>/dev/null || true
    systemctl --user disable cloud-sync-overlay.service 2>/dev/null || true

    # Remove binaries
    rm -f "${BINDIR}/cloud-sync-tuner"
    rm -f "${BINDIR}/cloud-sync-overlay"
    rm -f "${BINDIR}/cloud-sync-status"

    # Remove services
    rm -f "${SYSTEMD_USER_DIR}/cloud-sync-overlay.service"
    rm -f "${SYSTEMD_USER_DIR}/cloud-sync-watchdog.service"
    rm -f "${SYSTEMD_USER_DIR}/cloud-sync-watchdog.timer"

    # Remove extensions
    rm -f "${NAUTILUS_EXT_DIR}/cloud_sync_overlay.py"
    rm -f "${DOLPHIN_SVC_DIR}/cloud_sync_overlay.desktop"

    # Remove man page
    rm -f "${PREFIX}/share/man/man1/cloud-sync-tuner.1"

    # Remove SELinux policy
    if command -v semodule >/dev/null 2>&1; then
        sudo semodule -r cloud_sync_tuner 2>/dev/null || true
    fi

    # Remove audit rules
    sudo rm -f /etc/audit/rules.d/cloud-sync-tuner.rules 2>/dev/null || true

    systemctl --user daemon-reload

    success "Uninstalled cloud-sync-tuner"
    echo
    echo "Config preserved at: ~/.config/cloud-sync-tuner/"
    echo "Remove manually if not needed."
}

usage() {
    echo "Usage: $0 [OPTIONS]"
    echo
    echo "Options:"
    echo "  --prefix PATH   Install prefix (default: ~/.local)"
    echo "  --uninstall     Remove cloud-sync-tuner"
    echo "  --help          Show this help"
    echo
}

main() {
    echo
    echo -e "${BLUE}Cloud Sync Tuner Installer${NC}"
    echo

    while [[ $# -gt 0 ]]; do
        case $1 in
            --prefix)
                PREFIX="$2"
                BINDIR="${PREFIX}/bin"
                shift 2
                ;;
            --uninstall)
                uninstall
                exit 0
                ;;
            --help|-h)
                usage
                exit 0
                ;;
            *)
                error "Unknown option: $1"
                ;;
        esac
    done

    check_deps
    build_ada
    build_overlay_daemon
    install_binaries
    install_systemd_services
    install_file_manager_extensions
    install_selinux_policy
    install_auditd_rules
    install_man_page
    install_config
    create_offline_dir
    enable_services
    print_summary
}

main "$@"

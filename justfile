# SPDX-License-Identifier: AGPL-3.0-or-later
# Cloud Sync Tuner - Justfile
# https://github.com/casey/just

set shell := ["bash", "-euo", "pipefail", "-c"]

# Default recipe
default:
    @just --list

# ============================================
# BUILD RECIPES
# ============================================

# Build all components
build: build-tui build-overlay build-tray
    @echo "✓ All components built"

# Build Ada TUI
build-tui:
    @echo "Building cloud-sync-tuner (Ada)..."
    cd src && gnatmake -O2 -gnatn cloud_sync_tuner.adb -o cloud-sync-tuner

# Build overlay daemon (Rust)
build-overlay:
    @echo "Building cloud-sync-overlay (Rust)..."
    cd overlay-daemon && cargo build --release

# Build tray daemon (Rust)
build-tray:
    @echo "Building cloud-sync-tray (Rust)..."
    cd tray-daemon && cargo build --release

# Build SELinux policy
build-selinux:
    @echo "Building SELinux policy..."
    cd selinux && make

# Clean all build artifacts
clean:
    rm -f src/cloud-sync-tuner
    rm -rf overlay-daemon/target
    rm -rf tray-daemon/target
    cd selinux && make clean || true

# ============================================
# INSTALL RECIPES
# ============================================

# Install everything
install: build
    ./install.sh

# Uninstall
uninstall:
    ./install.sh --uninstall

# Install systemd services only
install-services:
    mkdir -p ~/.config/systemd/user
    cp systemd/*.service systemd/*.timer ~/.config/systemd/user/
    cp overlay-daemon/cloud-sync-overlay.service ~/.config/systemd/user/
    cp tray-daemon/cloud-sync-tray.service ~/.config/systemd/user/
    systemctl --user daemon-reload
    @echo "✓ Services installed"

# Install SELinux policy
install-selinux: build-selinux
    cd selinux && sudo make install

# ============================================
# SERVICE MANAGEMENT
# ============================================

# Start all cloud sync services
start:
    systemctl --user start rclone-dropbox rclone-gdrive rclone-onedrive
    systemctl --user start cloud-sync-overlay cloud-sync-tray
    @echo "✓ Services started"

# Stop all services
stop:
    systemctl --user stop cloud-sync-tray cloud-sync-overlay
    systemctl --user stop rclone-dropbox rclone-gdrive rclone-onedrive
    @echo "✓ Services stopped"

# Restart all services
restart: stop start

# Check service status
status:
    @cloud-sync-status || true

# View logs
logs:
    journalctl --user -f -u "rclone-*" -u cloud-sync-overlay -u cloud-sync-tray

# ============================================
# CONFIGURATION
# ============================================

# Apply writes mode (recommended for Dropbox)
apply-writes:
    ./src/cloud-sync-tuner writes
    @echo "✓ Applied writes mode"

# Apply full mode (aggressive caching)
apply-full:
    ./src/cloud-sync-tuner full
    @echo "✓ Applied full mode"

# Apply Dropbox-safe preset
preset-dropbox:
    ./src/cloud-sync-tuner --preset dropbox writes
    @echo "✓ Applied Dropbox-safe preset"

# Apply Google Drive preset
preset-gdrive:
    ./src/cloud-sync-tuner --preset gdrive writes
    @echo "✓ Applied Google Drive preset"

# Edit configuration
config:
    ${EDITOR:-nano} ~/.config/cloud-sync-tuner/config.toml

# ============================================
# DEVELOPMENT
# ============================================

# Run TUI in development mode
dev:
    cd src && gnatmake -g cloud_sync_tuner.adb -o cloud-sync-tuner && ./cloud-sync-tuner

# Run tests
test: test-ada test-rust
    @echo "✓ All tests passed"

# Run Ada tests
test-ada:
    @echo "Running Ada tests..."
    cd tests && gnatmake test_runner.adb && ./test_runner

# Run Rust tests
test-rust:
    cd overlay-daemon && cargo test
    cd tray-daemon && cargo test

# Lint code
lint:
    cd overlay-daemon && cargo clippy -- -D warnings
    cd tray-daemon && cargo clippy -- -D warnings

# Format code
fmt:
    cd overlay-daemon && cargo fmt
    cd tray-daemon && cargo fmt

# ============================================
# LAMINAR CI RECIPES
# ============================================

# Run full CI pipeline (for laminar)
ci: lint test build
    @echo "✓ CI pipeline complete"

# Create release tarball
release version:
    @echo "Creating release v{{version}}..."
    git tag -a "v{{version}}" -m "Release v{{version}}"
    mkdir -p dist
    tar czvf "dist/cloud-sync-tuner-{{version}}.tar.gz" \
        --transform "s,^,cloud-sync-tuner-{{version}}/," \
        --exclude="target" --exclude="*.o" --exclude="*.ali" \
        src/ overlay-daemon/ tray-daemon/ selinux/ audit/ \
        systemd/ config/ scripts/ man/ justfile install.sh \
        README.adoc LICENSE CHANGELOG.md
    @echo "✓ Created dist/cloud-sync-tuner-{{version}}.tar.gz"

# ============================================
# COOKBOOKS
# ============================================

# Cookbook: Fix Dropbox rate limiting
cookbook-dropbox-fix: preset-dropbox
    @echo "Applying Dropbox rate limiting fix..."
    just restart
    @echo ""
    @echo "Dropbox rate limiting fixed! Changes applied:"
    @echo "  - VFS cache mode: writes"
    @echo "  - TPS limit: 4 (Dropbox safe)"
    @echo "  - TPS burst: 1"
    @echo "  - Chunk size: 32MB"

# Cookbook: Setup offline folders
cookbook-offline-setup:
    @echo "Setting up offline folder sync..."
    mkdir -p ~/Offline/{Dropbox,GoogleDrive,OneDrive}
    ./src/cloud-sync-tuner --pin dropbox:Documents writes
    just restart
    @echo ""
    @echo "Offline sync configured!"
    @echo "Pinned folders will sync to ~/Offline/"

# Cookbook: Maximum performance
cookbook-max-performance:
    @echo "Applying maximum performance settings..."
    ./src/cloud-sync-tuner \
        --cache-size 50G \
        --transfers 8 \
        --checkers 16 \
        --buffer 64M \
        full
    just restart
    @echo ""
    @echo "Maximum performance mode enabled!"
    @echo "Warning: This uses more disk space and API calls"

# Cookbook: Minimal resources
cookbook-minimal:
    @echo "Applying minimal resource settings..."
    ./src/cloud-sync-tuner \
        --cache-size 2G \
        --transfers 2 \
        --checkers 4 \
        --buffer 8M \
        writes
    just restart
    @echo ""
    @echo "Minimal resource mode enabled"

# Cookbook: Enable enterprise compliance
cookbook-enterprise: install-selinux
    @echo "Enabling enterprise compliance..."
    sudo install -m 644 audit/cloud-sync-tuner.rules /etc/audit/rules.d/
    sudo augenrules --load
    @echo ""
    @echo "Enterprise compliance enabled:"
    @echo "  - SELinux policy installed"
    @echo "  - Audit rules active"
    @echo "  - File access logging enabled"

# ============================================
# HELP
# ============================================

# Show help
help:
    @echo "Cloud Sync Tuner - rclone mount configuration"
    @echo ""
    @echo "Quick Start:"
    @echo "  just build          Build all components"
    @echo "  just install        Install to ~/.local"
    @echo "  just start          Start services"
    @echo ""
    @echo "Cookbooks (common tasks):"
    @echo "  just cookbook-dropbox-fix      Fix Dropbox rate limiting"
    @echo "  just cookbook-offline-setup    Setup offline folders"
    @echo "  just cookbook-max-performance  Maximum caching"
    @echo "  just cookbook-enterprise       Enable SELinux + audit"
    @echo ""
    @echo "Run 'just --list' for all recipes"

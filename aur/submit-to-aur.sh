#!/bin/bash
# SPDX-License-Identifier: AGPL-3.0-or-later
# AUR Submission Script for cloud-sync-tuner
# Run this from the aur/ directory

set -e

PKGNAME="cloud-sync-tuner"
AUR_SSH="aur@aur.archlinux.org"

echo "=== AUR Submission for $PKGNAME ==="
echo ""

# Check for AUR SSH key
if ! ssh-add -l &>/dev/null; then
    echo "ERROR: No SSH agent running or no keys loaded"
    echo "Run: eval \$(ssh-agent) && ssh-add ~/.ssh/aur_rsa"
    exit 1
fi

# Test AUR SSH connection
echo "Testing AUR SSH connection..."
if ! ssh -T $AUR_SSH 2>&1 | grep -q "authenticated"; then
    echo "ERROR: Cannot authenticate to AUR"
    echo "Make sure your AUR SSH key is:"
    echo "  1. Added to your AUR account at https://aur.archlinux.org/account/hyperpolymath"
    echo "  2. Loaded in ssh-agent"
    exit 1
fi
echo "SSH connection OK"

# Check if package exists
echo "Checking if package exists on AUR..."
if curl -s "https://aur.archlinux.org/rpc/v5/info?arg[]=$PKGNAME" | grep -q '"resultcount":0'; then
    echo "Package does not exist, will create new"
    NEW_PACKAGE=true
else
    echo "Package exists, will update"
    NEW_PACKAGE=false
fi

# Clone or create AUR repo
WORKDIR=$(mktemp -d)
cd "$WORKDIR"

if [ "$NEW_PACKAGE" = true ]; then
    echo "Creating new AUR package repository..."
    git clone ssh://$AUR_SSH/${PKGNAME}.git
    cd "$PKGNAME"
else
    echo "Cloning existing AUR repository..."
    git clone ssh://$AUR_SSH/${PKGNAME}.git
    cd "$PKGNAME"
fi

# Copy PKGBUILD and .SRCINFO
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cp "$SCRIPT_DIR/PKGBUILD" .
cp "$SCRIPT_DIR/.SRCINFO" .

# Update checksums
echo "Calculating source checksum..."
SOURCE_URL="https://github.com/hyperpolymath/cloud-sync-tuner/archive/v1.0.0.tar.gz"
# WARNING: Pipe-to-shell is unsafe — download and verify first
CHECKSUM=$(curl -sL "$SOURCE_URL" | sha256sum | awk '{print $1}')
sed -i "s/sha256sums=('SKIP')/sha256sums=('$CHECKSUM')/" PKGBUILD

# Regenerate .SRCINFO with checksum
echo "	sha256sums = $CHECKSUM" >> .SRCINFO

# Show diff
echo ""
echo "=== Changes to be committed ==="
git diff --stat 2>/dev/null || git status
echo ""

# Commit and push
read -p "Commit and push to AUR? [y/N] " -n 1 -r
echo
if [[ $REPLY =~ ^[Yy]$ ]]; then
    git add PKGBUILD .SRCINFO
    git commit -m "Update to v1.0.0"
    git push origin master
    echo ""
    echo "=== SUCCESS ==="
    echo "Package submitted to: https://aur.archlinux.org/packages/$PKGNAME"
else
    echo "Aborted. Files are in: $WORKDIR/$PKGNAME"
fi

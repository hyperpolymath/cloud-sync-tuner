# Cloud Sync Tuner - Packaging

This directory contains package definitions for various package managers and platforms.

## Package Managers

| Directory | Platform | Install Command |
|-----------|----------|-----------------|
| `aur/` | Arch Linux | `yay -S cloud-sync-tuner` |
| `deb/` | Debian/Ubuntu | `apt install cloud-sync-tuner` |
| `rpm/` | Fedora/RHEL | `dnf install cloud-sync-tuner` |
| `opensuse/` | openSUSE | `zypper install cloud-sync-tuner` |
| `homebrew/` | macOS/Linux | `brew install cloud-sync-tuner` |
| `macports/` | macOS | `port install cloud-sync-tuner` |
| `chocolatey/` | Windows | `choco install cloud-sync-tuner` |
| `scoop/` | Windows | `scoop install cloud-sync-tuner` |
| `winget/` | Windows | `winget install cloud-sync-tuner` |
| `nix/` | NixOS/Nix | `nix profile install` |
| `guix/` | GNU Guix | `guix install cloud-sync-tuner` |
| `flatpak/` | Universal Linux | `flatpak install cloud-sync-tuner` |
| `alire/` | Ada (Alire) | `alr get cloud_sync_tuner` |

## Containers

| File | Runtime | Command |
|------|---------|---------|
| `Dockerfile` | Docker | `docker build -t cloud-sync-tuner .` |
| `Containerfile` | Podman/Buildah | `podman build -t cloud-sync-tuner .` |
| `compose.yaml` | Docker Compose | `docker compose up` |
| `nerdctl-compose.yaml` | nerdctl | `nerdctl compose up` |

### Container Profiles

```bash
# Standard (VPN-routed)
nerdctl compose up

# Standalone (no VPN)
nerdctl compose --profile standalone up

# With aria2 acceleration
nerdctl compose --profile accelerated up

# With WireGuard VPN
nerdctl compose --profile vpn up
```

## Building Packages

### AUR (Arch Linux)

```bash
cd aur/
makepkg -si
# Or submit to AUR:
./submit-to-aur.sh
```

### DEB (Debian/Ubuntu)

```bash
cd ..
dpkg-buildpackage -us -uc
# Or use pbuilder/sbuild for clean builds
```

### RPM (Fedora/RHEL)

```bash
rpmbuild -ba packaging/rpm/cloud-sync-tuner.spec
# Or use mock for clean builds
# Or submit to Fedora COPR
```

### Homebrew (macOS)

```bash
# Test locally
brew install --build-from-source packaging/homebrew/cloud-sync-tuner.rb

# Submit to homebrew-core or a tap
```

### Nix

```bash
nix build .#cloud-sync-tuner
# Or add to nixpkgs
```

### Flatpak

```bash
flatpak-builder --user --install build packaging/flatpak/com.hyperpolymath.CloudSyncTuner.yml
# Or submit to Flathub
```

## Platform Notes

### Windows

Windows packages provide CLI mode only (no FUSE mounts).
Use `cloud-sync-tuner writes` for remote operations.

### macOS

Requires macFUSE for FUSE mounts:
1. Install macFUSE from https://osxfuse.github.io
2. Reboot
3. Allow kernel extension in System Preferences

### SELinux (Fedora/RHEL)

After installation, enable SELinux policy:
```bash
cd /usr/share/cloud-sync-tuner/selinux
sudo make install
```

### Audit Rules (Enterprise)

```bash
sudo cp /usr/share/cloud-sync-tuner/audit/*.rules /etc/audit/rules.d/
sudo systemctl restart auditd
```

## Checksums

After building a release, update SHA256 checksums:

```bash
# For source tarball
curl -sL https://github.com/hyperpolymath/cloud-sync-tuner/archive/v1.0.0.tar.gz | sha256sum

# Update PKGBUILD, *.spec, *.rb, etc. with the hash
```

## Submitting to Repositories

### AUR
1. Create account at https://aur.archlinux.org
2. Add SSH key to account
3. Run `aur/submit-to-aur.sh`

### Homebrew
1. Fork https://github.com/Homebrew/homebrew-core
2. Add formula
3. Submit PR

### Flathub
1. Fork https://github.com/flathub/flathub
2. Add manifest
3. Submit PR

### Fedora COPR
1. Create account at https://copr.fedorainfracloud.org
2. Create project
3. Upload SRPM or link to spec file

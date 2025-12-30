# Dolphin Integration

## Service Menu (Context Menu)

Install the service menu for right-click actions:

```bash
mkdir -p ~/.local/share/kservices5/ServiceMenus/
cp cloud_sync_overlay.desktop ~/.local/share/kservices5/ServiceMenus/
```

## Overlay Icons

Dolphin supports overlay icons via the `fileoverlaysplugin`. The cloud-sync-overlay
daemon exposes status via D-Bus which Dolphin can query.

For KDE Plasma 6+, install the overlay plugin:

```bash
# Fedora
sudo dnf install dolphin-plugins

# The daemon must be running:
systemctl --user start cloud-sync-overlay
```

## Alternative: Using .directory Files

For directories, you can set emblems via `.directory` files:

```ini
[Desktop Entry]
Icon=folder-cloud
```

The daemon can automatically manage these files for cloud directories.

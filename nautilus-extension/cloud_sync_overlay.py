# SPDX-License-Identifier: AGPL-3.0-or-later
"""
Nautilus extension for cloud-sync-tuner overlay icons.

This extension communicates with the cloud-sync-overlay D-Bus daemon
to display sync status emblems on files in cloud mount directories.

NOTE: Python is required by Nautilus extension API - this is an
exception to the project's language policy.

Installation:
    cp cloud_sync_overlay.py ~/.local/share/nautilus-python/extensions/
    nautilus -q  # Restart Nautilus
"""

import os
from urllib.parse import unquote
from gi.repository import GObject, Nautilus, Gio, GLib

# Cloud mount paths to monitor
CLOUD_MOUNT_PATHS = [
    "/run/media/hyper/eclipse/Cloud/Dropbox",
    "/run/media/hyper/eclipse/Cloud/GoogleDrive",
    "/run/media/hyper/eclipse/Cloud/OneDrive",
]

# D-Bus service name
DBUS_NAME = "org.cloudsynctune.Overlay1"
DBUS_PATH = "/org/cloudsynctune/Overlay1"
DBUS_INTERFACE = "org.cloudsynctune.Overlay1"


class CloudSyncOverlayExtension(GObject.GObject, Nautilus.InfoProvider):
    """Nautilus extension that adds sync status emblems to cloud files."""

    def __init__(self):
        super().__init__()
        self._proxy = None
        self._connect_dbus()

    def _connect_dbus(self):
        """Connect to the overlay daemon via D-Bus."""
        try:
            bus = Gio.bus_get_sync(Gio.BusType.SESSION, None)
            self._proxy = Gio.DBusProxy.new_sync(
                bus,
                Gio.DBusProxyFlags.NONE,
                None,
                DBUS_NAME,
                DBUS_PATH,
                DBUS_INTERFACE,
                None,
            )
        except Exception as e:
            print(f"cloud-sync-overlay: Failed to connect to D-Bus: {e}")
            self._proxy = None

    def _is_cloud_path(self, path: str) -> bool:
        """Check if path is under a monitored cloud mount."""
        return any(path.startswith(mount) for mount in CLOUD_MOUNT_PATHS)

    def _get_emblem(self, path: str) -> str:
        """Get emblem name for a file from the overlay daemon."""
        if self._proxy is None:
            self._connect_dbus()
            if self._proxy is None:
                return ""

        try:
            result = self._proxy.call_sync(
                "GetEmblem",
                GLib.Variant("(s)", (path,)),
                Gio.DBusCallFlags.NONE,
                1000,  # 1 second timeout
                None,
            )
            return result.unpack()[0] if result else ""
        except Exception as e:
            print(f"cloud-sync-overlay: Error getting emblem for {path}: {e}")
            return ""

    def update_file_info(self, file: Nautilus.FileInfo):
        """Called by Nautilus to update file info including emblems."""
        # Only process local files
        if file.get_uri_scheme() != "file":
            return

        # Get the file path
        uri = file.get_uri()
        path = unquote(uri[7:])  # Remove "file://" prefix

        # Only process files under cloud mounts
        if not self._is_cloud_path(path):
            return

        # Get emblem from daemon
        emblem = self._get_emblem(path)
        if emblem:
            file.add_emblem(emblem)


class CloudSyncMenuExtension(GObject.GObject, Nautilus.MenuProvider):
    """Nautilus extension that adds cloud sync context menu items."""

    def __init__(self):
        super().__init__()
        self._proxy = None
        self._connect_dbus()

    def _connect_dbus(self):
        """Connect to the overlay daemon via D-Bus."""
        try:
            bus = Gio.bus_get_sync(Gio.BusType.SESSION, None)
            self._proxy = Gio.DBusProxy.new_sync(
                bus,
                Gio.DBusProxyFlags.NONE,
                None,
                DBUS_NAME,
                DBUS_PATH,
                DBUS_INTERFACE,
                None,
            )
        except Exception:
            self._proxy = None

    def _is_cloud_path(self, path: str) -> bool:
        """Check if path is under a monitored cloud mount."""
        return any(path.startswith(mount) for mount in CLOUD_MOUNT_PATHS)

    def _on_pin_offline(self, menu, files):
        """Pin selected files for offline access."""
        for file in files:
            path = unquote(file.get_uri()[7:])
            # TODO: Call daemon to pin file
            print(f"cloud-sync-overlay: Pin offline: {path}")

    def _on_free_space(self, menu, files):
        """Free up space by removing local cache."""
        for file in files:
            path = unquote(file.get_uri()[7:])
            # TODO: Call daemon to free space
            print(f"cloud-sync-overlay: Free space: {path}")

    def _on_sync_now(self, menu, files):
        """Force immediate sync."""
        for file in files:
            path = unquote(file.get_uri()[7:])
            # TODO: Call daemon to sync now
            print(f"cloud-sync-overlay: Sync now: {path}")

    def get_file_items(self, files):
        """Get context menu items for selected files."""
        # Check if any file is in a cloud mount
        cloud_files = []
        for file in files:
            if file.get_uri_scheme() != "file":
                continue
            path = unquote(file.get_uri()[7:])
            if self._is_cloud_path(path):
                cloud_files.append(file)

        if not cloud_files:
            return []

        items = []

        # Pin for offline access
        pin_item = Nautilus.MenuItem(
            name="CloudSync::PinOffline",
            label="Make Available Offline",
            tip="Pin this file/folder for offline access",
            icon="emblem-favorite",
        )
        pin_item.connect("activate", self._on_pin_offline, cloud_files)
        items.append(pin_item)

        # Free up space
        free_item = Nautilus.MenuItem(
            name="CloudSync::FreeSpace",
            label="Free Up Space",
            tip="Remove local copy, keep in cloud only",
            icon="emblem-shared",
        )
        free_item.connect("activate", self._on_free_space, cloud_files)
        items.append(free_item)

        # Sync now
        sync_item = Nautilus.MenuItem(
            name="CloudSync::SyncNow",
            label="Sync Now",
            tip="Force immediate synchronization",
            icon="emblem-synchronizing",
        )
        sync_item.connect("activate", self._on_sync_now, cloud_files)
        items.append(sync_item)

        return items

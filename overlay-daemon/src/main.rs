// SPDX-License-Identifier: AGPL-3.0-or-later
//! cloud-sync-overlay: File manager overlay icon daemon for rclone cloud mounts
//!
//! Monitors rclone mount status and provides sync state to file managers
//! via D-Bus and GIO emblems.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};
use zbus::{interface, ConnectionBuilder};

/// Sync status for a file or directory
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum SyncStatus {
    /// Fully synced to cloud
    Synced,
    /// Currently syncing
    Syncing,
    /// Available online only (not cached locally)
    Online,
    /// Pinned for offline access
    Pinned,
    /// Sync error occurred
    Error,
    /// Status unknown
    Unknown,
}

impl SyncStatus {
    /// Get the emblem name for this status
    pub fn emblem(&self) -> &'static str {
        match self {
            SyncStatus::Synced => "emblem-default",    // checkmark
            SyncStatus::Syncing => "emblem-synchronizing",
            SyncStatus::Online => "emblem-shared",     // cloud
            SyncStatus::Pinned => "emblem-favorite",   // star/pin
            SyncStatus::Error => "emblem-important",   // warning
            SyncStatus::Unknown => "",
        }
    }

    /// Get the icon name for overlays
    pub fn icon(&self) -> &'static str {
        match self {
            SyncStatus::Synced => "cloud-sync-complete",
            SyncStatus::Syncing => "cloud-sync-progress",
            SyncStatus::Online => "cloud-online",
            SyncStatus::Pinned => "cloud-offline-available",
            SyncStatus::Error => "cloud-sync-error",
            SyncStatus::Unknown => "cloud-unknown",
        }
    }
}

/// Represents a monitored cloud mount
#[derive(Debug, Clone)]
pub struct CloudMount {
    pub name: String,
    pub mount_point: PathBuf,
    pub remote: String,
    pub rc_addr: String,
}

/// State manager for file sync status
#[derive(Debug, Default)]
pub struct SyncState {
    /// Map of file paths to their sync status
    file_status: HashMap<PathBuf, SyncStatus>,
    /// Currently active transfers
    active_transfers: Vec<String>,
    /// Monitored mounts
    mounts: Vec<CloudMount>,
}

impl SyncState {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn get_status(&self, path: &PathBuf) -> SyncStatus {
        self.file_status.get(path).copied().unwrap_or(SyncStatus::Unknown)
    }

    pub fn set_status(&mut self, path: PathBuf, status: SyncStatus) {
        self.file_status.insert(path, status);
    }

    pub fn is_syncing(&self, path: &PathBuf) -> bool {
        self.active_transfers.iter().any(|t| t.starts_with(&path.to_string_lossy().to_string()))
    }
}

/// D-Bus interface for file manager integration
struct CloudSyncOverlay {
    state: Arc<RwLock<SyncState>>,
}

#[interface(name = "org.cloudsynctune.Overlay1")]
impl CloudSyncOverlay {
    /// Get sync status for a file path
    async fn get_status(&self, path: &str) -> String {
        let state = self.state.read().await;
        let path = PathBuf::from(path);
        let status = state.get_status(&path);
        serde_json::to_string(&status).unwrap_or_else(|_| "\"unknown\"".to_string())
    }

    /// Get emblem name for a file path
    async fn get_emblem(&self, path: &str) -> String {
        let state = self.state.read().await;
        let path = PathBuf::from(path);
        let status = state.get_status(&path);
        status.emblem().to_string()
    }

    /// Get icon name for overlay
    async fn get_icon(&self, path: &str) -> String {
        let state = self.state.read().await;
        let path = PathBuf::from(path);
        let status = state.get_status(&path);
        status.icon().to_string()
    }

    /// Check if file is currently syncing
    async fn is_syncing(&self, path: &str) -> bool {
        let state = self.state.read().await;
        let path = PathBuf::from(path);
        state.is_syncing(&path)
    }

    /// Get all active transfers
    async fn get_active_transfers(&self) -> Vec<String> {
        let state = self.state.read().await;
        state.active_transfers.clone()
    }

    /// Signal emitted when sync status changes
    #[zbus(signal)]
    async fn status_changed(ctxt: &zbus::SignalContext<'_>, path: &str, status: &str) -> zbus::Result<()>;
}

/// Query rclone rc for current transfers
async fn query_rclone_transfers(rc_addr: &str) -> Result<Vec<String>, reqwest::Error> {
    let client = reqwest::Client::new();
    let url = format!("{}/core/stats", rc_addr);

    let response: serde_json::Value = client
        .post(&url)
        .json(&serde_json::json!({}))
        .send()
        .await?
        .json()
        .await?;

    let mut transfers = Vec::new();

    if let Some(transferring) = response.get("transferring").and_then(|t| t.as_array()) {
        for transfer in transferring {
            if let Some(name) = transfer.get("name").and_then(|n| n.as_str()) {
                transfers.push(name.to_string());
            }
        }
    }

    Ok(transfers)
}

/// Query rclone rc for VFS cache status
async fn query_vfs_stats(rc_addr: &str) -> Result<serde_json::Value, reqwest::Error> {
    let client = reqwest::Client::new();
    let url = format!("{}/vfs/stats", rc_addr);

    client
        .post(&url)
        .json(&serde_json::json!({}))
        .send()
        .await?
        .json()
        .await
}

/// Monitor rclone and update sync state
async fn monitor_rclone(state: Arc<RwLock<SyncState>>, mount: CloudMount) {
    let poll_interval = tokio::time::Duration::from_secs(2);

    loop {
        // Query active transfers
        match query_rclone_transfers(&mount.rc_addr).await {
            Ok(transfers) => {
                let mut state = state.write().await;
                state.active_transfers = transfers.clone();

                // Update status for files being transferred
                for transfer in &transfers {
                    let path = mount.mount_point.join(transfer);
                    state.set_status(path, SyncStatus::Syncing);
                }
            }
            Err(e) => {
                debug!("Failed to query rclone transfers for {}: {}", mount.name, e);
            }
        }

        // Query VFS cache status
        match query_vfs_stats(&mount.rc_addr).await {
            Ok(stats) => {
                debug!("VFS stats for {}: {:?}", mount.name, stats);
                // Parse cache status and update file states
                if let Some(disk_cache) = stats.get("diskCache") {
                    if let Some(files) = disk_cache.get("cacheFiles").and_then(|f| f.as_array()) {
                        let mut state = state.write().await;
                        for file in files {
                            if let Some(name) = file.get("name").and_then(|n| n.as_str()) {
                                let path = mount.mount_point.join(name);
                                // If in cache, it's synced locally
                                state.set_status(path, SyncStatus::Synced);
                            }
                        }
                    }
                }
            }
            Err(e) => {
                debug!("Failed to query VFS stats for {}: {}", mount.name, e);
            }
        }

        tokio::time::sleep(poll_interval).await;
    }
}

/// Set GIO emblem on a file (for Nautilus integration)
fn set_gio_emblem(path: &PathBuf, emblem: &str) -> Result<(), String> {
    use std::process::Command;

    if emblem.is_empty() {
        // Clear emblems
        Command::new("gio")
            .args(["set", "-t", "unset", &path.to_string_lossy(), "metadata::emblems"])
            .output()
            .map_err(|e| e.to_string())?;
    } else {
        // Set emblem
        Command::new("gio")
            .args(["set", "-t", "stringv", &path.to_string_lossy(), "metadata::emblems", emblem])
            .output()
            .map_err(|e| e.to_string())?;
    }

    Ok(())
}

/// Load mount configuration from cloud-sync-tuner config
fn load_mounts() -> Vec<CloudMount> {
    let config_path = xdg::BaseDirectories::with_prefix("cloud-sync-tuner")
        .ok()
        .and_then(|dirs| dirs.find_config_file("config.toml"));

    // Default mounts based on typical systemd service setup
    let default_mounts = vec![
        CloudMount {
            name: "dropbox".to_string(),
            mount_point: PathBuf::from("/run/media/hyper/eclipse/Cloud/Dropbox"),
            remote: "dropbox:".to_string(),
            rc_addr: "http://localhost:5572".to_string(),
        },
        CloudMount {
            name: "gdrive".to_string(),
            mount_point: PathBuf::from("/run/media/hyper/eclipse/Cloud/GoogleDrive"),
            remote: "gdrive:".to_string(),
            rc_addr: "http://localhost:5573".to_string(),
        },
        CloudMount {
            name: "onedrive".to_string(),
            mount_point: PathBuf::from("/run/media/hyper/eclipse/Cloud/OneDrive"),
            remote: "onedrive:".to_string(),
            rc_addr: "http://localhost:5574".to_string(),
        },
    ];

    if config_path.is_none() {
        info!("No config found, using default mount points");
        return default_mounts;
    }

    // TODO: Parse config.toml for mount definitions
    default_mounts
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("cloud_sync_overlay=info".parse()?)
        )
        .init();

    info!("Starting cloud-sync-overlay daemon");

    // Initialize state
    let state = Arc::new(RwLock::new(SyncState::new()));

    // Load mount configurations
    let mounts = load_mounts();
    {
        let mut s = state.write().await;
        s.mounts = mounts.clone();
    }

    // Start D-Bus service
    let overlay = CloudSyncOverlay {
        state: state.clone(),
    };

    let connection = ConnectionBuilder::session()?
        .name("org.cloudsynctune.Overlay1")?
        .serve_at("/org/cloudsynctune/Overlay1", overlay)?
        .build()
        .await?;

    info!("D-Bus service registered at org.cloudsynctune.Overlay1");

    // Start monitoring each mount
    let mut handles = Vec::new();
    for mount in mounts {
        let state = state.clone();
        info!("Monitoring mount: {} at {}", mount.name, mount.mount_point.display());
        handles.push(tokio::spawn(monitor_rclone(state, mount)));
    }

    // Keep running
    info!("Overlay daemon running. Press Ctrl+C to stop.");
    tokio::signal::ctrl_c().await?;

    info!("Shutting down...");
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sync_status_emblem() {
        assert_eq!(SyncStatus::Synced.emblem(), "emblem-default");
        assert_eq!(SyncStatus::Syncing.emblem(), "emblem-synchronizing");
        assert_eq!(SyncStatus::Error.emblem(), "emblem-important");
    }

    #[test]
    fn test_sync_state() {
        let mut state = SyncState::new();
        let path = PathBuf::from("/test/file.txt");

        assert_eq!(state.get_status(&path), SyncStatus::Unknown);

        state.set_status(path.clone(), SyncStatus::Synced);
        assert_eq!(state.get_status(&path), SyncStatus::Synced);
    }
}

// SPDX-License-Identifier: AGPL-3.0-or-later
//! cloud-sync-tray: System tray daemon for cloud sync status
//!
//! Displays sync status in system tray with menu for common actions.

use muda::{Menu, MenuEvent, MenuItem, PredefinedMenuItem, Submenu};
use notify_rust::Notification;
use serde::Deserialize;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;
use tao::event_loop::{ControlFlow, EventLoopBuilder};
use tray_icon::{
    menu::MenuEvent as TrayMenuEvent, Icon, TrayIcon, TrayIconBuilder, TrayIconEvent,
};
use tracing::{debug, error, info, warn};

/// Sync status for display
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum SyncStatus {
    AllSynced,
    Syncing,
    SomeOffline,
    Error,
    Unknown,
}

impl SyncStatus {
    fn icon_name(&self) -> &'static str {
        match self {
            SyncStatus::AllSynced => "cloud-sync-complete",
            SyncStatus::Syncing => "cloud-sync-progress",
            SyncStatus::SomeOffline => "cloud-offline",
            SyncStatus::Error => "cloud-sync-error",
            SyncStatus::Unknown => "cloud",
        }
    }

    fn tooltip(&self) -> &'static str {
        match self {
            SyncStatus::AllSynced => "Cloud Sync: All files synced",
            SyncStatus::Syncing => "Cloud Sync: Syncing...",
            SyncStatus::SomeOffline => "Cloud Sync: Some files offline",
            SyncStatus::Error => "Cloud Sync: Error occurred",
            SyncStatus::Unknown => "Cloud Sync: Checking status...",
        }
    }
}

/// Service health from cloud-sync-status
#[derive(Debug, Deserialize)]
struct ServiceHealth {
    status: String,
    service: String,
    mount: String,
    disk_usage_percent: u8,
}

#[derive(Debug, Deserialize)]
struct HealthSummary {
    healthy: u32,
    degraded: u32,
    failed: u32,
}

#[derive(Debug, Deserialize)]
struct HealthResponse {
    summary: HealthSummary,
    services: std::collections::HashMap<String, ServiceHealth>,
}

/// Check health status via rclone rc
async fn check_health() -> SyncStatus {
    // Try to read from cloud-sync-status
    let output = tokio::process::Command::new("cloud-sync-status")
        .arg("--json")
        .output()
        .await;

    match output {
        Ok(out) if out.status.success() => {
            let json = String::from_utf8_lossy(&out.stdout);
            match serde_json::from_str::<HealthResponse>(&json) {
                Ok(health) => {
                    if health.summary.failed > 0 {
                        SyncStatus::Error
                    } else if health.summary.degraded > 0 {
                        SyncStatus::SomeOffline
                    } else {
                        // Check if any are syncing
                        for (_name, svc) in &health.services {
                            if svc.status == "syncing" {
                                return SyncStatus::Syncing;
                            }
                        }
                        SyncStatus::AllSynced
                    }
                }
                Err(e) => {
                    debug!("Failed to parse health response: {}", e);
                    SyncStatus::Unknown
                }
            }
        }
        _ => SyncStatus::Unknown,
    }
}

/// Send desktop notification
fn notify(title: &str, body: &str, urgency: notify_rust::Urgency) {
    if let Err(e) = Notification::new()
        .summary(title)
        .body(body)
        .urgency(urgency)
        .icon("cloud")
        .show()
    {
        warn!("Failed to send notification: {}", e);
    }
}

/// Create embedded icon data
fn create_icon() -> Icon {
    // Simple cloud icon (16x16 RGBA)
    let width = 16u32;
    let height = 16u32;
    let mut rgba = vec![0u8; (width * height * 4) as usize];

    // Draw a simple cloud shape
    let cloud_color = [100u8, 149, 237, 255]; // Cornflower blue

    // Cloud shape coordinates (simplified)
    let cloud_pixels = [
        // Top arc
        (6, 3), (7, 3), (8, 3), (9, 3),
        (5, 4), (6, 4), (7, 4), (8, 4), (9, 4), (10, 4),
        // Middle body
        (3, 5), (4, 5), (5, 5), (6, 5), (7, 5), (8, 5), (9, 5), (10, 5), (11, 5), (12, 5),
        (2, 6), (3, 6), (4, 6), (5, 6), (6, 6), (7, 6), (8, 6), (9, 6), (10, 6), (11, 6), (12, 6), (13, 6),
        (2, 7), (3, 7), (4, 7), (5, 7), (6, 7), (7, 7), (8, 7), (9, 7), (10, 7), (11, 7), (12, 7), (13, 7),
        (2, 8), (3, 8), (4, 8), (5, 8), (6, 8), (7, 8), (8, 8), (9, 8), (10, 8), (11, 8), (12, 8), (13, 8),
        // Bottom
        (3, 9), (4, 9), (5, 9), (6, 9), (7, 9), (8, 9), (9, 9), (10, 9), (11, 9), (12, 9),
        (4, 10), (5, 10), (6, 10), (7, 10), (8, 10), (9, 10), (10, 10), (11, 10),
    ];

    for (x, y) in &cloud_pixels {
        let idx = ((y * width + x) * 4) as usize;
        if idx + 3 < rgba.len() {
            rgba[idx] = cloud_color[0];
            rgba[idx + 1] = cloud_color[1];
            rgba[idx + 2] = cloud_color[2];
            rgba[idx + 3] = cloud_color[3];
        }
    }

    Icon::from_rgba(rgba, width, height).expect("Failed to create icon")
}

fn main() {
    // Initialize logging
    tracing_subscriber::fmt()
        .with_env_filter("cloud_sync_tray=info")
        .init();

    info!("Starting cloud-sync-tray daemon");

    // Build event loop
    let event_loop = EventLoopBuilder::new().build();

    // Create menu
    let menu = Menu::new();

    let status_item = MenuItem::new("Status: Checking...", false, None);
    let separator = PredefinedMenuItem::separator();
    let open_tui = MenuItem::new("Open Cloud Sync Tuner", true, None);
    let sync_now = MenuItem::new("Sync Now", true, None);
    let view_logs = MenuItem::new("View Logs", true, None);

    let services_menu = Submenu::new("Services", true);
    let dropbox_item = MenuItem::new("Dropbox: Checking...", false, None);
    let gdrive_item = MenuItem::new("Google Drive: Checking...", false, None);
    let onedrive_item = MenuItem::new("OneDrive: Checking...", false, None);
    services_menu.append(&dropbox_item).unwrap();
    services_menu.append(&gdrive_item).unwrap();
    services_menu.append(&onedrive_item).unwrap();

    let quit_item = MenuItem::new("Quit", true, None);

    menu.append(&status_item).unwrap();
    menu.append(&separator).unwrap();
    menu.append(&services_menu).unwrap();
    menu.append(&separator).unwrap();
    menu.append(&open_tui).unwrap();
    menu.append(&sync_now).unwrap();
    menu.append(&view_logs).unwrap();
    menu.append(&separator).unwrap();
    menu.append(&quit_item).unwrap();

    // Create tray icon
    let icon = create_icon();
    let tray_icon = TrayIconBuilder::new()
        .with_menu(Box::new(menu))
        .with_tooltip("Cloud Sync Tuner")
        .with_icon(icon)
        .build()
        .expect("Failed to create tray icon");

    // Menu event receiver
    let menu_channel = MenuEvent::receiver();

    // Track quit state
    let should_quit = Arc::new(AtomicBool::new(false));
    let should_quit_clone = should_quit.clone();

    // Spawn status updater
    let runtime = tokio::runtime::Runtime::new().unwrap();
    std::thread::spawn(move || {
        runtime.block_on(async {
            let mut last_status = SyncStatus::Unknown;

            loop {
                if should_quit_clone.load(Ordering::Relaxed) {
                    break;
                }

                let status = check_health().await;

                // Notify on status change
                if status != last_status {
                    match status {
                        SyncStatus::Error => {
                            notify(
                                "Cloud Sync Error",
                                "One or more cloud services have failed",
                                notify_rust::Urgency::Critical,
                            );
                        }
                        SyncStatus::AllSynced if last_status == SyncStatus::Syncing => {
                            notify(
                                "Sync Complete",
                                "All files are now synced",
                                notify_rust::Urgency::Low,
                            );
                        }
                        _ => {}
                    }
                    last_status = status;
                }

                tokio::time::sleep(tokio::time::Duration::from_secs(10)).await;
            }
        });
    });

    // Run event loop
    let open_tui_id = open_tui.id().clone();
    let sync_now_id = sync_now.id().clone();
    let view_logs_id = view_logs.id().clone();
    let quit_id = quit_item.id().clone();

    event_loop.run(move |_event, _, control_flow| {
        *control_flow = ControlFlow::Poll;

        // Handle menu events
        if let Ok(event) = menu_channel.try_recv() {
            if event.id == open_tui_id {
                let _ = std::process::Command::new("cloud-sync-tuner").spawn();
            } else if event.id == sync_now_id {
                // Trigger immediate sync
                let _ = std::process::Command::new("systemctl")
                    .args(["--user", "restart", "rclone-dropbox", "rclone-gdrive", "rclone-onedrive"])
                    .spawn();
                notify("Sync Started", "Restarting cloud sync services", notify_rust::Urgency::Normal);
            } else if event.id == view_logs_id {
                let _ = std::process::Command::new("journalctl")
                    .args(["--user", "-f", "-u", "rclone-*"])
                    .spawn();
            } else if event.id == quit_id {
                should_quit.store(true, Ordering::Relaxed);
                *control_flow = ControlFlow::Exit;
            }
        }
    });
}

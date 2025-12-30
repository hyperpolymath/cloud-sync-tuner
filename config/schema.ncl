# SPDX-License-Identifier: AGPL-3.0-or-later
# Cloud Sync Tuner - Nickel Configuration Schema
# Validates and generates TOML configuration

let CacheMode = [| 'Off, 'Minimal, 'Writes, 'Full |] in

let ConflictStrategy = [| 'None, 'Newer, 'Older, 'Larger, 'Smaller, 'Path1, 'Path2 |] in

let RateLimiting = {
  tps_limit | Number | default = 4
    | doc "Transactions per second (1-100)",
  tps_burst | Number | default = 1
    | doc "Burst allowance (1-10)",
  chunk_size_mb | Number | default = 32
    | doc "Read chunk size in MB (1-512)",
  cache_age_hours | Number | default = 72
    | doc "Cache entry max age in hours (1-168)",
  dir_cache_min | Number | default = 5
    | doc "Directory cache time in minutes (1-60)",
} in

let SmartSync = {
  cache_max_size_gb | Number | default = 10
    | doc "Maximum cache size in GB (0 = unlimited)",
  min_free_space_gb | Number | default = 5
    | doc "Minimum free disk space in GB",
  write_back_sec | Number | default = 5
    | doc "Write-back delay in seconds (1-60)",
  poll_interval_min | Number | default = 1
    | doc "Remote change poll interval in minutes (1-60)",
  buffer_size_mb | Number | default = 16
    | doc "Buffer size in MB (1-128)",
  transfers | Number | default = 4
    | doc "Concurrent transfers (1-32)",
  checkers | Number | default = 8
    | doc "Concurrent checkers (1-64)",
} in

let ScheduleEntry = {
  start | Number
    | doc "Start hour (0-23)",
  end | Number
    | doc "End hour (0-23)",
  limit_kbps | Number
    | doc "Bandwidth limit in KB/s (0 = unlimited)",
} in

let Bandwidth = {
  enabled | Bool | default = false
    | doc "Enable bandwidth scheduling",
  schedule | Array ScheduleEntry | default = []
    | doc "Schedule entries",
} in

let Service = {
  name | String
    | doc "Display name",
  remote | String
    | doc "rclone remote name (e.g., 'dropbox:')",
  mount_point | String
    | doc "Local mount path",
  service_name | String
    | doc "systemd service name",
  enabled | Bool | default = true
    | doc "Whether service is enabled",
  rc_port | Number | optional
    | doc "rclone RC port for this service",

  # Service-specific overrides
  tps_limit | Number | optional,
  tps_burst | Number | optional,
  chunk_size_mb | Number | optional,
} in

let PinnedFolder = {
  service | String
    | doc "Service name (dropbox, gdrive, onedrive)",
  remote_path | String
    | doc "Path on remote (e.g., 'Documents/Important')",
  local_path | String
    | doc "Local sync path (e.g., '~/Offline/Dropbox/Documents')",
  enabled | Bool | default = true
    | doc "Whether folder is pinned",
} in

let Config = {
  general | {
    cache_mode | CacheMode | default = 'Writes
      | doc "VFS cache mode",
    desktop_notify | Bool | default = true
      | doc "Desktop notifications",
    tray_icon | Bool | default = true
      | doc "System tray icon",
    offline_dir | String | default = "~/Offline"
      | doc "Offline sync directory",
  },

  rate_limiting | RateLimiting | default = {},
  smart_sync | SmartSync | default = {},

  conflict_resolution | {
    strategy | ConflictStrategy | default = 'Newer
      | doc "Conflict resolution strategy",
  } | default = {},

  bandwidth | Bandwidth | default = {},

  services | Array Service | default = [],
  pinned_folders | Array PinnedFolder | default = [],
} in

# Presets
let presets = {
  dropbox = {
    general.cache_mode = 'Writes,
    rate_limiting = {
      tps_limit = 4,
      tps_burst = 1,
      chunk_size_mb = 32,
    },
  },

  gdrive = {
    general.cache_mode = 'Writes,
    rate_limiting = {
      tps_limit = 8,
      tps_burst = 2,
      chunk_size_mb = 64,
    },
  },

  onedrive = {
    general.cache_mode = 'Writes,
    rate_limiting = {
      tps_limit = 6,
      tps_burst = 2,
      chunk_size_mb = 32,
    },
  },

  minimal = {
    general.cache_mode = 'Writes,
    smart_sync = {
      cache_max_size_gb = 2,
      transfers = 2,
      checkers = 4,
      buffer_size_mb = 8,
    },
  },

  performance = {
    general.cache_mode = 'Full,
    smart_sync = {
      cache_max_size_gb = 50,
      transfers = 8,
      checkers = 16,
      buffer_size_mb = 64,
    },
  },
} in

# Export schema and presets
{
  Config,
  presets,

  # Validation function
  validate = fun config =>
    let valid_tps = config.rate_limiting.tps_limit >= 1
                  && config.rate_limiting.tps_limit <= 100 in
    let valid_burst = config.rate_limiting.tps_burst >= 1
                    && config.rate_limiting.tps_burst <= 10 in
    let valid_chunk = config.rate_limiting.chunk_size_mb >= 1
                    && config.rate_limiting.chunk_size_mb <= 512 in
    valid_tps && valid_burst && valid_chunk,

  # Generate TOML output
  to_toml = fun config => std.serialize 'Toml config,
}

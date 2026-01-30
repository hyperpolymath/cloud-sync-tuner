; SPDX-License-Identifier: MPL-2.0-or-later
; Cloud Sync Tuner - Ecosystem Position

(ecosystem
 (version . "1.0")
 (name . "cloud-sync-tuner")
 (type . "tool")
 (purpose . "Configure rclone cloud mounts with rate limiting and SDP security")

 (position-in-ecosystem
  (layer . "configuration")
  (domain . "cloud-storage")
  (security-model . "zero-trust-sdp"))

 (related-projects
  ;; Core dependencies
  ((name . "rclone")
   (relationship . "dependency")
   (description . "Multi-cloud sync tool")
   (integration . "Generates service files for rclone mounts"))

  ((name . "libfuse3")
   (relationship . "dependency")
   (description . "Filesystem in Userspace library")
   (integration . "FUSE mount for cloud storage"))

  ((name . "libwireguard")
   (relationship . "dependency")
   (description . "WireGuard VPN library")
   (integration . "SDP tunnel establishment"))

  ;; Zig FFI libraries (owned)
  ((name . "zig-wireguard")
   (relationship . "owned-component")
   (repo . "https://github.com/hyperpolymath/zig-wireguard")
   (description . "Zig FFI bindings to libwireguard")
   (integration . "VPN tunnel management, cicada key import"))

  ((name . "zig-rclone")
   (relationship . "owned-component")
   (repo . "https://github.com/hyperpolymath/zig-rclone")
   (description . "Zig FFI bindings to librclone")
   (integration . "Cloud storage operations, rate limiting"))

  ((name . "zig-fuse-ext")
   (relationship . "owned-component")
   (repo . "https://github.com/hyperpolymath/zig-fuse-ext")
   (description . "Extended Zig FUSE bindings with cloud optimizations")
   (integration . "Filesystem mount with caching and prefetch"))

  ;; Sibling projects
  ((name . "laminar")
   (relationship . "sibling-standard")
   (description . "Cloud-to-cloud streaming transfers")
   (integration . "Shares rclone as data plane, zig-rclone for embedded ops"))

  ((name . "cicada")
   (relationship . "security-integration")
   (description . "Post-quantum cryptographic identity management")
   (integration . "Network-isolated key generation, Unix socket IPC"))

  ;; Optional enhancements
  ((name . "aria2")
   (relationship . "optional-enhancement")
   (description . "Download accelerator")
   (integration . "Optional compose profile for faster downloads"))

  ((name . "pssh")
   (relationship . "optional-enhancement")
   (description . "Parallel SSH execution")
   (integration . "Multi-host deployment of generated configs")))

 (what-this-is
  ("TUI for selecting rclone VFS cache modes"
   "Service file generator with rate limiting"
   "SDP architecture with cicada identity integration"
   "Zig FFI orchestrator for cross-platform cloud access"
   "Wolfi-containerized for security"))

 (what-this-is-not
  ("Not a file manager"
   "Not a sync tool itself (uses rclone)"
   "Not a cloud storage provider"
   "Not a VPN service (uses WireGuard)"
   "Not a key management service (uses cicada)")))

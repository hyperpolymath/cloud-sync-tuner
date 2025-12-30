; SPDX-License-Identifier: AGPL-3.0-or-later
; Cloud Sync Tuner - Ecosystem Position

(ecosystem
 (version . "1.0")
 (name . "cloud-sync-tuner")
 (type . "tool")
 (purpose . "Configure rclone cloud mounts with rate limiting")

 (position-in-ecosystem
  (layer . "configuration")
  (domain . "cloud-storage"))

 (related-projects
  ((name . "laminar")
   (relationship . "sibling-standard")
   (description . "Cloud-to-cloud streaming transfers")
   (integration . "Shares rclone as data plane"))

  ((name . "rclone")
   (relationship . "dependency")
   (description . "Multi-cloud sync tool")
   (integration . "Generates service files for rclone mounts"))

  ((name . "aria2")
   (relationship . "optional-enhancement")
   (description . "Download accelerator")
   (integration . "Optional compose profile for faster downloads"))

  ((name . "pssh")
   (relationship . "optional-enhancement")
   (description . "Parallel SSH execution")
   (integration . "Multi-host deployment of generated configs"))

  ((name . "cicada")
   (relationship . "security-integration")
   (description . "Quantum-resistant cryptographic identity management")
   (integration . "SDP identity verification, post-quantum key generation"))

  ((name . "wireguard")
   (relationship . "network-layer")
   (description . "Modern VPN protocol")
   (integration . "SDP tunnel establishment before cloud access")))

 (what-this-is
  ("TUI for selecting rclone VFS cache modes"
   "Service file generator with rate limiting"
   "Wolfi-containerized for security"))

 (what-this-is-not
  ("Not a file manager"
   "Not a sync tool itself (uses rclone)"
   "Not a cloud storage provider")))

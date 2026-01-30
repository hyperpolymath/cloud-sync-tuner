; SPDX-License-Identifier: MPL-2.0-or-later
; Cloud Sync Tuner - Project State

(define state
  '((metadata
     (version . "0.2.0")
     (schema-version . "1.0")
     (created . "2025-12-30")
     (updated . "2025-12-30")
     (project . "cloud-sync-tuner")
     (repo . "https://github.com/hyperpolymath/cloud-sync-tuner"))

    (project-context
     (name . "Cloud Sync Tuner")
     (tagline . "Secure cloud mount configuration with SDP and Zig FFI")
     (tech-stack . (ada gnat zig rclone wireguard fuse wolfi nerdctl)))

    (current-position
     (phase . "zig-ffi-integration")
     (overall-completion . 45)
     (components
      ((ada-tui . 100)
       (container . 90)
       (documentation . 90)
       (zig-wireguard . 100)
       (zig-rclone . 100)
       (zig-fuse-ext . 100)
       (sdp-design . 100)
       (cicada-integration . 30)
       (aria2-integration . 30)
       (pssh-integration . 0)
       (laminar-integration . 10))))

    (route-to-mvp
     ((milestone . "v0.1.0 - Foundation")
      (status . "complete")
      (items
       ((done . "Core TUI implementation")
        (done . "Wolfi containerization")
        (done . "nerdctl compose")
        (done . "RSR compliance"))))

     ((milestone . "v0.2.0 - Zig FFI")
      (status . "in-progress")
      (items
       ((done . "zig-wireguard repo and implementation")
        (done . "zig-rclone repo and implementation")
        (done . "zig-fuse-ext repo and implementation")
        (todo . "Wire Zig libraries into Ada TUI")
        (todo . "Cross-platform build testing"))))

     ((milestone . "v0.3.0 - SDP")
      (status . "planned")
      (items
       ((done . "Cicada isolation architecture")
        (todo . "Unix socket IPC implementation")
        (todo . "WireGuard tunnel integration")
        (todo . "Key rotation automation")))))

    (blockers-and-issues
     (critical . ())
     (high
      (("Zig-Ada interop" . "Need to define FFI boundary between Ada TUI and Zig libs")))
     (medium
      (("aria2 RPC" . "aria2 integration not started")
       ("pssh deployment" . "Multi-host deployment scripts needed")))
     (low . ()))

    (critical-next-actions
     (immediate
      ("Wire zig-wireguard into cloud-sync container"
       "Test cicada Unix socket communication"))
     (this-week
      ("Implement aria2 download acceleration"
       "Create pssh deployment helpers"
       "Test cross-platform builds"))
     (this-month
      ("Complete SDP tunnel establishment"
       "Security hardening (seccomp, MAC policies)"
       "Integration tests")))

    (session-history
     ((date . "2025-12-30")
      (session . "initial-development")
      (accomplishments
       ("Created Ada TUI"
        "Wolfi containerization"
        "nerdctl compose with profiles"
        "RSR compliance files")))

     ((date . "2025-12-30")
      (session . "sdp-and-zig-ffi")
      (accomplishments
       ("SDP architecture design"
        "Cicada isolation documentation"
        "Created zig-wireguard repo"
        "Created zig-rclone repo"
        "Created zig-fuse-ext repo"
        "Pushed all repos to GitHub"
        "Updated README with Zig FFI section"
        "Created ROADMAP.adoc"
        "Updated META.scm with new ADRs"
        "Updated ECOSYSTEM.scm with Zig repos"))))))

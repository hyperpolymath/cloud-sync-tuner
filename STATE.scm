; SPDX-License-Identifier: AGPL-3.0-or-later
; Cloud Sync Tuner - Project State

(define state
  '((metadata
     (version . "0.1.0")
     (schema-version . "1.0")
     (created . "2025-12-30")
     (updated . "2025-12-30")
     (project . "cloud-sync-tuner")
     (repo . "https://github.com/hyperpolymath/cloud-sync-tuner"))

    (project-context
     (name . "Cloud Sync Tuner")
     (tagline . "TUI for rclone cloud mount configuration")
     (tech-stack . (ada gnat rclone wolfi nerdctl)))

    (current-position
     (phase . "initial-release")
     (overall-completion . 70)
     (components
      ((ada-tui . 100)
       (container . 90)
       (documentation . 80)
       (aria2-integration . 30)
       (pssh-integration . 0)
       (laminar-integration . 0))))

    (route-to-mvp
     ((milestone . "v0.1.0")
      (items
       ((done . "Core TUI implementation")
        (done . "Wolfi containerization")
        (done . "nerdctl compose")
        (todo . "aria2 RPC integration")
        (todo . "pssh deployment scripts")))))

    (critical-next-actions
     (immediate
      ("Test container build on Wolfi"
       "Verify GNAT availability"))
     (this-week
      ("Add aria2 download acceleration"
       "Create pssh deployment helpers")))))

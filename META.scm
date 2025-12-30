; SPDX-License-Identifier: AGPL-3.0-or-later
; Cloud Sync Tuner - Meta Information

(define meta
  '((architecture-decisions
     ((adr-001
       (status . "accepted")
       (date . "2025-12-30")
       (title . "Use Ada for TUI implementation")
       (context . "Need cross-platform TUI with minimal dependencies")
       (decision . "Ada/GNAT chosen for type safety and portability")
       (consequences . ("Excellent i386/amd64/ARM support"
                       "Limited mobile/Minix without libcurl fallback")))

      (adr-002
       (status . "accepted")
       (date . "2025-12-30")
       (title . "Default to writes cache mode")
       (context . "Dropbox rate limiting breaks full cache mode")
       (decision . "writes mode balances performance vs API usage")
       (consequences . ("Prevents rate limiting"
                       "Reasonable disk usage"
                       "Good for daily workflows")))

      (adr-003
       (status . "proposed")
       (date . "2025-12-30")
       (title . "Optional aria2 acceleration")
       (context . "Large file downloads benefit from parallelism")
       (decision . "aria2 as optional compose profile")
       (consequences . ("3-10x download speedup"
                       "Additional container dependency")))))

    (development-practices
     (code-style . "GNAT style guidelines")
     (security . "SPDX headers, minimal privileges")
     (testing . "gprbuild checks")
     (versioning . "semver")
     (documentation . "AsciiDoc"))

    (design-rationale
     (why-ada . "Type safety, cross-platform, no runtime dependencies")
     (why-wolfi . "Security-focused, minimal attack surface")
     (why-writes-default . "Best balance for Dropbox rate limits"))))

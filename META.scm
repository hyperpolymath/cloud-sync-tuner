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
       (status . "accepted")
       (date . "2025-12-30")
       (title . "Optional aria2 acceleration")
       (context . "Large file downloads benefit from parallelism")
       (decision . "aria2 as optional compose profile")
       (consequences . ("3-10x download speedup"
                       "Additional container dependency")))

      (adr-004
       (status . "accepted")
       (date . "2025-12-30")
       (title . "Zig FFI for cross-platform libraries")
       (context . "Need portable bindings to libwireguard, librclone, libfuse3")
       (decision . "Use Zig @cImport for direct C header binding")
       (consequences . ("Single codebase for all platforms"
                       "No manual binding maintenance"
                       "Compile-time type safety"
                       "Requires Zig toolchain")))

      (adr-005
       (status . "accepted")
       (date . "2025-12-30")
       (title . "Network-isolated cicada container")
       (context . "Post-quantum identity/key material must never touch network")
       (decision . "cicada runs with network_mode: none, Unix socket IPC only")
       (consequences . ("Key material never on network interfaces"
                       "Defense in depth against exfiltration"
                       "Requires bind-mounted socket directory"
                       "Slightly more complex container orchestration")))

      (adr-006
       (status . "accepted")
       (date . "2025-12-30")
       (title . "SDP architecture with WireGuard")
       (context . "Zero-trust model requires authenticated tunnel before cloud access")
       (decision . "WireGuard VPN established before any rclone operations")
       (consequences . ("All cloud traffic encrypted at network layer"
                       "Identity verified before data access"
                       "Requires WireGuard kernel module or userspace impl")))))

    (development-practices
     (code-style . "GNAT style for Ada, Zig style guide for Zig")
     (security . "SPDX headers, minimal privileges, network isolation")
     (testing . "gprbuild checks, zig build test")
     (versioning . "semver")
     (documentation . "AsciiDoc"))

    (design-rationale
     (why-ada . "Type safety, cross-platform, no runtime dependencies")
     (why-zig . "C interop via @cImport, cross-compilation, memory safety")
     (why-wolfi . "Security-focused, minimal attack surface")
     (why-writes-default . "Best balance for Dropbox rate limits")
     (why-cicada-isolation . "Key material must never touch network")
     (why-sdp . "Zero-trust requires identity verification before access"))))

<!-- SPDX-License-Identifier: PMPL-1.0-or-later -->
<!-- TOPOLOGY.md — Project architecture map and completion dashboard -->
<!-- Last updated: 2026-02-19 -->

# Cloud Sync Tuner — Project Topology

## System Architecture

```
                        ┌─────────────────────────────────────────┐
                        │              USER / TERMINAL            │
                        │        (Ada TUI / CLI Interface)        │
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │           CLOUD SYNC TUNER (ADA)        │
                        │    (Configuration, VFS Management)      │
                        └──────────┬───────────────────┬──────────┘
                                   │                   │
                                   ▼                   ▼
                        ┌───────────────────────┐  ┌────────────────────────────────┐
                        │ ZIG FFI INTERFACE     │  │ SDP SECURITY (CICADA)          │
                        │ - zig-rclone          │  │ - Post-quantum Identity        │
                        │ - zig-wireguard       │  │ - Unix Socket IPC              │
                        │ - zig-fuse-ext        │  │ - WireGuard Tunnel             │
                        └──────────┬────────────┘  └──────────┬─────────────────────┘
                                   │                          │
                                   └────────────┬─────────────┘
                                                ▼
                        ┌─────────────────────────────────────────┐
                        │             SYSTEM LAYER                │
                        │  ┌───────────┐  ┌───────────┐  ┌───────┐│
                        │  │ librclone │  │ libfuse3  │  │ libwg ││
                        │  └───────────┘  └───────────┘  └───────┘│
                        └───────────────────┬─────────────────────┘
                                            │
                                            ▼
                        ┌─────────────────────────────────────────┐
                        │          EXTERNAL CLOUD APIS            │
                        │      (Dropbox, GDrive, OneDrive)        │
                        └─────────────────────────────────────────┘

                        ┌─────────────────────────────────────────┐
                        │          REPO INFRASTRUCTURE            │
                        │  Justfile / GPR     .machine_readable/  │
                        │  Wolfi Containers   Laminar Integration │
                        └─────────────────────────────────────────┘
```

## Completion Dashboard

```
COMPONENT                          STATUS              NOTES
─────────────────────────────────  ──────────────────  ─────────────────────────────────
CORE APPLICATION
  Ada TUI Interface                 ██████████ 100%    Menu navigation stable
  rclone VFS Config                 ██████████ 100%    Optimal cache modes verified
  Smart Sync Logic                  ████████░░  80%    Bandwidth scheduling refining

INTERFACE & SECURITY
  Zig FFI Bridge                    ██████████ 100%    rclone/wg/fuse bindings stable
  SDP (Cicada)                      ██████████ 100%    PQ identity integration active
  FUSE Mount Management             ██████████ 100%    Mount/unmount verified

DESKTOP & ACCEL
  System Tray Daemon                ██████░░░░  60%    Sync status icon active
  File Manager Ext                  ████░░░░░░  40%    Nautilus/Dolphin icons pending
  aria2 Acceleration                ██████████ 100%    Multi-connection fetch active

REPO INFRASTRUCTURE
  Justfile                          ██████████ 100%    Standard build tasks
  Wolfi/nerdctl build               ██████████ 100%    Reproducible containers
  .machine_readable/                ██████████ 100%    STATE.a2ml tracking

─────────────────────────────────────────────────────────────────────────────
OVERALL:                            █████████░  ~90%   v1.0 Production-ready
```

## Key Dependencies

```
Ada TUI ──────► Zig FFI ──────► libfuse3 ──────► OS Mount
                  │                │
                  ▼                ▼
            libwireguard ───► librclone ───► Cloud API
```

## Update Protocol

This file is maintained by both humans and AI agents. When updating:

1. **After completing a component**: Change its bar and percentage
2. **After adding a component**: Add a new row in the appropriate section
3. **After architectural changes**: Update the ASCII diagram
4. **Date**: Update the `Last updated` comment at the top of this file

Progress bars use: `█` (filled) and `░` (empty), 10 characters wide.
Percentages: 0%, 10%, 20%, ... 100% (in 10% increments).

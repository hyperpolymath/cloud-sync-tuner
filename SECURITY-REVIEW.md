# Security and UX Seam Review

## Component Integration Points

### 1. TUI ↔ Config File (`config.toml`)

**Security:**
- ✓ Config file in user home (~/.config/), not world-readable
- ✓ No credentials stored in config (rclone handles auth separately)
- ⚠ **TODO**: Add config file validation before parsing
- ⚠ **TODO**: Add file permission check (should be 0600)

**UX:**
- ✓ TUI reads existing config on startup
- ⚠ **TODO**: Show warning if config has syntax errors
- ⚠ **TODO**: Add config file backup before overwrite

### 2. TUI ↔ systemd Services

**Security:**
- ✓ Services run as user (not root)
- ✓ Services use `NoNewPrivileges=true`
- ✓ `ProtectSystem=strict` limits writes
- ✓ Generated services go to user's systemd dir

**UX:**
- ✓ Clear feedback on apply success/failure
- ⚠ **TODO**: Show diff before applying changes
- ⚠ **TODO**: Add rollback capability

### 3. Overlay Daemon ↔ rclone RC

**Security:**
- ✓ Connects only to localhost
- ✓ Timeouts on RC requests (2 seconds)
- ⚠ **TODO**: Add authentication to RC if exposed
- ⚠ **RISK**: RC port could be accessed by local malware

**Mitigation**: Document that RC should bind to 127.0.0.1 only

**UX:**
- ✓ Graceful degradation if rclone not responding
- ✓ Status caching to reduce queries

### 4. Tray Daemon ↔ Health Check

**Security:**
- ✓ Health check is read-only
- ✓ No sensitive data in status output

**UX:**
- ✓ Desktop notifications for status changes
- ✓ Menu actions for common tasks
- ⚠ **TODO**: Add notification throttling (avoid spam)

### 5. SELinux Policy ↔ File Operations

**Security:**
- ✓ Confined rclone_t domain
- ✓ Cache files labeled rclone_cache_t
- ✓ Mount points labeled rclone_mount_t
- ✓ Denies access to shadow/etc
- ✓ Boolean tunables for debugging

**UX:**
- ⚠ **TODO**: Add semanage commands to install.sh
- ⚠ **TODO**: Document SELinux troubleshooting

### 6. Install Script ↔ Components

**Security:**
- ✓ Uses install(1) with explicit permissions
- ✓ Doesn't require root for most operations
- ✓ SELinux/audit install prompts for sudo
- ⚠ **TODO**: Verify downloaded checksums if fetching

**UX:**
- ✓ Progress messages with colors
- ✓ Uninstall option
- ✓ Dry-run capability needed

### 7. Nautilus Extension ↔ D-Bus

**Security:**
- ✓ D-Bus session bus (user only)
- ✓ Read-only status queries
- ⚠ Python extension runs in Nautilus process

**UX:**
- ✓ Emblems update automatically
- ⚠ **TODO**: Handle D-Bus connection failure gracefully

### 8. Watchdog Timer ↔ Services

**Security:**
- ✓ Only restarts services that should be running
- ✓ notify-send for visibility

**UX:**
- ✓ 5-minute check interval (not too aggressive)
- ⚠ **TODO**: Exponential backoff on repeated failures

## Privilege Escalation Review

| Operation | Requires sudo | Justification |
|-----------|---------------|---------------|
| Install binaries | No | User's ~/.local |
| Install services | No | User's systemd |
| Install SELinux | **Yes** | System policy |
| Install audit rules | **Yes** | System audit |
| Install Nautilus ext | No | User's data dir |

## Data Flow Security

```
User Input (TUI)
     ↓
Config Validation (TODO: add)
     ↓
Service File Generation
     ↓
systemd --user
     ↓
rclone (confined by SELinux)
     ↓
FUSE mount (rclone_mount_t)
     ↓
Cloud API (HTTPS only)
```

## Recommendations for v1.0

### Must Fix Before Release

1. **Config validation**: Add schema validation before parsing
2. **Permission checks**: Verify config file permissions

### Should Fix (can be post-v1.0)

1. Add notification throttling
2. Add config backup before overwrite
3. Add change diff preview
4. Document SELinux troubleshooting

### Nice to Have

1. Dry-run for install script
2. Exponential backoff in watchdog
3. RC authentication support

## UX Improvements Identified

1. **Error messages**: Make validation errors more helpful
2. **First-run experience**: Detect if rclone not configured
3. **Status bar**: Show sync progress in TUI
4. **Keyboard shortcuts**: Document all shortcuts in TUI header

## Conclusion

The v1.0 architecture is sound with appropriate security boundaries. The main gaps are:
- Input validation for config files
- Some edge cases in error handling

Recommend proceeding with v1.0 release after adding config validation.

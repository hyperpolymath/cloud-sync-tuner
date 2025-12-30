<!-- SPDX-License-Identifier: AGPL-3.0-or-later -->
# Security Policy

## Supported Versions

| Version | Supported          |
| ------- | ------------------ |
| 0.1.x   | :white_check_mark: |

## Reporting a Vulnerability

Please report security vulnerabilities via GitHub Security Advisories:
https://github.com/hyperpolymath/cloud-sync-tuner/security/advisories/new

Do NOT create public issues for security vulnerabilities.

## Security Considerations

This tool generates systemd service files that:
- Mount cloud storage with FUSE
- May contain sensitive OAuth tokens in rclone config
- Require appropriate file permissions (600 for service files)

Always review generated service files before applying.

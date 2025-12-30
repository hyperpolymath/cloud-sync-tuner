# SPDX-License-Identifier: AGPL-3.0-or-later
Name:           cloud-sync-tuner
Version:        1.0.0
Release:        1%{?dist}
Summary:        Ada TUI for managing rclone cloud mounts with rate limiting

License:        AGPL-3.0-or-later
URL:            https://github.com/hyperpolymath/cloud-sync-tuner
Source0:        %{url}/archive/v%{version}/%{name}-%{version}.tar.gz

BuildRequires:  gcc-gnat
BuildRequires:  gprbuild
BuildRequires:  rust
BuildRequires:  cargo
BuildRequires:  fuse3-devel
BuildRequires:  systemd-rpm-macros

Requires:       rclone
Requires:       fuse3
Recommends:     systemd
Suggests:       libnotify
Suggests:       nautilus-python
Suggests:       selinux-policy-devel
Suggests:       audit

%description
Cloud Sync Tuner provides an interactive terminal interface for
configuring rclone cloud mounts with optimal rate limiting settings.
It generates systemd service files with appropriate VFS cache modes
to prevent API rate limiting from cloud providers like Dropbox.

Features:
- TUI for cache mode selection (off, minimal, writes, full)
- Rate limiting configuration optimized per provider
- Smart sync with bandwidth scheduling
- SELinux policy for enterprise security
- Desktop integration (Nautilus, Dolphin)
- System tray daemon with notifications

%prep
%autosetup

%build
gprbuild -P cloud_sync_tuner.gpr -XBUILD_MODE=release
cd overlay-daemon && cargo build --release && cd ..
cd tray-daemon && cargo build --release && cd ..

%check
gprbuild -P tests/tests.gpr
./bin/test_runner
./bin/verify_formal

%install
install -Dm755 bin/cloud_sync_tuner %{buildroot}%{_bindir}/cloud-sync-tuner
install -Dm755 overlay-daemon/target/release/cloud-sync-overlay %{buildroot}%{_bindir}/cloud-sync-overlay
install -Dm755 tray-daemon/target/release/cloud-sync-tray %{buildroot}%{_bindir}/cloud-sync-tray
install -Dm755 scripts/cloud-sync-status %{buildroot}%{_bindir}/cloud-sync-status

install -Dm644 systemd/rclone-dropbox.service %{buildroot}%{_userunitdir}/rclone-dropbox.service
install -Dm644 systemd/cloud-sync-watchdog.service %{buildroot}%{_userunitdir}/cloud-sync-watchdog.service
install -Dm644 systemd/cloud-sync-watchdog.timer %{buildroot}%{_userunitdir}/cloud-sync-watchdog.timer
install -Dm644 overlay-daemon/cloud-sync-overlay.service %{buildroot}%{_userunitdir}/cloud-sync-overlay.service
install -Dm644 tray-daemon/cloud-sync-tray.service %{buildroot}%{_userunitdir}/cloud-sync-tray.service

install -Dm644 man/cloud-sync-tuner.1 %{buildroot}%{_mandir}/man1/cloud-sync-tuner.1
install -Dm644 config/config.toml.example %{buildroot}%{_datadir}/%{name}/config.toml.example
install -Dm644 config/schema.ncl %{buildroot}%{_datadir}/%{name}/schema.ncl

install -Dm644 nautilus-extension/cloud_sync_overlay.py %{buildroot}%{_datadir}/nautilus-python/extensions/cloud_sync_overlay.py
install -Dm644 dolphin-extension/cloud_sync_overlay.desktop %{buildroot}%{_datadir}/kservices5/ServiceMenus/cloud_sync_overlay.desktop

install -Dm644 selinux/cloud_sync_tuner.te %{buildroot}%{_datadir}/%{name}/selinux/cloud_sync_tuner.te
install -Dm644 selinux/cloud_sync_tuner.fc %{buildroot}%{_datadir}/%{name}/selinux/cloud_sync_tuner.fc
install -Dm644 selinux/cloud_sync_tuner.if %{buildroot}%{_datadir}/%{name}/selinux/cloud_sync_tuner.if
install -Dm644 audit/cloud-sync-tuner.rules %{buildroot}%{_datadir}/%{name}/audit/cloud-sync-tuner.rules

%files
%license LICENSE
%doc README.adoc SECURITY-REVIEW.md
%{_bindir}/cloud-sync-tuner
%{_bindir}/cloud-sync-overlay
%{_bindir}/cloud-sync-tray
%{_bindir}/cloud-sync-status
%{_userunitdir}/rclone-dropbox.service
%{_userunitdir}/cloud-sync-watchdog.service
%{_userunitdir}/cloud-sync-watchdog.timer
%{_userunitdir}/cloud-sync-overlay.service
%{_userunitdir}/cloud-sync-tray.service
%{_mandir}/man1/cloud-sync-tuner.1*
%{_datadir}/%{name}/
%{_datadir}/nautilus-python/extensions/cloud_sync_overlay.py
%{_datadir}/kservices5/ServiceMenus/cloud_sync_overlay.desktop

%post
%systemd_user_post cloud-sync-watchdog.timer cloud-sync-overlay.service cloud-sync-tray.service

%preun
%systemd_user_preun cloud-sync-watchdog.timer cloud-sync-overlay.service cloud-sync-tray.service

%changelog
* Mon Dec 30 2025 hyperpolymath <hyperpolymath@users.noreply.github.com> - 1.0.0-1
- Initial release

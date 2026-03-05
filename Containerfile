# SPDX-License-Identifier: AGPL-3.0-or-later
# Cloud Sync Tuner - Wolfi-based container image

FROM cgr.dev/chainguard/wolfi-base:latest AS builder

# Install build dependencies
RUN apk add --no-cache \
    gnat \
    gprbuild \
    rust \
    cargo \
    fuse3-dev \
    pkgconf \
    dbus-dev

WORKDIR /build
COPY . .

# Build Ada TUI
RUN gprbuild -P cloud_sync_tuner.gpr -XBUILD_MODE=release

# Build Rust daemons
RUN cd overlay-daemon && cargo build --release
RUN cd tray-daemon && cargo build --release

# Runtime image
FROM cgr.dev/chainguard/wolfi-base:latest

RUN apk add --no-cache \
    rclone \
    fuse3 \
    dbus \
    libnotify \
    tini

COPY --from=builder /build/bin/cloud_sync_tuner /usr/bin/cloud-sync-tuner
COPY --from=builder /build/overlay-daemon/target/release/cloud-sync-overlay /usr/bin/
COPY --from=builder /build/tray-daemon/target/release/cloud-sync-tray /usr/bin/
COPY --from=builder /build/scripts/cloud-sync-status /usr/bin/
COPY --from=builder /build/config/config.toml.example /etc/cloud-sync-tuner/config.toml.example

# Create non-root user
RUN adduser -D -u 1000 sync
USER sync
WORKDIR /home/sync

# FUSE needs /dev/fuse
VOLUME ["/home/sync/.config/rclone", "/home/sync/.cache/rclone", "/mnt/cloud"]

ENTRYPOINT ["/sbin/tini", "--"]
CMD ["cloud-sync-tuner"]

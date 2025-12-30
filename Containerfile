# SPDX-License-Identifier: AGPL-3.0-or-later
# Cloud Sync Tuner - Wolfi-based container
# Build with: nerdctl build -t cloud-sync-tuner .

# Stage 1: Build environment (Wolfi with GNAT)
FROM cgr.dev/chainguard/wolfi-base:latest AS builder

# Install build dependencies
RUN apk add --no-cache \
    gcc \
    gnat \
    gprbuild \
    make \
    git

WORKDIR /build

# Copy source files
COPY src/ src/
COPY cloud_sync_tuner.gpr .

# Build the application
RUN mkdir -p obj bin && \
    gprbuild -P cloud_sync_tuner.gpr -XBUILD_MODE=release

# Stage 2: Runtime environment (minimal Wolfi, hardened)
FROM cgr.dev/chainguard/wolfi-base:latest AS runtime

# Install runtime dependencies only
RUN apk add --no-cache \
    rclone \
    fuse3 \
    libgcc

# Create non-root user with specific UID for consistency
RUN adduser -D -u 1000 -h /home/tuner tuner

# Security: read-only filesystem prep
RUN mkdir -p /home/tuner/.config/cloud-sync-tuner \
             /home/tuner/output \
    && chown -R tuner:tuner /home/tuner

# Copy binary from builder
COPY --from=builder --chown=root:root --chmod=755 /build/bin/cloud_sync_tuner /usr/local/bin/

# Copy default config template
COPY --chown=tuner:tuner config/ /home/tuner/.config/cloud-sync-tuner/

# Switch to non-root user
USER tuner
WORKDIR /home/tuner

# Volume for output service files
VOLUME ["/home/tuner/output"]

# Environment for XDG compliance
ENV XDG_CONFIG_HOME=/home/tuner/.config
ENV HOME=/home/tuner

# Security: drop all capabilities, minimal attack surface
# Note: SYS_ADMIN still needed at runtime for FUSE, added in compose

# Default to interactive TUI mode
ENTRYPOINT ["/usr/local/bin/cloud_sync_tuner"]

# Health check
HEALTHCHECK --interval=30s --timeout=3s \
    CMD test -x /usr/local/bin/cloud_sync_tuner || exit 1

LABEL org.opencontainers.image.title="Cloud Sync Tuner" \
      org.opencontainers.image.description="TUI for managing rclone cloud mount configurations" \
      org.opencontainers.image.source="https://github.com/hyperpolymath/cloud-sync-tuner" \
      org.opencontainers.image.licenses="AGPL-3.0-or-later" \
      org.opencontainers.image.vendor="hyperpolymath"

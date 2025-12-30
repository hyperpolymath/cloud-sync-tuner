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

# Stage 2: Runtime environment (minimal Wolfi)
FROM cgr.dev/chainguard/wolfi-base:latest AS runtime

# Install runtime dependencies only
RUN apk add --no-cache \
    rclone \
    fuse3 \
    libgcc

# Create non-root user
RUN adduser -D -h /home/tuner tuner
USER tuner
WORKDIR /home/tuner

# Copy binary from builder
COPY --from=builder /build/bin/cloud_sync_tuner /usr/local/bin/

# Copy default config template
COPY --chown=tuner:tuner config/ /home/tuner/.config/cloud-sync-tuner/

# Volume for output service files
VOLUME ["/home/tuner/output"]

# Default to interactive TUI mode
ENTRYPOINT ["/usr/local/bin/cloud_sync_tuner"]

# Health check
HEALTHCHECK --interval=30s --timeout=3s \
    CMD ["/usr/local/bin/cloud_sync_tuner", "--version"] || exit 1

LABEL org.opencontainers.image.title="Cloud Sync Tuner" \
      org.opencontainers.image.description="TUI for managing rclone cloud mount configurations" \
      org.opencontainers.image.source="https://github.com/hyperpolymath/cloud-sync-tuner" \
      org.opencontainers.image.licenses="AGPL-3.0-or-later" \
      org.opencontainers.image.vendor="hyperpolymath"

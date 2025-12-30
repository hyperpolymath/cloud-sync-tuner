# SPDX-License-Identifier: AGPL-3.0-or-later
class CloudSyncTuner < Formula
  desc "Ada TUI for managing rclone cloud mounts with rate limiting"
  homepage "https://github.com/hyperpolymath/cloud-sync-tuner"
  url "https://github.com/hyperpolymath/cloud-sync-tuner/archive/v1.0.0.tar.gz"
  sha256 "PLACEHOLDER"
  license "AGPL-3.0-or-later"
  head "https://github.com/hyperpolymath/cloud-sync-tuner.git", branch: "main"

  depends_on "gprbuild" => :build
  depends_on "gcc" => :build  # for gnat
  depends_on "rust" => :build
  depends_on "rclone"
  depends_on "macfuse"

  def install
    system "gprbuild", "-P", "cloud_sync_tuner.gpr", "-XBUILD_MODE=release"

    cd "overlay-daemon" do
      system "cargo", "build", "--release"
    end

    cd "tray-daemon" do
      system "cargo", "build", "--release"
    end

    bin.install "bin/cloud_sync_tuner" => "cloud-sync-tuner"
    bin.install "overlay-daemon/target/release/cloud-sync-overlay"
    bin.install "tray-daemon/target/release/cloud-sync-tray"
    bin.install "scripts/cloud-sync-status"

    man1.install "man/cloud-sync-tuner.1"

    pkgshare.install "config/config.toml.example"
    pkgshare.install "config/schema.ncl"

    doc.install "README.adoc"
    doc.install "SECURITY-REVIEW.md"
  end

  def caveats
    <<~EOS
      To start the tray daemon:
        brew services start cloud-sync-tuner

      Copy example config to get started:
        mkdir -p ~/.config/cloud-sync-tuner
        cp #{pkgshare}/config.toml.example ~/.config/cloud-sync-tuner/config.toml

      macFUSE is required for FUSE mounts. After installation:
        1. Reboot your Mac
        2. Allow kernel extension in System Preferences > Security & Privacy
    EOS
  end

  service do
    run [opt_bin/"cloud-sync-tray"]
    keep_alive true
    log_path var/"log/cloud-sync-tuner.log"
    error_log_path var/"log/cloud-sync-tuner.log"
  end

  test do
    assert_match "1.0.0", shell_output("#{bin}/cloud-sync-tuner --version")
    assert_match "Usage:", shell_output("#{bin}/cloud-sync-tuner --help")
  end
end

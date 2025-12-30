# SPDX-License-Identifier: AGPL-3.0-or-later
{ lib
, stdenv
, fetchFromGitHub
, gnat
, gprbuild
, rustPlatform
, rclone
, fuse3
, pkg-config
, dbus
, systemd
}:

let
  version = "1.0.0";

  src = fetchFromGitHub {
    owner = "hyperpolymath";
    repo = "cloud-sync-tuner";
    rev = "v${version}";
    sha256 = lib.fakeSha256;
  };

  overlay-daemon = rustPlatform.buildRustPackage {
    pname = "cloud-sync-overlay";
    inherit version src;
    sourceRoot = "source/overlay-daemon";
    cargoSha256 = lib.fakeSha256;
    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ dbus ];
  };

  tray-daemon = rustPlatform.buildRustPackage {
    pname = "cloud-sync-tray";
    inherit version src;
    sourceRoot = "source/tray-daemon";
    cargoSha256 = lib.fakeSha256;
    nativeBuildInputs = [ pkg-config ];
    buildInputs = [ dbus ];
  };

in stdenv.mkDerivation {
  pname = "cloud-sync-tuner";
  inherit version src;

  nativeBuildInputs = [
    gnat
    gprbuild
    pkg-config
  ];

  buildInputs = [
    fuse3
  ];

  buildPhase = ''
    runHook preBuild
    gprbuild -P cloud_sync_tuner.gpr -XBUILD_MODE=release
    runHook postBuild
  '';

  installPhase = ''
    runHook preInstall

    install -Dm755 bin/cloud_sync_tuner $out/bin/cloud-sync-tuner
    install -Dm755 ${overlay-daemon}/bin/cloud-sync-overlay $out/bin/cloud-sync-overlay
    install -Dm755 ${tray-daemon}/bin/cloud-sync-tray $out/bin/cloud-sync-tray
    install -Dm755 scripts/cloud-sync-status $out/bin/cloud-sync-status

    install -Dm644 man/cloud-sync-tuner.1 $out/share/man/man1/cloud-sync-tuner.1

    install -Dm644 systemd/rclone-dropbox.service $out/lib/systemd/user/rclone-dropbox.service
    install -Dm644 systemd/cloud-sync-watchdog.service $out/lib/systemd/user/cloud-sync-watchdog.service
    install -Dm644 systemd/cloud-sync-watchdog.timer $out/lib/systemd/user/cloud-sync-watchdog.timer
    install -Dm644 overlay-daemon/cloud-sync-overlay.service $out/lib/systemd/user/cloud-sync-overlay.service
    install -Dm644 tray-daemon/cloud-sync-tray.service $out/lib/systemd/user/cloud-sync-tray.service

    install -Dm644 config/config.toml.example $out/share/cloud-sync-tuner/config.toml.example
    install -Dm644 config/schema.ncl $out/share/cloud-sync-tuner/schema.ncl

    install -Dm644 nautilus-extension/cloud_sync_overlay.py $out/share/nautilus-python/extensions/cloud_sync_overlay.py

    runHook postInstall
  '';

  meta = with lib; {
    description = "Ada TUI for managing rclone cloud mounts with rate limiting";
    homepage = "https://github.com/hyperpolymath/cloud-sync-tuner";
    license = licenses.agpl3Plus;
    maintainers = with maintainers; [ ];
    platforms = platforms.linux;
    mainProgram = "cloud-sync-tuner";
  };
}

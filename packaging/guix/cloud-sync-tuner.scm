;;; SPDX-License-Identifier: AGPL-3.0-or-later
;;; Cloud Sync Tuner - Guix package definition

(define-module (cloud-sync-tuner)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cargo)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages ada)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages freedesktop))

(define-public cloud-sync-overlay
  (package
    (name "cloud-sync-overlay")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyperpolymath/cloud-sync-tuner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "PLACEHOLDER"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-tokio" ,rust-tokio)
                       ("rust-zbus" ,rust-zbus)
                       ("rust-reqwest" ,rust-reqwest))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "overlay-daemon"))))))
    (native-inputs (list pkg-config))
    (inputs (list dbus))
    (home-page "https://github.com/hyperpolymath/cloud-sync-tuner")
    (synopsis "D-Bus daemon for cloud sync file status overlay")
    (description "Monitors rclone transfers and exposes sync status via D-Bus
for file manager integration.")
    (license license:agpl3+)))

(define-public cloud-sync-tray
  (package
    (name "cloud-sync-tray")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyperpolymath/cloud-sync-tuner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "PLACEHOLDER"))))
    (build-system cargo-build-system)
    (arguments
     `(#:cargo-inputs (("rust-tokio" ,rust-tokio)
                       ("rust-zbus" ,rust-zbus)
                       ("rust-notify-rust" ,rust-notify-rust))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "tray-daemon"))))))
    (native-inputs (list pkg-config))
    (inputs (list dbus libnotify))
    (home-page "https://github.com/hyperpolymath/cloud-sync-tuner")
    (synopsis "System tray daemon for cloud sync status")
    (description "Shows cloud sync status in the system tray with
notifications for sync events.")
    (license license:agpl3+)))

(define-public cloud-sync-tuner
  (package
    (name "cloud-sync-tuner")
    (version "1.0.0")
    (source
     (origin
       (method git-fetch)
       (uri (git-reference
             (url "https://github.com/hyperpolymath/cloud-sync-tuner")
             (commit (string-append "v" version))))
       (file-name (git-file-name name version))
       (sha256
        (base32 "PLACEHOLDER"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure (lambda _ #t))
         (replace 'build
           (lambda _
             (invoke "gprbuild" "-P" "cloud_sync_tuner.gpr"
                     "-XBUILD_MODE=release")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (install-file "bin/cloud_sync_tuner"
                            (string-append out "/bin"))
               (rename-file (string-append out "/bin/cloud_sync_tuner")
                           (string-append out "/bin/cloud-sync-tuner"))
               (install-file "scripts/cloud-sync-status"
                            (string-append out "/bin"))
               (install-file "man/cloud-sync-tuner.1"
                            (string-append out "/share/man/man1"))))))))
    (native-inputs
     (list gnat gprbuild))
    (inputs
     (list fuse-3))
    (propagated-inputs
     (list cloud-sync-overlay cloud-sync-tray rclone))
    (home-page "https://github.com/hyperpolymath/cloud-sync-tuner")
    (synopsis "Ada TUI for managing rclone cloud mounts with rate limiting")
    (description "Cloud Sync Tuner provides an interactive terminal interface
for configuring rclone cloud mounts with optimal rate limiting settings.
It generates systemd service files with appropriate VFS cache modes to
prevent API rate limiting from cloud providers like Dropbox.

Features include:
@itemize
@item TUI for cache mode selection (off, minimal, writes, full)
@item Rate limiting configuration optimized per provider
@item Smart sync with bandwidth scheduling
@item SELinux policy for enterprise security
@item Desktop integration (Nautilus, Dolphin)
@item System tray daemon with notifications
@end itemize")
    (license license:agpl3+)))

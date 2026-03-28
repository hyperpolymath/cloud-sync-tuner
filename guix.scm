; SPDX-License-Identifier: PMPL-1.0-or-later
;; guix.scm — GNU Guix package definition for cloud-sync-tuner
;; Usage: guix shell -f guix.scm

(use-modules (guix packages)
             (guix build-system gnu)
             (guix licenses))

(package
  (name "cloud-sync-tuner")
  (version "0.1.0")
  (source #f)
  (build-system gnu-build-system)
  (synopsis "cloud-sync-tuner")
  (description "cloud-sync-tuner — part of the hyperpolymath ecosystem.")
  (home-page "https://github.com/hyperpolymath/cloud-sync-tuner")
  (license ((@@ (guix licenses) license) "PMPL-1.0-or-later"
             "https://github.com/hyperpolymath/palimpsest-license")))

<!--
SPDX-License-Identifier: CC-BY-SA-4.0
SPDX-FileCopyrightText: 2026 Jonathan D.A. Jewell (hyperpolymath)
-->

# Changelog

All notable changes to `cloud-sync-tuner` will be documented in this file.

This file is generated from conventional commits by the
[`changelog-reusable.yml`](https://github.com/hyperpolymath/standards/blob/main/.github/workflows/changelog-reusable.yml)
workflow (`hyperpolymath/standards#206`). Adopt the workflow in this repo's CI to keep this file in sync automatically — see
[`templates/cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml)
for the canonical config.

The format follows [Keep a Changelog](https://keepachangelog.com/en/1.1.0/);
this project aims to follow [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added

- feat(crg): add crg-grade and crg-badge justfile recipes
- feat: add stapeln.toml layer-based container definition\n\nConverted from existing Containerfile to stapeln format.\nIncludes Chainguard base, security hardening, SBOM generation.\n\nCo-Authored-By: Claude Opus 4.6 (1M context) <noreply@anthropic.com>
- feat: deploy UX Manifesto infrastructure
- feat: add CLADE.a2ml — clade taxonomy declaration
- feat(ci): enable Hypatia scanning

### Fixed

- fix(ci): bump a2ml/k9-validate-action pins to canonical (standards#85) (#13)
- fix(ci): sync hypatia-scan.yml to canonical (kill cd-scanner build drift) (#12)
- fix(ci): adopt canonical hypatia-scan.yml (env.HOME/scanner-layout + Comment-step gate) (#11)
- fix(ci): Phase-2 fleet submission must not fail the security gate (#10)
- fix(ci): rsr-antipattern.yml duplicate heredoc (#7)
- fix(ci): repair YAML block-scalar in workflow-linter Check Permissions step (#8)
- fix(ci): move secret-scanner Cargo.toml gate from job-level if: to step-level (#9)
- fix(scorecard): enforce granular permissions and add fuzzing placeholder
- fix(ci): Resolve workflow-linter self-matching and metadata issues
- fix: global AGPL-3.0-or-later → PMPL-1.0-or-later replacement

### Changed

- refactor: migrate 6SCM → 6A2 (.scm → .a2ml format)

### Documentation

- docs: add TEST-NEEDS.md (CRG C)
- docs: add TEST-NEEDS.md (CRG C)
- docs: add EXPLAINME.adoc — prove-it file backing README claims
- docs: add checkpoint files for state tracking

### CI

- ci: redistribute concurrency-cancel guard to read-only check workflows (#15)
- ci: bump actions/upload-artifact SHA to current v4 (#6)
- ci: SHA-pin hyperpolymath validate-actions in dogfood-gate
- ci(antipattern): fix top-level dir matching + benchmarks/lsp/bench filename allowlists (#5)
- ci(antipattern): TS check reads .claude/CLAUDE.md exemption table (#4)

## Pre-history

Prior commits to this file's introduction are recorded in git history but not formally classified into Keep-a-Changelog sections. To backfill, run `git cliff -o CHANGELOG.md` locally using the canonical [`cliff.toml`](https://github.com/hyperpolymath/standards/blob/main/templates/cliff.toml) — this is one-shot mechanical work.

---

<!-- This file was seeded by the 2026-05-26 estate tech-debt audit follow-up (Row-2 Phase 3); see [`hyperpolymath/standards/docs/audits/2026-05-26-estate-documentation-debt.md`](https://github.com/hyperpolymath/standards/blob/main/docs/audits/2026-05-26-estate-documentation-debt.md). -->

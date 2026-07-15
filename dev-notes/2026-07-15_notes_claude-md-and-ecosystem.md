# hsical — CLAUDE.md added; canonical ecosystem file located

**Date:** 2026-07-15
**Type:** session notes
**Context:** first `CLAUDE.md` for hsical; reconciling with the HSItools ecosystem.

## What happened

Wrote `CLAUDE.md` for hsical. The first draft was written blind and made a wrong
claim (that no CLAUDE.md existed anywhere); corrected after Maury pointed at the local
repos. Net result: hsical's `CLAUDE.md` now defers to the canonical ecosystem file and
restates only what a hsical-only session needs.

## Findings worth keeping

- **There IS a canonical ecosystem CLAUDE.md** at `C:\GitHub\HSItools\CLAUDE.md`
  (v1.8.0, 2026-07-14) — the non-negotiable house rules for HSItools + zarowka +
  hsical. It was **not on GitHub** (local/unpushed), which is why the initial web
  check missed it. Claude Code auto-loads only the CWD repo's CLAUDE.md, so hsical's
  file restates the rules a hsical-only session needs and points at the ecosystem file
  for the rest.
- **Behavioral rules that bind hsical work** (ecosystem §0/§9): approach-before-code,
  one-change-at-a-time, **git belongs to Maury** (no state-changing git), verify where
  R lives, CLAUDE.md carries durable conventions only (time-bound notes go in dated
  dev-notes like this one).
- **Authoritative hsical spec** is `HSItools/dev-notes/hsi-interface-contract.md`
  (contract v3.0, schema v1.1.0) — the field→GUI-source map. Don't re-list the 32
  fields in hsical's CLAUDE.md (drift risk); point at the contract + validator.
- **Sidecar contract confirmed** from `deparse` of `hsi_create_metadata` /
  `new_hsi_metadata` / `validate_hsi_metadata` / `check_numeric` (installed HSItools
  0.5.3): 32 flat fields, only `name` required, all else `NULL`-when-absent.
  Strictly-positive numerics vs. the four any-sign fields (`camera_position_mm`,
  `stage_position_mm`, `dropped_frames`, `gcp_count`). `wavelengths`/`fwhm` length
  must equal `nlyr`. `NA != NULL` → the `nz()` helper strips blank-`NA` to `NULL`;
  never bypass it.
- **air formatter** confirmed as house style; Maury added `air.toml` to hsical this
  session (plus `.vscode/`), and both are now in `.Rbuildignore`. `zarowka` has no
  CLAUDE.md.
- **Tooling gotcha:** `library(HSItools)` segfaults under Git-Bash `Rscript` (terra
  DLLs); run R via PowerShell's `Rscript.exe`. To read a compiled function without
  attaching: `deparse(get(f, asNamespace("HSItools")))`.

## Open items for Maury

- **Interface contract lags the code.** `hsi-interface-contract.md` (v3.0) still
  describes a two-mode Calibrate|Log app; hsical v2.1 collapsed to one Scan panel +
  Review. Core contract (own no schema, wrap the trio, flat sidecar) is intact, but the
  doc's app-structure section is stale. Contract edits belong to Maury — flagged, not
  fixed.
- **`README.md` refreshed** this session (was pre-2.0 three-tab app with dead deps).

## Standing v2.1 observations (context, not tasks)

- **Design ethos to preserve:** a scan is five numbers (lines, samples, start, stop,
  fov); everything else is derived. The form is an argument collector for
  `HSItools::hsi_create_metadata()`; hsical owns no schema. Keep new features on the
  right side of that line.
- **`"Specim"` default is duplicated** (`app.R:438`, `:999`) — natural thing to
  collapse when the v2.2 defaults work happens (see the design note).
- **Lens choices carry a TODO in shipped source** (`app.R:447-452`) — real VNIR
  objective names still unentered.
- **DARKREF is discovered but deliberately never read** (`app.R:849`) — its absence at
  the specimen integration time is the signal; don't "fix" this into reading it.

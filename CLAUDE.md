# CLAUDE.md — hsical

Guidance for working in the **hsical** repo. hsical is one of three packages in the
HSItools ecosystem; the **canonical, non-negotiable conventions live in the ecosystem
file** at `C:\GitHub\HSItools\CLAUDE.md` (repo `mzarowka/HSItools`, root `CLAUDE.md`).
Read that file too — Claude Code only auto-loads *this* repo's `CLAUDE.md`, so the
rules that must survive a hsical-only session are restated here. On any conflict, the
ecosystem file wins.

## The ecosystem in one paragraph

**HSItools** = core R package (processing/analysis/viz of hyperspectral rasters;
CRAN-quality). **zarowka** = front-end scaffolding where new functions are battle-
tested before promotion to HSItools. **hsical** = this repo: a standalone, packaged
Shiny app (`hsical::run_app()`) that is the scan-session calibration companion and
metadata logger. **hsical never touches spectral data** — no reflectance, no masking,
no co-registration. It collects field values and writes a metadata sidecar.

## How to work here (behavioral rules — these outrank the code specifics)

From ecosystem §0/§9. They apply to hsical work:

1. **Approach before code.** Discuss design/contract first; don't code immediately.
   Wait for agreement, then implement.
2. **One change at a time.** Propose a single change, let Maury run it, then continue.
   Never batch unrelated edits.
3. **Git belongs to Maury.** Never run state-changing git (`add`, `commit`, `checkout`,
   `branch`, `merge`, `push`, `stash`, `restore`). Read-only inspection is fine.
4. **Don't generalize speculatively**; don't hardcode unless necessary. If tempted to
   add "flexibility", ask first.
5. **Verification runs where R lives.** If the session can run R, verify changes and
   show output before claiming success; otherwise hand off patches for Maury to run.
   Never claim a verification step ran unless its output was shown.
6. **CLAUDE.md carries durable conventions only.** No milestone state, session notes,
   TODOs, or roadmap here — those live in dated `dev-notes/YYYY-MM-DD_<topic>.md`
   documents (see "Dev-notes convention" below). Don't edit this file unless asked.

## The one architectural rule: hsical owns no schema

The sidecar's schema, YAML serialization, and validation belong to **HSItools**.
`inst/app/app.R` is an *argument collector* for `HSItools::hsi_create_metadata()`,
piped to `hsi_write_metadata()` / `hsi_read_metadata()`. **If hsical code ever contains
`yaml::` or a field list of its own, the design has been violated.**

**Authoritative spec:** `C:\GitHub\HSItools\dev-notes\hsi-interface-contract.md`
(contract v3.0, schema v1.1.0) — the human-readable field→GUI-source map and the list
of deliberately-rejected fields. The normative field types/ranges live in HSItools'
`validate_hsi_metadata()`. **Do not restate the 32-field schema here** — read the
contract and the validator so hsical never drifts from them. Change order when the
schema moves: contract doc first → HSItools → hsical's form.

hsical-critical invariants worth keeping in the head:
- **Only `name` is required.** Every other field is `NULL` when absent — never `""`,
  never `NA`. `NA` is not `NULL`: a blank `numericInput` returns logical `NA`, and
  HSItools' `check_numeric(positive=TRUE)` does `any(x<=0)` → `NA` → `if(NA)` errors.
  The `nz()` helper in `app.R` exists solely to strip blank/`NA` → `NULL`; **keep every
  value passed to `hsi_create_metadata()` / the Review save wrapped in `nz()`.**
- **No `camera` field** — the GUI sensor selector writes `sensor_type` (VNIR/SWIR/free
  text). Don't reintroduce a camera field.
- **`wavelengths`/`fwhm`** are autofilled from the `.hdr`, never typed, shown read-only;
  each must have `length == nlyr` or HSItools aborts. That's why the app carries them
  together with `nlyr`.
- **Zero is valid** for `dropped_frames` and `gcp_count` (and `camera_position_mm` /
  `stage_position_mm` accept any sign) — these four are *not* the strictly-positive kind.
- Errors carry condition class **`hsitools_error`**; the app wraps saves in `tryCatch`
  and surfaces `conditionMessage()`. HSItools validates on create *and* write — let it
  reject bad input; don't pre-empt its checks.

## The "five numbers" model (don't undo it)

A scan is five inputs: `lines`, `samples`, `target_start_mm`, `target_stop_mm`,
`fov_mm`. Everything else on Card 1 — length, xres, yres, aspect ratio, ideal FOV,
scan time — is **derived** in `geom()`, never typed. v2.1 collapsed the old split into
one Scan panel + a Review panel; "test", "confirmation" and "target" scans were never
different objects, only different things to look at. Don't reintroduce separate cards.

> Note: the interface contract (v3.0, 2026-07-11) still describes a two-mode
> Calibrate|Log app; the app (v2.1, 2026-07-14) has since collapsed to one Scan panel
> + Review. The core contract (own no schema, wrap the trio, flat sidecar) is intact,
> but the doc's app-structure section lags the code — flag to Maury rather than trusting
> it for layout.

## Code style (ecosystem §2 — house style, non-negotiable)

Formatter is **air** (`air.toml` at repo root pins it; run `air format` on files you
edit — never hand-format, never infer from a missing config that hand-formatting is OK).
Match `app.R`:
- Native pipe `|>` only (never `%>%`); anonymous functions as `\(x)` (never
  `function(x)`); `purrr::map*/walk` over `for`/apply.
- Explicit `package::function()` everywhere (`shiny::`, `bslib::`, `HSItools::`, …);
  no `library()` calls in the app.
- The only two local helpers: `` `%||%` `` (null-coalesce) and `nz()` (blank/`NA` →
  `NULL`). Reuse them; don't add a second flavor.
- Comments explain *why* (the geometry, the contract, a layout hack), not *what*.
- Windows/PowerShell dev environment; be alert to path issues.

## Layout

- `inst/app/app.R` — the entire app: helpers, `parse_hdr()`/`parse_log()`,
  `discover_capture()`, field-partition constants (`SESSION_*`, `PER_CAPTURE_*`,
  `REVIEW_*`), UI, server. No business logic lives outside this file.
- `R/run_app.R` — thin `shiny::runApp(system.file("app", package="hsical"))` wrapper.
- `inst/launch/` — Windows launcher (`hsical.cmd`, shortcut installer).
- `dev-notes/` — design notes (not shipped). HSItools is a GitHub dep
  (`Remotes: mzarowka/HSItools`), not on CRAN.

## Dev-notes convention

Ecosystem style is **dated documents**: `dev-notes/YYYY-MM-DD_<topic>.md` for handoffs,
memos, and audits, plus stable-name reference docs (e.g. HSItools'
`hsi-interface-contract.md`). Durable conventions go in CLAUDE.md; everything time-bound
goes in dated dev-notes — never in CLAUDE.md.

## Running & gotchas

- Run: `hsical::run_app()` in R, or `inst/launch/hsical.cmd` on Windows.
- **Inspecting HSItools internals:** `library(HSItools)` pulls in `terra`, whose native
  DLLs **segfault** under the Git-Bash `Rscript` in this environment. Run R through
  **PowerShell** (`& "C:\Program Files\R\R-4.6.0\bin\Rscript.exe"`) instead. To read a
  compiled function without attaching: `deparse(get(f, asNamespace("HSItools")))`.
- `README.md` was refreshed to v2.1 (2026-07-15). If the app changes again, keep it in
  step; on any doubt, `DESCRIPTION`, `app.R`, and the interface contract are ground truth.

## Housekeeping

Non-package files (`CLAUDE.md`, `dev-notes/`, `.claude/`, `air.toml`, `.vscode/`) are
listed in `.Rbuildignore` so `R CMD build`/`check` stays clean. Add any new dev-only
file there too.

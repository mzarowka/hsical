# CLAUDE.md — hsical

Conventions for working on **hsical**, a packaged Shiny app (`hsical::run_app()`): the
scan-session calibration companion and metadata logger for hyperspectral core scanning. It
depends on **HSItools**, whose own `CLAUDE.md` and `.claude/rules/` define the shared house
style for R code; this file carries what is specific to hsical. Personal workflow preferences
belong in each developer's own Claude Code configuration, not here.

This file carries durable conventions only — no milestone state, session notes or TODOs.

## What hsical is, and is not

hsical collects field values from the operator and the capture files and writes a metadata
sidecar. **It never processes spectral data** — no reflectance, no masking, no
co-registration, no raster written.

The one exception: the Saturation panel **reads raw DN for diagnostics and display only** —
screening the loaded capture through `HSItools::hsi_check_saturation()` and mapping where it
clips. Nothing it reads is written anywhere, and no spectral value (`saturation_ratio`
included) enters the sidecar.

## The one architectural rule: hsical owns no schema

The sidecar's schema, YAML serialization and validation belong to **HSItools**.
`inst/app/app.R` is an *argument collector* for `HSItools::hsi_create_metadata()`, piped to
`hsi_write_metadata()` / `hsi_read_metadata()`. **If hsical code ever contains `yaml::` or a
field list of its own, the design has been violated.**

The normative field types and ranges are HSItools' `validate_hsi_metadata()`. **Do not restate
the schema here** — read the validator, so hsical never drifts from it. When the schema moves,
HSItools changes first and hsical's form follows.

Invariants worth keeping in the head:
- **Only `name` is required.** Every other field is `NULL` when absent — never `""`, never
  `NA`. `NA` is not `NULL`: a blank `numericInput` returns logical `NA`, and HSItools'
  `check_numeric(positive = TRUE)` does `any(x <= 0)` → `NA` → `if (NA)` errors. The `nz()`
  helper in `app.R` exists solely to strip blank/`NA` → `NULL`; **keep every value passed to
  `hsi_create_metadata()` / the Review save wrapped in `nz()`.**
- **No `camera` field** — the sensor selector writes `sensor_type`. Don't reintroduce a
  camera field.
- **`wavelengths` / `fwhm`** are autofilled from the `.hdr`, never typed, shown read-only; each
  must have `length == nlyr` or HSItools aborts. That's why the app carries them together with
  `nlyr`.
- **Zero is valid** for `dropped_frames` and `gcp_count` (and `camera_position_mm` /
  `stage_position_mm` accept any sign) — these four are *not* the strictly-positive kind.
- Errors carry condition class **`hsitools_error`**; the app wraps saves in `tryCatch` and
  surfaces `conditionMessage()`. HSItools validates on create *and* write — let it reject bad
  input; don't pre-empt its checks.

## The "five numbers" model (don't undo it)

A scan is five inputs: `lines`, `samples`, `target_start_mm`, `target_stop_mm`, `fov_mm`.
Everything else on the Scan panel — length, xres, yres, aspect ratio, ideal FOV, scan time — is
**derived** in `geom()`, never typed. Scan length from the motor positions divided by the raster
dimensions *is* the pixel size (`yres = length·1000/nrow`, `xres = fov·1000/ncol`); a nominal
typed value was never the intent. "Test", "confirmation" and "target" scans were never different
objects, only different things to look at — one Scan panel, no separate cards, no manual
xres/yres inputs.

## Code style

The house style is HSItools'. For the app specifically, match `app.R`:
- Formatter is **air** (`air.toml` at the repo root pins it); run `air format` on files you
  edit — never hand-format.
- Native pipe `|>` only (never `%>%`); anonymous functions as `\(x)` (never `function(x)`);
  `purrr::map*/walk` over `for`/apply.
- Explicit `package::function()` everywhere (`shiny::`, `bslib::`, `HSItools::`, …); no
  `library()` calls in the app.
- One null-coalesce `` `%||%` `` and one blank/`NA` → `NULL` helper, `nz()`. Reuse them; don't
  add a second flavour of either.
- Comments explain *why* (the geometry, the contract, a layout hack), not *what*.
- The app runs on Windows lab PCs; be alert to path handling.

## Layout

- `inst/app/app.R` — the entire app: helpers, `parse_hdr()` / `parse_log()`,
  `discover_capture()`, field-partition constants (`SESSION_*`, `PER_CAPTURE_*`, `REVIEW_*`),
  UI, server. No business logic lives outside this file.
- `R/run_app.R` — thin `shiny::runApp(system.file("app", package = "hsical"))` wrapper.
- `inst/launch/` — Windows launcher (`hsical.cmd`, shortcut installer).
- HSItools is a GitHub dependency (`Remotes:` in `DESCRIPTION`), not on CRAN.

## Running

- `hsical::run_app()` in R, or `inst/launch/hsical.cmd` on Windows.
- After reinstalling HSItools, restart R — a running session keeps the old namespace loaded.
- `README.md` describes v2.2. If the app changes, keep it in step; on any doubt,
  `DESCRIPTION`, `app.R` and HSItools' validator are ground truth.

## Housekeeping

Non-package files (`CLAUDE.md`, `.claude/`, `air.toml`, `.vscode/`) are listed in
`.Rbuildignore` so `R CMD build` / `check` stays clean. Add any new dev-only file there too.

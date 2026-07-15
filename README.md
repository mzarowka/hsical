# hsical

A Shiny companion for hyperspectral core-scanning sessions on a Specim rig. It treats
every scan the same way — whether taken to test the geometry, confirm it, or keep it —
and turns one capture into one metadata sidecar.

hsical **owns no schema of its own**. The sidecar's structure, serialization, and
validation belong to [HSItools](https://github.com/mzarowka/HSItools); hsical is an
argument collector for `HSItools::hsi_create_metadata()` and a thin wrapper around
`hsi_write_metadata()` / `hsi_read_metadata()`. It never processes spectral data —
no reflectance, no masking, no co-registration.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mzarowka/hsical")
```

HSItools is pulled in automatically (it is a `Remotes` dependency, not on CRAN).

## Usage

```r
hsical::run_app()
```

On a rig PC, a one-double-click desktop launcher is available under
`system.file("launch", package = "hsical")` — see its README for setup.

## What it does

### Scan panel

Load one capture `.hdr` and hsical discovers the rest of the scan folder — the
`WHITEREF` and `DARKREF` siblings and the Lumo `.log` — from that single pick, and
autofills what the files already know (`lines`, `samples`, `bands`, integration times,
frame rate, binning, calibration pack, dropped frames, and the full wavelength / FWHM
axes).

A scan is **five numbers**: lines, samples, start position, stop position, and field of
view. Everything else is derived and shown live, never typed:

- scan length and estimated scan time,
- along-track (`yres`) and across-track (`xres`) pixel size in µm,
- the ideal FOV that would square the pixels,
- the aspect ratio, with a three-tier square-pixel indicator
  (green 0.95–1.05 / amber near-square / red — adjust FOV).

The remaining sidecar fields (session, instrument, acquisition, QC) are filled in as
needed, then **Save sidecar** writes one flat YAML file per capture, by default beside
the scan. Session-stable fields carry forward between saves; **Clear session** resets
them.

### Review panel

Load a `.yaml` sidecar back, edit any field in place, and save. Spectral axes and the
schema version are shown read-only and carried through untouched. HSItools validates on
write, so an out-of-range edit aborts with its own message.

## Output

One flat YAML sidecar per capture, written by HSItools (schema `1.1.0`, 32 fields, only
`name` required; absent fields are `NULL`). Illustrative excerpt:

```yaml
schema_version: '1.1.0'
name: LAZ-26_01-01
sensor_type: VNIR
manufacturer: Specim
lens: OLES30
session_id: LAZ-26-S1
operator: Jane Doe
campaign_prefix: LAZ-26
dataset_name: 01-01
nrow: 1200
ncol: 384
nlyr: 224
xres: 62.5
yres: 62.4
aspect_ratio: 0.9984
fov_mm: 24.0
et_target_ms: 12.5
et_white_ms: 3.0
dropped_frames: 0
wavelengths: [397.32, 399.65, ...]
fwhm: [2.1, 2.1, ...]
```

The full field-by-field definition — types, ranges, and each field's GUI source — is
the HSItools interface contract, not restated here.

## Dependencies

shiny · bslib · bsicons · shinyFiles · purrr · cli · HSItools

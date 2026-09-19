# HSIcal

A Shiny companion for hyperspectral core-scanning sessions on a Specim rig. It treats
every scan the same way — whether taken to test the geometry, confirm it, or keep it —
turns one capture into one metadata sidecar, screens raw captures for clipping, and keeps
track of which captures in a campaign are still unlogged.

HSIcal **owns no schema of its own**. The sidecar's structure, serialization, and
validation belong to [HSItools](https://github.com/mzarowka/HSItools); HSIcal is an
argument collector for `HSItools::hsi_create_metadata()` and a thin wrapper around
`hsi_write_metadata()` / `hsi_read_metadata()`. It never processes spectral data — no
reflectance, no masking, no co-registration. The one place it reads raw digital numbers
is the Saturation panel, for display and screening only; nothing it reads there is written
anywhere.

## Installation

```r
# install.packages("pak")
pak::pak("mzarowka/hsical")
```

HSItools is pulled in automatically. It is not on CRAN, and HSIcal currently needs its
**development branch** (`>= 0.5.3.9004`, for `hsi_check_saturation()`); the `Remotes` pin
in `DESCRIPTION` takes care of that. After reinstalling either package, **restart R** — a
running session keeps the old HSItools loaded.

## Usage

```r
hsical::run_app()
```

On a rig PC, a one-double-click desktop launcher is available under
`system.file("launch", package = "hsical")` — see its README for setup.

## What it does

### Scan panel

With nothing loaded, the panel shows **Before you start**: short reminders for the things
no header records — pausing Windows Update on the scan PC, placing the label, walking the
motor to the start and stop, setting exposure with headroom, running and copying the
white-reference session. They are reminders, not a checklist: nothing is ticked or stored,
and they disappear as soon as a capture is loaded.

Load one capture `.hdr` and HSIcal discovers the rest of the scan folder — the
`WHITEREF` and `DARKREF` siblings and the Lumo `.log` matched by the capture's own name —
from that single pick, and autofills what the files already know (`lines`, `samples`,
`bands`, integration times, frame rate, binning, lens and calibration pack, dropped frames,
and the full wavelength / FWHM axes).

**Dropped frames are flagged**, not just filled in: the discovery list states the count and
share of recorded frames (in red when any were lost), and a notification appears at load.
A dropped frame is a line the stage moved past unrecorded, so the scan should be judged at
the rig while the core is still on the stage.

A scan is **five numbers**: lines, samples, start position, stop position, and field of
view. Everything else is derived and shown live, never typed:

- scan length and estimated scan time,
- along-track (`yres`) and across-track (`xres`) pixel size in µm,
- the ideal FOV that would square the pixels,
- the aspect ratio, with a three-tier indicator (square 0.95–1.05 / nearly square
  0.90–1.10 / not square — adjust FOV).

The remaining sidecar fields (session, instrument, acquisition, QC) are filled in as
needed, then **Save sidecar** writes one flat YAML file per capture, `<name>.yaml` in the
scan folder by default. Session-stable fields carry forward between saves; **Clear session**
resets them.

### Review panel

Load a `.yaml` sidecar back, edit any field in place, and save. Spectral axes and the
schema version are shown read-only and carried through untouched. HSItools validates on
write, so an out-of-range edit aborts with its own message.

### Saturation panel

A screen of the loaded capture's raw digital numbers against a limit (default 97.5% of the
16-bit ceiling), computed by `HSItools::hsi_check_saturation()`. Drag on the decimated
preview to screen a region of interest and keep the tray and tape out of the count; screen
every Nth band to trade time for a lower-bound count.

After a screen, **clipped pixels are washed over the preview in red**, darker where more of
the area clips. The percentage says how much; the map says where — clipping on the specimen
means recapture, clipping confined to the tray or a column of hot pixels means screening a
tighter region instead.

Screen the capture that matters: the in-capture `WHITEREF` is taken at the specimen's
integration time and clips by design, so load the white-reference session itself to judge
the reference.

### Inventory panel

Point it at a campaign or core folder and it lists every capture under it, oldest first:
sensor, raster size, integration time, which references and log are present, the
white-reference session, and whether a sidecar has been written. The summary line counts
what is still unlogged. Headers and file names only — no capture is opened, so it is safe
to run while a scan is in progress.

The white-reference session is looked for **inside the capture first** — a copy under
`<capture>/whiteref/` — and beside it second, for older layouts. Several different copies
in one capture are reported as ambiguous rather than resolved silently. Captures reached
twice through a Windows junction are listed once.

**Load** puts any row on the Scan panel exactly as picking its `.hdr` would.

## Lab conventions it assumes

- Lumo capture folders: `<scan>/capture/` holding the capture `.hdr`/`.raw`, its
  `WHITEREF_`/`DARKREF_` siblings and the `.log`.
- White-reference sessions are named `WR_yyyy-mm-dd` (Lumo appends its own timestamp),
  the original kept in the country's `_WHITEREF/<sensor>/` folder and a copy placed in each
  capture's `whiteref/` folder. Older captures name sessions in many other ways; the
  Inventory recognises `WR` anywhere in a name as a separate token.
- Lumo folder names and header times are in **UTC**.

## Output

One flat YAML sidecar per capture, written by HSItools (schema `1.1.0`, 32 fields, only
`name` required; absent fields are `NULL`). Illustrative excerpt:

```yaml
schema_version: '1.1.0'
name: LAZ-26_01-01
sensor_type: SWIR
manufacturer: Specim
lens: OLES30
calibration_pack: 472194_OLES30_20250422_calpack_BPR.scp
session_id: LAZ-26-S1
operator: Jane Doe
campaign_prefix: LAZ-26
dataset_name: 01-01
nrow: 1200
ncol: 384
nlyr: 271
xres: 62.5
yres: 62.4
aspect_ratio: 0.9984
fov_mm: 24.0
et_target_ms: 10.0
et_white_ms: 3.0
dropped_frames: 0
wavelengths: [999.70, 1005.33, ...]
fwhm: [5.63, 5.63, ...]
```

`calibration_pack` stores the file name only; the lens is read from it. The full
field-by-field definition — types, ranges, and each field's GUI source — is the HSItools
interface contract, not restated here.

## Dependencies

shiny · bslib · bsicons · shinyFiles · purrr · cli · terra · HSItools (dev)

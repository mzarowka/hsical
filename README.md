# hsical

Hyperspectral Image Calibration Tool — a Shiny app for calculating pixel resolution from test scans and logging scan metadata.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mzarowka/hsical")
```

## Usage

```r
hsical::run_app()
```

## Features

### Tab 1: Check Calibration
- Load a test raster file (.raw with .hdr sidecar, or .tif)
- Enter the physical scan length and measured field of view
- Get pixel resolution in µm/pixel for both dimensions
- Check if pixels are square (aspect ratio near 1.0)

### Tab 2: Calculate Ideal FOV
- Load a test raster file
- Enter the physical scan length (known from software)
- Get the ideal FOV for perfectly square pixels
- Output in µm, mm, and cm for easy tape matching

### Tab 3: Scan Log
- Comprehensive metadata entry form for archival
- Auto-fill from ENVI .hdr and .log files
- Fields: location, sample info, scan setup, references, results
- Extensible material type dropdown (persists across sessions)
- Dual output:
  - Individual YAML file per scan (Quarto-friendly)
  - Master CSV log for continuity tracking

## Output Formats

**Individual scan log (YAML):**
```yaml
location:
  site_name: "Głodówka"
  site_code: "GLO24"
  country: "Poland"
  country_code: "PL"
sample:
  core_id: "GLO24-A"
  section_depth: "0-50"
  material_type: "lake sediments"
# ... etc
```

**Master log:** Appends to `hsical_master_log.csv` in working directory.

## Dependencies
- shiny
- bslib
- shinyFiles
- terra
- measurements
- yaml
- jsonlite
- countrycode

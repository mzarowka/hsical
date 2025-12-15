# hsical

Hyperspectral Image Calibration Tool — a Shiny app for calculating pixel resolution from test scans.

## Installation

```r
# install.packages("remotes")
remotes::install_github("mzarowka/hsical")
```

## Usage

```r
hsical::run_app()
```

## What it does

1. Load a test raster file (.raw with .hdr sidecar, or .tif)
2. Enter the physical scan length and field of view with units
3. Get pixel resolution in µm/pixel for both dimensions
4. Check if pixels are square (aspect ratio near 1.0)

## Dependencies
- shiny
- bslib
- shinyFiles
- terra
- measurements

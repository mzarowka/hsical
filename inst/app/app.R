# HSI Calibration Tool v1.0.0
# Protocol v2.0 — Department of Geomorphology and Quaternary Geology, UG
# Tabs: (1) Check pixel aspect ratio  (2) Calculate ideal FOV  (3) Scan Log

library(shiny)
library(bslib)
library(bsicons)
library(shinyFiles)
library(shinyjs)
library(terra)
library(measurements)
library(purrr)
library(yaml)
library(jsonlite)

# ============================================================================
# Helpers
# ============================================================================

get_config_dir <- function() {
  d <- file.path(path.expand("~"), ".hsical")
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
  d
}

load_material_types <- function() {
  f        <- file.path(get_config_dir(), "material_types.json")
  defaults <- c("lake sediments", "marine sediments", "peat", "soil",
                "rock", "outcrop", "other")
  if (file.exists(f)) {
    tryCatch(unique(c(jsonlite::fromJSON(f), defaults)), error = \(e) defaults)
  } else {
    defaults
  }
}

save_material_types <- function(types) {
  jsonlite::write_json(
    types,
    file.path(get_config_dir(), "material_types.json"),
    auto_unbox = TRUE
  )
}

# Tooltip label: short label + info icon
tip <- function(label, text) {
  tagList(
    label,
    bslib::tooltip(
      bsicons::bs_icon("info-circle", size = "0.85em", class = "text-muted ms-1"),
      text,
      placement = "right"
    )
  )
}

# Parse ENVI .hdr file
parse_hdr <- function(hdr_path) {
  if (!file.exists(hdr_path)) return(NULL)

  content <- readLines(hdr_path, warn = FALSE) |> paste(collapse = "\n")

  xval <- function(pattern) {
    m <- regmatches(content, regexpr(pattern, content, perl = TRUE))
    if (length(m) == 0 || identical(m, character(0)) || m == "") NA_character_ else m
  }

  xnum <- function(key) {
    v <- xval(paste0("(?<=", key, " = )[0-9.]+"))
    if (is.na(v)) NA_real_ else as.numeric(v)
  }

  # binning = {spectral, spatial}
  bin_m <- regmatches(
    content,
    regexpr("(?<=binning = \\{)[0-9]+, [0-9]+(?=\\})", content, perl = TRUE)
  )
  if (length(bin_m) > 0 && bin_m != "") {
    b        <- as.numeric(strsplit(bin_m, ", ")[[1]])
    spec_bin <- b[1]
    spat_bin <- b[2]
  } else {
    spec_bin <- NA_real_
    spat_bin <- NA_real_
  }

  # Camera from sensor type line
  sensor_raw <- xval("(?<=sensor type = )[^\n]+")
  camera <- if (!is.na(sensor_raw)) {
    if (grepl("SWIR", sensor_raw, ignore.case = TRUE)) "SWIR" else "VNIR"
  } else {
    NA_character_
  }

  # Calibration pack path
  cal_raw <- xval("(?<=calibration pack = )[^\n]+")
  cal     <- if (!is.na(cal_raw)) trimws(cal_raw) else NA_character_

  list(
    lines            = xnum("lines"),
    samples          = xnum("samples"),
    bands            = xnum("bands"),
    fps              = xnum("fps"),
    tint             = xnum("tint"),
    spectral_binning = spec_bin,
    spatial_binning  = spat_bin,
    camera           = camera,
    calibration_pack = cal,
    acquisition_date = xval("(?<=acquisition date = DATE\\(yyyy-mm-dd\\): )[0-9-]+"),
    start_time       = xval("(?<=Start Time = UTC TIME: )[0-9:]+"),
    stop_time        = xval("(?<=Stop Time = UTC TIME: )[0-9:]+")
  )
}

# Parse Lumo .log file
parse_log <- function(log_path) {
  if (!file.exists(log_path))
    return(list(dropped = NA_real_, recorded = NA_real_))

  content <- readLines(log_path, warn = FALSE) |> paste(collapse = "\n")

  dropped  <- regmatches(content,
    regexpr("(?<=incidents, )[0-9]+(?= dropped frames)", content, perl = TRUE))
  recorded <- regmatches(content,
    regexpr("[0-9]+(?= frames recorded)", content, perl = TRUE))

  list(
    dropped  = if (length(dropped)  == 0) NA_real_ else as.numeric(dropped),
    recorded = if (length(recorded) == 0) NA_real_ else as.numeric(recorded)
  )
}

# Three-tier aspect ratio classification
ratio_tier <- function(ratio) {
  if (is.na(ratio))                   return(list(theme = "secondary", icon = "square",             label = "—"))
  if (ratio >= 0.95 && ratio <= 1.05) return(list(theme = "success",   icon = "check-square",       label = "✓ Square pixels"))
  if (ratio >= 0.90 && ratio <= 1.10) return(list(theme = "warning",   icon = "exclamation-square", label = "⚠ Nearly square — check"))
                                      return(list(theme = "danger",    icon = "x-square",           label = "✗ Not square — adjust FOV"))
}

# --------------------------------------------------------------------------
# Validation
# --------------------------------------------------------------------------

# Expected session ID format: YYYY-MM-DD-CAMERA_NN (e.g. 2026-03-15-VNIR_01)
SESSION_ID_REGEX <- "^\\d{4}-\\d{2}-\\d{2}-(VNIR|SWIR)_\\d{2}$"

is_valid_session_id <- function(id) {
  nzchar(trimws(id)) && grepl(SESSION_ID_REGEX, trimws(id))
}

# Mandatory fields and their display labels
MANDATORY_FIELDS <- list(
  session_id       = "Session ID",
  operator         = "Operator",
  campaign_prefix  = "Campaign prefix",
  dataset_name     = "Dataset name",
  lens             = "Lens",
  calibration_pack = "Calibration pack",
  et_target        = "ET_target (ms)",
  et_white         = "ET_white (ms)",
  fov              = "FOV (mm)",
  spectral_binning = "Spectral binning",
  spatial_binning  = "Spatial binning",
  target_start     = "Target start (mm)",
  target_stop      = "Target stop (mm)",
  test_scan_start  = "Test scan start (mm)",
  test_scan_stop   = "Test scan stop (mm)",
  aspect_ratio     = "Aspect ratio"
)

is_blank <- function(val) {
  is.null(val) ||
    (is.character(val) && nchar(trimws(val)) == 0) ||
    (length(val) == 1 && is.na(val))
}

validate_form <- function(d) {
  missing_fields <- names(MANDATORY_FIELDS) |>
    purrr::keep(\(field) is_blank(d[[field]])) |>
    purrr::map_chr(\(field) MANDATORY_FIELDS[[field]])

  list(ok = length(missing_fields) == 0, fields = missing_fields)
}

# --------------------------------------------------------------------------
# Export helpers
# --------------------------------------------------------------------------

generate_yaml <- function(d) {
  scan_len <- if (!is_blank(d$target_start) && !is_blank(d$target_stop))
    d$target_stop - d$target_start else NA

  yaml::as.yaml(list(
    session = list(
      session_id      = d$session_id,
      operator        = d$operator,
      date            = d$scan_date,
      campaign_prefix = d$campaign_prefix,
      dataset_name    = d$dataset_name
    ),
    instrument = list(
      camera           = d$camera,
      lens             = d$lens,
      calibration_pack = d$calibration_pack,
      fov_mm           = d$fov,
      et_target_ms     = d$et_target,
      et_white_ms      = d$et_white,
      spectral_binning = d$spectral_binning,
      spatial_binning  = d$spatial_binning
    ),
    geometry = list(
      target_start_mm       = d$target_start,
      target_stop_mm        = d$target_stop,
      scan_length_mm        = scan_len,
      test_scan_start_mm    = d$test_scan_start,
      test_scan_stop_mm     = d$test_scan_stop,
      measured_aspect_ratio = d$aspect_ratio
    ),
    scan = list(
      filename             = d$filename,
      total_lines          = d$total_lines,
      dropped_frames       = d$dropped_frames,
      saturation_ratio_pct = d$saturation_ratio,
      gcp_pins             = d$gcp_pins
    ),
    sample = list(
      core_id        = d$core_id,
      section_depth  = d$section_depth,
      material_type  = d$material_type,
      material_owner = d$material_owner
    ),
    location = list(
      site_name = d$site_name,
      site_code = d$site_code,
      country   = d$country
    ),
    notes = d$notes
  ))
}

generate_csv_row <- function(d) {
  scan_len <- if (!is_blank(d$target_start) && !is_blank(d$target_stop))
    d$target_stop - d$target_start else NA

  data.frame(
    logged_at            = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    session_id           = d$session_id,
    operator             = d$operator,
    scan_date            = d$scan_date,
    campaign_prefix      = d$campaign_prefix,
    dataset_name         = d$dataset_name,
    filename             = d$filename,
    camera               = d$camera,
    lens                 = d$lens,
    calibration_pack     = d$calibration_pack,
    fov_mm               = d$fov,
    et_target_ms         = d$et_target,
    et_white_ms          = d$et_white,
    spectral_binning     = d$spectral_binning,
    spatial_binning      = d$spatial_binning,
    target_start_mm      = d$target_start,
    target_stop_mm       = d$target_stop,
    scan_length_mm       = scan_len,
    test_scan_start_mm   = d$test_scan_start,
    test_scan_stop_mm    = d$test_scan_stop,
    aspect_ratio         = d$aspect_ratio,
    total_lines          = d$total_lines,
    dropped_frames       = d$dropped_frames,
    saturation_ratio_pct = d$saturation_ratio,
    gcp_pins             = d$gcp_pins,
    core_id              = d$core_id,
    section_depth        = d$section_depth,
    material_type        = d$material_type,
    material_owner       = d$material_owner,
    site_name            = d$site_name,
    site_code            = d$site_code,
    country              = d$country,
    notes                = d$notes,
    stringsAsFactors     = FALSE
  )
}

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  title    = "HSI Calibration Tool",
  theme    = bs_theme(version = 5, bootswatch = "flatly", "navbar-bg" = "#2C3E50"),
  fillable = FALSE,

  useShinyjs(),

  sidebar = sidebar(
    width = 310,

    # ------------------------------------------------------------------
    # Sidebar content for Tabs 1 & 2
    # ------------------------------------------------------------------
    conditionalPanel(
      condition = "input.tabs !== 'Scan Log'",

      h6("Raster file (test scan)"),
      shinyFilesButton(
        id = "file_select", label = "Select raster file",
        title = "Choose a raster file (.raw or .tif)",
        multiple = FALSE, icon = icon("file")
      ),
      verbatimTextOutput("file_path", placeholder = TRUE),

      hr(),

      h6(tip(
        "Motor positions",
        "Read Target start and Target stop from Lumo's motor position display.
         These values are NOT saved in any Lumo output file — record them in
         your notebook first, then enter here. Scan length = Stop − Start."
      )),
      layout_columns(
        col_widths = c(6, 6),
        numericInput("motor_start", "Start (mm)", value = NULL, min = 0),
        numericInput("motor_stop",  "Stop (mm)",  value = NULL, min = 0)
      ),

      # FOV only shown in Tab 1
      conditionalPanel(
        condition = "input.tabs === 'Check Calibration'",
        hr(),
        h6(tip(
          "Field of view (mm)",
          "The FOV value currently set in Lumo's Scanning speed calculation
           panel. Enter in millimetres."
        )),
        numericInput("scan_fov", NULL, value = NULL, min = 0)
      )
    ),

    # ------------------------------------------------------------------
    # Sidebar content for Tab 3
    # ------------------------------------------------------------------
    conditionalPanel(
      condition = "input.tabs === 'Scan Log'",

      h6("Core scan files"),
      shinyFilesButton(
        id = "core_hdr_select", label = "Select core .hdr",
        title = "Choose the core scan .hdr file",
        multiple = FALSE, icon = icon("file-code")
      ),
      verbatimTextOutput("core_hdr_path", placeholder = TRUE),

      shinyFilesButton(
        id = "core_log_select", label = "Select core .log",
        title = "Choose the core scan .log file",
        multiple = FALSE, icon = icon("file-lines")
      ),
      verbatimTextOutput("core_log_path", placeholder = TRUE),

      actionButton("load_core", "Load core metadata",
                   icon  = icon("download"),
                   class = "btn-primary w-100 mb-2"),

      hr(),

      h6("White reference file"),
      shinyFilesButton(
        id = "white_hdr_select", label = "Select white ref .hdr",
        title = "Choose the white reference .hdr file",
        multiple = FALSE, icon = icon("file-code")
      ),
      verbatimTextOutput("white_hdr_path", placeholder = TRUE),

      actionButton("load_white", "Load ET_white",
                   icon  = icon("download"),
                   class = "btn-outline-primary w-100 mb-2"),

      hr(),

      h6("Session"),
      actionButton("toggle_lock", "Lock session",
                   icon  = icon("lock"),
                   class = "btn-warning w-100 mb-1"),
      actionButton("clear_scan", "New scan entry",
                   icon  = icon("rotate"),
                   class = "btn-outline-secondary w-100 mb-2"),

      hr(),

      h6("Save location"),
      shinyDirButton(
        id = "save_dir", label = "Choose folder",
        title = "Select folder for scan logs",
        icon = icon("folder-open")
      ),
      verbatimTextOutput("save_dir_path", placeholder = TRUE)
    )
  ),

  # ============================================================================
  # Main panel
  # ============================================================================

  navset_card_tab(
    id = "tabs",

    # ==========================================================================
    # Tab 1 — Check pixel aspect ratio
    # ==========================================================================
    nav_panel(
      title = "Check Calibration",
      icon  = icon("check-circle"),

      layout_columns(
        col_widths = 12,

        card(
          card_header("Raster dimensions"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              value_box(
                title    = "Scan lines (rows)",
                value    = textOutput("check_n_rows", inline = TRUE),
                showcase = bs_icon("arrow-down-up"),
                theme    = "secondary"
              ),
              value_box(
                title    = "Spatial pixels (columns)",
                value    = textOutput("check_n_cols", inline = TRUE),
                showcase = bs_icon("arrow-left-right"),
                theme    = "secondary"
              )
            )
          )
        ),

        card(
          card_header("Pixel resolution & aspect ratio"),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title    = "Along-track pixel size",
                value    = textOutput("res_len", inline = TRUE),
                showcase = bs_icon("arrow-down-up"),
                theme    = "primary"
              ),
              value_box(
                title    = "Across-track pixel size",
                value    = textOutput("res_fov", inline = TRUE),
                showcase = bs_icon("arrow-left-right"),
                theme    = "primary"
              ),
              uiOutput("ratio_box")
            )
          )
        )
      )
    ),

    # ==========================================================================
    # Tab 2 — Calculate ideal FOV
    # ==========================================================================
    nav_panel(
      title = "Calculate Ideal FOV",
      icon  = icon("calculator"),

      layout_columns(
        col_widths = 12,

        card(
          card_header("Raster dimensions"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              value_box(
                title    = "Scan lines (rows)",
                value    = textOutput("calc_n_rows", inline = TRUE),
                showcase = bs_icon("arrow-down-up"),
                theme    = "secondary"
              ),
              value_box(
                title    = "Spatial pixels (columns)",
                value    = textOutput("calc_n_cols", inline = TRUE),
                showcase = bs_icon("arrow-left-right"),
                theme    = "secondary"
              )
            )
          )
        ),

        card(
          card_header("Along-track pixel size (from motor positions)"),
          card_body(
            value_box(
              title    = "Pixel resolution",
              value    = textOutput("true_pixel_size", inline = TRUE),
              showcase = bs_icon("rulers"),
              theme    = "primary"
            )
          )
        ),

        card(
          card_header(
            tags$span(
              "Ideal FOV for square pixels",
              tags$small(class = "text-muted ms-2",
                "— enter the mm value in Lumo's Scanning speed calculation panel")
            )
          ),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title    = "FOV",
                value    = textOutput("ideal_fov_mm", inline = TRUE),
                p("millimeters"),
                showcase = bs_icon("rulers"),
                theme    = "success"
              ),
              value_box(
                title    = "FOV",
                value    = textOutput("ideal_fov_um", inline = TRUE),
                p("micrometers"),
                showcase = bs_icon("rulers"),
                theme    = "success"
              ),
              value_box(
                title    = "FOV",
                value    = textOutput("ideal_fov_cm", inline = TRUE),
                p("centimeters"),
                showcase = bs_icon("rulers"),
                theme    = "success"
              )
            )
          )
        )
      )
    ),

    # ==========================================================================
    # Tab 3 — Scan Log
    # ==========================================================================
    nav_panel(
      title = "Scan Log",
      icon  = icon("clipboard-list"),

      # Row 1: Session & Identity | Instrument | Motor Positions
      layout_columns(
        col_widths = c(4, 4, 4),

        # Session & Identity
        card(
          card_header("Session & identity", class = "bg-dark text-white"),
          card_body(
            textInput("session_id",
              tip("Session ID",
                "Groups all scans sharing the same camera, lens, FOV, ET_target,
                 and binning. Format: YYYY-MM-DD-CAMERA_NN, e.g.
                 2026-03-15-VNIR_01. Increment NN when a new session starts on
                 the same day with the same sensor. Lock the session after the
                 first scan to prevent accidental changes."),
              placeholder = "2026-03-15-VNIR_01"),

            textInput("operator",
              tip("Operator", "Full first and last name of the person operating the scanner."),
              placeholder = "Firstname Lastname"),

            textInput("campaign_prefix",
              tip("Campaign prefix",
                "Set once in Lumo's Setup tab. Format: SITE-YY where SITE is the
                 lake/site code and YY is the two-digit year. Example: LAZ-26."),
              placeholder = "LAZ-26"),

            textInput("dataset_name",
              tip("Dataset name",
                "Entered per scan in Lumo's Capture tab. Format: CC-SS with
                 zero-padded core and section numbers. Example: 01-01 = core 1,
                 section 1. Lumo appends this to the campaign prefix."),
              placeholder = "01-01"),

            dateInput("scan_date", "Date", value = Sys.Date()),

            selectInput("camera", "Camera",
              choices  = c("VNIR", "SWIR"),
              selected = "VNIR")
          )
        ),

        # Instrument
        card(
          card_header("Instrument", class = "bg-primary text-white"),
          card_body(
            selectizeInput("lens",
              tip("Lens",
                "Objective lens focal length. Typical values for this setup:
                 18.5 mm (VNIR), 50.0 mm (SWIR). You can type a custom value."),
              choices = c("18.5 mm", "50.0 mm"),
              options = list(create = TRUE, placeholder = "Select or type...")),

            textInput("calibration_pack",
              tip("Calibration pack",
                "Name or path of the Specim calibration pack (.scp file) used.
                 Auto-filled from the core scan .hdr file."),
              placeholder = "Auto-filled from .hdr"),

            numericInput("et_target",
              tip("ET_target (ms)",
                "Integration time for the core scan in milliseconds. Auto-filled
                 from the core .hdr file (tint field)."),
              value = NULL, min = 0),

            numericInput("et_white",
              tip("ET_white (ms)",
                "Integration time for the white reference scan in milliseconds.
                 Auto-filled from the white reference .hdr file (tint field).
                 In this protocol ET_dark = ET_white, so the dark current scaling
                 in the reflectance formula simplifies to 1."),
              value = NULL, min = 0),

            numericInput("log_fov",
              tip("FOV (mm)",
                "Field of view value entered in Lumo's Scanning speed calculation
                 panel, in millimeters."),
              value = NULL, min = 0),

            layout_columns(
              col_widths = c(6, 6),
              numericInput("spectral_binning",
                tip("Spectral binning",
                  "First value of the binning field in the .hdr file. Typical
                   production values: 1, 2, or 4. Auto-filled from .hdr."),
                value = NULL, min = 1),
              numericInput("spatial_binning",
                tip("Spatial binning",
                  "Second value of the binning field in the .hdr file. Always 1
                   in production scans; may differ in test scans. Auto-filled."),
                value = NULL, min = 1)
            )
          )
        ),

        # Motor Positions
        card(
          card_header("Motor positions", class = "bg-info text-white"),
          card_body(
            p(class = "text-muted small mb-3",
              "Motor positions are not saved in any Lumo output file.
               Record them from the Lumo display in your paper notebook,
               then enter here."),

            h6("Production scan"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("target_start",
                tip("Target start (mm)",
                  "Motor position displayed in Lumo at the beginning of the
                   core scan. Read from the Lumo interface."),
                value = NULL),
              numericInput("target_stop",
                tip("Target stop (mm)",
                  "Motor position displayed in Lumo at the end of the core
                   scan. Scan length = Stop − Start."),
                value = NULL)
            ),

            hr(),

            h6("Test scan (geometry calibration)"),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("test_scan_start",
                tip("Test start (mm)",
                  "Motor position at the start of the geometry test scan
                   used to calculate the ideal FOV (Tab 2)."),
                value = NULL),
              numericInput("test_scan_stop",
                tip("Test stop (mm)",
                  "Motor position at the end of the geometry test scan."),
                value = NULL)
            )
          )
        )
      ),

      # Row 2: Sample | Location | Results & QC
      layout_columns(
        col_widths = c(4, 4, 4),

        card(
          card_header("Sample", class = "bg-info text-white"),
          card_body(
            textInput("core_id", "Core ID", placeholder = "SITE-A"),
            textInput("section_depth",
              tip("Section depth (cm)",
                "Depth range of this core section in centimetres, e.g. 0-50."),
              placeholder = "0-50"),
            selectizeInput("material_type",
              "Material type",
              choices = NULL,
              options = list(create = TRUE, placeholder = "Select or type...")),
            textInput("material_owner",
              tip("Material owner",
                "Institution or person responsible for the material."),
              placeholder = "University of Gdańsk")
          )
        ),

        card(
          card_header("Location", class = "bg-info text-white"),
          card_body(
            textInput("site_name", "Site name", placeholder = "Łazy"),
            textInput("site_code",
              tip("Site code",
                "Short code used as the campaign prefix root, e.g. LAZ for Łazy."),
              placeholder = "LAZ24"),
            textInput("country", "Country", placeholder = "Poland")
          )
        ),

        card(
          card_header("Results & QC", class = "bg-warning text-dark"),
          card_body(
            numericInput("log_aspect_ratio",
              tip("Aspect ratio",
                "Measured aspect ratio from Tab 1, after the confirmation test
                 scan. Copy the value here for logging."),
              value = NULL, min = 0, step = 0.001),

            numericInput("log_total_lines",
              tip("Total scan lines",
                "Number of scan lines in the core scan. Auto-filled from .hdr."),
              value = NULL, min = 0),

            numericInput("log_dropped_frames",
              tip("Dropped frames",
                "Number of dropped frames from the .log file. Auto-filled
                 from .log. Any dropped frames invalidate affected scan lines."),
              value = NULL, min = 0),

            numericInput("saturation_ratio",
              tip("Saturation ratio (%)",
                "Fraction of core-area pixel positions where any single band
                 equals the detector ceiling DN. Calculated in HSItools post-
                 processing. Threshold: 0.1% — if exceeded, reduce ET_target
                 and re-scan."),
              value = NULL, min = 0, max = 100, step = 0.01),

            numericInput("gcp_pins",
              tip("GCP pins",
                "Number of steel pins placed in the sediment for VNIR–SWIR
                 co-registration. Required whenever co-registration is planned.
                 Both the VNIR and SWIR log entries for the same section must
                 record the same pin count."),
              value = NULL, min = 0)
          )
        )
      ),

      # Row 3: Filename | Notes
      layout_columns(
        col_widths = c(4, 8),

        card(
          card_header("File", class = "bg-secondary text-white"),
          card_body(
            textInput("log_filename",
              tip("Filename",
                "Auto-filled from the loaded .hdr file. Lumo names files as
                 PREFIX_CC-SS_TIMESTAMP. Used as the base name for the YAML
                 scan log file."),
              placeholder = "Auto-filled from .hdr")
          )
        ),

        card(
          card_header("Notes", class = "bg-secondary text-white"),
          card_body(
            textAreaInput("notes", NULL, rows = 2,
              placeholder = "Core condition, issues, anomalies, anything not captured above...")
          )
        )
      ),

      # Save buttons
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            actionButton("save_individual", "Save YAML",
              icon  = icon("file-export"),
              class = "btn-outline-primary w-100"),
            actionButton("save_master", "Append to master CSV",
              icon  = icon("database"),
              class = "btn-outline-success w-100"),
            actionButton("save_both", "Save Both",
              icon  = icon("floppy-disk"),
              class = "btn-primary w-100")
          )
        )
      )
    ) # end Tab 3 nav_panel
  )   # end navset_card_tab
)     # end page_sidebar

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  volumes <- getVolumes()()

  # File choosers — Tabs 1 & 2
  shinyFileChoose(input, "file_select",       roots = volumes, filetypes = c("raw", "tif", "tiff"))

  # File choosers — Tab 3
  shinyFileChoose(input, "core_hdr_select",   roots = volumes, filetypes = "hdr")
  shinyFileChoose(input, "core_log_select",   roots = volumes, filetypes = "log")
  shinyFileChoose(input, "white_hdr_select",  roots = volumes, filetypes = "hdr")
  shinyDirChoose(input,  "save_dir",          roots = volumes)

  # Material types (persisted in ~/.hsical)
  material_types <- reactiveVal(load_material_types())

  observe({
    updateSelectizeInput(session, "material_type",
                         choices = material_types(), server = TRUE)
  })

  observeEvent(input$material_type, {
    mt <- input$material_type
    if (!is.null(mt) && mt != "" && !mt %in% material_types()) {
      updated <- c(material_types(), mt)
      material_types(updated)
      save_material_types(updated)
    }
  })

  # --------------------------------------------------------------------------
  # Session locking
  # --------------------------------------------------------------------------

  session_locked <- reactiveVal(FALSE)

  # Instrument fields that are constant for the duration of a session
  SESSION_FIELDS <- c(
    "session_id", "operator", "camera", "lens", "calibration_pack",
    "et_target", "et_white", "log_fov", "spectral_binning", "spatial_binning"
  )

  # Scan-specific fields cleared between entries within the same session
  SCAN_TEXT_FIELDS <- c(
    "campaign_prefix", "dataset_name", "log_filename", "core_id",
    "section_depth", "material_owner", "site_name", "site_code",
    "country", "notes"
  )
  SCAN_NUMERIC_FIELDS <- c(
    "log_total_lines", "log_dropped_frames",
    "target_start", "target_stop",
    "test_scan_start", "test_scan_stop",
    "log_aspect_ratio", "saturation_ratio", "gcp_pins"
  )

  observeEvent(input$toggle_lock, {
    locked <- !session_locked()
    session_locked(locked)

    if (locked) {
      purrr::walk(SESSION_FIELDS, shinyjs::disable)
      updateActionButton(session, "toggle_lock",
                         label = "Unlock session",
                         icon  = icon("lock-open"))
      showNotification(
        "Session locked. Instrument fields are now read-only for this session.",
        type = "message"
      )
    } else {
      purrr::walk(SESSION_FIELDS, shinyjs::enable)
      updateActionButton(session, "toggle_lock",
                         label = "Lock session",
                         icon  = icon("lock"))
      showNotification("Session unlocked.", type = "message")
    }
  })

  observeEvent(input$clear_scan, {
    purrr::walk(
      SCAN_TEXT_FIELDS,
      \(f) updateTextInput(session, f, value = "")
    )
    purrr::walk(
      SCAN_NUMERIC_FIELDS,
      \(f) updateNumericInput(session, f, value = NA)
    )
    updateSelectizeInput(session, "material_type", selected = "")
    updateDateInput(session, "scan_date", value = Sys.Date())
    showNotification("Scan fields cleared. Ready for next entry.", type = "message")
  })

  # --------------------------------------------------------------------------
  # Session ID pre-fill hint
  # Pre-fills only when the field is empty — never overwrites operator input.
  # --------------------------------------------------------------------------

  observe({
    req(input$scan_date, input$camera)
    if (!session_locked() && nchar(trimws(input$session_id %||% "")) == 0) {
      stem <- paste0(format(input$scan_date, "%Y-%m-%d"), "-", input$camera, "_01")
      updateTextInput(session, "session_id", value = stem)
    }
  })

  # --------------------------------------------------------------------------
  # Tabs 1 & 2 — raster loading
  # --------------------------------------------------------------------------

  raster_path <- reactive({
    req(input$file_select)
    fi <- parseFilePaths(volumes, input$file_select)
    if (nrow(fi) == 0) return(NULL)
    as.character(fi$datapath)
  })

  output$file_path <- renderText({
    if (is.null(raster_path())) "No file selected" else basename(raster_path())
  })

  raster_data <- reactive({
    req(raster_path())
    tryCatch(terra::rast(raster_path()), error = \(e) NULL)
  })

  # --------------------------------------------------------------------------
  # Tab 1 — calculations
  # --------------------------------------------------------------------------

  check_calculations <- reactive({
    r     <- raster_data()
    start <- input$motor_start
    stop  <- input$motor_stop
    fov   <- input$scan_fov

    req(r, start, stop, fov)
    if (stop <= start) return(NULL)

    scan_len_um <- measurements::conv_unit(stop - start, "mm", "um")
    fov_um      <- measurements::conv_unit(fov,          "mm", "um")

    res_len <- scan_len_um / nrow(r)
    res_fov <- fov_um      / ncol(r)

    list(res_len = res_len, res_fov = res_fov, ratio = res_len / res_fov)
  })

  output$check_n_rows <- renderText({
    r <- raster_data()
    if (is.null(r)) "—" else format(nrow(r), big.mark = ",")
  })

  output$check_n_cols <- renderText({
    r <- raster_data()
    if (is.null(r)) "—" else format(ncol(r), big.mark = ",")
  })

  output$res_len <- renderText({
    calc <- check_calculations()
    if (is.null(calc)) "—" else paste(round(calc$res_len, 2), "µm/px")
  })

  output$res_fov <- renderText({
    calc <- check_calculations()
    if (is.null(calc)) "—" else paste(round(calc$res_fov, 2), "µm/px")
  })

  output$ratio_box <- renderUI({
    calc  <- check_calculations()
    ratio <- if (is.null(calc)) NA_real_ else calc$ratio
    tier  <- ratio_tier(ratio)
    value_box(
      title    = "Aspect ratio",
      value    = if (is.na(ratio)) "—" else round(ratio, 3),
      p(tier$label),
      showcase = bs_icon(tier$icon),
      theme    = tier$theme
    )
  })

  # --------------------------------------------------------------------------
  # Tab 2 — calculations
  # --------------------------------------------------------------------------

  true_pixel_res <- reactive({
    r     <- raster_data()
    start <- input$motor_start
    stop  <- input$motor_stop

    req(r, start, stop)
    if (stop <= start) return(NULL)

    measurements::conv_unit(stop - start, "mm", "um") / nrow(r)
  })

  ideal_fov <- reactive({
    res <- true_pixel_res()
    r   <- raster_data()

    req(res, r)
    fov_um <- res * ncol(r)

    list(
      um = fov_um,
      mm = measurements::conv_unit(fov_um, "um", "mm"),
      cm = measurements::conv_unit(fov_um, "um", "cm")
    )
  })

  output$calc_n_rows <- renderText({
    r <- raster_data()
    if (is.null(r)) "—" else format(nrow(r), big.mark = ",")
  })

  output$calc_n_cols <- renderText({
    r <- raster_data()
    if (is.null(r)) "—" else format(ncol(r), big.mark = ",")
  })

  output$true_pixel_size <- renderText({
    res <- true_pixel_res()
    if (is.null(res)) "—" else paste(round(res, 2), "µm/px")
  })

  output$ideal_fov_mm <- renderText({
    fov <- ideal_fov()
    if (is.null(fov)) "—" else round(fov$mm, 3)
  })

  output$ideal_fov_um <- renderText({
    fov <- ideal_fov()
    if (is.null(fov)) "—" else format(round(fov$um, 1), big.mark = ",")
  })

  output$ideal_fov_cm <- renderText({
    fov <- ideal_fov()
    if (is.null(fov)) "—" else round(fov$cm, 4)
  })

  # --------------------------------------------------------------------------
  # Tab 3 — file path reactives
  # --------------------------------------------------------------------------

  core_hdr_path <- reactive({
    req(input$core_hdr_select)
    fi <- parseFilePaths(volumes, input$core_hdr_select)
    if (nrow(fi) == 0) return(NULL)
    as.character(fi$datapath)
  })

  core_log_path <- reactive({
    req(input$core_log_select)
    fi <- parseFilePaths(volumes, input$core_log_select)
    if (nrow(fi) == 0) return(NULL)
    as.character(fi$datapath)
  })

  white_hdr_path <- reactive({
    req(input$white_hdr_select)
    fi <- parseFilePaths(volumes, input$white_hdr_select)
    if (nrow(fi) == 0) return(NULL)
    as.character(fi$datapath)
  })

  selected_save_dir <- reactive({
    req(input$save_dir)
    path <- parseDirPath(volumes, input$save_dir)
    if (length(path) == 0) return(NULL)
    as.character(path)
  })

  output$core_hdr_path  <- renderText({
    if (is.null(core_hdr_path()))  "No file selected" else basename(core_hdr_path())
  })
  output$core_log_path  <- renderText({
    if (is.null(core_log_path()))  "No file selected" else basename(core_log_path())
  })
  output$white_hdr_path <- renderText({
    if (is.null(white_hdr_path())) "No file selected" else basename(white_hdr_path())
  })
  output$save_dir_path  <- renderText({
    if (is.null(selected_save_dir())) "No folder selected" else selected_save_dir()
  })

  # --------------------------------------------------------------------------
  # Tab 3 — load core metadata (HDR + LOG)
  # --------------------------------------------------------------------------

  observeEvent(input$load_core, {
    req(core_hdr_path())
    hdr <- parse_hdr(core_hdr_path())

    if (!is.null(hdr)) {
      # Instrument fields: respect session lock
      if (!session_locked()) {
        if (!is.na(hdr$camera))           updateSelectInput(session,  "camera",            value = hdr$camera)
        if (!is.na(hdr$tint))             updateNumericInput(session, "et_target",          value = round(hdr$tint, 3))
        if (!is.na(hdr$spectral_binning)) updateNumericInput(session, "spectral_binning",   value = hdr$spectral_binning)
        if (!is.na(hdr$spatial_binning))  updateNumericInput(session, "spatial_binning",    value = hdr$spatial_binning)
        if (!is.na(hdr$calibration_pack)) updateTextInput(session,    "calibration_pack",   value = hdr$calibration_pack)
      }

      # Scan-level fields: always update
      if (!is.na(hdr$acquisition_date)) updateDateInput(session,    "scan_date",         value = as.Date(hdr$acquisition_date))
      if (!is.na(hdr$lines))            updateNumericInput(session, "log_total_lines",   value = hdr$lines)
      updateTextInput(session, "log_filename", value = basename(core_hdr_path()))
    }

    # Load dropped frames from .log if already selected
    if (!is.null(core_log_path())) {
      log_data <- parse_log(core_log_path())
      if (!is.na(log_data$dropped))
        updateNumericInput(session, "log_dropped_frames", value = log_data$dropped)
    }

    msg <- if (session_locked())
      "Core metadata loaded (instrument fields skipped — session is locked)."
    else
      "Core metadata loaded from .hdr."

    showNotification(msg, type = "message")
  })

  # Load .log separately if selected after core was already loaded
  observeEvent(input$core_log_select, {
    req(core_log_path())
    log_data <- parse_log(core_log_path())
    if (!is.na(log_data$dropped))
      updateNumericInput(session, "log_dropped_frames", value = log_data$dropped)
  })

  # --------------------------------------------------------------------------
  # Tab 3 — load ET_white from white reference HDR
  # --------------------------------------------------------------------------

  observeEvent(input$load_white, {
    req(white_hdr_path())
    white_hdr <- parse_hdr(white_hdr_path())

    if (is.null(white_hdr) || is.na(white_hdr$tint)) {
      showNotification("Could not read tint from white reference .hdr", type = "warning")
      return()
    }

    # ET_white is a session-level field; respect session lock
    if (!session_locked())
      updateNumericInput(session, "et_white", value = round(white_hdr$tint, 3))

    showNotification("ET_white loaded from white reference .hdr", type = "message")

    # Sanity check: camera and binning must match the core scan .hdr.
    # Mismatches warn but never block — the operator is the authority.
    core_camera   <- input$camera
    core_spec_bin <- input$spectral_binning
    core_spat_bin <- input$spatial_binning

    if (!is.na(white_hdr$camera) &&
        nzchar(core_camera) &&
        !identical(white_hdr$camera, core_camera)) {
      showNotification(
        paste0("Camera mismatch: core is ", core_camera,
               " but white ref reports ", white_hdr$camera, "."),
        type = "warning", duration = 10
      )
    }

    if (!is.na(white_hdr$spectral_binning) &&
        !is.na(core_spec_bin) &&
        !isTRUE(all.equal(white_hdr$spectral_binning, core_spec_bin))) {
      showNotification(
        paste0("Spectral binning mismatch: core is ", core_spec_bin,
               " but white ref reports ", white_hdr$spectral_binning, "."),
        type = "warning", duration = 10
      )
    }

    if (!is.na(white_hdr$spatial_binning) &&
        !is.na(core_spat_bin) &&
        !isTRUE(all.equal(white_hdr$spatial_binning, core_spat_bin))) {
      showNotification(
        paste0("Spatial binning mismatch: core is ", core_spat_bin,
               " but white ref reports ", white_hdr$spatial_binning, "."),
        type = "warning", duration = 10
      )
    }
  })

  # --------------------------------------------------------------------------
  # Tab 3 — collect form data
  # --------------------------------------------------------------------------

  collect_form_data <- reactive({
    list(
      session_id       = input$session_id       %||% "",
      operator         = input$operator         %||% "",
      campaign_prefix  = input$campaign_prefix  %||% "",
      dataset_name     = input$dataset_name     %||% "",
      scan_date        = as.character(input$scan_date),
      camera           = input$camera           %||% "",
      lens             = input$lens             %||% "",
      calibration_pack = input$calibration_pack %||% "",
      et_target        = input$et_target,
      et_white         = input$et_white,
      fov              = input$log_fov,
      spectral_binning = input$spectral_binning,
      spatial_binning  = input$spatial_binning,
      target_start     = input$target_start,
      target_stop      = input$target_stop,
      test_scan_start  = input$test_scan_start,
      test_scan_stop   = input$test_scan_stop,
      aspect_ratio     = input$log_aspect_ratio,
      total_lines      = input$log_total_lines,
      dropped_frames   = input$log_dropped_frames,
      saturation_ratio = input$saturation_ratio,
      gcp_pins         = input$gcp_pins,
      core_id          = input$core_id          %||% "",
      section_depth    = input$section_depth    %||% "",
      material_type    = input$material_type    %||% "",
      material_owner   = input$material_owner   %||% "",
      site_name        = input$site_name        %||% "",
      site_code        = input$site_code        %||% "",
      country          = input$country          %||% "",
      filename         = input$log_filename     %||% "",
      notes            = input$notes            %||% ""
    )
  })

  # --------------------------------------------------------------------------
  # Tab 3 — validation
  # --------------------------------------------------------------------------

  run_validation <- function(d) {
    result <- validate_form(d)

    if (!result$ok) {
      showNotification(
        paste0("Missing required fields: ", paste(result$fields, collapse = ", "), "."),
        type     = "error",
        duration = 12
      )
    }

    # Session ID format: soft warning, does not block save
    if (nzchar(trimws(d$session_id)) && !is_valid_session_id(d$session_id)) {
      showNotification(
        paste0(
          "Session ID '", d$session_id, "' does not match the expected format ",
          "YYYY-MM-DD-CAMERA_NN (e.g. 2026-03-15-VNIR_01). Saving anyway."
        ),
        type     = "warning",
        duration = 10
      )
    }

    result$ok
  }

  # --------------------------------------------------------------------------
  # Tab 3 — save functions
  # --------------------------------------------------------------------------

  save_yaml <- function() {
    d <- collect_form_data()
    if (!run_validation(d)) return(invisible(FALSE))

    dir <- selected_save_dir()
    if (is.null(dir)) {
      showNotification("Please select a save folder first.", type = "error")
      return(invisible(FALSE))
    }

    base <- if (nzchar(d$filename)) tools::file_path_sans_ext(d$filename)
            else format(Sys.time(), "%Y%m%d_%H%M%S")
    out  <- file.path(dir, paste0(base, "_scanlog.yaml"))

    tryCatch({
      writeLines(generate_yaml(d), out)
      showNotification(paste("Saved:", basename(out)), type = "message")
      invisible(TRUE)
    }, error = \(e) {
      showNotification(paste("Error saving YAML:", e$message), type = "error")
      invisible(FALSE)
    })
  }

  save_csv <- function() {
    d <- collect_form_data()
    if (!run_validation(d)) return(invisible(FALSE))

    master <- file.path(getwd(), "hsical_master_log.csv")
    row    <- generate_csv_row(d)

    tryCatch({
      if (file.exists(master)) {
        write.table(row, master, append = TRUE, sep = ",",
                    row.names = FALSE, col.names = FALSE, quote = TRUE)
      } else {
        write.csv(row, master, row.names = FALSE, quote = TRUE)
      }
      showNotification(paste("Appended to:", master), type = "message")
      invisible(TRUE)
    }, error = \(e) {
      showNotification(paste("Error saving CSV:", e$message), type = "error")
      invisible(FALSE)
    })
  }

  observeEvent(input$save_individual, save_yaml())
  observeEvent(input$save_master,     save_csv())
  observeEvent(input$save_both,       { save_yaml(); save_csv() })
}

# ============================================================================
# Run
# ============================================================================

shinyApp(ui, server)

# hsical — Specim single-core scanner calibration & logging companion
# Rebuild 2026-07-11 (contract v3.0): two modes (Calibrate | Log) over the
# HSItools metadata sidecar. hsical owns no schema — the Log form is an
# argument collector for HSItools::hsi_create_metadata(); writing and reading
# sidecars belong to HSItools. All calls ::-qualified, native pipe, \(x) lambdas.

# ==========================================================================
# Helpers
# ==========================================================================

# Label with an info tooltip
tip <- function(label, text) {
  shiny::tagList(
    label,
    bslib::tooltip(
      bsicons::bs_icon(
        "info-circle",
        size = "0.85em",
        class = "text-muted ms-1"
      ),
      text,
      placement = "right"
    )
  )
}

# Parse ENVI .hdr file. Scalars via xnum/xval; wavelength + fwhm vectors via xvec.
parse_hdr <- function(hdr_path) {
  if (!file.exists(hdr_path)) {
    return(NULL)
  }

  content <- readLines(hdr_path, warn = FALSE) |> paste(collapse = "\n")

  xval <- function(pattern) {
    m <- regmatches(content, regexpr(pattern, content, perl = TRUE))
    if (length(m) == 0 || identical(m, character(0)) || m == "") {
      NA_character_
    } else {
      m
    }
  }

  xnum <- function(key) {
    v <- xval(paste0("(?<=", key, " = )[0-9.]+"))
    if (is.na(v)) NA_real_ else as.numeric(v)
  }

  # Numeric vector from a brace-delimited block; NULL if absent. Case-insensitive
  # key, tolerant of spacing around `=`/`{`; block may span many lines with comma-
  # and/or newline-separated values. Capture group (no lookbehind) — a `(?i)` flag
  # in front of a PCRE lookbehind silently fails to match in R.
  xvec <- function(key) {
    m <- regmatches(
      content,
      regexec(
        paste0("(?i)", key, "\\s*=\\s*\\{([^}]*)\\}"),
        content,
        perl = TRUE
      )
    )[[1]]
    if (length(m) < 2) {
      return(NULL)
    }
    v <- strsplit(m[[2]], "[,\\s]+", perl = TRUE)[[1]]
    v <- suppressWarnings(as.numeric(v[nzchar(v)]))
    v[!is.na(v)]
  }

  # binning = {spectral, spatial}
  bin_m <- regmatches(
    content,
    regexpr("(?<=binning = \\{)[0-9]+, [0-9]+(?=\\})", content, perl = TRUE)
  )
  if (length(bin_m) > 0 && bin_m != "") {
    b <- as.numeric(strsplit(bin_m, ", ")[[1]])
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
  cal <- if (!is.na(cal_raw)) trimws(cal_raw) else NA_character_

  list(
    lines = xnum("lines"),
    samples = xnum("samples"),
    bands = xnum("bands"),
    fps = xnum("fps"),
    tint = xnum("tint"),
    spectral_binning = spec_bin,
    spatial_binning = spat_bin,
    camera = camera,
    calibration_pack = cal,
    acquisition_date = xval(
      "(?<=acquisition date = DATE\\(yyyy-mm-dd\\): )[0-9-]+"
    ),
    start_time = xval("(?<=Start Time = UTC TIME: )[0-9:]+"),
    stop_time = xval("(?<=Stop Time = UTC TIME: )[0-9:]+"),
    wavelengths = xvec("Wavelength"),
    fwhm = xvec("fwhm")
  )
}

# Parse Lumo .log file
parse_log <- function(log_path) {
  if (!file.exists(log_path)) {
    return(list(dropped = NA_real_, recorded = NA_real_))
  }

  content <- readLines(log_path, warn = FALSE) |> paste(collapse = "\n")

  dropped <- regmatches(
    content,
    regexpr("(?<=incidents, )[0-9]+(?= dropped frames)", content, perl = TRUE)
  )
  recorded <- regmatches(
    content,
    regexpr("[0-9]+(?= frames recorded)", content, perl = TRUE)
  )

  list(
    dropped = if (length(dropped) == 0) NA_real_ else as.numeric(dropped),
    recorded = if (length(recorded) == 0) NA_real_ else as.numeric(recorded)
  )
}

# Three-tier aspect ratio classification -> list(theme, icon, label)
ratio_tier <- function(ratio) {
  if (is.na(ratio)) {
    return(list(theme = "secondary", icon = "square", label = "—"))
  }
  if (ratio >= 0.95 && ratio <= 1.05) {
    return(list(
      theme = "success",
      icon = "check-square",
      label = "\u2713 Square pixels"
    ))
  }
  if (ratio >= 0.90 && ratio <= 1.10) {
    return(list(
      theme = "warning",
      icon = "exclamation-square",
      label = "\u26a0 Nearly square \u2014 check"
    ))
  }
  list(
    theme = "danger",
    icon = "x-square",
    label = "\u2717 Not square \u2014 adjust FOV"
  )
}

# Log fields stable across a scanning session: carried between saves (iter 6),
# blanked by "Clear session". Split by input type so updates use the right fn.
# log_sensor_type is session-stable too but a selectize — handled separately.
SESSION_TEXT <- c(
  "log_session_id",
  "log_operator",
  "log_campaign_prefix",
  "log_dataset_name",
  "log_manufacturer",
  "log_lens",
  "log_calibration_pack"
)
SESSION_NUMERIC <- c(
  "log_fov_mm",
  "log_et_target_ms",
  "log_et_white_ms",
  "log_spectral_binning",
  "log_spatial_binning",
  "log_camera_position_mm",
  "log_stage_position_mm"
)

# Per-capture fields: blanked after a successful save (session-stable ones stay).
PER_CAPTURE_TEXT <- c("log_name")
PER_CAPTURE_NUMERIC <- c(
  "log_target_start_mm",
  "log_target_stop_mm",
  "log_scanning_speed_mm_s",
  "log_aspect_ratio",
  "log_nrow",
  "log_ncol",
  "log_nlyr",
  "log_frame_rate_hz",
  "log_spectral_resolution_nm",
  "log_xres",
  "log_yres",
  "log_dropped_frames",
  "log_gcp_count"
)

# Blank -> NULL: hsi_create_metadata() wants NULL for absent fields, never "" or NA
# (a blank numericInput returns logical NA, so test is.na() before type).
nz <- function(v) {
  if (is.null(v) || length(v) == 0) {
    return(NULL)
  }
  if (length(v) == 1 && is.na(v)) {
    return(NULL)
  }
  if (is.character(v)) {
    v <- trimws(v)
    if (!nzchar(v)) return(NULL)
  }
  v
}

# ==========================================================================
# UI — two modes, one page_navbar
# ==========================================================================

ui <- bslib::page_navbar(
  title = "hsical",
  theme = bslib::bs_theme(version = 5, primary = "#2c6e8f"),

  bslib::nav_panel(
    title = "Calibrate",

    # ---- Card 1: Ideal FOV ------------------------------------------------
    bslib::card(
      bslib::card_header("Ideal FOV"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::div(
          shinyFiles::shinyFilesButton(
            "fov_hdr",
            "Load test-scan .hdr",
            "Select test-scan .hdr",
            multiple = FALSE,
            icon = bsicons::bs_icon("file-earmark-text")
          ),
          shiny::numericInput(
            "fov_lines",
            tip(
              "Lines (along-track)",
              "Test-scan .hdr `lines`, or type manually."
            ),
            value = NULL,
            min = 1
          ),
          shiny::numericInput(
            "fov_samples",
            tip(
              "Samples (across-track)",
              "Test-scan .hdr `samples`, or type manually."
            ),
            value = NULL,
            min = 1
          ),
          shiny::numericInput(
            "target_start_mm",
            tip(
              "Target start (mm)",
              "Motor position at scan start (Lumo display)."
            ),
            value = NULL
          ),
          shiny::numericInput(
            "target_stop_mm",
            tip(
              "Target stop (mm)",
              "Motor position at scan stop (Lumo display)."
            ),
            value = NULL
          )
        ),
        shiny::div(
          shiny::strong("Scan length: "),
          shiny::textOutput("cal_scan_len", inline = TRUE),
          shiny::br(),
          shiny::strong("Ideal FOV: "),
          shiny::textOutput("cal_ideal_fov", inline = TRUE),
          shiny::br(),
          shiny::strong("Square pixel size: "),
          shiny::textOutput("cal_square_px", inline = TRUE)
        )
      )
    ),

    # ---- Card 2: Aspect check --------------------------------------------
    bslib::card(
      bslib::card_header("Aspect check"),
      bslib::layout_columns(
        col_widths = c(6, 6),
        shiny::div(
          shinyFiles::shinyFilesButton(
            "asp_hdr",
            "Load confirmation-scan .hdr",
            "Select confirmation-scan .hdr",
            multiple = FALSE,
            icon = bsicons::bs_icon("file-earmark-text")
          ),
          shiny::numericInput(
            "asp_lines",
            tip(
              "Lines (along-track)",
              "Confirmation-scan .hdr `lines`, or type manually."
            ),
            value = NULL,
            min = 1
          ),
          shiny::numericInput(
            "asp_samples",
            tip(
              "Samples (across-track)",
              "Confirmation-scan .hdr `samples`, or type manually."
            ),
            value = NULL,
            min = 1
          ),
          shiny::numericInput(
            "fov_set_mm",
            tip(
              "FOV as set (mm)",
              "FOV entered in Lumo for the confirmation scan."
            ),
            value = NULL,
            min = 0
          ),
          shiny::helpText("Uses target start/stop from the Ideal FOV card.")
        ),
        shiny::div(
          shiny::uiOutput("cal_ratio_box"),
          shiny::br(),
          shiny::strong("Along-track pixel: "),
          shiny::textOutput("cal_along_um", inline = TRUE),
          shiny::br(),
          shiny::strong("Across-track pixel: "),
          shiny::textOutput("cal_across_um", inline = TRUE)
        )
      )
    )
  ),

  bslib::nav_panel(
    title = "Log",
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex gap-3 flex-wrap align-items-start",
          shiny::div(
            shinyFiles::shinyFilesButton(
              "log_target_hdr",
              "Load target .hdr",
              "Select target scan .hdr",
              multiple = FALSE,
              icon = bsicons::bs_icon("file-earmark-text")
            ),
            shiny::div(
              class = "small text-muted",
              shiny::textOutput("log_target_file")
            )
          ),
          shiny::div(
            shinyFiles::shinyFilesButton(
              "log_white_hdr",
              "Load white-ref .hdr",
              "Select white-reference .hdr",
              multiple = FALSE,
              icon = bsicons::bs_icon("file-earmark-text")
            ),
            shiny::div(
              class = "small text-muted",
              shiny::textOutput("log_white_file")
            )
          ),
          shiny::div(
            shinyFiles::shinyFilesButton(
              "log_log_file",
              "Load .log",
              "Select Lumo .log",
              multiple = FALSE,
              icon = bsicons::bs_icon("file-earmark-text")
            ),
            shiny::div(
              class = "small text-muted",
              shiny::textOutput("log_log_fname")
            )
          )
        )
      ),
      bslib::accordion(
        id = "log_accordion",
        open = TRUE,
        multiple = TRUE,

        bslib::accordion_panel(
          "Capture",
          icon = bsicons::bs_icon("file-earmark"),
          shiny::textInput(
            "log_name",
            tip(
              "Name",
              "Capture name; autofills from the .hdr filename (iter 5)."
            )
          )
        ),

        bslib::accordion_panel(
          "Session",
          icon = bsicons::bs_icon("collection"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::textInput(
              "log_session_id",
              tip(
                "Session ID",
                "e.g. LAZ-26-S1. Groups scans sharing a white reference."
              )
            ),
            shiny::textInput(
              "log_operator",
              tip("Operator", "Full first + last name.")
            ),
            shiny::textInput(
              "log_campaign_prefix",
              tip("Campaign prefix", "Lumo Setup value, e.g. LAZ-26.")
            ),
            shiny::textInput(
              "log_dataset_name",
              tip("Dataset name", "Lumo Capture value, CC-SS.")
            )
          )
        ),

        bslib::accordion_panel(
          "Instrument",
          icon = bsicons::bs_icon("camera"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::selectizeInput(
              "log_sensor_type",
              tip("Sensor type", "VNIR / SWIR, or type a custom value."),
              choices = c("VNIR", "SWIR"),
              selected = character(0),
              options = list(
                create = TRUE,
                placeholder = "VNIR / SWIR / custom"
              )
            ),
            shiny::textInput(
              "log_manufacturer",
              tip("Manufacturer", "e.g. Specim.")
            ),
            shiny::textInput(
              "log_lens",
              tip("Lens", "Free text, e.g. 18.5 mm.")
            ),
            shiny::textInput(
              "log_calibration_pack",
              tip("Calibration pack", "Autofills from the .hdr (iter 5).")
            )
          )
        ),

        bslib::accordion_panel(
          "Geometry",
          icon = bsicons::bs_icon("rulers"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput(
              "log_target_start_mm",
              tip("Target start (mm)", "Motor position at scan start."),
              value = NA
            ),
            shiny::numericInput(
              "log_target_stop_mm",
              tip("Target stop (mm)", "Motor position at scan stop."),
              value = NA
            ),
            shiny::numericInput(
              "log_fov_mm",
              tip("FOV (mm)", "FOV set in Lumo."),
              value = NA
            ),
            shiny::numericInput(
              "log_scanning_speed_mm_s",
              tip("Scanning speed (mm/s)", "As set in Lumo."),
              value = NA
            ),
            shiny::numericInput(
              "log_camera_position_mm",
              tip("Camera position (mm)", "Enables future focus-signature QC."),
              value = NA
            ),
            shiny::numericInput(
              "log_stage_position_mm",
              tip("Stage position (mm)", "Enables future focus-signature QC."),
              value = NA
            )
          ),
          shiny::div(
            class = "d-flex align-items-end gap-2",
            shiny::numericInput(
              "log_aspect_ratio",
              tip("Aspect ratio", "From Calibrate, or type manually."),
              value = NA
            ),
            shiny::actionButton(
              "log_use_ratio",
              "Use measured ratio",
              icon = bsicons::bs_icon("box-arrow-in-down")
            )
          )
        ),

        bslib::accordion_panel(
          "Acquisition",
          icon = bsicons::bs_icon("grid-3x3"),
          bslib::layout_columns(
            col_widths = c(4, 4, 4),
            shiny::numericInput(
              "log_nrow",
              tip("Rows (lines)", "From .hdr `lines` (iter 5)."),
              value = NA,
              min = 1
            ),
            shiny::numericInput(
              "log_ncol",
              tip("Cols (samples)", "From .hdr `samples`."),
              value = NA,
              min = 1
            ),
            shiny::numericInput(
              "log_nlyr",
              tip("Bands", "From .hdr `bands`."),
              value = NA,
              min = 1
            ),
            shiny::numericInput(
              "log_et_target_ms",
              tip("ET target (ms)", "From target .hdr `tint`."),
              value = NA
            ),
            shiny::numericInput(
              "log_et_white_ms",
              tip("ET white (ms)", "From the white-reference .hdr `tint`."),
              value = NA
            ),
            shiny::numericInput(
              "log_frame_rate_hz",
              tip("Frame rate (Hz)", "From .hdr `fps`."),
              value = NA
            ),
            shiny::numericInput(
              "log_spectral_binning",
              tip("Spectral binning", "From .hdr binning."),
              value = NA
            ),
            shiny::numericInput(
              "log_spatial_binning",
              tip("Spatial binning", "From .hdr binning."),
              value = NA
            ),
            shiny::numericInput(
              "log_spectral_resolution_nm",
              tip(
                "Spectral resolution (nm)",
                "Calibrated value or leave blank."
              ),
              value = NA
            ),
            shiny::numericInput(
              "log_xres",
              tip(
                "xres (\u00b5m)",
                "Calibrated pixel size; blank if not calibrated \u2014 never nominal."
              ),
              value = NA
            ),
            shiny::numericInput(
              "log_yres",
              tip(
                "yres (\u00b5m)",
                "Calibrated pixel size; blank if not calibrated \u2014 never nominal."
              ),
              value = NA
            )
          ),
          shiny::div(
            shiny::strong("Spectral axes: "),
            shiny::textOutput("log_spectral_chip", inline = TRUE)
          )
        ),

        bslib::accordion_panel(
          "QC",
          icon = bsicons::bs_icon("clipboard-check"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput(
              "log_dropped_frames",
              tip("Dropped frames", "From the .log (iter 5). Zero is valid."),
              value = NA,
              min = 0
            ),
            shiny::numericInput(
              "log_gcp_count",
              tip("GCP count", "Number of ground-control pins. Zero is valid."),
              value = NA,
              min = 0
            )
          )
        )
      ),
      bslib::card_footer(
        shiny::div(
          class = "d-flex flex-column gap-2",
          shiny::div(
            class = "d-flex align-items-center gap-2 flex-wrap",
            shinyFiles::shinyDirButton(
              "log_save_dir",
              "Folder\u2026",
              "Select save folder",
              icon = bsicons::bs_icon("folder")
            ),
            shiny::span(
              shiny::strong("Save to: "),
              shiny::textOutput("log_save_target", inline = TRUE)
            ),
            shiny::checkboxInput("log_overwrite", "Overwrite", value = FALSE)
          ),
          shiny::div(
            class = "d-flex gap-2",
            shiny::actionButton(
              "log_save",
              "Save sidecar",
              icon = bsicons::bs_icon("save"),
              class = "btn-primary"
            ),
            shiny::actionButton(
              "log_clear_session",
              "Clear session",
              icon = bsicons::bs_icon("eraser"),
              class = "btn-outline-secondary"
            )
          )
        )
      )
    )
  ),

  bslib::nav_panel(
    title = "Review",
    bslib::card(
      bslib::card_header(
        shinyFiles::shinyFilesButton(
          "review_yaml",
          "Load sidecar",
          "Select a .yaml sidecar",
          multiple = FALSE,
          icon = bsicons::bs_icon("file-earmark-text")
        )
      ),
      bslib::card_body(
        shiny::tableOutput("review_table")
      )
    )
  )
)

# ==========================================================================
# Server
# ==========================================================================

server <- function(input, output, session) {
  volumes <- shinyFiles::getVolumes()()

  # Autofilled spectral axes (never form fields): set on target .hdr load,
  # feeds the chip and the save call (iter 6).
  spectral <- shiny::reactiveVal(NULL)

  # Default sidecar folder: the scan root (parent of the capture/ folder that
  # holds the .hdr), overridable via the Folder… button.
  save_dir <- shiny::reactiveVal(NULL)

  # Sidecar loaded into the Review panel (read-only).
  review_md <- shiny::reactiveVal(NULL)

  # Basenames of the last-loaded scan files, shown beside each Log loader.
  f_target <- shiny::reactiveVal("")
  f_white <- shiny::reactiveVal("")
  f_log <- shiny::reactiveVal("")

  # ---- Calibrate: .hdr pickers ----------------------------------------
  shinyFiles::shinyFileChoose(
    input,
    "fov_hdr",
    roots = volumes,
    filetypes = "hdr"
  )
  shinyFiles::shinyFileChoose(
    input,
    "asp_hdr",
    roots = volumes,
    filetypes = "hdr"
  )

  # ---- Log: file pickers ----------------------------------------------
  shinyFiles::shinyFileChoose(
    input,
    "log_target_hdr",
    roots = volumes,
    filetypes = "hdr"
  )
  shinyFiles::shinyFileChoose(
    input,
    "log_white_hdr",
    roots = volumes,
    filetypes = "hdr"
  )
  shinyFiles::shinyFileChoose(
    input,
    "log_log_file",
    roots = volumes,
    filetypes = "log"
  )
  shinyFiles::shinyDirChoose(input, "log_save_dir", roots = volumes)
  shinyFiles::shinyFileChoose(
    input,
    "review_yaml",
    roots = volumes,
    filetypes = c("yaml", "yml")
  )

  # Autofill lines/samples from a chosen .hdr; fields stay editable.
  shiny::observeEvent(input$fov_hdr, {
    fi <- shinyFiles::parseFilePaths(volumes, input$fov_hdr)
    if (nrow(fi) == 0) {
      return()
    }
    hdr <- parse_hdr(fi$datapath[[1]])
    if (!is.na(hdr[["lines"]])) {
      shiny::updateNumericInput(session, "fov_lines", value = hdr[["lines"]])
    }
    if (!is.na(hdr[["samples"]])) {
      shiny::updateNumericInput(
        session,
        "fov_samples",
        value = hdr[["samples"]]
      )
    }
  })

  shiny::observeEvent(input$asp_hdr, {
    fi <- shinyFiles::parseFilePaths(volumes, input$asp_hdr)
    if (nrow(fi) == 0) {
      return()
    }
    hdr <- parse_hdr(fi$datapath[[1]])
    if (!is.na(hdr[["lines"]])) {
      shiny::updateNumericInput(session, "asp_lines", value = hdr[["lines"]])
    }
    if (!is.na(hdr[["samples"]])) {
      shiny::updateNumericInput(
        session,
        "asp_samples",
        value = hdr[["samples"]]
      )
    }
  })

  # ---- Shared: scan length (mm) ---------------------------------------
  scan_length_mm <- shiny::reactive({
    start <- input$target_start_mm
    stop <- input$target_stop_mm
    shiny::req(start, stop)
    if (stop <= start) {
      return(NULL)
    }
    stop - start
  })

  # ---- Card 1: Ideal FOV ----------------------------------------------
  fov_calc <- shiny::reactive({
    len <- scan_length_mm()
    lines <- input$fov_lines
    samp <- input$fov_samples
    shiny::req(len, lines, samp)
    if (lines <= 0 || samp <= 0) {
      return(NULL)
    }
    along_um <- len * 1000 / lines
    list(along_um = along_um, ideal_fov_mm = along_um * samp / 1000)
  })

  output$cal_scan_len <- shiny::renderText({
    len <- scan_length_mm()
    if (is.null(len)) "\u2014" else paste(round(len, 2), "mm")
  })
  output$cal_ideal_fov <- shiny::renderText({
    fc <- fov_calc()
    if (is.null(fc)) "\u2014" else paste(round(fc$ideal_fov_mm, 3), "mm")
  })
  output$cal_square_px <- shiny::renderText({
    fc <- fov_calc()
    if (is.null(fc)) "\u2014" else paste(round(fc$along_um, 2), "\u00b5m/px")
  })

  # ---- Card 2: Aspect check -------------------------------------------
  asp_calc <- shiny::reactive({
    len <- scan_length_mm()
    lines <- input$asp_lines
    samp <- input$asp_samples
    fov <- input$fov_set_mm
    shiny::req(len, lines, samp, fov)
    if (lines <= 0 || samp <= 0 || fov <= 0) {
      return(NULL)
    }
    along_um <- len * 1000 / lines
    across_um <- fov * 1000 / samp
    list(
      along_um = along_um,
      across_um = across_um,
      ratio = along_um / across_um
    )
  })

  # Bridge to Log mode: latest confirmed aspect ratio (NA until valid).
  calibrate_ratio <- shiny::reactive({
    ac <- asp_calc()
    if (is.null(ac)) NA_real_ else ac$ratio
  })

  output$cal_ratio_box <- shiny::renderUI({
    ratio <- calibrate_ratio()
    tier <- ratio_tier(ratio)
    bslib::value_box(
      title = "Aspect ratio",
      value = if (is.na(ratio)) "\u2014" else round(ratio, 3),
      shiny::p(tier$label),
      showcase = bsicons::bs_icon(tier$icon),
      theme = tier$theme
    )
  })
  output$cal_along_um <- shiny::renderText({
    ac <- asp_calc()
    if (is.null(ac)) "\u2014" else paste(round(ac$along_um, 2), "\u00b5m/px")
  })
  output$cal_across_um <- shiny::renderText({
    ac <- asp_calc()
    if (is.null(ac)) "\u2014" else paste(round(ac$across_um, 2), "\u00b5m/px")
  })

  # ---- Log: cross-fills and session reset -----------------------------
  shiny::observeEvent(input$log_use_ratio, {
    r <- calibrate_ratio()
    if (!is.na(r)) {
      shiny::updateNumericInput(
        session,
        "log_aspect_ratio",
        value = round(r, 4)
      )
    }
  })

  shiny::observeEvent(input$log_clear_session, {
    purrr::walk(SESSION_TEXT, \(id) {
      shiny::updateTextInput(session, id, value = "")
    })
    purrr::walk(SESSION_NUMERIC, \(id) {
      shiny::updateNumericInput(session, id, value = NA)
    })
    shiny::updateSelectizeInput(
      session,
      "log_sensor_type",
      selected = character(0)
    )
  })

  # ---- Log: autofill from scan files ----------------------------------
  shiny::observeEvent(input$log_target_hdr, {
    fi <- shinyFiles::parseFilePaths(volumes, input$log_target_hdr)
    if (nrow(fi) == 0) {
      return()
    }
    path <- fi$datapath[[1]]
    hdr <- parse_hdr(path)
    if (is.null(hdr)) {
      return()
    }

    shiny::updateTextInput(
      session,
      "log_name",
      value = tools::file_path_sans_ext(basename(path))
    )
    save_dir(dirname(dirname(path))) # scan root: the parent of the capture/ dir
    f_target(basename(path))
    if (!is.na(hdr[["camera"]])) {
      shiny::updateSelectizeInput(
        session,
        "log_sensor_type",
        selected = hdr[["camera"]]
      )
    }
    if (!is.na(hdr[["calibration_pack"]])) {
      shiny::updateTextInput(
        session,
        "log_calibration_pack",
        value = hdr[["calibration_pack"]]
      )
    }
    if (!is.na(hdr[["lines"]])) {
      shiny::updateNumericInput(session, "log_nrow", value = hdr[["lines"]])
    }
    if (!is.na(hdr[["samples"]])) {
      shiny::updateNumericInput(session, "log_ncol", value = hdr[["samples"]])
    }
    if (!is.na(hdr[["bands"]])) {
      shiny::updateNumericInput(session, "log_nlyr", value = hdr[["bands"]])
    }
    if (!is.na(hdr[["tint"]])) {
      shiny::updateNumericInput(
        session,
        "log_et_target_ms",
        value = round(hdr[["tint"]], 3)
      )
    }
    if (!is.na(hdr[["fps"]])) {
      shiny::updateNumericInput(
        session,
        "log_frame_rate_hz",
        value = hdr[["fps"]]
      )
    }
    if (!is.na(hdr[["spectral_binning"]])) {
      shiny::updateNumericInput(
        session,
        "log_spectral_binning",
        value = hdr[["spectral_binning"]]
      )
    }
    if (!is.na(hdr[["spatial_binning"]])) {
      shiny::updateNumericInput(
        session,
        "log_spatial_binning",
        value = hdr[["spatial_binning"]]
      )
    }

    spectral(list(wavelengths = hdr[["wavelengths"]], fwhm = hdr[["fwhm"]]))
  })

  shiny::observeEvent(input$log_white_hdr, {
    fi <- shinyFiles::parseFilePaths(volumes, input$log_white_hdr)
    if (nrow(fi) == 0) {
      return()
    }
    hdr <- parse_hdr(fi$datapath[[1]])
    if (is.null(hdr)) {
      return()
    }
    f_white(basename(fi$datapath[[1]]))
    if (!is.na(hdr[["tint"]])) {
      shiny::updateNumericInput(
        session,
        "log_et_white_ms",
        value = round(hdr[["tint"]], 3)
      )
    }
  })

  shiny::observeEvent(input$log_log_file, {
    fi <- shinyFiles::parseFilePaths(volumes, input$log_log_file)
    if (nrow(fi) == 0) {
      return()
    }
    f_log(basename(fi$datapath[[1]]))
    lg <- parse_log(fi$datapath[[1]])
    if (!is.na(lg[["dropped"]])) {
      shiny::updateNumericInput(
        session,
        "log_dropped_frames",
        value = lg[["dropped"]]
      )
    }
  })

  # Spectral axes summary from the loaded target .hdr.
  output$log_spectral_chip <- shiny::renderText({
    wl <- spectral()$wavelengths
    if (is.null(wl) || length(wl) == 0) {
      return("no .hdr loaded")
    }
    sprintf("%d bands, %.1f\u2013%.1f nm", length(wl), min(wl), max(wl))
  })

  output$log_target_file <- shiny::renderText(f_target())
  output$log_white_file <- shiny::renderText(f_white())
  output$log_log_fname <- shiny::renderText(f_log())

  # ---- Log: save sidecar ----------------------------------------------
  shiny::observeEvent(input$log_save_dir, {
    d <- shinyFiles::parseDirPath(volumes, input$log_save_dir)
    if (length(d) && nzchar(d)) save_dir(d)
  })

  output$log_save_target <- shiny::renderText({
    dir <- save_dir()
    nm <- nz(input$log_name)
    if (is.null(dir) || is.null(nm)) {
      return("\u2014 load a .hdr or pick a folder")
    }
    file.path(dir, paste0(nm, ".yaml"))
  })

  shiny::observeEvent(input$log_save, {
    dir <- save_dir()
    nm <- nz(input$log_name)
    if (is.null(dir) || is.null(nm)) {
      shiny::showNotification(
        "Need a name and a save folder before saving.",
        type = "warning"
      )
      return()
    }
    filename <- file.path(dir, paste0(nm, ".yaml"))

    sp <- spectral()
    args <- list(
      name = nm,
      sensor_type = nz(input$log_sensor_type),
      manufacturer = nz(input$log_manufacturer),
      lens = nz(input$log_lens),
      calibration_pack = nz(input$log_calibration_pack),
      session_id = nz(input$log_session_id),
      operator = nz(input$log_operator),
      campaign_prefix = nz(input$log_campaign_prefix),
      dataset_name = nz(input$log_dataset_name),
      nrow = nz(input$log_nrow),
      ncol = nz(input$log_ncol),
      nlyr = nz(input$log_nlyr),
      xres = nz(input$log_xres),
      yres = nz(input$log_yres),
      spectral_resolution_nm = nz(input$log_spectral_resolution_nm),
      frame_rate_hz = nz(input$log_frame_rate_hz),
      et_target_ms = nz(input$log_et_target_ms),
      et_white_ms = nz(input$log_et_white_ms),
      target_start_mm = nz(input$log_target_start_mm),
      target_stop_mm = nz(input$log_target_stop_mm),
      fov_mm = nz(input$log_fov_mm),
      camera_position_mm = nz(input$log_camera_position_mm),
      stage_position_mm = nz(input$log_stage_position_mm),
      scanning_speed_mm_s = nz(input$log_scanning_speed_mm_s),
      aspect_ratio = nz(input$log_aspect_ratio),
      spectral_binning = nz(input$log_spectral_binning),
      spatial_binning = nz(input$log_spatial_binning),
      dropped_frames = nz(input$log_dropped_frames),
      gcp_count = nz(input$log_gcp_count),
      wavelengths = sp$wavelengths,
      fwhm = sp$fwhm
    )

    res <- tryCatch(
      {
        md <- do.call(HSItools::hsi_create_metadata, args)
        HSItools::hsi_write_metadata(
          md,
          filename = filename,
          overwrite = isTRUE(input$log_overwrite)
        )
        filename
      },
      error = \(e) e
    )

    if (inherits(res, "error")) {
      shiny::showNotification(
        conditionMessage(res),
        type = "error",
        duration = NULL
      )
    } else {
      shiny::showNotification(paste("Saved", res), type = "message")
      purrr::walk(PER_CAPTURE_TEXT, \(id) {
        shiny::updateTextInput(session, id, value = "")
      })
      purrr::walk(PER_CAPTURE_NUMERIC, \(id) {
        shiny::updateNumericInput(session, id, value = NA)
      })
      spectral(NULL)
    }
  })

  # ---- Review: read one sidecar back (read-only) ----------------------
  shiny::observeEvent(input$review_yaml, {
    fi <- shinyFiles::parseFilePaths(volumes, input$review_yaml)
    if (nrow(fi) == 0) {
      return()
    }
    md <- tryCatch(HSItools::hsi_read_metadata(fi$datapath[[1]]), error = \(e) {
      e
    })
    if (inherits(md, "error")) {
      shiny::showNotification(
        conditionMessage(md),
        type = "error",
        duration = NULL
      )
      review_md(NULL)
      return()
    }
    review_md(md)
  })

  output$review_table <- shiny::renderTable(
    {
      md <- review_md()
      if (is.null(md)) {
        return(NULL)
      }
      fmt <- \(v) {
        if (is.null(v) || length(v) == 0) {
          return("\u2014")
        }
        if (length(v) > 1) {
          return(sprintf(
            "%d values, %s\u2013%s",
            length(v),
            format(min(v)),
            format(max(v))
          ))
        }
        as.character(v)
      }
      data.frame(
        Field = names(md),
        Value = purrr::map_chr(md, fmt),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    },
    striped = TRUE,
    spacing = "xs",
    width = "100%"
  )
}

shiny::shinyApp(ui, server)

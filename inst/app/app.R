# hsical — hyperspectral scan calibration & logging companion
# 2.1 (2026-07-14): one scan, one panel. A scan is five numbers — lines, samples,
# start, stop, fov — and everything else (length, xres, yres, aspect ratio, ideal
# FOV) is derived from them. "Test", "confirmation" and "target" were never
# different objects, only different things to look at, so v2.0's split cards and
# duplicated .hdr pickers are gone. hsical owns no schema: the form is an argument
# collector for HSItools::hsi_create_metadata(). All calls ::-qualified, native
# pipe, \(x) lambdas.

# ==========================================================================
# Helpers
# ==========================================================================

`%||%` <- function(x, y) if (is.null(x)) y else x

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

# First file in `dir` matching `pattern`, or NULL. Case-insensitive.
find_one <- function(dir, pattern) {
  hits <- list.files(
    dir,
    pattern = pattern,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(hits) == 0) NULL else hits[[1]]
}

# A Lumo capture folder holds the whole scan: the capture .hdr plus its WHITEREF
# and DARKREF siblings, with the .log beside them or one level up in the scan
# root. One pick gives us all four. Vendor naming is deliberate — instrument
# specificity belongs in hsical, not in HSItools.
discover_capture <- function(hdr_path) {
  capture_dir <- dirname(hdr_path)
  scan_root <- dirname(capture_dir)

  log_path <- find_one(capture_dir, "\\.log$") %||%
    find_one(scan_root, "\\.log$")

  list(
    target = hdr_path,
    white = find_one(capture_dir, "^WHITEREF.*\\.hdr$"),
    dark = find_one(capture_dir, "^DARKREF.*\\.hdr$"),
    log = log_path,
    scan_root = scan_root
  )
}

# Three-tier aspect ratio classification -> list(theme, icon, label)
ratio_tier <- function(ratio) {
  if (is.na(ratio)) {
    return(list(theme = "secondary", icon = "square", label = "\u2014"))
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

# Fields stable across a scanning session: carried between saves, blanked by
# "Clear session". Split by input type so updates use the right fn. Three more
# session-stable fields are handled separately in the clear: sensor_type and
# lens are selectizes, and manufacturer resets to its "Specim" default rather
# than to blank.
SESSION_TEXT <- c(
  "session_id",
  "operator",
  "campaign_prefix",
  "dataset_name",
  "calibration_pack"
)
SESSION_NUMERIC <- c(
  "fov_mm",
  "et_target_ms",
  "et_white_ms",
  "spectral_binning",
  "spatial_binning",
  "camera_position_mm",
  "stage_position_mm"
)

# Per-capture fields: blanked after a successful save (session-stable ones stay).
# xres, yres and aspect_ratio are absent by design — they are derived from the
# scan, never typed, so there is nothing to reset.
PER_CAPTURE_TEXT <- c("name")
PER_CAPTURE_NUMERIC <- c(
  "target_start_mm",
  "target_stop_mm",
  "scanning_speed_mm_s",
  "nrow",
  "ncol",
  "nlyr",
  "frame_rate_hz",
  "spectral_resolution_nm",
  "dropped_frames",
  "gcp_count"
)

# Review-editor field typing. Every numeric sidecar field, reused from the Scan
# form's own lists plus the three derived values (xres/yres/aspect_ratio) that
# are never typed there; anything else scalar is text. wavelengths/fwhm are
# vectors and schema_version is a protocol invariant, so all three are shown
# read-only and carried through a save untouched.
REVIEW_NUMERIC <- c(
  SESSION_NUMERIC,
  PER_CAPTURE_NUMERIC,
  "xres",
  "yres",
  "aspect_ratio"
)
REVIEW_READONLY <- c("schema_version", "wavelengths", "fwhm")

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
    if (!nzchar(v)) {
      return(NULL)
    }
  }
  v
}

# ==========================================================================
# UI — one scan panel, plus Review
# ==========================================================================

ui <- bslib::page_navbar(
  title = "hsical",
  theme = bslib::bs_theme(version = 5, primary = "#2c6e8f"),

  bslib::nav_panel(
    title = "Scan",

    # ---- Card 1: the scan and its geometry -------------------------------
    bslib::card(
      bslib::card_header(
        shiny::div(
          class = "d-flex gap-3 flex-wrap align-items-start",
          shiny::div(
            shinyFiles::shinyFilesButton(
              "scan_hdr",
              "Load scan .hdr",
              "Select the capture .hdr",
              multiple = FALSE,
              icon = bsicons::bs_icon("file-earmark-text")
            )
          ),
          shiny::div(
            class = "small",
            shiny::uiOutput("discovery")
          )
        )
      ),
      bslib::card_body(
        shiny::textInput(
          "name",
          tip("Name", "Capture name; autofills from the .hdr filename."),
          width = "100%"
        ),
        bslib::layout_columns(
          col_widths = c(6, 6),

          # Left: what you set on the rig and what the scan came out as.
          shiny::div(
            bslib::layout_columns(
              col_widths = c(4, 4, 4),
              shiny::numericInput(
                "target_start_mm",
                tip("Start (mm)", "Motor position at scan start."),
                value = NA
              ),
              shiny::numericInput(
                "target_stop_mm",
                tip("Stop (mm)", "Motor position at scan stop."),
                value = NA
              ),
              shiny::numericInput(
                "fov_mm",
                tip("FOV (mm)", "Across-track field of view set in Lumo."),
                value = NA,
                min = 0
              ),
              shiny::numericInput(
                "nrow",
                tip("Lines (rows)", "Along-track. From the .hdr `lines`."),
                value = NA,
                min = 1
              ),
              shiny::numericInput(
                "ncol",
                tip("Samples (cols)", "Across-track. From the .hdr `samples`."),
                value = NA,
                min = 1
              ),
              shiny::numericInput(
                "nlyr",
                tip("Bands", "From the .hdr `bands`."),
                value = NA,
                min = 1
              )
            )
          ),

          # Right: everything the five numbers above imply. Nothing is typed
          # here — that is the point.
          shiny::div(
            bslib::layout_columns(
              col_widths = c(7, 5),
              shiny::div(
                shiny::div(
                  shiny::strong("Scan length: "),
                  shiny::textOutput("out_length", inline = TRUE)
                ),
                shiny::div(
                  shiny::strong("Est. scan time: "),
                  shiny::textOutput("out_scan_time", inline = TRUE)
                ),
                shiny::div(
                  shiny::strong("yres (along-track): "),
                  shiny::textOutput("out_yres", inline = TRUE)
                ),
                shiny::div(
                  shiny::strong("xres (across-track): "),
                  shiny::textOutput("out_xres", inline = TRUE)
                ),
                shiny::div(
                  class = "mt-2",
                  shiny::strong("Ideal FOV for square pixels: "),
                  shiny::textOutput("out_ideal_fov", inline = TRUE)
                )
              ),
              shiny::uiOutput("out_ratio_box")
            )
          )
        )
      )
    ),

    # ---- Card 2: the rest of the sidecar ---------------------------------
    bslib::card(
      bslib::accordion(
        id = "meta_accordion",
        open = FALSE,
        multiple = TRUE,

        bslib::accordion_panel(
          "Session",
          icon = bsicons::bs_icon("collection"),
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 6, lg = 3),
            shiny::textInput(
              "session_id",
              tip(
                "Session ID",
                "e.g. LAZ-26-S1. Groups scans sharing a white reference."
              )
            ),
            shiny::textInput(
              "operator",
              tip("Operator", "Full first + last name.")
            ),
            shiny::textInput(
              "campaign_prefix",
              tip("Campaign prefix", "Lumo Setup value, e.g. LAZ-26.")
            ),
            shiny::textInput(
              "dataset_name",
              tip("Dataset name", "Lumo Capture value, CC-SS.")
            )
          )
        ),

        bslib::accordion_panel(
          "Instrument",
          icon = bsicons::bs_icon("camera"),
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 6, lg = 3),
            shiny::selectizeInput(
              "sensor_type",
              tip("Sensor type", "VNIR / SWIR, or type a custom value."),
              choices = c("VNIR", "SWIR"),
              selected = character(0),
              options = list(
                create = TRUE,
                placeholder = "VNIR / SWIR / custom"
              )
            ),
            shiny::textInput(
              "manufacturer",
              tip("Manufacturer", "Defaults to Specim, the rig this app targets."),
              value = "Specim"
            ),
            shiny::selectizeInput(
              "lens",
              tip("Lens", "Specim objective. Pick one, or type a custom value."),
              # SWIR objectives are OLES30 / OLESmacro. The two VNIR entries are
              # placeholders — replace with the real Specim VNIR lens names.
              choices = c(
                "OLES30",
                "OLESmacro",
                "VNIR lens 1 (TODO)",
                "VNIR lens 2 (TODO)"
              ),
              selected = character(0),
              options = list(create = TRUE, placeholder = "Select or type a lens")
            ),
            shiny::textInput(
              "calibration_pack",
              tip("Calibration pack", "Autofills from the .hdr.")
            )
          )
        ),

        bslib::accordion_panel(
          "Acquisition",
          icon = bsicons::bs_icon("grid-3x3"),
          bslib::layout_columns(
            col_widths = bslib::breakpoints(sm = 6, lg = 3),
            shiny::numericInput(
              "et_target_ms",
              tip("ET target (ms)", "From the capture .hdr `tint`."),
              value = NA
            ),
            shiny::numericInput(
              "et_white_ms",
              tip("ET white (ms)", "From the WHITEREF .hdr `tint`."),
              value = NA
            ),
            shiny::numericInput(
              "frame_rate_hz",
              tip("Frame rate (Hz)", "From the .hdr `fps`."),
              value = NA
            ),
            shiny::numericInput(
              "scanning_speed_mm_s",
              tip("Scanning speed (mm/s)", "As set in Lumo."),
              value = NA
            ),
            shiny::numericInput(
              "spectral_binning",
              tip("Spectral binning", "From the .hdr binning."),
              value = NA
            ),
            shiny::numericInput(
              "spatial_binning",
              tip("Spatial binning", "From the .hdr binning."),
              value = NA
            ),
            shiny::numericInput(
              "spectral_resolution_nm",
              tip("Spectral resolution (nm)", "Calibrated value or blank."),
              value = NA
            ),
            shiny::numericInput(
              "camera_position_mm",
              tip("Camera position (mm)", "Enables future focus-signature QC."),
              value = NA
            ),
            shiny::numericInput(
              "stage_position_mm",
              tip("Stage position (mm)", "Enables future focus-signature QC."),
              value = NA
            )
          ),
          shiny::div(
            shiny::strong("Spectral axes: "),
            shiny::textOutput("spectral_chip", inline = TRUE)
          )
        ),

        bslib::accordion_panel(
          "QC",
          icon = bsicons::bs_icon("clipboard-check"),
          bslib::layout_columns(
            col_widths = c(6, 6),
            shiny::numericInput(
              "dropped_frames",
              tip("Dropped frames", "From the .log. Zero is valid."),
              value = NA,
              min = 0
            ),
            shiny::numericInput(
              "gcp_count",
              tip("GCP count", "Number of ground-control pins. Zero is valid."),
              value = NA,
              min = 0
            )
          )
        )
      ),
      bslib::card_footer(
        shiny::div(
          class = "d-flex align-items-center gap-2 flex-wrap",
          shinyFiles::shinyDirButton(
            "save_dir_btn",
            "Folder\u2026",
            "Select save folder",
            icon = bsicons::bs_icon("folder")
          ),
          shiny::span(
            shiny::strong("Save to: "),
            shiny::textOutput("save_target", inline = TRUE)
          ),
          # checkboxInput's root is its .shiny-input-container, which carries a
          # bottom margin; mb-0 on that root drops it so the box sits on the
          # row's centre line with the buttons instead of nudging the row taller.
          htmltools::tagAppendAttributes(
            shiny::checkboxInput("overwrite", "Overwrite", value = FALSE),
            class = "mb-0"
          ),
          # ms-auto pushes the actions to the far right; the left cluster keeps
          # its natural width. flex-wrap drops them to a second line only when
          # the card is too narrow to hold everything.
          shiny::div(
            class = "d-flex gap-2 ms-auto",
            shiny::actionButton(
              "save",
              "Save sidecar",
              icon = bsicons::bs_icon("save"),
              class = "btn-primary"
            ),
            shiny::actionButton(
              "clear_session",
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
        shiny::div(
          class = "d-flex gap-3 flex-wrap align-items-center",
          shinyFiles::shinyFilesButton(
            "review_yaml",
            "Load sidecar",
            "Select a .yaml sidecar",
            multiple = FALSE,
            icon = bsicons::bs_icon("file-earmark-text")
          ),
          shiny::span(
            class = "small",
            shiny::strong("Editing: "),
            shiny::textOutput("review_path_label", inline = TRUE)
          )
        )
      ),
      bslib::card_body(
        shiny::uiOutput("review_editor")
      ),
      bslib::card_footer(
        shiny::actionButton(
          "review_save",
          "Save changes",
          icon = bsicons::bs_icon("save"),
          class = "btn-primary"
        )
      )
    )
  )
)

# ==========================================================================
# Server
# ==========================================================================

server <- function(input, output, session) {
  volumes <- shinyFiles::getVolumes()()

  # Autofilled spectral axes (never form fields): set on capture .hdr load,
  # feeds the chip and the save call.
  spectral <- shiny::reactiveVal(NULL)

  # What the last capture .hdr pick turned up in its folder.
  found <- shiny::reactiveVal(NULL)

  # Default sidecar folder: the scan root (parent of the capture/ folder that
  # holds the .hdr), overridable via the Folder… button.
  save_dir <- shiny::reactiveVal(NULL)

  # Sidecar loaded into the Review panel: the hsi_metadata object and the path
  # it came from. The object is edited in place and written back to the path.
  review_md <- shiny::reactiveVal(NULL)
  review_path <- shiny::reactiveVal(NULL)

  shinyFiles::shinyFileChoose(
    input,
    "scan_hdr",
    roots = volumes,
    filetypes = "hdr"
  )
  shinyFiles::shinyDirChoose(input, "save_dir_btn", roots = volumes)
  shinyFiles::shinyFileChoose(
    input,
    "review_yaml",
    roots = volumes,
    filetypes = c("yaml", "yml")
  )

  # ---- The scan --------------------------------------------------------
  # Five numbers in, four out. Each derived value is computed independently, so
  # a half-filled form shows what it can instead of blanking the whole readout:
  # you get the ideal FOV from a test scan before any FOV has been set, and the
  # aspect ratio the moment one has.
  geom <- shiny::reactive({
    start <- nz(input$target_start_mm)
    stop <- nz(input$target_stop_mm)
    fov <- nz(input$fov_mm)
    lines <- nz(input$nrow)
    samples <- nz(input$ncol)
    speed <- nz(input$scanning_speed_mm_s)

    length_mm <- if (!is.null(start) && !is.null(stop) && stop > start) {
      stop - start
    } else {
      NA_real_
    }

    # Along-track traverse time: how long the stage takes to cover the scan at
    # the set speed. A pre-scan sanity check, and a post-hoc read of duration.
    scan_time_s <- if (!is.na(length_mm) && !is.null(speed) && speed > 0) {
      length_mm / speed
    } else {
      NA_real_
    }

    # Along-track: fixed by the motors and the frame rate. FOV cannot change it.
    yres <- if (!is.na(length_mm) && !is.null(lines) && lines > 0) {
      length_mm * 1000 / lines
    } else {
      NA_real_
    }

    # Across-track: the optics. This is the only knob that squares the pixels.
    xres <- if (!is.null(fov) && !is.null(samples) && fov > 0 && samples > 0) {
      fov * 1000 / samples
    } else {
      NA_real_
    }

    list(
      length_mm = length_mm,
      yres = yres,
      xres = xres,
      aspect_ratio = yres / xres,
      ideal_fov_mm = if (!is.na(yres) && !is.null(samples)) {
        yres * samples / 1000
      } else {
        NA_real_
      },
      scan_time_s = scan_time_s
    )
  })

  fmt_num <- function(v, digits, unit) {
    if (is.na(v)) "\u2014" else paste(round(v, digits), unit)
  }

  # Duration as "45 s" or "3 min 07 s".
  fmt_time <- function(s) {
    if (is.na(s)) {
      return("\u2014")
    }
    s <- round(s)
    if (s < 60) {
      return(paste0(s, " s"))
    }
    sprintf("%d min %02d s", s %/% 60, s %% 60)
  }

  output$out_length <- shiny::renderText(
    fmt_num(geom()$length_mm, 2, "mm")
  )
  output$out_yres <- shiny::renderText(
    fmt_num(geom()$yres, 2, "\u00b5m/px")
  )
  output$out_xres <- shiny::renderText(
    fmt_num(geom()$xres, 2, "\u00b5m/px")
  )
  output$out_ideal_fov <- shiny::renderText(
    fmt_num(geom()$ideal_fov_mm, 3, "mm")
  )
  output$out_scan_time <- shiny::renderText(
    fmt_time(geom()$scan_time_s)
  )

  output$out_ratio_box <- shiny::renderUI({
    ratio <- geom()$aspect_ratio
    tier <- ratio_tier(ratio)
    bslib::value_box(
      title = "Aspect ratio",
      value = if (is.na(ratio)) "\u2014" else round(ratio, 3),
      shiny::p(tier$label),
      showcase = bsicons::bs_icon(tier$icon),
      theme = tier$theme
    )
  })

  # ---- Load a scan: one pick, whole capture folder ----------------------
  shiny::observeEvent(input$scan_hdr, {
    fi <- shinyFiles::parseFilePaths(volumes, input$scan_hdr)
    if (nrow(fi) == 0) {
      return()
    }
    path <- fi$datapath[[1]]
    hdr <- parse_hdr(path)
    if (is.null(hdr)) {
      return()
    }

    cap <- discover_capture(path)
    found(cap)
    save_dir(cap$scan_root)

    shiny::updateTextInput(
      session,
      "name",
      value = tools::file_path_sans_ext(basename(path))
    )
    if (!is.na(hdr[["camera"]])) {
      shiny::updateSelectizeInput(
        session,
        "sensor_type",
        selected = hdr[["camera"]]
      )
    }
    if (!is.na(hdr[["calibration_pack"]])) {
      shiny::updateTextInput(
        session,
        "calibration_pack",
        value = hdr[["calibration_pack"]]
      )
    }
    if (!is.na(hdr[["lines"]])) {
      shiny::updateNumericInput(session, "nrow", value = hdr[["lines"]])
    }
    if (!is.na(hdr[["samples"]])) {
      shiny::updateNumericInput(session, "ncol", value = hdr[["samples"]])
    }
    if (!is.na(hdr[["bands"]])) {
      shiny::updateNumericInput(session, "nlyr", value = hdr[["bands"]])
    }
    if (!is.na(hdr[["tint"]])) {
      shiny::updateNumericInput(
        session,
        "et_target_ms",
        value = round(hdr[["tint"]], 3)
      )
    }
    if (!is.na(hdr[["fps"]])) {
      shiny::updateNumericInput(session, "frame_rate_hz", value = hdr[["fps"]])
    }
    if (!is.na(hdr[["spectral_binning"]])) {
      shiny::updateNumericInput(
        session,
        "spectral_binning",
        value = hdr[["spectral_binning"]]
      )
    }
    if (!is.na(hdr[["spatial_binning"]])) {
      shiny::updateNumericInput(
        session,
        "spatial_binning",
        value = hdr[["spatial_binning"]]
      )
    }

    spectral(list(wavelengths = hdr[["wavelengths"]], fwhm = hdr[["fwhm"]]))

    # White reference: only its integration time matters to the sidecar.
    if (!is.null(cap$white)) {
      white <- parse_hdr(cap$white)
      if (!is.null(white) && !is.na(white[["tint"]])) {
        shiny::updateNumericInput(
          session,
          "et_white_ms",
          value = round(white[["tint"]], 3)
        )
      }
    }

    if (!is.null(cap$log)) {
      lg <- parse_log(cap$log)
      if (!is.na(lg[["dropped"]])) {
        shiny::updateNumericInput(
          session,
          "dropped_frames",
          value = lg[["dropped"]]
        )
      }
    }
  })

  # What the folder gave us. DARKREF is shown but never read: a matched dark at
  # the specimen's integration time is a protocol invariant, so its absence is
  # the thing worth seeing.
  output$discovery <- shiny::renderUI({
    cap <- found()
    if (is.null(cap)) {
      return(shiny::span(class = "text-muted", "No scan loaded."))
    }
    row <- function(label, path) {
      if (is.null(path)) {
        shiny::div(
          class = "text-danger",
          bsicons::bs_icon("x-lg"),
          " ",
          label,
          " not found"
        )
      } else {
        shiny::div(
          class = "text-success",
          bsicons::bs_icon("check-lg"),
          " ",
          shiny::span(class = "text-body", basename(path))
        )
      }
    }
    shiny::tagList(
      row("capture", cap$target),
      row("WHITEREF", cap$white),
      row("DARKREF", cap$dark),
      row(".log", cap$log)
    )
  })

  output$spectral_chip <- shiny::renderText({
    wl <- spectral()$wavelengths
    if (is.null(wl) || length(wl) == 0) {
      return("no .hdr loaded")
    }
    sprintf("%d bands, %.1f\u2013%.1f nm", length(wl), min(wl), max(wl))
  })

  # ---- Save ------------------------------------------------------------
  shiny::observeEvent(input$save_dir_btn, {
    d <- shinyFiles::parseDirPath(volumes, input$save_dir_btn)
    if (length(d) && nzchar(d)) {
      save_dir(d)
    }
  })

  output$save_target <- shiny::renderText({
    dir <- save_dir()
    nm <- nz(input$name)
    if (is.null(dir) || is.null(nm)) {
      return("\u2014 load a .hdr or pick a folder")
    }
    file.path(dir, paste0(nm, ".yaml"))
  })

  shiny::observeEvent(input$save, {
    dir <- save_dir()
    nm <- nz(input$name)
    if (is.null(dir) || is.null(nm)) {
      shiny::showNotification(
        "Need a name and a save folder before saving.",
        type = "warning"
      )
      return()
    }
    filename <- file.path(dir, paste0(nm, ".yaml"))

    sp <- spectral()
    g <- geom()
    args <- list(
      name = nm,
      sensor_type = nz(input$sensor_type),
      manufacturer = nz(input$manufacturer),
      lens = nz(input$lens),
      calibration_pack = nz(input$calibration_pack),
      session_id = nz(input$session_id),
      operator = nz(input$operator),
      campaign_prefix = nz(input$campaign_prefix),
      dataset_name = nz(input$dataset_name),
      nrow = nz(input$nrow),
      ncol = nz(input$ncol),
      nlyr = nz(input$nlyr),
      # Derived from this scan's own motor positions and FOV — measured, not
      # nominal, which is exactly what the sidecar asks these fields to be.
      xres = nz(round(g$xres, 2)),
      yres = nz(round(g$yres, 2)),
      aspect_ratio = nz(round(g$aspect_ratio, 4)),
      spectral_resolution_nm = nz(input$spectral_resolution_nm),
      frame_rate_hz = nz(input$frame_rate_hz),
      et_target_ms = nz(input$et_target_ms),
      et_white_ms = nz(input$et_white_ms),
      target_start_mm = nz(input$target_start_mm),
      target_stop_mm = nz(input$target_stop_mm),
      fov_mm = nz(input$fov_mm),
      camera_position_mm = nz(input$camera_position_mm),
      stage_position_mm = nz(input$stage_position_mm),
      scanning_speed_mm_s = nz(input$scanning_speed_mm_s),
      spectral_binning = nz(input$spectral_binning),
      spatial_binning = nz(input$spatial_binning),
      dropped_frames = nz(input$dropped_frames),
      gcp_count = nz(input$gcp_count),
      wavelengths = sp$wavelengths,
      fwhm = sp$fwhm
    )

    res <- tryCatch(
      {
        md <- do.call(HSItools::hsi_create_metadata, args)
        HSItools::hsi_write_metadata(
          md,
          filename = filename,
          overwrite = isTRUE(input$overwrite)
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
      found(NULL)
    }
  })

  shiny::observeEvent(input$clear_session, {
    purrr::walk(SESSION_TEXT, \(id) {
      shiny::updateTextInput(session, id, value = "")
    })
    purrr::walk(SESSION_NUMERIC, \(id) {
      shiny::updateNumericInput(session, id, value = NA)
    })
    shiny::updateSelectizeInput(session, "sensor_type", selected = character(0))
    shiny::updateSelectizeInput(session, "lens", selected = character(0))
    shiny::updateTextInput(session, "manufacturer", value = "Specim")
  })

  # ---- Review: read one sidecar back, edit it, write it in place --------
  shiny::observeEvent(input$review_yaml, {
    fi <- shinyFiles::parseFilePaths(volumes, input$review_yaml)
    if (nrow(fi) == 0) {
      return()
    }
    path <- fi$datapath[[1]]
    md <- tryCatch(HSItools::hsi_read_metadata(path), error = \(e) e)
    if (inherits(md, "error")) {
      shiny::showNotification(
        conditionMessage(md),
        type = "error",
        duration = NULL
      )
      review_md(NULL)
      review_path(NULL)
      return()
    }
    review_md(md)
    review_path(path)
  })

  output$review_path_label <- shiny::renderText({
    review_path() %||% "no sidecar loaded"
  })

  # One editable input per scalar field, typed from REVIEW_NUMERIC; vectors and
  # schema_version are read-only summaries. Fields absent from the sidecar still
  # render (as NA/blank), so a value forgotten at save time can be added here.
  output$review_editor <- shiny::renderUI({
    md <- review_md()
    if (is.null(md)) {
      return(shiny::span(class = "text-muted", "No sidecar loaded."))
    }
    field_ui <- function(f) {
      v <- md[[f]]
      id <- paste0("rev_", f)
      if (f %in% REVIEW_READONLY) {
        summary <- if (is.null(v) || length(v) == 0) {
          "\u2014"
        } else if (length(v) > 1) {
          sprintf("%d values, %s\u2013%s", length(v), format(min(v)), format(max(v)))
        } else {
          as.character(v)
        }
        return(shiny::div(
          class = "text-muted small",
          shiny::strong(f),
          ": ",
          summary
        ))
      }
      if (f %in% REVIEW_NUMERIC) {
        shiny::numericInput(id, f, value = if (is.null(v)) NA else v)
      } else {
        shiny::textInput(id, f, value = if (is.null(v)) "" else as.character(v))
      }
    }
    do.call(
      bslib::layout_column_wrap,
      c(list(width = 1 / 4), purrr::map(names(md), field_ui))
    )
  })

  # Save: fold each edited scalar back into the loaded object (blank -> absent),
  # leaving schema_version and the spectral vectors as read. HSItools validates
  # on write, so an out-of-range edit aborts with its own message.
  shiny::observeEvent(input$review_save, {
    md <- review_md()
    path <- review_path()
    if (is.null(md) || is.null(path)) {
      shiny::showNotification(
        "Load a sidecar before saving.",
        type = "warning"
      )
      return()
    }
    for (f in names(md)) {
      if (f %in% REVIEW_READONLY) {
        next
      }
      md[f] <- list(nz(input[[paste0("rev_", f)]]))
    }
    res <- tryCatch(
      HSItools::hsi_write_metadata(md, filename = path, overwrite = TRUE),
      error = \(e) e
    )
    if (inherits(res, "error")) {
      shiny::showNotification(
        conditionMessage(res),
        type = "error",
        duration = NULL
      )
    } else {
      shiny::showNotification(paste("Saved", path), type = "message")
      review_md(md)
    }
  })
}

shiny::shinyApp(ui, server)

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

  # Calibration pack: file name only. The directory is a fact about one rig's
  # disk — the two cameras' packs do not even share a folder, one living under
  # Documents and the other under Program Files — so the path would put a
  # machine's layout into the scan record. The file name is what identifies the
  # calibration, and it is what the sidecar stores.
  cal_raw <- xval("(?<=calibration pack = )[^\n]+")
  cal <- if (is.na(cal_raw)) NA_character_ else basename(trimws(cal_raw))

  # The pack file name carries the objective, but not at a fixed position: the
  # VNIR packs run it into the word (560025_20211124_OLE18.5calpack.scp) while
  # the SWIR ones set it off with underscores and precede "calpack" with one
  # (472194_OLES30_20250422_calpack_BPR.scp). Looking for the known names is
  # therefore sounder than looking at a position; longest match wins so a short
  # name nested in a longer one cannot win. The lens is unrecorded anywhere else
  # in the header, and leaving the operator to remember it is how a VNIR scan
  # ends up logged with a SWIR objective.
  lens <- if (is.na(cal)) {
    NA_character_
  } else {
    hits <- LENS_KNOWN[
      purrr::map_lgl(LENS_KNOWN, \(i) grepl(i, cal, fixed = TRUE))
    ]
    if (length(hits) == 0) NA_character_ else hits[[which.max(nchar(hits))]]
  }

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
    lens = lens,
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

  # The log is matched by the capture's own name, beside it or one level up. A
  # bare ".log$" search returns the alphabetically first hit, and a Lumo capture
  # folder holds three logs — the DARKREF one sorts ahead of the scan, so the
  # dropped-frame count logged would be the dark reference's. Finding nothing is
  # the better failure: the field stays empty instead of quietly wrong.
  log_name <- paste0(tools::file_path_sans_ext(basename(hdr_path)), ".log")

  log_path <- purrr::detect(
    file.path(c(capture_dir, scan_root), log_name),
    file.exists
  )

  list(
    target = hdr_path,
    white = find_one(capture_dir, "^WHITEREF.*\\.hdr$"),
    dark = find_one(capture_dir, "^DARKREF.*\\.hdr$"),
    log = log_path,
    scan_root = scan_root
  )
}

# terra refuses an ENVI .hdr path outright ("the data file should be selected
# instead"), so every raster read goes through the binary sibling. Lumo writes
# .raw; the fallback covers a vendor writing the same stem under another
# extension. NULL when nothing sits beside the header.
envi_data <- function(hdr_path) {
  candidate <- sub("\\.hdr$", ".raw", hdr_path, ignore.case = TRUE)
  if (file.exists(candidate)) {
    return(candidate)
  }

  stem <- tools::file_path_sans_ext(basename(hdr_path))
  siblings <- list.files(dirname(hdr_path), full.names = TRUE)
  hits <- siblings[
    tools::file_path_sans_ext(basename(siblings)) == stem &
      !grepl("\\.(hdr|log)$", siblings, ignore.case = TRUE)
  ]

  if (length(hits) == 0) NULL else hits[[1]]
}

# The composites HSItools::hsi_calc_preview() offers, resolved here against the
# header wavelengths: the screening preview is raw DN with no references, so
# hsi_calc_preview() itself — which calibrates to reflectance — does not apply.
PREVIEW_COMPOSITES <- list(RGB = c(650, 550, 450), SWIR = c(1650, 1100, 2200))

# Screening every Nth band costs proportionally less time and undercounts only
# slightly: a clipped pixel clips across dozens of neighbouring bands, so any of
# these steps still lands inside almost every run. Measured on a sparsely
# saturated GKUT VNIR region (0.4794% of pixels), a step of 8 recovered 97.5% of
# the count in an eighth of the time, and a step of 32 still recovered 96.2%.
# The result is always a lower bound — a subset can miss a saturated pixel but
# never invent one.
BAND_STEPS <- c(
  "all" = "1",
  "2nd" = "2",
  "4th" = "4",
  "8th" = "8",
  "16th" = "16"
)

# The scan's five numbers and the band count are all short — the widest real
# value is a five-digit line count — so the fields are sized to the numbers they
# hold rather than to the column they sit in. Wide enough that the longest label
# ("Samples (cols)") still fits on one line.
NUMERIC_FIELD_WIDTH <- "140px"

# Names, ids and prefixes. Wide enough for an operator's name, narrow enough
# that four sit on one row of a half-width column.
TEXT_FIELD_WIDTH <- "200px"

# The acquisition numerics hold values as short as `1`, but their labels do not:
# "Spectral resolution (nm)" needs 201px against a 140px field, and seven of the
# eleven wrapped onto a second line. A wrapped label costs 24px of height on
# every field in the row, which is more than the narrow box ever saved — so
# these are sized to the label rather than to the number.
LABELLED_FIELD_WIDTH <- "205px"

# The one non-white surface in the app: the page behind the cards, the closed
# accordion bars, and the input fields. Named because it is used in several
# places that must not drift apart.
SURFACE_GROUND <- "#edeff1"

# Specim objectives: OL50 and OLE18.5 on the VNIR camera, OLES30 and OLESmacro
# on the SWIR. The leading blank keeps the field empty until something fills it.
LENS_BY_SENSOR <- list(
  VNIR = c("OL50", "OLE18.5"),
  SWIR = c("OLES30", "OLESmacro")
)

# Every objective the app knows, for matching against a calibration pack name.
LENS_KNOWN <- unlist(LENS_BY_SENSOR, use.names = FALSE)

# The escape hatch. VNIR and SWIR are spectral ranges, so the sensor list is
# genuinely closed whatever camera the lab buys — but objective names are vendor
# hardware, and the lab has non-Specim data. Offering only the four Specim
# optics would force a wrong answer on a HySpex capture, so "Other" reveals a
# free-text field. Deliberate and visible, rather than a box that silently
# accepts anything.
LENS_OTHER <- "Other…"

# Both cameras are 16-bit. The screen limit is a percentage of this ceiling
# rather than the ceiling itself: detector response compresses before it clips,
# so an exact-ceiling test understates the damaged region.
SATURATION_CEILING <- 65535

# Brush rectangle -> a terra extent on the capture grid, or NULL for full frame.
# The preview is drawn with the long axis horizontal, so display x is the line
# index and display y the sample index, sample 1 at the top. terra's grid for an
# extent-less raster is the other way round — x is the sample, y is the line
# counted up from the last one — hence the swap and the flip.
brush_window <- function(brush, n_line, n_sample) {
  if (is.null(brush)) {
    return(NULL)
  }

  clamp <- \(v, hi) min(max(v, 0), hi)

  x_min <- clamp(brush[["ymin"]], n_sample)
  x_max <- clamp(brush[["ymax"]], n_sample)
  y_min <- n_line - clamp(brush[["xmax"]], n_line)
  y_max <- n_line - clamp(brush[["xmin"]], n_line)

  # A click rather than a drag leaves no area to window.
  if (x_max - x_min < 1 || y_max - y_min < 1) {
    return(NULL)
  }

  terra::ext(x_min, x_max, y_min, y_max)
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
  "dataset_name"
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

# A labelled group of fields. Grouping and collapsing are separate things: the
# labelled boundary is what an operator navigates by, while the collapsing cost
# four 52px headers and a click before anything could be typed — and it existed
# only to contain a height that came from fields three times wider than their
# own content.
field_group <- function(title, icon, ...) {
  shiny::div(
    class = "mb-4",
    shiny::div(
      class = "d-flex align-items-center gap-2 mb-2 pb-1 border-bottom",
      bsicons::bs_icon(icon, class = "text-primary"),
      shiny::strong(title)
    ),
    ...
  )
}

# The objectives a sensor offers, plus the escape hatch. Before a sensor is
# picked there is nothing to offer but the hatch.
lens_choices <- function(sensor) {
  known <- if (length(sensor) == 1 && sensor %in% names(LENS_BY_SENSOR)) {
    LENS_BY_SENSOR[[sensor]]
  } else {
    character(0)
  }
  c(known, LENS_OTHER)
}

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
  # Displayed as HSIcal to sit with HSItools; the package itself stays `hsical`.
  # The HSItools hex, left of the name, sized to the brand text. Served from
  # inst/app/www, which Shiny publishes at the app root.
  title = shiny::tagList(
    shiny::img(
      src = "hsitools-logo.png",
      alt = "",
      height = "28px",
      class = "me-2 align-text-bottom"
    ),
    "HSIcal"
  ),
  # Square corners and no drop shadows: the cards are here to group fields on an
  # instrument panel, not to float above a dashboard. The shadow is dropped with
  # bslib's own `bslib-card-box-shadow-none` class on each card rather than by
  # hand, because that class also restores the card border — bslib leaves the
  # border transparent and lets the shadow do the separating, so removing only
  # the shadow would leave the cards with no edge at all.
  theme = bslib::bs_theme(
    version = 5,
    primary = "#2c6e8f",
    # Square everything: `border-radius` covers the form controls, `card-border-
    # radius` is needed on top of it because bslib rounds cards to 8px of its own
    # accord. The card border replaces the shadow as the thing that separates one
    # panel from the next, so it is a solid grey rather than bslib's 10%-alpha
    # tint, which all but disappears on white.
    "border-radius" = "0",
    "card-border-radius" = "0",
    "card-border-color" = "#ced4da",
    # White cards on a white page left nothing to separate one panel from the
    # next once the shadows went. A near-white ground does that structural work
    # without spending any colour on it. Two surfaces only, used consistently:
    # the ground for the page, the accordion's closed header bars and the input
    # fields; white for anything holding content.
    #
    # The accordion needs both variables. Bootstrap derives the header bar from
    # `accordion-bg`, so leaving it at the ground colour tinted the open body
    # too — and grey fields on a grey body have no contrast at all, which is the
    # one place this scheme fell over.
    "body-bg" = SURFACE_GROUND,
    "card-bg" = "#ffffff",
    "accordion-bg" = "#ffffff",
    "accordion-button-bg" = SURFACE_GROUND
  ) |>
    # Notifications land bottom-right by default, which on a rig screen is where
    # nobody is looking. Centred and enlarged, because everything this app says
    # is a correction the operator has to act on before the next scan.
    bslib::bs_add_rules(
      # The tabs are the app's top-level navigation and rendered at the same
      # 14px as the field labels beneath them. Set here rather than through
      # `nav-link-font-size` / `nav-link-font-weight`, which bslib overrides
      # downstream of the theme — the variables took, the styling did not.
      ".navbar .navbar-nav .nav-link {
         font-size: 1.05rem;
         font-weight: 500;
       }
       .navbar .navbar-nav .nav-link.active {
         font-weight: 700;
       }
       /* Doubled id on purpose: bslib pins the panel with a rule of its own at
          `#shiny-notification-panel#shiny-notification-panel`, and a single id
          loses to it. Left unmatched, its `bottom` survived alongside our
          `top`, stretching the panel between the two — a 59px toast adrift in a
          438px box, sitting well above centre. */
       /* bslib gives every card `overflow: auto`, which makes the card itself
          the scroll container — and a footer cannot stick to a box that never
          scrolls. Letting this one overflow visibly hands the job back to the
          page, which is what the action bar sticks to. */
       .scan-form-card {
         overflow: visible;
       }
       .sticky-action-bar {
         position: sticky;
         bottom: 0;
         z-index: 5;
         background-color: #ffffff;
         border-top: 1px solid #ced4da;
       }
       #shiny-notification-panel#shiny-notification-panel {
         top: 50%; left: 50%; right: auto; bottom: auto;
         transform: translate(-50%, -50%);
         width: auto; max-width: 34rem;
         height: auto;
       }
       #shiny-notification-panel .shiny-notification {
         font-size: 1.05rem;
         padding: 1rem 2.5rem 1rem 1.25rem;
         opacity: 1;
       }"
    ),

  bslib::nav_panel(
    title = "Scan",

    # ---- Card 1: the scan and its geometry -------------------------------
    bslib::card(
      class = "bslib-card-box-shadow-none",
      # Sized to its content: as a fill item inside a fill page the card was
      # clipped to the viewport and scrolled internally, so the tab had a scroll
      # region inside a scroll region. Only the screening card fills.
      fill = FALSE,
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
                value = NA,
                width = NUMERIC_FIELD_WIDTH
              ),
              shiny::numericInput(
                "target_stop_mm",
                tip("Stop (mm)", "Motor position at scan stop."),
                value = NA,
                width = NUMERIC_FIELD_WIDTH
              ),
              shiny::numericInput(
                "fov_mm",
                tip("FOV (mm)", "Across-track field of view set in Lumo."),
                value = NA,
                min = 0,
                width = NUMERIC_FIELD_WIDTH
              ),
              shiny::numericInput(
                "nrow",
                tip("Lines (rows)", "Along-track. From the .hdr `lines`."),
                value = NA,
                min = 1,
                width = NUMERIC_FIELD_WIDTH
              ),
              shiny::numericInput(
                "ncol",
                tip("Samples (cols)", "Across-track. From the .hdr `samples`."),
                value = NA,
                min = 1,
                width = NUMERIC_FIELD_WIDTH
              ),
              shiny::numericInput(
                "nlyr",
                tip("Bands", "From the .hdr `bands`."),
                value = NA,
                min = 1,
                width = NUMERIC_FIELD_WIDTH
              )
            )
          ),

          # Right: everything the five numbers above imply. Nothing is typed
          # here — that is the point.
          shiny::div(
            # Derived values carry the one bit of colour on this card: the whole
            # point of the panel is that five numbers are typed and the rest is
            # computed, and in plain text the two were indistinguishable.
            bslib::layout_columns(
              col_widths = c(7, 5),
              shiny::div(
                shiny::div(
                  shiny::strong("Scan length: "),
                  shiny::span(
                    class = "text-primary fw-semibold",
                    shiny::textOutput("out_length", inline = TRUE)
                  )
                ),
                shiny::div(
                  shiny::strong("Est. scan time: "),
                  shiny::span(
                    class = "text-primary fw-semibold",
                    shiny::textOutput("out_scan_time", inline = TRUE)
                  )
                ),
                # Named for what it is rather than for the sidecar keys, which
                # read as jargon on screen — the keys stay in brackets because
                # that is what lands in the file. Two numbers, never averaged: a
                # single figure would describe no pixel in the raster and would
                # hide exactly the anisotropy this panel exists to expose.
                shiny::div(
                  class = "mt-2",
                  shiny::strong("Spatial resolution")
                ),
                shiny::div(
                  class = "ms-3",
                  shiny::strong("along-track (yres): "),
                  shiny::span(
                    class = "text-primary fw-semibold",
                    shiny::textOutput("out_yres", inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "ms-3",
                  shiny::strong("across-track (xres): "),
                  shiny::span(
                    class = "text-primary fw-semibold",
                    shiny::textOutput("out_xres", inline = TRUE)
                  )
                ),
                shiny::div(
                  class = "mt-2",
                  shiny::strong("Ideal FOV for square pixels: "),
                  shiny::span(
                    class = "text-primary fw-semibold",
                    shiny::textOutput("out_ideal_fov", inline = TRUE)
                  )
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
      class = c("bslib-card-box-shadow-none", "scan-form-card"),
      # Sized to its content: as a fill item inside a fill page the card was
      # clipped to the viewport and scrolled internally, so the tab had a scroll
      # region inside a scroll region. Only the screening card fills.
      fill = FALSE,
      bslib::card_body(
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(5, 7)),
          shiny::div(
            field_group(
              "Session",
              "collection",
              shiny::div(
                class = "d-flex flex-wrap gap-3",
                shiny::textInput(
                  "session_id",
                  tip(
                    "Session ID",
                    "Groups scans sharing one white reference."
                  ),
                  width = TEXT_FIELD_WIDTH
                ),
                shiny::textInput(
                  "operator",
                  tip("Operator", "Who ran the scan."),
                  width = TEXT_FIELD_WIDTH
                ),
                shiny::textInput(
                  "campaign_prefix",
                  tip(
                    "Campaign prefix",
                    "The PREFIX in PREFIX_CC-SS_TIMESTAMP."
                  ),
                  width = TEXT_FIELD_WIDTH
                ),
                shiny::textInput(
                  "dataset_name",
                  tip("Dataset name", "The dataset this capture belongs to."),
                  width = TEXT_FIELD_WIDTH
                )
              )
            ),
            field_group(
              "Instrument",
              "camera",
              shiny::div(
                class = "d-flex flex-wrap gap-3 align-items-start",
                shiny::radioButtons(
                  "sensor_type",
                  tip(
                    "Sensor type",
                    "The spectral range, not the camera model. Closed on purpose: a new camera of any make is still one of these two."
                  ),
                  choices = names(LENS_BY_SENSOR),
                  selected = character(0),
                  inline = TRUE
                ),
                shiny::textInput(
                  "manufacturer",
                  tip(
                    "Manufacturer",
                    "Defaults to Specim, the rig this app targets."
                  ),
                  value = "Specim",
                  width = TEXT_FIELD_WIDTH
                ),
                shiny::div(
                  shiny::radioButtons(
                    "lens",
                    tip(
                      "Lens",
                      "The objectives available for the selected sensor. Pick the sensor first — switching it clears an objective that does not belong to the new one."
                    ),
                    choices = LENS_OTHER,
                    selected = character(0),
                    inline = TRUE
                  ),
                  # Client-side, so the field appears the instant "Other" is picked.
                  shiny::conditionalPanel(
                    condition = sprintf("input.lens == '%s'", LENS_OTHER),
                    shiny::textInput(
                      "lens_other",
                      label = NULL,
                      placeholder = "Objective name"
                    )
                  ),
                  # Provenance for the field above rather than a field of its own:
                  # the pack is where the objective was read from, it is never
                  # typed, and only its file name is kept — the directory is a
                  # fact about one rig's disk, not about the capture.
                  shiny::div(
                    class = "small text-muted",
                    shiny::textOutput("cal_pack_note", inline = TRUE)
                  )
                )
              )
            )
          ),
          shiny::div(
            field_group(
              "Acquisition",
              "grid-3x3",
              shiny::div(
                class = "d-flex flex-wrap gap-3",
                shiny::numericInput(
                  "et_target_ms",
                  tip("ET target (ms)", "From the capture .hdr `tint`."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "et_white_ms",
                  tip("ET white (ms)", "From the WHITEREF .hdr `tint`."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "frame_rate_hz",
                  tip("Frame rate (Hz)", "From the .hdr `fps`."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "scanning_speed_mm_s",
                  tip("Scanning speed (mm/s)", "As set in Lumo."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "spectral_binning",
                  tip("Spectral binning", "From the .hdr binning."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "spatial_binning",
                  tip("Spatial binning", "From the .hdr binning."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "spectral_resolution_nm",
                  tip("Spectral resolution (nm)", "Calibrated value or blank."),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "camera_position_mm",
                  tip(
                    "Camera position (mm)",
                    "Enables future focus-signature QC."
                  ),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "stage_position_mm",
                  tip(
                    "Stage position (mm)",
                    "Enables future focus-signature QC."
                  ),
                  value = NA,
                  width = LABELLED_FIELD_WIDTH
                )
              ),
              shiny::div(
                class = "small",
                shiny::strong("Spectral axes: "),
                shiny::textOutput("spectral_chip", inline = TRUE)
              )
            ),
            field_group(
              "QC",
              "clipboard-check",
              shiny::div(
                class = "d-flex flex-wrap gap-3",
                shiny::numericInput(
                  "dropped_frames",
                  tip("Dropped frames", "From the .log. Zero is valid."),
                  value = NA,
                  min = 0,
                  width = LABELLED_FIELD_WIDTH
                ),
                shiny::numericInput(
                  "gcp_count",
                  tip(
                    "GCP count",
                    "Number of ground-control pins. Zero is valid."
                  ),
                  value = NA,
                  min = 0,
                  width = LABELLED_FIELD_WIDTH
                )
              )
            )
          )
        )
      ),
      # Pinned: the form is a little over one screen on a rig monitor, and the
      # commit action should never be the thing you have to go looking for.
      bslib::card_footer(
        class = "sticky-action-bar",
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
      class = "bslib-card-box-shadow-none",
      # Sized to its content: as a fill item inside a fill page the card was
      # clipped to the viewport and scrolled internally, so the tab had a scroll
      # region inside a scroll region. Only the screening card fills.
      fill = FALSE,
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
        # Nothing to save until a sidecar is read, so the button starts dead and
        # the load observer wakes it.
        shiny::actionButton(
          "review_save",
          "Save changes",
          icon = bsicons::bs_icon("save"),
          class = "btn-primary",
          disabled = TRUE
        )
      )
    )
  ),

  bslib::nav_panel(
    title = "Saturation",
    bslib::card(
      class = "bslib-card-box-shadow-none",
      # A viewport-relative height instead of page fill: naming a panel
      # `fillable` makes the whole page a fixed-height flex container, and the
      # form panels then overflowed it with nothing to scroll — on a 900px
      # window Save sat at y=996 and could not be reached at all. Only this
      # card needs the window, so only this card asks for it.
      height = "calc(100vh - 6rem)",
      bslib::card_header(
        shiny::div(
          class = "d-flex gap-3 flex-wrap align-items-center",
          shiny::div(
            class = "small",
            shiny::strong("Screening: "),
            shiny::textOutput("sat_source_label", inline = TRUE)
          )
        )
      ),
      bslib::card_body(
        # The preview is the only element that grows with the window; the note
        # and the verdict keep their natural height.
        class = "d-flex flex-column",
        shiny::div(
          class = "small text-muted mb-2 flex-shrink-0",
          "Raw digital numbers, no references. Drag on the preview to screen a",
          "region of interest and keep the tape and tray out of the count;",
          "release outside the image to screen the full frame. The preview is",
          "decimated and its aspect ratio is deliberately broken — axes are",
          "line and sample indices."
        ),
        shiny::div(
          class = "flex-grow-1",
          style = "min-height: 240px;",
          shiny::plotOutput(
            "sat_preview_plot",
            height = "100%",
            brush = shiny::brushOpts(id = "sat_brush", resetOnNew = TRUE)
          )
        ),
        # The verdict's slot is reserved whether or not there is a verdict in it:
        # letting it appear would resize the plot, and a redrawn plot drops the
        # brush the reader just made.
        shiny::div(
          class = "flex-shrink-0 overflow-auto",
          style = "height: 76px;",
          shiny::uiOutput("sat_report")
        )
      ),
      bslib::card_footer(
        shiny::div(
          class = "d-flex gap-3 flex-wrap align-items-end",
          # Radios rather than dropdowns: the card fills the window, so its
          # controls sit against the bottom edge and a dropdown would open into
          # it. Both lists are short enough to show whole.
          shiny::div(
            shiny::radioButtons(
              "sat_composite",
              tip(
                "Preview composite",
                "Which three raw bands to draw. Display only — it has no bearing on which bands are screened."
              ),
              choices = names(PREVIEW_COMPOSITES),
              selected = "RGB",
              inline = TRUE
            )
          ),
          shiny::div(
            shiny::radioButtons(
              "sat_band_step",
              tip(
                "Screen every Nth band",
                "Reading fewer bands is proportionally faster and can only undercount: clipping runs across dozens of neighbouring bands, so a sampled screen still finds nearly every affected pixel, and every pixel it does find is genuinely saturated."
              ),
              choices = BAND_STEPS,
              selected = "1",
              inline = TRUE
            )
          ),
          shiny::div(
            shiny::numericInput(
              "sat_percent",
              tip(
                "Limit (% of full scale)",
                "A pixel counts as overexposed at or above this share of the 16-bit ceiling. Response compresses before it clips, so the limit sits below 100%."
              ),
              value = 97.5,
              min = 50,
              max = 100,
              step = 0.5,
              width = "180px"
            )
          ),
          shiny::div(
            class = "small text-muted pb-3",
            shiny::textOutput("sat_limit_label", inline = TRUE)
          ),
          shiny::div(
            class = "pb-3",
            shiny::actionButton(
              "sat_run",
              "Screen for saturation",
              icon = bsicons::bs_icon("eyedropper"),
              class = "btn-primary"
            )
          )
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

  # The single place the lens radio is rewritten, so a sensor change and a scan
  # load cannot fight over it. `keep_mismatch` says what to do with an objective
  # that does not belong to `sensor`: an operator switching camera means the old
  # objective is simply wrong and goes, while a calibration pack naming one means
  # something is off with the capture and the value has to be seen, not dropped.
  refresh_lens <- function(sensor, desired, keep_mismatch = FALSE) {
    valid <- lens_choices(sensor)
    desired <- nz(desired)

    if (is.null(desired) || desired %in% valid) {
      shiny::updateRadioButtons(
        session,
        "lens",
        choices = valid,
        selected = desired %||% character(0),
        inline = TRUE
      )
      return(invisible(NULL))
    }

    shiny::updateRadioButtons(
      session,
      "lens",
      choices = valid,
      selected = if (keep_mismatch) LENS_OTHER else character(0),
      inline = TRUE
    )

    if (keep_mismatch) {
      shiny::updateTextInput(session, "lens_other", value = desired)
      shiny::showNotification(
        paste0(
          desired,
          " is not a ",
          sensor,
          " objective. Kept under Other — check the capture."
        ),
        type = "warning",
        duration = NULL
      )
    } else {
      shiny::updateTextInput(session, "lens_other", value = "")
      shiny::showNotification(
        paste0(desired, " is not a ", sensor, " objective. Lens cleared."),
        type = "warning"
      )
    }
  }

  # Switching sensor re-offers that sensor's objectives and drops one that does
  # not belong to it.
  shiny::observeEvent(
    input$sensor_type,
    refresh_lens(input$sensor_type, input$lens, keep_mismatch = FALSE),
    ignoreInit = TRUE
  )

  # Autofilled spectral axes (never form fields): set on capture .hdr load,
  # feeds the chip and the save call.
  spectral <- shiny::reactiveVal(NULL)

  # Read from the header, shown under the lens, never typed — so it is held here
  # rather than in an input.
  cal_pack <- shiny::reactiveVal(NULL)

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
    # Capped to the height of the two rows of fields beside it. Left uncapped the
    # box stretches to the grid row and, being the tallest thing in it, sets the
    # row height itself \u2014 241px against 149px of fields, which is the overhang.
    # `fill = FALSE` does not help: the stretch comes from the grid, not the box.
    bslib::value_box(
      title = "Aspect ratio",
      value = if (is.na(ratio)) "\u2014" else round(ratio, 3),
      shiny::p(tier$label),
      showcase = bsicons::bs_icon(tier$icon),
      showcase_layout = "top right",
      theme = tier$theme,
      max_height = "150px"
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
      shiny::updateRadioButtons(
        session,
        "sensor_type",
        selected = hdr[["camera"]],
        inline = TRUE
      )
    }
    cal_pack(nz(hdr[["calibration_pack"]]))
    # The header's sensor governs which objectives are on offer, so the lens is
    # applied against the camera this capture actually names rather than against
    # whatever was selected before. An objective the pack names that contradicts
    # that sensor is surfaced under Other, never quietly dropped.
    sensor <- if (is.na(hdr[["camera"]])) {
      input$sensor_type
    } else {
      hdr[["camera"]]
    }

    refresh_lens(sensor, hdr[["lens"]], keep_mismatch = TRUE)
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

  output$cal_pack_note <- shiny::renderText({
    cal_pack() %||% "no calibration pack in the header"
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
      # "Other" is a marker for the radio, never a lens name.
      lens = if (identical(input$lens, LENS_OTHER)) {
        nz(input$lens_other)
      } else {
        nz(input$lens)
      },
      calibration_pack = cal_pack(),
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
    shiny::updateRadioButtons(
      session,
      "sensor_type",
      selected = character(0),
      inline = TRUE
    )
    shiny::updateRadioButtons(
      session,
      "lens",
      choices = LENS_OTHER,
      selected = character(0),
      inline = TRUE
    )
    shiny::updateTextInput(session, "lens_other", value = "")
    shiny::updateTextInput(session, "manufacturer", value = "Specim")
    cal_pack(NULL)
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
      shiny::updateActionButton(session, "review_save", disabled = TRUE)
      return()
    }
    review_md(md)
    review_path(path)
    shiny::updateActionButton(session, "review_save", disabled = FALSE)
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
          sprintf(
            "%d values, %s\u2013%s",
            length(v),
            format(min(v)),
            format(max(v))
          )
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

  # ---- Saturation screening ----------------------------------------------
  # A screen of the capture already loaded on the Scan panel, and nothing else:
  # load the core scan to judge the specimen, load the white reference session
  # scan to judge the reference. The WHITEREF sibling is never screened — Lumo
  # captures it at the specimen's integration time, so it clips by design.

  output$sat_source_label <- shiny::renderText({
    cap <- found()
    if (is.null(cap)) "no scan loaded" else basename(cap[["target"]])
  })

  output$sat_limit_label <- shiny::renderText({
    pct <- input$sat_percent
    if (is.null(pct) || is.na(pct)) {
      return("")
    }
    paste0(
      "= ",
      round(SATURATION_CEILING * pct / 100),
      " DN of ",
      SATURATION_CEILING
    )
  })

  # Full-resolution dimensions of the loaded capture. Opening is lazy, so this
  # costs a header read.
  sat_source <- shiny::reactive({
    cap <- found()
    shiny::req(cap)
    path <- envi_data(cap[["target"]])
    shiny::req(path)
    terra::rast(path)
  })

  # Decimated three-band preview. terra pushes the decimation down into the GDAL
  # read, so this stays under a second even on a 50 GB capture; a full-resolution
  # three-band read would seek once per line per band through BIL data.
  sat_preview <- shiny::reactive({
    cap <- found()
    shiny::req(cap)
    wl <- spectral()[["wavelengths"]]
    shiny::req(length(wl) > 0)
    path <- envi_data(cap[["target"]])
    shiny::req(path)

    targets <- PREVIEW_COMPOSITES[[input$sat_composite %||% "RGB"]]
    idx <- purrr::map_int(targets, \(w) which.min(abs(wl - w)))

    terra::rast(path, lyrs = idx) |>
      terra::spatSample(size = 4e5, method = "regular", as.raster = TRUE)
  })

  # Long axis horizontal, aspect deliberately broken: a 24339 x 2184 strip drawn
  # true to scale is either an unusable ribbon or an endless scroll. Plot units
  # are full-resolution line and sample indices, so brush coordinates arrive in
  # capture coordinates and need no rescaling from the decimated preview.
  output$sat_preview_plot <- shiny::renderPlot({
    preview <- sat_preview()
    src <- sat_source()
    n_line <- terra::nrow(src)
    n_sample <- terra::ncol(src)

    values <- terra::stretch(preview, minq = 0.02, maxq = 0.98) |>
      terra::values()
    values[is.na(values)] <- 0

    img <- grDevices::rgb(
      values[, 1],
      values[, 2],
      values[, 3],
      maxColorValue = 255
    ) |>
      matrix(
        nrow = terra::nrow(preview),
        ncol = terra::ncol(preview),
        byrow = TRUE
      ) |>
      t()

    graphics::par(mar = c(4, 4, 1, 1))
    graphics::plot(
      NA,
      xlim = c(0, n_line),
      ylim = c(n_sample, 0),
      xlab = "line",
      ylab = "sample",
      xaxs = "i",
      yaxs = "i",
      asp = NA
    )
    graphics::rasterImage(img, 0, n_sample, n_line, 0, interpolate = FALSE)
  })

  sat_result <- shiny::eventReactive(input$sat_run, {
    cap <- found()
    shiny::req(cap)
    path <- envi_data(cap[["target"]])
    shiny::req(path)

    pct <- input$sat_percent
    shiny::req(is.numeric(pct), !is.na(pct))
    limit <- SATURATION_CEILING * pct / 100

    step <- as.integer(input$sat_band_step %||% "1")

    # A fresh handle, because SpatRaster carries a shared pointer and setting a
    # window on the cached one would leak the region of interest into the preview.
    # Bands are chosen at open time: subsetting afterwards would still read the
    # whole cube.
    n_band <- terra::nlyr(terra::rast(path))
    bands <- seq(1, n_band, by = step)
    x <- terra::rast(path, lyrs = bands)
    n_line <- terra::nrow(x)
    n_sample <- terra::ncol(x)
    roi <- brush_window(input$sat_brush, n_line, n_sample)

    # The region to screen, in the raster's own coordinates: the brush rectangle
    # or the whole frame.
    x_min <- if (is.null(roi)) 0 else terra::xmin(roi)
    x_max <- if (is.null(roi)) n_sample else terra::xmax(roi)
    y_min <- if (is.null(roi)) 0 else terra::ymin(roi)
    y_max <- if (is.null(roi)) n_line else terra::ymax(roi)

    # Screened in blocks of lines so the progress bar tracks real work rather
    # than spinning: the collapsed mask is per pixel, so block counts sum to the
    # count for the whole region. Blocks are contiguous line ranges, which is the
    # order the data sits in on disk, and integer edges fall on cell boundaries
    # so the blocks partition the region exactly. Reading runs from the first
    # line to the last.
    # Twelve blocks at most: each window change costs roughly 0.7 s of GDAL
    # re-initialisation, measured at about 20% overhead over fifteen blocks, and
    # twelve updates already read as a moving bar.
    n_block <- max(1, min(12, floor((y_max - y_min) / 200)))
    edges <- round(seq(y_max, y_min, length.out = n_block + 1))

    message <- if (step == 1L) {
      "Reading every band"
    } else {
      paste("Reading", length(bands), "of", n_band, "bands")
    }

    shiny::withProgress(message = message, value = 0, {
      tryCatch(
        {
          screened <- purrr::map(seq_len(n_block), \(i) {
            # window(), never crop(): cropping a raw integer capture materialises
            # a copy in the source datatype, which makes terra reserve 65535 as
            # NoData. Every genuinely clipped reading would come back NA and the
            # screen would report a clean scan.
            terra::window(x) <- NULL
            terra::window(x) <- terra::ext(
              x_min,
              x_max,
              edges[[i + 1]],
              edges[[i]]
            )

            mask <- HSItools::hsi_check_saturation(
              x,
              limit = limit,
              collapse = TRUE
            )
            # Dimensions come back from the windowed raster, never from the
            # brush: terra snaps the window to cell boundaries, and the brush
            # rectangle is fractional.
            block <- list(
              rows = terra::nrow(x),
              cols = terra::ncol(x),
              cells = terra::ncell(x),
              saturated = terra::global(mask, "sum", na.rm = TRUE)[[1]]
            )

            shiny::incProgress(
              1 / n_block,
              detail = paste("block", i, "of", n_block)
            )

            block
          })

          list(
            limit = limit,
            # Summed rather than derived, so the report describes what was
            # actually read.
            lines = sum(purrr::map_dbl(screened, "rows")),
            samples = screened[[1]][["cols"]],
            cells = sum(purrr::map_dbl(screened, "cells")),
            saturated = sum(purrr::map_dbl(screened, "saturated")),
            roi = !is.null(roi),
            bands = length(bands),
            n_band = n_band
          )
        },
        error = \(e) e
      )
    })
  })

  output$sat_report <- shiny::renderUI({
    res <- sat_result()

    if (inherits(res, "error")) {
      return(shiny::div(
        class = "alert alert-danger mt-3 mb-0",
        conditionMessage(res)
      ))
    }

    share <- 100 * res[["saturated"]] / res[["cells"]]

    # Any clipping at all is worth seeing; the tiers only say how loudly.
    theme <- if (res[["saturated"]] == 0) {
      "success"
    } else if (share < 0.1) {
      "warning"
    } else {
      "danger"
    }

    # A sampled screen can miss a saturated pixel but never invent one, so its
    # number is a floor rather than a measurement, and it is worded as one.
    sampled <- res[["bands"]] < res[["n_band"]]

    verdict <- if (res[["saturated"]] == 0 && sampled) {
      "No overexposed pixels found"
    } else if (res[["saturated"]] == 0) {
      "No overexposed pixels"
    } else {
      paste0(
        if (sampled) "At least " else "",
        format(round(share, 3), trim = TRUE),
        "% of pixels overexposed"
      )
    }

    shiny::div(
      class = paste0("alert alert-", theme, " mt-2 mb-0 py-2"),
      shiny::strong(verdict),
      shiny::br(),
      shiny::span(
        class = "small",
        sprintf(
          "%s of %s pixels at or above %g DN, over %s lines x %s samples (%s).",
          format(
            res[["saturated"]],
            big.mark = " ",
            scientific = FALSE,
            trim = TRUE
          ),
          format(
            res[["cells"]],
            big.mark = " ",
            scientific = FALSE,
            trim = TRUE
          ),
          round(res[["limit"]]),
          format(res[["lines"]], big.mark = " ", trim = TRUE),
          format(res[["samples"]], big.mark = " ", trim = TRUE),
          if (res[["roi"]]) "region of interest" else "full frame"
        ),
        if (sampled) {
          sprintf(
            " Screened %s of %s bands, so the count is a lower bound.",
            res[["bands"]],
            res[["n_band"]]
          )
        }
      )
    )
  })
}

shiny::shinyApp(ui, server)

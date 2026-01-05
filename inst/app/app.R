# HSI Calibration Tool v0.3.0
# Tab 1: Check calibration
# Tab 2: Calculate ideal FOV
# Tab 3: Scan Log

library(shiny)
library(bslib)
library(shinyFiles)
library(terra)
library(measurements)
library(yaml)
library(jsonlite)
library(countrycode)

# ============================================================================
# Helper Functions
# ============================================================================

# Get config directory path
get_config_dir <- function() {
  config_dir <- file.path(path.expand("~"), ".hsical")
  if (!dir.exists(config_dir)) dir.create(config_dir, recursive = TRUE)
  config_dir
}

# Load material types from config
load_material_types <- function() {
  config_file <- file.path(get_config_dir(), "material_types.json")
  defaults <- c(
    "lake sediments", "marine sediments", "peat", "soil",
    "rock", "outcrop", "other"
  )
  
  if (file.exists(config_file)) {
    tryCatch({
      saved <- fromJSON(config_file)
      unique(c(saved, defaults))
    }, error = \(e) defaults)
  } else {
    defaults
  }
}

# Save material types to config
save_material_types <- function(types) {
  config_file <- file.path(get_config_dir(), "material_types.json")
  write_json(types, config_file, auto_unbox = TRUE)
}

# Parse ENVI .hdr file
parse_hdr <- function(hdr_path) {
  if (!file.exists(hdr_path)) return(NULL)
  
  lines <- readLines(hdr_path, warn = FALSE)
  content <- paste(lines, collapse = "\n")
  
  extract_value <- function(pattern) {
    match <- regmatches(content, regexpr(pattern, content, perl = TRUE))
    if (length(match) == 0 || match == "") return(NA)
    match
  }
  
  extract_numeric <- function(key) {
    pattern <- paste0("(?<=", key, " = )[0-9.]+")
    val <- extract_value(pattern)
    if (is.na(val)) return(NA)
    as.numeric(val)
  }
  
  binning_match <- regmatches(content, regexpr("(?<=binning = \\{)[0-9]+, [0-9]+(?=\\})", content, perl = TRUE))
  if (length(binning_match) > 0 && binning_match != "") {
    binning <- as.numeric(strsplit(binning_match, ", ")[[1]])
    spectral_bin <- binning[1]
    spatial_bin <- binning[2]
  } else {
    spectral_bin <- NA
    spatial_bin <- NA
  }
  
  list(
    lines = extract_numeric("lines"),
    samples = extract_numeric("samples"),
    bands = extract_numeric("bands"),
    fps = extract_numeric("fps"),
    tint = extract_numeric("tint"),
    sensor_id = extract_numeric("sensorid"),
    spectral_binning = spectral_bin,
    spatial_binning = spatial_bin,
    acquisition_date = extract_value("(?<=acquisition date = DATE\\(yyyy-mm-dd\\): )[0-9-]+"),
    start_time = extract_value("(?<=Start Time = UTC TIME: )[0-9:]+"),
    stop_time = extract_value("(?<=Stop Time = UTC TIME: )[0-9:]+")
  )
}

# Parse .log file for dropped frames
parse_log <- function(log_path) {
  if (!file.exists(log_path)) return(list(dropped = NA, recorded = NA))
  
  content <- paste(readLines(log_path, warn = FALSE), collapse = "\n")
  
  dropped <- as.numeric(regmatches(content, regexpr("(?<=incidents, )[0-9]+(?= dropped frames)", content, perl = TRUE)))
  recorded <- as.numeric(regmatches(content, regexpr("[0-9]+(?= frames recorded)", content, perl = TRUE)))
  
  list(
    dropped = if (length(dropped) == 0) NA else dropped,
    recorded = if (length(recorded) == 0) NA else recorded
  )
}

# Zero-pad numbers for natural sorting
zero_pad <- function(x, width = 2) {
  if (is.na(x)) return(NA)
  sprintf(paste0("%0", width, "d"), as.integer(x))
}

# Generate YAML content for individual scan log
generate_yaml <- function(data) {
  yaml_list <- list(
    location = list(
      site_name = data$site_name,
      site_code = data$site_code,
      country = data$country,
      country_code = tolower(data$country_code)
    ),
    sample = list(
      core_id = data$core_id,
      section_depth = data$section_depth,
      section_number = zero_pad(data$section_number),
      material_type = data$material_type
    ),
    ownership = list(
      material_owner = data$material_owner,
      operator = data$operator
    ),
    scan_setup = list(
      camera = data$camera,
      date = data$scan_date,
      time = data$scan_time,
      scan_start_mm = data$scan_start,
      scan_end_mm = data$scan_end,
      fov_mm = data$fov,
      spectral_binning = data$spectral_binning,
      spatial_binning = data$spatial_binning,
      integration_time = data$integration_time,
      frame_rate = data$frame_rate
    ),
    references = list(
      additional_whiteref = data$additional_whiteref,
      additional_whiteref_exposure = data$whiteref_exposure
    ),
    results = list(
      pixel_resolution_um = data$pixel_resolution,
      aspect_ratio = data$aspect_ratio,
      total_lines = data$total_lines,
      dropped_frames = data$dropped_frames
    ),
    admin = list(
      project = data$project,
      filename = data$filename,
      comments = data$comments
    )
  )
  
  as.yaml(yaml_list)
}

# Generate CSV row for master log
generate_csv_row <- function(data) {
  data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    site_name = data$site_name,
    site_code = data$site_code,
    country = data$country,
    country_code = tolower(data$country_code),
    core_id = data$core_id,
    section_depth = data$section_depth,
    section_number = zero_pad(data$section_number),
    material_type = data$material_type,
    material_owner = data$material_owner,
    operator = data$operator,
    camera = data$camera,
    scan_date = data$scan_date,
    scan_time = data$scan_time,
    scan_start_mm = data$scan_start,
    scan_end_mm = data$scan_end,
    fov_mm = data$fov,
    spectral_binning = data$spectral_binning,
    spatial_binning = data$spatial_binning,
    integration_time = data$integration_time,
    frame_rate = data$frame_rate,
    additional_whiteref = data$additional_whiteref,
    whiteref_exposure = data$whiteref_exposure,
    pixel_resolution_um = data$pixel_resolution,
    aspect_ratio = data$aspect_ratio,
    total_lines = data$total_lines,
    dropped_frames = data$dropped_frames,
    project = data$project,
    filename = data$filename,
    comments = data$comments,
    stringsAsFactors = FALSE
  )
}

# ============================================================================
# UI
# ============================================================================

ui <- page_sidebar(
  title = "HSI Calibration Tool",
  theme = bs_theme(version = 5, bootswatch = "flatly", "navbar-bg" = "#2C3E50"),
  fillable = FALSE,  # Allow page to scroll
  
  sidebar = sidebar(
    width = 300,
    
    # File selection (shared)
    shinyFilesButton(
      id = "file_select",
      label = "Select raster file",
      title = "Choose a raster file",
      multiple = FALSE,
      icon = icon("file")
    ),
    verbatimTextOutput("file_path", placeholder = TRUE),
    
    hr(),
    
    # Scan length input (shared for Tab 1 & 2)
    conditionalPanel(
      condition = "input.tabs != 'Scan Log'",
      h6("Scan Length"),
      layout_columns(
        col_widths = c(7, 5),
        numericInput("scan_len", label = NULL, value = NULL, min = 0),
        selectInput("scan_len_unit", label = NULL,
                    choices = c("cm", "mm", "µm" = "um"), selected = "cm")
      )
    ),
    
    # FOV input (Tab 1 only)
    conditionalPanel(
      condition = "input.tabs == 'Check Calibration'",
      hr(),
      h6("Field of View (measured)"),
      layout_columns(
        col_widths = c(7, 5),
        numericInput("scan_fov", label = NULL, value = NULL, min = 0),
        selectInput("scan_fov_unit", label = NULL,
                    choices = c("cm", "mm", "µm" = "um"), selected = "mm")
      )
    ),
    
    # Tab 3 sidebar controls
    conditionalPanel(
      condition = "input.tabs == 'Scan Log'",
      
      actionButton("load_from_raster", "Load metadata from raster",
                   icon = icon("download"), class = "btn-primary mb-3 w-100"),
      
      hr(),
      
      h6("Individual file save location"),
      shinyDirButton(
        id = "save_dir",
        label = "Choose folder",
        title = "Select folder for individual scan log",
        icon = icon("folder-open")
      ),
      verbatimTextOutput("save_dir_path", placeholder = TRUE)
    )
  ),
  
  # Main panel with tabs
  navset_card_tab(
    id = "tabs",
    
    # =========================================================================
    # Tab 1: Check Calibration
    # =========================================================================
    nav_panel(
      title = "Check Calibration",
      icon = icon("check-circle"),
      
      layout_columns(
        col_widths = 12,
        
        card(
          card_header("Raster Dimensions"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              value_box(
                title = "Rows",
                value = textOutput("check_n_rows", inline = TRUE),
                showcase = icon("arrows-up-down"),
                theme = "secondary"
              ),
              value_box(
                title = "Columns",
                value = textOutput("check_n_cols", inline = TRUE),
                showcase = icon("arrows-left-right"),
                theme = "secondary"
              )
            )
          )
        ),
        
        card(
          card_header("Calculated Resolution"),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title = "Length Resolution",
                value = textOutput("res_len", inline = TRUE),
                showcase = icon("ruler-vertical"),
                theme = "primary"
              ),
              value_box(
                title = "FOV Resolution",
                value = textOutput("res_fov", inline = TRUE),
                showcase = icon("ruler-horizontal"),
                theme = "primary"
              ),
              uiOutput("ratio_box")
            )
          )
        )
      )
    ),
    
    # =========================================================================
    # Tab 2: Calculate Ideal FOV
    # =========================================================================
    nav_panel(
      title = "Calculate Ideal FOV",
      icon = icon("calculator"),
      
      layout_columns(
        col_widths = 12,
        
        card(
          card_header("Raster Dimensions"),
          card_body(
            layout_columns(
              col_widths = c(6, 6),
              value_box(
                title = "Rows",
                value = textOutput("calc_n_rows", inline = TRUE),
                showcase = icon("arrows-up-down"),
                theme = "secondary"
              ),
              value_box(
                title = "Columns",
                value = textOutput("calc_n_cols", inline = TRUE),
                showcase = icon("arrows-left-right"),
                theme = "secondary"
              )
            )
          )
        ),
        
        card(
          card_header("True Pixel Size (from scan length)"),
          card_body(
            value_box(
              title = "Pixel Resolution",
              value = textOutput("true_pixel_size", inline = TRUE),
              showcase = icon("expand"),
              theme = "primary"
            )
          )
        ),
        
        card(
          card_header("Ideal FOV for Square Pixels"),
          card_body(
            layout_columns(
              col_widths = c(4, 4, 4),
              value_box(
                title = "FOV",
                value = textOutput("ideal_fov_um", inline = TRUE),
                p("micrometers"),
                showcase = icon("ruler-horizontal"),
                theme = "success"
              ),
              value_box(
                title = "FOV",
                value = textOutput("ideal_fov_mm", inline = TRUE),
                p("millimeters"),
                showcase = icon("ruler-horizontal"),
                theme = "success"
              ),
              value_box(
                title = "FOV",
                value = textOutput("ideal_fov_cm", inline = TRUE),
                p("centimeters"),
                showcase = icon("ruler-horizontal"),
                theme = "success"
              )
            )
          )
        )
      )
    ),
    
    # =========================================================================
    # Tab 3: Scan Log
    # =========================================================================
    nav_panel(
      title = "Scan Log",
      icon = icon("clipboard-list"),
      
      # Row 1: Location, Sample, Ownership
      layout_columns(
        col_widths = c(4, 4, 4),
        
        card(
          card_header("Location", class = "bg-info text-white"),
          card_body(
            textInput("site_name", "Site name (full)", placeholder = "Name"),
            textInput("site_code", "Site code", placeholder = "CODE"),
            textInput("country", "Country (full, English)", placeholder = "Country"),
            textInput("country_code", "Country code (2-letter)", 
                      placeholder = "xx")
          )
        ),
        
        card(
          card_header("Sample", class = "bg-info text-white"),
          card_body(
            textInput("core_id", "Core ID", placeholder = "CORE-ID"),
            textInput("section_depth", "Section depth (cm)", placeholder = "0-50"),
            numericInput("section_number", "Section number", value = 1, min = 1),
            selectizeInput("material_type", "Material type",
                           choices = NULL,
                           options = list(create = TRUE, placeholder = "Select or add..."))
          )
        ),
        
        card(
          card_header("Ownership", class = "bg-info text-white"),
          card_body(
            textInput("material_owner", "Material owner", 
                      placeholder = "Institution / PI"),
            textInput("operator", "Operator (who scanned)", 
                      placeholder = "Initials")
          )
        )
      ),
      
      # Row 2: Scan Setup, References, Results
      layout_columns(
        col_widths = c(4, 4, 4),
        
        card(
          card_header("Scan Setup", class = "bg-primary text-white"),
          card_body(
            selectInput("camera", "Camera", 
                        choices = c("VNIR", "SWIR"), selected = "VNIR"),
            layout_columns(
              col_widths = c(6, 6),
              dateInput("scan_date", "Date", value = Sys.Date()),
              textInput("scan_time", "Time", placeholder = "HH:MM:SS")
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("log_scan_start", "Scan start (mm)", value = NULL),
              numericInput("log_scan_end", "Scan end (mm)", value = NULL)
            ),
            numericInput("log_fov", "FOV (mm)", value = NULL),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("log_spectral_bin", "Spectral bin", value = NULL),
              numericInput("log_spatial_bin", "Spatial bin", value = NULL)
            ),
            layout_columns(
              col_widths = c(6, 6),
              numericInput("log_tint", "Integration time", value = NULL),
              numericInput("log_fps", "Frame rate", value = NULL)
            )
          )
        ),
        
        card(
          card_header("References", class = "bg-warning"),
          card_body(
            selectInput("additional_whiteref", "Additional whiteref taken?",
                        choices = c("No" = "no", "Yes" = "yes"), selected = "no"),
            conditionalPanel(
              condition = "input.additional_whiteref == 'yes'",
              numericInput("whiteref_exposure", "Whiteref exposure", value = NULL)
            )
          )
        ),
        
        card(
          card_header("Results", class = "bg-success text-white"),
          card_body(
            numericInput("log_pixel_res", "Pixel resolution (µm/px)", value = NULL),
            numericInput("log_aspect_ratio", "Aspect ratio", value = NULL, step = 0.001),
            numericInput("log_total_lines", "Total lines", value = NULL),
            numericInput("log_dropped_frames", "Dropped frames", value = NULL)
          )
        )
      ),
      
      # Row 3: Admin
      card(
        card_header("Admin", class = "bg-secondary text-white"),
        card_body(
          layout_columns(
            col_widths = c(6, 6),
            textInput("project", "Project / Campaign", placeholder = "Project name"),
            textInput("log_filename", "Filename", placeholder = "Auto-filled from raster")
          ),
          textAreaInput("comments", "Comments", rows = 3,
                        placeholder = "Any additional notes...")
        )
      ),
      
      # Save buttons
      card(
        card_body(
          layout_columns(
            col_widths = c(4, 4, 4),
            actionButton("save_individual", "Save Individual (YAML)",
                         icon = icon("file-export"), class = "btn-outline-primary w-100"),
            actionButton("save_master", "Save to Master Log (CSV)",
                         icon = icon("database"), class = "btn-outline-success w-100"),
            actionButton("save_both", "Save Both",
                         icon = icon("save"), class = "btn-primary w-100")
          )
        )
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {
  
  # Initialize volumes for file/folder pickers
  volumes <- getVolumes()()
  
  shinyFileChoose(input, "file_select", roots = volumes,
                  filetypes = c("raw", "tif", "tiff"))
  
  shinyDirChoose(input, "save_dir", roots = volumes)
  
  # Load and initialize material types
  material_types <- reactiveVal(load_material_types())
  
  observe({
    updateSelectizeInput(session, "material_type",
                         choices = material_types(),
                         server = TRUE)
  })
  
  # When user adds a new material type, persist it
  observeEvent(input$material_type, {
    if (!is.null(input$material_type) && 
        input$material_type != "" &&
        !input$material_type %in% material_types()) {
      new_types <- c(material_types(), input$material_type)
      material_types(new_types)
      save_material_types(new_types)
    }
  })
  
  # Auto-fill country code from country name
 observeEvent(input$country, {
    req(input$country)
    if (nchar(input$country) >= 3) {
      code <- tryCatch(
        countrycode(input$country, origin = "country.name", destination = "iso2c"),
        warning = \(w) NA,
        error = \(e) NA
      )
      if (!is.na(code)) {
        updateTextInput(session, "country_code", value = tolower(code))
      }
    }
  })
  
  # ---------------------------------------------------------------------------
  # Shared reactives
  # ---------------------------------------------------------------------------
  
  selected_path <- reactive({
    req(input$file_select)
    file_info <- parseFilePaths(volumes, input$file_select)
    if (nrow(file_info) == 0) return(NULL)
    as.character(file_info$datapath)
  })
  
  raster_data <- reactive({
    req(selected_path())
    tryCatch(
      terra::rast(selected_path()),
      error = \(e) {
        showNotification(paste("Error reading raster:", e$message), type = "error")
        NULL
      }
    )
  })
  
  scan_len_um <- reactive({
    req(input$scan_len, input$scan_len > 0)
    conv_unit(input$scan_len, input$scan_len_unit, "um")
  })
  
  true_pixel_res <- reactive({
    req(raster_data(), scan_len_um())
    scan_len_um() / nrow(raster_data())
  })
  
  check_calculations <- reactive({
    req(raster_data(), scan_len_um(), input$scan_fov, input$scan_fov > 0)
    r <- raster_data()
    fov_um <- conv_unit(input$scan_fov, input$scan_fov_unit, "um")
    res_len <- scan_len_um() / nrow(r)
    res_fov <- fov_um / ncol(r)
    list(res_len = res_len, res_fov = res_fov, ratio = res_len / res_fov)
  })
  
  ideal_fov <- reactive({
    req(raster_data(), true_pixel_res())
    r <- raster_data()
    fov_um <- true_pixel_res() * ncol(r)
    list(
      um = fov_um,
      mm = conv_unit(fov_um, "um", "mm"),
      cm = conv_unit(fov_um, "um", "cm")
    )
  })
  
  # ---------------------------------------------------------------------------
  # Outputs: File path display
  # ---------------------------------------------------------------------------
  
  output$file_path <- renderText({
    path <- selected_path()
    if (is.null(path)) "No file selected" else basename(path)
  })
  
  # ---------------------------------------------------------------------------
  # Outputs: Tab 1 - Check Calibration
  # ---------------------------------------------------------------------------
  
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
    calc <- check_calculations()
    if (is.null(calc)) {
      value_box(title = "Aspect Ratio", value = "—",
                showcase = icon("square"), theme = "secondary")
    } else {
      ratio <- calc$ratio
      is_square <- ratio >= 0.95 && ratio <= 1.05
      value_box(
        title = "Aspect Ratio",
        value = round(ratio, 3),
        p(if (is_square) "✓ Pixels are square" else "⚠ Pixels not square"),
        showcase = icon(if (is_square) "square-check" else "square"),
        theme = if (is_square) "success" else "warning"
      )
    }
  })
  
  # ---------------------------------------------------------------------------
  # Outputs: Tab 2 - Calculate Ideal FOV
  # ---------------------------------------------------------------------------
  
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
  
  output$ideal_fov_um <- renderText({
    fov <- ideal_fov()
    if (is.null(fov)) "—" else format(round(fov$um, 1), big.mark = ",")
  })
  
  output$ideal_fov_mm <- renderText({
    fov <- ideal_fov()
    if (is.null(fov)) "—" else round(fov$mm, 2)
  })
  
  output$ideal_fov_cm <- renderText({
    fov <- ideal_fov()
    if (is.null(fov)) "—" else round(fov$cm, 3)
  })
  
  # ---------------------------------------------------------------------------
  # Tab 3: Save directory path display
  # ---------------------------------------------------------------------------
  
  selected_save_dir <- reactive({
    req(input$save_dir)
    path <- parseDirPath(volumes, input$save_dir)
    if (length(path) == 0) return(NULL)
    as.character(path)
  })
  
  output$save_dir_path <- renderText({
    dir <- selected_save_dir()
    if (is.null(dir)) "No folder selected" else dir
  })
  
  # ---------------------------------------------------------------------------
  # Tab 3: Load metadata from raster
  # ---------------------------------------------------------------------------
  
  observeEvent(input$load_from_raster, {
    req(selected_path())
    
    raster_path <- selected_path()
    base_path <- tools::file_path_sans_ext(raster_path)
    hdr_path <- paste0(base_path, ".hdr")
    log_path <- paste0(base_path, ".log")
    
    hdr_data <- parse_hdr(hdr_path)
    log_data <- parse_log(log_path)
    
    if (!is.null(hdr_data)) {
      updateNumericInput(session, "log_spectral_bin", value = hdr_data$spectral_binning)
      updateNumericInput(session, "log_spatial_bin", value = hdr_data$spatial_binning)
      updateNumericInput(session, "log_tint", value = round(hdr_data$tint, 2))
      updateNumericInput(session, "log_fps", value = hdr_data$fps)
      updateNumericInput(session, "log_total_lines", value = hdr_data$lines)
      
      if (!is.na(hdr_data$acquisition_date)) {
        updateDateInput(session, "scan_date", value = as.Date(hdr_data$acquisition_date))
      }
      if (!is.na(hdr_data$start_time)) {
        updateTextInput(session, "scan_time", value = hdr_data$start_time)
      }
    }
    
    if (!is.null(log_data)) {
      updateNumericInput(session, "log_dropped_frames", value = log_data$dropped)
    }
    
    updateTextInput(session, "log_filename", value = basename(raster_path))
    
    calc <- check_calculations()
    if (!is.null(calc)) {
      updateNumericInput(session, "log_pixel_res", value = round(calc$res_len, 2))
      updateNumericInput(session, "log_aspect_ratio", value = round(calc$ratio, 3))
    }
    
    showNotification("Metadata loaded from raster files", type = "message")
  })
  
  # ---------------------------------------------------------------------------
  # Tab 3: Collect form data
  # ---------------------------------------------------------------------------
  
  collect_form_data <- reactive({
    list(
      site_name = input$site_name %||% "",
      site_code = input$site_code %||% "",
      country = input$country %||% "",
      country_code = input$country_code %||% "",
      core_id = input$core_id %||% "",
      section_depth = input$section_depth %||% "",
      section_number = input$section_number %||% NA,
      material_type = input$material_type %||% "",
      material_owner = input$material_owner %||% "",
      operator = input$operator %||% "",
      camera = input$camera %||% "",
      scan_date = as.character(input$scan_date) %||% "",
      scan_time = input$scan_time %||% "",
      scan_start = input$log_scan_start %||% NA,
      scan_end = input$log_scan_end %||% NA,
      fov = input$log_fov %||% NA,
      spectral_binning = input$log_spectral_bin %||% NA,
      spatial_binning = input$log_spatial_bin %||% NA,
      integration_time = input$log_tint %||% NA,
      frame_rate = input$log_fps %||% NA,
      additional_whiteref = input$additional_whiteref %||% "no",
      whiteref_exposure = if (input$additional_whiteref == "yes") input$whiteref_exposure else NA,
      pixel_resolution = input$log_pixel_res %||% NA,
      aspect_ratio = input$log_aspect_ratio %||% NA,
      total_lines = input$log_total_lines %||% NA,
      dropped_frames = input$log_dropped_frames %||% NA,
      project = input$project %||% "",
      filename = input$log_filename %||% "",
      comments = input$comments %||% ""
    )
  })
  
  # ---------------------------------------------------------------------------
  # Tab 3: Save functions
  # ---------------------------------------------------------------------------
  
  save_individual_file <- function() {
    dir <- selected_save_dir()
    if (is.null(dir)) {
      showNotification("Please select a folder first", type = "error")
      return(FALSE)
    }
    
    data <- collect_form_data()
    yaml_content <- generate_yaml(data)
    
    base_name <- if (data$filename != "") {
      tools::file_path_sans_ext(data$filename)
    } else {
      format(Sys.time(), "%Y%m%d_%H%M%S")
    }
    
    file_path <- file.path(dir, paste0(base_name, "_scanlog.yaml"))
    
    tryCatch({
      writeLines(yaml_content, file_path)
      showNotification(paste("Saved:", basename(file_path)), type = "message")
      TRUE
    }, error = \(e) {
      showNotification(paste("Error saving:", e$message), type = "error")
      FALSE
    })
  }
  
  save_to_master <- function() {
    data <- collect_form_data()
    row <- generate_csv_row(data)
    
    master_path <- file.path(getwd(), "hsical_master_log.csv")
    
    tryCatch({
      if (file.exists(master_path)) {
        write.table(row, master_path, append = TRUE, sep = ",",
                    row.names = FALSE, col.names = FALSE, quote = TRUE)
      } else {
        write.csv(row, master_path, row.names = FALSE, quote = TRUE)
      }
      showNotification(paste("Appended to master log:", master_path), type = "message")
      TRUE
    }, error = \(e) {
      showNotification(paste("Error saving to master:", e$message), type = "error")
      FALSE
    })
  }
  
  observeEvent(input$save_individual, { save_individual_file() })
  observeEvent(input$save_master, { save_to_master() })
  observeEvent(input$save_both, {
    save_individual_file()
    save_to_master()
  })
}

# ============================================================================
# Run
# ============================================================================

shinyApp(ui, server)

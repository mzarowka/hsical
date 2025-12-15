# Raster Calibration Tool
# Calculates pixel resolution from test scans

library(shiny)
library(bslib)
library(shinyFiles)
library(terra)
library(measurements)

# UI ----
ui <- page_sidebar(
  title = "Raster Calibration Tool",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  sidebar = sidebar(
    width = 300,
    
    # File selection
    shinyFilesButton(
      id = "file_select",
      label = "Select raster file",
      title = "Choose a raster file",
      multiple = FALSE,
      icon = icon("file")
    ),
    verbatimTextOutput("file_path", placeholder = TRUE),
    
    hr(),
    
    # Scan length input
    h6("Scan Length"),
    layout_columns(
      col_widths = c(7, 5),
      numericInput(
        "scan_len",
        label = NULL,
        value = NULL,
        min = 0
      ),
      selectInput(
        "scan_len_unit",
        label = NULL,
        choices = c("cm", "mm", "µm" = "um"),
        selected = "cm"
      )
    ),
    
    # Field of view input
    h6("Field of View"),
    layout_columns(
      col_widths = c(7, 5),
      numericInput(
        "scan_fov",
        label = NULL,
        value = NULL,
        min = 0
      ),
      selectInput(
        "scan_fov_unit",
        label = NULL,
        choices = c("cm", "mm", "µm" = "um"),
        selected = "mm"
      )
    )
  ),
  
  # Main panel
  layout_columns(
    col_widths = 12,
    
    # Raster info card
    card(
      card_header("Raster Dimensions"),
      card_body(
        layout_columns(
          col_widths = c(6, 6),
          value_box(
            title = "Rows",
            value = textOutput("n_rows", inline = TRUE),
            showcase = icon("arrows-up-down"),
            theme = "secondary"
          ),
          value_box(
            title = "Columns",
            value = textOutput("n_cols", inline = TRUE),
            showcase = icon("arrows-left-right"),
            theme = "secondary"
          )
        )
      )
    ),
    
    # Resolution results
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
)

# Server ----
server <- function(input, output, session) {
  

  # Set up file chooser volumes
  volumes <- getVolumes()
  
  shinyFileChoose(
    input,
    "file_select",
    roots = volumes,
    filetypes = c("raw", "tif", "tiff")
  )
  
# Reactive: selected file path
  selected_path <- reactive({
    req(input$file_select)
    
    file_info <- parseFilePaths(volumes, input$file_select)
    
    if (nrow(file_info) == 0) return(NULL)
    
    as.character(file_info$datapath)
  })
  
  # Reactive: loaded raster
  raster_data <- reactive({
    req(selected_path())
    
    tryCatch(
      terra::rast(selected_path()),
      error = \(e) {
        showNotification(
          paste("Error reading raster:", e$message),
          type = "error"
        )
        NULL
      }
    )
  })
  
  # Reactive: calculated values
  calculations <- reactive({
    req(raster_data(), input$scan_len, input$scan_fov)
    req(input$scan_len > 0, input$scan_fov > 0)
    
    r <- raster_data()
    
    # Convert inputs to micrometers
    len_um <- conv_unit(input$scan_len, input$scan_len_unit, "um")
    fov_um <- conv_unit(input$scan_fov, input$scan_fov_unit, "um")
    
    # Calculate resolutions
    res_len <- len_um / nrow(r)
    res_fov <- fov_um / ncol(r)
    ratio <- res_len / res_fov
    
    list(
      res_len = res_len,
      res_fov = res_fov,
      ratio = ratio
    )
  })
  
  # Outputs: file path display
  output$file_path <- renderText({
    path <- selected_path()
    if (is.null(path)) {
      "No file selected"
    } else {
      basename(path)
    }
  })
  
  # Outputs: raster dimensions
  output$n_rows <- renderText({
    r <- raster_data()
    if (is.null(r)) "—" else format(nrow(r), big.mark = ",")
  })
  
  output$n_cols <- renderText({
    r <- raster_data()
    if (is.null(r)) "—" else format(ncol(r), big.mark = ",")
  })
  
  # Outputs: resolution values
  output$res_len <- renderText({
    calc <- calculations()
    if (is.null(calc)) {
      "—"
    } else {
      paste(round(calc$res_len, 2), "µm/px")
    }
  })
  
  output$res_fov <- renderText({
    calc <- calculations()
    if (is.null(calc)) {
      "—"
    } else {
      paste(round(calc$res_fov, 2), "µm/px")
    }
  })
  
  # Output: ratio box with conditional coloring
  output$ratio_box <- renderUI({
    calc <- calculations()
    
    if (is.null(calc)) {
      value_box(
        title = "Aspect Ratio",
        value = "—",
        showcase = icon("square"),
        theme = "secondary"
      )
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
}

# Run ----
shinyApp(ui, server)

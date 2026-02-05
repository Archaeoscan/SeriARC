# mod_data_import.R
# i18n: tr function is passed as parameter

mod_data_import_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)

  # Column choices with translation
  col_indices <- c(2:6, 40:43)
  col_choices <- setNames(
    c("none", as.character(col_indices)),
    c(tr("import.column.none"), paste(tr("term.column"), col_indices))
  )

  tagList(
    conditionalPanel(
      condition = "!output.user_file_loaded", ns = ns,
      div(class="alert alert-success", style="margin-bottom: 15px; padding: 10px;",
          HTML(paste0(tr("import.example.active"), "<br>
                <small style='color: #555;'>
                ", tr("import.example.source"), ": Carlson, D.L. & Roth, G. (2021). <em>archdata</em>: Example Datasets from Archaeological Research.
                R package version 1.2-1.
                <a href='https://CRAN.R-project.org/package=archdata' target='_blank'>https://CRAN.R-project.org/package=archdata</a>
                </small>"))
      )
    ),

    fileInput(ns("file"), tr("import.file.label"),
              accept = c(".xlsx", ".xls"),
              placeholder = tr("import.file.placeholder")),

    div(class="alert alert-info", style="margin: 10px 0; padding: 8px;",
        HTML(paste0("<strong>", tr("import.column.mapping"), "</strong> ", tr("import.column.mapping.hint")))
    ),

    h5(paste0("🔧 ", tr("import.column.mapping")), style="margin: 15px 0 10px 0;"),

    fluidRow(
      column(6,
             selectInput(ns("group_col"),
                         HTML(paste0(tr("import.group.column"), " <span class='info-icon' title='", tr("import.group.tooltip"), "'>ℹ️</span>")),
                         choices = col_choices,
                         selected = "none")
      ),
      column(6,
             div(style="margin-top: 25px; font-size: 0.9em; color: #666;",
                 tr("import.column.warning"))
      )
    ),

    fluidRow(
      column(6,
             selectInput(ns("x_coord_col"),
                         HTML(paste0(tr("import.x.coord"), " <span class='info-icon' title='", tr("import.x.tooltip"), "'>ℹ️</span>")),
                         choices = col_choices,
                         selected = "none")
      ),
      column(6,
             selectInput(ns("y_coord_col"),
                         HTML(paste0(tr("import.y.coord"), " <span class='info-icon' title='", tr("import.y.tooltip"), "'>ℹ️</span>")),
                         choices = col_choices,
                         selected = "none")
      )
    ),

    conditionalPanel(
      condition = "output.utm_detected == true", ns = ns,
      div(class="alert alert-warning", style="margin: 10px 0; padding: 8px;",
          HTML(paste0("<strong>", tr("import.utm.detected"), "</strong>")),
          fluidRow(style="margin-top: 5px;",
                   column(6, numericInput(ns("utm_zone"),
                                          HTML(paste0(tr("import.utm.zone"), " <span class='info-icon' title='", tr("import.utm.zone.tooltip"), "'>ℹ️</span>")),
                                          value = 32, min = 1, max = 60, step = 1)),
                   column(6, selectInput(ns("utm_hemi"), tr("import.utm.hemisphere"),
                                         choices = setNames(c("N", "S"), c(tr("import.utm.north"), tr("import.utm.south"))),
                                         selected = "N"))
          )
      )
    ),

    uiOutput(ns("file_status_ui"))
  )
}

mod_data_import_server <- function(id, tr = function(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    coerce_num <- function(x) {
      if (is.numeric(x)) return(x)
      x <- gsub("[^-0-9.,]", "", as.character(x))
      x <- gsub(",", ".", x, fixed = TRUE)
      suppressWarnings(as.numeric(x))
    }
    
    detect_coord_type <- function(coord1, coord2) {
      x <- suppressWarnings(as.numeric(coord1))
      y <- suppressWarnings(as.numeric(coord2))
      x <- x[!is.na(x)]
      y <- y[!is.na(y)]
      
      if (length(x) == 0 || length(y) == 0) {
        return(list(type = NA, order = NA))
      }
      
      if (all(x >= -180 & x <= 180) && all(y >= -90 & y <= 90)) {
        return(list(type = "wgs84", order = "lonlat"))
      }
      if (all(y >= -180 & y <= 180) && all(x >= -90 & x <= 90)) {
        return(list(type = "wgs84", order = "latlon"))
      }
      
      if (all(x > 100000) && all(y > 1000000)) {
        return(list(type = "utm", order = "en"))
      }
      if (all(y > 100000) && all(x > 1000000)) {
        return(list(type = "utm", order = "ne"))
      }
      
      return(list(type = NA, order = NA))
    }
    
    utm_to_wgs84 <- function(easting, northing, zone, hemisphere = "N") {
      e <- suppressWarnings(as.numeric(easting))
      n <- suppressWarnings(as.numeric(northing))
      
      if (length(e) == 0 || length(n) == 0) {
        return(list(lon = numeric(0), lat = numeric(0)))
      }
      
      if (!requireNamespace("sf", quietly = TRUE)) {
        warning(tr("import.sf.unavailable"))
        return(list(lon = e, lat = n))
      }
      
      tryCatch({
        utm_crs <- paste0("+proj=utm +zone=", zone, 
                          ifelse(hemisphere == "S", " +south", ""), 
                          " +datum=WGS84 +units=m +no_defs")
        
        valid_idx <- !is.na(e) & !is.na(n)
        if (!any(valid_idx)) {
          return(list(lon = e, lat = n))
        }
        
        utm_points <- sf::st_as_sf(
          data.frame(x = e[valid_idx], y = n[valid_idx]), 
          coords = c("x", "y"), 
          crs = utm_crs
        )
        
        wgs84_points <- sf::st_transform(utm_points, crs = 4326)
        coords <- sf::st_coordinates(wgs84_points)
        
        result_lon <- rep(NA_real_, length(e))
        result_lat <- rep(NA_real_, length(n))
        result_lon[valid_idx] <- coords[,1]
        result_lat[valid_idx] <- coords[,2]
        
        return(list(lon = result_lon, lat = result_lat))
        
      }, error = function(err) {
        warning(tr("import.utm.failed"), ": ", err$message)
        return(list(lon = e, lat = n))
      })
    }
    
    mapping <- reactiveValues(meta = NULL, coord_info = NULL, raw = NULL)
    file_status <- reactiveVal(tr("import.status.none"))
    
    default_loaded <- reactiveVal(FALSE)
    
    output$user_file_loaded <- reactive({
      !is.null(input$file)
    })
    outputOptions(output, "user_file_loaded", suspendWhenHidden = FALSE)
    
    observe({
      if (!default_loaded() && is.null(input$file)) {
        if (exists("DEFAULT_DATASET") && !is.null(DEFAULT_DATASET)) {
          if (ncol(DEFAULT_DATASET) >= 41) updateSelectInput(session, "group_col", selected = "41")
          if (ncol(DEFAULT_DATASET) >= 42) updateSelectInput(session, "x_coord_col", selected = "42")
          if (ncol(DEFAULT_DATASET) >= 43) updateSelectInput(session, "y_coord_col", selected = "43")
          
          updateNumericInput(session, "utm_zone", value = 32)
          updateSelectInput(session, "utm_hemi", selected = "N")
          
          default_loaded(TRUE)
          file_status(tr("import.status.example"))
        }
      }
    })
    
    observeEvent(input$file, {
      req(input$file)
      updateSelectInput(session, "group_col", selected = "none")
      updateSelectInput(session, "x_coord_col", selected = "none")
      updateSelectInput(session, "y_coord_col", selected = "none")
      
    })
    
    data_raw <- reactive({
      
      if (!is.null(input$file)) {
        req(input$file)
        
        tryCatch({
          
          ext <- tolower(tools::file_ext(input$file$name))
          if (!ext %in% c("xlsx","xls")) stop(tr("import.error.format"))

          df0 <- readxl::read_excel(input$file$datapath, col_names = TRUE)
          df0 <- as.data.frame(df0)
          if (nrow(df0) == 0 || ncol(df0) < 1) stop(tr("import.error.empty"))

          sites <- as.character(df0[[1]])
          
          group <- if (!is.null(input$group_col) && input$group_col != "none" && ncol(df0) >= as.numeric(input$group_col)) {
            as.character(df0[[as.numeric(input$group_col)]])
          } else { NA_character_ }
          
          coord1 <- coord2 <- NA
          lon <- lat <- rep(NA_real_, length(sites))
          cinfo <- list(type = NA, order = NA)
          coord_status <- tr("import.coord.none")
          
          if (!is.null(input$x_coord_col) && !is.null(input$y_coord_col) &&
              input$x_coord_col != "none" && input$y_coord_col != "none" && 
              ncol(df0) >= max(as.numeric(input$x_coord_col), as.numeric(input$y_coord_col))) {
            
            coord1 <- df0[[as.numeric(input$x_coord_col)]]
            coord2 <- df0[[as.numeric(input$y_coord_col)]]
            
            if (!all(is.na(coord1)) && !all(is.na(coord2))) {
              info <- detect_coord_type(coord1, coord2)
              cinfo <- info
              
              if (!is.na(info$type) && info$type == "wgs84") {
                x <- coerce_num(coord1); y <- coerce_num(coord2)
                if (identical(info$order, "lonlat")) { 
                  lon <- x; lat <- y 
                } else { 
                  lon <- y; lat <- x 
                }
                valid_count <- sum(!is.na(lon) & !is.na(lat))
                coord_status <- sprintf(tr("import.coord.wgs84"), valid_count)
                
              } else if (!is.na(info$type) && info$type == "utm") {
                zone <- if (!is.null(input$utm_zone)) input$utm_zone else 32
                hemi <- if (!is.null(input$utm_hemi)) input$utm_hemi else "N"
                
                conv <- if (identical(info$order, "en")) {
                  tryCatch(utm_to_wgs84(coord1, coord2, zone, hemi), error = function(e) NULL)
                } else if (identical(info$order, "ne")) {
                  tryCatch(utm_to_wgs84(coord2, coord1, zone, hemi), error = function(e) NULL)
                } else NULL
                
                if (!is.null(conv)) {
                  lon <- conv$lon; lat <- conv$lat
                  valid_count <- sum(!is.na(lon) & !is.na(lat))
                  coord_status <- sprintf(tr("import.coord.utm"), valid_count)
                } else {
                  coord_status <- tr("import.utm.failed")
                }
              } else {
                coord_status <- tr("import.coord.unknown")
              }
            }
          }
          
          mapping$meta <- data.frame(
            site = sites, group = group, lon = as.numeric(lon), lat = as.numeric(lat),
            stringsAsFactors = FALSE
          )
          mapping$coord_info <- cinfo
          mapping$raw <- list(site = sites, group = group, coord1 = coord1, coord2 = coord2)

          used <- c(1L)
          if (!is.null(input$group_col) && input$group_col != "none") used <- c(used, as.numeric(input$group_col))
          if (!is.null(input$x_coord_col) && input$x_coord_col != "none") used <- c(used, as.numeric(input$x_coord_col))
          if (!is.null(input$y_coord_col) && input$y_coord_col != "none") used <- c(used, as.numeric(input$y_coord_col))
          used <- unique(used[!is.na(used) & used <= ncol(df0)])
          type_cols <- setdiff(seq_len(ncol(df0)), used)
          
          if (length(type_cols) > 0) {
            df <- data.frame(Entity = sites, df0[, type_cols, drop = FALSE], check.names = FALSE)
            if (ncol(df) > 1) for (jj in 2:ncol(df)) df[[jj]] <- coerce_num(df[[jj]])
          } else {
            df <- data.frame(Entity = sites, check.names = FALSE)
          }
          
          if (nrow(df) == 0 || ncol(df) < 2) stop(tr("import.error.minimum"))
          colnames(df)[1] <- "Entity"
          
          df <- df[rowSums(is.na(df)) < ncol(df), ]
          df <- df[, c(TRUE, colSums(!is.na(df[, -1, drop = FALSE])) > 0), drop = FALSE]
          if (ncol(df) > 1) df <- df[rowSums(df[, -1, drop = FALSE], na.rm = TRUE) > 0, ]
          
          if (nrow(df) < 2 || ncol(df) < 2) stop(tr("import.error.cleaned"))

          type_groups <- NULL
          if (ncol(df) > 1) {
            type_names <- colnames(df)[-1]  # Without "Entity" column
            # Try different separators
            if (any(grepl("_", type_names))) {
              # Format: "Group_Type"
              parts <- strsplit(type_names, "_", fixed = TRUE)
              has_group <- sapply(parts, length) >= 2
              if (any(has_group)) {
                groups <- sapply(parts[has_group], function(x) x[1])
                types <- sapply(parts[has_group], function(x) paste(x[-1], collapse = "_"))
                type_groups <- setNames(groups, type_names[has_group])
              }
            } else if (any(grepl(":", type_names, fixed = TRUE))) {
              # Format: "Group:Type"
              parts <- strsplit(type_names, ":", fixed = TRUE)
              has_group <- sapply(parts, length) >= 2
              if (any(has_group)) {
                groups <- sapply(parts[has_group], function(x) x[1])
                types <- sapply(parts[has_group], function(x) paste(x[-1], collapse = ":"))
                type_groups <- setNames(groups, type_names[has_group])
              }
            }
          }

          mapping$type_groups <- type_groups

          file_status(sprintf(tr("import.status.loaded"), nrow(df), ncol(df)-1, coord_status))
          df

        }, error = function(e) {
          file_status(paste(tr("import.error.import"), e$message))
          NULL
        })
        
      } else if (exists("DEFAULT_DATASET") && !is.null(DEFAULT_DATASET)) {
        df0 <- as.data.frame(DEFAULT_DATASET)
        if (nrow(df0) == 0 || ncol(df0) < 1) return(NULL)

        inventare <- as.character(df0[[1]])
        group <- if (ncol(df0) >= 41) as.character(df0[[41]]) else NA_character_
        
        coord1 <- coord2 <- NA
        lon <- lat <- rep(NA_real_, length(inventare))
        cinfo <- list(type = "utm", order = "en")
        coord_status <- tr("import.coord.none")
        
        if (ncol(df0) >= 43) {
          coord1 <- df0[[42]]
          coord2 <- df0[[43]]
          
          conv <- tryCatch(utm_to_wgs84(coord1, coord2, 32, "N"), error = function(e) NULL)
          
          if (!is.null(conv)) {
            lon <- conv$lon; lat <- conv$lat
            valid_count <- sum(!is.na(lon) & !is.na(lat))
            coord_status <- sprintf(tr("import.coord.utm"), valid_count)
          }
        }
        
        mapping$meta <- data.frame(
          site = inventare, group = group, lon = as.numeric(lon), lat = as.numeric(lat),
          stringsAsFactors = FALSE
        )
        mapping$coord_info <- cinfo
        mapping$raw <- list(site = inventare, group = group, coord1 = coord1, coord2 = coord2)

        mapping$type_groups <- NULL

        all_cols <- colnames(df0)
        metadata_cols <- c("Entity", "id", "catalogue_nr", "feature_nr", "phase", 
                          "x", "y", "x_utm32n", "y_utm32n")
        type_col_names <- setdiff(all_cols, metadata_cols)
        
        if (length(type_col_names) > 0) {
          df <- df0[, c("Entity", type_col_names), drop = FALSE]
          if (ncol(df) > 1) for (jj in 2:ncol(df)) df[[jj]] <- coerce_num(df[[jj]])
        } else {
          df <- data.frame(Entity = inventare, check.names = FALSE)
        }
        
        if (nrow(df) == 0 || ncol(df) < 2) return(NULL)
        colnames(df)[1] <- "Entity"
        
        df <- df[rowSums(is.na(df)) < ncol(df), ]
        df <- df[, c(TRUE, colSums(!is.na(df[, -1, drop = FALSE])) > 0), drop = FALSE]
        if (ncol(df) > 1) df <- df[rowSums(df[, -1, drop = FALSE], na.rm = TRUE) > 0, ]
        
        if (nrow(df) < 2 || ncol(df) < 2) return(NULL)
        
        file_status(sprintf(tr("import.status.loaded"), nrow(df), ncol(df)-1, coord_status))
        df
        
      } else {
        return(NULL)
      }
    })
    
    output$utm_detected <- reactive({
      !is.null(mapping$coord_info) && !is.na(mapping$coord_info$type) && 
        identical(mapping$coord_info$type, "utm")
    })
    outputOptions(output, "utm_detected", suspendWhenHidden = FALSE)
    
    detection_status <- reactive({
      mm <- mapping$meta
      if (is.null(mm)) return(span(tr("import.no.data"), style="color:#c0392b;"))

      has_group <- "group" %in% names(mm) && any(!is.na(mm$group))
      has_coords <- all(c("lon","lat") %in% names(mm)) && any(!is.na(mm$lon) & !is.na(mm$lat))

      group_text <- if (has_group) {
        unique_count <- length(unique(mm$group[!is.na(mm$group)]))
        sprintf(tr("import.groups.count"), unique_count)
      } else tr("import.no.groups")

      coord_text <- if (has_coords) {
        valid_count <- sum(!is.na(mm$lon) & !is.na(mm$lat))
        coord_type <- mapping$coord_info$type %||% "unknown"
        sprintf(tr("import.coords.count"), valid_count, coord_type)
      } else tr("import.no.coords")

      span(paste(group_text, coord_text, sep = " | "))
    })
    
    output$file_status_ui <- renderUI({ 
      div(class="small text-muted", file_status()) 
    })
    
    # Reactive value: is the example dataset active (no user upload)?
    is_example_data <- reactive({
      is.null(input$file) && exists("DEFAULT_DATASET") && !is.null(DEFAULT_DATASET)
    })

    return(list(
      data_raw = data_raw,
      mapping_meta = reactive({ mapping$meta }),
      mapping_coord_info = reactive({ mapping$coord_info }),
      mapping_raw = reactive({ mapping$raw }),
      mapping_type_groups = reactive({ mapping$type_groups }),
      file_status = file_status,
      detection_status = detection_status,
      is_example_data = is_example_data
    ))
  })
}

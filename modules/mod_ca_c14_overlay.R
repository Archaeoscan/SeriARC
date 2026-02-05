# mod_ca_c14_overlay.R
# 14C Overlay Module for Correspondence Analysis
# Handles display of calibrated and modelled 14C dates on CA plots
# Author: Daniel Meixner (University of Regensburg)

# ===================== MODULE UI =====================

#' UI for 14C Overlay Panel (to be embedded in CA tab)
#'
#' This UI is designed to work WITH the existing CA module.
#' The main toggle checkbox uses the GLOBAL ID "show_c14_overlay" (no namespace)
#' so that the existing CA module can read it directly.
#' Extended options use the module namespace.
#'
#' @param id Namespace ID
#' @param tr Translation function
#' @return tagList with overlay controls
mod_ca_c14_overlay_ui <- function(id, tr = function(x) x) {

  ns <- NS(id)

  # Use uiOutput for server-side rendering (more reliable than conditionalPanel for modules)
  uiOutput(ns("overlay_panel_ui"))
}

# ===================== MODULE SERVER =====================

#' Server for 14C Overlay Module
#'
#' @param id Namespace ID
#' @param c14_calibrated_reactive Reactive containing calibrated 14C data
#' @param oxcal_results_reactive Reactive containing OxCal modelled results (optional)
#' @param plot_data_reactive Reactive containing CA plot data (x, y, label, type)
#' @param parent_input Parent session's input (to access show_c14_overlay)
#' @param tr Translation function
#' @return List with reactive overlay data and render function
mod_ca_c14_overlay_server <- function(id,
                                       c14_calibrated_reactive,
                                       oxcal_results_reactive = NULL,
                                       plot_data_reactive = NULL,
                                       parent_input = NULL,
                                       tr = function(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Source helper functions
    source("modules/mod_c14_functions.R", local = TRUE)

    # Helper to check if overlay is enabled (from parent input)
    is_overlay_enabled <- reactive({
      if (!is.null(parent_input)) {
        isTRUE(parent_input$show_c14_overlay)
      } else {
        FALSE
      }
    })

    # ===================== REACTIVE: DATA AVAILABILITY =====================

    # Check if unmodelled 14C data is available
    c14_data_available <- reactive({
      c14_data <- tryCatch(c14_calibrated_reactive(), error = function(e) NULL)
      !is.null(c14_data) && nrow(c14_data) > 0
    })

    # Check if modelled (OxCal) data is available
    oxcal_available <- reactive({
      if (is.null(oxcal_results_reactive)) return(FALSE)
      oxcal_data <- tryCatch(oxcal_results_reactive(), error = function(e) NULL)
      !is.null(oxcal_data) && !is.null(oxcal_data$tidy) && nrow(oxcal_data$tidy) > 0
    })

    # ===================== RENDER UI =====================

    output$overlay_panel_ui <- renderUI({
      has_c14 <- c14_data_available()
      has_oxcal <- oxcal_available()

      # Show overlay panel if EITHER unmodelled OR modelled data is available
      if (!has_c14 && !has_oxcal) {
        # No 14C data at all - show hint
        div(class = "alert alert-secondary", style = "font-size: 0.9em; padding: 10px; margin: 0;",
          tags$em(style = "color: #6c757d;",
            "📊 ", tr("ca.panel.overlay.hint")
          )
        )
      } else {
        # 14C data available - show full panel
        tagList(
          # Main toggle - IMPORTANT: Uses GLOBAL ID for CA module compatibility
          checkboxInput("show_c14_overlay", tr("c14_overlay.show"), value = FALSE),

          conditionalPanel(
            condition = "input.show_c14_overlay",  # Global ID

            # Data source selection - GLOBAL ID for CA module compatibility
            # Auto-select based on what's available
            radioButtons("c14_data_source", tr("c14_overlay.source"),
              choiceNames = c(tr("c14_overlay.source.unmodelled"), tr("c14_overlay.source.modelled")),
              choiceValues = c("unmodelled", "modelled"),
              # Select "modelled" if only OxCal data available, otherwise "unmodelled"
              selected = if (!has_c14 && has_oxcal) "modelled" else "unmodelled",
              inline = TRUE
            ),

            # Hint if only one data source is available
            if (!has_c14) {
              div(class = "text-muted small", style = "margin-top: -10px; margin-bottom: 10px;",
                tags$em(tr("c14_overlay.only_modelled"))
              )
            } else if (!has_oxcal) {
              div(class = "text-muted small", style = "margin-top: -10px; margin-bottom: 10px;",
                tags$em(tr("c14_overlay.no_oxcal"))
              )
            },

            # Sum-Posterior Info Box
            div(class = "alert alert-info", style = "font-size: 0.85em; padding: 8px; margin: 10px 0;",
              tags$span(
                style = "cursor: help;",
                title = tr("c14_overlay.spd.tooltip"),
                strong("ℹ️ ", tr("c14_overlay.spd.title"))
              ),
              br(),
              tags$small(tr("c14_overlay.spd.desc"))
            ),

            tags$hr(style = "margin: 10px 0;"),

            # Display options
            h6(tr("c14_overlay.display_options")),

            # Label content - GLOBAL IDs for CA module access
            checkboxInput("c14_show_n", tr("c14_overlay.show_n"), value = TRUE),
            checkboxInput("c14_show_interval", tr("c14_overlay.show_interval"), value = FALSE),
            checkboxInput("c14_show_labids", tr("c14_overlay.show_labids"), value = FALSE),

            # Label size slider - GLOBAL ID for CA module access
            sliderInput("c14_label_size", tr("c14_overlay.label_size"),
              min = 6, max = 14, value = 9, step = 1
            ),

            # Positioning options - GLOBAL IDs for CA module access
            sliderInput("c14_label_offset", tr("c14_overlay.offset"),
              min = 0.2, max = 1.0, value = 0.4, step = 0.1
            ),
            sliderInput("c14_min_separation", tr("c14_overlay.separation"),
              min = 0.1, max = 0.5, value = 0.2, step = 0.05
            ),

            # Advanced options (collapsible)
            tags$details(
              tags$summary(tr("c14_overlay.advanced"), style = "cursor: pointer; font-size: 0.9em;"),
              div(style = "padding-top: 10px;",
                sliderInput("c14_iterations", tr("c14_overlay.iterations"),
                  min = 5, max = 30, value = 15, step = 5
                ),
                selectInput("c14_label_style", tr("c14_overlay.style"),
                  choices = c(
                    "compact" = "compact",
                    "detailed" = "detailed",
                    "minimal" = "minimal"
                  ),
                  selected = "compact"
                )
              )
            )
          )
        )
      }
    })

    # ===================== REACTIVE: AGGREGATED 14C DATA =====================

    # Get aggregated 14C data based on selected source
    c14_aggregated <- reactive({
      req(is_overlay_enabled())

      data_source <- parent_input$c14_data_source %||% "unmodelled"

      if (data_source == "modelled" && !is.null(oxcal_results_reactive)) {
        # Use modelled data from OxCal
        oxcal_data <- tryCatch(oxcal_results_reactive(), error = function(e) NULL)
        if (!is.null(oxcal_data) && !is.null(oxcal_data$tidy)) {
          aggregate_oxcal_posteriors(oxcal_data$tidy)
        } else if (c14_data_available()) {
          # Fallback to unmodelled only if available
          aggregate_unmodelled_c14(c14_calibrated_reactive())
        } else {
          NULL
        }
      } else if (c14_data_available()) {
        # Use unmodelled (calibrated) data
        aggregate_unmodelled_c14(c14_calibrated_reactive())
      } else if (!is.null(oxcal_results_reactive)) {
        # No unmodelled data, try OxCal as fallback
        oxcal_data <- tryCatch(oxcal_results_reactive(), error = function(e) NULL)
        if (!is.null(oxcal_data) && !is.null(oxcal_data$tidy)) {
          aggregate_oxcal_posteriors(oxcal_data$tidy)
        } else {
          NULL
        }
      } else {
        NULL
      }
    })

    # ===================== HELPER: AGGREGATE UNMODELLED DATA =====================

    aggregate_unmodelled_c14 <- function(c14_data) {
      if (is.null(c14_data) || nrow(c14_data) == 0) return(NULL)

      # Use summarise function which now includes lab_ids collection
      site_summary <- summarise_c14_by_site(c14_data)

      # Ensure lab_ids column exists (fallback for older code paths)
      if (!"lab_ids" %in% names(site_summary)) {
        site_summary$lab_ids <- ""
      }

      # Add formatted labels
      site_summary$label <- sapply(1:nrow(site_summary), function(i) {
        format_c14_label(
          site_summary$site_median_calBP[i],
          site_summary$hpd68_intervals[[i]],
          site_summary$n_dates[i],
          site_name = site_summary$Site[i],
          lab_ids = site_summary$lab_ids[i],
          style = parent_input$c14_label_style %||% "compact",
          show_n = parent_input$c14_show_n %||% TRUE,
          show_interval = parent_input$c14_show_interval %||% FALSE,
          show_labids = parent_input$c14_show_labids %||% FALSE
        )
      })

      site_summary$source <- "unmodelled"
      site_summary
    }

    # ===================== HELPER: AGGREGATE MODELLED DATA =====================

    aggregate_oxcal_posteriors <- function(tidy_df) {
      if (is.null(tidy_df) || nrow(tidy_df) == 0) return(NULL)

      # Filter to R_Date entries (individual dates, not boundaries)
      # OxCal names can be: "R_Date Sitename_1", "R_Date:Site:Lab", etc.
      dates_df <- tidy_df[grepl("R_Date", tidy_df$name, ignore.case = TRUE), ]

      if (nrow(dates_df) == 0) {
        # Try to extract site information from all entries (excluding structural elements)
        dates_df <- tidy_df[!grepl("Boundary|Phase|Sequence|Span|Sum|Order", tidy_df$name, ignore.case = TRUE), ]
      }

      if (nrow(dates_df) == 0) return(NULL)

      # Extract site name from OxCal name
      # Format from SeriARC: "R_Date(Site_LabNr)" where Site may contain underscores
      # The LabNr is typically at the end after the last underscore (e.g., "KIA-17793")
      dates_df$Site <- sapply(dates_df$name, function(n) {
        # Remove "R_Date" prefix (with optional space, colon, or parenthesis)
        cleaned <- gsub("^R_Date[\\s:(]*", "", n, perl = TRUE)
        # Remove trailing parenthesis and semicolon if present
        cleaned <- gsub("[\\);]+$", "", cleaned)

        # SeriARC format: "SiteName_LabNr" (e.g., "87 Ergolding LA 26_KIA-17793")
        # LabNr typically contains a hyphen (e.g., KIA-17793, ETH-12345)
        # Strategy: Find the last underscore followed by a pattern like XXX-NNNNN
        if (grepl("_[A-Za-z]+-\\d+$", cleaned) || grepl("_[A-Za-z]+\\d+$", cleaned)) {
          # Has LabID pattern at end - remove it to get site name
          site_name <- sub("_[A-Za-z]+-?\\d+$", "", cleaned)
          return(trimws(site_name))
        }

        # Fallback: Try to detect numeric suffix (Site_1, Site_2)
        if (grepl("_\\d+$", cleaned)) {
          site_name <- sub("_\\d+$", "", cleaned)
          return(trimws(site_name))
        }

        # No pattern found - return as is
        trimws(cleaned)
      })

      # Extract LabID (if present in name)
      dates_df$LabID <- sapply(dates_df$name, function(n) {
        # Remove "R_Date" prefix and clean
        cleaned <- gsub("^R_Date[\\s(]*", "", n, perl = TRUE)
        cleaned <- gsub("[\\);]+$", "", cleaned)

        # SeriARC format: "SiteName_LabNr" (e.g., "87 Ergolding_KIA-17793")
        # Try to extract LabID pattern (XXX-NNNNN or XXXNNNNN)
        if (grepl("_([A-Za-z]+-?\\d+)$", cleaned)) {
          match <- regmatches(cleaned, regexpr("_([A-Za-z]+-?\\d+)$", cleaned))
          if (length(match) > 0) {
            return(gsub("^_", "", match))
          }
        }

        # Try format "Site_N" - extract sequence number
        match <- regmatches(cleaned, regexpr("_\\d+$", cleaned))
        if (length(match) > 0) {
          return(gsub("^_", "", match))
        }

        ""
      })

      # Aggregate by site
      site_summary <- dates_df %>%
        dplyr::group_by(Site) %>%
        dplyr::summarise(
          site_median_calBP = median(median, na.rm = TRUE),
          from_68 = min(from_68, na.rm = TRUE),
          to_68 = max(to_68, na.rm = TRUE),
          from_95 = min(from_95, na.rm = TRUE),
          to_95 = max(to_95, na.rm = TRUE),
          n_dates = dplyr::n(),
          lab_ids = paste(LabID[LabID != ""], collapse = ", "),
          .groups = "drop"
        )

      # Convert to cal BC for labels (OxCal outputs are typically in cal BP or BC)
      site_summary$label <- sapply(1:nrow(site_summary), function(i) {
        format_modelled_label(
          site_summary$site_median_calBP[i],
          site_summary$from_68[i],
          site_summary$to_68[i],
          site_summary$n_dates[i],
          site_name = site_summary$Site[i],
          lab_ids = site_summary$lab_ids[i],
          style = parent_input$c14_label_style %||% "compact",
          show_n = parent_input$c14_show_n %||% TRUE,
          show_interval = parent_input$c14_show_interval %||% FALSE,
          show_labids = parent_input$c14_show_labids %||% FALSE
        )
      })

      site_summary$source <- "modelled"
      site_summary
    }

    # ===================== LABEL FORMATTING =====================

    format_c14_label <- function(median_bp, hpd68_df, n_dates,
                                  site_name = NULL,
                                  lab_ids = NULL,
                                  style = "compact", show_n = TRUE, show_interval = FALSE,
                                  show_labids = FALSE) {
      # Convert median to cal BC/AD
      median_calbc <- format_point_calbc(median_bp)

      # Build interval string if requested
      interval_str <- NULL
      if (show_interval && !is.null(hpd68_df)) {
        # Handle both data.frame and list cases
        if (is.data.frame(hpd68_df) && nrow(hpd68_df) > 0 && !all(is.na(hpd68_df$low_bp))) {
          interval_str <- format_simple_hpd_calbc(hpd68_df)
        } else if (is.list(hpd68_df) && !is.data.frame(hpd68_df)) {
          # Try to extract from list
          tryCatch({
            if (!is.null(hpd68_df[[1]]) && is.data.frame(hpd68_df[[1]])) {
              interval_str <- format_simple_hpd_calbc(hpd68_df[[1]])
            }
          }, error = function(e) NULL)
        }
      }

      # Prepare LabID string if requested
      lab_id_str <- NULL
      if (show_labids && !is.null(lab_ids) && !is.na(lab_ids) && nchar(as.character(lab_ids)) > 0) {
        lab_id_str <- as.character(lab_ids)
        # Truncate very long lists
        if (nchar(lab_id_str) > 25) {
          lab_id_str <- paste0(substr(lab_id_str, 1, 22), "...")
        }
      }

      # Build label based on style
      switch(style,
        "compact" = {
          # Compact: "3500 cal BC (n=3)" or "3500 cal BC [3600-3400 cal BC] (n=3) [LabID]"
          label_parts <- c(median_calbc)
          if (!is.null(interval_str) && !is.na(interval_str)) {
            label_parts <- c(label_parts, paste0("[", interval_str, "]"))
          }
          if (show_n && n_dates > 0) {
            label_parts <- c(label_parts, paste0("(n=", n_dates, ")"))
          }
          if (!is.null(lab_id_str)) {
            label_parts <- c(label_parts, paste0("[", lab_id_str, "]"))
          }
          paste(label_parts, collapse = " ")
        },
        "detailed" = {
          # Detailed: All information on multiple lines
          # Line 1: Site name (if available)
          # Line 2: Median
          # Line 3: 68% interval (if show_interval)
          # Line 4: n dates + LabIDs
          lines <- c()

          # Add site name as first line in detailed mode
          if (!is.null(site_name) && !is.na(site_name) && nchar(as.character(site_name)) > 0) {
            site_str <- as.character(site_name)
            # Truncate very long site names
            if (nchar(site_str) > 35) {
              site_str <- paste0(substr(site_str, 1, 32), "...")
            }
            lines <- c(lines, site_str)
          }

          lines <- c(lines, median_calbc)

          if (!is.null(interval_str) && !is.na(interval_str)) {
            lines <- c(lines, paste0("68%: ", interval_str))
          }

          info_parts <- c()
          if (show_n && n_dates > 0) {
            info_parts <- c(info_parts, paste0("n=", n_dates))
          }
          if (!is.null(lab_ids) && !is.na(lab_ids) && nchar(as.character(lab_ids)) > 0) {
            # Truncate very long LabID lists
            lab_ids_str <- as.character(lab_ids)
            if (nchar(lab_ids_str) > 30) {
              lab_ids_str <- paste0(substr(lab_ids_str, 1, 27), "...")
            }
            info_parts <- c(info_parts, lab_ids_str)
          }
          if (length(info_parts) > 0) {
            lines <- c(lines, paste(info_parts, collapse = " | "))
          }

          paste(lines, collapse = "\n")
        },
        "minimal" = {
          # Minimal: just the median
          median_calbc
        },
        # Default fallback
        {
          label_parts <- c(median_calbc)
          if (!is.null(interval_str) && !is.na(interval_str)) {
            label_parts <- c(label_parts, paste0("[", interval_str, "]"))
          }
          if (show_n && n_dates > 0) {
            label_parts <- c(label_parts, paste0("(n=", n_dates, ")"))
          }
          if (!is.null(lab_id_str)) {
            label_parts <- c(label_parts, paste0("[", lab_id_str, "]"))
          }
          paste(label_parts, collapse = " ")
        }
      )
    }

    format_modelled_label <- function(median_bp, from_68, to_68, n_dates,
                                       site_name = NULL,
                                       lab_ids = NULL,
                                       style = "compact", show_n = TRUE, show_interval = FALSE,
                                       show_labids = FALSE) {
      # For OxCal data, values might already be in BC (negative)
      # Handle both cases
      if (median_bp > 0) {
        # Assume cal BP, convert to BC/AD
        median_calbc <- format_point_calbc(median_bp)
      } else {
        # Already in BC (negative) or AD (positive near 0)
        median_calbc <- format_year_label(median_bp)
      }

      # Build interval string
      interval_str <- NULL
      if (show_interval && !is.na(from_68) && !is.na(to_68)) {
        from_str <- if (from_68 > 0) format_point_calbc(from_68) else format_year_label(from_68)
        to_str <- if (to_68 > 0) format_point_calbc(to_68) else format_year_label(to_68)
        interval_str <- paste0(from_str, " - ", to_str)
      }

      # Prepare LabID string if requested
      lab_id_str <- NULL
      if (show_labids && !is.null(lab_ids) && !is.na(lab_ids) && nchar(as.character(lab_ids)) > 0) {
        lab_id_str <- as.character(lab_ids)
        if (nchar(lab_id_str) > 25) {
          lab_id_str <- paste0(substr(lab_id_str, 1, 22), "...")
        }
      }

      # Marker for modelled data
      modelled_marker <- "\u2713"  # Checkmark

      switch(style,
        "compact" = {
          label_parts <- c(modelled_marker, median_calbc)
          if (!is.null(interval_str)) {
            label_parts <- c(label_parts, paste0("[", interval_str, "]"))
          }
          if (show_n && n_dates > 0) {
            label_parts <- c(label_parts, paste0("(n=", n_dates, ")"))
          }
          if (!is.null(lab_id_str)) {
            label_parts <- c(label_parts, paste0("[", lab_id_str, "]"))
          }
          paste(label_parts, collapse = " ")
        },
        "detailed" = {
          lines <- c()

          # Add site name as first line in detailed mode
          if (!is.null(site_name) && !is.na(site_name) && nchar(as.character(site_name)) > 0) {
            site_str <- as.character(site_name)
            # Truncate very long site names
            if (nchar(site_str) > 35) {
              site_str <- paste0(substr(site_str, 1, 32), "...")
            }
            lines <- c(lines, site_str)
          }

          lines <- c(lines, paste(modelled_marker, median_calbc))
          if (!is.null(interval_str)) {
            lines <- c(lines, paste0("68%: ", interval_str))
          }
          info_parts <- c()
          if (show_n && n_dates > 0) {
            info_parts <- c(info_parts, paste0("n=", n_dates))
          }
          # In detailed mode, always show LabIDs if available (regardless of checkbox)
          if (!is.null(lab_ids) && !is.na(lab_ids) && nchar(as.character(lab_ids)) > 0) {
            lab_ids_str <- as.character(lab_ids)
            if (nchar(lab_ids_str) > 30) {
              lab_ids_str <- paste0(substr(lab_ids_str, 1, 27), "...")
            }
            info_parts <- c(info_parts, lab_ids_str)
          }
          if (length(info_parts) > 0) {
            lines <- c(lines, paste(info_parts, collapse = " | "))
          }
          paste(lines, collapse = "\n")
        },
        "minimal" = {
          paste(modelled_marker, median_calbc)
        },
        {
          # Default
          label_parts <- c(modelled_marker, median_calbc)
          if (!is.null(interval_str)) {
            label_parts <- c(label_parts, paste0("[", interval_str, "]"))
          }
          if (show_n && n_dates > 0) {
            label_parts <- c(label_parts, paste0("(n=", n_dates, ")"))
          }
          if (!is.null(lab_id_str)) {
            label_parts <- c(label_parts, paste0("[", lab_id_str, "]"))
          }
          paste(label_parts, collapse = " ")
        }
      )
    }

    # ===================== FORCE-DIRECTED LABEL POSITIONING =====================

    #' Calculate label positions using force-directed algorithm
    #'
    #' @param site_x X coordinates of sites
    #' @param site_y Y coordinates of sites
    #' @param base_offset Base distance from point to label
    #' @param min_separation Minimum distance between labels
    #' @param iterations Number of force iterations
    #' @param repulsion Repulsion strength between labels
    #' @param spring Spring constant pulling label back to point
    #' @return List with x, y coordinates for labels and leader line data
    calculate_force_directed_positions <- function(
        site_x, site_y,
        base_offset = 0.4,
        min_separation = 0.2,
        iterations = 15,
        repulsion = 0.5,
        spring = 0.1
    ) {
      n_sites <- length(site_x)
      if (n_sites == 0) return(list(x = numeric(0), y = numeric(0), lengths = numeric(0)))

      # Calculate plot range for scaling
      x_range <- diff(range(site_x, na.rm = TRUE))
      y_range <- diff(range(site_y, na.rm = TRUE))
      scale_factor <- max(x_range, y_range, 1)

      # Normalize separation parameters to plot scale
      sep_scaled <- min_separation * scale_factor / 2
      offset_scaled <- base_offset * scale_factor / 2

      # Calculate center of mass
      center_x <- mean(site_x, na.rm = TRUE)
      center_y <- mean(site_y, na.rm = TRUE)

      # Initialize label positions: radially away from center
      label_x <- numeric(n_sites)
      label_y <- numeric(n_sites)

      for (i in 1:n_sites) {
        dx <- site_x[i] - center_x
        dy <- site_y[i] - center_y
        dist_from_center <- sqrt(dx^2 + dy^2)

        if (dist_from_center > 1e-6) {
          # Direction away from center
          unit_x <- dx / dist_from_center
          unit_y <- dy / dist_from_center
        } else {
          # Random direction if at center
          angle <- runif(1, 0, 2 * pi)
          unit_x <- cos(angle)
          unit_y <- sin(angle)
        }

        label_x[i] <- site_x[i] + unit_x * offset_scaled
        label_y[i] <- site_y[i] + unit_y * offset_scaled
      }

      # Force-directed iterations
      if (n_sites > 1) {
        for (iter in 1:iterations) {
          # Store forces for each label
          force_x <- rep(0, n_sites)
          force_y <- rep(0, n_sites)

          # Repulsion between labels
          for (i in 1:(n_sites - 1)) {
            for (j in (i + 1):n_sites) {
              dx <- label_x[j] - label_x[i]
              dy <- label_y[j] - label_y[i]
              dist <- sqrt(dx^2 + dy^2)

              if (dist < sep_scaled && dist > 1e-6) {
                # Repulsion force (inverse square law)
                force_magnitude <- repulsion * (sep_scaled - dist) / dist

                # Normalize direction
                nx <- dx / dist
                ny <- dy / dist

                # Apply opposing forces
                force_x[i] <- force_x[i] - nx * force_magnitude
                force_y[i] <- force_y[i] - ny * force_magnitude
                force_x[j] <- force_x[j] + nx * force_magnitude
                force_y[j] <- force_y[j] + ny * force_magnitude
              }
            }
          }

          # Spring force pulling label back towards point (but not too close)
          for (i in 1:n_sites) {
            dx <- site_x[i] - label_x[i]
            dy <- site_y[i] - label_y[i]
            dist <- sqrt(dx^2 + dy^2)

            if (dist > offset_scaled * 2) {
              # Pull back if too far
              force_x[i] <- force_x[i] + dx * spring
              force_y[i] <- force_y[i] + dy * spring
            } else if (dist < offset_scaled * 0.5) {
              # Push away if too close
              if (dist > 1e-6) {
                force_x[i] <- force_x[i] - dx * spring * 2
                force_y[i] <- force_y[i] - dy * spring * 2
              }
            }
          }

          # Apply forces with damping
          damping <- 0.8 * (1 - iter / iterations)  # Decrease over iterations
          label_x <- label_x + force_x * damping
          label_y <- label_y + force_y * damping
        }
      }

      # Calculate final leader line lengths
      lengths <- sqrt((label_x - site_x)^2 + (label_y - site_y)^2)

      list(
        x = label_x,
        y = label_y,
        lengths = lengths,
        min_length = min(lengths),
        max_length = max(lengths)
      )
    }

    # ===================== OVERLAY DATA FOR CA PLOT =====================

    #' Get overlay data ready for rendering on CA plot
    overlay_data <- reactive({
      req(is_overlay_enabled())

      c14_agg <- c14_aggregated()
      if (is.null(c14_agg) || nrow(c14_agg) == 0) return(NULL)

      # Get plot data if available (to match sites)
      plot_data <- if (!is.null(plot_data_reactive)) {
        tryCatch(plot_data_reactive(), error = function(e) NULL)
      } else NULL

      # If we have plot data, match sites
      if (!is.null(plot_data)) {
        # Find sites that exist in both CA and 14C data
        ca_sites <- plot_data[plot_data$type == "Site", ]
        matched_sites <- c14_agg$Site[c14_agg$Site %in% ca_sites$label]

        if (length(matched_sites) == 0) return(NULL)

        # Get coordinates for matched sites
        site_coords <- ca_sites[ca_sites$label %in% matched_sites, c("label", "x", "y")]
        c14_matched <- c14_agg[c14_agg$Site %in% matched_sites, ]

        # Merge coordinates with 14C data
        overlay <- merge(c14_matched, site_coords, by.x = "Site", by.y = "label")

        # Calculate label positions
        label_pos <- calculate_force_directed_positions(
          overlay$x, overlay$y,
          base_offset = parent_input$c14_label_offset %||% 0.4,
          min_separation = parent_input$c14_min_separation %||% 0.2,
          iterations = parent_input$c14_iterations %||% 15
        )

        overlay$label_x <- label_pos$x
        overlay$label_y <- label_pos$y
        overlay$leader_length <- label_pos$lengths

        overlay
      } else {
        # Return aggregated data without coordinates
        c14_agg
      }
    })

    # ===================== RENDER OVERLAY ON PLOTLY =====================

    #' Add 14C overlay to existing Plotly object
    #'
    #' @param p Existing plotly object
    #' @return Modified plotly object with 14C overlay
    render_overlay <- function(p) {
      if (!is_overlay_enabled()) return(p)

      overlay <- overlay_data()
      if (is.null(overlay) || nrow(overlay) == 0) return(p)

      # Determine colors based on data source
      if (overlay$source[1] == "modelled") {
        line_color <- "#27ae60"  # Green for modelled
        label_color <- "#1e8449"
        border_color <- "#27ae60"
      } else {
        line_color <- "#34495e"  # Dark gray for unmodelled
        label_color <- "#2c3e50"
        border_color <- "#bdc3c7"
      }

      # Add leader lines
      for (i in 1:nrow(overlay)) {
        p <- p %>% plotly::add_segments(
          x = overlay$x[i], y = overlay$y[i],
          xend = overlay$label_x[i], yend = overlay$label_y[i],
          line = list(color = line_color, width = 1.5),
          showlegend = FALSE,
          hoverinfo = "skip"
        )
      }

      # Add labels as scatter with text mode for hover support
      hover_text <- sapply(1:nrow(overlay), function(i) {
        # Determine aggregation method description
        agg_method <- if (!is.null(overlay$aggregation_method) && !is.na(overlay$aggregation_method[i])) {
          switch(overlay$aggregation_method[i],
            "sum_posterior" = "Sum-Posterior (SPD)",
            "median" = "Median",
            "single" = "Single date",
            overlay$aggregation_method[i]
          )
        } else {
          "Unknown"
        }

        source_desc <- if (overlay$source[i] == "modelled") {
          "OxCal modelled"
        } else {
          paste0("Calibrated (", agg_method, ")")
        }

        # Get LabIDs if available
        lab_ids_str <- ""
        if ("lab_ids" %in% names(overlay)) {
          lab_id_val <- overlay$lab_ids[i]
          if (!is.null(lab_id_val) && !is.na(lab_id_val) && nchar(as.character(lab_id_val)) > 0) {
            lab_ids_str <- paste0("<br>LabIDs: ", lab_id_val)
          }
        }

        # Get 68% interval if available
        interval_str <- ""
        if ("hpd68_intervals" %in% names(overlay) && !is.null(overlay$hpd68_intervals[[i]])) {
          hpd68 <- overlay$hpd68_intervals[[i]]
          if (is.data.frame(hpd68) && nrow(hpd68) > 0 && !all(is.na(hpd68$low_bp))) {
            interval_formatted <- format_simple_hpd_calbc(hpd68)
            if (!is.na(interval_formatted)) {
              interval_str <- paste0("<br>68% HPD: ", interval_formatted)
            }
          }
        }

        sprintf(
          "<b>%s</b><br>%s<br>Dates: %d<br>Source: %s%s%s",
          overlay$Site[i],
          overlay$label[i],
          overlay$n_dates[i],
          source_desc,
          interval_str,
          lab_ids_str
        )
      })

      # Get label size from settings (default 9)
      label_size <- parent_input$c14_label_size %||% 9

      p <- p %>% plotly::add_annotations(
        x = overlay$label_x,
        y = overlay$label_y,
        text = overlay$label,
        font = list(size = label_size, color = label_color, family = "Arial, sans-serif"),
        bgcolor = "rgba(255, 255, 255, 0.9)",
        bordercolor = border_color,
        borderwidth = 1,
        borderpad = 4,
        showarrow = FALSE,
        xanchor = "center",
        yanchor = "middle",
        hovertext = hover_text
      )

      p
    }

    # ===================== RETURN MODULE INTERFACE =====================

    list(
      # Reactive: Is overlay enabled?
      is_enabled = is_overlay_enabled,

      # Reactive: Get overlay data
      overlay_data = overlay_data,

      # Reactive: Get aggregated 14C data
      c14_data = c14_aggregated,

      # Function: Render overlay on Plotly
      render_overlay = render_overlay,

      # Reactive: Current data source
      data_source = reactive({ parent_input$c14_data_source %||% "unmodelled" }),

      # Reactive: Label positioning parameters
      positioning = reactive({
        list(
          offset = parent_input$c14_label_offset %||% 0.4,
          separation = parent_input$c14_min_separation %||% 0.2,
          iterations = parent_input$c14_iterations %||% 15
        )
      })
    )
  })
}

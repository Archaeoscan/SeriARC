# modules/mod_chronology_curve.R
# Central Prior interface for OxCal Sequence modeling
# Focus on archaeologically established methods

mod_chronology_curve_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  
  # Tooltip-Helper
  tooltip_icon <- function(text) {
    tags$span("ℹ️", title = text, style = "cursor:help; margin-left:6px;")
  }
  
  tagList(
    div(class = "seriarc-panel",
      h4(tr("mod_chrono.title")),

      div(class = "alert alert-info", style = "margin-bottom: 15px;",
        HTML(paste0("<strong>", tr("mod_chrono.info.title"), "</strong> ", tr("mod_chrono.info.text")))
      ),
      
      fluidRow(
        column(3,
          # Method selection
          div(style="display:flex; align-items:center; gap:.5rem;",
            tags$label(tr("mod_chrono.method.label")),
            tooltip_icon(tr("mod_chrono.method.tooltip"))
          ),
          radioButtons(ns("method"), NULL,
            choices = setNames(c("dim1", "parabola"),
                               c(tr("mod_chrono.method.dim1"), tr("mod_chrono.method.parabola"))),
            selected = "dim1"
          ),
          
          # LOESS smoothing
          checkboxInput(ns("use_loess"),
            label = div(style="display:flex; align-items:center; gap:.5rem;",
              span(tr("mod_chrono.loess.label")),
              tooltip_icon(tr("mod_chrono.loess.tooltip"))
            ),
            value = FALSE
          ),

          conditionalPanel(
            condition = "input.use_loess",
            ns = ns,
            sliderInput(ns("span"), tr("mod_chrono.loess.span"),
              min=.2, max=.9, value=.5, step=.05)
          ),
          
          hr(),

          # BERECHNEN-BUTTON (schmaler)
          actionButton(ns("calculate"), tr("mod_chrono.btn.calculate"),
            class = "btn btn-primary",
            style = "font-weight: bold; width: 100%;"),

          br(), br(),

          # Auto-detection status indicator (updated by server)
          uiOutput(ns("direction_status")),

          div(class = "alert alert-warning", style = "padding: 8px; margin-top: 10px; font-size: 0.85em;",
            HTML(paste0("<strong>", tr("mod_chrono.tip.title"), "</strong> ", tr("mod_chrono.tip.text")))
          ),

          br(),
          helpText(tr("mod_chrono.source"))
        ),
        column(9, 
          plotly::plotlyOutput(ns("plot_chrono"), height = 420)
        )
      )
    )
  )
}

mod_chronology_curve_server <- function(id, ca_scores_reactive, c14_data_reactive = NULL, tr = function(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    req <- shiny::req

    # ========== REACTIVE CALCULATION (WITH BUTTON) ==========
    smoothed_base <- eventReactive(input$calculate, {
      # Robuste Input-Validierung
      ca_data <- ca_scores_reactive()
      if (is.null(ca_data)) {
        showNotification(tr("notify.chrono.no.ca"),
                        type = "error", duration = 6)
        return(NULL)
      }

      df <- ca_data
      if (is.null(df) || nrow(df) == 0) {
        showNotification(tr("notify.chrono.empty"), type = "error", duration = 5)
        return(NULL)
      }
      
      # === CA-ONLY METHODE ===
      method_choice <- if (is.null(input$method)) "dim1" else input$method
      use_loess_flag <- isTRUE(input$use_loess)
      span_val <- if (use_loess_flag) input$span else 0.5
      
      # Rename columns if necessary
      if ("Site" %in% names(df)) names(df)[names(df) == "Site"] <- "site_id"
      if ("Dim1" %in% names(df)) names(df)[names(df) == "Dim1"] <- "dim1"
      if ("Dim2" %in% names(df)) names(df)[names(df) == "Dim2"] <- "dim2"
      
      if (!"dim1" %in% names(df)) {
        showNotification(tr("notify.chrono.dim1.missing"), type = "error", duration = 5)
        return(NULL)
      }
      
      df <- df[order(df$dim1), ]
      idx <- seq_len(nrow(df))
      
      x_values <- df$dim1
      method_info <- tr("plot.chrono.method.fallback")
      parabola_details <- NULL

      # === METHOD SELECTION ===
      if (method_choice == "dim1") {
        # DIMENSION 1
        x_values <- df$dim1
        method_info <- if(use_loess_flag) tr("plot.chrono.method.dim1.loess") else tr("plot.chrono.method.dim1")

      } else if (method_choice == "parabola") {
        # === PARABEL-METHODE ===
        if (!"dim2" %in% names(df)) {
          showNotification(tr("notify.chrono.dim2.missing"), type = "warning", duration = 5)
          x_values <- df$dim1
          method_info <- tr("plot.chrono.method.fallback.dim2")
        } else {
          tryCatch({
            # Parabel fitten: dim2 ~ a*dim1^2 + b*dim1 + c
            parabola_fit <- lm(dim2 ~ dim1 + I(dim1^2), data = df)

            x_values <- calculate_parabola_arclength(df$dim1, df$dim2, parabola_fit)

            method_info <- if(use_loess_flag) tr("plot.chrono.method.parabola.loess") else tr("plot.chrono.method.parabola")

            # Store parabola details
            parabola_details <- list(
              fit = parabola_fit,
              r_squared = summary(parabola_fit)$r.squared
            )

          }, error = function(e) {
            showNotification(paste(tr("notify.chrono.parabola.error"), e$message), type = "warning", duration = 6)
            x_values <- df$dim1
            method_info <- tr("plot.chrono.method.fallback.error")
            parabola_details <- NULL
          })
        }
      }
      
      # === LOESS-FIT (optional) ===
      if (use_loess_flag) {
        tryCatch({
          fit <- loess(x_values ~ idx, span = span_val)
          t_idx <- as.numeric(predict(fit))
        }, error = function(e) {
          showNotification(paste(tr("notify.chrono.loess.error"), e$message), type = "error", duration = 8)
          return(NULL)
        })
      } else {
        fit <- NULL
        t_idx <- x_values
      }
      
      # Result WITHOUT direction application (done later)
      out <- data.frame(
        site_id = if(is.null(df$site_id)) paste0("Site_", seq_len(nrow(df))) else df$site_id, 
        t_idx = t_idx, 
        dim1 = df$dim1,
        dim2 = if("dim2" %in% names(df)) df$dim2 else rep(NA, nrow(df)),
        x_values = x_values,
        stringsAsFactors = FALSE
      )
      
      list(
        out = out, 
        fit = fit, 
        method_info = method_info,
        parabola_details = parabola_details,
        stretch_factors = NULL
      )
    }, ignoreInit = FALSE, ignoreNULL = TRUE)
    
    # ========== DIRECTION APPLICATION (AUTO-DETECT FROM C14 OR MANUAL) ==========
    chrono_with_direction <- reactive({
      base <- smoothed_base()
      if (is.null(base)) return(NULL)

      out <- base$out

      # === AUTO-DETECT DIRECTION FROM C14 DATA ===
      # Goal: OxCal requires OLDER phases FIRST in sequence (=lower t_idx for older sites)
      # So we need: sites with higher BP (=older) should have LOWER t_idx
      # In the plot: position 1 (left) = oldest, position N (right) = youngest
      # If C14 data available, auto-detect; otherwise fall back to checkbox

      auto_reversed <- FALSE
      direction_source <- "manual"

      c14_data <- NULL
      tryCatch({
        if (!is.null(c14_data_reactive)) {
          c14_data <- c14_data_reactive()
        }
      }, error = function(e) NULL)

      if (!is.null(c14_data) && nrow(c14_data) >= 2) {
        # Try to auto-detect direction from C14 BP values
        tryCatch({
          # Find BP column
          bp_col <- NULL
          for (col in c("BP", "bp", "Age", "age", "C14_Age", "c14_age")) {
            if (col %in% names(c14_data)) {
              bp_col <- col
              break
            }
          }

          # Find site column
          site_col <- NULL
          for (col in c("Fundstelle", "Site", "site_id", "site", "SITE", "Site_ID")) {
            if (col %in% names(c14_data)) {
              site_col <- col
              break
            }
          }

          if (!is.null(bp_col) && !is.null(site_col)) {
            # Calculate mean BP per site
            c14_sites <- unique(c14_data[[site_col]])
            site_mean_bp <- sapply(c14_sites, function(s) {
              mean(as.numeric(c14_data[[bp_col]][c14_data[[site_col]] == s]), na.rm = TRUE)
            })
            names(site_mean_bp) <- c14_sites

            # Match with our chrono sites
            chrono_sites <- out$site_id
            matched_bp <- site_mean_bp[chrono_sites]
            matched_bp <- matched_bp[!is.na(matched_bp)]

            if (length(matched_bp) >= 2) {
              # Compare BP at start vs end of current sequence
              # Current order: sorted by t_idx (CA order)
              out_ordered <- out[order(out$t_idx), ]
              sites_ordered <- out_ordered$site_id

              # Get first and last sites that have C14 data
              first_sites <- head(sites_ordered[sites_ordered %in% names(matched_bp)], 3)
              last_sites <- tail(sites_ordered[sites_ordered %in% names(matched_bp)], 3)

              if (length(first_sites) > 0 && length(last_sites) > 0) {
                mean_bp_first <- mean(site_mean_bp[first_sites], na.rm = TRUE)
                mean_bp_last <- mean(site_mean_bp[last_sites], na.rm = TRUE)

                # Higher BP = older
                # OxCal needs oldest FIRST (=low t_idx for older sites)
                # So: first sites (low t_idx) should have HIGHER BP (=older)
                # If first sites have LOWER BP than last sites → wrong order → reverse
                if (!is.na(mean_bp_first) && !is.na(mean_bp_last)) {
                  if (mean_bp_first < mean_bp_last) {
                    # First sites have LOWER BP = younger
                    # But we need older first → reverse
                    auto_reversed <- TRUE
                    direction_source <- "auto_c14"
                  } else {
                    # First sites have HIGHER BP = older
                    # This is correct: older already first
                    auto_reversed <- FALSE
                    direction_source <- "auto_c14"
                  }
                }
              }
            }
          }
        }, error = function(e) {
          # If auto-detection fails, fall back to manual
          direction_source <- "manual"
        })
      }

      # Use auto-detected direction (C14 data should always be available for this module)
      reverse <- auto_reversed

      # Apply reversal if needed
      if (reverse) {
        out$t_idx <- -out$t_idx
        out <- out[nrow(out):1, ]
        direction_applied <- "reversed"
      } else {
        direction_applied <- "standard"
      }

      list(
        out = out,
        fit = base$fit,
        method_info = base$method_info,
        parabola_details = base$parabola_details,
        stretch_factors = base$stretch_factors,
        direction_applied = direction_applied,
        direction_source = direction_source,
        auto_reversed = auto_reversed
      )
    })

    # ========== DIRECTION STATUS INDICATOR ==========
    output$direction_status <- renderUI({
      s <- chrono_with_direction()
      if (is.null(s)) return(NULL)

      direction_source <- if (!is.null(s$direction_source)) s$direction_source else "none"

      if (direction_source == "auto_c14") {
        # Auto-detection successful
        div(class = "alert alert-success", style = "padding: 6px 10px; margin-bottom: 10px; font-size: 0.85em;",
          HTML(paste0(
            "<i class='fa fa-check-circle'></i> ",
            tr("mod_chrono.auto.active")
          ))
        )
      } else {
        # No C14 data - warn user
        div(class = "alert alert-warning", style = "padding: 6px 10px; margin-bottom: 10px; font-size: 0.85em;",
          HTML(paste0(
            "<i class='fa fa-exclamation-triangle'></i> ",
            tr("mod_chrono.auto.inactive")
          ))
        )
      }
    })

    # ========== PLOT ==========
    output$plot_chrono <- plotly::renderPlotly({
      s <- chrono_with_direction()
      if (is.null(s)) {
        plotly::plot_ly() %>%
          plotly::add_annotations(
            text = tr("plot.chrono.no.calc"),
            xref = "paper", yref = "paper",
            x = 0.5, y = 0.5,
            showarrow = FALSE,
            font = list(size = 18, color = "orange")
          ) %>%
          plotly::layout(
            xaxis = list(visible = FALSE),
            yaxis = list(visible = FALSE)
          )
      } else {
        method_choice <- if (is.null(input$method)) "dim1" else input$method
        use_loess_flag <- isTRUE(input$use_loess)
        span_val <- if (use_loess_flag) input$span else NA

        ylab_text <- tr("plot.chrono.ylab.time")

        site_idx <- seq_len(nrow(s$out))
        hover_base <- paste0(
          tr("plot.chrono.hover.site"), ": ", s$out$site_id, "<br>",
          tr("plot.chrono.hover.position"), ": ", site_idx, "<br>",
          "t_idx: ", round(s$out$t_idx, 2)
        )
        
        p <- plotly::plot_ly()
        
        # LOESS line (without name in legend)
        if (use_loess_flag && !is.null(s$fit)) {
          p <- p %>%
            plotly::add_lines(
              x = site_idx,
              y = s$out$t_idx,
              name = "LOESS",
              line = list(color = "#2980b9", width = 2),
              showlegend = FALSE,
              hoverinfo = "skip"
            )
        }
        
        # CA points (without "Sites" in legend)
        p <- p %>%
          plotly::add_markers(
            x = site_idx,
            y = s$out$t_idx,
            marker = list(color = "#e74c3c", size = 8),
            text = hover_base,
            hoverinfo = "text",
            showlegend = FALSE
          )
        
        # ANNOTATIONEN
        annotations <- list()
        
        # LOESS span
        if (use_loess_flag && !is.na(span_val)) {
          annotations[[length(annotations) + 1]] <- list(
            text = sprintf("LOESS span: %.2f", span_val),
            xref = "paper", yref = "paper",
            x = 0.98, y = 0.98,
            xanchor = "right", yanchor = "top",
            showarrow = FALSE,
            font = list(size = 11, color = "gray")
          )
        }
        
        # Parabel Info (unten links)
        if (!is.null(s$parabola_details)) {
          info_text <- sprintf("Parabel R²: %.3f", s$parabola_details$r_squared)
          
          annotations[[length(annotations) + 1]] <- list(
            text = info_text,
            xref = "paper", yref = "paper",
            x = 0.02, y = 0.02,
            xanchor = "left", yanchor = "bottom",
            showarrow = FALSE,
            font = list(size = 10, color = "darkblue")
          )
        }
        
        # DIRECTION ARROW (top center) - shows actual data direction
        # The label shows what is LEFT (position 1) and RIGHT (position N) in the plot
        direction_source <- if (!is.null(s$direction_source)) s$direction_source else "none"

        if (direction_source == "auto_c14") {
          # Auto-detected from C14: Oldest first (left), youngest last (right)
          direction_label <- paste0(tr("plot.chrono.dir.oldest.youngest"), " [", tr("plot.chrono.auto.c14"), "]")
        } else {
          # No C14 data available - show CA order without direction info
          direction_label <- tr("plot.chrono.dir.ca.order")
        }

        annotations[[length(annotations) + 1]] <- list(
          text = direction_label,
          xref = "paper", yref = "paper",
          x = 0.5, y = 0.98,
          xanchor = "center", yanchor = "top",
          showarrow = FALSE,
          font = list(size = 14, color = "darkred", family = "Arial Black")
        )
        
        # LAYOUT
        p %>%
          plotly::layout(
            title = list(
              text = sprintf(tr("plot.chrono.title"), s$method_info),
              font = list(size = 14)
            ),
            xaxis = list(
              title = tr("plot.chrono.xlab"),
              showgrid = TRUE,
              gridcolor = "lightgray"
            ),
            yaxis = list(
              title = ylab_text,
              showgrid = TRUE,
              gridcolor = "lightgray"
            ),
            hovermode = "closest",
            showlegend = TRUE,
            legend = list(
              x = 0.02, y = 0.90,
              xanchor = "left", yanchor = "top",
              bgcolor = "rgba(255,255,255,0.8)"
            ),
            annotations = annotations,
            margin = list(l = 60, r = 20, t = 60, b = 60)
          )
      }
    })

    # ========== RETURN ==========
    return(list(
      get_chrono_curve = reactive({
        s <- chrono_with_direction()
        if (is.null(s)) return(NULL)
        s$out
      }),
      get_stretch_factors = reactive({
        s <- chrono_with_direction()
        if (is.null(s) || is.null(s$stretch_factors)) return(NULL)
        s$stretch_factors
      })
    ))
  })
}

# HILFSFUNKTIONEN

calculate_parabola_arclength <- function(dim1, dim2, parabola_fit) {
  # Sort by dim1
  order_idx <- order(dim1)
  x <- dim1[order_idx]

  # Parabola coefficients
  coefs <- coef(parabola_fit)
  a <- coefs[3]  # I(dim1^2)
  b <- coefs[2]  # dim1
  c <- coefs[1]  # Intercept
  
  # s = ∫√(1 + (dy/dx)²) dx
  # dy/dx = 2*a*x + b
  
  arclengths <- numeric(length(x))
  arclengths[1] <- 0
  
  for (i in 2:length(x)) {
    # Numerical integration between x[i-1] and x[i]
    x_seq <- seq(x[i-1], x[i], length.out = 20)
    dy_dx <- 2 * a * x_seq + b
    integrand <- sqrt(1 + dy_dx^2)
    # Simpson-Regel (approximativ)
    dx <- diff(x_seq)[1]
    arc_segment <- sum(integrand) * dx
    arclengths[i] <- arclengths[i-1] + arc_segment
  }
  
  # Back to original order
  result <- numeric(length(dim1))
  result[order_idx] <- arclengths
  
  return(result)
}

# 14C calibration (existing function - unchanged)
calibrate_single_14c_median <- function(c14age, c14sd) {
  tryCatch({
    intcal <- readRDS("data/intcal_lookup.rds")
    if (is.null(intcal)) stop("IntCal20 not available")
    
    tau_lab <- 1 / (c14sd^2)
    tau_curve <- intcal$tau_curve
    mu_curve <- intcal$mu_curve
    
    loglik <- -0.5 * (tau_lab + tau_curve) * (c14age - mu_curve)^2
    lik <- exp(loglik - max(loglik))
    
    post <- lik / sum(lik)
    cumsum_post <- cumsum(post)
    median_idx <- which.min(abs(cumsum_post - 0.5))
    
    return(intcal$theta_grid[median_idx])
    
  }, error = function(e) {
    warning(sprintf("14C calibration failed: %s", e$message))
    return(NA_real_)
  })
}

calibrate_14c_for_chrono <- function(c14_data, ca_scores, ca_positions, span = 0.5) {
  
  if (is.null(c14_data) || nrow(c14_data) == 0) {
    stop("No 14C data available")
  }
  
  if (is.null(ca_scores) || nrow(ca_scores) == 0) {
    stop("No CA scores available")
  }
  
  site_col <- age_col <- error_col <- NA
  
  for(col in c("Site","site_id","site","SITE","Site_ID")) {
    if(col %in% names(c14_data)) { site_col <- col; break }
  }
  for(col in c("Age","age","bp","BP","C14_Age","c14_age")) {
    if(col %in% names(c14_data)) { age_col <- col; break }
  }
  for(col in c("Error","error","std","STD","Std","C14_Error","c14_error")) {
    if(col %in% names(c14_data)) { error_col <- col; break }
  }
  
  if (is.na(site_col) || is.na(age_col) || is.na(error_col)) {
    stop("14C data: Columns not recognized (Site, Age, Error required)")
  }
  
  c14_clean <- data.frame(
    site_id = trimws(as.character(c14_data[[site_col]])),
    c14age = as.numeric(c14_data[[age_col]]),
    c14sd = as.numeric(c14_data[[error_col]]),
    stringsAsFactors = FALSE
  )
  
  c14_clean <- c14_clean[!is.na(c14_clean$site_id) & 
                         !is.na(c14_clean$c14age) & 
                         !is.na(c14_clean$c14sd), ]
  
  if (nrow(c14_clean) == 0) stop("No valid 14C data after cleaning")

  site_medians <- dplyr::group_by(c14_clean, site_id) %>%
    dplyr::summarise(
      cal_median = mean(sapply(seq_len(dplyr::n()), function(i) {
        calibrate_single_14c_median(c14age[i], c14sd[i])
      }), na.rm = TRUE),
      .groups = 'drop'
    )
  
  ca_with_id <- data.frame(
    site_id = ca_scores$Site,
    ca_position = ca_positions,
    stringsAsFactors = FALSE
  )
  
  merged <- merge(site_medians, ca_with_id, by = "site_id", all = FALSE)
  
  if (nrow(merged) < 3) {
    stop("Too few sites with 14C+CA (min. 3 required)")
  }
  
  merged <- merged[order(merged$ca_position), ]
  
  fit <- loess(cal_median ~ ca_position, data = merged, span = span)
  predicted <- predict(fit, newdata = data.frame(ca_position = ca_positions))
  
  slopes <- diff(predicted)
  mean_slope <- mean(slopes)
  stretch_factors <- c(1.0, slopes / mean_slope)
  
  list(
    predicted = predicted,
    stretch_factors = stretch_factors,
    fit = fit,
    merged_data = merged,
    mean_slope = mean_slope
  )
}

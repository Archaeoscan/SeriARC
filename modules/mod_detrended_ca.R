# ======================= mod_detrended_ca.R =======================
# Detrended Correspondence Analysis Module - based on PAST
# Implements DCA according to Hill & Gauch (1980) and ter Braak & Prentice (1988)

mod_detrended_ca_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL  # UI ist in ui.R definiert
}

mod_detrended_ca_server <- function(filtered_data, meta_data, cache, get_site_group, get_element_details = NULL, input, output, session, tr = function(x) x) {

  source("helpers/plot_components.R", local = TRUE)
  source("helpers/download_components.R", local = TRUE)

  last_dca_plotly <- reactiveVal(NULL)

  # ===== HELPER FUNCTIONS =====

  # PAST-like detrending implementation
  # Based on Hill & Gauch (1980) and ter Braak & Prentice (1988)
  perform_detrended_ca <- function(mat, detrending_method = "linear", n_segments = 26) {
    
    # STEP 1: Perform standard CA
    ca_result <- FactoMineR::CA(mat, graph = FALSE)

    # STEP 2: Detrending according to selected method
    row_coords_original <- as.matrix(ca_result$row$coord)
    col_coords_original <- as.matrix(ca_result$col$coord)

    # For each dimension from the second onwards: Detrend against first dimension
    n_dims <- min(ncol(row_coords_original), ncol(col_coords_original))
    
    if (n_dims < 2) {
      warning("Not enough dimensions available for detrending")
      return(ca_result)
    }
    
    # Detrended Koordinaten initialisieren (explizite Kopien!)
    detrended_row_coords <- as.matrix(row_coords_original)
    detrended_col_coords <- as.matrix(col_coords_original)
    
    if (detrending_method == "linear") {
      # LINEARES DETRENDING (wie in PAST)
      for (dim in 2:n_dims) {
        # Sites detrenden (gegen Original-Koordinaten)
        lm_sites <- lm(row_coords_original[, dim] ~ row_coords_original[, 1])
        detrended_row_coords[, dim] <- residuals(lm_sites)
        
        # Types detrenden (gegen Original-Koordinaten)
        lm_types <- lm(col_coords_original[, dim] ~ col_coords_original[, 1])
        detrended_col_coords[, dim] <- residuals(lm_types)
      }
    } else if (detrending_method == "nonlinear") {
      # NON-LINEARES DETRENDING (Segmentierung wie in PAST)
      for (dim in 2:n_dims) {
        # Sites: Segmentweises Detrending (gegen Original-Koordinaten)
        detrended_row_coords[, dim] <- nonlinear_detrend(row_coords_original[, 1], row_coords_original[, dim], n_segments)
        
        # Types: Segmentweises Detrending (gegen Original-Koordinaten)
        detrended_col_coords[, dim] <- nonlinear_detrend(col_coords_original[, 1], col_coords_original[, dim], n_segments)
      }
    }
    
    result <- ca_result
    result$row$coord <- detrended_row_coords
    result$col$coord <- detrended_col_coords

    # Add DCA-specific attributes
    result$dca_method <- detrending_method
    result$dca_segments <- if(detrending_method == "nonlinear") n_segments else NA
    # Store original coordinates (IMPORTANT: these are unchanged)
    result$dca_original_coords <- list(row = row_coords_original, col = col_coords_original)
    
    return(result)
  }
  
  # Non-lineares Detrending (Distanz-basierte Segmentierung)
  # According to Hill & Gauch (1980): uniform distance intervals, not point count
  nonlinear_detrend <- function(x_vals, y_vals, n_segments = 26) {
    # Sortierung nach x-Werten
    ord <- order(x_vals)
    x_sorted <- x_vals[ord]
    y_sorted <- y_vals[ord]
    
    # Distanz-basierte Segmentgrenzen definieren
    x_range <- range(x_sorted, na.rm = TRUE)
    breaks <- seq(x_range[1], x_range[2], length.out = n_segments + 1)
    
    detrended_y <- numeric(length(y_vals))
    
    for (seg in 1:n_segments) {
      # Identify points in this segment
      if (seg < n_segments) {
        in_segment <- x_sorted >= breaks[seg] & x_sorted < breaks[seg + 1]
      } else {
        # Last segment inclusive of upper bound
        in_segment <- x_sorted >= breaks[seg] & x_sorted <= breaks[seg + 1]
      }

      if (sum(in_segment) == 0) next  # Skip empty segment
      
      segment_x <- x_sorted[in_segment]
      segment_y <- y_sorted[in_segment]
      
      if (length(segment_x) > 1) {
        # Lineare Regression innerhalb des Segments
        segment_lm <- lm(segment_y ~ segment_x)
        segment_residuals <- residuals(segment_lm)
      } else if (length(segment_x) == 1) {
        # Einzelner Punkt: Residuum = 0
        segment_residuals <- 0
      } else {
        next
      }
      
      # Map residuals to original indices
      orig_indices <- ord[in_segment]
      detrended_y[orig_indices] <- segment_residuals
    }
    
    return(detrended_y)
  }
  
  # Identify supplementary elements for DCA (analogous to standard CA)
  identify_supplementary_dca <- function(mat, meta_data) {
    req(meta_data$data)
    
    available_sites <- rownames(mat)
    available_types <- colnames(mat)
    
    all_sites <- meta_data$data$sites[meta_data$data$sites$Selected & 
                                        meta_data$data$sites$Entity %in% available_sites, ]
    all_types <- meta_data$data$types[meta_data$data$types$Selected & 
                                        meta_data$data$types$Entity %in% available_types, ]
    
    suppl_row_indices <- NULL
    suppl_col_indices <- NULL
    
    if (any(all_sites$Supplementary)) {
      suppl_sites <- all_sites$Entity[all_sites$Supplementary]
      suppl_row_indices <- which(rownames(mat) %in% suppl_sites)
    }
    
    if (any(all_types$Supplementary)) {
      suppl_types <- all_types$Entity[all_types$Supplementary]
      suppl_col_indices <- which(colnames(mat) %in% suppl_types)
    }
    
    list(
      row_indices = suppl_row_indices,
      col_indices = suppl_col_indices,
      active_rows = nrow(mat) - length(suppl_row_indices),
      active_cols = ncol(mat) - length(suppl_col_indices)
    )
  }
  
  # Generate plot data for DCA
  generate_dca_plot_data <- function(res, x_idx, y_idx, focus) {
    plot_data <- data.frame()
    
    # Add sites
    if (focus %in% c("both", "sites_only")) {
      if (!is.null(res$row) && !is.null(res$row$coord)) {
        row_coords <- res$row$coord[, c(x_idx, y_idx), drop = FALSE]
        sites_data <- data.frame(
          x = row_coords[, 1], y = row_coords[, 2],
          label = rownames(row_coords), type = 'Site', element_type = 'Active',
          contrib_x = NA,  # DCA: Contribs not valid (from original CA)
          contrib_y = NA,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, sites_data)
      }
      
      # Supplementary Sites (falls vorhanden)
      if (!is.null(res$row.sup) && !is.null(res$row.sup$coord)) {
        suppl_row_coords <- res$row.sup$coord[, c(x_idx, y_idx), drop = FALSE]
        suppl_row_data <- data.frame(
          x = suppl_row_coords[, 1], y = suppl_row_coords[, 2],
          label = rownames(suppl_row_coords), type = 'Site', element_type = 'Supplementary',
          contrib_x = NA, contrib_y = NA, stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, suppl_row_data)
      }
    }
    
    # Add types
    if (focus %in% c("both", "types_only")) {
      if (!is.null(res$col) && !is.null(res$col$coord)) {
        col_coords <- res$col$coord[, c(x_idx, y_idx), drop = FALSE]
        types_data <- data.frame(
          x = col_coords[, 1], y = col_coords[, 2],
          label = rownames(col_coords), type = 'Type', element_type = 'Active',
          contrib_x = NA,  # DCA: Contribs not valid (from original CA)
          contrib_y = NA,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, types_data)
      }
      
      # Supplementary Types (falls vorhanden)
      if (!is.null(res$col.sup) && !is.null(res$col.sup$coord)) {
        suppl_col_coords <- res$col.sup$coord[, c(x_idx, y_idx), drop = FALSE]
        suppl_col_data <- data.frame(
          x = suppl_col_coords[, 1], y = suppl_col_coords[, 2],
          label = rownames(suppl_col_coords), type = 'Type', element_type = 'Supplementary',
          contrib_x = NA, contrib_y = NA, stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, suppl_col_data)
      }
    }
    
    plot_data
  }
  
  # Generate hover text for DCA points
  generate_dca_hover_text <- function(plot_data, x_dim, y_dim, method, get_element_details = NULL) {
    method_info <- switch(method,
      "linear" = "Linear detrended",
      "nonlinear" = "Non-linear detrended (segmented)", 
      "Detrended"
    )
    
    if (is.null(get_element_details)) {
      sprintf(
        "<b>%s</b><br>Type: %s %s<br>%s: %.3f<br>%s: %.3f<br>Method: %s<br><i>SeriARC DCA (PAST-like)</i>",
        plot_data$label, plot_data$type,
        ifelse(plot_data$element_type == "Supplementary", "(🔹 Projected)", "(🔵 Active)"),
        x_dim, plot_data$x, y_dim, plot_data$y, method_info
      )
    } else {
      sapply(1:nrow(plot_data), function(i) {
        details <- get_element_details(plot_data$label[i], plot_data$type[i])

        basic_info <- sprintf(
          "<b>%s</b><br>Type: %s %s<br>%s: %.3f<br>%s: %.3f<br>Method: %s",
          plot_data$label[i], plot_data$type[i],
          ifelse(plot_data$element_type[i] == "Supplementary", "(🔹 Projected)", "(🔵 Active)"),
          x_dim, plot_data$x[i], y_dim, plot_data$y[i], method_info
        )

        param_info <- sprintf("<br><b>⚖️ Weight:</b> %.2f (%s)", details$weight, details$impact)

        paste0(basic_info, param_info, "<br><i>SeriARC DCA - ter Braak & Prentice (1988)</i>")
      })
    }
  }
  
  # ===== DCA BERECHNUNG =====
  
  dca_res <- eventReactive(input$dca_refresh, {
    req(filtered_data())
    
    withProgress(message = '📈 Computing Detrended CA...', {
      incProgress(0.2, detail = "Validating matrix...")
      
      tryCatch({
        mat <- filtered_data()
        validate(need(sum(mat, na.rm = TRUE) > 0, "⚠ Matrix must contain positive values!"))
        
        # Supplementary Elemente identifizieren
        suppl_info <- identify_supplementary_dca(mat, meta_data)
        
        validate(need(suppl_info$active_rows >= 3, tr("validate.dca.min.sites")))
        validate(need(suppl_info$active_cols >= 3, tr("validate.dca.min.types")))
        
        incProgress(0.4, detail = tr("dca.progress.running"))
        
        # Prepare matrix for analysis (only active elements)
        analysis_mat <- mat
        if (!is.null(suppl_info$row_indices) && length(suppl_info$row_indices) > 0) {
          analysis_mat <- analysis_mat[-suppl_info$row_indices, , drop = FALSE]
        }
        if (!is.null(suppl_info$col_indices) && length(suppl_info$col_indices) > 0) {
          analysis_mat <- analysis_mat[, -suppl_info$col_indices, drop = FALSE]
        }
        
        incProgress(0.6, detail = "Applying detrending...")
        
        # Perform DCA
        detrending_method <- input$dca_method %||% "linear"
        n_segments <- input$dca_segments %||% 26
        
        result <- perform_detrended_ca(analysis_mat, detrending_method, n_segments)
        
        # Supplementary Elemente projizieren (falls vorhanden)
        if (!is.null(suppl_info$row_indices) || !is.null(suppl_info$col_indices)) {
          # Simplified projection: use standard CA for supplementary
          full_ca <- FactoMineR::CA(
            mat, 
            row.sup = suppl_info$row_indices,
            col.sup = suppl_info$col_indices,
            graph = FALSE
          )
          
          # Add supplementary coordinates to DCA result
          result$row.sup <- full_ca$row.sup
          result$col.sup <- full_ca$col.sup
        }
        
        cache$dca_result <- result
        
        # Erfolgsmeldung
        method_text <- if (detrending_method == "linear") {
          "Linear"
        } else {
          sprintf("Non-linear (%d segments)", n_segments)
        }
        
        showNotification(
          sprintf("✅ DCA successful: %d active sites, %d active types (%s detrending)", 
                  suppl_info$active_rows, suppl_info$active_cols, method_text), 
          type = "message", duration = 4
        )
        
        incProgress(1.0, detail = "Completed!")
        result
        
      }, error = function(e) {
        showNotification(paste("⚠ DCA Error:", e$message), type = "error", duration = 8)
        return(NULL)
      })
    })
  })
  
  # ===== UI COMPONENTS =====
  
  # Dimension selection for DCA
  output$dca_dim_select <- renderUI({
    req(dca_res())
    
    n_dims <- ncol(dca_res()$row$coord)
    dims <- paste0("DCA", seq_len(n_dims))
    
    list(
      selectInput("dca_x_dim",
                  div("X-Axis:", tags$span("ℹ️", style="margin-left:5px; cursor:help;",
                                            title="First detrended dimension (corresponds to CA Dim1 without arch effect)")),
                  choices = dims, selected = dims[1]),
      selectInput("dca_y_dim",
                  div("Y-Axis:", tags$span("ℹ️", style="margin-left:5px; cursor:help;",
                                            title="Second detrended dimension (cleaned of gradient effect)")),
                  choices = dims, selected = dims[min(2, length(dims))])
    )
  })
  
  # DCA statistics (PAST-like)
  output$dca_stats <- renderText({
    req(dca_res())
    
    result <- dca_res()
    method <- result$dca_method %||% "linear"
    
    # Grundlegende Informationen
    n_dims <- ncol(result$row$coord)
    n_sites <- nrow(result$row$coord)
    n_types <- nrow(result$col$coord)
    
    # Comparison with original CA coordinates (DYNAMIC based on selected dimensions)
    cor_dim_x <- cor_dim_y <- NA
    x_dim_label <- "X"
    y_dim_label <- "Y"
    
    if (!is.null(result$dca_original_coords) && !is.null(input$dca_x_dim) && !is.null(input$dca_y_dim)) {
      orig_coords <- result$dca_original_coords
      detrend_coords <- result$row$coord
      
      # Aktuelle Dimensionen extrahieren
      x_idx <- as.numeric(gsub("DCA", "", input$dca_x_dim))
      y_idx <- as.numeric(gsub("DCA", "", input$dca_y_dim))
      
      x_dim_label <- paste0("DCA", x_idx, " vs CA", x_idx)
      y_dim_label <- paste0("DCA", y_idx, " vs CA", y_idx)
      
      if (x_idx <= ncol(orig_coords$row) && x_idx <= ncol(detrend_coords)) {
        cor_dim_x <- cor(orig_coords$row[, x_idx], detrend_coords[, x_idx], use = "complete.obs")
      }
      if (y_idx <= ncol(orig_coords$row) && y_idx <= ncol(detrend_coords)) {
        cor_dim_y <- cor(orig_coords$row[, y_idx], detrend_coords[, y_idx], use = "complete.obs")
      }
    }
    
    method_description <- switch(method,
      "linear" = "📐 LINEAR DETRENDING:\n• Linear regression between dimensions\n• Residuals as cleaned coordinates",
      "nonlinear" = sprintf("📊 NON-LINEAR DETRENDING:\n• Segment-wise cleaning (%d segments)\n• Locally adapted regression", result$dca_segments %||% 26),
      "📈 DETRENDING METHOD"
    )
    
    # Quality assessment based on Y-dimension (typically the detrended)
    quality_assessment <- ""
    if (!is.na(cor_dim_y)) {
      r_squared <- cor_dim_y^2
      if (abs(cor_dim_y) < 0.1) {
        quality_assessment <- sprintf("🟢 Excellent cleaning (r² = %.3f)", r_squared)
      } else if (abs(cor_dim_y) < 0.3) {
        quality_assessment <- sprintf("🟡 Good cleaning (r² = %.3f)", r_squared)
      } else if (abs(cor_dim_y) < 0.5) {
        quality_assessment <- sprintf("🟠 Moderate cleaning (r² = %.3f)", r_squared)
      } else {
        quality_assessment <- sprintf("🔴 Weak cleaning (r² = %.3f)", r_squared)
      }
    }
    
    sprintf(
      "📊 DETRENDED CORRESPONDENCE ANALYSIS\n\n%s\n\n🔍 ANALYSIS RESULT:\n• Sites: %d\n• Types: %d\n• Dimensions: %d\n\n📈 DETRENDING EFFECTIVENESS (currently selected dimensions):\n• %s: r = %.3f%s\n• %s: r = %.3f%s\n%s\n\n💡 INTERPRETATION:\n• r ≈ 1.0 (Dim1): First dimension remains unchanged (correct!)\n• r < 0.3: Arch effect successfully removed\n• r > 0.7: Strong correlation = weak cleaning\n\n📚 Method: Hill & Gauch (1980), ter Braak (1988)\nℹ️ Removes the \"arch effect\" of standard CA",
      method_description, n_sites, n_types, n_dims,
      x_dim_label, if(!is.na(cor_dim_x)) cor_dim_x else 0,
      if(!is.na(cor_dim_x) && abs(cor_dim_x) > 0.95) " ✓ expected" else "",
      y_dim_label, if(!is.na(cor_dim_y)) cor_dim_y else 0,
      if(!is.na(cor_dim_y) && abs(cor_dim_y) < 0.3) " ✓ well cleaned" else if(!is.na(cor_dim_y) && abs(cor_dim_y) > 0.7) " ⚠ weakly cleaned" else "",
      if(quality_assessment != "") paste0("\n• ", quality_assessment) else ""
    )
  })
  
  # ===== DCA PLOT DATA =====
  
  dca_plot_data <- reactive({
    req(dca_res(), input$dca_x_dim, input$dca_y_dim, input$dca_analysis_focus)
    
    res <- dca_res()
    x_idx <- as.numeric(gsub("DCA", "", input$dca_x_dim))
    y_idx <- as.numeric(gsub("DCA", "", input$dca_y_dim))
    
    # Generate plot data
    plot_data <- generate_dca_plot_data(res, x_idx, y_idx, input$dca_analysis_focus)
    
    if (nrow(plot_data) == 0) return(data.frame())
    
    # Group assignment for sites
    plot_data$group <- NA_character_
    is_site <- plot_data$type == 'Site'
    if (any(is_site)) {
      plot_data$group[is_site] <- get_site_group(plot_data$label[is_site])
    }
    
    # Labels generieren
    if (!is.null(input$dca_show_labels) && input$dca_show_labels) {
      plot_data$lab <- substr(plot_data$label, 1, input$dca_label_chars %||% 12)
    } else {
      plot_data$lab <- ""
    }
    
    # DCA-spezifischen Hover-Text generieren
    plot_data$hover_text <- generate_dca_hover_text(
      plot_data, input$dca_x_dim, input$dca_y_dim, 
      res$dca_method %||% "linear", get_element_details
    )
    
    plot_data
  })
  
  # ===== DCA PLOTLY =====
  
  output$dca_plotly <- renderPlotly({
    req(dca_plot_data())
    
    plot_data <- dca_plot_data()
    
    # Farbgruppierung
    plot_data$color_group <- if (isTRUE(input$dca_color_by_group)) {
      ifelse(is.na(plot_data$group), paste(plot_data$type, plot_data$element_type), paste0("G:", plot_data$group))
    } else {
      paste(plot_data$type, plot_data$element_type)
    }
    
    # SeriARC Farbschema
    seri_colors <- seri_arc_colors()
    colors <- if (isTRUE(input$dca_color_by_group)) {
      cc <- seri_arc_factor_colors(factor(plot_data$color_group))
      unname(cc$cols)
    } else {
      c("Site Active" = seri_colors$site_active, "Type Active" = seri_colors$type_active,
        "Site Supplementary" = seri_colors$site_supplementary, "Type Supplementary" = seri_colors$type_supplementary)
    }
    
    # Axis titles with DCA prefix
    x_title <- paste(input$dca_x_dim %||% "DCA1", "(detrended)")
    y_title <- paste(input$dca_y_dim %||% "DCA2", "(detrended)")
    
    method_info <- if (!is.null(dca_res()$dca_method)) {
      switch(dca_res()$dca_method,
        "linear" = "Linear detrending", 
        "nonlinear" = sprintf("Non-linear detrending (%d segments)", dca_res()$dca_segments %||% 26),
        "Detrended"
      )
    } else "Detrended"
    
    p <- plot_ly() %>%
      layout(ca_plotly_layout(
        x_title = x_title,
        y_title = y_title, 
        plot_title = "SeriARC Detrended Correspondence Analysis",
        subtitle = sprintf("📈 %s (PAST-like implementation)", method_info)
      ))
    
    # Separate active and supplementary data
    active_data <- plot_data[plot_data$element_type == 'Active', ]
    suppl_data <- plot_data[plot_data$element_type == 'Supplementary', ]
    
    # Add active points
    if (nrow(active_data) > 0) {
      # Sites (Kreise)
      site_subset <- active_data[active_data$type == 'Site', ]
      if (nrow(site_subset) > 0) {
        site_color <- if (isTRUE(input$dca_color_by_group)) {
          sapply(site_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else seri_colors$site_active
          })
        } else rep(colors["Site Active"], nrow(site_subset))
        
        p <- p %>% add_markers(
          x = site_subset$x, y = site_subset$y,
          marker = ca_site_marker(site_color, (input$dca_point_size %||% 3) * 4, element_type = "Active"),
          text = site_subset$lab, textposition = "middle right",
          textfont = list(size = 10, color = "#2c3e50"),
          hovertext = site_subset$hover_text, hoverinfo = 'text',
          name = "Sites (aktiv)", showlegend = TRUE
        )
      }
      
      # Types (Dreiecke)
      type_subset <- active_data[active_data$type == 'Type', ]
      if (nrow(type_subset) > 0) {
        type_color <- if (isTRUE(input$dca_color_by_group)) {
          sapply(type_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else seri_colors$type_active
          })
        } else rep(colors["Type Active"], nrow(type_subset))
        
        p <- p %>% add_markers(
          x = type_subset$x, y = type_subset$y,
          marker = ca_type_marker(type_color, (input$dca_point_size %||% 3) * 4, element_type = "Active"),
          text = type_subset$lab, textposition = "middle right",
          textfont = list(size = 10, color = "#2c3e50"),
          hovertext = type_subset$hover_text, hoverinfo = 'text',
          name = "Types (aktiv)", showlegend = FALSE
        )
      }
    }
    
    # Add supplementary points
    if (nrow(suppl_data) > 0) {
      # Supplementary Sites
      site_subset <- suppl_data[suppl_data$type == 'Site', ]
      if (nrow(site_subset) > 0) {
        site_color <- if (isTRUE(input$dca_color_by_group)) {
          sapply(site_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else seri_colors$site_supplementary
          })
        } else rep(colors["Site Supplementary"], nrow(site_subset))
        
        p <- p %>% add_markers(
          x = site_subset$x, y = site_subset$y,
          marker = ca_site_marker(site_color, (input$dca_point_size %||% 3) * 4, element_type = "Supplementary"),
          text = site_subset$lab, textposition = "middle right",
          textfont = list(size = 10, color = "#2c3e50"),
          hovertext = site_subset$hover_text, hoverinfo = 'text',
          name = "Sites (supplementary)", showlegend = TRUE
        )
      }
      
      # Supplementary Types
      type_subset <- suppl_data[suppl_data$type == 'Type', ]
      if (nrow(type_subset) > 0) {
        type_color <- if (isTRUE(input$dca_color_by_group)) {
          sapply(type_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else seri_colors$type_supplementary
          })
        } else rep(colors["Type Supplementary"], nrow(type_subset))
        
        p <- p %>% add_markers(
          x = type_subset$x, y = type_subset$y,
          marker = ca_type_marker(type_color, (input$dca_point_size %||% 3) * 4, element_type = "Supplementary"),
          text = type_subset$lab, textposition = "middle right",
          textfont = list(size = 10, color = "#2c3e50"),
          hovertext = type_subset$hover_text, hoverinfo = 'text',
          name = "Types (supplementary)", showlegend = FALSE
        )
      }
    }
    
    last_dca_plotly(standard_plotly_config(p, "2d"))
  })

  # ===== DOWNLOAD-HANDLER =====

  # DCA Plot exports (with real data)
  output$download_dca_plot_png  <- create_plotly_png_handler(last_dca_plotly, "DCA_Plot", tr = tr)
  output$download_dca_plot_svg  <- create_plotly_png_handler(last_dca_plotly, "DCA_Plot_SVG", tr = tr)
  output$download_dca_plot_pdf  <- create_plotly_pdf_handler(last_dca_plotly, "DCA_Plot", tr = tr)
  
  # DCA data export
  output$download_dca_data <- create_excel_download_handler(
    function() {
      req(dca_res())
      # Create DCA-specific Excel sheets (analog zu CA)
      create_dca_excel_sheets(dca_res(), filtered_data())
    },
    "DCA_Results", "Detrended Correspondence Analysis",
    tr = tr
  )
  
  # CSV export for DCA
  output$download_dca_data_csv <- create_csv_download_handler(
    function() {
      req(dca_plot_data())
      data <- dca_plot_data()
      data_export <- data[, c("label", "type", "element_type", "x", "y")]
      colnames(data_export) <- c("Entity", "Element_Type", "Status", "DCA_Dim1", "DCA_Dim2")
      data_export
    },
    "DCA_Koordinaten", add_header = TRUE,
    tr = tr
  )
  
  # HTML export for DCA — 1:1 copy of the interactive figure shown on screen
  output$download_dca_plot_html <- create_plotly_html_handler(last_dca_plotly, "SeriARC_DCA_Plot", tr = tr)
  
  # Return for other modules
  return(list(
    dca_result = dca_res,
    dca_plot_data = dca_plot_data
  ))
}

# ===== HELPER FOR DCA EXCEL EXPORT =====

# Create DCA-specific Excel sheets
create_dca_excel_sheets <- function(dca_result, original_matrix) {
  sheets <- list()
  
  # Sheet 1: DCA Koordinaten
  coords_data <- data.frame(
    Element = c(rownames(dca_result$row$coord), rownames(dca_result$col$coord)),
    Type = c(rep("Site", nrow(dca_result$row$coord)), rep("Type", nrow(dca_result$col$coord))),
    dca_result$row$coord,
    dca_result$col$coord[, 1:ncol(dca_result$row$coord), drop = FALSE],
    stringsAsFactors = FALSE
  )
  sheets[["DCA_Coordinates"]] <- coords_data

  # Sheet 2: DCA method info
  method_info <- data.frame(
    Parameter = c("Detrending_Method", "Segments", "Dimensions", "Sites", "Types"),
    Value = c(
      dca_result$dca_method %||% "linear",
      if(!is.na(dca_result$dca_segments)) dca_result$dca_segments else "NA",
      ncol(dca_result$row$coord),
      nrow(dca_result$row$coord),
      nrow(dca_result$col$coord)
    ),
    stringsAsFactors = FALSE
  )
  sheets[["DCA_Method"]] <- method_info
  
  # Sheet 3: Originaldaten (falls vorhanden)
  if (!is.null(original_matrix)) {
    sheets[["Originaldaten"]] <- as.data.frame(as.matrix(original_matrix))
  }
  
  return(sheets)
}


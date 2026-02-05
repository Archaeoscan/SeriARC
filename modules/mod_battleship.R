# mod_battleship.R - BATTLESHIP CURVES
# SeriARC: Chronological visualization of artifact frequencies

mod_battleship_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL
}

mod_battleship_server <- function(filtered_data, cache, cluster_names, cluster_colors,
                                 get_site_group = NULL,
                                 get_element_details = NULL, input, output, session, tr = function(x) x) {
  
  # Helper functions
  get_cluster_name <- function(cluster_id) {
    if (is.null(cluster_names) || is.null(cluster_names())) {
      return(paste("Cluster", cluster_id))
    }
    names_list <- cluster_names()
    names_list[[as.character(cluster_id)]] %||% paste("Cluster", cluster_id)
  }
  
  get_cluster_color <- function(cluster_id) {
    default_colors <- c("#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6",
                        "#1abc9c", "#e67e22", "#34495e", "#e91e63", "#795548")
    if (is.null(cluster_colors) || is.null(cluster_colors())) {
      return(default_colors[((as.numeric(cluster_id) - 1) %% length(default_colors)) + 1])
    }
    colors_list <- cluster_colors()
    colors_list[[as.character(cluster_id)]] %||% default_colors[((as.numeric(cluster_id) - 1) %% length(default_colors)) + 1]
  }

  get_group_color <- function(group_name) {
    # Colors for type groups (from column 2 of the original data)
    group_colors <- c("#e74c3c", "#3498db", "#2ecc71", "#f39c12", "#9b59b6",
                     "#1abc9c", "#e67e22", "#34495e", "#e91e63", "#16a085")
    # Use hash of group name for consistent color
    hash_val <- sum(utf8ToInt(as.character(group_name))) %% length(group_colors)
    group_colors[hash_val + 1]
  }
  
  # Matrix for battleship analysis
  get_current_matrix_for_bship <- reactive({
    m <- if (!is.null(cache$seriation_result$permuted_matrix)) {
      cache$seriation_result$permuted_matrix
    } else {
      filtered_data()
    }
    
    validate(need(!is.null(m), tr("validate.battleship.no.matrix")))
    
    m <- as.matrix(m)
    storage.mode(m) <- "numeric"
    m[is.na(m)] <- 0
    
    if (is.null(rownames(m))) rownames(m) <- paste0("Site_", seq_len(nrow(m)))
    if (is.null(colnames(m))) colnames(m) <- paste0("Type_", seq_len(ncol(m)))
    m
  })
  
  # Smoothing for chronological trends
  smooth_series <- function(x, y, method = "loess", param = 0.3) {
    y[!is.finite(y)] <- 0
    if (length(x) < 3L) return(list(x = x, y = y))
    
    tryCatch({
      switch(method,
        "loess" = {
          sp <- stats::loess(y ~ x, span = max(0.1, min(0.95, as.numeric(param))))
          xs <- seq(min(x), max(x), length.out = max(200L, length(x)))
          ys <- stats::predict(sp, newdata = xs)
          list(x = xs, y = pmax(ys, 0, na.rm = TRUE))
        },
        "spline" = {
          sp <- stats::smooth.spline(x, y, spar = max(0.05, min(1.5, as.numeric(param))))
          xs <- seq(min(x), max(x), length.out = max(200L, length(x)))
          list(x = xs, y = pmax(stats::predict(sp, xs)$y, 0, na.rm = TRUE))
        },
        "gaussian" = {
          bw <- max(0.5, min(diff(range(x)) / 2, as.numeric(param)))
          xs <- seq(min(x), max(x), length.out = max(200L, length(x)))
          ks <- stats::ksmooth(x, y, kernel = "normal", bandwidth = bw, x.points = xs)
          list(x = xs, y = pmax(ks$y, 0, na.rm = TRUE))
        },
        list(x = x, y = y)
      )
    }, error = function(e) list(x = x, y = y))
  }
  
  # Polygon generation for battleship curves
  make_polygon <- function(x_vals, widths, center, orientation = "horizontal") {
    if (orientation == "horizontal") {
      list(x = c(x_vals, rev(x_vals)), y = c(center + widths, rev(center - widths)))
    } else {
      list(x = c(center + widths, rev(center - widths)), y = c(x_vals, rev(x_vals)))
    }
  }
  
  # UI components
  output$bship_types_ui <- renderUI({
    has_seriation <- !is.null(cache$seriation_result$permuted_matrix)
    
    if (!has_seriation) {
      div(class = "alert alert-warning", style = "margin-top: 20px; padding: 20px;",
        h4("🚢 SeriARC Battleship Analysis", 
           actionButton(inputId = "info_battleship", label = "", icon = icon("info-circle"), 
                       style = "background: none; border: none; color: #3498db; float: right;",
                       onclick = "Shiny.setInputValue('show_info_battleship', Math.random());")),
        p(tr("battleship.need.seriation")),
        tags$ol(
          tags$li(tr("battleship.steps.goto"), " ", strong("Seriation"), " Tab"),
          tags$li(tr("battleship.steps.perform")),
          tags$li(tr("battleship.steps.return"))
        ),
        p(em(tr("battleship.description")),
          style = "color: #7f8c8d; margin-top: 15px;")
      )
    } else {
      m <- get_current_matrix_for_bship()
      
      tagList(
        div(class = "alert alert-success", style = "padding: 10px; margin-bottom: 15px;",
            tr("battleship.seriation.active")),
        
        div(style = "margin-bottom: 10px;",
            actionButton("bship_all", tr("battleship.btn.all"), class = "btn-primary btn-sm", style = "margin-right: 5px;"),
            actionButton("bship_none", tr("battleship.btn.none"), class = "btn-secondary btn-sm")),

        selectizeInput("bship_types", tr("battleship.types.label"),
                      choices = colnames(m), selected = character(0), multiple = TRUE,
                      options = list(placeholder = tr("battleship.types.placeholder"),
                                   plugins = list('remove_button', 'drag_drop')))
      )
    }
  })
  
  # Info modal
  observeEvent(input$show_info_battleship, {
    showModal(modalDialog(
      title = tr("battleship.modal.title"),
      HTML(paste0("
        <h5>", tr("battleship.modal.theory.title"), "</h5>
        <p>", tr("battleship.modal.theory.text"), "</p>

        <h5>", tr("battleship.modal.metaphor.title"), "</h5>
        <ul>
          <li><strong>", tr("battleship.modal.metaphor.rise"), "</strong> ", tr("battleship.modal.metaphor.rise.text"), "</li>
          <li><strong>", tr("battleship.modal.metaphor.max"), "</strong> ", tr("battleship.modal.metaphor.max.text"), "</li>
          <li><strong>", tr("battleship.modal.metaphor.decline"), "</strong> ", tr("battleship.modal.metaphor.decline.text"), "</li>
        </ul>

        <h5>", tr("battleship.modal.interpretation.title"), "</h5>
        <p><strong>", tr("battleship.modal.interpretation.width"), "</strong> ", tr("battleship.modal.interpretation.width.text"), " • <strong>", tr("battleship.modal.interpretation.position"), "</strong> ", tr("battleship.modal.interpretation.position.text"), "<br>
        <strong>", tr("battleship.modal.interpretation.overlap"), "</strong> ", tr("battleship.modal.interpretation.overlap.text"), "</p>

        <h5>", tr("battleship.modal.application.title"), "</h5>
        <p>", tr("battleship.modal.application.text"), "</p>
      ")),
      easyClose = TRUE, footer = modalButton(tr("battleship.modal.btn.ok"))
    ))
  })
  
  # Button actions
  observeEvent(input$bship_all, {
    m <- get_current_matrix_for_bship()
    updateSelectizeInput(session, "bship_types", selected = colnames(m))
  })
  
  observeEvent(input$bship_none, {
    updateSelectizeInput(session, "bship_types", selected = character(0))
  })
  
  # Main battleship plot
  output$battleship_plot <- plotly::renderPlotly({
    has_seriation <- !is.null(cache$seriation_result$permuted_matrix)
    
    if (!has_seriation) {
      return(
        plotly::plot_ly() %>%
          plotly::layout(
            title = list(text = "SeriARC Battleship Analysis", font = list(size = 20), x = 0.5),
            xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
            annotations = list(list(
              x = 0.5, y = 0.5,
              text = "Seriation required<br><br>Go to the Seriation Tab",
              showarrow = FALSE, font = list(size = 16, color = "#34495e")
            ))
          )
      )
    }
    
    # Battleship analysis
    m <- get_current_matrix_for_bship()
    sel <- input$bship_types
    validate(need(length(sel) > 0, tr("validate.battleship.select.types")))
    
    sel <- intersect(colnames(m), sel)
    m <- m[, sel, drop = FALSE]

    # Cluster aggregation
    type_cluster_ids <- NULL
    
    if (!is.null(input$bship_aggregate_clusters) && input$bship_aggregate_clusters && 
        !is.null(cache$kmeans_result$active_data)) {
      df_sites <- subset(cache$kmeans_result$active_data, type == 'Site')
      site2cl <- setNames(as.character(df_sites$cluster), df_sites$label)
      common_sites <- intersect(rownames(m), names(site2cl))
      
      if (length(common_sites) > 0) {
        m_sub <- m[common_sites, , drop = FALSE]
        cl_ids <- site2cl[rownames(m_sub)]
        m <- as.matrix(rowsum(m_sub, group = cl_ids, reorder = FALSE))
        rownames(m) <- sapply(rownames(m), get_cluster_name)
      }
    }
    
    if (!is.null(input$bship_aggregate_types) && input$bship_aggregate_types &&
        !is.null(cache$kmeans_result$active_data)) {
      df_types <- subset(cache$kmeans_result$active_data, type == 'Type')
      type2cl <- setNames(as.character(df_types$cluster), df_types$label)
      common_types <- intersect(colnames(m), names(type2cl))

      if (length(common_types) > 0) {
        m_sub <- m[, common_types, drop = FALSE]
        cl_ids <- type2cl[colnames(m_sub)]
        m_t_agg <- as.matrix(rowsum(t(m_sub), group = cl_ids, reorder = FALSE))
        m <- t(m_t_agg)
        type_cluster_ids <- colnames(m)
        colnames(m) <- sapply(colnames(m), get_cluster_name)
      }
    }

    # Site group aggregation (uses get_site_group from server_helpers)
    site_group_ids <- NULL

    if (!is.null(input$bship_aggregate_type_groups) && input$bship_aggregate_type_groups) {
      if (!is.null(get_site_group)) {
        # Get site groups for all rows
        site_groups <- get_site_group(rownames(m))

        # Only aggregate sites with defined groups
        has_group <- !is.na(site_groups)

        if (any(has_group)) {
          m_sub <- m[has_group, , drop = FALSE]
          group_ids <- site_groups[has_group]

          # Aggregate by groups (sites are rows)
          m <- as.matrix(rowsum(m_sub, group = group_ids, reorder = FALSE))
          site_group_ids <- rownames(m)
          # Keep group names as row names
        } else {
          showNotification(
            tr("notify.battleship.groups.unavailable"),
            type = "warning",
            duration = 5
          )
        }
      } else {
        showNotification(
          "get_site_group function not available.",
          type = "error",
          duration = 5
        )
      }
    }

    # Normalization
    if (!is.null(input$bship_norm) && input$bship_norm) {
      mx <- apply(m, 2, function(v) max(v, na.rm = TRUE))
      mx[mx == 0] <- 1
      m <- sweep(m, 2, mx, "/")
    }

    hw <- pmin(pmax(m, 0), 1) * (input$bship_halfwidth %||% 0.8)
    cutn <- max(3, min(30, as.integer(input$bship_label_chars %||% 20)))
    row_labs <- substr(rownames(hw), 1, cutn)
    col_labs <- substr(colnames(hw), 1, cutn)
    flip <- !is.null(input$bship_flip_axes) && input$bship_flip_axes

    # Adaptive font size and label thinning for many sites/types
    n_sites <- nrow(hw)
    n_types <- ncol(hw)

    # Adaptive font size: smaller with more labels
    site_font_size <- max(7, min(12, 120 / n_sites))
    type_font_size <- max(7, min(12, 120 / n_types))

    # Label thinning: show every n-th label when >40 labels
    if (n_sites > 40) {
      skip <- ceiling(n_sites / 40)
      site_tickvals <- seq(1, n_sites, by = skip)
      site_ticktext <- row_labs[site_tickvals]
    } else {
      site_tickvals <- seq_len(n_sites)
      site_ticktext <- row_labs
    }

    if (n_types > 40) {
      skip <- ceiling(n_types / 40)
      type_tickvals <- seq(1, n_types, by = skip)
      type_ticktext <- col_labs[type_tickvals]
    } else {
      type_tickvals <- seq_len(n_types)
      type_ticktext <- col_labs
    }

    use_cluster_colors <- !is.null(input$bship_aggregate_types) &&
                         input$bship_aggregate_types &&
                         !is.null(type_cluster_ids)

    p <- plotly::plot_ly(type = "scatter", mode = "lines")

    if (flip) {
      # Flip mode: x = Sites, y = Types
      x_seq <- seq_len(nrow(hw))
      y_pos <- seq_len(ncol(hw))

      for (i in seq_len(ncol(hw))) {
        w <- as.numeric(hw[, i])

        if (!is.null(input$bship_curve_smooth) && input$bship_curve_smooth) {
          sm <- smooth_series(x_seq, w,
                             method = input$bship_curve_method %||% "loess",
                             param = input$bship_curve_param %||% 0.3)
          poly <- make_polygon(sm$x, sm$y, center = y_pos[i], "horizontal")
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
        } else {
          poly <- make_polygon(x_seq, w, center = y_pos[i], "horizontal")
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
        }

        type_color <- if (use_cluster_colors && i <= length(type_cluster_ids)) {
          get_cluster_color(type_cluster_ids[i])
        } else NULL

        p <- plotly::add_polygons(p, x = poly$x, y = poly$y, name = colnames(m)[i],
                                 hoverinfo = "text", text = hover_text,
                                 opacity = input$bship_alpha %||% 0.6,
                                 fillcolor = type_color)
      }

      p <- plotly::layout(p,
                         xaxis = list(title = tr("plot.battleship.xlab.cluster"),
                                    tickmode = "array", tickvals = site_tickvals, ticktext = site_ticktext,
                                    tickfont = list(size = site_font_size),
                                    fixedrange = TRUE),
                         yaxis = list(title = tr("term.types"), tickmode = "array",
                                    tickvals = type_tickvals, ticktext = type_ticktext,
                                    tickfont = list(size = type_font_size), autorange = "reversed",
                                    fixedrange = TRUE))
    } else {
      # Standard mode: x = Types, y = Sites
      x_seq <- seq_len(ncol(hw))
      y_pos <- seq_len(nrow(hw))

      for (i in seq_len(ncol(hw))) {
        w <- as.numeric(hw[, i])

        if (!is.null(input$bship_curve_smooth) && input$bship_curve_smooth) {
          sm <- smooth_series(y_pos, w,
                             method = input$bship_curve_method %||% "loess",
                             param = input$bship_curve_param %||% 0.3)
          poly <- make_polygon(sm$x, sm$y, center = i, "vertical")
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
        } else {
          poly <- make_polygon(y_pos, w, center = i, "vertical")
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
        }

        type_color <- if (use_cluster_colors && i <= length(type_cluster_ids)) {
          get_cluster_color(type_cluster_ids[i])
        } else NULL

        p <- plotly::add_polygons(p, x = poly$x, y = poly$y, name = colnames(m)[i],
                                 hoverinfo = "text", text = hover_text,
                                 opacity = input$bship_alpha %||% 0.6,
                                 fillcolor = type_color)
      }

      p <- plotly::layout(p,
                         xaxis = list(title = tr("term.types"), tickmode = "array",
                                    tickvals = type_tickvals, ticktext = type_ticktext,
                                    tickfont = list(size = type_font_size),
                                    fixedrange = TRUE),
                         yaxis = list(title = tr("plot.battleship.xlab.cluster"),
                                    tickmode = "array", tickvals = site_tickvals,
                                    ticktext = site_ticktext, tickfont = list(size = site_font_size),
                                    autorange = "reversed", fixedrange = TRUE))
    }

    p %>% plotly::layout(
      title = sprintf("%s • %s", tr("plot.battleship.title.curves"),
                     if(flip) "Sites × Types" else "Types × Sites"),
      showlegend = FALSE,
      margin = list(l = 80, r = 20, t = 60, b = 120)
    ) %>%
    standard_plotly_config(., "2d")
  })
  
  # ===== BATTLESHIP EXPORT HANDLER =====

  # Prepare battleship data for export
  battleship_export_data <- reactive({
    req(cache$seriation_result$permuted_matrix)
    
    m <- get_current_matrix_for_bship()
    selected_types <- input$bship_types
    
    if (is.null(selected_types) || length(selected_types) == 0) {
      return(NULL)
    }
    
    # Filter to selected types
    selected_types <- intersect(colnames(m), selected_types)
    if (length(selected_types) == 0) return(NULL)
    
    m_filtered <- m[, selected_types, drop = FALSE]
    
    # Apply cluster aggregation (if enabled)
    if (!is.null(input$bship_aggregate_clusters) && input$bship_aggregate_clusters && 
        !is.null(cache$kmeans_result$active_data)) {
      df_sites <- subset(cache$kmeans_result$active_data, type == 'Site')
      site2cl <- setNames(as.character(df_sites$cluster), df_sites$label)
      common_sites <- intersect(rownames(m_filtered), names(site2cl))
      
      if (length(common_sites) > 0) {
        m_sub <- m_filtered[common_sites, , drop = FALSE]
        cl_ids <- site2cl[rownames(m_sub)]
        m_filtered <- as.matrix(rowsum(m_sub, group = cl_ids, reorder = FALSE))
        rownames(m_filtered) <- paste0("Cluster_", sapply(rownames(m_filtered), get_cluster_name))
      }
    }
    
    # Apply normalization (if enabled)
    if (!is.null(input$bship_norm) && input$bship_norm) {
      mx <- apply(m_filtered, 2, function(v) max(v, na.rm = TRUE))
      mx[mx == 0] <- 1
      m_filtered <- sweep(m_filtered, 2, mx, "/")
    }
    
    list(
      matrix = m_filtered,
      types_selected = selected_types,
      normalized = !is.null(input$bship_norm) && input$bship_norm,
      cluster_aggregated = !is.null(input$bship_aggregate_clusters) && input$bship_aggregate_clusters,
      flip_axes = !is.null(input$bship_flip_axes) && input$bship_flip_axes
    )
  })
  
  # PNG export for battleship (with actual data)
  output$download_battleship_png <- create_png_download_handler(
    "battleship_plot", "SeriARC_Battleship_Curves", session,
    plot_data = battleship_export_data,
    tr = tr,
    plot_generator_func = function(data) {
      if (!is.null(data) && !is.null(data$matrix) && nrow(data$matrix) > 0 && ncol(data$matrix) > 0) {
        m <- data$matrix
        hw <- pmin(pmax(m, 0), 1) * (input$bship_halfwidth %||% 0.8)
        
        par(mar = c(6, 5, 4, 2))
        
        if (data$flip_axes) {
          # Sites horizontal, types vertical
          plot(0, 0, type = "n", xlim = c(0.5, nrow(hw) + 0.5), ylim = c(0.5, ncol(hw) + 0.5),
               xlab = tr("term.sites.chronological"), ylab = tr("term.artifact.types"),
               main = tr("plot.battleship.title.curves"), axes = FALSE)

          # Axes
          axis(1, at = 1:nrow(hw), labels = substr(rownames(hw), 1, 8), las = 2, cex.axis = 0.8)
          axis(2, at = 1:ncol(hw), labels = substr(colnames(hw), 1, 6), las = 2, cex.axis = 0.8)

          # Draw battleship curves
          colors <- rainbow(ncol(hw), alpha = 0.7)
          for (i in 1:ncol(hw)) {
            w <- as.numeric(hw[, i])
            x_seq <- 1:nrow(hw)
            y_pos <- rep(i, length(x_seq))
            polygon(c(x_seq, rev(x_seq)), c(y_pos + w, rev(y_pos - w)), 
                   col = colors[i], border = rainbow(ncol(hw))[i], lwd = 1.5)
          }
        } else {
          # Types horizontal, sites vertical (standard)
          plot(0, 0, type = "n", xlim = c(0.5, ncol(hw) + 0.5), ylim = c(0.5, nrow(hw) + 0.5),
               xlab = tr("term.artifact.types"), ylab = tr("term.sites.chronological"),
               main = tr("plot.battleship.title.curves"), axes = FALSE)

          # Axes
          axis(1, at = 1:ncol(hw), labels = substr(colnames(hw), 1, 8), las = 2, cex.axis = 0.8)
          axis(2, at = 1:nrow(hw), labels = substr(rownames(hw), 1, 8), las = 1, cex.axis = 0.8)

          # Draw battleship curves
          colors <- rainbow(ncol(hw), alpha = 0.7)
          for (i in 1:ncol(hw)) {
            w <- as.numeric(hw[, i])
            y_seq <- 1:nrow(hw)
            x_pos <- rep(i, length(y_seq))

            # Only draw sites with value > 0 (no "empty" bellies)
            has_value <- w > 0
            if (any(has_value)) {
              # Find contiguous regions with values
              first_val <- min(which(has_value))
              last_val <- max(which(has_value))

              # Only draw relevant region
              y_range <- first_val:last_val
              w_range <- w[y_range]
              x_range <- rep(i, length(y_range))
              
              polygon(c(x_range - w_range, rev(x_range + w_range)), 
                     c(y_range, rev(y_range)), 
                     col = colors[i], border = rainbow(ncol(hw))[i], lwd = 1.5)
            }
          }
        }
        
        # Info text
        norm_text <- if (data$normalized) "(normalized)" else ""
        cluster_text <- if (data$cluster_aggregated) "(cluster-aggregated)" else ""
        mtext(sprintf("PNG Export | %d Types %s %s | %s", ncol(m), norm_text, cluster_text, format(Sys.time(), "%Y-%m-%d %H:%M")), 
              side = 1, line = 5, cex = 0.7, col = "gray")
      }
    }
  )
  
  # SVG export for battleship
  output$download_battleship_svg <- create_svg_download_handler(
    "battleship_plot", "SeriARC_Battleship_Curves", session,
    plot_data = battleship_export_data,
    tr = tr,
    plot_generator_func = function(data) {
      if (!is.null(data) && !is.null(data$matrix) && nrow(data$matrix) > 0 && ncol(data$matrix) > 0) {
        m <- data$matrix
        hw <- pmin(pmax(m, 0), 1) * (input$bship_halfwidth %||% 0.8)
        
        par(mar = c(6, 5, 4, 2))
        
        if (data$flip_axes) {
          # Sites horizontal, types vertical
          plot(0, 0, type = "n", xlim = c(0.5, nrow(hw) + 0.5), ylim = c(0.5, ncol(hw) + 0.5),
               xlab = tr("term.sites.chronological"), ylab = tr("term.artifact.types"),
               main = paste(tr("plot.battleship.title.curves"), "- SVG Export"), axes = FALSE)

          # Axes
          axis(1, at = 1:nrow(hw), labels = substr(rownames(hw), 1, 10), las = 2, cex.axis = 0.8)
          axis(2, at = 1:ncol(hw), labels = substr(colnames(hw), 1, 8), las = 2, cex.axis = 0.8)

          # Draw battleship curves
          for (i in 1:ncol(hw)) {
            w <- as.numeric(hw[, i])
            x_seq <- 1:nrow(hw)
            y_pos <- rep(i, length(x_seq))

            # Upper and lower curves
            upper_y <- y_pos + w
            lower_y <- y_pos - w

            # Draw polygons
            polygon(c(x_seq, rev(x_seq)), c(upper_y, rev(lower_y)), 
                   col = paste0(rainbow(ncol(hw))[i], "80"), border = rainbow(ncol(hw))[i])
          }
        } else {
          # Types horizontal, sites vertical (standard)
          plot(0, 0, type = "n", xlim = c(0.5, ncol(hw) + 0.5), ylim = c(0.5, nrow(hw) + 0.5),
               xlab = tr("term.artifact.types"), ylab = tr("term.sites.chronological"),
               main = paste(tr("plot.battleship.title.curves"), "- SVG Export"), axes = FALSE)

          # Axes
          axis(1, at = 1:ncol(hw), labels = substr(colnames(hw), 1, 8), las = 2, cex.axis = 0.8)
          axis(2, at = 1:nrow(hw), labels = substr(rownames(hw), 1, 10), las = 1, cex.axis = 0.8)

          # Draw battleship curves
          for (i in 1:ncol(hw)) {
            w <- as.numeric(hw[, i])
            y_seq <- 1:nrow(hw)
            x_pos <- rep(i, length(y_seq))

            # Only draw sites with value > 0
            has_value <- w > 0
            if (any(has_value)) {
              first_val <- min(which(has_value))
              last_val <- max(which(has_value))

              y_range <- first_val:last_val
              w_range <- w[y_range]
              x_range <- rep(i, length(y_range))

              # Left and right curves
              left_x <- x_range - w_range
              right_x <- x_range + w_range

              # Draw polygons
              polygon(c(left_x, rev(right_x)), c(y_range, rev(y_range)), 
                     col = paste0(rainbow(ncol(hw))[i], "80"), border = rainbow(ncol(hw))[i])
            }
          }
        }
        
        # Info text
        norm_text <- if (data$normalized) "(normalized)" else ""
        cluster_text <- if (data$cluster_aggregated) "(aggregated)" else ""
        mtext(sprintf("SVG: %d Types %s %s", ncol(m), norm_text, cluster_text),
              side = 1, line = 5, cex = 0.8, col = "gray")
      }
    }
  )
  
  # PDF export for battleship
  output$download_battleship_pdf <- create_pdf_download_handler(
    battleship_export_data, "SeriARC_Battleship_Curves",
    tr = tr,
    plot_generator_func = function(data) {
      if (!is.null(data) && !is.null(data$matrix) && nrow(data$matrix) > 0 && ncol(data$matrix) > 0) {
        m <- data$matrix
        hw <- pmin(pmax(m, 0), 1) * (input$bship_halfwidth %||% 0.8)
        
        if (data$flip_axes) {
          plot(0, 0, type = "n", xlim = c(0.5, nrow(hw) + 0.5), ylim = c(0.5, ncol(hw) + 0.5),
               xlab = tr("term.sites.chronological"), ylab = tr("term.artifact.types"),
               main = tr("plot.battleship.title.curves"))

          for (i in 1:ncol(hw)) {
            w <- as.numeric(hw[, i])
            x_seq <- 1:nrow(hw)
            y_pos <- rep(i, length(x_seq))
            polygon(c(x_seq, rev(x_seq)), c(y_pos + w, rev(y_pos - w)),
                   col = paste0(rainbow(ncol(hw))[i], "60"), border = rainbow(ncol(hw))[i])
          }
        } else {
          plot(0, 0, type = "n", xlim = c(0.5, ncol(hw) + 0.5), ylim = c(0.5, nrow(hw) + 0.5),
               xlab = tr("term.artifact.types"), ylab = tr("term.sites.chronological"),
               main = tr("plot.battleship.title.curves"))
          
          for (i in 1:ncol(hw)) {
            w <- as.numeric(hw[, i])
            y_seq <- 1:nrow(hw)
            x_pos <- rep(i, length(y_seq))
            
            # Only draw sites with value > 0
            has_value <- w > 0
            if (any(has_value)) {
              first_val <- min(which(has_value))
              last_val <- max(which(has_value))
              
              y_range <- first_val:last_val
              w_range <- w[y_range]
              x_range <- rep(i, length(y_range))
              
              polygon(c(x_range - w_range, rev(x_range + w_range)), c(y_range, rev(y_range)), 
                     col = paste0(rainbow(ncol(hw))[i], "60"), border = rainbow(ncol(hw))[i])
            }
          }
        }
        
        norm_text <- if (data$normalized) "(normalized)" else ""
        cluster_text <- if (data$cluster_aggregated) "(aggregated)" else ""
        mtext(sprintf("%d Types %s %s", ncol(m), norm_text, cluster_text),
              side = 1, line = 4, cex = 0.8, col = "gray")
      } else {
        plot(1, 1, type = "n", xlab = "", ylab = "", main = tr("plot.battleship.nodata"))
      }
    }
  )

  # HTML export for battleship (Plotly HTML)
  output$download_battleship_html <- downloadHandler(
    filename = function() sprintf("SeriARC_Battleship_Curves_%s.html", Sys.Date()),
    content = function(file) {
      showNotification("🌐 Battleship HTML Export...", type = "message", duration = 3)

      tryCatch({
        export_data <- battleship_export_data()

        if (is.null(export_data) || is.null(export_data$matrix)) {
          stop("No Battleship data for HTML export")
        }

        # Simplified HTML export with Plotly
        m <- export_data$matrix

        # Prepare data for Plotly
        melted_data <- reshape2::melt(as.matrix(m))
        colnames(melted_data) <- c("Site", "Type", "Value")

        p <- plot_ly(
          melted_data,
          x = ~Type, y = ~Site, z = ~Value,
          type = "heatmap", colorscale = "Viridis",
          hovertemplate = "<b>%{y}</b><br>Type: %{x}<br>Value: %{z:.3f}<extra></extra>"
        ) %>%
          layout(
            title = list(text = tr("plot.battleship.html.title"),
                        font = list(size = 18, family = "Arial")),
            xaxis = list(title = tr("term.artifact.types"), tickangle = -45),
            yaxis = list(title = tr("term.sites.chronological"), autorange = "reversed"),
            plot_bgcolor = "white", paper_bgcolor = "white"
          )
        
        htmlwidgets::saveWidget(p, file, selfcontained = TRUE)

        showNotification("Battleship HTML export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("Battleship HTML export error:", e$message), type = "error", duration = 5)
      })
    }
  )
  
  # Excel export for battleship
  output$download_battleship_data <- create_excel_download_handler(
    function() {
      export_data <- battleship_export_data()
      
      if (is.null(export_data) || is.null(export_data$matrix)) {
        return(list("Info" = data.frame(Info = tr("battleship.no.data.available"))))
      }
      
      m <- export_data$matrix
      
      sheets <- list()
      
      # Battleship matrix
      battleship_df <- data.frame(
        Site_or_Cluster = rownames(m),
        m,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      sheets[["Battleship_Matrix"]] <- battleship_df
      
      # Parameters sheet
      params_df <- data.frame(
        Parameter = c("Types_selected", "Normalized", "Cluster_aggregated", "Axes_flipped", "Exported_at"),
        Value = c(
          paste(export_data$types_selected, collapse = ", "),
          if (export_data$normalized) "Yes" else "No",
          if (export_data$cluster_aggregated) "Yes" else "No",
          if (export_data$flip_axes) "Yes" else "No",
          format(Sys.time(), "%Y-%m-%d %H:%M:%S")
        ),
        stringsAsFactors = FALSE
      )
      sheets[["Battleship_Parameter"]] <- params_df

      # Statistics
      stats_df <- data.frame(
        Metric = c("Sites_count", "Types_count", "Max_value", "Min_value", "Average"),
        Value = c(
          nrow(m),
          ncol(m),
          round(max(m, na.rm = TRUE), 3),
          round(min(m, na.rm = TRUE), 3),
          round(mean(m, na.rm = TRUE), 3)
        ),
        stringsAsFactors = FALSE
      )
      sheets[["Battleship_Statistics"]] <- stats_df
      
      return(sheets)
    },
    "Battleship_Curves", "Battleship Curve Analysis",
    tr = tr
  )
  
  # CSV export for battleship
  output$download_battleship_data_csv <- create_csv_download_handler(
    function() {
      export_data <- battleship_export_data()
      
      if (is.null(export_data) || is.null(export_data$matrix)) {
        return(data.frame(Info = tr("battleship.no.data.available")))
      }
      
      m <- export_data$matrix
      
      melted_data <- reshape2::melt(as.matrix(m))
      colnames(melted_data) <- c("Site_or_Cluster", "Type", "Value")
      melted_data$Normalized <- export_data$normalized
      melted_data$Cluster_Aggregated <- export_data$cluster_aggregated
      
      return(melted_data)
    },
    "Battleship_Data", add_header = TRUE
  )
  
  return(list(get_current_matrix = get_current_matrix_for_bship))
}

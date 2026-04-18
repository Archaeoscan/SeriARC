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

    # Site ordering (seriation / CA Dim1 / arc length)
    order_by <- if (!is.null(input$bship_order_by)) input$bship_order_by else "seriation"

    if (order_by == "dim1" &&
        !is.null(cache$ca_result) &&
        !is.null(cache$ca_result$row) &&
        !is.null(cache$ca_result$row$coord)) {

      dim1_coords <- cache$ca_result$row$coord[, 1]
      sites_in_m  <- rownames(m)
      matched     <- dim1_coords[names(dim1_coords) %in% sites_in_m]

      if (length(matched) >= 2) {
        ordered_sites <- names(sort(matched))
        ordered_sites <- ordered_sites[ordered_sites %in% sites_in_m]
        remaining     <- setdiff(sites_in_m, ordered_sites)
        m <- m[c(ordered_sites, remaining), , drop = FALSE]
      }

    } else if (order_by == "arc_length" &&
        !is.null(cache$polynomial_result) &&
        !is.null(cache$polynomial_result$arc_length)) {

      al_df <- cache$polynomial_result$arc_length  # data.frame: Site, ArcLength
      sites_in_m <- rownames(m)
      matched <- al_df[al_df$Site %in% sites_in_m, , drop = FALSE]

      if (nrow(matched) >= 2) {
        ordered_sites <- matched$Site[order(matched$ArcLength)]
        ordered_sites <- ordered_sites[ordered_sites %in% sites_in_m]
        remaining <- setdiff(sites_in_m, ordered_sites)
        m <- m[c(ordered_sites, remaining), , drop = FALSE]
      }
    }

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
  
  # Main battleship plot (shared reactive so HTML export can reuse it 1:1)
  battleship_plot_obj <- reactive({
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

    display_mode <- input$bship_display_mode %||% "ships"
    p <- plotly::plot_ly(type = "scatter", mode = "lines")

    # ---- helper: build block polygons for one type column (NA-separated rectangles) ----
    # gap = 0.5 → blocks touch seamlessly; < 0.5 → visible gap between rows
    make_blocks <- function(w_vec, axis_pos, center, orientation = "vertical", gap = 0.5) {
      xs <- c()
      ys <- c()
      for (j in seq_along(w_vec)) {
        w <- w_vec[j]
        if (w > 0) {
          if (orientation == "vertical") {
            xs <- c(xs, center - w, center + w, center + w, center - w, center - w, NA)
            ys <- c(ys, axis_pos[j] - gap, axis_pos[j] - gap,
                        axis_pos[j] + gap, axis_pos[j] + gap, axis_pos[j] - gap, NA)
          } else {
            xs <- c(xs, axis_pos[j] - gap, axis_pos[j] - gap,
                        axis_pos[j] + gap, axis_pos[j] + gap, axis_pos[j] - gap, NA)
            ys <- c(ys, center - w, center + w, center + w, center - w, center - w, NA)
          }
        }
      }
      list(x = xs, y = ys)
    }

    # Default fill color for blocks mode (single uniform blue, Petrie-style)
    blocks_default_color <- "#2980b9"

    if (flip) {
      # Flip mode: x = Sites, y = Types
      x_seq <- seq_len(nrow(hw))
      y_pos <- seq_len(ncol(hw))

      for (i in seq_len(ncol(hw))) {
        w <- as.numeric(hw[, i])

        if (display_mode == "blocks") {
          poly <- make_blocks(w, x_seq, center = y_pos[i], orientation = "horizontal")
          type_color <- if (use_cluster_colors && i <= length(type_cluster_ids)) {
            get_cluster_color(type_cluster_ids[i])
          } else blocks_default_color
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
          p <- plotly::add_polygons(p, x = poly$x, y = poly$y, name = colnames(m)[i],
                                   hoverinfo = "text", text = hover_text,
                                   opacity = input$bship_alpha %||% 0.8,
                                   fillcolor = type_color,
                                   line = list(color = "white", width = 0.5))
        } else {
          if (!is.null(input$bship_curve_smooth) && input$bship_curve_smooth) {
            sm <- smooth_series(x_seq, w,
                               method = input$bship_curve_method %||% "loess",
                               param = input$bship_curve_param %||% 0.3)
            poly <- make_polygon(sm$x, sm$y, center = y_pos[i], "horizontal")
          } else {
            poly <- make_polygon(x_seq, w, center = y_pos[i], "horizontal")
          }
          type_color <- if (use_cluster_colors && i <= length(type_cluster_ids)) {
            get_cluster_color(type_cluster_ids[i])
          } else NULL
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
          p <- plotly::add_polygons(p, x = poly$x, y = poly$y, name = colnames(m)[i],
                                   hoverinfo = "text", text = hover_text,
                                   opacity = input$bship_alpha %||% 0.6,
                                   fillcolor = type_color)
        }
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

        if (display_mode == "blocks") {
          poly <- make_blocks(w, y_pos, center = i, orientation = "vertical")
          type_color <- if (use_cluster_colors && i <= length(type_cluster_ids)) {
            get_cluster_color(type_cluster_ids[i])
          } else blocks_default_color
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
          p <- plotly::add_polygons(p, x = poly$x, y = poly$y, name = colnames(m)[i],
                                   hoverinfo = "text", text = hover_text,
                                   opacity = input$bship_alpha %||% 0.8,
                                   fillcolor = type_color,
                                   line = list(color = "white", width = 0.5))
        } else {
          if (!is.null(input$bship_curve_smooth) && input$bship_curve_smooth) {
            sm <- smooth_series(y_pos, w,
                               method = input$bship_curve_method %||% "loess",
                               param = input$bship_curve_param %||% 0.3)
            poly <- make_polygon(sm$x, sm$y, center = i, "vertical")
          } else {
            poly <- make_polygon(y_pos, w, center = i, "vertical")
          }
          type_color <- if (use_cluster_colors && i <= length(type_cluster_ids)) {
            get_cluster_color(type_cluster_ids[i])
          } else NULL
          hover_text <- sprintf(tr("battleship.hover.format"), colnames(m)[i], "", 0)
          p <- plotly::add_polygons(p, x = poly$x, y = poly$y, name = colnames(m)[i],
                                   hoverinfo = "text", text = hover_text,
                                   opacity = input$bship_alpha %||% 0.6,
                                   fillcolor = type_color)
        }
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

    mode_label <- if (display_mode == "blocks") tr("bship.display.blocks") else tr("bship.display.ships")
    p %>% plotly::layout(
      title = sprintf("%s [%s] • %s", tr("plot.battleship.title.curves"),
                     mode_label,
                     if(flip) "Sites × Types" else "Types × Sites"),
      showlegend = FALSE,
      margin = list(l = 80, r = 20, t = 60, b = 120)
    ) %>%
    standard_plotly_config(., "2d")
  })

  output$battleship_plot <- plotly::renderPlotly({
    battleship_plot_obj()
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
  # PNG export: webshot2 renders the actual plotly figure (1:1 with screen)
  output$download_battleship_png <- downloadHandler(
    filename = function() sprintf("SeriARC_Battleship_Curves_%s.png", Sys.Date()),
    content = function(file) {
      tryCatch({
        p <- battleship_plot_obj()
        tmp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(plotly::as_widget(p), tmp_html, selfcontained = TRUE)

        if (requireNamespace("webshot2", quietly = TRUE)) {
          webshot2::webshot(tmp_html, file, vwidth = 1400, vheight = 900, delay = 1.5)
        } else if (requireNamespace("webshot", quietly = TRUE)) {
          webshot::webshot(tmp_html, file, vwidth = 1400, vheight = 900, delay = 1.5)
        } else {
          showNotification(
            "webshot2 nicht installiert. Für PNG-Export: install.packages('webshot2'). Nutze stattdessen das Kamera-Symbol im Plot.",
            type = "warning", duration = 8
          )
        }
        file.remove(tmp_html)
        showNotification(tr("export.png.complete") %||% "PNG export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("PNG export error:", e$message), type = "error", duration = 5)
      })
    }
  )

  # SVG export: base R rendering into SVG device
  output$download_battleship_svg <- downloadHandler(
    filename = function() sprintf("SeriARC_Battleship_Curves_%s.svg", Sys.Date()),
    content = function(file) {
      tryCatch({
        data <- battleship_export_data()
        req(!is.null(data), !is.null(data$matrix))
        m  <- data$matrix
        hw <- pmin(pmax(m, 0), 1) * (input$bship_halfwidth %||% 0.8)
        n_types <- ncol(hw); n_sites <- nrow(hw)
        svg(file,
            width  = max(12, min(60, n_types * 0.35)),
            height = max(8,  min(40, n_sites * 0.25)))
        par(mar = c(7, 6, 4, 2))
        colors <- rainbow(n_types, alpha = 0.75)

        if (data$flip_axes) {
          plot(0, 0, type = "n",
               xlim = c(0.5, n_sites + 0.5), ylim = c(0.5, n_types + 0.5),
               xlab = tr("term.sites.chronological"), ylab = tr("term.artifact.types"),
               main = tr("plot.battleship.title.curves"), axes = FALSE)
          axis(1, at = seq_len(n_sites), labels = substr(rownames(hw), 1, 10), las = 2, cex.axis = 0.7)
          axis(2, at = seq_len(n_types), labels = substr(colnames(hw), 1, 10), las = 2, cex.axis = 0.7)
          for (i in seq_len(n_types)) {
            w <- as.numeric(hw[, i])
            xs <- seq_len(n_sites)
            polygon(c(xs, rev(xs)), c(i + w, rev(i - w)), col = colors[i], border = colors[i])
          }
        } else {
          plot(0, 0, type = "n",
               xlim = c(0.5, n_types + 0.5), ylim = c(0.5, n_sites + 0.5),
               xlab = tr("term.artifact.types"), ylab = tr("term.sites.chronological"),
               main = tr("plot.battleship.title.curves"), axes = FALSE)
          axis(1, at = seq_len(n_types), labels = substr(colnames(hw), 1, 10), las = 2, cex.axis = 0.7)
          axis(2, at = seq_len(n_sites), labels = substr(rownames(hw), 1, 10), las = 1, cex.axis = 0.7)
          for (i in seq_len(n_types)) {
            w <- as.numeric(hw[, i])
            ys <- seq_len(n_sites)
            polygon(c(i - w, rev(i + w)), c(ys, rev(ys)), col = colors[i], border = colors[i])
          }
        }
        dev.off()
        showNotification(tr("export.svg.complete") %||% "SVG export completed!", type = "message", duration = 2)
      }, error = function(e) {
        if (exists("dev.list") && length(dev.list()) > 1) dev.off()
        showNotification(paste("SVG export error:", e$message), type = "error", duration = 5)
      })
    }
  )
  
  # PDF export: webshot2 renders the actual plotly figure (1:1 with screen)
  output$download_battleship_pdf <- downloadHandler(
    filename = function() sprintf("SeriARC_Battleship_Curves_%s.pdf", Sys.Date()),
    content = function(file) {
      tryCatch({
        p <- battleship_plot_obj()
        tmp_html <- tempfile(fileext = ".html")
        htmlwidgets::saveWidget(plotly::as_widget(p), tmp_html, selfcontained = TRUE)

        if (requireNamespace("webshot2", quietly = TRUE)) {
          webshot2::webshot(tmp_html, file, vwidth = 1400, vheight = 900, delay = 1.5)
        } else if (requireNamespace("webshot", quietly = TRUE)) {
          webshot::webshot(tmp_html, file, vwidth = 1400, vheight = 900, delay = 1.5)
        } else {
          # Fallback: package not available — save HTML instead and notify
          file.copy(tmp_html, sub("\\.pdf$", "_interactive.html", file))
          showNotification(
            "webshot2 nicht installiert. PDF-Export benötigt: install.packages('webshot2'). HTML wurde stattdessen gespeichert.",
            type = "warning", duration = 8
          )
        }
        file.remove(tmp_html)
        showNotification(tr("export.pdf.complete") %||% "PDF export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("PDF export error:", e$message), type = "error", duration = 5)
      })
    }
  )

  # HTML export: 1:1 copy of the interactive plotly figure shown on screen
  output$download_battleship_html <- downloadHandler(
    filename = function() sprintf("SeriARC_Battleship_Curves_%s.html", Sys.Date()),
    content = function(file) {
      tryCatch({
        p <- battleship_plot_obj()
        htmlwidgets::saveWidget(plotly::as_widget(p), file, selfcontained = TRUE)
        showNotification(tr("export.html.complete") %||% "HTML export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("HTML export error:", e$message), type = "error", duration = 5)
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

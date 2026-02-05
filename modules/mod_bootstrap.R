# mod_bootstrap.R - Bootstrap resampling for statistical validation

source("helpers/plot_components.R", local = TRUE)
source("helpers/download_components.R", local = TRUE) 
source("helpers/input_components.R", local = TRUE)
source("helpers/ellipse_helper.R", local = TRUE)

mod_bootstrap_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)

  tagList(
    bootstrap_options_ui(ns, default_iterations = 999),
    plot_display_options_ui(ns,
                           include_labels = TRUE,
                           include_colors = FALSE,
                           include_elements = TRUE,
                           include_size = TRUE),

    combined_download_buttons_ui(ns("downloads")),

    div(class = "mt-3",
      h5(tr("bootstrap.display")),
      checkboxInput(ns("show_bootstrap_labels"), tr("bootstrap.show.labels"), value = TRUE),
      conditionalPanel(
        condition = sprintf("input['%s'] === true", ns("show_bootstrap_labels")),
        sliderInput(ns("bootstrap_label_chars"), tr("bootstrap.label.length"), min = 3, max = 20, value = 12)
      )
    )
  )
}

mod_bootstrap_server <- function(filtered_data, meta_data, cache, get_site_group, get_element_details = NULL, ca_result = NULL, input, output, session, tr = function(x) x) {

  check_cabootcrs <- reactive({
    if (!requireNamespace("cabootcrs", quietly = TRUE)) {
      showNotification(tr("bootstrap.cabootcrs.required"), type = "error", duration = 8)
      return(FALSE)
    }
    TRUE
  })
  
  # Data model validation
  validate_data_model <- function(mat) {
    warnings <- c()
    
    has_negatives <- any(mat < 0, na.rm = TRUE)
    has_decimals <- any((mat %% 1) != 0, na.rm = TRUE)
    total_sum <- sum(mat, na.rm = TRUE)

    if (has_negatives) {
      warnings <- c(warnings, tr("bootstrap.negative.values"))
    }

    if (has_decimals && max(mat, na.rm = TRUE) < 10) {
      warnings <- c(warnings, tr("bootstrap.normalized.warning"))
    }

    if (total_sum < 50) {
      warnings <- c(warnings, tr("bootstrap.small.sum"))
    }

    sparsity <- sum(mat == 0, na.rm = TRUE) / length(mat) * 100
    if (sparsity > 95) {
      warnings <- c(warnings, sprintf(tr("bootstrap.sparse.warning"), sparsity))
    }
    
    list(valid = length(warnings) == 0, warnings = warnings, sparsity = sparsity)
  }
  
  # Invalidate cache when filtered_data changes
  observe({
    filtered_data()
    cache$bootstrap_result <- NULL
    cache$bootstrap_key <- NULL
  })
  
  bootstrap_res <- eventReactive(input$run_bootstrap, {
    req(filtered_data(), check_cabootcrs())
    
    # Cache validation: Check if cached matrix is still current
    current_mat <- as.matrix(filtered_data())
    if (requireNamespace("digest", quietly = TRUE)) {
      current_hash <- digest::digest(current_mat)
      if (!is.null(cache$bootstrap_key) && !identical(cache$bootstrap_key$data_hash, current_hash)) {
        cache$bootstrap_result <- NULL
        cache$bootstrap_key <- NULL
      }
    }
    
    withProgress(message = tr("bootstrap.running"), {
      tryCatch({
        mat <- current_mat
        
        n_iterations <- as.integer(input$bootstrap_iterations %||% 999)
        validate(need(!is.na(n_iterations) && n_iterations >= 10, tr("bootstrap.iterations.error")))

        confidence_percent <- as.numeric(input$bootstrap_confidence %||% 95)
        validate(need(!is.na(confidence_percent) && confidence_percent > 0 && confidence_percent < 100,
                     tr("bootstrap.confidence.error")))
        
        validate(need(sum(mat, na.rm = TRUE) > 0, tr("bootstrap.validate.matrix.data")))
        validate(need(nrow(mat) >= 3 && ncol(mat) >= 3, tr("bootstrap.validate.matrix.size")))
        
        data_check <- validate_data_model(mat)
        if (!data_check$valid) {
          for (warn in data_check$warnings) {
            showNotification(warn, type = "warning", duration = 8)
          }
        }
        
        req(meta_data$data)
        available_sites <- rownames(mat); available_types <- colnames(mat)
        all_sites <- meta_data$data$sites[meta_data$data$sites$Selected & 
                                         meta_data$data$sites$Entity %in% available_sites, , drop = FALSE]
        all_types <- meta_data$data$types[meta_data$data$types$Selected & 
                                         meta_data$data$types$Entity %in% available_types, , drop = FALSE]
        
        suppl_row_indices <- suppl_col_indices <- integer(0)
        suppl_rows <- suppl_cols <- character(0)
        
        if (nrow(all_sites) && any(all_sites$Supplementary)) {
          suppl_row_indices <- which(rownames(mat) %in% all_sites$Entity[all_sites$Supplementary])
          suppl_rows <- rownames(mat)[suppl_row_indices]
        }
        if (nrow(all_types) && any(all_types$Supplementary)) {
          suppl_col_indices <- which(colnames(mat) %in% all_types$Entity[all_types$Supplementary])
          suppl_cols <- colnames(mat)[suppl_col_indices]
        }
        
        active_mat <- mat
        if (length(suppl_row_indices)) active_mat <- active_mat[-suppl_row_indices, , drop = FALSE]
        if (length(suppl_col_indices)) active_mat <- active_mat[, -suppl_col_indices, drop = FALSE]
        validate(need(nrow(active_mat) >= 3 && ncol(active_mat) >= 3, tr("bootstrap.validate.active.small")))
        
        # Poisson resampling (fixed setting)
        resampling_method <- "Poisson"
        
        calc_dims <- max(2L, min(5L, nrow(active_mat) - 1L, ncol(active_mat) - 1L))
        
        tmp_pdf <- tempfile(fileext = ".pdf")
        pdf(file = tmp_pdf)
        on.exit({
          try(dev.off(), silent = TRUE)
          try(unlink(tmp_pdf), silent = TRUE)
        }, add = TRUE)
        
        bootstrap_result <- cabootcrs::cabootcrs(
          active_mat, nboots = n_iterations, resampledistn = resampling_method,
          catype = "sca", crpercent = confidence_percent, lastaxis = calc_dims, printdims = calc_dims
        )
        
        bootstrap_stats <- list(
          cabootcrs_result = bootstrap_result, 
          active_mat = active_mat,
          full_mat = mat,
          suppl_row_indices = if (length(suppl_row_indices)) suppl_row_indices else NULL,
          suppl_col_indices = if (length(suppl_col_indices)) suppl_col_indices else NULL,
          suppl_rows = suppl_rows,
          suppl_cols = suppl_cols,
          n_iterations = n_iterations, 
          confidence_level = confidence_percent / 100,
          confidence_percent = confidence_percent, 
          resampling_method = resampling_method,
          data_validation = data_check,
          calc_dims = calc_dims
        )
        
        if (requireNamespace("digest", quietly = TRUE)) {
          cache$bootstrap_key <- list(
            n_iterations = n_iterations,
            confidence_percent = confidence_percent,
            method = resampling_method,
            dims = calc_dims,
            data_hash = digest::digest(active_mat)
          )
        }
        cache$bootstrap_result <- bootstrap_stats
        
        showNotification(sprintf(tr("bootstrap.success"),
                                n_iterations, resampling_method, data_check$sparsity),
                        type = "message", duration = 4)
        bootstrap_stats

      }, error = function(e) {
        showNotification(paste(tr("bootstrap.error"), e$message), type = "error", duration = 8)
        NULL
      })
    })
  })
  
  output$bootstrap_dim_select <- renderUI({
    req(bootstrap_res())
    bs <- bootstrap_res()
    
    nd <- tryCatch({
      max(2L, ncol(bs$cabootcrs_result@Rowprinccoord))
    }, error = function(e) 2L)
    
    dims <- setNames(paste0("Dim", 1:nd),
                    sprintf(tr("bootstrap.dim.label"), 1:nd))

    ns <- session$ns

    tagList(
      selectInput(ns("bootstrap_x_dim"), tr("bootstrap.xaxis"), choices = dims, selected = dims[1]),
      selectInput(ns("bootstrap_y_dim"), tr("bootstrap.yaxis"), choices = dims, selected = dims[min(2, length(dims))])
    )
  })
  
  bootstrap_plot_data <- reactive({
    req(bootstrap_res(), input$bootstrap_x_dim, input$bootstrap_y_dim)
    bs <- bootstrap_res(); res <- bs$cabootcrs_result
    
    dim_indices <- extract_dimension_indices(input$bootstrap_x_dim, input$bootstrap_y_dim)
    nd <- ncol(res@Rowprinccoord)
    x_idx <- min(dim_indices$x, nd)
    y_idx <- min(dim_indices$y, nd)
    if (x_idx == y_idx) y_idx <- max(1L, min(nd, x_idx + 1L))
    
    out <- list()
    
    # Get contribution if CA result available
    contrib_sites <- contrib_types <- NULL
    if (!is.null(ca_result) && !is.null(ca_result())) {
      ca_res <- ca_result()
      if (!is.null(ca_res$row) && !is.null(ca_res$row$contrib)) {
        contrib_sites <- ca_res$row$contrib
      }
      if (!is.null(ca_res$col) && !is.null(ca_res$col$contrib)) {
        contrib_types <- ca_res$col$contrib
      }
    }
    
    # Sites with extended metrics
    if (input$bootstrap_elements %in% c("both", "sites")) {
      row_coords <- res@Rowprinccoord[, c(x_idx, y_idx), drop = FALSE]
      row_names <- rownames(row_coords)
      n_rows <- nrow(row_coords)
      
      row_sds_x <- numeric(n_rows)
      row_sds_y <- numeric(n_rows)
      row_sds_total <- numeric(n_rows)
      row_ellipse_area <- numeric(n_rows)
      site_ellipses <- vector("list", n_rows); names(site_ellipses) <- row_names
      
      for (i in seq_len(n_rows)) {
        cov_result <- try(cabootcrs::covmat(res, i, "row", x_idx, y_idx, show = FALSE), silent = TRUE)
        if (!inherits(cov_result, "try-error") && is.matrix(cov_result)) {
          row_sds_x[i] <- sqrt(max(cov_result[1,1], 0))
          row_sds_y[i] <- sqrt(max(cov_result[2,2], 0))
          row_sds_total[i] <- sqrt(max(cov_result[1,1], 0) + max(cov_result[2,2], 0))
          
          det_cov <- det(cov_result)
          if (det_cov > 0) {
            chi2_val <- qchisq(bs$confidence_level, df = 2)
            row_ellipse_area[i] <- pi * sqrt(det_cov) * chi2_val
          } else {
            row_ellipse_area[i] <- NA_real_
          }
          
          el <- calculate_ellipse_from_cov(cov_result, row_coords[i,1], row_coords[i,2], bs$confidence_level)
          if (!is.null(el)) site_ellipses[[i]] <- el
        } else {
          row_sds_x[i] <- row_sds_y[i] <- row_sds_total[i] <- row_ellipse_area[i] <- NA_real_
        }
      }
      
      # Add contribution
      contrib_x <- contrib_y <- contrib_total <- rep(NA_real_, n_rows)
      if (!is.null(contrib_sites)) {
        matched_idx <- match(row_names, rownames(contrib_sites))
        contrib_x <- ifelse(!is.na(matched_idx), contrib_sites[matched_idx, x_idx], NA_real_)
        contrib_y <- ifelse(!is.na(matched_idx), contrib_sites[matched_idx, y_idx], NA_real_)
        # Total contribution as sum over all available dims
        if (ncol(contrib_sites) >= 2) {
          contrib_total <- ifelse(!is.na(matched_idx), 
                                 rowSums(contrib_sites[matched_idx, 1:min(5, ncol(contrib_sites)), drop=FALSE], na.rm=TRUE),
                                 NA_real_)
        }
      }
      
      out$sites <- data.frame(
        x = row_coords[,1], y = row_coords[,2], label = row_names, 
        type = "Site", 
        sd_x = row_sds_x,
        sd_y = row_sds_y,
        sd_total = row_sds_total,
        ellipse_area = row_ellipse_area,
        contrib_x = contrib_x,
        contrib_y = contrib_y,
        contrib_total = contrib_total,
        stringsAsFactors = FALSE
      )
      out$site_ellipses <- site_ellipses
    }
    
    # Types analog
    if (input$bootstrap_elements %in% c("both", "types")) {
      col_coords <- res@Colprinccoord[, c(x_idx, y_idx), drop = FALSE]
      col_names <- rownames(col_coords)
      n_cols <- nrow(col_coords)
      
      col_sds_x <- numeric(n_cols)
      col_sds_y <- numeric(n_cols)
      col_sds_total <- numeric(n_cols)
      col_ellipse_area <- numeric(n_cols)
      type_ellipses <- vector("list", n_cols); names(type_ellipses) <- col_names
      
      for (i in seq_len(n_cols)) {
        cov_result <- try(cabootcrs::covmat(res, i, "column", x_idx, y_idx, show = FALSE), silent = TRUE)
        if (!inherits(cov_result, "try-error") && is.matrix(cov_result)) {
          col_sds_x[i] <- sqrt(max(cov_result[1,1], 0))
          col_sds_y[i] <- sqrt(max(cov_result[2,2], 0))
          col_sds_total[i] <- sqrt(max(cov_result[1,1], 0) + max(cov_result[2,2], 0))
          
          det_cov <- det(cov_result)
          if (det_cov > 0) {
            chi2_val <- qchisq(bs$confidence_level, df = 2)
            col_ellipse_area[i] <- pi * sqrt(det_cov) * chi2_val
          } else {
            col_ellipse_area[i] <- NA_real_
          }
          
          el <- calculate_ellipse_from_cov(cov_result, col_coords[i,1], col_coords[i,2], bs$confidence_level)
          if (!is.null(el)) type_ellipses[[i]] <- el
        } else {
          col_sds_x[i] <- col_sds_y[i] <- col_sds_total[i] <- col_ellipse_area[i] <- NA_real_
        }
      }
      
      contrib_x <- contrib_y <- contrib_total <- rep(NA_real_, n_cols)
      if (!is.null(contrib_types)) {
        matched_idx <- match(col_names, rownames(contrib_types))
        contrib_x <- ifelse(!is.na(matched_idx), contrib_types[matched_idx, x_idx], NA_real_)
        contrib_y <- ifelse(!is.na(matched_idx), contrib_types[matched_idx, y_idx], NA_real_)
        if (ncol(contrib_types) >= 2) {
          contrib_total <- ifelse(!is.na(matched_idx), 
                                 rowSums(contrib_types[matched_idx, 1:min(5, ncol(contrib_types)), drop=FALSE], na.rm=TRUE),
                                 NA_real_)
        }
      }
      
      out$types <- data.frame(
        x = col_coords[,1], y = col_coords[,2], label = col_names,
        type = "Type", 
        sd_x = col_sds_x,
        sd_y = col_sds_y,
        sd_total = col_sds_total,
        ellipse_area = col_ellipse_area,
        contrib_x = contrib_x,
        contrib_y = contrib_y,
        contrib_total = contrib_total,
        stringsAsFactors = FALSE
      )
      out$type_ellipses <- type_ellipses
    }
    
    if (length(bs$suppl_rows) > 0 || length(bs$suppl_cols) > 0) {
      out$has_supplementary <- TRUE
    }
    
    out$.__axes__ <- c(x_idx, y_idx)
    out
  })
  
  output$bootstrap_plotly <- plotly::renderPlotly({
    req(bootstrap_plot_data(), bootstrap_res())
    pd <- bootstrap_plot_data(); bs <- bootstrap_res(); res <- bs$cabootcrs_result
    x_idx <- pd$.__axes__[1]; y_idx <- pd$.__axes__[2]
    
    mu <- tryCatch(res@br@mu, error = function(e) numeric(0))
    ev2 <- mu^2
    tot <- sum(ev2)
    var_x <- if (x_idx <= length(ev2) && tot > 0) ev2[x_idx] / tot * 100 else NA_real_
    var_y <- if (y_idx <= length(ev2) && tot > 0) ev2[y_idx] / tot * 100 else NA_real_
    
    axis_title_x <- sprintf("%s (%.1f%% )", paste0("Dim", x_idx), ifelse(is.na(var_x), 0, var_x))
    axis_title_y <- sprintf("%s (%.1f%%)", paste0("Dim", y_idx), ifelse(is.na(var_y), 0, var_y))
    
    plot_title <- sprintf(tr("plot.bootstrap.title"),
                         bs$n_iterations, bs$confidence_percent, bs$resampling_method)
    
    p <- plotly::plot_ly() %>%
      plotly::layout(ca_plotly_layout(axis_title_x, axis_title_y, plot_title))
    
    # Sites zeichnen
    if (!is.null(pd$sites)) {
      site_data <- pd$sites
      
      # Stability metric
      stability_metric <- ifelse(!is.na(site_data$ellipse_area), site_data$ellipse_area, site_data$sd_total)
      
      if (isTRUE(input$highlight_stability) && nrow(site_data) > 0) {
        qs <- stats::quantile(stability_metric, c(0.1, 0.9), na.rm = TRUE)

        # Stability categories based on quantiles
        stab_very <- tr("bootstrap.stability.very")
        stab_less <- tr("bootstrap.stability.less")
        stab_normal <- tr("bootstrap.stability.normal")

        site_data$stability <- ifelse(stability_metric <= qs[1], stab_very,
                                      ifelse(stability_metric >= qs[2], stab_less, stab_normal))
        site_data$marker_size  <- ifelse(site_data$stability == stab_very, 12,
                                         ifelse(site_data$stability == stab_less, 16, 8))
        colors <- seri_arc_colors()
        site_data$marker_color <- ifelse(site_data$stability == stab_very, colors$excellent,
                                         ifelse(site_data$stability == stab_less, colors$error, colors$site_active))
      } else {
        colors <- seri_arc_colors()
        site_data$stability <- tr("bootstrap.stability.normal")
        site_data$marker_size <- 8
        site_data$marker_color <- colors$site_active
      }
      
      site_data$hover_text <- generate_bootstrap_hover_text(
        site_data, paste0("Dim", x_idx), paste0("Dim", y_idx),
        x_idx, y_idx, bs$resampling_method, get_element_details
      )
      
      for (stab in unique(site_data$stability)) {
        sub <- site_data[site_data$stability == stab, , drop = FALSE]
        site_marker_config <- ca_site_marker(sub$marker_color, sub$marker_size, 0.8, "Active")
        p <- p %>% plotly::add_markers(
          x = sub$x, y = sub$y,
          marker = site_marker_config,
          text = if (isTRUE(input$show_bootstrap_labels)) substr(sub$label, 1, input$bootstrap_label_chars %||% 12) else "",
          textposition = "middle right",
          textfont = list(size = 10, color = "#2c3e50"),
          hovertext = sub$hover_text, hoverinfo = "text",
          name = paste("Sites", stab), showlegend = TRUE
        )
      }
    }
    
    # Types analog
    if (!is.null(pd$types)) {
      type_data <- pd$types
      
      stability_metric <- ifelse(!is.na(type_data$ellipse_area), type_data$ellipse_area, type_data$sd_total)
      
      if (isTRUE(input$highlight_stability) && nrow(type_data) > 0) {
        qs <- stats::quantile(stability_metric, c(0.1, 0.9), na.rm = TRUE)

        # Stability categories based on quantiles
        stab_very <- tr("bootstrap.stability.very")
        stab_less <- tr("bootstrap.stability.less")
        stab_normal <- tr("bootstrap.stability.normal")

        type_data$stability <- ifelse(stability_metric <= qs[1], stab_very,
                                      ifelse(stability_metric >= qs[2], stab_less, stab_normal))
        type_data$marker_size  <- ifelse(type_data$stability == stab_very, 12,
                                         ifelse(type_data$stability == stab_less, 16, 8))
        colors <- seri_arc_colors()
        type_data$marker_color <- ifelse(type_data$stability == stab_very, colors$excellent,
                                         ifelse(type_data$stability == stab_less, colors$error, colors$type_active))
      } else {
        colors <- seri_arc_colors()
        type_data$stability <- tr("bootstrap.stability.normal")
        type_data$marker_size <- 8
        type_data$marker_color <- colors$type_active
      }
      
      type_data$hover_text <- generate_bootstrap_hover_text(
        type_data, paste0("Dim", x_idx), paste0("Dim", y_idx),
        x_idx, y_idx, bs$resampling_method, get_element_details
      )
      
      for (stab in unique(type_data$stability)) {
        sub <- type_data[type_data$stability == stab, , drop = FALSE]
        type_marker_config <- ca_type_marker(sub$marker_color, sub$marker_size, 0.8, "Active")
        p <- p %>% plotly::add_markers(
          x = sub$x, y = sub$y,
          marker = type_marker_config,
          text = if (isTRUE(input$show_bootstrap_labels)) substr(sub$label, 1, input$bootstrap_label_chars %||% 12) else "",
          textposition = "middle right",
          textfont = list(size = 10, color = "#2c3e50"),
          hovertext = sub$hover_text, hoverinfo = "text",
          name = paste("Types", stab), showlegend = TRUE
        )
      }
    }
    
    # Konfidenzellipsen
    if (!is.null(input$show_confidence_ellipses_bootstrap) && input$show_confidence_ellipses_bootstrap) {
      if (!is.null(pd$site_ellipses)) {
        for (i in seq_along(pd$site_ellipses)) {
          ellipse_data <- pd$site_ellipses[[i]]
          if (!is.null(ellipse_data) && nrow(ellipse_data) > 3) {
            site_name <- names(pd$site_ellipses)[i]
            ellipse_data <- rbind(ellipse_data, ellipse_data[1, ])
            colors <- seri_arc_colors()
            p <- p %>% plotly::add_trace(
              x = ellipse_data$x, y = ellipse_data$y,
              type = "scatter", mode = "lines",
              line = list(color = colors$site_active, width = 2, dash = "dot"),
              fill = "none", opacity = 0.4,
              hoverinfo = "none", showlegend = FALSE,
              name = paste("Region:", site_name)
            )
          }
        }
      }
      
      if (!is.null(pd$type_ellipses)) {
        for (i in seq_along(pd$type_ellipses)) {
          ellipse_data <- pd$type_ellipses[[i]]
          if (!is.null(ellipse_data) && nrow(ellipse_data) > 3) {
            type_name <- names(pd$type_ellipses)[i]
            ellipse_data <- rbind(ellipse_data, ellipse_data[1, ])
            colors <- seri_arc_colors()
            p <- p %>% plotly::add_trace(
              x = ellipse_data$x, y = ellipse_data$y,
              type = "scatter", mode = "lines",
              line = list(color = colors$type_active, width = 2, dash = "dot"),
              fill = "none", opacity = 0.4,
              hoverinfo = "none", showlegend = FALSE,
              name = paste("Region:", type_name)
            )
          }
        }
      }
    }
    
    standard_plotly_config(p, "2d")
  })
  
  # Extended statistics table with multi-dim
  output$bootstrap_stats_table <- DT::renderDataTable({
    req(bootstrap_res())
    bs <- bootstrap_res()
    res <- bs$cabootcrs_result
    pd <- bootstrap_plot_data()
    
    dim_indices <- extract_dimension_indices(input$bootstrap_x_dim %||% "Dim1", input$bootstrap_y_dim %||% "Dim2")
    nd <- ncol(res@Rowprinccoord)
    x_idx <- min(dim_indices$x, nd)
    y_idx <- min(dim_indices$y, nd)
    if (x_idx == y_idx) y_idx <- max(1L, min(nd, x_idx + 1L))
    
    stats_rows <- list()
    row_idx <- 1
    
    if (input$bootstrap_elements %in% c("both", "sites")) {
      row_names <- rownames(res@Rowprinccoord)
      for (i in seq_along(row_names)) {
        sd_x <- sd_y <- sd_total <- ellipse_area <- NA_real_
        cov_result <- try(cabootcrs::covmat(res, i, "row", x_idx, y_idx, show = FALSE), silent = TRUE)
        if (!inherits(cov_result, "try-error") && is.matrix(cov_result)) {
          sd_x <- sqrt(max(cov_result[1,1], 0))
          sd_y <- sqrt(max(cov_result[2,2], 0))
          sd_total <- sqrt(max(cov_result[1,1], 0) + max(cov_result[2,2], 0))
          det_cov <- det(cov_result)
          if (det_cov > 0) {
            chi2_val <- qchisq(bs$confidence_level, df = 2)
            ellipse_area <- pi * sqrt(det_cov) * chi2_val
          }
        }
        
        row_data <- data.frame(
          Element = row_names[i],
          Typ = tr("bootstrap.table.type.site"),
          SD_X = round(sd_x, 3),
          SD_Y = round(sd_y, 3),
          SD_Tot = round(sd_total, 3),
          Area = round(ellipse_area, 3),
          stringsAsFactors = FALSE
        )
        
        stats_rows[[row_idx]] <- row_data
        row_idx <- row_idx + 1
      }
    }
    
    if (input$bootstrap_elements %in% c("both", "types")) {
      col_names <- rownames(res@Colprinccoord)
      for (i in seq_along(col_names)) {
        sd_x <- sd_y <- sd_total <- ellipse_area <- NA_real_
        cov_result <- try(cabootcrs::covmat(res, i, "column", x_idx, y_idx, show = FALSE), silent = TRUE)
        if (!inherits(cov_result, "try-error") && is.matrix(cov_result)) {
          sd_x <- sqrt(max(cov_result[1,1], 0))
          sd_y <- sqrt(max(cov_result[2,2], 0))
          sd_total <- sqrt(max(cov_result[1,1], 0) + max(cov_result[2,2], 0))
          det_cov <- det(cov_result)
          if (det_cov > 0) {
            chi2_val <- qchisq(bs$confidence_level, df = 2)
            ellipse_area <- pi * sqrt(det_cov) * chi2_val
          }
        }
        
        col_data <- data.frame(
          Element = col_names[i],
          Typ = tr("bootstrap.table.type.type"),
          SD_X = round(sd_x, 3),
          SD_Y = round(sd_y, 3),
          SD_Tot = round(sd_total, 3),
          Area = round(ellipse_area, 3),
          stringsAsFactors = FALSE
        )
        
        stats_rows[[row_idx]] <- col_data
        row_idx <- row_idx + 1
      }
    }
    
    stats_data <- do.call(rbind, stats_rows)
    
    # Perzentil-basierte Klassifikation
    stability_col <- "Area"
    quant <- stats::quantile(stats_data[[stability_col]], c(0.1, 0.9), na.rm = TRUE)

    stats_data$Stable <- ifelse(is.na(stats_data[[stability_col]]), "⚪",
                                    ifelse(stats_data[[stability_col]] <= quant[1], "🟢",
                                           ifelse(stats_data[[stability_col]] >= quant[2], "🔴", "🟡")))

    stats_data <- stats_data[order(stats_data[[stability_col]]), , drop = FALSE]

    caption_text <- sprintf(tr("bootstrap.table.caption"),
                           bs$n_iterations, bs$confidence_percent, bs$resampling_method)
    
    DT::datatable(
      stats_data,
      options = list(
        pageLength = 15, 
        scrollY = "400px",  # Vertically scrollable!
        scrollX = FALSE,    # NO horizontal scroll!
        paging = FALSE,     # No pagination!
        searching = FALSE,  # No search box
        info = FALSE,       # No "Showing 1 to X" info
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '35px', targets = 1),  # Type narrower
          list(width = '35px', targets = ncol(stats_data) - 1)  # Stable narrower
        )
      ),
      rownames = FALSE,
      class = 'compact cell-border stripe hover',
      caption = caption_text
    ) %>%
      DT::formatStyle(
        'Stable',
        backgroundColor = DT::styleEqual(
          c("🟢", "🟡", "🔴", "⚪"),
          c("#d4edda", "#fff3cd", "#f8d7da", "#f0f0f0")
        )
      )
  })
  
  # ROBUSTE TOP-10 FUNKTION (ersetzt output$bootstrap_critical_elements):
  
  output$bootstrap_critical_elements <- DT::renderDataTable({
    req(bootstrap_plot_data())
    pd <- bootstrap_plot_data()
    
    # Combine sites and types - ROBUST
    all_elements <- tryCatch({
      rbind(
        if (!is.null(pd$sites) && nrow(pd$sites) > 0) pd$sites else NULL,
        if (!is.null(pd$types) && nrow(pd$types) > 0) pd$types else NULL
      )
    }, error = function(e) NULL)
    
    # Early exit if no data
    if (is.null(all_elements) || nrow(all_elements) == 0) {
      return(DT::datatable(
        data.frame(Info = tr("bootstrap.critical.no.data")),
        options = list(dom = 't'),
        rownames = FALSE
      ))
    }
    
    # Extract instability - ROBUST
    instability <- if ("ellipse_area" %in% colnames(all_elements)) {
      ifelse(!is.na(all_elements$ellipse_area), all_elements$ellipse_area, 
             ifelse("sd_total" %in% colnames(all_elements), all_elements$sd_total, NA_real_))
    } else if ("sd_total" %in% colnames(all_elements)) {
      all_elements$sd_total
    } else {
      rep(NA_real_, nrow(all_elements))
    }
    
    # Contribution extrahieren - ROBUST
    contribution <- if ("contrib_total" %in% colnames(all_elements)) {
      all_elements$contrib_total
    } else if (all(c("contrib_x", "contrib_y") %in% colnames(all_elements))) {
      all_elements$contrib_x + all_elements$contrib_y
    } else {
      rep(NA_real_, nrow(all_elements))
    }
    
    # Validierung
    valid_instab <- !is.na(instability) & is.finite(instability)
    valid_contrib <- !is.na(contribution) & is.finite(contribution)
    
    # CASE 1: No contribution available → only instability
    if (sum(valid_contrib) == 0) {
      if (sum(valid_instab) == 0) {
        return(DT::datatable(
          data.frame(Info = tr("bootstrap.critical.no.stability")),
          options = list(dom = 't'), rownames = FALSE
        ))
      }

      # Sort only by instability
      result_df <- data.frame(
        Element = all_elements$label[valid_instab],
        Typ = all_elements$type[valid_instab],
        Instab = round(instability[valid_instab], 4),
        stringsAsFactors = FALSE
      )

      result_df <- result_df[order(result_df$Instab, decreasing = TRUE), ]
      top10 <- head(result_df, 10)

      if (nrow(top10) > 0) {
        top10 <- cbind(Rank = seq_len(nrow(top10)), top10)
      }

      return(DT::datatable(
        top10,
        options = list(pageLength = 10, scrollX = FALSE, dom = 't', ordering = FALSE),
        rownames = FALSE,
        class = 'cell-border stripe hover',
        caption = tr("bootstrap.critical.caption.instab")
      ) %>%
        DT::formatStyle('Rank',
                        backgroundColor = DT::styleInterval(c(3.5, 7.5), c("#f8d7da", "#fff3cd", "#d4edda"))
        ))
    }
    
    # Only use valid rows
    valid_rows <- valid_instab & valid_contrib
    
    if (sum(valid_rows) == 0) {
      return(DT::datatable(
        data.frame(Info = tr("bootstrap.critical.no.contrib")),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    # Normalisierung
    max_contrib <- max(contribution[valid_contrib], na.rm = TRUE)
    max_instab <- max(instability[valid_instab], na.rm = TRUE)
    
    if (!is.finite(max_contrib) || max_contrib == 0) max_contrib <- 1
    if (!is.finite(max_instab) || max_instab == 0) max_instab <- 1
    
    contrib_norm <- contribution / max_contrib
    instab_norm <- instability / max_instab
    criticality <- contrib_norm * instab_norm
    
    # Dataframe with valid rows only - COMPACT
    result_df <- data.frame(
      Element = all_elements$label[valid_rows],
      Typ = all_elements$type[valid_rows],
      Instab = round(instability[valid_rows], 3),
      Contr = round(contribution[valid_rows], 1),
      Crit = round(criticality[valid_rows], 3),
      stringsAsFactors = FALSE
    )

    # Sort and top 10
    result_df <- result_df[order(result_df$Crit, decreasing = TRUE), ]
    top10 <- head(result_df, 10)

    if (nrow(top10) > 0) {
      top10 <- cbind(Rank = seq_len(nrow(top10)), top10)
    }

    DT::datatable(
      top10,
      options = list(
        pageLength = 10,
        scrollX = FALSE,
        dom = 't',
        ordering = FALSE,
        paging = FALSE,
        columnDefs = list(
          list(className = 'dt-center', targets = "_all"),
          list(width = '30px', targets = 0),   # Rank
          list(width = '180px', targets = 1),  # Element
          list(width = '40px', targets = 2),   # Typ
          list(width = '55px', targets = 3),   # Instab
          list(width = '50px', targets = 4),   # Contr
          list(width = '50px', targets = 5)    # Crit
        )
      ),
      rownames = FALSE,
      class = 'compact cell-border stripe hover',
      caption = tr("bootstrap.critical.caption")
    ) %>%
      DT::formatStyle('Rank',
                      backgroundColor = DT::styleInterval(c(3.5, 7.5), c("#f8d7da", "#fff3cd", "#d4edda"))
      ) %>%
      DT::formatStyle('Element',
                      `white-space` = 'normal',
                      `word-wrap` = 'break-word',
                      `text-align` = 'left'  # Left align for better readability
      )
  })
  
  
  # Quality diagnostics
  output$bootstrap_quality_stats <- renderText({
    req(bootstrap_res())
    bs <- bootstrap_res()
    res <- bs$cabootcrs_result

    avail <- tryCatch(methods::slotNames(res), error = function(e) character(0))
    has_br <- "br" %in% avail
    br_slots <- if (has_br) tryCatch(methods::slotNames(res@br), error = function(e) character(0)) else character(0)
    has_mu <- has_br && ("mu" %in% br_slots)

    eigenvalues_info <- if (has_mu) {
      mu <- tryCatch(res@br@mu, error = function(e) numeric(0))

      if (length(mu) >= 2) {
        ratio_12 <- mu[1] / mu[2]
        eig_warning <- if (ratio_12 < 1.2) {
          paste0("\n", tr("bootstrap.quality.warning.axis"))
        } else {
          ""
        }
      } else {
        eig_warning <- ""
      }

      paste(tr("bootstrap.quality.singular"), length(mu),
            "|", tr("bootstrap.quality.first3"), paste(round(mu[seq_len(min(3, length(mu)))], 4), collapse = ", "),
            eig_warning)
    } else {
      tr("bootstrap.quality.mu.unavail")
    }

    data_info <- if (!is.null(bs$data_validation)) {
      valid <- bs$data_validation
      warn_text <- if (length(valid$warnings) > 0) {
        paste0("\n\n", tr("bootstrap.quality.data.warnings"), "\n", paste(valid$warnings, collapse = "\n"))
      } else {
        paste0("\n\n", tr("bootstrap.quality.data.ok"))
      }
      warn_text
    } else ""

    paste(
      tr("bootstrap.quality.title"),
      "",
      tr("bootstrap.quality.eigenvalue"),
      eigenvalues_info,
      "",
      sprintf("%s\n\u2022 %s %d\n\u2022 %s %g%%\n\u2022 %s %s %s",
              tr("bootstrap.quality.params"),
              tr("bootstrap.quality.iterations"), bs$n_iterations,
              tr("bootstrap.quality.region"), bs$confidence_percent,
              tr("bootstrap.quality.model"), bs$resampling_method, tr("bootstrap.quality.poisson.fixed")),
      "",
      tr("bootstrap.quality.poisson.title"),
      paste0("\u2022 ", tr("bootstrap.quality.poisson.desc1")),
      paste0("\u2022 ", tr("bootstrap.quality.poisson.desc2")),
      data_info,
      sep = "\n"
    )
  })
  
  # Download handler (extended with multi-dim + contribution)
  output$download_plot_png <- create_png_download_handler(
    "bootstrap_plotly", "SeriARC_Bootstrap_Plot", session,
    tr = tr
  )
  
  output$download_plot_svg <- create_svg_download_handler(
    "bootstrap_plotly", "SeriARC_Bootstrap_Plot", session,
    plot_data = bootstrap_plot_data,
    plot_generator_func = function(plot_data) {
      if (!is.null(plot_data$sites) || !is.null(plot_data$types)) {
        all_data <- rbind(
          if (!is.null(plot_data$sites)) plot_data$sites else data.frame(),
          if (!is.null(plot_data$types)) plot_data$types else data.frame()
        )
        if (nrow(all_data) > 0) {
          colors <- seri_arc_colors()
          par(mar = c(5, 4, 4, 2))
          point_colors <- ifelse(all_data$type == "Site", colors$site_active, colors$type_active)
          point_shapes <- ifelse(all_data$type == "Site", 16, 17)

          stability_metric <- all_data$ellipse_area
          point_sizes <- pmax(0.5, 2 - stability_metric * 5)

          plot(all_data$x, all_data$y,
               col = point_colors, pch = point_shapes, cex = point_sizes,
               xlab = sprintf("%s (Bootstrap)", input$bootstrap_x_dim %||% "Dim1"),
               ylab = sprintf("%s (Bootstrap)", input$bootstrap_y_dim %||% "Dim2"),
               main = tr("plot.bootstrap.svg.title"))
          abline(h = 0, v = 0, col = "gray", lty = 2)
          grid(col = "lightgray", lty = "dotted")
          legend("topright",
                 legend = c("Sites", "Types", tr("plot.bootstrap.legend.instab")),
                 col = c(colors$site_active, colors$type_active, "gray"),
                 pch = c(16, 17, NA), lty = c(0, 0, 1), cex = 0.8)
          bs <- bootstrap_res()
          mtext(sprintf("SVG: %d Iter., %g%% Region, %s, n = %d",
                       bs$n_iterations, bs$confidence_percent, bs$resampling_method, nrow(all_data)),
                side = 1, line = 4, cex = 0.8, col = "gray")
        }
      }
    },
    tr = tr
  )
  
  output$download_plot_pdf <- create_pdf_download_handler(
    bootstrap_plot_data, "SeriARC_Bootstrap_Plot",
    plot_generator_func = function(plot_data) {
      if (!is.null(plot_data$sites) || !is.null(plot_data$types)) {
        all_data <- rbind(
          if (!is.null(plot_data$sites)) plot_data$sites else data.frame(),
          if (!is.null(plot_data$types)) plot_data$types else data.frame()
        )
        if (nrow(all_data) > 0) {
          colors <- seri_arc_colors()
          point_colors <- ifelse(all_data$type == "Site", colors$site_active, colors$type_active)
          point_shapes <- ifelse(all_data$type == "Site", 16, 17)
          plot(all_data$x, all_data$y,
               col = point_colors, pch = point_shapes, cex = 1.2,
               xlab = "Bootstrap Dimension X", ylab = "Bootstrap Dimension Y",
               main = tr("plot.bootstrap.pdf.title"))
          grid(col = "lightgray", lty = "dotted")
          legend("topright", legend = c("Sites", "Types"),
                 col = c(colors$site_active, colors$type_active), pch = c(16, 17))
        }
      }
    },
    tr = tr
  )
  
  output$download_data_excel <- create_excel_download_handler(
    function() {
      req(bootstrap_res())
      bs <- bootstrap_res()
      sheets <- list()

      sheets[[tr("bootstrap.excel.param.sheet")]] <- data.frame(
        Parameter = c(tr("bootstrap.excel.param.iterations"), tr("bootstrap.excel.param.region"),
                     tr("bootstrap.excel.param.model"), tr("bootstrap.excel.param.date")),
        Value = c(bs$n_iterations, bs$confidence_percent, bs$resampling_method,
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      )

      if (!is.null(bootstrap_plot_data())) {
        pd <- bootstrap_plot_data()
        if (!is.null(pd$sites)) {
          sheets[["Bootstrap_Sites"]] <- pd$sites[, c("label", "x", "y", "sd_total", "ellipse_area", "contrib_x", "contrib_y", "contrib_total")]
        }
        if (!is.null(pd$types)) {
          sheets[["Bootstrap_Types"]] <- pd$types[, c("label", "x", "y", "sd_total", "ellipse_area", "contrib_x", "contrib_y", "contrib_total")]
        }
      }

      return(sheets)
    },
    "Bootstrap_Analysis", tr("plot.bootstrap.pdf.title"),
    tr = tr
  )
  
  return(list(
    bootstrap_result    = bootstrap_res,
    bootstrap_plot_data = bootstrap_plot_data
  ))
}

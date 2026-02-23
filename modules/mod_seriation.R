# SERIATION CORE FUNCTION

compute_seriation <- function(mat, method = "CA", transform = "none", binary_threshold = 0) {
  # Ensure numeric matrix and clean missing values
  mat_clean <- as.matrix(mat)
  mat_clean[is.na(mat_clean)] <- 0
  mat_clean[!is.finite(mat_clean)] <- 0
  
  # Apply transformation based on setting
  if (transform == "binary") {
    threshold <- if (is.null(binary_threshold) || is.na(binary_threshold)) 0 else binary_threshold
    mat_transformed <- ifelse(mat_clean > threshold, 1, 0)
  } else if (transform == "relative") {
    row_sums <- rowSums(mat_clean, na.rm = TRUE)
    row_sums[row_sums == 0] <- 1
    mat_transformed <- sweep(mat_clean, 1, row_sums, FUN = "/")
  } else if (transform == "log") {
    mat_transformed <- log(mat_clean + 1)
  } else {
    mat_transformed <- mat_clean
  }
  
  # SERIATION: CA or RA
  # CA = Gold standard for archaeological frequency seriation (Ford 1951)
  # RA = Reciprocal Averaging, historical predecessor of CA
  # Matches tosca, PAST and modern archaeological practice

  # RA: Custom implementation (Hill 1973)
  if (method == "RA") {
    # Reciprocal Averaging algorithm
    # Initialization
    row_scores <- seq_len(nrow(mat_transformed))
    col_scores <- rep(0, ncol(mat_transformed))
    
    # Iterative procedure (max 100 iterations)
    for (iter in 1:100) {
      old_row_scores <- row_scores
      
      # Column scores as weighted average of rows
      col_sums <- colSums(mat_transformed)
      col_sums[col_sums == 0] <- 1  # Avoid division by zero
      col_scores <- as.vector((t(mat_transformed) %*% row_scores) / col_sums)

      # Row scores as weighted average of columns
      row_sums <- rowSums(mat_transformed)
      row_sums[row_sums == 0] <- 1
      row_scores <- as.vector((mat_transformed %*% col_scores) / row_sums)
      
      # Check convergence
      if (max(abs(row_scores - old_row_scores)) < 1e-6) break
    }
    
    # Order based on scores
    row_order_idx <- order(row_scores, decreasing = FALSE, na.last = NA)
    col_order_idx <- order(col_scores, decreasing = FALSE, na.last = NA)
    permuted_mat <- mat_transformed[row_order_idx, col_order_idx, drop = FALSE]
    
    display_name <- "Reciprocal Averaging"
    
    return(list(
      permuted_mat = permuted_mat,
      permuted_matrix = permuted_mat,
      row_order = rownames(permuted_mat),
      col_order = colnames(permuted_mat),
      method_name = display_name,
      transformation = transform
    ))
  }
  
  # CA: Use seriation package
  seriation_method <- "CA"

  # Perform seriation with seriation package
  tryCatch({
    # Check if seriation is available
    if (!requireNamespace("seriation", quietly = TRUE)) {
      stop("Package 'seriation' is not installed. Please install with: install.packages('seriation')")
    }

    # Perform seriation
    ser <- seriation::seriate(mat_transformed, method = seriation_method)
    
    # Extract order
    row_order_idx <- seriation::get_order(ser, dim = 1)
    col_order_idx <- seriation::get_order(ser, dim = 2)

    # Permute matrix
    permuted_mat <- mat_transformed[row_order_idx, col_order_idx, drop = FALSE]
    
    display_name <- "Correspondence Analysis"
    
    # Return WITHOUT quality tests
    list(
      permuted_mat = permuted_mat,
      permuted_matrix = permuted_mat,
      row_order = rownames(permuted_mat),
      col_order = colnames(permuted_mat),
      method_name = display_name,
      transformation = transform,
      seriation_object = ser  # For extended analyses
    )
    
  }, error = function(e) {
    # Fallback to CA method if seriation fails (RA not available)
    warning("Seriation package error: ", e$message, ". Fallback to CA.")
    
    ca_res <- FactoMineR::CA(mat_transformed, graph = FALSE)
    row_scores <- ca_res$row$coord[, 1]
    col_scores <- ca_res$col$coord[, 1]
    row_order_idx <- order(row_scores, decreasing = FALSE, na.last = NA)
    col_order_idx <- order(col_scores, decreasing = FALSE, na.last = NA)
    permuted_mat <- mat_transformed[row_order_idx, col_order_idx, drop = FALSE]
    
    # Method name - Fallback is always CA
    display_name <- "Correspondence Analysis [Fallback]"
    
    list(
      permuted_mat = permuted_mat,
      permuted_matrix = permuted_mat,
      row_order = rownames(permuted_mat),
      col_order = colnames(permuted_mat),
      method_name = display_name,
      transformation = transform
    )
  })
}

# mod_seriation.R - Archaeological Seriation Module
# Scientifically based on Goldmann (1972) and Ihm (1983)

mod_seriation_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL  # UI is defined in ui_main.R
}

mod_seriation_server <- function(filtered_data, cache, get_site_group, get_element_details = NULL, input, output, session, tr = function(x) x) {
  
  # DEPENDENCIES CHECK (Point 11)
  required_funcs <- c("compute_seriation", "standard_plotly_config",
                      "show_success_notification", "show_error_notification")
  for (func in required_funcs) {
    if (!exists(func, mode = "function")) {
      stop(sprintf("Missing dependency: %s() not found", func))
    }
  }
  
  required_pkgs <- c("plotly", "htmlwidgets", "data.table")
  for (pkg in required_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Missing package: %s must be installed", pkg))
    }
  }
  
  # HELPER FUNCTIONS

  # Safe conversion for R objects
  safe_toString <- function(obj) {
    tryCatch({
      if (is.null(obj)) return("NULL")
      if (is.character(obj) || is.numeric(obj) || is.factor(obj)) return(as.character(obj))
      return(toString(obj))
    }, error = function(e) "Conversion error")
  }
  
  # SERIATION CALCULATION

  seriation_result <- eventReactive(input$refresh2, {
    req(filtered_data())
    
    withProgress(message = tr("seriation.progress.calculating"), {
      # Clean matrix copy (independent of CA transformations)
      original_data <- filtered_data()
      mat <- as.matrix(original_data)
      mat <- matrix(as.numeric(mat), nrow = nrow(mat), ncol = ncol(mat), 
                    dimnames = list(rownames(mat), colnames(mat)))
      mat[!is.finite(mat)] <- 0
      
      validate(need(nrow(mat) >= 2, tr("validate.seriation.min.sites")))
      validate(need(ncol(mat) >= 2, tr("validate.seriation.min.types")))
      
      incProgress(0.5, detail = tr("seriation.progress.calculating"))
      
      # NEW SAFE SERIATION (Demo - can be activated)
      if (DEBUG_MODE) {
        log_seriation("start", "CA", 
                      sprintf("Sites: %d, Types: %d", nrow(mat), ncol(mat)))
      }
      
      tryCatch({
        result <- compute_seriation(
          mat,
          method = input$seriation_method %||% "CA",
          transform = input$seriation_transform %||% "none",
          binary_threshold = input$binary_threshold %||% 0
        )
        
        cache$current_seriation <- result
        cache$seriation_result <- result
        
        if (DEBUG_MODE) {
          log_seriation("success", "CA")
        }
        
        incProgress(1.0, detail = tr("term.completed"))
        show_success_notification(tr("notify.seriation.success"))
        return(result)
        
      }, error = function(e) {
        if (DEBUG_MODE) {
          log_seriation("error", "CA", e$message)
        }
        show_error_notification(paste(tr("error.seriation"), e$message))
        return(NULL)
      })
      
      # ALTERNATIVE: Use safe seriation (uncomment for complete safety)
      # safe_result <- safe_seriation(mat, 
      #                               method = input$seriation_method %||% DEFAULT_SERIATION_METHOD,
      #                               transform = input$seriation_transform %||% DEFAULT_TRANSFORM,
      #                               binary_threshold = input$binary_threshold %||% 0)
      # if (safe_result$success) {
      #   cache$current_seriation <- safe_result$data
      #   cache$seriation_result <- safe_result$data
      #   show_success_notification(safe_result$message)
      #   return(safe_result$data)
      # } else {
      #   show_error_notification(safe_result$error)
      #   return(NULL)
      # }
    })
  })
  
  # Extract matrix and orderings
  data_mat <- reactive({
    seriation_out <- seriation_result()
    if (is.null(seriation_out)) return(NULL)
    
    permuted_mat <- seriation_out$permuted_mat %||% seriation_out$permuted_matrix
    if (is.null(permuted_mat)) return(NULL)
    
    return(permuted_mat)
  })
  
  row_order <- reactive({
    permuted_mat <- data_mat()
    if (is.null(permuted_mat)) return(NULL)
    
    original_sites <- rownames(filtered_data())
    seriated_sites <- rownames(permuted_mat)
    match(seriated_sites, original_sites)
  })
  
  col_order <- reactive({
    permuted_mat <- data_mat()
    if (is.null(permuted_mat)) return(NULL)
    
    original_types <- colnames(filtered_data())
    seriated_types <- colnames(permuted_mat)
    match(seriated_types, original_types)
  })
  
  
  # Store Plotly object for HTML export
  plotly_obj <- reactiveVal(NULL)

  # MAIN PLOT

  output$seriation_plotly <- renderPlotly({
    seriation_out <- seriation_result()
    
    if (is.null(seriation_out)) {
      return(plot_ly() %>% 
               add_annotations(
                 text = tr("validate.seriation.click.calculate"),
                 x = 0.5, y = 0.5, xref = "paper", yref = "paper",
                 showarrow = FALSE, font = list(size = 16, color = "#7f8c8d")
               ))
    }
    
    permuted_mat <- data_mat()
    if (is.null(permuted_mat)) return(NULL)
    
    # Names WITHOUT transposition
    site_order <- rownames(permuted_mat)
    type_order <- colnames(permuted_mat)
    plot_type <- input$seriation_plot_type %||% "ford"

    # Transpose option: Swap only in PLOT, not the matrix!
    transpose_active <- !is.null(input$transpose_matrix) && input$transpose_matrix

    # Checkbox status for labels (ONCE for ALL plots!)
    show_sites <- !is.null(input$show_site_names) && input$show_site_names
    show_types <- !is.null(input$show_type_names) && input$show_type_names

    # Label truncation
    max_chars <- input$ser_label_chars %||% 12
    site_labels <- substr(site_order, 1, max_chars)
    type_labels <- substr(type_order, 1, max_chars)
    
    # Plot creation based on type
    if (plot_type == "ford") {
      # Ford diagram (chronological matrix following Goldmann 1972)
      mat_binary <- ifelse(permuted_mat > 0, 1, 0)
      # Point 7: reshape2::melt() -> data.table::melt()
      mat_melted <- data.table::melt(
        data.table::as.data.table(mat_binary, keep.rownames = TRUE),
        id.vars = "rn",
        variable.name = "Type",
        value.name = "Presence"
      )
      # TOSCA: Sites horizontal (x), Types vertical (y)
      mat_melted$SiteIndex <- match(mat_melted$rn, rownames(mat_binary))
      mat_melted$TypeIndex <- as.numeric(mat_melted$Type)
      mat_melted$rn <- NULL
      mat_melted$Type <- NULL
      mat_melted$Presence_Label <- ifelse(mat_melted$Presence == 1, tr("term.present"), tr("term.absent"))
      
      # Add original values (Point 5 - VECTORIZED)
      idx <- cbind(mat_melted$SiteIndex, mat_melted$TypeIndex)
      mat_melted$original_value <- permuted_mat[idx]

      # Scientifically grounded hover texts
      mat_melted$hover_text <- sprintf(
        "<b>%s:</b> %s<br><b>%s:</b> %s<br><b>%s:</b> %s<br><b>%s:</b> %.3f<br><i>SeriARC %s</i>",
        tr("seriation.hover.site"),
        safe_toString(site_order[mat_melted$SiteIndex]),
        tr("seriation.hover.type"),
        safe_toString(type_order[mat_melted$TypeIndex]),
        tr("seriation.hover.status"),
        safe_toString(mat_melted$Presence_Label),
        tr("seriation.hover.value"),
        mat_melted$original_value,
        safe_toString(seriation_out$method_name)
      )
      
      # Ford diagram plot - Swap x/y when transposed
      if (transpose_active) {
        p <- plot_ly(
          mat_melted,
          x = ~TypeIndex, y = ~SiteIndex, z = ~Presence,  # SWAPPED!
          type = "heatmap",
          colorscale = list(c(0, "#ecf0f1"), c(1, "#2c3e50")),
          hovertext = ~hover_text,
          hoverinfo = 'text',
          showscale = FALSE,
          name = "Ford Diagramm"
        ) %>% layout(
          xaxis = list(title = "", showticklabels = FALSE, tickvals = list(), ticktext = list()),
          yaxis = list(title = "", showticklabels = FALSE, tickvals = list(), ticktext = list())
        )
      } else {
        p <- plot_ly(
          mat_melted,
          x = ~SiteIndex, y = ~TypeIndex, z = ~Presence,  # STANDARD
          type = "heatmap",
          colorscale = list(c(0, "#ecf0f1"), c(1, "#2c3e50")),
          hovertext = ~hover_text,
          hoverinfo = 'text',
          showscale = FALSE,
          name = "Ford Diagramm"
        ) %>% layout(
          xaxis = list(title = "", showticklabels = FALSE, tickvals = list(), ticktext = list()),
          yaxis = list(title = "", showticklabels = FALSE, tickvals = list(), ticktext = list())
        )
      }
      
      # ADAPTIVE FONT SIZE based on count
      n_sites <- nrow(permuted_mat)
      n_types <- ncol(permuted_mat)

      # Font size adaptive: max 14, min 8, depending on count
      x_tick_font_size <- max(8, min(14, 100 / n_sites))
      y_tick_font_size <- max(8, min(14, 100 / n_types))

      # Axis titles and labels - FULL WIDTH/HEIGHT!
      if (transpose_active) {
        p <- p %>% layout(
          xaxis = list(
            title = list(text = tr("term.types"), standoff = 0),
            range = c(0.5, ncol(permuted_mat) + 0.5),
            showticklabels = show_types,
            showgrid = FALSE,
            showline = FALSE,
            zeroline = FALSE,
            tickmode = "array",
            tickvals = 1:ncol(permuted_mat),
            ticktext = type_labels,  # TRUNCATED!
            tickangle = -90,
            tickfont = list(size = y_tick_font_size),
            side = "bottom",
            fixedrange = TRUE
          ),
          yaxis = list(
            title = list(text = tr("term.sites"), standoff = 0),
            range = c(0.5, nrow(permuted_mat) + 0.5),
            showticklabels = show_sites,
            showgrid = FALSE,
            showline = FALSE,
            zeroline = FALSE,
            tickmode = "array",
            tickvals = 1:nrow(permuted_mat),
            ticktext = site_labels,  # TRUNCATED!
            tickfont = list(size = x_tick_font_size),
            fixedrange = TRUE
          ),
          margin = list(l = 50, r = 5, t = 30, b = 60)
        )
      } else {
        p <- p %>% layout(
          xaxis = list(
            title = list(text = tr("term.sites"), standoff = 0),
            range = c(0.5, nrow(permuted_mat) + 0.5),
            showticklabels = show_sites,
            showgrid = FALSE,
            showline = FALSE,
            zeroline = FALSE,
            tickmode = "array",
            tickvals = 1:nrow(permuted_mat),
            ticktext = site_labels,  # TRUNCATED!
            tickangle = -90,
            tickfont = list(size = x_tick_font_size),
            side = "bottom",
            fixedrange = TRUE
          ),
          yaxis = list(
            title = list(text = tr("term.types"), standoff = 0),
            range = c(0.5, ncol(permuted_mat) + 0.5),
            showticklabels = show_types,
            showgrid = FALSE,
            showline = FALSE,
            zeroline = FALSE,
            tickmode = "array",
            tickvals = 1:ncol(permuted_mat),
            ticktext = type_labels,  # TRUNCATED!
            tickfont = list(size = y_tick_font_size),
            fixedrange = TRUE
          ),
          margin = list(l = 50, r = 5, t = 30, b = 60)
        )
      }
      
    } else if (plot_type == "bubble") {
      # BUBBLE DIAGRAM directly with plot_ly (NOT ggplotly!)

      # Prepare data like TOSCA
      ser.df <- data.frame(melt(permuted_mat))
      colnames(ser.df) <- c("sites", "types", "abundance")
      ser.df$sites <- factor(ser.df$sites, levels = rownames(permuted_mat))
      ser.df$types <- factor(ser.df$types, levels = colnames(permuted_mat))
      
      # Indices for plot
      ser.df$site_idx <- as.numeric(ser.df$sites)
      ser.df$type_idx <- as.numeric(ser.df$types)

      # Only positive values
      ser.df <- ser.df[ser.df$abundance > 0 & !is.na(ser.df$abundance), ]

      # Hover text
      ser.df$hover_text <- sprintf(
        "<b>Site:</b> %s<br><b>Type:</b> %s<br><b>Abundanz:</b> %.1f",
        ser.df$sites, ser.df$types, ser.df$abundance
      )
      
      plot_height <- max(600, min(2000, if (transpose_active) nrow(permuted_mat) else ncol(permuted_mat)) * 25)

      # DIRECT plot_ly (not ggplot!) - with height!
      if (transpose_active) {
        p <- plot_ly(
          ser.df,
          x = ~type_idx, y = ~site_idx,
          type = "scatter", mode = "markers",
          height = plot_height,  # SET HERE, not in layout()!
          marker = list(
            size = ~sqrt(abundance),  # sqrt transformation
            sizemode = "diameter",
            sizeref = sqrt(max(ser.df$abundance)) / 30,  # Larger bubbles (was /20)
            sizemin = 2,  # Minimum size for small values
            color = "#2c3e50",
            opacity = 0.7,
            line = list(color = "white", width = 1)
          ),
          text = ~hover_text,
          hoverinfo = "text",
          showlegend = FALSE,
          name = "Bubbles"
        )
      } else {
        p <- plot_ly(
          ser.df,
          x = ~site_idx, y = ~type_idx,
          type = "scatter", mode = "markers",
          height = plot_height,  # SET HERE, not in layout()!
          marker = list(
            size = ~sqrt(abundance),
            sizemode = "diameter",
            sizeref = sqrt(max(ser.df$abundance)) / 30,  # Larger bubbles
            sizemin = 2,  # Minimum
            color = "#2c3e50",
            opacity = 0.7,
            line = list(color = "white", width = 1)
          ),
          text = ~hover_text,
          hoverinfo = "text",
          showlegend = FALSE,
          name = "Bubbles"
        )
      }
      
      # LEGEND as Shapes (not text!)
      legend_annotations <- list()
      legend_shapes <- list()

      if (!is.null(input$bubble_show_legend) && input$bubble_show_legend) {
        max_val <- max(ser.df$abundance)

        # INTELLIGENT BREAKS based on data distribution!
        # Quantiles of ACTUAL values (not evenly distributed)
        all_vals <- ser.df$abundance[ser.df$abundance > 0]

        # INTELLIGENT BREAKS based on max_val!
        if (max_val <= 10) {
          legend_vals <- unique(c(min(all_vals), quantile(all_vals, 0.25),
                                  median(all_vals), quantile(all_vals, 0.75), max_val))
        } else if (max_val <= 50) {
          legend_vals <- c(1, 5, 10, 20, max_val)
          legend_vals <- legend_vals[legend_vals <= max_val]
        } else if (max_val <= 200) {
          legend_vals <- c(1, 10, 25, 50, 100, max_val)
          legend_vals <- legend_vals[legend_vals <= max_val]
        } else {
          legend_vals <- c(1, 10, 50, 100, 200, max_val)
          legend_vals <- legend_vals[legend_vals <= max_val]
        }
        legend_vals <- unique(round(legend_vals, 1))
        
        # Title
        legend_annotations[[1]] <- list(
          xref = "paper", yref = "paper",
          x = 1.10, y = 1.0,  # Top aligned!
          text = paste0("<b>", tr("seriation.bubble.legend.title"), "</b>"),
          showarrow = FALSE,
          xanchor = "left",
          font = list(size = 12, color = "#2c3e50")
        )

        # --- Legend circle sizes: EXACT match to the plot formula ---
        # Plot uses: diameter_px = max(sizemin, sqrt(val) / sizeref)
        #          = max(2, 30 * sqrt(val / max_val))
        # Legend uses scale = 1.0 so the circles directly mirror the data.
        sizeref_val <- sqrt(max_val) / 30   # same as marker sizeref
        sizemin_val <- 2                    # same as marker sizemin

        # Effective plot-area height (paper [0,1] maps to this many pixels)
        effective_ht <- max(400, plot_height - 110)  # subtract top(30) + bottom(80) margins

        # Fixed 44px between circle centres — consistent regardless of plot height
        y_spacing <- 44 / effective_ht
        y_start   <- if (transpose_active) 0.94 else 0.91

        for (i in seq_along(legend_vals)) {
          val   <- legend_vals[i]
          y_pos <- y_start - (i - 1) * y_spacing

          # Exact plotly formula (no scale-up) — legend mirrors actual bubble size
          actual_px      <- max(sizemin_val, sqrt(val) / sizeref_val)
          pixel_diameter <- max(5, actual_px)   # min 5px so tiny values remain visible
          radius_px      <- pixel_diameter / 2

          # Paper-coordinate radius (based on y-axis scale; slight x-ellipse is
          # acceptable since browser width is unknown)
          radius_paper <- radius_px / effective_ht

          # Circle shape
          legend_shapes[[length(legend_shapes) + 1]] <- list(
            type = "circle",
            xref = "paper", yref = "paper",
            x0 = 1.12 - radius_paper,
            y0 = y_pos - radius_paper,
            x1 = 1.12 + radius_paper,
            y1 = y_pos + radius_paper,
            fillcolor = "#2c3e50",
            opacity = 0.7,
            line = list(color = "white", width = 1)
          )

          # Number label left of circle, vertically centred on circle
          legend_annotations[[length(legend_annotations) + 1]] <- list(
            xref = "paper", yref = "paper",
            x = 1.12 - radius_paper - 0.01, y = y_pos,
            text = as.character(round(val, 0)),
            showarrow = FALSE,
            xanchor = "right",
            yanchor = "middle",
            font = list(size = 10, color = "#2c3e50")
          )
        }
      }
      
      # Axes + Layout
      if (transpose_active) {
        p <- p %>% layout(
          plot_bgcolor = "white",
          xaxis = list(
            title = tr("term.types"),
            range = c(0.5, ncol(permuted_mat) + 0.5),
            showgrid = TRUE,
            gridcolor = "#e0e0e0",
            dtick = 1,
            showticklabels = show_types,
            tickmode = "array",
            tickvals = 1:ncol(permuted_mat),
            ticktext = type_labels,
            tickangle = -90,
            fixedrange = TRUE
          ),
          yaxis = list(
            title = tr("term.sites"),
            range = c(0.5, nrow(permuted_mat) + 0.5),
            showgrid = TRUE,
            gridcolor = "#e0e0e0",
            dtick = 1,
            showticklabels = show_sites,
            tickmode = "array",
            tickvals = 1:nrow(permuted_mat),
            ticktext = site_labels,
            fixedrange = TRUE
          ),
          annotations = legend_annotations,
          shapes = legend_shapes,  # SHAPES for legend bubbles!
          margin = list(l = 80, r = 150, t = 30, b = 80)
        )
      } else {
        p <- p %>% layout(
          plot_bgcolor = "white",
          xaxis = list(
            title = tr("term.sites"),
            range = c(0.5, nrow(permuted_mat) + 0.5),
            showgrid = TRUE,
            gridcolor = "#e0e0e0",
            dtick = 1,
            showticklabels = show_sites,
            tickmode = "array",
            tickvals = 1:nrow(permuted_mat),
            ticktext = site_labels,
            tickangle = -90,
            fixedrange = TRUE
          ),
          yaxis = list(
            title = tr("term.types"),
            range = c(0.5, ncol(permuted_mat) + 0.5),
            showgrid = TRUE,
            gridcolor = "#e0e0e0",
            dtick = 1,
            showticklabels = show_types,
            tickmode = "array",
            tickvals = 1:ncol(permuted_mat),
            ticktext = type_labels,
            fixedrange = TRUE
          ),
          annotations = legend_annotations,
          shapes = legend_shapes,
          margin = list(l = 80, r = 150, t = 30, b = 80)
        )
      }
      
    } else if (plot_type == "heatmap") {
      # Abundance heatmap - Point 7: data.table::melt()
      mat_melted <- data.table::melt(
        data.table::as.data.table(permuted_mat, keep.rownames = TRUE),
        id.vars = "rn",
        variable.name = "Type",
        value.name = "Abundance"
      )
      colnames(mat_melted) <- c("Sites", "Types", "Abundance")
      
      # TOSCA: Sites horizontal (x), Types vertical (y) - SWAP!
      mat_melted$Sites <- factor(safe_toString(mat_melted$Sites), levels = safe_toString(site_order))
      mat_melted$Types <- factor(safe_toString(mat_melted$Types), levels = safe_toString(type_order))
      
      mat_melted$hover_text <- sprintf(
        "<b>Site:</b> %s<br><b>Type:</b> %s<br><b>Abundanz:</b> %.3f<br><i>SeriARC %s</i>",
        safe_toString(mat_melted$Sites), safe_toString(mat_melted$Types), 
        mat_melted$Abundance, safe_toString(seriation_out$method_name)
      )
      
      colorscale <- switch(input$heatmap_palette %||% "viridis",
                           "viridis" = "Viridis", "plasma" = "Plasma", 
                           "inferno" = "Inferno", "greys" = "Greys", "Viridis")
      
      # Heatmap with or without transpose
      if (transpose_active) {
        p <- plot_ly(
          mat_melted,
          x = ~Types, y = ~Sites, z = ~Abundance,  # SWAPPED!
          type = "heatmap", colorscale = colorscale,
          reversescale = TRUE,
          hovertext = ~hover_text, hoverinfo = 'text',
          colorbar = list(title = tr("term.abundance"))
        )
      } else {
        p <- plot_ly(
          mat_melted,
          x = ~Sites, y = ~Types, z = ~Abundance,  # STANDARD
          type = "heatmap", colorscale = colorscale,
          reversescale = TRUE,
          hovertext = ~hover_text, hoverinfo = 'text',
          colorbar = list(title = tr("term.abundance"))
        )
      }
      
    } else {
      # Battleship Curve
      type_freqs <- colMeans(permuted_mat, na.rm = TRUE)
      plot_data <- data.frame(
        Position = seq_along(type_freqs),
        Type = safe_toString(names(type_freqs)),
        Frequency = type_freqs,
        stringsAsFactors = FALSE
      )
      
      plot_data$hover_text <- sprintf(
        "<b>Type:</b> %s<br><b>Position:</b> %d<br><b>%s:</b> %.3f<br><i>SeriARC %s</i>",
        plot_data$Type, plot_data$Position, tr("seriation.hover.frequency"), plot_data$Frequency,
        safe_toString(seriation_out$method_name)
      )
      
      p <- plot_ly(
        plot_data, x = ~Position, y = ~Frequency,
        type = 'scatter', mode = 'lines+markers',
        line = list(color = "#3498db", width = 3),
        marker = list(color = "#e74c3c", size = 8),
        hovertext = ~hover_text, hoverinfo = 'text'
      )
    }
    
    # Heatmap and Battleship - Axis titles
    if (plot_type == "heatmap") {
      if (transpose_active) {
        p <- p %>% layout(
          xaxis = list(title = tr("term.types"), tickangle = -90),
          yaxis = list(title = tr("term.sites"))
        )
      } else {
        p <- p %>% layout(
          xaxis = list(title = tr("term.sites"), tickangle = -90),
          yaxis = list(title = tr("term.types"))
        )
      }
    } else if (plot_type == "battleship") {
      # Battleship Curve with axis titles
      p <- p %>% layout(
        xaxis = list(title = tr("term.types")),
        yaxis = list(title = tr("term.frequency"))
      )
    }

    # Final layout with scientific title
    title_text <- sprintf("SeriARC %s (%s)",
                          switch(plot_type,
                                 "ford" = "Ford Diagramm",
                                 "bubble" = "Bubble-Diagramm",
                                 "heatmap" = "Heatmap",
                                 "battleship" = "Battleship Curve"),
                          safe_toString(seriation_out$method_name))

    
    # Apply layout
    if (plot_type != "bubble") {
      p_final <- p %>% layout(
        title = list(text = title_text, font = list(size = 16, color = "#2c3e50", family = "Arial, sans-serif")),
        plot_bgcolor = "white", 
        paper_bgcolor = "white",
        height = 600,
        dragmode = FALSE
      ) %>% standard_plotly_config(., "2d")
    } else {
      # Bubble: DO NOT override background (stays gray)
      p_final <- p %>% layout(
        title = list(text = title_text, font = list(size = 16, color = "#2c3e50", family = "Arial, sans-serif")),
        dragmode = FALSE
      ) %>% standard_plotly_config(., "2d")
    }
    
    # Store Plotly object for HTML export (Point 4)
    plotly_obj(p_final)
    
    return(p_final)
  })
  
  # REACTIVE OUTPUTS

  # Chronological order (scientifically grounded)
  output$seriation_order <- renderText({
    seriation_data <- seriation_result()
    if (is.null(seriation_data)) {
      return(tr("validate.seriation.click.first"))
    }
    
    permuted_mat <- seriation_data$permuted_matrix %||% seriation_data$permuted_mat
    if (is.null(permuted_mat) || nrow(permuted_mat) == 0) {
      return(tr("validate.seriation.no.matrix"))
    }
    
    ordered_sites <- safe_toString(rownames(permuted_mat))
    ordered_types <- safe_toString(colnames(permuted_mat))

    # Intelligent truncation for long lists
    format_list <- function(items, max_display = 10) {
      if (length(items) <= max_display) {
        paste(items, collapse = ", ")
      } else {
        first_items <- paste(items[1:5], collapse = ", ")
        last_items <- paste(items[(length(items)-2):length(items)], collapse = ", ")
        sprintf("%s ... %s (%d total)", first_items, last_items, length(items))
      }
    }
    
    show_full <- !is.null(input$show_full_seriation_order) && input$show_full_seriation_order
    sites_text <- if (show_full) paste(ordered_sites, collapse = ", ") else format_list(ordered_sites)
    types_text <- if (show_full) paste(ordered_types, collapse = ", ") else format_list(ordered_types)
    
    sprintf(
      tr("seriation.order.format"),
      length(ordered_sites), sites_text,
      length(ordered_types), types_text,
      safe_toString(seriation_data$method_name),
      safe_toString(seriation_data$transformation),
      ""  # Placeholder for additional info (e.g., cluster overlay status)
    )
  })
  
  # Quality metrics (scientifically grounded)
  output$seriation_metrics <- renderText({
    seriation_data <- seriation_result()
    if (is.null(seriation_data)) {
      return(tr("validate.seriation.no.results"))
    }
    
    permuted_mat <- seriation_data$permuted_matrix %||% seriation_data$permuted_mat
    if (is.null(permuted_mat)) return(tr("validate.seriation.no.matrix"))

    sites_count <- nrow(permuted_mat)
    types_count <- ncol(permuted_mat)
    total_entries <- sites_count * types_count
    filled_entries <- sum(permuted_mat > 0, na.rm = TRUE)
    fill_rate <- round(filled_entries / total_entries * 100, 1)

    # Archaeological fill rate assessment (Sparsity is NORMAL!)
    # Archaeological matrices: up to 95% sparsity = 5% filled is GOOD!
    quality_score <- if (fill_rate > 15) tr("seriation.quality.excellent") else
      if (fill_rate > 10) tr("seriation.quality.verygood") else
        if (fill_rate > 5) tr("seriation.quality.good") else
          if (fill_rate > 2) tr("seriation.quality.acceptable") else tr("seriation.quality.weak")

    sprintf(
      tr("seriation.metrics.format"),
      safe_toString(seriation_data$method_name),
      safe_toString(seriation_data$transformation),
      sites_count, types_count, filled_entries, total_entries, fill_rate,
      quality_score,
      ""  # Placeholder for additional info (e.g., overlay status)
    )
  })
  
  # Seriation export data for download handler
  seriation_export_data <- reactive({
    seriation_data <- seriation_result()
    if (is.null(seriation_data)) return(NULL)
    
    permuted_mat <- seriation_data$permuted_matrix %||% seriation_data$permuted_mat
    if (is.null(permuted_mat)) return(NULL)
    
    list(
      permuted_matrix = permuted_mat,
      method_name = seriation_data$method_name %||% "Unknown",
      transformation = seriation_data$transformation %||% "None",
      plot_type = input$seriation_plot_type %||% "ford"
    )
  })
  
  # EXPORT HANDLERS - TEMPLATE-BASED

  # Helper for consistent plot exports (Point 10)
  plot_seriation_matrix <- function(mat, plot_type, method_name, max_labels = 10) {
    if (plot_type == "ford") {
      binary_mat <- ifelse(mat > 0, 1, 0)
      par(mar = c(5, 6, 4, 2))

      # Correct flip direction
      image(1:ncol(binary_mat), 1:nrow(binary_mat),
            t(binary_mat[nrow(binary_mat):1, ]),
            col = c("white", "black"), axes = FALSE,
            xlab = tr("plot.seriation.xlab.chrono"), ylab = tr("plot.seriation.ylab.chrono"),
            main = sprintf("SeriARC Ford Diagramm - %s", method_name))
      
      # Axes - ALL LABELS with adaptive font size!
      n_sites <- nrow(mat)
      n_types <- ncol(mat)

      # Font size adaptive: More labels = smaller font - INCREASED!
      type_cex <- max(0.5, min(1.0, 15 / n_types))  # min 0.5, max 1.0 (was 0.3-0.8)
      site_cex <- max(0.5, min(1.0, 15 / n_sites))

      # Show ALL labels
      axis(1, at = 1:n_types,
           labels = substr(colnames(mat), 1, 8),
           las = 2, cex.axis = type_cex)
      axis(2, at = 1:n_sites,
           labels = substr(rev(rownames(mat)), 1, 8),
           las = 2, cex.axis = site_cex)

    } else if (plot_type == "bubble") {
      # Bubble plot export
      par(mar = c(5, 6, 4, 8), bg = "#d9d9d9")  # MORE space right: 8 instead of 2!

      plot(1, type = "n", xlim = c(0.5, ncol(mat) + 0.5), ylim = c(0.5, nrow(mat) + 0.5),
           xlab = tr("plot.seriation.xlab.chrono"), ylab = tr("plot.seriation.ylab.chrono"),
           main = sprintf("SeriARC Bubble-Diagramm - %s", method_name),
           axes = FALSE)
      
      # WHITE grid
      abline(h = 1:nrow(mat), col = "white", lwd = 2)
      abline(v = 1:ncol(mat), col = "white", lwd = 2)

      # Bubbles with adjusted scaling
      for (i in 1:nrow(mat)) {
        for (j in 1:ncol(mat)) {
          val <- mat[i, j]
          if (val > 0) {
            # SQRT scaling like interactive, but larger
            cex_val <- sqrt(val / max(mat, na.rm = TRUE)) * 3.5 + 0.6
            points(j, nrow(mat) - i + 1, pch = 21, cex = cex_val, 
                   bg = "#2c3e50", col = "white", lwd = 0.8)
          }
        }
      }
      
      # Axes - ALL LABELS with adaptive font size!
      n_sites <- nrow(mat)
      n_types <- ncol(mat)

      # Font size adaptive: More labels = smaller font - INCREASED!
      type_cex <- max(0.5, min(1.0, 15 / n_types))  # min 0.5, max 1.0
      site_cex <- max(0.5, min(1.0, 15 / n_sites))

      # Show ALL labels
      axis(1, at = 1:n_types,
           labels = substr(colnames(mat), 1, 6),
           las = 2, cex.axis = type_cex)
      axis(2, at = 1:n_sites,
           labels = substr(rev(rownames(mat)), 1, 6),
           las = 2, cex.axis = site_cex)

      # Legend right with RANGES - more spacing!
      max_val <- max(mat, na.rm = TRUE)
      
      # Adaptive ranges like in the plot
      if (max_val <= 10) {
        legend_vals <- c(1.5, 3.5, 7.5)
        legend_labels <- c("1-2", "2-5", "5-10")
      } else if (max_val <= 30) {
        legend_vals <- c(1.5, 3.5, 7.5, 15, 25)
        legend_labels <- c("1-2", "2-5", "5-10", "10-20", "20-30")
      } else if (max_val <= 100) {
        legend_vals <- c(3.5, 7.5, 15, 35, 75)
        legend_labels <- c("2-5", "5-10", "10-20", "20-50", "50-100")
      } else {
        legend_vals <- c(3.5, 7.5, 15, 35, 75, max_val * 0.8)
        legend_labels <- c("2-5", "5-10", "10-20", "20-50", "50-100", paste0(">", 100))
      }
      
      legend_x <- ncol(mat) + 2  # FURTHER right: +2 instead of +1
      legend_y_start <- nrow(mat) * 0.85  # Higher positioned

      text(legend_x, legend_y_start + 3, tr("term.abundance"), cex = 1.0, font = 2, col = "#2c3e50")  # LARGER!

      for (i in seq_along(legend_vals)) {
        val <- legend_vals[i]
        y_pos <- legend_y_start - (i - 1) * 3  # MORE spacing: 3 instead of 2.5
        # Same scaling as interactive
        cex_val <- sqrt(val / max_val) * 3.5 + 0.6
        points(legend_x - 0.8, y_pos, pch = 21, cex = cex_val,  # Further left
               bg = "#2c3e50", col = "white", lwd = 0.8)
        text(legend_x + 0.3, y_pos, legend_labels[i],  # Further right
             cex = 0.85, col = "#2c3e50", adj = 0)  # LARGER: 0.85 instead of 0.7
      }
      
    } else if (plot_type == "heatmap") {
      par(mar = c(5, 6, 4, 2))

      image(1:ncol(mat), 1:nrow(mat), t(mat[nrow(mat):1, ]),
            col = heat.colors(100), axes = FALSE,
            xlab = tr("plot.seriation.xlab.chrono"), ylab = tr("plot.seriation.ylab.chrono"),
            main = sprintf("SeriARC Heatmap - %s", method_name))

      # Axes - ALL LABELS with adaptive font size!
      n_sites <- nrow(mat)
      n_types <- ncol(mat)

      # Font size adaptive: More labels = smaller font - INCREASED!
      type_cex <- max(0.5, min(1.0, 15 / n_types))  # min 0.5, max 1.0
      site_cex <- max(0.5, min(1.0, 15 / n_sites))

      # Show ALL labels
      axis(1, at = 1:n_types,
           labels = substr(colnames(mat), 1, 8),
           las = 2, cex.axis = type_cex)
      axis(2, at = 1:n_sites,
           labels = substr(rev(rownames(mat)), 1, 8),
           las = 2, cex.axis = site_cex)

    } else {
      # Battleship
      type_freqs <- colMeans(mat, na.rm = TRUE)
      par(mar = c(5, 4, 4, 2))

      plot(1:length(type_freqs), type_freqs, type = "l", lwd = 3, col = "#3498db",
           xlab = tr("plot.seriation.xlab.position"), ylab = tr("plot.seriation.ylab.avg"),
           main = sprintf("SeriARC Battleship - %s", method_name))
      points(1:length(type_freqs), type_freqs, pch = 16, col = "#e74c3c", cex = 1.2)
      grid(col = "lightgray", lty = "dotted")
    }
  }
  
  # PDF handler with create_pdf_download_handler template (Point 10)
  output$download_seriation_plot_pdf <- create_pdf_download_handler(
    reactive({ seriation_export_data() }), "Seriation_Plot",
    plot_generator_func = function(data) {
      req(data, data$permuted_matrix)
      plot_seriation_matrix(data$permuted_matrix, data$plot_type, data$method_name)
      mtext(sprintf("PDF Export | %s", Sys.Date()),
            side = 1, line = 4, cex = 0.7, col = "gray")
    },
    tr = tr
  )
  
  # PNG export with create_png_download_handler template (Point 10)
  output$download_seriation_plot_png <- create_png_download_handler(
    "seriation_plotly", "Seriation_Plot", session,
    plot_data = reactive({ seriation_export_data() }),
    plot_generator_func = function(data) {
      req(data, data$permuted_matrix)
      plot_seriation_matrix(data$permuted_matrix, data$plot_type, data$method_name)
      mtext(sprintf("PNG Export | %s", Sys.Date()),
            side = 1, line = 4, cex = 0.7, col = "gray")
    },
    tr = tr
  )
  
  # HTML handler for interactive Plotly exports (Point 4 - FIXED)
  output$download_seriation_plot_html <- downloadHandler(
    filename = function() sprintf("SeriARC_Seriation_%s_%s.html",
                                  input$seriation_plot_type %||% "ford", Sys.Date()),
    content = function(file) {
      req(plotly_obj())
      tryCatch({
        htmlwidgets::saveWidget(plotly_obj(), file = file, selfcontained = TRUE)
        showNotification(tr("notify.export.html.success"),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste(tr("error.export.html"), e$message),
                         type = "error", duration = 5)
      })
    }
  )
  
  # CSV handler with create_csv_download_handler template
  output$download_seriation_data_csv <- create_csv_download_handler(
    function() {
      req(seriation_export_data())
      data <- seriation_export_data()
      permuted_mat <- data$permuted_matrix

      csv_data <- data.frame(
        Site = rownames(permuted_mat),
        permuted_mat,
        check.names = FALSE
      )

      return(csv_data)
    },
    "Seriation_Data",
    add_header = TRUE,
    tr = tr
  )
  
  # SVG export with create_svg_download_handler template (Point 10)
  output$download_seriation_plot_svg <- create_svg_download_handler(
    "seriation_plotly", "Seriation_Plot", session,
    plot_data = reactive({ seriation_export_data() }),
    plot_generator_func = function(data) {
      req(data, data$permuted_matrix)
      plot_seriation_matrix(data$permuted_matrix, data$plot_type, data$method_name)
      mtext(sprintf("SVG Export | %s", Sys.Date()),
            side = 1, line = 4, cex = 0.7, col = "gray")
    },
    tr = tr
  )
  
  # Excel Matrix export with create_excel_download_handler template
  output$download_seriation_matrix <- create_excel_download_handler(
    function() {
      req(seriation_export_data())
      data <- seriation_export_data()
      create_seriation_excel_sheets(list(
        permuted_matrix = data$permuted_matrix,
        method_name = data$method_name,
        transformation = data$transformation
      ))
    },
    "Seriation_Matrix",
    "Seriation Matrix",
    tr = tr
  )
  
  # Excel Report export with create_excel_download_handler template
  output$download_seriation_report <- create_excel_download_handler(
    function() {
      req(seriation_export_data())
      data <- seriation_export_data()

      # Complete seriation sheets with additional information
      sheets <- create_seriation_excel_sheets(list(
        permuted_matrix = data$permuted_matrix,
        method_name = data$method_name,
        transformation = data$transformation
      ))

      return(sheets)
    },
    "Seriation_Report",
    "Seriation Report",
    tr = tr
  )
  
  # Return for other modules
  return(list(
    seriation_result = reactive({ cache$current_seriation })
  ))
}

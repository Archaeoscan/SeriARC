# mod_3d_ca.R - 3D CORRESPONDENCE ANALYSIS
# SeriARC: 3D-CA for multivariate archaeological data analysis

source("helpers/plot_components.R", local = TRUE)
source("helpers/download_components.R", local = TRUE)
source("helpers/input_components.R", local = TRUE)  

mod_3d_ca_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)

  tagList(
    h4(tr("3d.title")),

    # Standard plot options with 3D adjustments
    plot_display_options_ui(ns,
                           include_labels = TRUE,
                           include_colors = TRUE,
                           include_elements = TRUE,
                           include_size = TRUE),

    # 3D-specific display options
    div(class = "mt-3",
      h5(tr("3d.config")),
      sliderInput(ns("point_size_3d"), tr("3d.point.size"), min = 1, max = 10, value = 5),
      # Color by group applies only to sites
      selectInput(ns("ca_3d_color_by"), tr("3d.coloring"),
                  choices = c("Nach Typ" = "type", "Nach Fundstellen-Gruppe (nur Sites)" = "group"),
                  selected = "type"),
      checkboxInput(ns("show_cluster_ellipsoids"), tr("3d.show.ellipsoids"), value = TRUE)
    ),

    # Download buttons
    combined_download_buttons_ui("", include_csv = FALSE, show_plots = TRUE, show_data = TRUE)
  )
}

mod_3d_ca_server <- function(ca_result, cache, get_site_group, cluster_names = NULL,
                            cluster_colors = NULL, get_element_details = NULL,
                            input, output, session, tr = function(x) x) {

  # Helper functions
  get_cluster_name <- function(cluster_id) {
    if (!is.null(cluster_names) && is.function(cluster_names)) {
      names_mapping <- tryCatch(cluster_names(), error = function(e) NULL)
      return(names_mapping[[as.character(cluster_id)]] %||% paste("Cluster", cluster_id))
    }
    paste("Cluster", cluster_id)
  }
  
  # Safe conversion with vector length safety
  safe_num <- function(x, n) {
    if (is.null(x)) return(rep(NA_real_, n))
    result <- suppressWarnings(as.numeric(x))
    if (length(result) != n) return(rep(NA_real_, n))
    return(result)
  }
  
  safe_char <- function(x, n) {
    if (is.null(x)) return(rep("NULL", n))
    result <- as.character(x)
    if (length(result) != n) return(rep("NULL", n))
    return(result)
  }
  
  # Hex to RGBA conversion for reliable Plotly transparency
  hex_to_rgba <- function(hex, alpha = 1) {
    hex <- gsub("#", "", hex)
    r <- strtoi(substr(hex, 1, 2), 16L)
    g <- strtoi(substr(hex, 3, 4), 16L)
    b <- strtoi(substr(hex, 5, 6), 16L)
    sprintf("rgba(%d,%d,%d,%.2f)", r, g, b, alpha)
  }
  
  # 3D ellipsoid calculation (uses helper component)
  calculate_ellipsoid <- function(points, confidence = 0.95, resolution = 15) {
    calculate_confidence_ellipsoid(points, confidence, resolution)
  }

  # Data extraction (uses safe_num/safe_char with correct lengths)
  extract_ca_data <- function(coords, contrib, names, type_label, element_type, x_idx, y_idx, z_idx) {
    if (is.null(coords)) return(data.frame())
    
    coords_df <- as.data.frame(coords)
    n <- nrow(coords_df)
    
    data.frame(
      x = safe_num(coords_df[, x_idx], n),
      y = safe_num(coords_df[, y_idx], n),
      z = safe_num(coords_df[, z_idx], n),
      label = safe_char(names, n),
      type = rep(type_label, n), 
      element_type = rep(element_type, n),
      contrib_x = if(!is.null(contrib)) safe_num(contrib[, x_idx], n) else rep(0, n),
      contrib_y = if(!is.null(contrib)) safe_num(contrib[, y_idx], n) else rep(0, n),
      contrib_z = if(!is.null(contrib) && ncol(contrib) >= z_idx) safe_num(contrib[, z_idx], n) else rep(0, n),
      stringsAsFactors = FALSE
    )
  }
  
  # UI components (with correct namespacing)
  ns <- session$ns
  
  output$ca_3d_dim_select <- renderUI({
    req(ca_result())
    res <- ca_result()
    max_dims <- min(nrow(res$eig), 10)
    
    dim_choices <- setNames(
      paste0("Dim", 1:max_dims),
      sprintf("Dim%d (%.1f%%)", 1:max_dims, res$eig[1:max_dims, 2])
    )
    
    tagList(
      h4(tr("3d.dim.select.title"),
         actionButton(inputId = ns("info_3d_dims"), label = "", icon = icon("info-circle"),
                     style = "background: none; border: none; color: #3498db; padding: 0; margin-left: 5px;",
                     onclick = sprintf("Shiny.setInputValue('%s', Math.random());", ns("show_info_3d_dims")))),
      selectInput(ns("x_dim_3d"), tr("3d.axis.x"), choices = dim_choices, selected = "Dim1"),
      selectInput(ns("y_dim_3d"), tr("3d.axis.y"), choices = dim_choices, selected = "Dim2"),
      selectInput(ns("z_dim_3d"), tr("3d.axis.z"), choices = dim_choices, selected = "Dim3")
    )
  })
  
  # Info modal
  observeEvent(input$show_info_3d_dims, {
    showModal(modalDialog(
      title = tr("3d.modal.title"),
      HTML(paste0("
        <h5>", tr("3d.modal.theory.title"), "</h5>
        <p>", tr("3d.modal.theory.text"), "</p>

        <h5>", tr("3d.modal.interpretation.title"), "</h5>
        <ul>
          <li><strong>", tr("3d.modal.interpretation.dims"), "</strong> ", tr("3d.modal.interpretation.dims.text"), "</li>
          <li><strong>", tr("3d.modal.interpretation.chi2"), "</strong> ", tr("3d.modal.interpretation.chi2.text"), "</li>
          <li><strong>", tr("3d.modal.interpretation.ellipsoids"), "</strong> ", tr("3d.modal.interpretation.ellipsoids.text"), "</li>
        </ul>

        <h5>", tr("3d.modal.arch.title"), "</h5>
        <p>", tr("3d.modal.arch.text"), "</p>
      ")),
      easyClose = TRUE, footer = modalButton(tr("3d.modal.btn.ok"))
    ))
  })
  
  # 3D visualization
  output$ca_3d_plotly <- renderPlotly({
    req(ca_result(), input$x_dim_3d, input$y_dim_3d, input$z_dim_3d, input$ca_3d_plot_elements)

    withProgress(message = tr("3d.progress"), value = 0, {
      tryCatch({
        res <- ca_result()
        
        # Extract dimension indices with helper
        dim_indices <- extract_dimension_indices(input$x_dim_3d, input$y_dim_3d, input$z_dim_3d)
        x_idx <- dim_indices$x
        y_idx <- dim_indices$y
        z_idx <- dim_indices$z

        incProgress(0.25, detail = "Collecting data...")
        
        plot_data <- data.frame()
        
        # Sites (NULL safety)
        if (input$ca_3d_plot_elements %in% c("both", "sites_only")) {
          plot_data <- rbind(plot_data,
            extract_ca_data(res$row$coord, res$row$contrib, rownames(res$row$coord), 'Site', 'Active', x_idx, y_idx, z_idx)
          )
          # Supplementary only if available
          if (!is.null(res$row.sup) && !is.null(res$row.sup$coord)) {
            plot_data <- rbind(plot_data,
              extract_ca_data(res$row.sup$coord, NULL, rownames(res$row.sup$coord), 'Site', 'Supplementary', x_idx, y_idx, z_idx)
            )
          }
        }

        # Types (NULL safety)
        if (input$ca_3d_plot_elements %in% c("both", "types_only")) {
          plot_data <- rbind(plot_data,
            extract_ca_data(res$col$coord, res$col$contrib, rownames(res$col$coord), 'Type', 'Active', x_idx, y_idx, z_idx)
          )
          # Supplementary only if available
          if (!is.null(res$col.sup) && !is.null(res$col.sup$coord)) {
            plot_data <- rbind(plot_data,
              extract_ca_data(res$col.sup$coord, NULL, rownames(res$col.sup$coord), 'Type', 'Supplementary', x_idx, y_idx, z_idx)
            )
          }
        }
        
        if (nrow(plot_data) == 0) {
          showNotification(tr("notify.3d.no.data"), type = "warning")
          return(NULL)
        }
        
        incProgress(0.30, detail = "Properties...")

        # Groups
        plot_data$group <- NA_character_
        is_site <- plot_data$type == 'Site'
        if (any(is_site)) {
          site_groups <- get_site_group(plot_data$label[is_site])
          plot_data$group[is_site] <- if (is.null(site_groups)) rep(NA_character_, sum(is_site)) else as.character(site_groups)
        }
        
        # Visual properties - translated legend groups
        active_label <- if (exists("tr")) tr("term.active") else "active"
        suppl_label <- if (exists("tr")) "suppl." else "suppl."
        plot_data$legend_group <- paste(plot_data$type, ifelse(plot_data$element_type == 'Active',
                                                               paste0("(", active_label, ")"),
                                                               paste0("(", suppl_label, ")")))
        
        # Color scheme with helper components
        colors <- seri_arc_colors()
        plot_data$color <- if (!is.null(input$ca_3d_color_by) && input$ca_3d_color_by == "group") {
          unique_groups <- unique(plot_data$group[!is.na(plot_data$group)])
          if (length(unique_groups) > 0) {
            n_groups <- length(unique_groups)
            n_colors <- length(colors$clusters)
            cluster_colors <- colors$clusters[((seq_len(n_groups) - 1) %% n_colors) + 1]
            names(cluster_colors) <- unique_groups
            ifelse(is.na(plot_data$group), colors$neutral, cluster_colors[plot_data$group])
          } else colors$neutral
        } else {
          # Dynamic color assignment based on type/element_type
          ifelse(
            plot_data$type == "Site",
            ifelse(plot_data$element_type == "Active", colors$site_active, colors$site_supplementary),
            ifelse(plot_data$element_type == "Active", colors$type_active, colors$type_supplementary)
          )
        }
        
        plot_data$symbol <- ifelse(plot_data$type == 'Site',
                                  ifelse(plot_data$element_type == 'Active', "circle", "circle-open"),
                                  ifelse(plot_data$element_type == 'Active', "diamond", "diamond-open"))
        
        point_size <- if (!is.null(input$point_size_3d)) as.numeric(input$point_size_3d) else 5
        plot_data$size <- point_size
        
        # Remove NA coordinates before Plotly
        plot_data <- plot_data[complete.cases(plot_data[, c("x", "y", "z")]), ]

        # 3D layout with helper component
        x_title <- ca_3d_axis_title(input$x_dim_3d, res$eig, x_idx)
        y_title <- ca_3d_axis_title(input$y_dim_3d, res$eig, y_idx)
        z_title <- ca_3d_axis_title(input$z_dim_3d, res$eig, z_idx)
        plot_title <- sprintf("SeriARC 3D-CA: %s × %s × %s", input$x_dim_3d, input$y_dim_3d, input$z_dim_3d)
        
        # 3D scatter plot with helper layout
        p <- plot_ly() %>%
          layout(ca_3d_plotly_layout(x_title, y_title, z_title, plot_title))
        
        # Stabilize group order (defined sorting)
        legend_groups <- sort(unique(plot_data$legend_group))

        # Add groups
        for (group_name in legend_groups) {
          group_data <- plot_data[plot_data$legend_group == group_name, ]
          
          # Build hover text completely in R
          type_label <- tr("3d.hover.type")
          contrib_label <- tr("3d.hover.contrib")
          group_data$hover_text <- sprintf(
            "<b>%s</b><br>%s: %s (%s)<br>X: %.3f | Y: %.3f | Z: %.3f<br>%s: %.1f%% | %.1f%% | %.1f%%",
            group_data$label,
            type_label,
            group_data$type,
            group_data$element_type,
            group_data$x,
            group_data$y,
            group_data$z,
            contrib_label,
            group_data$contrib_x,
            group_data$contrib_y,
            group_data$contrib_z
          )

          # Opacity based on element_type
          is_active_group <- any(group_data$element_type == "Active")

          p <- p %>% add_trace(
            data = group_data,
            x = ~x, y = ~y, z = ~z,
            type = "scatter3d", mode = "markers",
            marker = list(
              size = group_data$size,
              color = group_data$color,
              symbol = group_data$symbol,
              opacity = ifelse(is_active_group, 0.9, 0.6),
              line = list(width = 1, color = "white")
            ),
            hovertext = ~hover_text,
            hoverinfo = "text",
            name = group_name,
            showlegend = TRUE
          )
        }
        
        # Cluster-Ellipsoide
        if (!is.null(input$show_cluster_ellipsoids) && input$show_cluster_ellipsoids && !is.null(cache$kmeans_result)) {
          
          incProgress(0.25, detail = tr("3d.progress.ellipsoids"))
          
          km_data <- cache$kmeans_result
          required_cols <- paste0("Dim.", c(x_idx, y_idx, z_idx))
          
          if (!is.null(km_data$data) && "cluster" %in% colnames(km_data$data) && 
              all(required_cols %in% colnames(km_data$data))) {
            
            site_cluster_data <- km_data$data[km_data$data$type == 'Site' & km_data$data$element_type == 'Active', ]
            
            if (nrow(site_cluster_data) > 0) {
              coords_3d <- data.frame(
                x = suppressWarnings(as.numeric(site_cluster_data[[paste0("Dim.", x_idx)]])),
                y = suppressWarnings(as.numeric(site_cluster_data[[paste0("Dim.", y_idx)]])),
                z = suppressWarnings(as.numeric(site_cluster_data[[paste0("Dim.", z_idx)]])),
                cluster = as.numeric(as.character(site_cluster_data$cluster))
              )
              coords_3d <- coords_3d[complete.cases(coords_3d), ]
              
              cluster_color_map <- if (!is.null(cluster_colors) && is.function(cluster_colors)) {
                tryCatch(cluster_colors(), error = function(e) colors$clusters)
              } else {
                colors$clusters
              }
              
              ellipsoid_count <- 0
              
              for (cluster_id in unique(coords_3d$cluster)) {
                cluster_points <- coords_3d[coords_3d$cluster == cluster_id, c("x", "y", "z")]
                
                # Minimum 3 points for ellipsoid
                if (nrow(cluster_points) >= 3) {
                  ellipsoid <- calculate_ellipsoid(cluster_points)
                  
                  if (!is.null(ellipsoid)) {
                    base_color <- if (is.null(names(cluster_color_map))) {
                      cluster_color_map[((cluster_id - 1) %% length(cluster_color_map)) + 1]
                    } else {
                      cluster_color_map[as.character(cluster_id)] %||% cluster_color_map[1]
                    }
                    
                    cluster_name <- get_cluster_name(cluster_id)
                    
                    # RGBA instead of Hex+Alpha for Plotly compatibility
                    p <- p %>% add_surface(
                      x = ellipsoid$x, y = ellipsoid$y, z = ellipsoid$z,
                      opacity = 0.35,
                      colorscale = list(
                        c(0, hex_to_rgba(base_color, 0.15)),
                        c(1, hex_to_rgba(base_color, 0.6))
                      ),
                      showscale = FALSE,
                      contours = list(
                        x = list(show = FALSE),
                        y = list(show = FALSE),
                        z = list(show = FALSE)
                      ),
                      name = sprintf("%s (95%%)", cluster_name),
                      hovertemplate = sprintf("<b>%s</b><br>95%% Ellipsoid<br>n=%d", cluster_name, nrow(cluster_points))
                    )
                    ellipsoid_count <- ellipsoid_count + 1
                  }
                }
              }
            }
          }
        }
        
        # Cache result
        total_var_3d <- res$eig[x_idx, 2] + res$eig[y_idx, 2] + res$eig[z_idx, 2]
        cache$ca_3d_result <- list(
          data = plot_data, 
          x_col = x_idx, y_col = y_idx, z_col = z_idx,
          total_variance = total_var_3d, 
          dimensions = c(input$x_dim_3d, input$y_dim_3d, input$z_dim_3d)
        )
        
        return(standard_plotly_config(p, "3d"))
        
      }, error = function(e) {
        showNotification(paste("3D-CA Error:", e$message), type = "error")
        return(NULL)
      })
    })
  })

  # Statistical output (compact format for narrow sidebar)
  output$ca_3d_stats <- renderText({
    if (is.null(cache$ca_3d_result)) return(tr("validate.3d.no.data"))

    result <- cache$ca_3d_result
    active_count <- sum(result$data$element_type == 'Active')
    suppl_count <- sum(result$data$element_type == 'Supplementary')
    sites_count <- sum(result$data$type == 'Site')
    types_count <- sum(result$data$type == 'Type')

    ellipsoid_status <- if (!is.null(input$show_cluster_ellipsoids) && input$show_cluster_ellipsoids) {
      tr("3d.ellipsoid.status.on")
    } else tr("3d.ellipsoid.status.off")

    # Build compact stats string with real newlines
    paste0(
      tr("3d.stats.variance"), ": ", sprintf("%.1f%%", result$total_variance), "\n",
      tr("3d.stats.points"), ": ", active_count, " ", tr("3d.stats.active"), ", ", suppl_count, " ", tr("3d.stats.suppl"), "\n",
      tr("3d.stats.ellipsoids"), ": ", ellipsoid_status
    )
  })
  
  # Eigenvalues table
  output$ca_3d_eigen <- renderTable({
    req(ca_result())
    res <- ca_result()
    max_dims <- min(nrow(res$eig), 6)

    eigen_table <- data.frame(
      Dimension = paste0("Dim", 1:max_dims),
      Eigenvalue = round(res$eig[1:max_dims, 1], 4),
      Variance = paste0(round(res$eig[1:max_dims, 2], 2), "%"),
      Cumulative = paste0(round(res$eig[1:max_dims, 3], 2), "%"),
      stringsAsFactors = FALSE
    )

    if (!is.null(input$x_dim_3d) && !is.null(input$y_dim_3d) && !is.null(input$z_dim_3d)) {
      selected_dims <- c(input$x_dim_3d, input$y_dim_3d, input$z_dim_3d)
      eigen_table$Status <- ifelse(eigen_table$Dimension %in% selected_dims, "✔", "")
    }

    # Translated column names
    names(eigen_table) <- c(
      tr("3d.eigen.col.dimension"),
      tr("3d.eigen.col.eigenvalue"),
      tr("3d.eigen.col.variance"),
      tr("3d.eigen.col.cumulative"),
      if (ncol(eigen_table) == 5) tr("3d.eigen.col.status") else NULL
    )

    eigen_table
  }, striped = TRUE, hover = TRUE)
  
  # === DOWNLOAD HANDLERS WITH HELPER COMPONENTS ===

  # PNG Export (with real 3D data as 2D projections)
  output$download_plot_png <- create_png_download_handler(
    "ca_3d_plotly", "SeriARC_3D_CA_Plot", session,
    tr = tr,
    plot_data = reactive({ cache$ca_3d_result$data }),
    plot_generator_func = function(plot_data) {
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        colors <- seri_arc_colors()
        
        # 3D as 2D projections for PNG
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

        dim_labels <- cache$ca_3d_result$dimensions

        # Distinguish supplementary by color and shape
        point_colors <- ifelse(
          plot_data$type == "Site",
          ifelse(plot_data$element_type == "Active", colors$site_active, colors$site_supplementary),
          ifelse(plot_data$element_type == "Active", colors$type_active, colors$type_supplementary)
        )
        point_shapes <- ifelse(
          plot_data$type == "Site",
          ifelse(plot_data$element_type == "Active", 16, 1),  # filled vs open circle
          ifelse(plot_data$element_type == "Active", 17, 2)   # filled vs open triangle
        )
        point_cex <- ifelse(plot_data$element_type == "Active", 1.3, 1.0)
        
        # XY projection (main plane)
        plot(plot_data$x, plot_data$y,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[1], ylab = dim_labels[2],
             main = "SeriARC 3D-CA: XY Projection")
        grid(col = "lightgray", lty = "dotted")

        # XZ projection
        plot(plot_data$x, plot_data$z,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[1], ylab = dim_labels[3],
             main = "SeriARC 3D-CA: XZ Projection")
        grid(col = "lightgray", lty = "dotted")

        # YZ projection
        plot(plot_data$y, plot_data$z,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[2], ylab = dim_labels[3],
             main = "SeriARC 3D-CA: YZ Projection")
        grid(col = "lightgray", lty = "dotted")

        # Information panel
        plot.new()
        legend("center",
               legend = c("Sites (active)", "Types (active)", "Supplementary", "3D to PNG projections"),
               col = c(colors$site_active, colors$type_active, "gray", "black"),
               pch = c(16, 17, 1, NA), lty = c(NA, NA, NA, 1),
               cex = 1.0, title = "SeriARC 3D-CA", bty = "n")

        # Variance info
        text(0.5, 0.3, sprintf("Variance: %.1f%% (3D)", cache$ca_3d_result$total_variance),
             cex = 1.1, font = 2, col = "#2c3e50")
        text(0.5, 0.2, sprintf("PNG Export | %d points | %s", nrow(plot_data), format(Sys.time(), "%Y-%m-%d %H:%M")),
             cex = 0.9, col = "gray")
      }
    }
  )
  
  # SVG Export with real 3D data (as 2D projections)
  output$download_plot_svg <- create_svg_download_handler(
    "ca_3d_plotly", "SeriARC_3D_CA_Plot", session,
    tr = tr,
    plot_data = reactive({ cache$ca_3d_result$data }),
    plot_generator_func = function(plot_data) {
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        colors <- seri_arc_colors()

        dim_labels <- cache$ca_3d_result$dimensions

        # Distinguish supplementary
        point_colors <- ifelse(
          plot_data$type == "Site",
          ifelse(plot_data$element_type == "Active", colors$site_active, colors$site_supplementary),
          ifelse(plot_data$element_type == "Active", colors$type_active, colors$type_supplementary)
        )
        point_shapes <- ifelse(
          plot_data$type == "Site",
          ifelse(plot_data$element_type == "Active", 16, 1),  # filled vs open circle
          ifelse(plot_data$element_type == "Active", 17, 2)   # filled vs open triangle
        )
        point_cex <- ifelse(plot_data$element_type == "Active", 1.3, 1.0)
        
        # 3D as 2D projections for SVG
        par(mfrow = c(2, 2), mar = c(4, 4, 3, 2))

        # XY projection (main plane)
        plot(plot_data$x, plot_data$y,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[1], ylab = dim_labels[2],
             main = "SeriARC 3D-CA: XY Projection")
        grid(col = "lightgray", lty = "dotted")

        # XZ projection
        plot(plot_data$x, plot_data$z,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[1], ylab = dim_labels[3],
             main = "SeriARC 3D-CA: XZ Projection")
        grid(col = "lightgray", lty = "dotted")

        # YZ projection
        plot(plot_data$y, plot_data$z,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[2], ylab = dim_labels[3],
             main = "SeriARC 3D-CA: YZ Projection")
        grid(col = "lightgray", lty = "dotted")

        # Information panel
        plot.new()
        legend("center",
               legend = c("Sites (active)", "Types (active)", "3D to SVG projections"),
               col = c(colors$site_active, colors$type_active, "black"),
               pch = c(16, 17, NA), lty = c(NA, NA, 1),
               cex = 1.2, title = "SeriARC 3D-CA", bty = "n")
        text(0.5, 0.2, sprintf("Created: %s", format(Sys.time(), "%Y-%m-%d %H:%M")),
             cex = 0.9, col = "gray")
      }
    }
  )
  
  # PDF Export (for 3D - as 2D projection)
  output$download_plot_pdf <- create_pdf_download_handler(
    reactive({ cache$ca_3d_result$data }), "SeriARC_3D_CA_Plot",
    tr = tr,
    plot_generator_func = function(plot_data) {
      if (!is.null(plot_data) && nrow(plot_data) > 0) {
        colors <- seri_arc_colors()

        dim_labels <- cache$ca_3d_result$dimensions

        # Distinguish supplementary
        point_colors <- ifelse(
          plot_data$type == "Site",
          ifelse(plot_data$element_type == "Active", colors$site_active, colors$site_supplementary),
          ifelse(plot_data$element_type == "Active", colors$type_active, colors$type_supplementary)
        )
        point_shapes <- ifelse(
          plot_data$type == "Site",
          ifelse(plot_data$element_type == "Active", 16, 1),
          ifelse(plot_data$element_type == "Active", 17, 2)
        )
        point_cex <- ifelse(plot_data$element_type == "Active", 1.3, 1.0)
        
        par(mfrow = c(2, 2))
        
        # XY projection
        plot(plot_data$x, plot_data$y,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[1], ylab = dim_labels[2],
             main = "SeriARC 3D-CA: XY Projection")
        grid(col = "lightgray", lty = "dotted")

        # XZ projection
        plot(plot_data$x, plot_data$z,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[1], ylab = dim_labels[3],
             main = "SeriARC 3D-CA: XZ Projection")
        grid(col = "lightgray", lty = "dotted")

        # YZ projection
        plot(plot_data$y, plot_data$z,
             col = point_colors, pch = point_shapes, cex = point_cex,
             xlab = dim_labels[2], ylab = dim_labels[3],
             main = "SeriARC 3D-CA: YZ Projection")
        grid(col = "lightgray", lty = "dotted")

        # Legend
        plot.new()
        legend("center", legend = c("Sites", "Types"), 
               col = c(colors$site_active, colors$type_active), 
               pch = c(16, 17), cex = 1.5, title = "SeriARC 3D-CA")
      }
    }
  )
  
  # Excel Export
  output$download_data_excel <- create_excel_download_handler(
    function() {
      req(cache$ca_3d_result)
      result <- cache$ca_3d_result

      sheets <- list()

      # 3D coordinates
      coords_data <- result$data[, c("label", "type", "element_type", "x", "y", "z")]
      colnames(coords_data) <- c("Entity", "Element_Type", "Status", "X_Coordinate", "Y_Coordinate", "Z_Coordinate")
      sheets[["3D_Coordinates"]] <- coords_data

      # 3D parameters
      sheets[["3D_Parameters"]] <- data.frame(
        Parameter = c("X_Dimension", "Y_Dimension", "Z_Dimension", "Total_Variance", "Analyzed_at"),
        Value = c(result$dimensions[1], result$dimensions[2], result$dimensions[3],
                sprintf("%.1f%%", result$total_variance),
                format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
      )

      # Contributions if available
      if (any(c("contrib_x", "contrib_y", "contrib_z") %in% colnames(result$data))) {
        contrib_data <- result$data[, c("label", "type", "contrib_x", "contrib_y", "contrib_z")]
        colnames(contrib_data) <- c("Entity", "Element_Type", "Contrib_X", "Contrib_Y", "Contrib_Z")
        sheets[["3D_Contributions"]] <- contrib_data
      }

      return(sheets)
    },
    "3D_CA_Analysis", "3D Correspondence Analysis",
    tr = tr
  )

  # Reset 3D view
  observeEvent(input$reset_3d_view, {
    # Reset Plotly 3D camera to default position
    plotlyProxy("ca_3d_plotly", session) %>%
      plotlyProxyInvoke("relayout", list(
        scene = list(
          camera = list(
            eye = list(x = 1.25, y = 1.25, z = 1.25)
          )
        )
      ))
    showNotification(tr("notify.3d.reset"), type = "message", duration = 2)
  })
  
  return(list(ca_3d_result = reactive({ cache$ca_3d_result })))
}

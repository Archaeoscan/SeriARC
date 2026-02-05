# PLOT COMPONENTS HELPER - SeriARC V1.5
# Reusable Plotly configurations and plot utilities

# === STANDARD COLOR SCHEMES ===

# KHROMA: Scientific, colorblind-safe color schemes
seri_arc_khroma_colors <- function(scheme = "bright", n = NULL) {
  # Ensure khroma is available
  if (!requireNamespace("khroma", quietly = TRUE)) {
    warning("khroma not available - using fallback colors")
    return(seri_arc_colors_fallback())
  }
  
  tryCatch({
    # Main schemes for different applications
    color_func <- switch(scheme,
      # === QUALITATIVE SCHEMES (for groups, clusters) ===
      "bright"     = khroma::colour("bright"),      # Paul Tol - colorblind-safe
      "vibrant"    = khroma::colour("vibrant"),     # Paul Tol - vivid
      "muted"      = khroma::colour("muted"),       # Paul Tol - muted
      "okabe_ito"  = khroma::colour("okabe ito"),   # Okabe & Ito - classic scientific

      # === DIVERGING SCHEMES (for CA dimensions) ===
      "sunset"     = khroma::colour("sunset"),      # Warm-cold transition
      "BuRd"       = khroma::colour("BuRd"),        # Blue-red classic
      "PRGn"       = khroma::colour("PRGn"),        # Purple-green

      # === SEQUENTIAL SCHEMES (for intensities) ===
      "YlOrBr"     = khroma::colour("YlOrBr"),      # Yellow-orange-brown
      "iridescent" = khroma::colour("iridescent"),  # Rainbow alternative

      # === FALLBACK ===
      khroma::colour("bright")  # Default: Paul Tol bright
    )
    
    # Determine or use number automatically
    n_colors <- if (is.null(n)) {
      switch(scheme,
        "bright" = 7, "vibrant" = 7, "muted" = 9, "okabe_ito" = 8,
        "sunset" = 11, "BuRd" = 9, "PRGn" = 9,
        "YlOrBr" = 9, "iridescent" = 23,
        7  # Standard
      )
    } else n
    
    # Generate colors
    colors <- color_func(n_colors)
    
    # Return as character vector (without attributes)
    return(as.character(colors))
    
  }, error = function(e) {
    warning("Khroma scheme '", scheme, "' failed: ", e$message, " - using fallback")
    return(seri_arc_colors_fallback())
  })
}

# Fallback colors when Khroma is not available
seri_arc_colors_fallback <- function() {
  c("#2980b9", "#c0392b", "#27ae60", "#f39c12", "#9b59b6", 
    "#1abc9c", "#e67e22", "#34495e", "#e91e63", "#795548")
}

# SeriARC standard color palette (EXTENDED with Khroma)
seri_arc_colors <- function(use_khroma = TRUE) {
  # Base scheme: Paul Tol "bright" (colorblind-safe)
  khroma_colors <- if (use_khroma) {
    seri_arc_khroma_colors("bright", 10)
  } else {
    seri_arc_colors_fallback()
  }
  
  list(
    # CA elements (first 4 colors from Khroma)
    site_active = khroma_colors[1],          # Blue for active sites
    site_supplementary = khroma_colors[5],   # Light blue for suppl. sites
    type_active = khroma_colors[2],          # Red for active types
    type_supplementary = khroma_colors[6],   # Light red for suppl. types

    # Cluster colors (all Khroma colors)
    clusters = khroma_colors,

    # Seriation colors (sequential scheme)
    seriation_gradient = if (use_khroma) {
      seri_arc_khroma_colors("YlOrBr", 6)
    } else {
      c("#ffffcc", "#c7e9b4", "#7fcdbb", "#41b6c4", "#2c7fb8", "#253494")
    },
    
    # UI status colors (keep original)
    excellent = "#27ae60",
    good = "#f39c12", 
    warning = "#e67e22",
    error = "#e74c3c",
    neutral = "#95a5a6"
  )
}

# Factor-based color assignment (for groups) - EXTENDED with Khroma
seri_arc_factor_colors <- function(factor_var, palette = "default", use_khroma = TRUE) {
  colors <- seri_arc_colors(use_khroma)
  n_levels <- nlevels(factor_var)
  
  color_palette <- switch(palette,
    "default" = colors$clusters[1:min(n_levels, length(colors$clusters))],
    "clusters" = colors$clusters[1:min(n_levels, length(colors$clusters))],
    "seriation" = colors$seriation_gradient[1:min(n_levels, length(colors$seriation_gradient))],
    
    # NEW KHROMA OPTIONS
    "bright" = if (use_khroma) {
      seri_arc_khroma_colors("bright", n_levels)
    } else {
      colors$clusters[1:min(n_levels, length(colors$clusters))]
    },
    "vibrant" = if (use_khroma) {
      seri_arc_khroma_colors("vibrant", n_levels) 
    } else {
      colors$clusters[1:min(n_levels, length(colors$clusters))]
    },
    "muted" = if (use_khroma) {
      seri_arc_khroma_colors("muted", n_levels)
    } else {
      colors$clusters[1:min(n_levels, length(colors$clusters))]
    },
    "okabe_ito" = if (use_khroma) {
      seri_arc_khroma_colors("okabe_ito", n_levels)
    } else {
      colors$clusters[1:min(n_levels, length(colors$clusters))]
    },
    
    # Fallback
    colors$clusters[1:min(n_levels, length(colors$clusters))]
  )
  
  # Repeat colors if too few
  if (n_levels > length(color_palette)) {
    color_palette <- rep(color_palette, length.out = n_levels)
  }
  
  names(color_palette) <- levels(factor_var)
  list(cols = color_palette, factor = factor_var)
}

# === LABEL POSITIONING ===

# Smart label distribution for 14C overlays
calculate_smart_label_positions <- function(site_x, site_y, offset_distance = 0.25, min_separation = 0.15) {
  n_sites <- length(site_x)
  if (n_sites == 0) return(list(x = numeric(0), y = numeric(0)))
  
  center_x <- mean(site_x, na.rm = TRUE)
  center_y <- mean(site_y, na.rm = TRUE)
  
  # Initial positions: radially away from center
  label_x <- numeric(n_sites)
  label_y <- numeric(n_sites)
  
  for (i in 1:n_sites) {
    # Direction vector away from center
    dx <- site_x[i] - center_x
    dy <- site_y[i] - center_y
    length_vec <- sqrt(dx^2 + dy^2)
    
    if (length_vec > 0) {
      # Normalize and scale for offset
      unit_dx <- dx / length_vec
      unit_dy <- dy / length_vec
      
      label_x[i] <- site_x[i] + unit_dx * offset_distance
      label_y[i] <- site_y[i] + unit_dy * offset_distance
    } else {
      # If site is at center: random direction
      angle <- runif(1, 0, 2 * pi)
      label_x[i] <- site_x[i] + cos(angle) * offset_distance
      label_y[i] <- site_y[i] + sin(angle) * offset_distance
    }
  }
  
  # Simple anti-overlap logic (iterative adjustment)
  if (n_sites > 1) {
    for (iteration in 1:3) {  # Maximum 3 iterations
      adjusted <- FALSE
      
      for (i in 1:(n_sites-1)) {
        for (j in (i+1):n_sites) {
          dist <- sqrt((label_x[i] - label_x[j])^2 + (label_y[i] - label_y[j])^2)
          
          if (dist < min_separation) {
            # Labels too close: push them apart slightly
            mid_x <- (label_x[i] + label_x[j]) / 2
            mid_y <- (label_y[i] + label_y[j]) / 2
            
            # Direction between labels
            dx <- label_x[j] - label_x[i]
            dy <- label_y[j] - label_y[i]
            
            if (dx != 0 || dy != 0) {
              # Normalize
              norm <- sqrt(dx^2 + dy^2)
              dx <- dx / norm
              dy <- dy / norm
              
              # Push labels apart
              push_distance <- (min_separation - dist) / 2 + 0.02
              label_x[i] <- label_x[i] - dx * push_distance
              label_y[i] <- label_y[i] - dy * push_distance
              label_x[j] <- label_x[j] + dx * push_distance
              label_y[j] <- label_y[j] + dy * push_distance
              
              adjusted <- TRUE
            }
          }
        }
      }
      
      if (!adjusted) break  # No more adjustments needed
    }
  }
  
  list(x = label_x, y = label_y)
}

# NEW FUNCTION: Spread Labels for CA plots
# Similar to tosca, but optimized for SeriARC
spread_labels_ca <- function(point_x, point_y, labels, 
                             offset_distance = 0.05, 
                             min_separation = 0.08,
                             max_iterations = 5) {
  n_points <- length(point_x)
  if (n_points == 0 || length(labels) == 0) {
    return(list(x = numeric(0), y = numeric(0), positions = character(0)))
  }
  
  # Only consider points with labels
  has_label <- nzchar(labels)
  if (!any(has_label)) {
    return(list(x = point_x, y = point_y, positions = rep("middle center", n_points)))
  }
  
  # Filter to labeled points
  idx <- which(has_label)
  px <- point_x[idx]
  py <- point_y[idx]
  
  center_x <- mean(point_x, na.rm = TRUE)
  center_y <- mean(point_y, na.rm = TRUE)
  
  # Initial label positions: away from center
  label_x <- numeric(length(px))
  label_y <- numeric(length(py))
  text_pos <- character(length(px))
  
  for (i in seq_along(px)) {
    dx <- px[i] - center_x
    dy <- py[i] - center_y
    
    # At 0 offset: labels directly at point
    if (offset_distance == 0) {
      label_x[i] <- px[i]
      label_y[i] <- py[i]
    } else {
      # With offset: quadrant-based away from center
      if (abs(dx) > abs(dy)) {
        if (dx > 0) {
          label_x[i] <- px[i] + offset_distance
          label_y[i] <- py[i]
        } else {
          label_x[i] <- px[i] - offset_distance
          label_y[i] <- py[i]
        }
      } else {
        if (dy > 0) {
          label_x[i] <- px[i]
          label_y[i] <- py[i] + offset_distance
        } else {
          label_x[i] <- px[i]
          label_y[i] <- py[i] - offset_distance
        }
      }
    }
    
    # Text position: quadrant-based (label away from center)
    if (abs(dx) > abs(dy)) {
      text_pos[i] <- if (dx > 0) "middle left" else "middle right"
    } else {
      text_pos[i] <- if (dy > 0) "bottom center" else "top center"
    }
  }
  
  # Iterative anti-overlap
  for (iter in 1:max_iterations) {
    moved <- FALSE
    
    for (i in 1:(length(label_x)-1)) {
      for (j in (i+1):length(label_x)) {
        # Distance between labels
        dist <- sqrt((label_x[i] - label_x[j])^2 + (label_y[i] - label_y[j])^2)
        
        if (dist < min_separation) {
          dx <- label_x[j] - label_x[i]
          dy <- label_y[j] - label_y[i]
          
          if (dx == 0 && dy == 0) {
            # Identical position: random direction
            angle <- runif(1, 0, 2*pi)
            dx <- cos(angle)
            dy <- sin(angle)
          }
          
          # Normalize
          len <- sqrt(dx^2 + dy^2)
          if (len > 0) {
            dx <- dx / len
            dy <- dy / len

            # Push apart
            push <- (min_separation - dist) / 2 + 0.01
            label_x[i] <- label_x[i] - dx * push
            label_y[i] <- label_y[i] - dy * push
            label_x[j] <- label_x[j] + dx * push
            label_y[j] <- label_y[j] + dy * push
            
            moved <- TRUE
          }
        }
      }
    }
    
    if (!moved) break
  }
  
  result_x <- point_x
  result_y <- point_y
  result_pos <- rep("middle center", n_points)
  
  result_x[idx] <- label_x
  result_y[idx] <- label_y
  result_pos[idx] <- text_pos
  
  list(x = result_x, y = result_y, positions = result_pos)
}

# Standard Plotly configuration for double-click reset
# CHANGED: Explicitly sets dragmode='pan' for 2D plots!
standard_plotly_config <- function(plot, plot_type = "2d") {
  if (plot_type == "3d") {
    plot %>% 
      plotly::layout(dragmode = "turntable") %>%
      plotly::config(
        doubleClick = "reset",
        scrollZoom = TRUE,
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian", "toggleSpikelines"),
        displaylogo = FALSE,
        responsive = TRUE
      )
  } else {
    plot %>% 
      plotly::layout(dragmode = "pan") %>%
      plotly::config(
        doubleClick = "reset",
        scrollZoom = TRUE,
        displayModeBar = TRUE,
        modeBarButtonsToRemove = c("lasso2d", "select2d", "hoverClosestCartesian", "hoverCompareCartesian"),
        displaylogo = FALSE,
        responsive = TRUE
      )
  }
}

# Standard CA-Plot Layout
ca_plotly_layout <- function(x_title, y_title, plot_title = NULL, subtitle = NULL) {
  layout_config <- list(
    xaxis = list(
      title = x_title,
      zeroline = TRUE, 
      zerolinewidth = 2, 
      zerolinecolor = "#95a5a6", 
      gridcolor = "#ecf0f1",
      showgrid = TRUE
    ),
    yaxis = list(
      title = y_title,
      zeroline = TRUE, 
      zerolinewidth = 2, 
      zerolinecolor = "#95a5a6", 
      gridcolor = "#ecf0f1",
      showgrid = TRUE
    ),
    hovermode = 'closest',
    plot_bgcolor = "white",
    paper_bgcolor = "white",
    font = list(family = "Arial, sans-serif", size = 12, color = "#2c3e50"),
    margin = list(r = 150),  # Right margin for legend
    legend = list(
      orientation = "v",
      x = 1.02, y = 1,
      xanchor = "left",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.95)",
      bordercolor = "#2c3e50",
      borderwidth = 1
    ),
    showlegend = TRUE,
    dragmode = "pan",
    doubleClickDelay = 300
  )
  
  # Title configuration
  if (!is.null(plot_title)) {
    title_text <- plot_title
    if (!is.null(subtitle)) {
      title_text <- paste0(plot_title, "<br><sub style='font-size:12px'>", subtitle, "</sub>")
    }
    
    layout_config$title <- list(
      text = title_text,
      x = 0.5,
      font = list(size = 18, color = "#2c3e50", family = "Arial, sans-serif")
    )
  }
  
  layout_config
}

# 3D-Plot Layout Template
ca_3d_plotly_layout <- function(x_title, y_title, z_title, plot_title = NULL) {
  list(
    title = plot_title,
    scene = list(
      xaxis = list(title = x_title, zeroline = TRUE),
      yaxis = list(title = y_title, zeroline = TRUE), 
      zaxis = list(title = z_title, zeroline = TRUE),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5)),
      aspectmode = 'cube'
    ),
    showlegend = TRUE,
    font = list(family = "Arial, sans-serif", size = 12, color = "#2c3e50"),
    dragmode = "turntable",
    doubleClickDelay = 300
  )
}

# === HOVER TEXT GENERATORS ===

# CA Hover text generator
generate_ca_hover_text <- function(data, x_dim, y_dim, get_element_details = NULL) {
  if (is.null(get_element_details)) {
    # Standard hover text with group (if available)
    sapply(1:nrow(data), function(i) {
      # Basic info
      basic_info <- sprintf(
        "<b>%s</b><br>Typ: %s %s<br>%s: %.3f<br>%s: %.3f",
        data$label[i], data$type[i], 
        ifelse(data$element_type[i] == "Supplementary", "(🔹 Projected)", "(🔵 Active)"),
        x_dim, data$x[i], y_dim, data$y[i]
      )
      
      # Group information (only for sites with group)
      group_info <- ""
      if (data$type[i] == "Site" && "group" %in% colnames(data) && !is.na(data$group[i])) {
        group_info <- sprintf("<br>🎯 <b>Group:</b> %s", data$group[i])
      }

      # Contribution info
      contrib_info <- if (data$element_type[i] == "Supplementary") {
        "<br>Contribution: 0% (not in calculation)"
      } else {
        sprintf("<br>Contribution %s: %.2f%% | %s: %.2f%%", 
                x_dim, data$contrib_x[i], y_dim, data$contrib_y[i])
      }
      
      paste0(basic_info, group_info, contrib_info, "<br><i>SeriARC CA</i>")
    })
  } else {
    # Extended hover text with element details AND group
    sapply(1:nrow(data), function(i) {
      details <- get_element_details(data$label[i], data$type[i])
      
      basic_info <- sprintf(
        "<b>%s</b><br>Typ: %s %s<br>%s: %.3f<br>%s: %.3f",
        data$label[i], data$type[i], 
        ifelse(data$element_type[i] == "Supplementary", "(🔹 Projected)", "(🔵 Active)"),
        x_dim, data$x[i], y_dim, data$y[i]
      )
      
      # Group information (only for sites with group)
      group_info <- ""
      if (data$type[i] == "Site" && "group" %in% colnames(data) && !is.na(data$group[i])) {
        group_info <- sprintf("<br>🎯 <b>Group:</b> %s", data$group[i])
      }

      param_info <- sprintf("<br><b>⚖️ Weight:</b> %.2f (%s)", details$weight, details$impact)

      contrib_info <- if (data$element_type[i] == "Supplementary") {
        "<br><b>Contribution:</b> 0% (projected)"
      } else {
        sprintf("<br><b>Contribution %s:</b> %.2f%% | <b>%s:</b> %.2f%%", 
                x_dim, data$contrib_x[i], y_dim, data$contrib_y[i])
      }
      
      paste0(basic_info, group_info, param_info, contrib_info, "<br><i>SeriARC CA (Greenacre 2007)</i>")
    })
  }
}

# K-Means Hover text generator
generate_kmeans_hover_text <- function(data, x_dim, y_dim, get_cluster_name_func, get_element_details = NULL) {
  projection_status <- ifelse(data$element_type == 'Active', "clustered", "projected")

  if (is.null(get_element_details)) {
    # Standard hover text
    sprintf(
      "<b>%s</b><br>Typ: %s (%s)<br>Cluster: %s (%s)<br>%s: %.3f<br>%s: %.3f<br><i>SeriARC Clustering</i>",
      data$label, data$type, data$element_type, 
      sapply(data$cluster, get_cluster_name_func), projection_status,
      x_dim, data$x, y_dim, data$y
    )
  } else {
    # Extended hover text with element details
    sapply(1:nrow(data), function(i) {
      details <- get_element_details(data$label[i], data$type[i])
      
      basic_info <- sprintf(
        "<b>%s</b><br>Typ: %s (%s)<br>Cluster: %s (%s)",
        data$label[i], data$type[i], data$element_type[i], 
        get_cluster_name_func(data$cluster[i]), projection_status[i]
      )
      
      coord_info <- sprintf(
        "<br>%s: %.3f<br>%s: %.3f",
        x_dim, data$x[i], y_dim, data$y[i]
      )
      
      param_info <- sprintf(
        "<br><b>⚖️ Weight:</b> %.2f (%s)<br><b>🎯 Status:</b> %s",
        details$weight, details$impact, details$status
      )

      paste0(basic_info, coord_info, param_info, "<br><i>SeriARC Transparency Clustering</i>")
    })
  }
}

# Bootstrap Hover text generator  
generate_bootstrap_hover_text <- function(data, x_dim, y_dim, x_idx, y_idx, method, get_element_details = NULL) {
  base_text <- sprintf(
    "<b>%s</b><br>%s (cabootcrs)<br>Dim%d: %.3f<br>Dim%d: %.3f<br>SD: %.4f (%s)<br><i>%s</i>",
    data$label, data$type, x_idx, data$x, y_idx, data$y,
    data$sd_total, data$stability, method
  )
  
  if (!is.null(get_element_details)) {
    tryCatch({
      details <- get_element_details(data$label, data$type)
      if (!is.null(details)) {
        transparency_text <- sprintf(
          "<br><hr><b>Transparency:</b><br>Weight: %.2f<br>Status: %s<br>%s",
          details$weight, details$status, details$impact
        )
        base_text <- paste0(base_text, transparency_text)
      }
    }, error = function(e) {})
  }
  
  base_text
}

# === MARKER CONFIGURATIONS ===

# Standard CA marker for sites
ca_site_marker <- function(colors, sizes = 8, opacity = 0.9, element_type = "Active") {
  if (element_type == "Supplementary") {
    list(
      symbol = "circle-open", 
      size = sizes * 1.25,
      color = colors, 
      opacity = opacity * 0.7,
      line = list(width = 2, color = colors)
    )
  } else {
    list(
      symbol = "circle", 
      size = sizes,
      color = colors, 
      opacity = opacity,
      line = list(width = 1, color = "white")
    )
  }
}

# Standard CA marker for types
ca_type_marker <- function(colors, sizes = 8, opacity = 0.9, element_type = "Active") {
  if (element_type == "Supplementary") {
    list(
      symbol = "triangle-up-open", 
      size = sizes * 1.25,
      color = colors, 
      opacity = opacity * 0.7,
      line = list(width = 2, color = colors)
    )
  } else {
    list(
      symbol = "triangle-up", 
      size = sizes,
      color = colors, 
      opacity = opacity,
      line = list(width = 1, color = "white")
    )
  }
}

# === AXIS TITLE GENERATORS ===

# CA axis title with variance
ca_axis_title <- function(dimension, eigenvalues, dim_index) {
  variance <- if (!is.null(eigenvalues) && length(eigenvalues) >= dim_index) {
    eigenvalues[dim_index, 2]
  } else 0
  sprintf("%s (%.1f%% Varianz)", dimension, variance)
}

# 3D CA axis title
ca_3d_axis_title <- function(dimension, eigenvalues, dim_index) {
  variance <- if (!is.null(eigenvalues) && length(eigenvalues) >= dim_index) {
    eigenvalues[dim_index, 2]  
  } else 0
  sprintf("%s (%.1f%%)", dimension, variance)
}

# === CONFIDENCE ELLIPSE CALCULATION ===

calculate_confidence_ellipse <- function(x, y, conf_level = 0.95, npoints = 100) {
  if (length(x) < 3 || length(y) < 3) return(NULL)

  tryCatch({
    # Filter valid points
    valid_idx <- !is.na(x) & !is.na(y)
    x <- x[valid_idx]
    y <- y[valid_idx]
    
    if (length(x) < 3) return(NULL)
    
    # Center and covariance matrix
    center_x <- mean(x)
    center_y <- mean(y)
    cov_mat <- cov(cbind(x, y))
    
    # Eigenvalues and eigenvectors
    eg <- eigen(cov_mat)
    a <- sqrt(eg$values[1] * qchisq(conf_level, df = 2))
    b <- sqrt(eg$values[2] * qchisq(conf_level, df = 2))
    angle <- atan2(eg$vectors[2, 1], eg$vectors[1, 1])
    
    # Generate ellipse points
    t <- seq(0, 2*pi, length.out = npoints)
    ex <- a * cos(t)
    ey <- b * sin(t)
    ca <- cos(angle)
    sa <- sin(angle)
    
    data.frame(
      x = center_x + ex * ca - ey * sa,
      y = center_y + ex * sa + ey * ca
    )
  }, error = function(e) NULL)
}

calculate_confidence_ellipsoid <- function(points, confidence = 0.95, resolution = 15) {
  if (nrow(points) < 3) return(NULL)
  
  tryCatch({
    center <- colMeans(points, na.rm = TRUE)
    cov_matrix <- cov(points)
    eigen_decomp <- eigen(cov_matrix)
    
    chi2_val <- qchisq(confidence, df = 3)
    radii <- sqrt(pmax(eigen_decomp$values, 0.001) * chi2_val)
    
    # Sphere coordinates
    u <- seq(0, 2*pi, length.out = resolution)
    v <- seq(0, pi, length.out = resolution)
    
    x_sphere <- outer(cos(u), sin(v))
    y_sphere <- outer(sin(u), sin(v))
    z_sphere <- outer(rep(1, length(u)), cos(v))
    
    # Ellipsoid transformation
    ellipsoid_points <- array(0, dim = c(resolution, resolution, 3))
    
    for (i in 1:resolution) {
      for (j in 1:resolution) {
        scaled_point <- c(
          radii[1] * x_sphere[i, j], 
          radii[2] * y_sphere[i, j], 
          radii[3] * z_sphere[i, j]
        )
        ellipsoid_points[i, j, ] <- eigen_decomp$vectors %*% scaled_point + center
      }
    }
    
    list(
      x = ellipsoid_points[, , 1], 
      y = ellipsoid_points[, , 2], 
      z = ellipsoid_points[, , 3], 
      center = center
    )
  }, error = function(e) NULL)
}

# === PLOT UTILITY FUNCTIONS ===

# Extract dimension indices (for 3D-CA)
extract_dimension_indices <- function(x_dim, y_dim, z_dim = NULL) {
  x_idx <- as.numeric(gsub("Dim", "", x_dim))
  y_idx <- as.numeric(gsub("Dim", "", y_dim))
  
  result <- list(x = x_idx, y = y_idx)
  
  if (!is.null(z_dim)) {
    z_idx <- as.numeric(gsub("Dim", "", z_dim))
    result$z <- z_idx
  }
  
  return(result)
}

# Extend Plotly plot with standard layout
apply_ca_layout <- function(p, x_title, y_title, plot_title = NULL, subtitle = NULL) {
  p %>% plotly::layout(ca_plotly_layout(x_title, y_title, plot_title, subtitle))
}

# Optimize dimension range for plot zoom
optimize_plot_range <- function(coords, margin_factor = 0.1) {
  if (is.null(coords) || length(coords) == 0) return(NULL)
  
  range_min <- min(coords, na.rm = TRUE)
  range_max <- max(coords, na.rm = TRUE)
  range_span <- range_max - range_min
  margin <- range_span * margin_factor
  
  list(
    min = range_min - margin,
    max = range_max + margin
  )
}

# Prepare plot data for export
prepare_plot_export_data <- function(plot_data, analysis_type = "CA") {
  export_data <- plot_data[, c("label", "type", "element_type", "x", "y")]

  # Additional columns depending on analysis type
  if ("cluster" %in% colnames(plot_data)) {
    export_data$cluster <- plot_data$cluster
  }
  
  if ("contrib_x" %in% colnames(plot_data)) {
    export_data$contrib_x <- plot_data$contrib_x
    export_data$contrib_y <- plot_data$contrib_y
  }
  
  if ("group" %in% colnames(plot_data)) {
    export_data$group <- plot_data$group
  }
  
  export_data
}

# Scientific quality assessment for plots
evaluate_plot_quality <- function(variance_explained, n_points, n_dims = 2) {
  quality_score <- 0

  # Variance criterion
  if (variance_explained > 70) quality_score <- quality_score + 40
  else if (variance_explained > 50) quality_score <- quality_score + 30
  else if (variance_explained > 30) quality_score <- quality_score + 20
  else quality_score <- quality_score + 10
  
  # Data point criterion
  if (n_points >= 50) quality_score <- quality_score + 30
  else if (n_points >= 20) quality_score <- quality_score + 25
  else if (n_points >= 10) quality_score <- quality_score + 15
  else quality_score <- quality_score + 5
  
  # Dimension criterion
  if (n_dims >= 3) quality_score <- quality_score + 20
  else if (n_dims >= 2) quality_score <- quality_score + 15
  else quality_score <- quality_score + 5
  
  # Assessment
  if (quality_score >= 85) "🟢 Excellent"
  else if (quality_score >= 70) "🟡 Good"
  else if (quality_score >= 50) "🟠 Medium"
  else "🔴 Weak"
}

# === CHRONOLOGY-SPECIFIC PLOT FUNCTIONS ===

# Chronology curve plot (base R)
plot_chrono_curve <- function(chrono_df, main_title = "Chronology Curve", method_info = NULL) {
  # Validate input data
  if (is.null(chrono_df) || nrow(chrono_df) == 0) {
    plot(1, 1, type="n", xlab="", ylab="", main="No chronology data available")
    text(1, 1, "Calculate a chronology curve first", cex=1.2, col="red")
    return(invisible())
  }

  # Check required columns
  required_cols <- c("site_id", "t_idx")
  missing_cols <- required_cols[!required_cols %in% names(chrono_df)]
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in chronology data: %s", paste(missing_cols, collapse=", ")))
  }

  # Sort by t_idx for correct curve
  ord <- chrono_df[order(chrono_df$t_idx), ]
  n_sites <- nrow(ord)
  
  # Title with method info
  plot_title <- if (!is.null(method_info)) {
    paste(main_title, "-", method_info)
  } else {
    main_title
  }
  
  # Main plot: Time index curve
  plot(ord$t_idx, type="l",
       xlab="CA Order (Sites)",
       ylab="Time Index (smoothed)",
       main=plot_title,
       col="#2980b9", lwd=2,
       xaxt="n")  # Manual axis labels

  # Add site points
  points(ord$t_idx, pch=16, cex=0.6, col="#e74c3c")

  # Nice x-axis with site names (max 10)
  if (n_sites <= 10) {
    axis(1, at=1:n_sites, labels=ord$site_id, las=2, cex.axis=0.8)
  } else {
    # For many sites: label only every n-th site
    step <- ceiling(n_sites / 8)
    label_idx <- seq(1, n_sites, by=step)
    axis(1, at=label_idx, labels=ord$site_id[label_idx], las=2, cex.axis=0.7)
  }

  # Add grid
  grid(col="lightgray", lty=2, lwd=0.5)
  
  # Statistics info as text
  stats_text <- sprintf("Sites: %d | Range: %.2f | SD: %.2f", 
                       n_sites, 
                       diff(range(ord$t_idx, na.rm=TRUE)),
                       sd(ord$t_idx, na.rm=TRUE))
  mtext(stats_text, side=3, line=0.5, cex=0.8, col="gray50")
  
  invisible(ord)
}

# Site posteriors plot with confidence intervals (base R)
plot_site_posteriors <- function(df_sites, main_title = "Bayesian Site Dates", sort_by = "mean") {
  # Input validation
  if (is.null(df_sites) || nrow(df_sites) == 0) {
    plot(1, 1, type="n", xlab="", ylab="", main="No posterior data available")
    text(1, 1, "Run Stan alignment first", cex=1.2, col="red")
    return(invisible())
  }

  # Check required columns
  required_cols <- c("site_id", "calBP_mean", "calBP_q025", "calBP_q975")
  missing_cols <- required_cols[!required_cols %in% names(df_sites)]
  if (length(missing_cols) > 0) {
    stop(sprintf("Missing columns in site posteriors: %s", paste(missing_cols, collapse=", ")))
  }

  # Sort by selected criterion
  if (sort_by == "mean" && "calBP_mean" %in% names(df_sites)) {
    ord <- df_sites[order(df_sites$calBP_mean, decreasing = TRUE), ]
  } else if (sort_by == "chronological") {
    ord <- df_sites[order(df_sites$calBP_mean, decreasing = FALSE), ]
  } else if (sort_by == "uncertainty" && "calBP_sd" %in% names(df_sites)) {
    ord <- df_sites[order(df_sites$calBP_sd, decreasing = TRUE), ]
  } else {
    # Fallback: alphabetical
    ord <- df_sites[order(df_sites$site_id), ]
  }
  
  n_sites <- nrow(ord)
  
  # Adjust plot margins for site labels
  old_par <- par(mar=c(5, max(7, max(nchar(ord$site_id))*0.6), 4, 2))
  on.exit(par(old_par))
  
  # Main plot: Means with confidence intervals
  plot(ord$calBP_mean, seq_len(n_sites),
       xlab="Calibrated Years BP", 
       ylab="", 
       yaxt="n",
       main=main_title,
       pch=16, 
       cex=1.2, 
       col="#2980b9",
       xlim=range(c(ord$calBP_q025, ord$calBP_q975), na.rm=TRUE) * c(0.98, 1.02))
  
  # Site labels on y-axis
  axis(2, at=seq_len(n_sites), labels=ord$site_id, las=1, cex.axis=0.8)
  
  # Confidence intervals (95% CrI)
  segments(ord$calBP_q025, seq_len(n_sites), 
           ord$calBP_q975, seq_len(n_sites),
           col="#e74c3c", lwd=2)
  
  # Interval endpoints
  segments(ord$calBP_q025, seq_len(n_sites) - 0.1, 
           ord$calBP_q025, seq_len(n_sites) + 0.1,
           col="#e74c3c", lwd=2)
  segments(ord$calBP_q975, seq_len(n_sites) - 0.1, 
           ord$calBP_q975, seq_len(n_sites) + 0.1,
           col="#e74c3c", lwd=2)
  
  # Standard deviation as point size (if available)
  if ("calBP_sd" %in% names(ord)) {
    # Overlay means with SD-proportional size
    # Simple rescaling function (instead of scales::rescale)
    sd_range <- range(ord$calBP_sd, na.rm=TRUE)
    if (diff(sd_range) > 0) {
      sd_sizes <- 0.8 + (ord$calBP_sd - sd_range[1]) / diff(sd_range) * (2.0 - 0.8)
    } else {
      sd_sizes <- rep(1.2, nrow(ord))
    }
    
    points(ord$calBP_mean, seq_len(n_sites), 
           pch=21, 
           cex=sd_sizes, 
           bg="white",
           col="#2980b9", 
           lwd=1.5)
  }
  
  # Add grid
  grid(col="lightgray", lty=2, lwd=0.5)

  # Statistics info
  age_range <- range(ord$calBP_mean, na.rm=TRUE)
  stats_text <- sprintf("Sites: %d | Time span: %.0f - %.0f BP | Δ: %.0f years",
                       n_sites, age_range[2], age_range[1], diff(age_range))
  mtext(stats_text, side=3, line=0.5, cex=0.8, col="gray50")

  # Add legend
  legend("topright", 
         legend=c("Posterior Mean", "95% CrI"), 
         pch=c(16, NA), 
         lty=c(NA, 1),
         col=c("#2980b9", "#e74c3c"),
         lwd=c(NA, 2),
         pt.cex=1.2,
         cex=0.9,
         bg="white")
  
  invisible(ord)
}

# Plotly version of chronology curve (interactive)
plotly_chrono_curve <- function(chrono_df, method_info = NULL) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package required for interactive plots")
  }

  if (is.null(chrono_df) || nrow(chrono_df) == 0) {
    return(plotly::plot_ly() %>%
           plotly::add_text(x=0.5, y=0.5, text="No chronology data",
                          showlegend=FALSE) %>%
           plotly::layout(title="Chronology curve not available"))
  }

  ord <- chrono_df[order(chrono_df$t_idx), ]

  # Hover text for sites
  hover_text <- sprintf(
    "<b>%s</b><br>Position: %d<br>Time index: %.3f<br>Relative time: %s",
    ord$site_id,
    seq_len(nrow(ord)),
    ord$t_idx,
    ifelse(ord$t_idx > mean(ord$t_idx, na.rm=TRUE), "↑ Later", "↓ Earlier")
  )

  # Plotly curve
  p <- plotly::plot_ly(data = ord, type = 'scatter', mode = 'lines+markers') %>%
    plotly::add_trace(
      x = seq_len(nrow(ord)),
      y = ~t_idx,
      mode = 'lines',
      line = list(color = '#2980b9', width = 3),
      name = 'Time Index Curve',
      hoverinfo = 'none'
    ) %>%
    plotly::add_trace(
      x = seq_len(nrow(ord)),
      y = ~t_idx,
      mode = 'markers',
      marker = list(color = '#e74c3c', size = 8),
      name = 'Sites',
      text = hover_text,
      hoverinfo = 'text'
    )

  # Layout with method info
  title_text <- if (!is.null(method_info)) {
    paste("Chronology Curve -", method_info)
  } else {
    "Chronology Curve (LOESS over CA coordinates)"
  }
  
  p %>%
    plotly::layout(
      title = title_text,
      xaxis = list(title = "CA Order (Sites)"),
      yaxis = list(title = "Time Index (smoothed)"),
      hovermode = 'closest'
    ) %>%
    standard_plotly_config("2d")
}

# Plotly version of site posteriors (interactive)
plotly_site_posteriors <- function(df_sites, sort_by = "mean") {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("plotly package required for interactive plots")
  }

  if (is.null(df_sites) || nrow(df_sites) == 0) {
    return(plotly::plot_ly() %>%
           plotly::add_text(x=0.5, y=0.5, text="No posterior data",
                          showlegend=FALSE) %>%
           plotly::layout(title="Site posteriors not available"))
  }

  # Sorting as in base R version
  if (sort_by == "mean") {
    ord <- df_sites[order(df_sites$calBP_mean, decreasing = TRUE), ]
  } else if (sort_by == "chronological") {
    ord <- df_sites[order(df_sites$calBP_mean, decreasing = FALSE), ]
  } else {
    ord <- df_sites[order(df_sites$site_id), ]
  }

  # Hover text
  hover_text <- sprintf(
    "<b>%s</b><br>Posterior Mean: %.0f BP<br>95%% CrI: %.0f - %.0f BP<br>SD: %.1f years<br>Span: %.0f years",
    ord$site_id,
    ord$calBP_mean,
    ord$calBP_q025, ord$calBP_q975,
    if("calBP_sd" %in% names(ord)) ord$calBP_sd else 0,
    ord$calBP_q975 - ord$calBP_q025
  )

  # Plotly plot
  p <- plotly::plot_ly(data = ord, type = 'scatter') %>%
    # Confidence intervals
    plotly::add_segments(
      x = ~calBP_q025, xend = ~calBP_q975,
      y = seq_len(nrow(ord)), yend = seq_len(nrow(ord)),
      line = list(color = '#e74c3c', width = 3),
      name = '95% CrI',
      hoverinfo = 'none'
    ) %>%
    # Posterior Means
    plotly::add_markers(
      x = ~calBP_mean,
      y = seq_len(nrow(ord)),
      marker = list(color = '#2980b9', size = 10),
      name = 'Posterior Mean',
      text = hover_text,
      hoverinfo = 'text'
    )

  p %>%
    plotly::layout(
      title = "Bayesian Site Dates (Stan MCMC)",
      xaxis = list(title = "Calibrated Years BP"),
      yaxis = list(
        title = "Sites",
        tickvals = seq_len(nrow(ord)),
        ticktext = ord$site_id
      ),
      hovermode = 'closest'
    ) %>%
    standard_plotly_config("2d")
}

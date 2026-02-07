# mod_correspondence_analysis.R

mod_correspondence_analysis_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL  # UI is defined in ui.R
}

mod_correspondence_analysis_server <- function(filtered_data, meta_data, cache, get_site_group, get_element_details = NULL, c14_calibrated_reactive = NULL, oxcal_results_reactive = NULL, input, output, session, tr = function(x) x) {
  
  # Namespace for module (important for dynamic UI elements!)
  ns <- session$ns
  
  source("helpers/plot_components.R", local = TRUE)
  source("helpers/download_components.R", local = TRUE)
  source("helpers/helper_robust_outliers.R", local = TRUE)

  # ===== HELPER FUNCTIONS FOR 14C OVERLAY =====

  # Aggregate OxCal posteriors for CA plot overlay
  # Uses the modelled posteriors from OxCal R_Date entries
  .aggregate_oxcal_for_ca <- function(tidy_df) {
    if (is.null(tidy_df) || nrow(tidy_df) == 0) return(NULL)

    # DEBUG: Show what we have
    message(sprintf("[.aggregate_oxcal_for_ca] tidy_df has %d rows", nrow(tidy_df)))
    if ("type" %in% names(tidy_df)) {
      message(sprintf("[.aggregate_oxcal_for_ca] Types: %s", paste(unique(tidy_df$type), collapse = ", ")))
    }
    message(sprintf("[.aggregate_oxcal_for_ca] Sample names: %s", paste(head(unique(tidy_df$name), 5), collapse = ", ")))

    # Filter to R_Date entries by TYPE column (not by name pattern)
    if ("type" %in% names(tidy_df)) {
      dates_df <- tidy_df[tidy_df$type == "R_Date", ]
      message(sprintf("[.aggregate_oxcal_for_ca] After type=='R_Date' filter: %d rows", nrow(dates_df)))
    } else {
      # Fallback: try to match R_Date in name (old behavior)
      dates_df <- tidy_df[grepl("R_Date", tidy_df$name, ignore.case = TRUE), ]
      message(sprintf("[.aggregate_oxcal_for_ca] Fallback name filter: %d rows", nrow(dates_df)))
    }

    if (nrow(dates_df) == 0) return(NULL)

    # Extract site name from OxCal name
    # Format is typically "Site_LabNr" (e.g., "60 Blankenburg_KIA-12345")
    dates_df$Site <- sapply(dates_df$name, function(n) {
      # Remove LabID suffix (pattern: _XXX-NNNNN or _XXX+NNNNN or just _NNNNN)
      cleaned <- trimws(n)
      if (grepl("_[A-Za-z]+-?\\d+$", cleaned) || grepl("_[A-Za-z]+\\d+$", cleaned)) {
        return(trimws(sub("_[A-Za-z]+-?\\d+$", "", cleaned)))
      }
      if (grepl("_\\d+$", cleaned)) {
        return(trimws(sub("_\\d+$", "", cleaned)))
      }
      cleaned
    })

    # Extract LabID from name
    dates_df$LabID <- sapply(dates_df$name, function(n) {
      cleaned <- trimws(n)
      if (grepl("_([A-Za-z]+-?\\d+)$", cleaned)) {
        match <- regmatches(cleaned, regexpr("_([A-Za-z]+-?\\d+)$", cleaned))
        if (length(match) > 0) return(gsub("^_", "", match))
      }
      if (grepl("_(\\d+)$", cleaned)) {
        match <- regmatches(cleaned, regexpr("_\\d+$", cleaned))
        if (length(match) > 0) return(gsub("^_", "", match))
      }
      ""
    })

    message(sprintf("[.aggregate_oxcal_for_ca] Extracted sites: %s", paste(head(unique(dates_df$Site), 5), collapse = ", ")))

    # Aggregate by site - use modelled posteriors
    site_summary <- dates_df %>%
      dplyr::group_by(Site) %>%
      dplyr::summarise(
        site_median_calBP = median(median, na.rm = TRUE),
        from_68 = min(from_68, na.rm = TRUE),
        to_68 = max(to_68, na.rm = TRUE),
        n_dates = dplyr::n(),
        lab_ids = paste(LabID[LabID != ""], collapse = ", "),
        .groups = "drop"
      )

    message(sprintf("[.aggregate_oxcal_for_ca] Aggregated to %d sites", nrow(site_summary)))
    site_summary
  }

  # Format label for modelled (OxCal) data with checkmark marker
  .format_modelled_c14_label <- function(median_bp, from_68, to_68, n_dates,
                                          show_n = TRUE, show_interval = FALSE,
                                          style = "compact", lab_ids = NULL,
                                          show_labids = FALSE) {
    # Convert to cal BC (assuming input is cal BP)
    median_calbc <- round(1950 - median_bp)
    from_calbc <- if (!is.infinite(from_68)) round(1950 - from_68) else NA
    to_calbc <- if (!is.infinite(to_68)) round(1950 - to_68) else NA

    # Modelled marker (checkmark)
    marker <- "\u2713"

    # Build label based on style
    if (style == "compact") {
      label <- paste0(marker, " ", median_calbc, " cal BC")
      if (show_n && !is.na(n_dates)) label <- paste0(label, " (n=", n_dates, ")")
      if (show_interval && !is.na(from_calbc) && !is.na(to_calbc)) {
        label <- paste0(label, "\n", to_calbc, "-", from_calbc)
      }
    } else {
      label <- paste0(marker, " ", median_calbc)
      if (show_n && !is.na(n_dates)) label <- paste0(label, " (n=", n_dates, ")")
    }

    if (show_labids && !is.null(lab_ids) && !is.na(lab_ids) && nchar(lab_ids) > 0) {
      label <- paste0(label, "\n", lab_ids)
    }

    label
  }

  # ===== HELPER FUNCTIONS =====
  
  # Matrix transformation to counter mega-sites/types
  apply_matrix_transformation <- function(mat, type_transform, site_transform) {
    mat_transformed <- mat
    transform_info <- c()
    
    # Transform types (columns) - normalize to proportions
    if (type_transform == "normalize") {
      col_sums <- colSums(mat_transformed)
      col_sums[col_sums == 0] <- 1
      mat_transformed <- sweep(mat_transformed, 2, col_sums, "/")
      transform_info <- c(transform_info, tr("ca.types.normalized"))
    } else if (type_transform == "log") {
      mat_transformed <- log1p(mat_transformed)
      transform_info <- c(transform_info, tr("ca.types.log"))
    }

    # Transform sites (rows) - normalize to proportions
    if (site_transform == "normalize") {
      row_sums <- rowSums(mat_transformed)
      row_sums[row_sums == 0] <- 1
      mat_transformed <- sweep(mat_transformed, 1, row_sums, "/")
      transform_info <- c(transform_info, tr("ca.sites.normalized"))
    } else if (site_transform == "log") {
      mat_transformed <- log1p(mat_transformed)
      transform_info <- c(transform_info, tr("ca.sites.log"))
    }
    
    list(matrix = mat_transformed, info = transform_info)
  }
  
  # Identify supplementary elements (with optional additional outlier exclusions)
  identify_supplementary <- function(mat, meta_data, additional_outliers = NULL) {
    req(meta_data$data)
    
    available_sites <- rownames(mat)
    available_types <- colnames(mat)
    
    all_sites <- meta_data$data$sites[meta_data$data$sites$Selected & 
                                        meta_data$data$sites$Entity %in% available_sites, ]
    all_types <- meta_data$data$types[meta_data$data$types$Selected & 
                                        meta_data$data$types$Entity %in% available_types, ]
    
    suppl_row_indices <- NULL
    suppl_col_indices <- NULL
    remove_row_indices <- NULL  # NEW: Rows to be completely removed
    remove_col_indices <- NULL  # NEW: Columns to be completely removed
    
    # Normal supplementary from metadata
    if (any(all_sites$Supplementary)) {
      suppl_sites <- all_sites$Entity[all_sites$Supplementary]
      suppl_row_indices <- which(rownames(mat) %in% suppl_sites)
    }
    
    if (any(all_types$Supplementary)) {
      suppl_types <- all_types$Entity[all_types$Supplementary]
      suppl_col_indices <- which(colnames(mat) %in% suppl_types)
    }
    
    if (!is.null(additional_outliers)) {
      if (!is.null(additional_outliers$sites) && length(additional_outliers$sites) > 0) {
        add_site_indices <- which(rownames(mat) %in% additional_outliers$sites)
        suppl_row_indices <- unique(c(suppl_row_indices, add_site_indices))
      }
      
      if (!is.null(additional_outliers$types) && length(additional_outliers$types) > 0) {
        add_type_indices <- which(colnames(mat) %in% additional_outliers$types)
        suppl_col_indices <- unique(c(suppl_col_indices, add_type_indices))
      }
    }
    
    # CRITICAL: Check whether supplementary rows only have values in supplementary columns
    # These must be COMPLETELY removed, not marked as supplementary!
    if (!is.null(suppl_row_indices) && length(suppl_row_indices) > 0) {
      active_cols <- setdiff(1:ncol(mat), suppl_col_indices)
      
      for (row_idx in suppl_row_indices) {
        if (length(active_cols) > 0) {
          row_sum_in_active_cols <- sum(mat[row_idx, active_cols], na.rm = TRUE)
          
          if (abs(row_sum_in_active_cols) < 1e-10) {
            # This row has no values in active columns -> remove completely
            remove_row_indices <- c(remove_row_indices, row_idx)
          }
        }
      }
    }
    
    # CRITICAL: Check whether supplementary columns only have values in supplementary rows
    if (!is.null(suppl_col_indices) && length(suppl_col_indices) > 0) {
      active_rows <- setdiff(1:nrow(mat), suppl_row_indices)
      
      for (col_idx in suppl_col_indices) {
        if (length(active_rows) > 0) {
          col_sum_in_active_rows <- sum(mat[active_rows, col_idx], na.rm = TRUE)
          
          if (abs(col_sum_in_active_rows) < 1e-10) {
            # This column has no values in active rows -> remove completely
            remove_col_indices <- c(remove_col_indices, col_idx)
          }
        }
      }
    }
    
    # Remove the problematic elements from the supplementary lists
    if (!is.null(remove_row_indices)) {
      suppl_row_indices <- setdiff(suppl_row_indices, remove_row_indices)
    }
    if (!is.null(remove_col_indices)) {
      suppl_col_indices <- setdiff(suppl_col_indices, remove_col_indices)
    }
    
    # CRITICAL: Check AGAIN on the simulated active submatrix
    # whether zero sums still exist after removal of all supplementary
    if (!is.null(suppl_row_indices) || !is.null(suppl_col_indices)) {
      # Simulate active matrix (without supplementary)
      active_rows <- setdiff(1:nrow(mat), suppl_row_indices)
      active_cols <- setdiff(1:ncol(mat), suppl_col_indices)
      
      if (length(active_rows) > 0 && length(active_cols) > 0) {
        active_mat <- mat[active_rows, active_cols, drop = FALSE]
        
        # Check rows in active matrix
        active_row_sums <- rowSums(active_mat, na.rm = TRUE)
        zero_active_rows <- which(abs(active_row_sums) < 1e-10)
        
        if (length(zero_active_rows) > 0) {
          # These rows of the ACTIVE matrix correspond to specific rows of the original matrix
          original_row_indices <- active_rows[zero_active_rows]
          remove_row_indices <- c(remove_row_indices, original_row_indices)
          
        }
        
        # Check columns in active matrix
        active_col_sums <- colSums(active_mat, na.rm = TRUE)
        zero_active_cols <- which(abs(active_col_sums) < 1e-10)
        
        if (length(zero_active_cols) > 0) {
          original_col_indices <- active_cols[zero_active_cols]
          remove_col_indices <- c(remove_col_indices, original_col_indices)
          
        }
      }
      
      # Remove newly found items from supplementary lists
      if (!is.null(remove_row_indices)) {
        suppl_row_indices <- setdiff(suppl_row_indices, remove_row_indices)
      }
      if (!is.null(remove_col_indices)) {
        suppl_col_indices <- setdiff(suppl_col_indices, remove_col_indices)
      }
    }
    
    if (length(suppl_row_indices) == 0) suppl_row_indices <- NULL
    if (length(suppl_col_indices) == 0) suppl_col_indices <- NULL
    
    list(
      row_indices = suppl_row_indices,
      col_indices = suppl_col_indices,
      remove_rows = remove_row_indices,  # NEW
      remove_cols = remove_col_indices,  # NEW
      active_rows = nrow(mat) - length(suppl_row_indices %||% integer(0)) - length(remove_row_indices %||% integer(0)),
      active_cols = ncol(mat) - length(suppl_col_indices %||% integer(0)) - length(remove_col_indices %||% integer(0))
    )
  }
  
  # Generate plot data for CA with biplot scaling (Greenacre 2007)
  generate_ca_plot_data <- function(res, x_idx, y_idx, focus, biplot_type = "symmetric") {
    # Validation
    validate(
      need(!is.null(res), "CA result missing"),
      need(!is.null(focus) && focus != "", "Analysis focus not defined"),
      need(!is.null(biplot_type) && biplot_type != "", "Biplot type not defined")
    )
    
    plot_data <- data.frame()
    
    # FactoMineR structure: row$coord, col$coord, row.sup$coord, col.sup$coord
    # Apply biplot scaling according to Greenacre (2007)
    
    # Extract coordinates based on biplot type
    if (biplot_type == "rowprincipal") {
      # Asymmetric: Site-focused (types shrunk)
      # Standard coordinates for sites, scaled coordinates for types
      row_coords <- if(!is.null(res$row$coord)) res$row$coord else NULL
      col_coords <- if(!is.null(res$col$coord)) {
        # Skalierung: multiply by sqrt of eigenvalues
        eig_sqrt <- sqrt(res$eig[1:ncol(res$col$coord), 1])
        sweep(res$col$coord, 2, eig_sqrt, "*")
      } else NULL
      row_coords_sup <- if(!is.null(res$row.sup$coord)) res$row.sup$coord else NULL
      col_coords_sup <- if(!is.null(res$col.sup$coord)) {
        eig_sqrt <- sqrt(res$eig[1:ncol(res$col.sup$coord), 1])
        sweep(res$col.sup$coord, 2, eig_sqrt, "*")
      } else NULL
    } else if (biplot_type == "colprincipal") {
      # Asymmetric: Type-focused (sites shrunk)
      row_coords <- if(!is.null(res$row$coord)) {
        eig_sqrt <- sqrt(res$eig[1:ncol(res$row$coord), 1])
        sweep(res$row$coord, 2, eig_sqrt, "*")
      } else NULL
      col_coords <- if(!is.null(res$col$coord)) res$col$coord else NULL
      row_coords_sup <- if(!is.null(res$row.sup$coord)) {
        eig_sqrt <- sqrt(res$eig[1:ncol(res$row.sup$coord), 1])
        sweep(res$row.sup$coord, 2, eig_sqrt, "*")
      } else NULL
      col_coords_sup <- if(!is.null(res$col.sup$coord)) res$col.sup$coord else NULL
    } else {
      # Symmetric (default)
      row_coords <- if(!is.null(res$row$coord)) res$row$coord else NULL
      col_coords <- if(!is.null(res$col$coord)) res$col$coord else NULL
      row_coords_sup <- if(!is.null(res$row.sup$coord)) res$row.sup$coord else NULL
      col_coords_sup <- if(!is.null(res$col.sup$coord)) res$col.sup$coord else NULL
    }
    
    # Add sites
    if (focus %in% c("both", "sites_only")) {
      # Active sites
      if (!is.null(row_coords) && ncol(row_coords) >= max(x_idx, y_idx)) {
        coords_subset <- row_coords[, c(x_idx, y_idx), drop = FALSE]
        
        # Extract contributions (from FactoMineR CA result)
        contrib_x_vals <- if (!is.null(res$row$contrib) && ncol(res$row$contrib) >= x_idx) {
          res$row$contrib[, x_idx]
        } else rep(0, nrow(coords_subset))
        
        contrib_y_vals <- if (!is.null(res$row$contrib) && ncol(res$row$contrib) >= y_idx) {
          res$row$contrib[, y_idx]
        } else rep(0, nrow(coords_subset))
        
        sites_data <- data.frame(
          x = coords_subset[, 1], y = coords_subset[, 2],
          label = rownames(coords_subset), type = 'Site', element_type = 'Active',
          contrib_x = contrib_x_vals, contrib_y = contrib_y_vals,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, sites_data)
      }
      
      # Supplementary sites (contributions = 0, not in calculation)
      if (!is.null(row_coords_sup) && ncol(row_coords_sup) >= max(x_idx, y_idx)) {
        coords_subset <- row_coords_sup[, c(x_idx, y_idx), drop = FALSE]
        suppl_row_data <- data.frame(
          x = coords_subset[, 1], y = coords_subset[, 2],
          label = rownames(coords_subset), type = 'Site', element_type = 'Supplementary',
          contrib_x = 0, contrib_y = 0,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, suppl_row_data)
      }
    }
    
    # Add types
    if (focus %in% c("both", "types_only")) {
      # Active types
      if (!is.null(col_coords) && ncol(col_coords) >= max(x_idx, y_idx)) {
        coords_subset <- col_coords[, c(x_idx, y_idx), drop = FALSE]
        
        # Extract contributions (from FactoMineR CA result)
        contrib_x_vals <- if (!is.null(res$col$contrib) && ncol(res$col$contrib) >= x_idx) {
          res$col$contrib[, x_idx]
        } else rep(0, nrow(coords_subset))
        
        contrib_y_vals <- if (!is.null(res$col$contrib) && ncol(res$col$contrib) >= y_idx) {
          res$col$contrib[, y_idx]
        } else rep(0, nrow(coords_subset))
        
        types_data <- data.frame(
          x = coords_subset[, 1], y = coords_subset[, 2],
          label = rownames(coords_subset), type = 'Type', element_type = 'Active',
          contrib_x = contrib_x_vals, contrib_y = contrib_y_vals,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, types_data)
      }
      
      # Supplementary types (contributions = 0, not in calculation)
      if (!is.null(col_coords_sup) && ncol(col_coords_sup) >= max(x_idx, y_idx)) {
        coords_subset <- col_coords_sup[, c(x_idx, y_idx), drop = FALSE]
        suppl_col_data <- data.frame(
          x = coords_subset[, 1], y = coords_subset[, 2],
          label = rownames(coords_subset), type = 'Type', element_type = 'Supplementary',
          contrib_x = 0, contrib_y = 0,
          stringsAsFactors = FALSE
        )
        plot_data <- rbind(plot_data, suppl_col_data)
      }
    }
    
    plot_data
  }
  
  # ===== JACKKNIFE-INFLUENCE CALCULATION (PROCRUSTES) =====
  # Calculates Leave-One-Out influence for sites/types
  # Uses Procrustes alignment to correct axis rotation/flip
  calculate_jackknife_influence <- function(mat, meta_data, threshold = 0.5, n_dims = 2) {
    tryCatch({
      if (!requireNamespace("vegan", quietly = TRUE)) {
        stop("Package 'vegan' is required for Jackknife analysis. Please install: install.packages('vegan')")
      }
      
      n <- nrow(mat)
      m <- ncol(mat)
      
      # Full CA
      suppl_info <- identify_supplementary(mat, meta_data, NULL)
      full_ca <- FactoMineR::CA(
        mat,
        row.sup = suppl_info$row_indices,
        col.sup = suppl_info$col_indices,
        graph = FALSE
      )
      
      # Check if enough dimensions are available
      max_dims <- ncol(full_ca$row$coord)
      n_dims_use <- min(n_dims, max_dims)
      
      
      # Extract k-dimensional scores (default: Dim1+Dim2)
      full_scores <- full_ca$row$coord[, 1:n_dims_use, drop = FALSE]
      
      # Leave-One-Out for sites
      site_influences <- numeric(n)
      names(site_influences) <- rownames(mat)
      
      for (i in 1:n) {
        # LOO (Leave-One-Out) CA
        loo_mat <- mat[-i, ]
        loo_suppl <- identify_supplementary(loo_mat, meta_data, NULL)
        
        loo_ca <- FactoMineR::CA(
          loo_mat,
          row.sup = loo_suppl$row_indices,
          col.sup = loo_suppl$col_indices,
          graph = FALSE
        )
        loo_scores <- loo_ca$row$coord[, 1:n_dims_use, drop = FALSE]
        
        # Procrustes alignment: LOO on full (without site i)
        # symmetric=FALSE: Only LOO is transformed
        proc <- vegan::procrustes(
          X = full_scores[-i, ], 
          Y = loo_scores, 
          symmetric = FALSE,
          scale = FALSE  # Scaling not needed for CA
        )
        
        # Influence = square root of mean Procrustes distance
        site_influences[i] <- sqrt(mean(proc$ss))
      }
      
      # Leave-One-Out for types
      type_influences <- numeric(m)
      names(type_influences) <- colnames(mat)
      
      for (j in 1:m) {
        # LOO (Leave-One-Out) CA
        loo_mat <- mat[, -j]
        loo_suppl <- identify_supplementary(loo_mat, meta_data, NULL)
        
        loo_ca <- FactoMineR::CA(
          loo_mat,
          row.sup = loo_suppl$row_indices,
          col.sup = loo_suppl$col_indices,
          graph = FALSE
        )
        loo_scores <- loo_ca$row$coord[, 1:n_dims_use, drop = FALSE]
        
        # Procrustes alignment
        proc <- vegan::procrustes(
          X = full_scores,
          Y = loo_scores, 
          symmetric = FALSE,
          scale = FALSE
        )
        
        # Influence = square root of mean Procrustes distance
        type_influences[j] <- sqrt(mean(proc$ss))
      }
      
      # Adaptive thresholds: Median + 2xIQR (only extreme outliers)
      site_iqr <- IQR(site_influences, na.rm = TRUE)
      type_iqr <- IQR(type_influences, na.rm = TRUE)
      
      site_threshold_adaptive <- median(site_influences, na.rm = TRUE) + 2 * site_iqr
      type_threshold_adaptive <- median(type_influences, na.rm = TRUE) + 2 * type_iqr
      
      # Use adaptive threshold (more robust than fixed value)
      influential_sites <- names(site_influences[site_influences > site_threshold_adaptive])
      influential_types <- names(type_influences[type_influences > type_threshold_adaptive])
      
      
      list(
        site_influences = site_influences,
        type_influences = type_influences,
        influential_sites = influential_sites,
        influential_types = influential_types,
        threshold = threshold,
        n_dims = n_dims_use  # Actually used dimensions
      )
    }, error = function(e) {
      list(
        site_influences = NULL,
        type_influences = NULL,
        influential_sites = character(0),
        influential_types = character(0),
        error = as.character(e$message)
      )
    })
  }
  
  
  # ===== EXCLUDE OUTLIERS: BUTTON OBSERVER =====
  # STRUCTURE: excluded_outliers() is a list with $sites and $types
  excluded_outliers <- reactiveVal(list(sites = character(0), types = character(0)))
  
  observeEvent(input$ca_remove_outliers, {
    lookup <- outlier_lookup()
    
    if (length(lookup) == 0) {
      showNotification(tr("notify.ca.no.outliers"), type = "warning", duration = 3)
      return()
    }
    
    # Collect all selected checkboxes - SEPARATED by sites/types
    selected_sites <- character(0)
    selected_types <- character(0)

    for (cb_id in names(lookup)) {
      checkbox_value <- input[[cb_id]]

      if (!is.null(checkbox_value) && checkbox_value) {
        entity_value <- lookup[[cb_id]]

        if (is.list(entity_value)) {
          if (entity_value$type == "Site") {
            selected_sites <- c(selected_sites, entity_value$entity)
          } else {
            selected_types <- c(selected_types, entity_value$entity)
          }
        } else {
          # Robust outlier structure (simple string)
          req(ca_res())
          res <- ca_res()

          is_site <- (!is.null(res$row$coord) && entity_value %in% rownames(res$row$coord)) ||
                     (!is.null(res$row.sup$coord) && entity_value %in% rownames(res$row.sup$coord))

          if (is_site) {
            selected_sites <- c(selected_sites, entity_value)
          } else {
            selected_types <- c(selected_types, entity_value)
          }
        }
      }
    }

    n_selected <- length(selected_sites) + length(selected_types)
    
    if (n_selected == 0) {
      showNotification(tr("notify.ca.no.outliers"), type = "warning", duration = 3)
      return()
    }
    
    # Add to excluded (list structure!)
    current <- excluded_outliers()
    new_excluded <- list(
      sites = unique(c(current$sites, selected_sites)),
      types = unique(c(current$types, selected_types))
    )
    excluded_outliers(new_excluded)
    
    showNotification(
      sprintf(tr("notify.ca.outliers.excluded"), n_selected),
      type = "message",
      duration = 5
    )
    
  })
  
  observeEvent(input$ca_reset_outliers, {
    excluded_outliers(list(sites = character(0), types = character(0)))
    showNotification("All outliers included again!", type = "message", duration = 3)
  })
  
  observeEvent(input$ca_select_all_outliers, {
    lookup <- outlier_lookup()

    if (length(lookup) == 0) return()

    for (cb_id in names(lookup)) {
      tryCatch({
        updateCheckboxInput(session, cb_id, value = TRUE)
      }, error = function(e) NULL)
    }

    showNotification(
      sprintf(tr("notify.ca.outliers.selected"), length(lookup)),
      type = "message",
      duration = 2
    )
  })
  
  # Status display
  output$ca_excluded_outliers_status <- renderUI({
    excl <- excluded_outliers()
    
    # Check if exclusions exist (NEVER NULL, always list()!)
    if (length(excl$sites) == 0 && length(excl$types) == 0) {
      return(NULL)
    }
    
    n_sites <- length(excl$sites)
    n_types <- length(excl$types)
    n_total <- n_sites + n_types
    
    div(class="alert alert-success", style="font-size: 0.9em; margin-bottom: 10px;",
        tags$b(sprintf("%d outliers excluded", n_total)),
        br(),
        if (n_sites > 0) tags$span(sprintf("Sites: %d | ", n_sites)),
        if (n_types > 0) tags$span(sprintf("Types: %d", n_types)),
        br(),
        tags$small("Click 'Calculate CA' to recalculate CA without these elements.")
    )
  })
  
  # ===== CA CALCULATION =====

  # Reacts ONLY to button click (excluded_outliers is only READ, not used as trigger)
  ca_res <- eventReactive(input$refresh, {
    req(filtered_data())
    
    withProgress(message = 'Calculating correspondence analysis...', {
      incProgress(0.2, detail = "Preparing matrix...")
      
      tryCatch({
        mat <- filtered_data()
        validate(need(sum(mat, na.rm = TRUE) > 0, "Matrix must contain positive values!"))
        
        incProgress(0.3, detail = "Matrix transformation...")
        
        # Matrix transformation to counter mega-sites/types
        transform_result <- apply_matrix_transformation(
          mat,
          input$ca_transform_types %||% "none",
          input$ca_transform_sites %||% "none"
        )
        
        # CRITICAL: Remove rows/columns with sum 0 BEFORE identify_supplementary!
        # FactoMineR removes these automatically, which shifts indices
        mat_clean <- transform_result$matrix
        
        # Rows with sum approx. 0 (with tolerance for numerical errors)
        row_sums <- rowSums(mat_clean, na.rm = TRUE)
        zero_rows <- which(abs(row_sums) < 1e-10)
        
        if (length(zero_rows) > 0) {
          mat_clean <- mat_clean[-zero_rows, , drop = FALSE]
        }
        
        # Columns with sum approx. 0
        col_sums <- colSums(mat_clean, na.rm = TRUE)
        zero_cols <- which(abs(col_sums) < 1e-10)
        
        if (length(zero_cols) > 0) {
          mat_clean <- mat_clean[, -zero_cols, drop = FALSE]
        }
        
        incProgress(0.4, detail = tr("ca.progress.running"))
        
        # ITERATIVE supplementary identification and cleanup
        # Repeat as long as problematic elements are found
        max_iterations <- 10
        iteration <- 0
        
        repeat {
          iteration <- iteration + 1

          suppl_info <- identify_supplementary(mat_clean, meta_data, excluded_outliers())

          has_removals <- (!is.null(suppl_info$remove_rows) && length(suppl_info$remove_rows) > 0) ||
                         (!is.null(suppl_info$remove_cols) && length(suppl_info$remove_cols) > 0)

          if (!has_removals || iteration >= max_iterations) break

          if (!is.null(suppl_info$remove_rows) && length(suppl_info$remove_rows) > 0) {
            mat_clean <- mat_clean[-suppl_info$remove_rows, , drop = FALSE]
          }

          if (!is.null(suppl_info$remove_cols) && length(suppl_info$remove_cols) > 0) {
            mat_clean <- mat_clean[, -suppl_info$remove_cols, drop = FALSE]
          }
        }
        
        # Final supplementary identification
        suppl_info <- identify_supplementary(mat_clean, meta_data, excluded_outliers())

        suppl_info$active_rows <- nrow(mat_clean) - length(suppl_info$row_indices %||% integer(0))
        suppl_info$active_cols <- ncol(mat_clean) - length(suppl_info$col_indices %||% integer(0))
        
        # Validation AFTER cleanup
        validate(need(suppl_info$active_rows >= 2, tr("validate.ca.min.sites")))
        validate(need(suppl_info$active_cols >= 2, tr("validate.ca.min.types")))
        
        # Calculate correspondence analysis with FactoMineR (with cleaned matrix!)
        result <- FactoMineR::CA(
          mat_clean,
          row.sup = suppl_info$row_indices,
          col.sup = suppl_info$col_indices,
          graph = FALSE
        )
        # Determine biplot type (Greenacre 2007)
        biplot_type <- input$ca_biplot_type %||% "symmetric"
        
        result$biplot_type <- biplot_type
        
        cache$ca_result <- result
        
        # Scientifically founded notification
        transform_text <- if (length(transform_result$info) > 0) {
          paste(transform_result$info, collapse = " + ")
        } else tr("ca.transform.none.text")

        biplot_text <- switch(biplot_type,
                              "symmetric" = tr("ca.biplot.text.symmetric"),
                              "rowprincipal" = tr("ca.biplot.text.rowprincipal"),
                              "colprincipal" = tr("ca.biplot.text.colprincipal"),
                              tr("ca.biplot.text.symmetric")
        )

        showNotification(
          sprintf(tr("notify.ca.success"),
                  suppl_info$active_rows, suppl_info$active_cols, biplot_text, transform_text),
          type = "message", duration = 5
        )

        incProgress(1.0, detail = tr("ca.progress.complete"))
        result
        
      }, error = function(e) {
        showNotification(paste(tr("notify.ca.error"), e$message), type = "error", duration = 8)
        return(NULL)
      })
    })
  })
  
  # ===== UI COMPONENTS =====

  # Dimension selection
  output$ca_dim_select <- renderUI({
    req(ca_res())

    # ca package: dimensions from sv (singular values)
    n_dims <- length(ca_res()$sv)
    dims <- paste0("Dim", seq_len(n_dims))

    list(
      selectInput("x_dim",
                  div(tr("term.xaxis"), tags$span("\u2139\ufe0f", style="margin-left:5px; cursor:help;",
                                            title=tr("plot.ca.xaxis.tooltip"))),
                  choices = dims, selected = dims[1]),
      selectInput("y_dim",
                  div(tr("term.yaxis"), tags$span("\u2139\ufe0f", style="margin-left:5px; cursor:help;",
                                            title=tr("plot.ca.yaxis.tooltip"))),
                  choices = dims, selected = dims[min(2, length(dims))])
    )
  })
  
  # Quick CA statistics (without subjective evaluation)
  output$ca_quick_stats <- renderTable({
    req(ca_res())

    eig <- ca_res()$eig
    validate(need(!is.null(eig) && nrow(eig) > 0, tr("ca.eigen.no.data")))

    n_show <- min(4, nrow(eig))

    # Dynamic column names
    df <- data.frame(
      Dimension = paste0("Dim", 1:n_show),
      Varianz = paste0(round(as.numeric(eig[1:n_show, 2]), 1), "%"),
      Kumuliert = paste0(round(as.numeric(eig[1:n_show, 3]), 1), "%"),
      check.names = FALSE
    )
    names(df) <- c(tr("term.dimension"), tr("term.variance.pct"), tr("term.cumulative.pct"))
    df
  }, bordered = TRUE, striped = TRUE, hover = TRUE)
  
  # ===== DYNAMIC DIMENSION SELECTION FOR CONTRIBUTIONS =====
  output$ca_contrib_dim_selector <- renderUI({
    req(ca_res())
    
    # Get number of available dimensions (max. 3)
    n_dims <- min(nrow(ca_res()$eig), 3)
    
    dim_choices <- setNames(
      paste0("dim", 1:n_dims),
      paste("Dimension", 1:n_dims)
    )
    
    radioButtons("contrib_dimension", "Dimension:",
                 choices = dim_choices,
                 selected = "dim1", inline = TRUE)
  })
  
  # ===== CA PLOT DATA =====
  
  ca_plot_data <- reactive({
    req(ca_res(), input$x_dim, input$y_dim, input$ca_analysis_focus)

    # Explicit dependencies on 14C overlay options (triggers re-render when changed)
    show_c14 <- input$show_c14_overlay
    c14_show_n <- input$c14_show_n
    c14_show_interval <- input$c14_show_interval
    c14_label_style <- input$c14_label_style
    c14_show_labids <- input$c14_show_labids
    # Data source: "unmodelled" (default) or "modelled"
    c14_data_source <- if (is.null(input$c14_data_source)) "unmodelled" else input$c14_data_source

    res <- ca_res()
    x_idx <- as.numeric(gsub("Dim", "", input$x_dim))
    y_idx <- as.numeric(gsub("Dim", "", input$y_dim))
    biplot_type <- res$biplot_type %||% "symmetric"
    
    # Generate plot data with biplot scaling
    plot_data <- generate_ca_plot_data(res, x_idx, y_idx, input$ca_analysis_focus, biplot_type)
    
    if (nrow(plot_data) == 0) return(data.frame())
    
    # Group assignment for sites
    plot_data$group <- NA_character_
    is_site <- plot_data$type == 'Site'
    if (any(is_site)) {
      plot_data$group[is_site] <- get_site_group(plot_data$label[is_site])
    }
    
    # Generate labels
    if (!is.null(input$show_labels) && input$show_labels) {
      # Base labels (truncated)
      plot_data$lab <- substr(plot_data$label, 1, input$label_chars %||% 12)
      
      # Filter by "Only marked" if selected
      if (!is.null(input$label_selection) && input$label_selection == "selected") {
        req(meta_data, meta_data$data)
        
        # Identify sites/types with ShowLabel = TRUE
        labeled_sites <- character(0)
        labeled_types <- character(0)
        
        if (!is.null(meta_data$data$sites)) {
          labeled_sites <- meta_data$data$sites$Entity[!is.na(meta_data$data$sites$ShowLabel) & meta_data$data$sites$ShowLabel]
        }
        if (!is.null(meta_data$data$types)) {
          labeled_types <- meta_data$data$types$Entity[!is.na(meta_data$data$types$ShowLabel) & meta_data$data$types$ShowLabel]
        }
        
        for (i in 1:nrow(plot_data)) {
          is_labeled <- if (plot_data$type[i] == "Site") {
            plot_data$label[i] %in% labeled_sites
          } else {
            plot_data$label[i] %in% labeled_types
          }
          
          if (!is_labeled) {
            plot_data$lab[i] <- ""
          }
        }
      }
    } else {
      plot_data$lab <- ""
    }
    
    # Generate hover text (NEW helper integration)
    plot_data$hover_text <- generate_ca_hover_text(plot_data, input$x_dim, input$y_dim, get_element_details)
    
    # === 14C OVERLAY INTEGRATION ===
    # Extend hover text with 14C information if available
    # Check for EITHER unmodelled OR modelled data
    has_any_c14_data <- (!is.null(c14_calibrated_reactive) &&
                         tryCatch({ d <- c14_calibrated_reactive(); !is.null(d) && nrow(d) > 0 }, error = function(e) FALSE)) ||
                        (!is.null(oxcal_results_reactive) &&
                         tryCatch({ d <- oxcal_results_reactive(); !is.null(d) && !is.null(d$tidy) && nrow(d$tidy) > 0 }, error = function(e) FALSE))

    if (has_any_c14_data && isTRUE(input$show_c14_overlay)) {
      tryCatch({
        # Source helper functions
        source("modules/mod_c14_functions.R", local = TRUE)

        # Determine data source: "unmodelled" (individual calibration) or "modelled" (OxCal)
        # Use the c14_data_source variable captured above (with NULL handling)
        use_modelled <- c14_data_source == "modelled" && !is.null(oxcal_results_reactive)

        # DEBUG: Log data source selection
        message(sprintf("[14C Overlay] Data source: '%s', use_modelled: %s", c14_data_source, use_modelled))

        site_summary <- NULL
        is_modelled <- FALSE

        if (use_modelled) {
          # Try to get OxCal modelled data
          oxcal_data <- tryCatch(oxcal_results_reactive(), error = function(e) NULL)
          message(sprintf("[14C Overlay] OxCal data available: %s", !is.null(oxcal_data)))
          if (!is.null(oxcal_data) && !is.null(oxcal_data$tidy) && nrow(oxcal_data$tidy) > 0) {
            message(sprintf("[14C Overlay] OxCal tidy rows: %d", nrow(oxcal_data$tidy)))
            # Aggregate OxCal posteriors per site
            site_summary <- .aggregate_oxcal_for_ca(oxcal_data$tidy)
            is_modelled <- TRUE
          }
        }

        # Fallback to unmodelled data if modelled not available or not selected
        if (is.null(site_summary) && !is.null(c14_calibrated_reactive)) {
          c14_data <- tryCatch(c14_calibrated_reactive(), error = function(e) NULL)
          if (!is.null(c14_data) && nrow(c14_data) > 0) {
            site_summary <- summarise_c14_by_site(c14_data)
            is_modelled <- FALSE
          }
        }

        # Last resort: Try OxCal if unmodelled is selected but no unmodelled data
        if (is.null(site_summary) && !is.null(oxcal_results_reactive)) {
          oxcal_data <- tryCatch(oxcal_results_reactive(), error = function(e) NULL)
          if (!is.null(oxcal_data) && !is.null(oxcal_data$tidy) && nrow(oxcal_data$tidy) > 0) {
            site_summary <- .aggregate_oxcal_for_ca(oxcal_data$tidy)
            is_modelled <- TRUE
          }
        }

        if (!is.null(site_summary) && nrow(site_summary) > 0) {
          # Extend site data with 14C information
          site_indices <- which(plot_data$type == 'Site')
          for (i in site_indices) {
            site_name <- plot_data$label[i]
            c14_info <- site_summary[site_summary$Site == site_name, ]

            if (nrow(c14_info) > 0) {
              # Format 14C information using UI options
              show_n_val <- if (is.null(input$c14_show_n)) TRUE else input$c14_show_n
              show_interval_val <- if (is.null(input$c14_show_interval)) FALSE else input$c14_show_interval
              show_labids_val <- if (is.null(input$c14_show_labids)) FALSE else input$c14_show_labids
              style_val <- if (is.null(input$c14_label_style)) "compact" else input$c14_label_style

              # Get LabIDs if available
              lab_ids_val <- if ("lab_ids" %in% names(c14_info)) c14_info$lab_ids else NULL

              # Format label with modelled marker if using OxCal data
              if (is_modelled) {
                message(sprintf("[14C Overlay] Using MODELLED format for site: %s", site_name))
                c14_label <- .format_modelled_c14_label(
                  c14_info$site_median_calBP,
                  c14_info$from_68,
                  c14_info$to_68,
                  c14_info$n_dates,
                  show_n = show_n_val,
                  show_interval = show_interval_val,
                  style = style_val,
                  lab_ids = lab_ids_val,
                  show_labids = show_labids_val
                )
              } else {
                message(sprintf("[14C Overlay] Using UNMODELLED format for site: %s", site_name))
                c14_label <- format_compact_c14_label(
                  c14_info$site_median_calBP,
                  c14_info$hpd68_intervals[[1]],
                  c14_info$n_dates,
                  show_n = show_n_val,
                  show_interval = show_interval_val,
                  style = style_val,
                  lab_ids = lab_ids_val,
                  show_labids = show_labids_val
                )
              }

              # Build hover text with LabIDs always shown
              hover_c14_text <- c14_label
              if (!is.null(lab_ids_val) && !is.na(lab_ids_val) && nchar(as.character(lab_ids_val)) > 0) {
                hover_c14_text <- paste0(hover_c14_text, "\nLabIDs: ", lab_ids_val)
              }
              # Add source indicator to hover
              source_text <- if (is_modelled) "OxCal modelled" else "individually calibrated"

              # Extend hover text
              plot_data$hover_text[i] <- paste0(
                plot_data$hover_text[i],
                "\n\n📅 14C (", source_text, "):\n", hover_c14_text
              )

              plot_data$c14_label[i] <- c14_label
            } else {
              plot_data$c14_label[i] <- ""
            }
          }
        } else {
          plot_data$c14_label <- rep("", nrow(plot_data))
        }
      }, error = function(e) {
        # Silent error - 14C overlay is optional
        plot_data$c14_label <- rep("", nrow(plot_data))
      })
    } else {
      plot_data$c14_label <- rep("", nrow(plot_data))
    }
    
    plot_data
  })
  
  # ===== CA PLOTLY =====
  
  output$ca_plotly <- renderPlotly({
    req(ca_plot_data())

    # Explicit dependencies on 14C positioning options (triggers re-render when changed)
    c14_label_offset <- input$c14_label_offset
    c14_min_separation <- input$c14_min_separation
    c14_label_size <- input$c14_label_size
    # Data source: Trigger re-render when data source changes (with NULL handling)
    c14_data_source <- if (is.null(input$c14_data_source)) "unmodelled" else input$c14_data_source
    message(sprintf("[CA Plotly] Rendering with c14_data_source: '%s'", c14_data_source))

    calculate_confidence_ellipse <- function(x, y, confidence = 0.95, n_points = 100) {
      if (length(x) < 3) return(NULL)
      center_x <- mean(x, na.rm = TRUE)
      center_y <- mean(y, na.rm = TRUE)
      coords <- cbind(x, y)
      cov_mat <- cov(coords)
      eigen_decomp <- eigen(cov_mat)
      eigenvalues <- eigen_decomp$values
      eigenvectors <- eigen_decomp$vectors
      chi_sq <- qchisq(confidence, df = 2)
      a <- sqrt(eigenvalues[1] * chi_sq)
      b <- sqrt(eigenvalues[2] * chi_sq)
      angle <- atan2(eigenvectors[2, 1], eigenvectors[1, 1])
      theta <- seq(0, 2 * pi, length.out = n_points)
      ellipse_x <- center_x + a * cos(theta) * cos(angle) - b * sin(theta) * sin(angle)
      ellipse_y <- center_y + a * cos(theta) * sin(angle) + b * sin(theta) * cos(angle)
      list(x = ellipse_x, y = ellipse_y)
    }
    
    plot_data <- ca_plot_data()
    
    # Color grouping
    plot_data$color_group <- if (isTRUE(input$ca_color_by_group)) {
      ifelse(is.na(plot_data$group), paste(plot_data$type, plot_data$element_type), paste0("G:", plot_data$group))
    } else {
      paste(plot_data$type, plot_data$element_type)
    }
    
    # Scientifically founded coloring (NEW helper integration)
    seri_colors <- seri_arc_colors()
    colors <- if (isTRUE(input$ca_color_by_group)) {
      cc <- seri_arc_factor_colors(factor(plot_data$color_group))
      unname(cc$cols)
    } else {
      c("Site Active" = seri_colors$site_active, "Type Active" = seri_colors$type_active,
        "Site Supplementary" = seri_colors$site_supplementary, "Type Supplementary" = seri_colors$type_supplementary)
    }
    
    # Base plot with scientific axis titles (NEW helper integration)
    x_title <- ca_axis_title(input$x_dim %||% "Dim1", if(!is.null(ca_res())) ca_res()$eig else NULL, as.numeric(gsub("Dim", "", input$x_dim %||% "1")))
    y_title <- ca_axis_title(input$y_dim %||% "Dim2", if(!is.null(ca_res())) ca_res()$eig else NULL, as.numeric(gsub("Dim", "", input$y_dim %||% "2")))
    
    p <- plot_ly() %>%
      layout(ca_plotly_layout(
        x_title = x_title,
        y_title = y_title,
        plot_title = tr("plot.ca.title"),
        subtitle = tr("plot.ca.subtitle")
      ))
    
    # ===== LABEL SETTINGS =====
    label_type <- input$label_type %||% "both"
    show_site_labels <- label_type %in% c("both", "sites")
    show_type_labels <- label_type %in% c("both", "types")
    label_size <- input$label_size %||% 12
    label_colored <- isTRUE(input$label_colored)
    
    # ===== LEGEND LOGIC: Only from real traces =====
    # Separate data by element type
    active_data <- plot_data[plot_data$element_type == 'Active', ]
    suppl_data <- plot_data[plot_data$element_type == 'Supplementary', ]
    
    # Check which combinations actually occur
    has_site_active <- nrow(active_data[active_data$type == 'Site', ]) > 0
    has_type_active <- nrow(active_data[active_data$type == 'Type', ]) > 0
    has_site_suppl <- nrow(suppl_data[suppl_data$type == 'Site', ]) > 0
    has_type_suppl <- nrow(suppl_data[suppl_data$type == 'Type', ]) > 0
    
    # Add active points
    if (nrow(active_data) > 0) {
      # Sites (circles)
      site_subset <- active_data[active_data$type == 'Site', ]
      if (nrow(site_subset) > 0) {
        site_color <- if (isTRUE(input$ca_color_by_group)) {
          sapply(site_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else "#2980b9"
          })
        } else rep(colors["Site Active"], nrow(site_subset))
        
        # Labels directly integrated (like DCA)
        site_labels <- if (show_site_labels && input$show_labels) site_subset$lab else ""
        site_label_color <- if (label_colored) site_color else "#2c3e50"
        
        p <- p %>% add_markers(
          x = site_subset$x, y = site_subset$y,
          marker = ca_site_marker(site_color, (input$point_size %||% 3) * 4, element_type = "Active"),
          text = site_labels, textposition = "middle right",
          textfont = list(size = label_size, color = site_label_color),
          hovertext = site_subset$hover_text, hoverinfo = 'text',
          name = tr("plot.ca.legend.sites"),
          showlegend = TRUE,
          legendgroup = "sites",
          legendrank = 1
        )
        
        # Add 14C labels with leader lines and smart distribution
        if (!is.null(c14_calibrated_reactive) && isTRUE(input$show_c14_overlay)) {
          c14_sites <- site_subset[site_subset$c14_label != "", ]
          if (nrow(c14_sites) > 0) {
            # Smart label positioning - use values from overlay module UI
            label_positions <- calculate_smart_label_positions(
              c14_sites$x, c14_sites$y,
              offset_distance = input$c14_label_offset %||% 0.4,
              min_separation = input$c14_min_separation %||% 0.2
            )
            
            # For each 14C site: label with leader line
            for (idx in 1:nrow(c14_sites)) {
              site_x <- c14_sites$x[idx]
              site_y <- c14_sites$y[idx]
              label_x <- label_positions$x[idx]
              label_y <- label_positions$y[idx]
              
              # Add leader line (solid line)
              p <- p %>% add_segments(
                x = site_x, y = site_y,
                xend = label_x, yend = label_y,
                line = list(color = "#34495e", width = 1.5),  # Durchgezogene Linie
                showlegend = FALSE,
                hoverinfo = "skip"
              )
              
              # Add 14C label with configurable size
              c14_font_size <- if (is.null(input$c14_label_size)) 9 else input$c14_label_size
              p <- p %>% add_annotations(
                x = label_x, y = label_y,
                text = c14_sites$c14_label[idx],
                font = list(
                  size = c14_font_size,
                  color = "#2c3e50",
                  family = "Arial, sans-serif"
                ),
                # White background with light border
                bgcolor = "rgba(255, 255, 255, 0.85)",
                bordercolor = "#bdc3c7",
                borderwidth = 1,
                borderpad = 3,
                showarrow = FALSE,
                xanchor = "center",
                yanchor = "middle"
              )
            }
          }
        }
      }
      
      # Types (triangles)
      type_subset <- active_data[active_data$type == 'Type', ]
      if (nrow(type_subset) > 0) {
        type_color <- if (isTRUE(input$ca_color_by_group)) {
          sapply(type_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else "#c0392b"
          })
        } else rep(colors["Type Active"], nrow(type_subset))
        
        # Labels directly integrated (like DCA)
        type_labels <- if (show_type_labels && input$show_labels) type_subset$lab else ""
        type_label_color <- if (label_colored) type_color else "#2c3e50"
        
        p <- p %>% add_markers(
          x = type_subset$x, y = type_subset$y,
          marker = ca_type_marker(type_color, (input$point_size %||% 3) * 4, element_type = "Active"),
          text = type_labels, textposition = "middle right",
          textfont = list(size = label_size, color = type_label_color),
          hovertext = type_subset$hover_text, hoverinfo = 'text',
          name = tr("plot.ca.legend.types"),
          showlegend = TRUE,
          legendgroup = "types",
          legendrank = 2
        )
      }
    }
    
    # Add supplementary points (open symbols)
    if (nrow(suppl_data) > 0) {
      # Supplementary Sites
      site_subset <- suppl_data[suppl_data$type == 'Site', ]
      if (nrow(site_subset) > 0) {
        site_color <- if (isTRUE(input$ca_color_by_group)) {
          sapply(site_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else "#7fb3d3"
          })
        } else rep(colors["Site Supplementary"], nrow(site_subset))
        
        # Labels directly integrated (like DCA)
        site_labels <- if (show_site_labels && input$show_labels) site_subset$lab else ""
        site_label_color <- if (label_colored) site_color else "#2c3e50"
        
        p <- p %>% add_markers(
          x = site_subset$x, y = site_subset$y,
          marker = ca_site_marker(site_color, (input$point_size %||% 3) * 4, element_type = "Supplementary"),
          text = site_labels, textposition = "middle right",
          textfont = list(size = label_size, color = site_label_color),
          hovertext = site_subset$hover_text, hoverinfo = 'text',
          name = tr("plot.ca.legend.supplementary"),
          showlegend = has_site_suppl && !has_site_active,
          legendgroup = "supplementary",
          legendrank = 3
        )
        
        # 14C labels also for supplementary sites with smart distribution
        if (!is.null(c14_calibrated_reactive) && isTRUE(input$show_c14_overlay)) {
          suppl_c14_sites <- site_subset[site_subset$c14_label != "", ]
          if (nrow(suppl_c14_sites) > 0) {
            # Smart label positioning for supplementary sites - use values from overlay module UI
            label_positions <- calculate_smart_label_positions(
              suppl_c14_sites$x, suppl_c14_sites$y,
              offset_distance = input$c14_label_offset %||% 0.4,
              min_separation = input$c14_min_separation %||% 0.2
            )
            
            # For each supplementary 14C site: label with leader line
            for (idx in 1:nrow(suppl_c14_sites)) {
              site_x <- suppl_c14_sites$x[idx]
              site_y <- suppl_c14_sites$y[idx]
              label_x <- label_positions$x[idx]
              label_y <- label_positions$y[idx]
              
              # Add leader line (slightly lighter for supplementary)
              p <- p %>% add_segments(
                x = site_x, y = site_y,
                xend = label_x, yend = label_y,
                line = list(color = "#7f8c8d", width = 1.5),  # Solid line, lighter
                showlegend = FALSE,
                hoverinfo = "skip"
              )
              
              # Add 14C label with configurable size (supplementary)
              c14_font_size <- if (is.null(input$c14_label_size)) 9 else input$c14_label_size
              p <- p %>% add_annotations(
                x = label_x, y = label_y,
                text = suppl_c14_sites$c14_label[idx],
                font = list(
                  size = c14_font_size,
                  color = "#7f8c8d",  # Slightly muted for supplementary
                  family = "Arial, sans-serif"
                ),
                # White background with light border (slightly more transparent for supplementary)
                bgcolor = "rgba(255, 255, 255, 0.8)",
                bordercolor = "#95a5a6",
                borderwidth = 1,
                borderpad = 3,
                showarrow = FALSE,
                xanchor = "center",
                yanchor = "middle"
              )
            }
          }
        }
      }
      
      # Supplementary types
      type_subset <- suppl_data[suppl_data$type == 'Type', ]
      if (nrow(type_subset) > 0) {
        type_color <- if (isTRUE(input$ca_color_by_group)) {
          sapply(type_subset$color_group, function(cg) {
            cc_idx <- match(cg, unique(plot_data$color_group))
            if (!is.na(cc_idx) && cc_idx <= length(colors)) colors[cc_idx] else "#ec7063"
          })
        } else rep(colors["Type Supplementary"], nrow(type_subset))
        
        p <- p %>% add_markers(
          x = type_subset$x, y = type_subset$y,
          marker = ca_type_marker(type_color, (input$point_size %||% 3) * 4, element_type = "Supplementary"),
          hovertext = type_subset$hover_text, hoverinfo = 'text',
          name = paste0(tr("plot.ca.legend.types"), " (", tolower(tr("plot.ca.legend.supplementary")), ")"),
          showlegend = FALSE,  # Types already in legend
          legendgroup = "types"
        )
      }
    }
    
    # ===== CONFIDENCE ELLIPSES =====
    if (isTRUE(input$show_confidence_ellipses)) {
      # Only when groups are colored
      sites_only <- plot_data[plot_data$type == 'Site' & !is.na(plot_data$group), ]
      
      if (nrow(sites_only) > 0) {
        confidence_level <- 0.95  # Fixed at 95%
        unique_groups <- unique(sites_only$group)
        
        # Hole Gruppenfarben
        seri_colors <- seri_arc_colors()
        if (!is.null(seri_colors) && "groups" %in% names(seri_colors)) {
          group_colors <- seri_colors$groups
        } else {
          # Fallback: Default colors
          group_colors <- setNames(
            scales::hue_pal()(length(unique_groups)),
            unique_groups
          )
        }
        
        for (grp in unique_groups) {
          grp_data <- sites_only[sites_only$group == grp, ]
          
          if (nrow(grp_data) >= 3) {  # At least 3 points for ellipse
            ellipse <- calculate_confidence_ellipse(
              grp_data$x, grp_data$y, 
              confidence = confidence_level
            )
            
            if (!is.null(ellipse)) {
              # Halbdurchsichtige Ellipse in Gruppenfarbe
              grp_color <- group_colors[grp]
              if (is.null(grp_color)) grp_color <- "#999999"
              
              rgb_col <- col2rgb(grp_color)
              fill_color <- sprintf("rgba(%d, %d, %d, 0.15)", 
                                    rgb_col[1], rgb_col[2], rgb_col[3])
              line_color <- sprintf("rgba(%d, %d, %d, 0.5)", 
                                    rgb_col[1], rgb_col[2], rgb_col[3])
              
              # Zeichne Ellipse als Polygon
              p <- p %>% add_polygons(
                x = ellipse$x,
                y = ellipse$y,
                fillcolor = fill_color,
                line = list(color = line_color, width = 2, dash = "dash"),
                hoverinfo = 'text',
                text = paste0(tr("plot.ca.confidence.ellipse"), "<br>",
                              tr("term.group"), ": ", grp, "<br>",
                              "n = ", nrow(grp_data)),
                name = paste0(tr("plot.ca.ellipse"), " ", grp),
                showlegend = FALSE
              )
            }
          }
        }
      }
    }
    
    # ===== GRUPPEN-LEGENDE (bei Gruppierung) =====
    if (isTRUE(input$ca_color_by_group)) {
      unique_groups <- unique(plot_data$color_group[!is.na(plot_data$color_group)])
      real_groups <- unique_groups[grepl("^G:", unique_groups)]
      if (length(real_groups) > 0) {
        x_range <- range(plot_data$x, na.rm = TRUE)
        y_range <- range(plot_data$y, na.rm = TRUE)
        invisible_x <- x_range[1] - diff(x_range) * 10  # Far left outside visible area
        invisible_y <- mean(y_range)
        
        for (grp in real_groups) {
          grp_idx <- match(grp, unique(plot_data$color_group))
          grp_color <- if (!is.na(grp_idx) && grp_idx <= length(colors)) colors[grp_idx] else "#7f8c8d"
          grp_name <- sub("^G:", "", grp)
          
          p <- p %>% add_markers(
            x = invisible_x, y = invisible_y,
            marker = list(size = 12, color = grp_color, symbol = 'square'),
            name = grp_name,
            showlegend = TRUE,
            hoverinfo = 'skip',
            legendgroup = "groups",
            legendrank = 10 + match(grp, real_groups)
          )
        }
      }
    }
    
    # ===== GRUPPENSCHWERPUNKTE (wie PAST) =====
    if (isTRUE(input$show_group_centroids)) {
      sites_only <- plot_data[plot_data$type == 'Site' & !is.na(plot_data$group), ]
      
      if (nrow(sites_only) > 0) {
        # Aggregate by group: Mean of x/y coordinates
        centroids <- aggregate(cbind(x, y) ~ group, data = sites_only, FUN = mean)

        # Red crosses at centroids (like PAST)
        p <- p %>% add_markers(
          x = centroids$x, y = centroids$y,
          marker = list(symbol = 'x', size = 16, color = '#e74c3c', line = list(width = 3)),
          hoverinfo = 'text',
          text = paste0(tr("plot.ca.centroid"), " ", centroids$group,
                        "<br>X: ", round(centroids$x, 3),
                        "<br>Y: ", round(centroids$y, 3)),
          name = tr("term.centroids"),
          showlegend = FALSE
        )
        
        # Red labels above centroids (like PAST - "in red font")
        p <- p %>% add_text(
          x = centroids$x,
          y = centroids$y,
          text = centroids$group,
          textfont = list(size = 14, color = '#e74c3c', family = "Arial, sans-serif"),
          textposition = "top center",
          showlegend = FALSE,
          hoverinfo = "skip"
        )
      }
    }
    
    # Fix axis range to actual data points (ignore invisible legend markers)
    x_range <- range(plot_data$x, na.rm = TRUE)
    y_range <- range(plot_data$y, na.rm = TRUE)
    x_buffer <- diff(x_range) * 0.05  # 5% Puffer
    y_buffer <- diff(y_range) * 0.05
    
    p <- p %>% layout(
      xaxis = list(range = c(x_range[1] - x_buffer, x_range[2] + x_buffer)),
      yaxis = list(range = c(y_range[1] - y_buffer, y_range[2] + y_buffer))
    )
    
    standard_plotly_config(p, "2d")
  })
  
  # ===== SCIENTIFIC OUTPUTS =====
  
  # Eigenvalues (without subjective assessment)
  output$ca_eigen <- renderTable({
    req(ca_res())

    eig <- ca_res()$eig
    validate(need(!is.null(eig) && nrow(eig) > 0, tr("validate.ca.no.eigenvalues")))

    df <- data.frame(
      Dimension = paste0("Dim", 1:nrow(eig)),
      Eigenwert = round(as.numeric(eig[, 1]), 4),
      Varianz = round(as.numeric(eig[, 2]), 2),
      Kumuliert = round(as.numeric(eig[, 3]), 2),
      check.names = FALSE
    )
    names(df) <- c(tr("ca.eigen.col.dimension"), tr("ca.eigen.col.eigenvalue"),
                   tr("ca.eigen.col.variance"), tr("ca.eigen.col.cumulative"))
    df
  }, bordered = TRUE, striped = TRUE, hover = TRUE)
  
  # CA statistics (without subjective assessment)
  output$ca_stats <- renderText({
    req(ca_res())

    eig <- ca_res()$eig
    validate(need(!is.null(eig) && nrow(eig) > 0, tr("validate.ca.no.eigenvalues")))

    total_inertia <- sum(as.numeric(eig[, 1]))
    dim1_var <- as.numeric(eig[1, 2])
    dim2_var <- if(nrow(eig) >= 2) as.numeric(eig[2, 2]) else 0
    cumulative <- if(nrow(eig) >= 2) as.numeric(eig[2, 3]) else dim1_var

    sprintf(tr("ca.eigen.stats.format"),
      dim1_var, dim2_var, cumulative, total_inertia, nrow(eig)
    )
  })
  
  # ===== MALINVAUD-TEST =====
  output$ca_malinvaud <- renderTable({
    req(ca_res(), filtered_data())
    
    tryCatch({
      res <- ca_res()
      
      # Extract necessary values
      eig <- res$eig
      n_obs <- sum(filtered_data())
      n_dims <- min(nrow(eig), 10)  # Teste max. 10 Dimensionen
      
      malinvaud_results <- data.frame(
        Dimension = paste0("Dim", 1:n_dims),
        Eigenvalue = round(as.numeric(eig[1:n_dims, 1]), 4),
        Chi_sq = NA,
        df = NA,
        p_value = NA,
        Significance = "",
        stringsAsFactors = FALSE
      )
      # Translated column names
      names(malinvaud_results) <- c(
        tr("ca.malinvaud.col.dimension"),
        tr("ca.malinvaud.col.eigenvalue"),
        tr("ca.malinvaud.col.chisq"),
        tr("ca.malinvaud.col.df"),
        tr("ca.malinvaud.col.pvalue"),
        tr("ca.malinvaud.col.significance")
      )
      
      for (i in 1:n_dims) {
        # Cumulative inertia from dimension i
        lambda_sum <- sum(as.numeric(eig[i:nrow(eig), 1]))

        # Chi-squared statistic with Bartlett correction (for sparse matrices)
        p <- nrow(res$row$coord)  # Number of rows
        q <- nrow(res$col$coord)  # Number of columns
        bartlett_correction <- n_obs - (p + q - 1) / 2
        chi_sq <- bartlett_correction * lambda_sum

        # Freiheitsgrade
        df <- (p - i) * (q - i)

        if (df > 0) {
          # P-Wert
          p_val <- pchisq(chi_sq, df, lower.tail = FALSE)

          # Use column indices since names are translated
          malinvaud_results[i, 3] <- round(chi_sq, 2)
          malinvaud_results[i, 4] <- df
          malinvaud_results[i, 5] <- if (p_val < 0.001) "<0.001" else sprintf("%.3f", p_val)
          malinvaud_results[i, 6] <- if (p_val < 0.001) "***" else
            if (p_val < 0.01) "**" else
              if (p_val < 0.05) "*" else "n.s."
        }
      }
      
      malinvaud_results
    }, error = function(e) {
      error_df <- data.frame(
        Info = tr("ca.malinvaud.error.info"),
        Error = as.character(e$message),
        stringsAsFactors = FALSE
      )
      names(error_df) <- c("Info", tr("ca.malinvaud.error.label"))
      error_df
    })
  }, striped = TRUE, hover = TRUE, bordered = TRUE, spacing = "xs",
  caption = tr("ca.malinvaud.caption"))
  
  # Warning when too many dimensions are significant (sample size problem)
  output$ca_malinvaud_warning <- renderUI({
    req(ca_res(), filtered_data())
    
    tryCatch({
      res <- ca_res()
      eig <- res$eig
      n_obs <- sum(filtered_data())
      
      # Count significant dimensions (p<0.05)
      n_dims <- min(nrow(eig), 10)
      n_significant <- 0
      
      for (i in 1:n_dims) {
        lambda_sum <- sum(as.numeric(eig[i:nrow(eig), 1]))
        p <- nrow(res$row$coord)
        q <- nrow(res$col$coord)
        bartlett_correction <- n_obs - (p + q - 1) / 2
        chi_sq <- bartlett_correction * lambda_sum
        df <- (p - i) * (q - i)
        
        if (df > 0) {
          p_val <- pchisq(chi_sq, df, lower.tail = FALSE)
          if (p_val < 0.05) n_significant <- n_significant + 1
        }
      }
      
      # Show warning when >70% of tested dimensions are significant AND n_obs > 5000
      if (n_significant / n_dims > 0.7 && n_obs > 5000) {
        div(class="alert alert-warning", style="margin-top: 15px; font-size: 0.9em;",
            tags$b(tr("ca.malinvaud.warning.title")), br(), br(),
            sprintf(tr("ca.malinvaud.warning.text"),
                    n_significant, n_dims, (n_significant/n_dims)*100, format(n_obs, big.mark=".")), br(), br(),
            tags$b(tr("ca.malinvaud.warning.meaning")), br(),
            tr("ca.malinvaud.warning.explanation"), br(), br(),
            tags$b(tr("ca.malinvaud.warning.recommendation")), " ", tr("ca.malinvaud.warning.use.scree"), " ", tags$b(tr("ca.malinvaud.warning.scree.plot")), " ",
            tr("ca.malinvaud.warning.interpret")
        )
      } else if (n_significant / n_dims < 0.3 && n_obs < 1000) {
        # For small datasets: Few significant dimensions = possibly insufficient data
        div(class="alert alert-info", style="margin-top: 15px; font-size: 0.9em;",
            tags$b(tr("ca.malinvaud.small.title")), br(), br(),
            sprintf(tr("ca.malinvaud.small.text"),
                    n_significant, n_dims, format(n_obs, big.mark=".")), br(), br(),
            tr("ca.malinvaud.small.explanation"), " ", tags$b(tr("ca.malinvaud.small.meaningful")), ". ",
            tr("ca.malinvaud.small.caution")
        )
      } else {
        NULL  # No warning needed
      }
      
    }, error = function(e) {
      NULL
    })
  })
  
  # ===== SCREE-PLOT =====
  output$ca_screeplot <- renderPlotly({
    req(ca_res())
    
    tryCatch({
      res <- ca_res()
      eig <- res$eig
      
      validate(need(!is.null(eig) && nrow(eig) > 0, tr("validate.ca.no.eigenvalues")))
      
      # Prepare data
      n_dims <- min(nrow(eig), 15)  # Show max 15 dimensions
      scree_data <- data.frame(
        Dimension = 1:n_dims,
        Eigenwert = as.numeric(eig[1:n_dims, 1]),
        Varianz = as.numeric(eig[1:n_dims, 2]),
        stringsAsFactors = FALSE
      )
      
      # Create plot with both Y-axes
      p <- plot_ly(scree_data) %>%
        # Line for eigenvalues (left Y-axis)
        add_trace(
          x = ~Dimension,
          y = ~Eigenwert,
          type = 'scatter',
          mode = 'lines+markers',
          name = tr("plot.ca.scree.trace.eigenvalue"),
          line = list(color = '#2980b9', width = 3),
          marker = list(size = 10, color = '#2980b9', symbol = 'circle'),
          hovertemplate = paste0(
            '<b>', tr("term.dimension"), ' %{x}</b><br>',
            tr("plot.ca.scree.trace.eigenvalue"), ': %{y:.4f}<br>',
            '<extra></extra>'
          ),
          yaxis = 'y1'
        ) %>%
        # Bars for variance (right Y-axis)
        add_trace(
          x = ~Dimension,
          y = ~Varianz,
          type = 'bar',
          name = tr("plot.ca.scree.trace.variance"),
          marker = list(color = '#e74c3c', opacity = 0.3),
          hovertemplate = paste0(
            '<b>', tr("term.dimension"), ' %{x}</b><br>',
            tr("plot.ca.scree.trace.variance"), ': %{y:.1f}%<br>',
            '<extra></extra>'
          ),
          yaxis = 'y2'
        ) %>%
        layout(
          title = list(
            text = tr("plot.ca.scree.title"),
            font = list(size = 16, family = "Arial, sans-serif")
          ),
          xaxis = list(
            title = tr("plot.ca.scree.xaxis"),
            dtick = 1,
            gridcolor = '#ecf0f1',
            titlefont = list(size = 14)
          ),
          yaxis = list(
            title = tr("plot.ca.scree.yaxis.left"),
            side = 'left',
            gridcolor = '#ecf0f1',
            titlefont = list(size = 14, color = '#2980b9'),
            tickfont = list(color = '#2980b9')
          ),
          yaxis2 = list(
            title = tr("plot.ca.scree.yaxis.right"),
            side = 'right',
            overlaying = 'y',
            gridcolor = 'transparent',
            titlefont = list(size = 14, color = '#e74c3c'),
            tickfont = list(color = '#e74c3c')
          ),
          hovermode = 'x unified',
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          showlegend = TRUE,
          legend = list(
            x = 0.7, y = 0.95,
            bgcolor = 'rgba(255,255,255,0.8)',
            bordercolor = '#bdc3c7',
            borderwidth = 1
          ),
          margin = list(t = 60, b = 60, l = 70, r = 70)
        ) %>%
        config(displayModeBar = TRUE, modeBarButtonsToRemove = c('lasso2d', 'select2d'))
      
      p
      
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste(tr("plot.ca.error"), e$message), font = list(color = "red")))
    })
  })

  # ===== BETWEEN-GROUP INERTIA =====
  output$ca_between_group <- renderText({
    req(ca_res())
    
    tryCatch({
      res <- ca_res()
      
      # Hole plot_data reaktiv
      plot_data <- isolate(ca_plot_data())
      
      # Check if groups are present
      if (!"group" %in% names(plot_data)) {
        return(tr("ca.grouping.unavailable"))
      }
      
      sites_with_groups <- plot_data[plot_data$type == 'Site' & !is.na(plot_data$group), ]
      
      if (nrow(sites_with_groups) == 0) {
        return(tr("ca.grouping.unavailable"))
      }
      
      unique_groups <- unique(sites_with_groups$group)
      n_total <- nrow(sites_with_groups)
      
      # Gesamtschwerpunkt
      grand_center_x <- mean(sites_with_groups$x, na.rm = TRUE)
      grand_center_y <- mean(sites_with_groups$y, na.rm = TRUE)
      
      between_inertia <- 0
      within_inertia <- 0
      
      for (grp in unique_groups) {
        grp_data <- sites_with_groups[sites_with_groups$group == grp, ]
        n_grp <- nrow(grp_data)
        
        if (n_grp == 0) next
        
        # Gruppenschwerpunkt
        grp_center_x <- mean(grp_data$x, na.rm = TRUE)
        grp_center_y <- mean(grp_data$y, na.rm = TRUE)
        
        # Between: Abstand Gruppenschwerpunkt zu Gesamtschwerpunkt
        between_inertia <- between_inertia + 
          n_grp * ((grp_center_x - grand_center_x)^2 + (grp_center_y - grand_center_y)^2)
        
        # Within: Distances within the group
        for (i in 1:n_grp) {
          within_inertia <- within_inertia + 
            (grp_data$x[i] - grp_center_x)^2 + (grp_data$y[i] - grp_center_y)^2
        }
      }
      
      # Normalisierung
      between_inertia <- between_inertia / n_total
      within_inertia <- within_inertia / n_total
      total_inertia_2d <- between_inertia + within_inertia
      
      # Verhindere Division durch 0
      if (total_inertia_2d == 0) {
        return("⚠️ No variance in data.")
      }

      # Percentage distribution
      between_pct <- (between_inertia / total_inertia_2d) * 100
      within_pct <- (within_inertia / total_inertia_2d) * 100

      sprintf(
        "🎯 VARIANCE PARTITIONING (2D space):\n\n• Between-Group: %.1f%%\n  (Differences BETWEEN groups)\n\n• Within-Group: %.1f%%\n  (Variation WITHIN groups)\n\n📈 INTERPRETATION:\n%s\n\n📂 DETAILS:\n• Number of groups: %d\n• Inertia (between): %.4f\n• Inertia (within): %.4f",
        between_pct, within_pct,
        if (between_pct > 70) "✅ Groups are well separated" else
          if (between_pct > 50) "⚠️ Groups partially overlap" else
            "❌ Groups are weakly separated",
        length(unique_groups),
        between_inertia, within_inertia
      )
    }, error = function(e) {
      paste0("❌ Calculation error:\n", e$message)
    })
  })
  
  # ===== DOWNLOAD-HANDLER =====
  
  
  # ===== PLOTLY QUALITY PLOTS (Contributions & Cos²) =====
  
  # Helper: Extract and sort contributions
  get_contrib_data <- function(ca_obj, choice, axis) {
    if (choice == "row") {
      contrib <- ca_obj$row$contrib[, axis]
      names <- rownames(ca_obj$row$contrib)
    } else {
      contrib <- ca_obj$col$contrib[, axis]
      names <- rownames(ca_obj$col$contrib)
    }
    
    # Sort descending
    ord <- order(contrib, decreasing = TRUE)
    data.frame(
      name = names[ord],
      value = contrib[ord],
      stringsAsFactors = FALSE
    )
  }
  
  # Helper: Extract and sort Cos²
  get_cos2_data <- function(ca_obj, choice, axes = 1:2) {
    if (choice == "row") {
      cos2_vals <- rowSums(ca_obj$row$cos2[, axes, drop = FALSE])
      names <- rownames(ca_obj$row$cos2)
    } else {
      cos2_vals <- rowSums(ca_obj$col$cos2[, axes, drop = FALSE])
      names <- rownames(ca_obj$col$cos2)
    }
    
    # Sort descending
    ord <- order(cos2_vals, decreasing = TRUE)
    data.frame(
      name = names[ord],
      value = cos2_vals[ord],
      stringsAsFactors = FALSE
    )
  }
  
  # CONTRIBUTIONS - Combined plot based on selection
  output$ca_contrib_selected <- renderPlotly({
    req(ca_res(), input$contrib_dimension, input$contrib_element)

    tryCatch({
      # Select dimension - extract number from "dim1", "dim2", etc.
      axis_num <- as.numeric(gsub("dim", "", input$contrib_dimension))
      dim_label <- paste0("Dim", axis_num)

      # Select element & color
      element_label <- if (input$contrib_element == "row") tr("term.sites") else tr("term.types")
      plot_color <- if (input$contrib_element == "row") '#3498db' else '#e74c3c'

      # Get data
      data <- get_contrib_data(ca_res(), input$contrib_element, axis_num)

      # Plot erstellen
      plot_ly(data, x = ~seq_along(name), y = ~value, type = 'bar',
              marker = list(color = plot_color, line = list(color = '#2c3e50', width = 1)),
              hovertemplate = paste0('<b>%{text}</b><br>',
                                     tr("term.contribution"), ': %{y:.2f}%<extra></extra>'),
              text = ~name) %>%
        layout(
          title = list(text = sprintf(tr("plot.ca.contrib.title"), element_label, dim_label),
                       font = list(size = 18, family = "Arial, sans-serif")),
          xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
          yaxis = list(title = tr("plot.ca.contrib.yaxis"), gridcolor = '#ecf0f1',
                       titlefont = list(size = 14)),
          hovermode = 'closest',
          plot_bgcolor = 'white',
          paper_bgcolor = 'white',
          margin = list(t = 60, b = 40, l = 60, r = 40)
        ) %>%
        config(displayModeBar = FALSE)
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste(tr("plot.ca.error"), e$message), font = list(color = "red")))
    })
  })
  
  # COS² - Combined plot based on selection
  output$ca_cos2_selected <- renderPlotly({
    req(ca_res(), input$cos2_element)

    tryCatch({
      # Select element & color
      element_label <- if (input$cos2_element == "row") tr("term.sites") else tr("term.types")
      plot_color <- if (input$cos2_element == "row") '#3498db' else '#e74c3c'

      # Get data (always Dim1+Dim2)
      data <- get_cos2_data(ca_res(), input$cos2_element, axes = 1:2)

      # DATENSPEZIFISCHE SCHWELLENWERTE (Quartile)
      cos2_median <- median(data$value, na.rm = TRUE)
      cos2_q25 <- quantile(data$value, 0.25, na.rm = TRUE)
      cos2_q75 <- quantile(data$value, 0.75, na.rm = TRUE)

      # Plot erstellen
      p <- plot_ly(data, x = ~seq_along(name), y = ~value, type = 'bar',
                   marker = list(color = plot_color, line = list(color = '#2c3e50', width = 1)),
                   hovertemplate = paste0('<b>%{text}</b><br>',
                                          'Cos\u00b2: %{y:.3f}<extra></extra>'),
                   text = ~name)

      # QUARTIL-LINIEN (datenspezifisch)
      p <- p %>%
        # Upper quartile (75%) - Green
        add_segments(x = 0, xend = nrow(data) + 1, y = cos2_q75, yend = cos2_q75,
                     line = list(color = '#27ae60', width = 2, dash = 'dash'),
                     showlegend = FALSE, hoverinfo = 'skip') %>%
        add_annotations(x = nrow(data), y = cos2_q75 + 0.02,
                        text = sprintf("%s (%.3f)", tr("plot.ca.cos2.upper.quartile"), cos2_q75),
                        showarrow = FALSE, font = list(color = '#27ae60', size = 11),
                        xanchor = 'right', yanchor = 'bottom') %>%
        # Median (50%) - Grau
        add_segments(x = 0, xend = nrow(data) + 1, y = cos2_median, yend = cos2_median,
                     line = list(color = '#7f8c8d', width = 2, dash = 'dot'),
                     showlegend = FALSE, hoverinfo = 'skip') %>%
        add_annotations(x = nrow(data), y = cos2_median + 0.02,
                        text = sprintf("%s (%.3f)", tr("plot.ca.cos2.median"), cos2_median),
                        showarrow = FALSE, font = list(color = '#7f8c8d', size = 11),
                        xanchor = 'right', yanchor = 'bottom') %>%
        # Unteres Quartil (25%) - Orange
        add_segments(x = 0, xend = nrow(data) + 1, y = cos2_q25, yend = cos2_q25,
                     line = list(color = '#e67e22', width = 2, dash = 'dash'),
                     showlegend = FALSE, hoverinfo = 'skip') %>%
        add_annotations(x = nrow(data), y = cos2_q25 + 0.02,
                        text = sprintf("%s (%.3f)", tr("plot.ca.cos2.lower.quartile"), cos2_q25),
                        showarrow = FALSE, font = list(color = '#e67e22', size = 11),
                        xanchor = 'right', yanchor = 'bottom')

      # Layout with explanation
      p <- p %>% layout(
        title = list(
          text = paste0(sprintf(tr("plot.ca.cos2.title"), element_label, "Dim1+Dim2"), "<br>",
                        "<sub style='font-size:10px; color:#7f8c8d;'>", tr("plot.ca.cos2.subtitle"), "</sub>"),
          font = list(size = 16, family = "Arial, sans-serif")
        ),
        xaxis = list(title = "", showticklabels = FALSE, showgrid = FALSE),
        yaxis = list(title = tr("plot.ca.cos2.yaxis"), gridcolor = '#ecf0f1', range = c(0, 1),
                     titlefont = list(size = 14)),
        hovermode = 'closest',
        plot_bgcolor = 'white',
        paper_bgcolor = 'white',
        margin = list(t = 80, b = 40, l = 60, r = 40)
      ) %>%
        config(displayModeBar = FALSE)

      p
    }, error = function(e) {
      plotly_empty() %>%
        layout(title = list(text = paste(tr("plot.ca.error"), e$message), font = list(color = "red")))
    })
  })
  
  # INFO-BOX: Explanation of quartile lines
  output$ca_cos2_info <- renderUI({
    div(class="alert alert-info", style="margin-top: 15px; font-size: 0.9em;",
        tags$b(tr("ca.cos2.info.title")), br(), br(),
        tags$b(tr("ca.cos2.info.important")), " ", tr("ca.cos2.info.no.thresholds"), " ", tags$b(tr("ca.cos2.info.no.absolute")), " ", tr("ca.cos2.info.for.cos2"), br(), br(),
        tags$b(tr("ca.cos2.info.quartiles.title")), br(),
        tags$ul(
          tags$li(tags$b(style="color: #27ae60;", tr("ca.cos2.info.upper.quartile")), " ", tr("ca.cos2.info.upper.desc")),
          tags$li(tags$b(style="color: #7f8c8d;", tr("ca.cos2.info.median")), " ", tr("ca.cos2.info.median.desc")),
          tags$li(tags$b(style="color: #e67e22;", tr("ca.cos2.info.lower.quartile")), " ", tr("ca.cos2.info.lower.desc"))
        ),
        tags$b(tr("ca.cos2.info.science.title")), br(),
        tr("ca.cos2.info.science.text1"), " ", tags$b(tr("ca.cos2.info.science.normal")), tr("ca.cos2.info.science.text2"), br(), br(),
        tags$b(tr("ca.cos2.info.recommendation")), " ", tr("ca.cos2.info.recommend.text"), " ", tags$b(tr("ca.cos2.info.recommend.relative")), " ", tr("ca.cos2.info.recommend.within"), br(), br(),
        tags$b(tr("ca.cos2.info.sources")), br(),
        tags$ul(
          tags$li("Greenacre (2007): 'No fixed thresholds can be universally applied'"),
          tags$li("Lebart et al. (1984): 'Evaluation based on relative comparisons'"),
          tags$li("Benzécri (1992): ", tr("ca.cos2.info.benzecri"))
        )
    )
  })
  
  # ===== EXCEL EXPORT HANDLER =====
  
  # Contributions Excel Export - ALWAYS all 3 dimensions for Sites & Types
  output$download_contrib_excel <- downloadHandler(
    filename = function() {
      paste0("CA_Contributions_Dim1-3_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(ca_res())
      
      tryCatch({
        ca_obj <- ca_res()
        n_dims <- min(nrow(ca_obj$eig), 3)  # Max. 3 Dimensionen
        
        # Write Excel with metadata
        wb <- openxlsx::createWorkbook()
        
        # Sheet 1: Info
        openxlsx::addWorksheet(wb, "Info")
        info_df <- data.frame(
          Parameter = c("Export_Type", "Dimensions", "Sites_Count", "Types_Count", "Export_Date"),
          Value = c(
            "Contributions for Sites & Types",
            paste0("Dim1-", n_dims),
            nrow(ca_obj$row$contrib),
            nrow(ca_obj$col$contrib),
            format(Sys.time(), "%Y-%m-%d %H:%M:%S")
          ),
          stringsAsFactors = FALSE
        )
        openxlsx::writeData(wb, "Info", info_df)

        # Sheet 2: Sites Contributions (all dimensions)
        openxlsx::addWorksheet(wb, "Sites_Contributions")
        sites_data <- data.frame(
          Site = rownames(ca_obj$row$contrib),
          stringsAsFactors = FALSE
        )
        
        # Add contributions for each dimension
        for (i in 1:n_dims) {
          col_name <- paste0("Dim", i, "_Contribution_%")
          sites_data[[col_name]] <- round(ca_obj$row$contrib[, i], 2)
        }
        
        # Sort by Dim1 contribution
        sites_data <- sites_data[order(-sites_data[["Dim1_Contribution_%"]]), ]
        sites_data$Rang <- 1:nrow(sites_data)
        sites_data <- sites_data[, c("Rang", "Site", paste0("Dim", 1:n_dims, "_Contribution_%"))]
        
        openxlsx::writeData(wb, "Sites_Contributions", sites_data)
        
        # Sheet 3: Types Contributions (alle Dimensionen)
        openxlsx::addWorksheet(wb, "Types_Contributions")
        types_data <- data.frame(
          Type = rownames(ca_obj$col$contrib),
          stringsAsFactors = FALSE
        )
        
        # Add contributions for each dimension
        for (i in 1:n_dims) {
          col_name <- paste0("Dim", i, "_Contribution_%")
          types_data[[col_name]] <- round(ca_obj$col$contrib[, i], 2)
        }
        
        # Sort by Dim1 contribution
        types_data <- types_data[order(-types_data[["Dim1_Contribution_%"]]), ]
        types_data$Rang <- 1:nrow(types_data)
        types_data <- types_data[, c("Rang", "Type", paste0("Dim", 1:n_dims, "_Contribution_%"))]
        
        openxlsx::writeData(wb, "Types_Contributions", types_data)
        
        # Formatierung
        for (sheet_name in c("Sites_Contributions", "Types_Contributions")) {
          # Header-Stil
          header_style <- openxlsx::createStyle(
            textDecoration = "bold", 
            fgFill = "#3498db", 
            fontColour = "white", 
            halign = "center"
          )
          openxlsx::addStyle(wb, sheet_name, header_style, 
                             rows = 1, cols = 1:(2 + n_dims), gridExpand = TRUE)
          
          # Column widths
          openxlsx::setColWidths(wb, sheet_name, 
                                 cols = 1:(2 + n_dims), 
                                 widths = c(8, 30, rep(18, n_dims)))
        }
        
        # Save
        openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
        
        showNotification(tr("notify.ca.excel.complete"),
                         type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("⚠ Excel Export Error:", e$message),
                         type = "error", duration = 5)
      })
    }
  )
  
  # Cos² Excel Export
  output$download_cos2_excel <- downloadHandler(
    filename = function() {
      element_label <- if (input$cos2_element == "row") "Sites" else "Types"
      paste0("CA_Cos2_", element_label, "_Dim1+Dim2_", 
             format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      req(ca_res(), input$cos2_element)
      
      # Get ALL data (Dim1+Dim2)
      data <- get_cos2_data(ca_res(), input$cos2_element, axes = 1:2)
      
      # Add rank and rating
      data$Rank <- seq_len(nrow(data))
      data$Rating <- ifelse(data$value >= 0.7, "Good",
                               ifelse(data$value >= 0.3, "Medium", "Weak"))

      # Reorder
      export_data <- data.frame(
        Rank = data$Rank,
        Element = data$name,
        Cos2_Value = round(data$value, 4),
        Rating = data$Rating,
        stringsAsFactors = FALSE
      )
      
      wb <- openxlsx::createWorkbook()

      # Sheet 1: Metadata
      openxlsx::addWorksheet(wb, "Info")
      info_df <- data.frame(
        Parameter = c("Element_Type", "Dimensions", "Element_Count",
                      "Thresholds", "Export_Date"),
        Value = c(if (input$cos2_element == "row") "Sites" else "Types",
                 "Dim1 + Dim2",
                 nrow(export_data),
                 "Good: >0.7, Medium: 0.3-0.7, Weak: <0.3",
                 format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
        stringsAsFactors = FALSE
      )
      openxlsx::writeData(wb, "Info", info_df)

      # Sheet 2: Cos² Data
      openxlsx::addWorksheet(wb, "Cos2_Quality")
      openxlsx::writeData(wb, "Cos2_Quality", export_data)

      # Formatting
      openxlsx::setColWidths(wb, "Cos2_Quality", cols = 1:4, widths = c(8, 30, 15, 12))
      header_style <- openxlsx::createStyle(textDecoration = "bold", fgFill = "#e74c3c",
                                            fontColour = "white", halign = "center")
      openxlsx::addStyle(wb, "Cos2_Quality", header_style, rows = 1, cols = 1:4, gridExpand = TRUE)

      # Conditional formatting for evaluation
      good_style <- openxlsx::createStyle(fgFill = "#d4edda")
      medium_style <- openxlsx::createStyle(fgFill = "#fff3cd")
      weak_style <- openxlsx::createStyle(fgFill = "#f8d7da")

      for (i in 2:(nrow(export_data) + 1)) {
        rating <- export_data$Rating[i - 1]
        if (rating == "Good") {
          openxlsx::addStyle(wb, "Cos2_Quality", good_style, rows = i, cols = 4)
        } else if (rating == "Medium") {
          openxlsx::addStyle(wb, "Cos2_Quality", medium_style, rows = i, cols = 4)
        } else {
          openxlsx::addStyle(wb, "Cos2_Quality", weak_style, rows = i, cols = 4)
        }
      }

      openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  # ===== EIGENVALUES TAB HELP MODAL =====
  observeEvent(input$help_eigenwerte, {
    showModal(modalDialog(
      title = "📊 Eigenvalues Tab: Help & Interpretation",
      size = "l",
      easyClose = TRUE,
      footer = modalButton(tr("ca.btn.close")),
      
      h4("📊 What does this tab show?"),
      p("The Eigenvalues tab provides statistical analyses for quality and significance of the correspondence analysis."),

      hr(),

      h4("📈 1. Eigenvalues & Variance"),
      p("This table shows the variance explained by each CA dimension:"),
      tags$ul(
        tags$li(tags$b("Eigenvalue:"), " Inertia value of the dimension"),
        tags$li(tags$b("Percentage of variance:"), " How much % of total variance does this dimension explain?"),
        tags$li(tags$b("Cumulative % of variance:"), " Cumulative variance explained up to this dimension")
      ),
      div(class="alert alert-info",
          tags$b("ℹ️ Important:"), " Low variance values are completely normal in CA!", br(), br(),
          tags$b("Greenacre (2017):"), " \"In sparse contingency tables the first dimensions often account for only ",
          tags$b("10-20%"), " of the total inertia. This is entirely normal and does not affect the interpretability of the solution.\"", br(), br(),
          tags$b("Benzécri (1992):"), " \"Because the chi-square distance captures structure in many small contrasts, the explained inertia of the first axes is usually low. A dimension with only ",
          tags$b("10%"), " can still represent the main underlying factor.\"", br(), br(),
          tags$b("Husson et al. (2010):"), " \"In correspondence analysis, it is common that the first two axes explain ",
          tags$b("15-30%"), " of the inertia even when the pattern is very clear.\""
      ),

      hr(),

      h4("🔬 2. Malinvaud Test"),
      p("Statistical significance test for CA dimensions:"),
      tags$ul(
        tags$li(tags$b("Chi-Square:"), " Test statistic"),
        tags$li(tags$b("df:"), " Degrees of freedom"),
        tags$li(tags$b("p-value:"), " Significance level"),
        tags$li(tags$b("Significance:"), " *** p<0.001 (highly significant), ** p<0.01, * p<0.05, n.s. (not significant)")
      ),
      div(class="alert alert-success",
          tags$b("✅ Interpretation:"), " Dimensions with p<0.05 are statistically meaningful. ",
          "Typically Dim1-Dim3 are significant, higher dimensions often not."
      ),

      hr(),

      h4("🎯 3. Between-Group Inertia"),
      p("Measures how well groups are separated from each other:"),
      tags$ul(
        tags$li(tags$b("Between-Group:"), " Variance BETWEEN groups (= differences between phases)"),
        tags$li(tags$b("Within-Group:"), " Variance WITHIN groups (= scatter within phases)")
      ),
      tags$b("Assessment:"),
      tags$ul(
        tags$li("✅ >70% Between = Groups are well separated (clear chronological phases)"),
        tags$li("⚠️ 50-70% Between = Partial overlap (phases not clearly separable)"),
        tags$li("❌ <50% Between = Weak separation (grouping may not be meaningful)")
      ),
      div(class="alert alert-warning",
          tags$b("⚠️ Note:"), " Between-Group Inertia is only calculated when 'Color by groups' is enabled in the CA biplot."
      ),

      hr(),

      h4("📚 References"),
      tags$ul(
        tags$li("Benzécri, J.-P. (1992). ", tags$em("Correspondence Analysis Handbook"), ". Marcel Dekker."),
        tags$li("Greenacre, M. (2017). ", tags$em("Correspondence Analysis in Practice"), " (3rd ed.). Chapman & Hall/CRC."),
        tags$li("Husson, F., Lê, S., & Pagès, J. (2010). ", tags$em("Exploratory Multivariate Analysis by Example Using R"), ". CRC Press."),
        tags$li("Baxter, M. (1994). ", tags$em("Exploratory Multivariate Analysis in Archaeology"), ". Edinburgh University Press."),
        tags$li("Malinvaud, E. (1987). Data Analysis in Official Socio-Economic Statistics. In: ", tags$em("Revue d’Économie Politique"), ".")
      )
    ))
  })
  
  # ===== CA PLOT DOWNLOAD-HANDLER =====
  
  # PNG Export
  output$download_ca_plot_png <- create_png_download_handler(
    plot_output_id = "ca_plotly",
    base_filename = "CA_Biplot",
    session = session,
    tr = tr
  )
  
  # SVG Export
  output$download_ca_plot_svg <- create_svg_download_handler(
    plot_output_id = "ca_plotly",
    base_filename = "CA_Biplot",
    session = session,
    tr = tr
  )
  
  # PDF Export (with real data)
  output$download_ca_plot_pdf <- create_pdf_download_handler(
    plot_data = ca_plot_data(),
    base_filename = "CA_Biplot",
    tr = tr,
    plot_generator_func = function(data) {
      # Basis-Parameter
      point_colors <- ifelse(data$type == "Site", "#3498db", "#e74c3c")
      point_shapes <- ifelse(data$type == "Site", 16, 17)
      point_sizes <- ifelse(data$element_type == "Active", 1.5, 1.0)
      
      # Plot erstellen
      plot(data$x, data$y,
           col = point_colors,
           pch = point_shapes,
           cex = point_sizes,
           xlab = input$x_dim %||% "Dim1",
           ylab = input$y_dim %||% "Dim2",
           main = "SeriARC Correspondence Analysis",
           las = 1)
      
      # Grid
      grid(col = "lightgray", lty = "dotted")
      
      # Achsen bei 0
      abline(h = 0, v = 0, col = "gray50", lty = "dashed")
      
      # Add labels (if enabled)
      if (!is.null(input$show_labels) && input$show_labels) {
        text(data$x, data$y, labels = data$lab, 
             pos = 4, cex = 0.7, col = "#2c3e50")
      }
      
      # Legende
      legend("topright",
             legend = c(tr("plot.ca.legend.sites.active"), tr("plot.ca.legend.types.active"), tr("plot.ca.legend.supplementary")),
             col = c("#3498db", "#e74c3c", "gray50"),
             pch = c(16, 17, 1),
             cex = 0.8,
             bg = "white")
    }
  )
  
  # HTML Export (Plotly)
  output$download_ca_plot_html <- downloadHandler(
    filename = function() sprintf("CA_Biplot_%s.html", Sys.Date()),
    content = function(file) {
      showNotification(tr("notify.export.html"), type = "message", duration = 3)

      tryCatch({
        # Generiere den Plot neu
        p <- isolate(output$ca_plotly())

        # Save as HTML
        htmlwidgets::saveWidget(p, file, selfcontained = TRUE)

        showNotification(tr("notify.export.html.success"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(tr("notify.export.html.error"), e$message), type = "error", duration = 5)
      })
    }
  )
  
  # Excel data export
  output$download_ca_data <- downloadHandler(
    filename = function() sprintf("SeriARC_CA_Daten_%s.xlsx", Sys.Date()),
    content = function(file) {
      showNotification(tr("notify.export.excel"), type = "message", duration = 3)

      tryCatch({
        req(ca_res(), filtered_data())

        # Use helper function from download_components.R
        sheets <- create_ca_excel_sheets(ca_res(), filtered_data())

        # Schreibe Excel
        writexl::write_xlsx(sheets, file)
        
        showNotification(tr("notify.export.excel.success"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(tr("notify.export.excel.error"), e$message), type = "error", duration = 5)
      })
    }
  )

  # CSV data export
  output$download_ca_data_csv <- downloadHandler(
    filename = function() sprintf("SeriARC_CA_Koordinaten_%s.csv", Sys.Date()),
    content = function(file) {
      showNotification(tr("notify.export.csv"), type = "message", duration = 3)

      tryCatch({
        req(ca_plot_data())

        # Export plot data (Sites + Types)
        export_data <- ca_plot_data()[, c("label", "type", "element_type", "x", "y")]
        colnames(export_data) <- c("Entity", "Element_Type", "Status", "Dim1", "Dim2")

        # Header schreiben
        header_lines <- c(
          paste("#", tr("plot.ca.title"), "- Coordinates"),
          sprintf("# %s: %s", tr("export.label.date"), format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
          sprintf("# %s: %s, %s", tr("term.dimension"), input$x_dim %||% "Dim1", input$y_dim %||% "Dim2"),
          ""
        )
        writeLines(header_lines, file)

        # Write data
        write.csv(export_data, file, row.names = FALSE, append = TRUE)

        showNotification(tr("notify.export.csv.success"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(tr("notify.export.csv.error"), e$message), type = "error", duration = 5)
      })
    }
  )
  
  # ===== NEUE OUTLIER-DETECTION: 3 MODI + 4 KATEGORIEN =====
  # ERSETZT die alte outlier_candidates reactive
  
  outlier_candidates <- eventReactive(input$outlier_recalculate, {
    req(ca_res(), filtered_data())
    
    showNotification(tr("notify.ca.outlier.detection"), type = "message", duration = 2)
    
    res <- ca_res()
    mat <- filtered_data()
    
    # Hole Methode aus UI
    detection_method <- input$outlier_detection_method %||% "quantile"
    
    # NEW: Get selected dimensions (incl. "robust")
    dim_selection <- input$outlier_dimensions %||% "12"
    
    # ===== ROBUST OUTLIER DETECTION (check all dimensions) =====
    if (dim_selection == "robust") {
      showNotification(tr("notify.ca.robust.detection"), type = "message", duration = 3)

      cos2_quantile <- (input$outlier_cos2_quantile %||% 10) / 100
      contrib_quantile <- (input$outlier_contrib_quantile %||% 5) / 100
      
      # Rufe helper_robust_outliers.R Funktion auf
      robust_results <- compute_robust_outliers(
        res = res,
        mat = mat,
        meta_data = meta_data,
        settings = list(
          cos2_quantile = cos2_quantile,
          contrib_quantile = contrib_quantile
        ),
        identify_supplementary = identify_supplementary,
        calculate_jackknife_influence = calculate_jackknife_influence
      )
      
      # Notification with result
      n_sites_robust <- if(!is.null(robust_results$sites)) sum(robust_results$sites$Count >= 2) else 0
      n_types_robust <- if(!is.null(robust_results$types)) sum(robust_results$types$Count >= 2) else 0
      
      showNotification(
        sprintf("✅ Robuste Outlier: %d Sites, %d Types (in ≥ 2 Dimensionen)", n_sites_robust, n_types_robust),
        type = "message", duration = 5
      )
      
      return(list(
        method = "robust",
        sites = robust_results$sites,
        types = robust_results$types,
        settings = list(
          cos2_quantile = cos2_quantile,
          contrib_quantile = contrib_quantile
        )
      ))
    }
    
    # Parse Dimensionen
    if (dim_selection == "12") {
      cos2_dims <- c(1, 2)
      contrib_dim <- 1
      dim_label <- "Dim1+Dim2"
    } else if (dim_selection == "13") {
      cos2_dims <- c(1, 3)
      contrib_dim <- 1
      dim_label <- "Dim1+Dim3"
    } else if (dim_selection == "23") {
      cos2_dims <- c(2, 3)
      contrib_dim <- 2
      dim_label <- "Dim2+Dim3"
    } else if (dim_selection == "123") {
      cos2_dims <- c(1, 2, 3)
      contrib_dim <- 1
      dim_label <- "Dim1+Dim2+Dim3"
    } else {
      cos2_dims <- c(1, 2)
      contrib_dim <- 1
      dim_label <- "Dim1+Dim2"
    }
    
    # Check if selected dimensions exist
    n_dims <- nrow(res$eig)
    if (max(cos2_dims) > n_dims) {
      showNotification(
        sprintf("⚠️ CA hat nur %d Dimensionen! Verwende Dim1+Dim2.", n_dims),
        type = "warning", duration = 5
      )
      cos2_dims <- c(1, min(2, n_dims))
      contrib_dim <- 1
      dim_label <- "Dim1+Dim2"
    }
    
    # Schwellenwerte (aus UI: kommen jetzt als Ganzzahlen 5-20, umrechnen zu 0.05-0.20)
    cos2_quantile <- (input$outlier_cos2_quantile %||% 10) / 100
    contrib_quantile <- (input$outlier_contrib_quantile %||% 5) / 100
    jackknife_threshold <- input$outlier_jackknife_threshold %||% 0.1
    
    # Feste Schwellenwerte
    cos2_threshold_fixed <- 0.3
    contrib_threshold_fixed <- 3.0
    
    # ===== 1. QUANTIL-BASIERTE DETECTION =====
    quantil_outliers <- list()
    
    if (detection_method %in% c("quantile", "combined")) {
      # SITES: Low Cos² (with selected dimensions)
      if (!is.null(res$row$cos2)) {
        if (ncol(res$row$cos2) >= max(cos2_dims)) {
          sites_cos2 <- rowSums(res$row$cos2[, cos2_dims, drop = FALSE])
        } else {
          # Fallback: Use available dimensions
          available_dims <- 1:min(ncol(res$row$cos2), max(cos2_dims))
          sites_cos2 <- rowSums(res$row$cos2[, available_dims, drop = FALSE])
        }
        
        cos2_quantile_threshold <- quantile(sites_cos2, probs = cos2_quantile, na.rm = TRUE)
        
        low_cos2_sites <- names(sites_cos2[
          sites_cos2 < cos2_quantile_threshold & sites_cos2 < cos2_threshold_fixed
        ])
        
        if (length(low_cos2_sites) > 0) {
          quantil_outliers$sites_low_cos2 <- data.frame(
            Entity = low_cos2_sites,
            Cos2 = sites_cos2[low_cos2_sites],
            stringsAsFactors = FALSE
          )
        }
      }
      
      # SITES: High Contribution (with selected dimension)
      if (!is.null(res$row$contrib) && contrib_quantile > 0) {
        # Use the first of the selected dimensions for Contribution
        if (ncol(res$row$contrib) >= contrib_dim) {
          sites_contrib <- res$row$contrib[, contrib_dim]
          contrib_quantile_threshold <- quantile(sites_contrib, probs = 1 - contrib_quantile, na.rm = TRUE)
          
          high_contrib_sites <- names(sites_contrib[
            sites_contrib > contrib_quantile_threshold & sites_contrib > contrib_threshold_fixed
          ])
          
          if (length(high_contrib_sites) > 0) {
            quantil_outliers$sites_high_contrib <- data.frame(
              Entity = high_contrib_sites,
              Contribution = sites_contrib[high_contrib_sites],
              stringsAsFactors = FALSE
            )
          }
        }
      }
      
      # TYPES: Low Cos² (with selected dimensions)
      if (!is.null(res$col$cos2)) {
        if (ncol(res$col$cos2) >= max(cos2_dims)) {
          types_cos2 <- rowSums(res$col$cos2[, cos2_dims, drop = FALSE])
        } else {
          available_dims <- 1:min(ncol(res$col$cos2), max(cos2_dims))
          types_cos2 <- rowSums(res$col$cos2[, available_dims, drop = FALSE])
        }
        
        cos2_quantile_threshold <- quantile(types_cos2, probs = cos2_quantile, na.rm = TRUE)
        
        low_cos2_types <- names(types_cos2[
          types_cos2 < cos2_quantile_threshold & types_cos2 < cos2_threshold_fixed
        ])
        
        if (length(low_cos2_types) > 0) {
          quantil_outliers$types_low_cos2 <- data.frame(
            Entity = low_cos2_types,
            Cos2 = types_cos2[low_cos2_types],
            stringsAsFactors = FALSE
          )
        }
      }
      
      # TYPES: High Contribution (with selected dimension)
      if (!is.null(res$col$contrib) && contrib_quantile > 0) {
        if (ncol(res$col$contrib) >= contrib_dim) {
          types_contrib <- res$col$contrib[, contrib_dim]
          contrib_quantile_threshold <- quantile(types_contrib, probs = 1 - contrib_quantile, na.rm = TRUE)
          
          high_contrib_types <- names(types_contrib[
            types_contrib > contrib_quantile_threshold & types_contrib > contrib_threshold_fixed
          ])
          
          if (length(high_contrib_types) > 0) {
            quantil_outliers$types_high_contrib <- data.frame(
              Entity = high_contrib_types,
              Contribution = types_contrib[high_contrib_types],
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }
    
    # ===== 2. JACKKNIFE-INFLUENCE DETECTION =====
    jackknife_outliers <- list()
    
    if (detection_method %in% c("jackknife", "combined")) {
      withProgress(message = '🔄 Running jackknife analysis...', value = 0, {
        jk_result <- calculate_jackknife_influence(mat, meta_data, threshold = jackknife_threshold)
        
        if (!is.null(jk_result$influential_sites) && length(jk_result$influential_sites) > 0) {
          jackknife_outliers$sites_influential <- data.frame(
            Entity = jk_result$influential_sites,
            Influence = jk_result$site_influences[jk_result$influential_sites],
            stringsAsFactors = FALSE
          )
        }
        
        if (!is.null(jk_result$influential_types) && length(jk_result$influential_types) > 0) {
          jackknife_outliers$types_influential <- data.frame(
            Entity = jk_result$influential_types,
            Influence = jk_result$type_influences[jk_result$influential_types],
            stringsAsFactors = FALSE
          )
        }
      })
    }
    
    # ===== 3. KOMBINIERTE KATEGORISIERUNG (4 GRUPPEN) =====
    if (detection_method == "combined") {
      # Extract entities
      sites_marginal <- if (!is.null(quantil_outliers$sites_low_cos2)) {
        quantil_outliers$sites_low_cos2$Entity
      } else character(0)
      
      sites_structural <- if (!is.null(jackknife_outliers$sites_influential)) {
        jackknife_outliers$sites_influential$Entity
      } else character(0)
      
      types_marginal <- if (!is.null(quantil_outliers$types_low_cos2)) {
        quantil_outliers$types_low_cos2$Entity
      } else character(0)
      
      types_structural <- if (!is.null(jackknife_outliers$types_influential)) {
        jackknife_outliers$types_influential$Entity
      } else character(0)
      
      # SITES KATEGORIEN
      sites_critical <- intersect(sites_marginal, sites_structural)
      sites_marginal_only <- setdiff(sites_marginal, sites_structural)
      sites_structural_only <- setdiff(sites_structural, sites_marginal)
      
      categorized_sites <- list()

      if (length(sites_critical) > 0) {
        categorized_sites$critical <- data.frame(
          Entity = sites_critical,
          Category = "CRITICAL",
          Reason = "Poorly represented AND structurally critical",
          stringsAsFactors = FALSE
        )
      }

      if (length(sites_marginal_only) > 0) {
        categorized_sites$marginal <- data.frame(
          Entity = sites_marginal_only,
          Category = "Marginally represented",
          Reason = "Low Cos² - weakly represented in 2D space",
          stringsAsFactors = FALSE
        )
      }

      if (length(sites_structural_only) > 0) {
        categorized_sites$structural <- data.frame(
          Entity = sites_structural_only,
          Category = "Structurally critical",
          Reason = "High Jackknife influence - strongly affects CA structure",
          stringsAsFactors = FALSE
        )
      }

      # TYPES CATEGORIES
      types_critical <- intersect(types_marginal, types_structural)
      types_marginal_only <- setdiff(types_marginal, types_structural)
      types_structural_only <- setdiff(types_structural, types_marginal)

      categorized_types <- list()

      if (length(types_critical) > 0) {
        categorized_types$critical <- data.frame(
          Entity = types_critical,
          Category = "CRITICAL",
          Reason = "Poorly represented AND structurally critical",
          stringsAsFactors = FALSE
        )
      }

      if (length(types_marginal_only) > 0) {
        categorized_types$marginal <- data.frame(
          Entity = types_marginal_only,
          Category = "Marginally represented",
          Reason = "Low Cos² - weakly represented in 2D space",
          stringsAsFactors = FALSE
        )
      }

      if (length(types_structural_only) > 0) {
        categorized_types$structural <- data.frame(
          Entity = types_structural_only,
          Category = "Structurally critical",
          Reason = "High Jackknife influence - strongly affects CA structure",
          stringsAsFactors = FALSE
        )
      }
      
      # Count total for combined
      n_total <- sum(
        if(!is.null(categorized_sites$critical)) nrow(categorized_sites$critical) else 0,
        if(!is.null(categorized_sites$marginal)) nrow(categorized_sites$marginal) else 0,
        if(!is.null(categorized_sites$structural)) nrow(categorized_sites$structural) else 0,
        if(!is.null(categorized_types$critical)) nrow(categorized_types$critical) else 0,
        if(!is.null(categorized_types$marginal)) nrow(categorized_types$marginal) else 0,
        if(!is.null(categorized_types$structural)) nrow(categorized_types$structural) else 0
      )
      
      showNotification(
        sprintf("✅ %d Outlier kategorisiert (%s)", n_total, dim_label),
        type = "message", duration = 3
      )
      
      return(list(
        method = "combined",
        sites = categorized_sites,
        types = categorized_types
      ))
    }
    
    # ===== 4. RETURN FOR INDIVIDUAL METHODS =====
    if (detection_method == "quantile") {
      result <- list(
        method = "quantile",
        sites_low_cos2 = quantil_outliers$sites_low_cos2,
        sites_high_contrib = quantil_outliers$sites_high_contrib,
        types_low_cos2 = quantil_outliers$types_low_cos2,
        types_high_contrib = quantil_outliers$types_high_contrib
      )
      
      # Count total
      n_total <- sum(
        if(!is.null(result$sites_low_cos2)) nrow(result$sites_low_cos2) else 0,
        if(!is.null(result$sites_high_contrib)) nrow(result$sites_high_contrib) else 0,
        if(!is.null(result$types_low_cos2)) nrow(result$types_low_cos2) else 0,
        if(!is.null(result$types_high_contrib)) nrow(result$types_high_contrib) else 0
      )
      
      showNotification(
        sprintf("✅ %d outliers found (%s)", n_total, dim_label),
        type = "message", duration = 3
      )

      return(result)
    } else if (detection_method == "jackknife") {
      result <- list(
        method = "jackknife",
        sites_influential = jackknife_outliers$sites_influential,
        types_influential = jackknife_outliers$types_influential
      )

      n_total <- sum(
        if(!is.null(result$sites_influential)) nrow(result$sites_influential) else 0,
        if(!is.null(result$types_influential)) nrow(result$types_influential) else 0
      )

      showNotification(
        sprintf("✅ %d influential elements found (%s)", n_total, dim_label),
        type = "message", duration = 3
      )
      
      return(result)
    }
    
    return(list(method = detection_method))
  })
  
  # ===== ROBUST OUTLIERS: COMPARISON ACROSS ALL DIMENSIONS =====
  robust_outliers <- eventReactive(input$outlier_find_robust, {
    req(ca_res(), filtered_data())
    
    showNotification(tr("notify.ca.robust.detection"), type = "message", duration = 3)
    
    res <- ca_res()
    mat <- filtered_data()
    
    detection_method <- input$outlier_detection_method %||% "quantile"
    cos2_quantile <- (input$outlier_cos2_quantile %||% 10) / 100
    contrib_quantile <- (input$outlier_contrib_quantile %||% 5) / 100
    cos2_threshold_fixed <- 0.3
    contrib_threshold_fixed <- 5.0
    
    # All 4 dimension combinations
    dim_combinations <- list(
      list(dims = c(1, 2), label = "Dim1+Dim2"),
      list(dims = c(1, 3), label = "Dim1+Dim3"),
      list(dims = c(2, 3), label = "Dim2+Dim3"),
      list(dims = c(1, 2, 3), label = "Dim1+Dim2+Dim3")
    )
    
    # Collect outliers for each combination
    all_sites_outliers <- list()
    all_types_outliers <- list()
    
    for (i in seq_along(dim_combinations)) {
      combo <- dim_combinations[[i]]
      dims <- combo$dims
      
      # Check if dimensions exist
      if (max(dims) > ncol(res$row$cos2)) next
      
      if (!is.null(res$row$cos2)) {
        sites_cos2 <- rowSums(res$row$cos2[, dims, drop = FALSE])
        cos2_threshold <- quantile(sites_cos2, probs = cos2_quantile, na.rm = TRUE)
        
        low_cos2_sites <- names(sites_cos2[
          sites_cos2 < cos2_threshold & sites_cos2 < cos2_threshold_fixed
        ])
        
        all_sites_outliers[[combo$label]] <- low_cos2_sites
      }
      
      if (!is.null(res$col$cos2)) {
        types_cos2 <- rowSums(res$col$cos2[, dims, drop = FALSE])
        cos2_threshold <- quantile(types_cos2, probs = cos2_quantile, na.rm = TRUE)
        
        low_cos2_types <- names(types_cos2[
          types_cos2 < cos2_threshold & types_cos2 < cos2_threshold_fixed
        ])
        
        all_types_outliers[[combo$label]] <- low_cos2_types
      }
    }
    
    # Count how often each outlier occurs
    sites_counts <- table(unlist(all_sites_outliers))
    types_counts <- table(unlist(all_types_outliers))
    
    robust_sites <- NULL
    if (length(sites_counts) > 0) {
      robust_sites <- data.frame(
        Entity = names(sites_counts),
        Count = as.integer(sites_counts),
        Dimensions = sapply(names(sites_counts), function(site) {
          dims_found <- names(all_sites_outliers)[sapply(all_sites_outliers, function(x) site %in% x)]
          paste(dims_found, collapse = ", ")
        }),
        stringsAsFactors = FALSE
      )
      # Sort by count (descending)
      robust_sites <- robust_sites[order(-robust_sites$Count), ]
    }
    
    robust_types <- NULL
    if (length(types_counts) > 0) {
      robust_types <- data.frame(
        Entity = names(types_counts),
        Count = as.integer(types_counts),
        Dimensions = sapply(names(types_counts), function(type) {
          dims_found <- names(all_types_outliers)[sapply(all_types_outliers, function(x) type %in% x)]
          paste(dims_found, collapse = ", ")
        }),
        stringsAsFactors = FALSE
      )
      robust_types <- robust_types[order(-robust_types$Count), ]
    }
    
    # Notification with result
    n_sites_robust <- sum(sites_counts >= 2)
    n_types_robust <- sum(types_counts >= 2)
    
    showNotification(
      sprintf("✅ Robuste Outlier: %d Sites, %d Types (in ≥ 2 Dimensionen)", n_sites_robust, n_types_robust),
      type = "message", duration = 5
    )
    
    return(list(
      method = "robust",
      sites = robust_sites,
      types = robust_types
    ))
  })
  
  # Outlier summary for UI
  output$ca_outlier_summary <- renderText({
    # If not yet calculated
    if (input$outlier_recalculate == 0) {
      return(tr("ca.outlier.click.detect"))
    }
    
    # Get results from outlier_candidates()
    candidates <- outlier_candidates()
    req(candidates)

    method <- candidates$method %||% "quantile"

    # ===== SHOW ROBUST OUTLIERS =====
    if (method == "robust") {
      summary_text <- tr("ca.outlier.sum.robust.header")

      sites <- candidates$sites
      types <- candidates$types

      if (!is.null(sites) && nrow(sites) > 0) {
        # Categories (only >= 2)
        n_all_4 <- sum(sites$Count == 4)
        n_3_of_4 <- sum(sites$Count == 3)
        n_2_of_4 <- sum(sites$Count == 2)

        summary_text <- paste0(summary_text, tr("ca.outlier.sum.sites.header"))
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.in.all4"), n_all_4))
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.in.3of4"), n_3_of_4))
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.in.2of4"), n_2_of_4))

        # Examples
        if (n_all_4 > 0) {
          all4_sites <- sites$Entity[sites$Count == 4]
          summary_text <- paste0(summary_text, tr("ca.outlier.sum.critical44"), paste(all4_sites, collapse=", "), "\n\n")
        }
        if (n_3_of_4 > 0) {
          three_sites <- head(sites$Entity[sites$Count == 3], 5)
          summary_text <- paste0(summary_text, tr("ca.outlier.sum.very.problematic"), paste(three_sites, collapse=", "))
          if (n_3_of_4 > 5) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.more"), n_3_of_4 - 5))
          summary_text <- paste0(summary_text, "\n\n")
        }
      }

      if (!is.null(types) && nrow(types) > 0) {
        n_all_4 <- sum(types$Count == 4)
        n_3_of_4 <- sum(types$Count == 3)
        n_2_of_4 <- sum(types$Count == 2)

        summary_text <- paste0(summary_text, tr("ca.outlier.sum.types.header"))
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.in.all4"), n_all_4))
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.in.3of4"), n_3_of_4))
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.in.2of4"), n_2_of_4))
      }

      summary_text <- paste0(summary_text, tr("ca.outlier.sum.recommendation"))
      
      return(summary_text)
    }
    
    # Hole Dimensionslabel
    dim_selection <- input$outlier_dimensions %||% "12"
    dim_label <- if (dim_selection == "12") "Dim1+Dim2" else
                 if (dim_selection == "13") "Dim1+Dim3" else
                 if (dim_selection == "23") "Dim2+Dim3" else
                 if (dim_selection == "123") "Dim1+Dim2+Dim3" else "Dim1+Dim2"
    
    summary_text <- ""
    
    if (method == "quantile") {
      summary_text <- sprintf(tr("ca.outlier.sum.quantile.header"), dim_label)

      n_sites_cos2 <- if (!is.null(candidates$sites_low_cos2)) nrow(candidates$sites_low_cos2) else 0
      n_sites_contrib <- if (!is.null(candidates$sites_high_contrib)) nrow(candidates$sites_high_contrib) else 0
      n_types_cos2 <- if (!is.null(candidates$types_low_cos2)) nrow(candidates$types_low_cos2) else 0
      n_types_contrib <- if (!is.null(candidates$types_high_contrib)) nrow(candidates$types_high_contrib) else 0
      total <- n_sites_cos2 + n_sites_contrib + n_types_cos2 + n_types_contrib

      if (total == 0) {
        summary_text <- paste0(summary_text, tr("ca.outlier.sum.no.outliers"))
      } else {
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.total"), total))
        if (n_sites_cos2 > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.sites.low.cos2"), n_sites_cos2))
        if (n_sites_contrib > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.sites.high.contrib"), n_sites_contrib))
        if (n_types_cos2 > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.types.low.cos2"), n_types_cos2))
        if (n_types_contrib > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.types.high.contrib"), n_types_contrib))
      }

    } else if (method == "jackknife") {
      summary_text <- sprintf(tr("ca.outlier.sum.jackknife.header"), dim_label)

      n_sites <- if (!is.null(candidates$sites_influential)) nrow(candidates$sites_influential) else 0
      n_types <- if (!is.null(candidates$types_influential)) nrow(candidates$types_influential) else 0
      total <- n_sites + n_types

      if (total == 0) {
        summary_text <- paste0(summary_text, tr("ca.outlier.sum.no.structural"))
      } else {
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.total.structural"), total))
        if (n_sites > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.sites"), n_sites))
        if (n_types > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.types"), n_types))
      }

    } else if (method == "combined") {
      summary_text <- sprintf(tr("ca.outlier.sum.combined.header"), dim_label)

      n_sites_critical <- if (!is.null(candidates$sites$critical)) nrow(candidates$sites$critical) else 0
      n_sites_marginal <- if (!is.null(candidates$sites$marginal)) nrow(candidates$sites$marginal) else 0
      n_sites_structural <- if (!is.null(candidates$sites$structural)) nrow(candidates$sites$structural) else 0

      n_types_critical <- if (!is.null(candidates$types$critical)) nrow(candidates$types$critical) else 0
      n_types_marginal <- if (!is.null(candidates$types$marginal)) nrow(candidates$types$marginal) else 0
      n_types_structural <- if (!is.null(candidates$types$structural)) nrow(candidates$types$structural) else 0

      total <- n_sites_critical + n_sites_marginal + n_sites_structural +
        n_types_critical + n_types_marginal + n_types_structural

      if (total == 0) {
        summary_text <- paste0(summary_text, tr("ca.outlier.sum.no.outliers"))
      } else {
        summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.total.combined"), total))

        if (n_sites_critical > 0 || n_types_critical > 0) {
          summary_text <- paste0(summary_text, tr("ca.outlier.sum.critical"))
          if (n_sites_critical > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.sites"), n_sites_critical))
          if (n_types_critical > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.types"), n_types_critical))
          summary_text <- paste0(summary_text, "\n")
        }

        if (n_sites_marginal > 0 || n_types_marginal > 0) {
          summary_text <- paste0(summary_text, tr("ca.outlier.sum.marginal"))
          if (n_sites_marginal > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.sites"), n_sites_marginal))
          if (n_types_marginal > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.types"), n_types_marginal))
          summary_text <- paste0(summary_text, "\n")
        }

        if (n_sites_structural > 0 || n_types_structural > 0) {
          summary_text <- paste0(summary_text, tr("ca.outlier.sum.structural"))
          if (n_sites_structural > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.sites"), n_sites_structural))
          if (n_types_structural > 0) summary_text <- paste0(summary_text, sprintf(tr("ca.outlier.sum.types"), n_types_structural))
        }
      }
    }
    
    summary_text
  })
  
  # Outlier table for UI → REPLACED BY checkbox list
  # Lookup table for outliers (since entity names can contain special characters)
  outlier_lookup <- reactiveVal(list())
  
  output$ca_outlier_checkboxes <- renderUI({
    # If not yet calculated
    if (input$outlier_recalculate == 0) {
      return(div(style="text-align: center; padding: 40px; color: #6c757d;",
                 tags$b(tr("ca.outlier.cb.no.detection")), br(), br(),
                 tr("ca.outlier.cb.click.detect")))
    }

    # Get results
    candidates <- outlier_candidates()
    req(candidates)

    method <- candidates$method %||% "quantile"
    filter_mode <- input$outlier_filter %||% "all"

    # ===== ROBUST OUTLIER CHECKBOXES =====
    if (method == "robust") {
      sites_df <- candidates$sites
      types_df <- candidates$types

      checkbox_list <- list()
      lookup <- list()

      # Count filtered outliers BEFORE creating
      n_sites_filtered <- if (!is.null(sites_df)) sum(sites_df$Count >= 2) else 0
      n_types_filtered <- if (!is.null(types_df)) sum(types_df$Count >= 2) else 0
      n_total_filtered <- 0
      if (filter_mode == "sites") n_total_filtered <- n_sites_filtered
      else if (filter_mode == "types") n_total_filtered <- n_types_filtered
      else n_total_filtered <- n_sites_filtered + n_types_filtered

      # Header with count and buttons
      header <- div(style="margin-bottom: 15px;",
        div(style="display: flex; justify-content: space-between; align-items: center;",
          div(style="font-weight: bold; color: #495057;",
            sprintf(tr("ca.outlier.cb.robust.count"), n_total_filtered)
          ),
          div(style="display: flex; gap: 5px;",
            actionButton("ca_select_all_outliers_js",
                        tr("ca.btn.select.all"),
                        class = "btn btn-sm btn-outline-success",
                        style = "padding: 2px 10px; font-size: 0.85em;",
                        onclick = "$('input[id^=\"outlier_\"]').prop('checked', true).trigger('change');"),
            actionButton("ca_deselect_all_outliers_js",
                        tr("ca.btn.deselect.all"),
                        class = "btn btn-sm btn-outline-secondary",
                        style = "padding: 2px 10px; font-size: 0.85em;",
                        onclick = "$('input[id^=\"outlier_\"]').prop('checked', false).trigger('change');")
          )
        ),
        tags$hr(style="margin: 8px 0;")
      )
      
      # Sites
      if (!is.null(sites_df) && nrow(sites_df) > 0 && filter_mode %in% c("sites", "all")) {
        # ONLY show outliers with Count >= 2 (in at least 2 dimensions)
        sites_filtered <- sites_df[sites_df$Count >= 2, ]
        
        if (nrow(sites_filtered) > 0) {
          for (i in 1:nrow(sites_filtered)) {
            entity <- sites_filtered$Entity[i]
            count <- sites_filtered$Count[i]
            dims <- sites_filtered$Dimensions[i]
            
            # Color coding + reason
            color <- if (count == 4) "🔴" else if (count == 3) "🟠" else "🟡"
            reason <- if (count == 4) {
              tr("ca.outlier.cb.reason.critical")
            } else if (count == 3) {
              tr("ca.outlier.cb.reason.very.problematic")
            } else {
              tr("ca.outlier.cb.reason.dimension.specific")
            }

            checkbox_id <- paste0("outlier_cb_", i)
            label <- sprintf("%s %s (%d/4: %s) | %s", color, entity, count, dims, reason)

            checkbox_list[[checkbox_id]] <- checkboxInput(
              checkbox_id,
              label,
              value = FALSE
            )
            lookup[[checkbox_id]] <- entity
          }
        }
      }

      # Types
      if (!is.null(types_df) && nrow(types_df) > 0 && filter_mode %in% c("types", "all")) {
        # Only show outliers with Count >= 2
        types_filtered <- types_df[types_df$Count >= 2, ]

        if (nrow(types_filtered) > 0) {
          start_idx <- length(checkbox_list) + 1
          for (i in 1:nrow(types_filtered)) {
            entity <- types_filtered$Entity[i]
            count <- types_filtered$Count[i]
            dims <- types_filtered$Dimensions[i]

            # Color coding + reason
            color <- if (count == 4) "🔴" else if (count == 3) "🟠" else "🟡"
            reason <- if (count == 4) {
              tr("ca.outlier.cb.reason.critical")
            } else if (count == 3) {
              tr("ca.outlier.cb.reason.very.problematic")
            } else {
              tr("ca.outlier.cb.reason.dimension.specific")
            }
            
            checkbox_id <- paste0("outlier_cb_", start_idx + i - 1)
            label <- sprintf("%s %s (%d/4: %s) | %s", color, entity, count, dims, reason)
            
            checkbox_list[[checkbox_id]] <- checkboxInput(
              checkbox_id,
              label,
              value = FALSE
            )
            lookup[[checkbox_id]] <- entity
          }
        }
      }
      
      outlier_lookup(lookup)

      if (length(checkbox_list) == 0) {
        return(div(style="text-align: center; padding: 40px; color: #6c757d;",
                   tags$b(tr("ca.outlier.cb.no.robust")),
                   br(), br(),
                   tr("ca.outlier.cb.no.robust.desc")))
      }

      return(tagList(header, do.call(tagList, checkbox_list)))
    }

    # NORMAL OUTLIER CHECKBOXES
    candidates <- outlier_candidates()
    req(candidates)
    
    method <- candidates$method %||% "quantile"
    filter_type <- input$outlier_filter %||% "all"
    
    all_outliers <- data.frame()
    
    # QUANTILE METHOD
    if (method == "quantile") {
      if (!is.null(candidates$sites_low_cos2)) {
        df <- candidates$sites_low_cos2
        df$Type <- "Site"
        df$Metric <- "Cos²"
        df$Value <- round(df$Cos2, 4)
        df$Reason <- tr("ca.outlier.cb.reason.low.cos2")
        df$Cos2 <- NULL
        all_outliers <- rbind(all_outliers, df)
      }

      if (!is.null(candidates$sites_high_contrib)) {
        df <- candidates$sites_high_contrib
        df$Type <- "Site"
        df$Metric <- "Contribution"
        df$Value <- round(df$Contribution, 2)
        df$Reason <- tr("ca.outlier.cb.reason.too.dominant")
        df$Contribution <- NULL
        all_outliers <- rbind(all_outliers, df)
      }

      if (!is.null(candidates$types_low_cos2)) {
        df <- candidates$types_low_cos2
        df$Type <- "Type"
        df$Metric <- "Cos²"
        df$Value <- round(df$Cos2, 4)
        df$Reason <- tr("ca.outlier.cb.reason.low.cos2")
        df$Cos2 <- NULL
        all_outliers <- rbind(all_outliers, df)
      }

      if (!is.null(candidates$types_high_contrib)) {
        df <- candidates$types_high_contrib
        df$Type <- "Type"
        df$Metric <- "Contribution"
        df$Value <- round(df$Contribution, 2)
        df$Reason <- tr("ca.outlier.cb.reason.too.dominant")
        df$Contribution <- NULL
        all_outliers <- rbind(all_outliers, df)
      }
    }

    # JACKKNIFE METHOD
    else if (method == "jackknife") {
      if (!is.null(candidates$sites_influential)) {
        df <- candidates$sites_influential
        df$Type <- "Site"
        df$Metric <- "Influence"
        df$Value <- round(df$Influence, 4)
        df$Reason <- tr("ca.outlier.cb.reason.structural")
        df$Influence <- NULL
        all_outliers <- rbind(all_outliers, df)
      }

      if (!is.null(candidates$types_influential)) {
        df <- candidates$types_influential
        df$Type <- "Type"
        df$Metric <- "Influence"
        df$Value <- round(df$Influence, 4)
        df$Reason <- tr("ca.outlier.cb.reason.structural")
        df$Influence <- NULL
        all_outliers <- rbind(all_outliers, df)
      }
    }

    # COMBINED METHOD (4 CATEGORIES)
    else if (method == "combined") {
      if (!is.null(candidates$sites$critical)) {
        df <- candidates$sites$critical
        df$Type <- "Site"
        df$Metric <- "CRITICAL"
        df$Value <- NA
        all_outliers <- rbind(all_outliers, df)
      }
      if (!is.null(candidates$sites$marginal)) {
        df <- candidates$sites$marginal
        df$Type <- "Site"
        df$Metric <- "Marginal"
        df$Value <- NA
        all_outliers <- rbind(all_outliers, df)
      }
      if (!is.null(candidates$sites$structural)) {
        df <- candidates$sites$structural
        df$Type <- "Site"
        df$Metric <- "Structural"
        df$Value <- NA
        all_outliers <- rbind(all_outliers, df)
      }
      
      if (!is.null(candidates$types$critical)) {
        df <- candidates$types$critical
        df$Type <- "Type"
        df$Metric <- "CRITICAL"
        df$Value <- NA
        all_outliers <- rbind(all_outliers, df)
      }
      if (!is.null(candidates$types$marginal)) {
        df <- candidates$types$marginal
        df$Type <- "Type"
        df$Metric <- "Marginal"
        df$Value <- NA
        all_outliers <- rbind(all_outliers, df)
      }
      if (!is.null(candidates$types$structural)) {
        df <- candidates$types$structural
        df$Type <- "Type"
        df$Metric <- "Structural"
        df$Value <- NA
        all_outliers <- rbind(all_outliers, df)
      }
    }
    
    # Filter
    if (filter_type == "sites") {
      all_outliers <- all_outliers[all_outliers$Type == "Site", ]
    } else if (filter_type == "types") {
      all_outliers <- all_outliers[all_outliers$Type == "Type", ]
    }
    
    if (nrow(all_outliers) == 0) {
      return(div(style="text-align: center; padding: 20px; color: #6c757d;",
                 tags$b(tr("ca.outlier.cb.no.found"))))
    }

    # Sort (CRITICAL first)
    category_order <- c("CRITICAL", "Marginal", "Structural", "Cos²", "Contribution", "Influence")
    all_outliers$sort_key <- match(all_outliers$Metric, category_order)
    all_outliers <- all_outliers[order(all_outliers$sort_key, all_outliers$Type), ]
    all_outliers$sort_key <- NULL

    checkbox_items <- lapply(1:nrow(all_outliers), function(i) {
      entity <- all_outliers$Entity[i]
      type <- all_outliers$Type[i]
      metric <- all_outliers$Metric[i]
      reason <- all_outliers$Reason[i]
      value <- all_outliers$Value[i]

      # Traffic light icons based on severity
      ampel <- if (metric == "CRITICAL") "🔴" else
        if (metric == "Structural") "🟠" else
          if (metric == "Marginal") "🟡" else
            if (metric == "Cos²") "🟡" else  # Low Cos² = Marginal
              if (metric == "Contribution") "🟠" else  # High Contribution = Structural
                if (metric == "Influence") "🟠" else "⚪"  # High Influence = Structural

      icon <- if (type == "Site") "🏛️" else "🏺"

      color <- if (metric == "CRITICAL") "#e74c3c" else
        if (metric == "Marginal") "#f39c12" else
          if (metric == "Structural") "#e67e22" else
            if (metric == "Cos²") "#c0392b" else "#7f8c8d"
      
      entity_display <- if (nchar(entity) > 50) paste0(substr(entity, 1, 47), "...") else entity
      
      label_text <- if (!is.na(value)) {
        sprintf("%s %s %s | %s: %s | %s", ampel, icon, entity_display, metric, 
                if (metric %in% c("Cos²", "Influence")) sprintf("%.4f", value) else sprintf("%.2f%%", value),
                reason)
      } else {
        sprintf("%s %s %s | %s | %s", ampel, icon, entity_display, metric, reason)
      }
      
      item_style <- paste0(
        "margin-bottom: 8px; padding: 5px; background: white; ",
        "border-left: 3px solid ", color, "; border-radius: 3px;"
      )
      
      div(style=item_style,
        checkboxInput(
          inputId = paste0("outlier_idx_", i),
          label = div(style="font-size: 0.9em;", HTML(label_text)),
          value = FALSE
        )
      )
    })
    
    # Store lookup - WITH correct checkbox IDs!
    lookup <- list()
    for (i in 1:nrow(all_outliers)) {
      checkbox_id <- paste0("outlier_idx_", i)
      lookup[[checkbox_id]] <- list(
        entity = all_outliers$Entity[i],
        type = all_outliers$Type[i]
      )
    }
    outlier_lookup(lookup)
    
    tagList(
      div(style="margin-bottom: 15px;",
        div(style="display: flex; justify-content: space-between; align-items: center;",
          div(style="font-weight: bold; color: #495057;",
            sprintf(tr("ca.outlier.cb.count"), nrow(all_outliers))
          ),
          div(style="display: flex; gap: 5px;",
            actionButton("ca_select_all_outliers_js",
                        tr("ca.btn.select.all"),
                        class = "btn btn-sm btn-outline-success",
                        style = "padding: 2px 10px; font-size: 0.85em;",
                        onclick = "$('input[id^=\"outlier_\"]').prop('checked', true).trigger('change');"),
            actionButton("ca_deselect_all_outliers_js",
                        tr("ca.btn.deselect.all"),
                        class = "btn btn-sm btn-outline-secondary",
                        style = "padding: 2px 10px; font-size: 0.85em;",
                        onclick = "$('input[id^=\"outlier_\"]').prop('checked', false).trigger('change');")
          )
        ),
        tags$hr(style="margin: 8px 0;")
      ),
      checkbox_items
    )
  })
  
  # Button: Reset all outlier exclusions
  observeEvent(input$ca_reset_outliers, {
    # Reset reactiveVal
    excluded_outliers(NULL)
    
    showNotification(
      tr("ca.outlier.restored"),
      type = "message", duration = 4
    )
  })
  
  # ===== ENDE OUTLIER-DETECTION =====
  
  # Return for other modules
  return(list(
    ca_result = ca_res,
    ca_plot_data = ca_plot_data,
    get_ca_scores = reactive({
      req(ca_res())
      res <- ca_res()
      # Extract sites (active and supplementary)
      sites_coords <- data.frame()
      
      # Active sites
      if (!is.null(res$row) && !is.null(res$row$coord)) {
        active_sites <- data.frame(
          Site = rownames(res$row$coord),
          Dim1 = res$row$coord[, 1],
          Dim2 = res$row$coord[, 2],
          stringsAsFactors = FALSE
        )
        sites_coords <- rbind(sites_coords, active_sites)
      }
      
      # Supplementary Sites
      if (!is.null(res$row.sup) && !is.null(res$row.sup$coord)) {
        suppl_sites <- data.frame(
          Site = rownames(res$row.sup$coord),
          Dim1 = res$row.sup$coord[, 1],
          Dim2 = res$row.sup$coord[, 2],
          stringsAsFactors = FALSE
        )
        sites_coords <- rbind(sites_coords, suppl_sites)
      }
      
      if (nrow(sites_coords) == 0) return(NULL)
      sites_coords
    })
  ))
}

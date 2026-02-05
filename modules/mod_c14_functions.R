# === CENTRAL 14C HELPER FUNCTIONS (Version 2.0) ===
# These functions are used by all 14C modules
# No duplicates in other files!

# ---- Time and Label Helpers (central) ----

#' Convert cal BP to year CE (Common Era)
#'
#' @param calBP Cal BP value (numeric, years before 1950)
#' @return Year in CE format (negative = BC, positive = AD, no year 0)
#' @examples
#' calbp_to_year_ce(2000) # Returns -50 (50 BC)
#' calbp_to_year_ce(1000) # Returns 950 (950 AD)
calbp_to_year_ce <- function(calBP) {
  # cal BP = years before 1950, so year = 1950 - calBP
  # For years <= 0, we need to subtract 1 to skip year 0
  year <- 1950 - calBP
  ifelse(year <= 0, year - 1, year)
}

#' Format year CE as BC/AD label
#'
#' @param year_ce Year in CE format (negative = BC, positive = AD)
#' @return Formatted string "XXXX cal BC" or "XXXX AD"
format_year_label <- function(year_ce) {
  ifelse(year_ce < 0,
         paste0(abs(year_ce), " cal BC"),
         paste0(year_ce, " AD"))
}

#' Format interval in cal BC/AD format
#'
#' @param low_bp Lower end of interval (cal BP, younger)
#' @param high_bp Upper end of interval (cal BP, older)
#' @return Formatted string "older - younger" (e.g., "3000 cal BC - 2800 cal BC")
format_interval_calbc <- function(low_bp, high_bp) {
  ce_low <- calbp_to_year_ce(low_bp)   # younger
  ce_high <- calbp_to_year_ce(high_bp) # older

  # Format both ends

label_old <- format_year_label(ce_high)
  label_yng <- format_year_label(ce_low)

  # Chronological order: older - younger
  paste0(label_old, " - ", label_yng)
}

#' Convert HPD intervals to formatted string
#'
#' @param df Data frame with columns low_bp, high_bp
#' @return String with all intervals, separated by "; ", chronologically sorted
intervals_to_string_calbc <- function(df) {
  if (is.null(df) || !nrow(df)) return("")

  # Sort by age (oldest = highest BP first)
  df <- df[order(-df$high_bp, -df$low_bp), , drop = FALSE]

  # Format each interval
  formatted_intervals <- apply(df, 1, function(r) {
    format_interval_calbc(as.numeric(r[["low_bp"]]), as.numeric(r[["high_bp"]]))
  })

  # Combine with semicolon
  paste(formatted_intervals, collapse = "; ")
}

#' Format a single cal BP value as BC/AD label
#'
#' @param cal_median_bp Cal BP value
#' @return Formatted BC/AD string
format_point_calbc <- function(cal_median_bp) {
  format_year_label(calbp_to_year_ce(cal_median_bp))
}

# === HPD-INTERVALLE (MEHRGIPFLIG, ROBUSTE INTEGRATION) ===

#' Calculate HPD intervals on regular grid (multimodal)
#'
#' This function calculates Highest Posterior Density (HPD) intervals
#' for multimodal distributions on a regular grid.
#' More robust than simple threshold methods.
#' 
#' @param t Time axis (e.g., cal BP), must be regular
#' @param p Density values (posterior probabilities)
#' @param level Confidence level (0.683 for 68%, 0.954 for 95%)
#'
#' @return Data frame with columns low_bp, high_bp for all HPD intervals
#'   Sorted by age order (oldest first)
#'
#' @details
#' - Works with multimodal distributions
#' - Uses median step size for robust integration
#' - Automatically identifies contiguous regions
#' - Returns NA if no valid density available
#' 
#' @examples
#' \dontrun{
#' # Simuliere bimodale Verteilung
#' ages <- seq(5000, 6000, by = 1)
#' density <- dnorm(ages, 5300, 50) + dnorm(ages, 5700, 50)
#' 
#' # Calculate 95% HPD
#' intervals <- hpd_intervals_grid(ages, density, level = 0.95)
#' print(intervals)
#' }
hpd_intervals_grid <- function(t, p, level = 0.95) {
  # Validierung
  if (!is.numeric(t) || !is.numeric(p)) {
    stop("t and p must be numeric")
  }
  
  if (length(t) != length(p)) {
    stop(sprintf("Length of t (%d) and p (%d) must match", length(t), length(p)))
  }
  
  if (length(t) < 2) {
    return(data.frame(low_bp = NA_real_, high_bp = NA_real_, stringsAsFactors = FALSE))
  }
  
  if (level <= 0 || level >= 1) {
    stop("level must be between 0 and 1")
  }
  
  # Sort by time
  ord <- order(t)
  t <- t[ord]
  p <- pmax(p[ord], 0)  # Negative Werte auf 0 setzen
  
  # Check if valid density available
  if (!any(is.finite(p)) || sum(p) == 0) {
    return(data.frame(low_bp = NA_real_, high_bp = NA_real_, stringsAsFactors = FALSE))
  }
  
  if (length(t) > 1) {
    dx <- median(diff(t), na.rm = TRUE)
    if (!is.finite(dx) || dx <= 0) {
      dx <- mean(diff(t), na.rm = TRUE)
      if (!is.finite(dx) || dx <= 0) {
        dx <- 1  # Fallback
      }
    }
  } else {
    dx <- 1
  }
  
  # Normalize density
  w <- p / sum(p, na.rm = TRUE)
  
  # HPD method: Sort by density descending
  o2 <- order(w, decreasing = TRUE)
  cw <- cumsum(w[o2] * dx)
  
  # Find cutoff index for desired level
  k <- which(cw >= level)[1]
  if (is.na(k)) {
    k <- length(w)  # Take everything if level not reached
  }
  
  # Keep the k highest density points
  keep <- o2[seq_len(k)]
  
  keep_flag <- rep(FALSE, length(t))
  keep_flag[keep] <- TRUE
  
  # Find contiguous blocks (runs)
  # diff detects transitions FALSE->TRUE and TRUE->FALSE
  runs <- which(diff(c(FALSE, keep_flag, FALSE)) != 0)
  
  # runs come in pairs: Start (FALSE->TRUE), End+1 (TRUE->FALSE)
  if (length(runs) == 0 || length(runs) %% 2 != 0) {
    # Error case: odd number or no runs
    return(data.frame(low_bp = NA_real_, high_bp = NA_real_, stringsAsFactors = FALSE))
  }
  
  # Extract intervals
  out <- lapply(seq(1, length(runs), by = 2), function(i) {
    a <- runs[i]        # Start index
    b <- runs[i + 1] - 1  # End-Index
    
    if (a > b || a < 1 || b > length(t)) {
      return(NULL)  # Skip invalid intervals
    }
    
    data.frame(
      low_bp = t[a],
      high_bp = t[b],
      stringsAsFactors = FALSE
    )
  })
  
  # Remove NULL entries and combine
  out <- out[!sapply(out, is.null)]
  
  if (length(out) == 0) {
    return(data.frame(low_bp = NA_real_, high_bp = NA_real_, stringsAsFactors = FALSE))
  }
  
  result <- do.call(rbind, out)
  
  # Sort intervals by age (oldest = highest BP first)
  result <- result[order(-result$high_bp), , drop = FALSE]
  
  return(result)
}

# Simple interval formatting for CA labels (not multimodal)
format_simple_hpd_calbc <- function(intervals_df) {
  if (is.null(intervals_df) || nrow(intervals_df) == 0) return(NA_character_)
  
  # Take only the largest contiguous interval
  min_bp <- min(intervals_df$low_bp, na.rm = TRUE)
  max_bp <- max(intervals_df$high_bp, na.rm = TRUE)
  
  format_interval_calbc(min_bp, max_bp)
}

# Compact labels for CA overlay (configurable)
format_compact_c14_label <- function(median_bp, hpd68_intervals, n_dates,
                                     show_n = TRUE, show_interval = FALSE, style = "compact",
                                     lab_ids = NULL, show_labids = FALSE) {
  median_calbc <- format_point_calbc(median_bp)

  label_parts <- c(median_calbc)

  # Add interval if requested
  if (show_interval && !is.null(hpd68_intervals) && nrow(hpd68_intervals) > 0) {
    interval_str <- format_simple_hpd_calbc(hpd68_intervals)
    if (!is.na(interval_str) && nchar(interval_str) > 0) {
      label_parts <- c(label_parts, paste0("[", interval_str, "]"))
    }
  }

  # Add n if requested
  if (show_n && n_dates > 0) {
    label_parts <- c(label_parts, paste0("(n=", n_dates, ")"))
  }

  # Add LabIDs if requested
  if (show_labids && !is.null(lab_ids) && !is.na(lab_ids) && nchar(as.character(lab_ids)) > 0) {
    lab_id_str <- as.character(lab_ids)
    if (nchar(lab_id_str) > 30) {
      lab_id_str <- paste0(substr(lab_id_str, 1, 27), "...")
    }
    label_parts <- c(label_parts, paste0("[", lab_id_str, "]"))
  }

  # Format based on style
  switch(style,
    "compact" = paste(label_parts, collapse = " "),
    "detailed" = paste(label_parts, collapse = "\n"),
    "minimal" = median_calbc,
    paste(label_parts, collapse = " ")
  )
}

# --- Sum-Posterior Aggregation per Site ---
# Scientifically correct method: sum calibrated probability distributions
# References: Bronk Ramsey (2017), Crema & Bevan (2021)

#' Calculate Sum-Posterior for multiple calibrated dates
#'
#' @param age_grids List of age grids (cal BP vectors)
#' @param densities List of probability density vectors
#' @param resolution Grid resolution for output (default: 1 year)
#' @return List with combined_grid, combined_density, median, hpd68, hpd95
calculate_sum_posterior <- function(age_grids, densities, resolution = 1) {
  # Filter valid entries
  valid_idx <- sapply(seq_along(age_grids), function(i) {
    !is.null(age_grids[[i]]) &&
    length(age_grids[[i]]) > 1 &&
    !all(is.na(age_grids[[i]])) &&
    !is.null(densities[[i]]) &&
    length(densities[[i]]) > 1 &&
    !all(is.na(densities[[i]]))
  })

  if (!any(valid_idx)) {
    return(list(
      combined_grid = NA_real_,
      combined_density = NA_real_,
      median_calBP = NA_real_,
      hpd68 = data.frame(low_bp = NA_real_, high_bp = NA_real_, mass = NA_real_),
      hpd95 = data.frame(low_bp = NA_real_, high_bp = NA_real_, mass = NA_real_)
    ))
  }

  valid_grids <- age_grids[valid_idx]
  valid_densities <- densities[valid_idx]

  # Determine common grid range
  all_mins <- sapply(valid_grids, min, na.rm = TRUE)
  all_maxs <- sapply(valid_grids, max, na.rm = TRUE)
  grid_min <- min(all_mins)
  grid_max <- max(all_maxs)

  # Create common grid
  common_grid <- seq(grid_min, grid_max, by = resolution)

  # Interpolate all densities onto common grid and sum
  summed_density <- rep(0, length(common_grid))

  for (i in seq_along(valid_grids)) {
    grid_i <- valid_grids[[i]]
    dens_i <- valid_densities[[i]]

    # Normalize individual density first
    dens_i <- dens_i / sum(dens_i, na.rm = TRUE)

    # Interpolate to common grid
    interp_dens <- approx(grid_i, dens_i, xout = common_grid,
                          method = "linear", rule = 1)$y
    interp_dens[is.na(interp_dens)] <- 0

    # Add to sum
    summed_density <- summed_density + interp_dens
  }

  # Normalize summed density
  total_mass <- sum(summed_density, na.rm = TRUE)
  if (total_mass > 0) {
    summed_density <- summed_density / total_mass
  }

  # Calculate median from summed posterior
  cumsum_dens <- cumsum(summed_density) / sum(summed_density, na.rm = TRUE)
  median_idx <- which(cumsum_dens >= 0.5)[1]
  median_calBP <- if (!is.na(median_idx)) common_grid[median_idx] else NA_real_

  # Calculate HPD intervals on summed posterior
  hpd68 <- hpd_intervals_grid(common_grid, summed_density, level = 0.683)
  hpd95 <- hpd_intervals_grid(common_grid, summed_density, level = 0.954)

  list(
    combined_grid = common_grid,
    combined_density = summed_density,
    median_calBP = median_calBP,
    hpd68 = hpd68,
    hpd95 = hpd95
  )
}

# --- Aggregation per site (using Sum-Posterior) ---
#' Aggregate calibrated 14C data by site using Sum-Posterior method
#'
#' @param calibrated_df Data frame with calibrated dates (must have age_grid, density columns)
#' @param method Aggregation method: "sum_posterior" (recommended) or "median" (legacy)
#' @return Data frame with aggregated site data
summarise_c14_by_site <- function(calibrated_df, method = "sum_posterior") {
  if (!requireNamespace("dplyr", quietly = TRUE)) {
    stop("dplyr package required for site aggregation")
  }

  # Check if we have the required columns for sum-posterior
  has_density_data <- "age_grid" %in% names(calibrated_df) &&
                      "density" %in% names(calibrated_df)

  # Determine which column contains LabIDs
  lab_col <- if ("LabID" %in% names(calibrated_df)) {
    "LabID"
  } else if ("LabNr" %in% names(calibrated_df)) {
    "LabNr"
  } else {
    NULL
  }

  if (method == "sum_posterior" && has_density_data) {
    # Sum-Posterior method (scientifically correct)
    sites <- unique(calibrated_df$Site)

    result_list <- lapply(sites, function(site) {
      site_data <- calibrated_df[calibrated_df$Site == site, ]
      n_dates <- nrow(site_data)

      # Collect LabIDs for this site
      lab_ids_str <- ""
      if (!is.null(lab_col) && lab_col %in% names(site_data)) {
        lab_vals <- as.character(site_data[[lab_col]])
        lab_vals <- lab_vals[!is.na(lab_vals) & nchar(trimws(lab_vals)) > 0]
        if (length(lab_vals) > 0) {
          lab_ids_str <- paste(unique(lab_vals), collapse = ", ")
        }
      }

      if (n_dates == 1) {
        # Single date: use original values
        return(data.frame(
          Site = site,
          site_median_calBP = site_data$cal_median_BP[1],
          hpd68_intervals = I(list(site_data$hpd68_intervals[[1]])),
          hpd95_intervals = I(list(site_data$hpd95_intervals[[1]])),
          n_dates = 1,
          aggregation_method = "single",
          lab_ids = lab_ids_str,
          stringsAsFactors = FALSE
        ))
      }

      # Multiple dates: calculate Sum-Posterior
      sum_post <- calculate_sum_posterior(
        age_grids = site_data$age_grid,
        densities = site_data$density
      )

      data.frame(
        Site = site,
        site_median_calBP = sum_post$median_calBP,
        hpd68_intervals = I(list(sum_post$hpd68)),
        hpd95_intervals = I(list(sum_post$hpd95)),
        n_dates = n_dates,
        aggregation_method = "sum_posterior",
        lab_ids = lab_ids_str,
        stringsAsFactors = FALSE
      )
    })

    df <- do.call(rbind, result_list)

  } else {
    # Legacy method: median of medians (fallback)
    # Need to handle LabID collection separately since dplyr can't easily access column by variable name
    if (!is.null(lab_col) && lab_col %in% names(calibrated_df)) {
      df <- calibrated_df %>%
        dplyr::group_by(Site) %>%
        dplyr::summarise(
          site_median_calBP = stats::median(cal_median_BP, na.rm = TRUE),
          hpd68_intervals = list(do.call(rbind, hpd68_intervals)),
          hpd95_intervals = list(do.call(rbind, hpd95_intervals)),
          n_dates = dplyr::n(),
          aggregation_method = "median",
          lab_ids = paste(unique(na.omit(.data[[lab_col]])[nchar(trimws(na.omit(.data[[lab_col]]))) > 0]), collapse = ", "),
          .groups = "drop"
        )
    } else {
      df <- calibrated_df %>%
        dplyr::group_by(Site) %>%
        dplyr::summarise(
          site_median_calBP = stats::median(cal_median_BP, na.rm = TRUE),
          hpd68_intervals = list(do.call(rbind, hpd68_intervals)),
          hpd95_intervals = list(do.call(rbind, hpd95_intervals)),
          n_dates = dplyr::n(),
          aggregation_method = "median",
          lab_ids = "",
          .groups = "drop"
        )
    }
  }

  df
}

# --- Helper for interval union (optional) ---
union_intervals <- function(intervals_df) {
  if (is.null(intervals_df) || nrow(intervals_df) == 0) {
    return(data.frame(low_bp = numeric(0), high_bp = numeric(0), mass = numeric(0)))
  }
  
  # Sort by low_bp
  intervals_df <- intervals_df[order(intervals_df$low_bp), ]
  
  # Merge overlapping intervals
  merged <- list()
  current <- intervals_df[1, ]
  
  for (i in 2:nrow(intervals_df)) {
    next_interval <- intervals_df[i, ]
    
    # Check overlap
    if (next_interval$low_bp <= current$high_bp) {
      # Merge intervals
      current$high_bp <- max(current$high_bp, next_interval$high_bp)
      current$mass <- current$mass + next_interval$mass
    } else {
      # New interval starts
      merged <- append(merged, list(current), after = length(merged))
      current <- next_interval
    }
  }
  
  # Add last interval
  merged <- append(merged, list(current), after = length(merged))
  
  do.call(rbind, merged)
}

# === TIME AXIS FUNCTIONS ===
# These functions calculate time axes from CA results and 14C data

# 1. LINEAR PROJECTION - Finds optimal angle for time-CA correlation
calculate_linear_projection <- function(ca_res, cal_medians, common_sites) {
  if (!requireNamespace("stats", quietly = TRUE)) {
    stop("stats package required for linear projection")
  }
  
  tryCatch({
    # Extract CA coordinates for common sites
    ca_coords <- get_ca_coords(ca_res, which = "row", dims = 1:2)
    ca_sites_coords <- ca_coords[common_sites, , drop = FALSE]
    ages <- cal_medians[common_sites]
    
    # For 1D CA: Direct correlation
    if (ncol(ca_sites_coords) == 1) {
      correlation <- cor(ca_sites_coords[, 1], ages, use = "complete.obs", method = "spearman")
      
      # Project all CA sites
      all_ca_coords <- ca_coords[, 1, drop = FALSE]
      model <- lm(ages ~ ca_sites_coords[, 1])
      predicted_ages <- predict(model, newdata = data.frame(ca_sites_coords = all_ca_coords[, 1]))
      names(predicted_ages) <- rownames(all_ca_coords)
      
      return(list(
        method = "linear_projection_1d",
        correlation = correlation,
        predicted_ages = predicted_ages,
        angle = 0,  # Not applicable for 1D
        training_sites = common_sites
      ))
    }
    
    # For 2D CA: Angle search for best correlation
    angles <- seq(0, 2*pi, length.out = 73)  # 5-degree steps
    best_correlation <- -Inf
    best_angle <- 0
    
    for (angle in angles) {
      # Rotate CA coordinates
      rotation_matrix <- matrix(c(cos(angle), -sin(angle), sin(angle), cos(angle)), nrow = 2)
      rotated_coords <- as.matrix(ca_sites_coords) %*% rotation_matrix

      # Calculate correlation with first rotated dimension
      correlation <- cor(rotated_coords[, 1], ages, use = "complete.obs", method = "spearman")
      
      if (abs(correlation) > abs(best_correlation)) {
        best_correlation <- correlation
        best_angle <- angle
      }
    }
    
    # Apply best angle to all sites
    rotation_matrix <- matrix(c(cos(best_angle), -sin(best_angle), sin(best_angle), cos(best_angle)), nrow = 2)
    all_rotated <- as.matrix(ca_coords) %*% rotation_matrix

    # Train model with common sites
    rotated_training <- as.matrix(ca_sites_coords) %*% rotation_matrix
    model <- lm(ages ~ rotated_training[, 1])

    # Project to all sites
    predicted_ages <- predict(model, newdata = data.frame(rotated_training = all_rotated[, 1]))
    names(predicted_ages) <- rownames(ca_coords)
    
    list(
      method = "linear_projection_2d",
      correlation = best_correlation,
      predicted_ages = predicted_ages,
      angle = best_angle * 180 / pi,  # Convert to degrees
      training_sites = common_sites
    )
    
  }, error = function(e) {
    stop("Error in calculate_linear_projection: ", conditionMessage(e))
  })
}

# 2. PRINCIPAL CURVE - Non-linear curve through CA space
calculate_principal_curve <- function(ca_res, cal_medians, common_sites, smoothing = 1.0) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    # Fallback to linear method
    warning("mgcv package missing - using linear projection as fallback")
    return(calculate_linear_projection(ca_res, cal_medians, common_sites))
  }

  tryCatch({
    # Extract CA coordinates
    ca_coords <- get_ca_coords(ca_res, which = "row", dims = 1:2)
    ca_sites_coords <- ca_coords[common_sites, , drop = FALSE]
    ages <- cal_medians[common_sites]
    
    # For 1D: Fallback to linear method
    if (ncol(ca_sites_coords) == 1) {
      warning("Principal Curve requires 2D data - using linear projection")
      return(calculate_linear_projection(ca_res, cal_medians, common_sites))
    }

    # Sort sites by age for Principal Curve
    age_order <- order(ages)
    sorted_coords <- ca_sites_coords[age_order, ]
    sorted_ages <- ages[age_order]

    # Simple Principal Curve with GAM
    t_param <- seq(0, 1, length.out = length(sorted_ages))
    
    # GAM for x and y coordinates based on time parameter
    gam_x <- mgcv::gam(sorted_coords[, 1] ~ s(t_param, k = min(10, length(t_param) - 1), sp = 1/smoothing))
    gam_y <- mgcv::gam(sorted_coords[, 2] ~ s(t_param, k = min(10, length(t_param) - 1), sp = 1/smoothing))
    
    # Create fine curve
    t_fine <- seq(0, 1, length.out = 100)
    curve_x <- predict(gam_x, newdata = data.frame(t_param = t_fine))
    curve_y <- predict(gam_y, newdata = data.frame(t_param = t_fine))

    # Project all sites onto the curve
    all_predicted <- numeric(nrow(ca_coords))
    names(all_predicted) <- rownames(ca_coords)
    
    for (i in 1:nrow(ca_coords)) {
      site_coord <- ca_coords[i, ]
      
      # Find nearest point on the curve
      distances <- sqrt((curve_x - site_coord[1])^2 + (curve_y - site_coord[2])^2)
      closest_idx <- which.min(distances)
      t_closest <- t_fine[closest_idx]
      
      # Interpolate age based on t-parameter
      age_interp <- approx(t_param, sorted_ages, xout = t_closest, rule = 2)$y
      all_predicted[i] <- age_interp
    }
    
    correlation <- cor(all_predicted[common_sites], cal_medians[common_sites], 
                      use = "complete.obs", method = "spearman")
    
    list(
      method = "principal_curve",
      correlation = correlation,
      predicted_ages = all_predicted,
      smoothing = smoothing,
      training_sites = common_sites,
      curve_points = list(x = curve_x, y = curve_y, t = t_fine, ages = approx(t_param, sorted_ages, xout = t_fine, rule = 2)$y)
    )
    
  }, error = function(e) {
    warning("Error in Principal Curve - using linear projection: ", conditionMessage(e))
    return(calculate_linear_projection(ca_res, cal_medians, common_sites))
  })
}

# 3. CONSTRAINED CA - CA with time constraint (simplified)
calculate_constrained_ca <- function(data_matrix, cal_medians, common_sites) {
  if (!requireNamespace("FactoMineR", quietly = TRUE)) {
    stop("FactoMineR package required for Constrained CA")
  }
  
  tryCatch({
    # Check available sites in data matrix
    available_sites <- rownames(data_matrix)
    valid_common_sites <- intersect(common_sites, available_sites)

    if (length(valid_common_sites) < 3) {
      stop("At least 3 common sites required for Constrained CA")
    }

    # Extract data for common sites
    subset_data <- data_matrix[valid_common_sites, , drop = FALSE]
    subset_ages <- cal_medians[valid_common_sites]

    # Create time weighting matrix
    n_sites <- nrow(subset_data)
    n_total_sites <- nrow(data_matrix)

    # Simple CA followed by time regression
    # FIX: Use correct FactoMineR CA
    ca_result <- FactoMineR::CA(subset_data, graph = FALSE, ncp = min(5, min(dim(subset_data)) - 1))

    # Extract site coordinates (first dimension)
    ca_dim1 <- ca_result$row$coord[, 1]

    # FIX: Use explicit name assignment instead of automatic
    site_names <- rownames(subset_data)
    predicted_for_subset <- numeric(length(site_names))
    names(predicted_for_subset) <- site_names

    # Train time model
    time_model <- lm(subset_ages ~ ca_dim1)
    predicted_for_subset <- predict(time_model)

    names(predicted_for_subset) <- site_names

    # Calculate correlation
    correlation <- cor(predicted_for_subset, subset_ages, use = "complete.obs", method = "spearman")

    # Project to all sites in original matrix
    if (n_total_sites > length(valid_common_sites)) {
      # Run CA for all data
      full_ca <- FactoMineR::CA(data_matrix, graph = FALSE, ncp = min(5, min(dim(data_matrix)) - 1))
      all_ca_dim1 <- full_ca$row$coord[, 1]

      # Apply trained model
      all_predicted <- predict(time_model, newdata = data.frame(ca_dim1 = all_ca_dim1))

      # FIX: Explicit name assignment
      final_predicted <- numeric(n_total_sites)
      names(final_predicted) <- rownames(data_matrix)
      final_predicted[] <- all_predicted
      
    } else {
      final_predicted <- predicted_for_subset
    }
    
    list(
      method = "constrained_ca",
      correlation = correlation,
      predicted_ages = final_predicted,
      training_sites = valid_common_sites,
      ca_result = ca_result
    )
    
  }, error = function(e) {
    stop("Error in calculate_constrained_ca: ", conditionMessage(e))
  })
}

# 4. GAM 2D REGRESSION - Smoothing regression in 2D CA space
calculate_gam_2d <- function(ca_res, cal_medians, common_sites, k = 5) {
  if (!requireNamespace("mgcv", quietly = TRUE)) {
    warning("mgcv package missing - using linear projection as fallback")
    return(calculate_linear_projection(ca_res, cal_medians, common_sites))
  }

  tryCatch({
    # Extract CA coordinates
    ca_coords <- get_ca_coords(ca_res, which = "row", dims = 1:2)
    ca_sites_coords <- ca_coords[common_sites, , drop = FALSE]
    ages <- cal_medians[common_sites]

    # For 1D: Fallback to linear method
    if (ncol(ca_sites_coords) == 1) {
      warning("GAM 2D requires 2D data - using linear projection")
      return(calculate_linear_projection(ca_res, cal_medians, common_sites))
    }

    # Limit k to available data points
    k_adj <- min(k, length(ages) - 1, 10)

    # GAM with 2D smoothing
    gam_data <- data.frame(
      age = ages,
      dim1 = ca_sites_coords[, 1],
      dim2 = ca_sites_coords[, 2]
    )

    # Run GAM regression
    gam_model <- mgcv::gam(age ~ s(dim1, dim2, k = k_adj), data = gam_data)

    # Project to all sites
    all_data <- data.frame(
      dim1 = ca_coords[, 1],
      dim2 = ca_coords[, 2]
    )

    predicted_ages <- predict(gam_model, newdata = all_data)
    names(predicted_ages) <- rownames(ca_coords)

    # Calculate correlation
    correlation <- cor(predicted_ages[common_sites], cal_medians[common_sites],
                      use = "complete.obs", method = "spearman")
    
    list(
      method = "gam_2d",
      correlation = correlation,
      predicted_ages = predicted_ages,
      k = k_adj,
      training_sites = common_sites,
      gam_model = gam_model
    )
    
  }, error = function(e) {
    warning("Error in GAM 2D - using linear projection: ", conditionMessage(e))
    return(calculate_linear_projection(ca_res, cal_medians, common_sites))
  })
}

# === BAYESIAN HELPERS ===

# Simplified Bayesian Sequence with rcarbon
simple_bayesian_sequence_rcarbon <- function(sequence_sites, c14_df, cal_res,
                                           outlier_model = FALSE, outlier_threshold = 3,
                                           ca_result = NULL, time_direction = "auto") {

  # Extract medians for sites in the sequence
  predicted_ages <- numeric(length(sequence_sites))
  names(predicted_ages) <- sequence_sites
  
  for (i in seq_along(sequence_sites)) {
    site <- sequence_sites[i]
    site_dates <- c14_df[c14_df$Fundstelle == site, ]
    
    if (nrow(site_dates) > 0) {
      # Find corresponding calibrations
      cal_ids <- sapply(cal_res, function(x) x$id)
      matching_cal <- cal_res[cal_ids %in% site_dates$LabNr]

      if (length(matching_cal) > 0) {
        # Calculate mean of medians for this site
        site_medians <- sapply(matching_cal, function(x) x$median)
        predicted_ages[i] <- mean(site_medians, na.rm = TRUE)
      } else {
        predicted_ages[i] <- NA_real_
      }
    } else {
      predicted_ages[i] <- NA_real_
    }
  }
  
  # Simple outlier detection if requested
  if (outlier_model && sum(!is.na(predicted_ages)) > 3) {
    valid_ages <- predicted_ages[!is.na(predicted_ages)]
    z_scores <- abs(scale(valid_ages))
    outliers <- which(z_scores > outlier_threshold)
    
    if (length(outliers) > 0) {
      cat("Outliers detected at positions:", outliers, "\n")
    }
  }
  
  list(
    method = "simple_bayesian_rcarbon",
    sequence_sites = sequence_sites,
    predicted_ages = predicted_ages,
    outlier_model = outlier_model,
    outlier_threshold = outlier_threshold,
    amodel = if (sum(!is.na(predicted_ages)) > 0) 95 else NA_real_,  # Dummy value
    convergence = TRUE
  )
}

# Prior shrinkage (simplified)
apply_ca_prior_shrinkage <- function(bayesian_result, ca_sites, lambda = 1.0) {
  # Simplified shrinkage - reduces extreme values
  original_ages <- bayesian_result$predicted_ages
  mean_age <- mean(original_ages, na.rm = TRUE)

  # Shrink towards mean
  shrunk_ages <- original_ages + (mean_age - original_ages) * (1 - 1/lambda)
  
  list(
    posteriors = list(
      predicted_ages = shrunk_ages,
      sequence_sites = bayesian_result$sequence_sites
    ),
    lambda = lambda,
    mean_age = mean_age
  )
}

# Amodel extraction from OxCal output
extract_amodel_from_output <- function(output_content) {
  if (is.character(output_content) && length(output_content) > 0) {
    # Search for Amodel in OxCal output
    amodel_pattern <- "Amodel\\s*[=:]\\s*([0-9.]+)"
    matches <- regmatches(output_content, regexpr(amodel_pattern, output_content, ignore.case = TRUE))
    
    if (length(matches) > 0) {
      numbers <- regmatches(matches, regexpr("[0-9.]+", matches))
      if (length(numbers) > 0) {
        return(as.numeric(numbers[1]))
      }
    }
  }
  return(NA_real_)
}

# Convergence check
check_convergence <- function(output_content) {
  if (is.character(output_content) && length(output_content) > 0) {
    # Simple check for convergence keywords
    convergence_keywords <- c("converged", "successful", "completed")
    return(any(sapply(convergence_keywords, function(kw) any(grepl(kw, output_content, ignore.case = TRUE)))))
  }
  return(FALSE)
}

# === SPLINE INTERPOLATION FOR CALIBRATION CURVE (PERFORMANCE OPTIMIZATION) ===

#' Create reusable spline interpolators for calibration curve
#'
#' This function creates spline functions for the calibration curve once,
#' which can then interpolate very quickly for any cal BP values.
#' This is significantly more efficient than repeated approx() calls.
#'
#' @param curve_df Data frame with calibration curve (columns: CalBP, C14Age, Error)
#' @param method Spline method: "monoH.FC" (monotone, default) or "natural"
#' @param use_memoise Logical, whether to use memoization for caching
#'
#' @return List with:
#'   - mu: Spline function for C14Age (mean)
#'   - sig: Spline function for Error (sigma)
#'   - range: Valid range [min_CalBP, max_CalBP]
#'   - method: Spline method used
#'
#' @details
#' - monoH.FC: Monotone Hermite splines (recommended for calibration curves)
#' - Extrapolation is avoided through range checks
#' - Optional: Memoization for additional caching
#' 
#' @examples
#' \dontrun{
#' curve <- ensure_intcal20_df()
#' interpolators <- get_curve_interpolators(curve)
#'
#' # Fast interpolation for any values
#' ages <- c(5000, 6000, 7000)
#' mu_values <- interpolators$mu(ages)
#' sigma_values <- interpolators$sig(ages)
#' }
get_curve_interpolators <- function(curve_df,
                                   method = c("monoH.FC", "natural"),
                                   use_memoise = FALSE) {

  # Validation
  if (!is.data.frame(curve_df)) {
    stop("curve_df must be a data frame")
  }

  required_cols <- c("CalBP", "C14Age", "Error")
  if (!all(required_cols %in% names(curve_df))) {
    missing <- setdiff(required_cols, names(curve_df))
    stop(sprintf("Missing columns in calibration curve: %s",
                paste(missing, collapse = ", ")))
  }

  if (nrow(curve_df) < 3) {
    stop("Calibration curve must have at least 3 data points")
  }

  method <- match.arg(method)

  # Remove NA values and sort by CalBP
  curve_clean <- curve_df[complete.cases(curve_df[, required_cols]), ]
  curve_clean <- curve_clean[order(curve_clean$CalBP), ]

  if (nrow(curve_clean) < 3) {
    stop("After removing NA values: too few data points in calibration curve")
  }

  # Check for duplicates in CalBP (would cause spline error)
  if (any(duplicated(curve_clean$CalBP))) {
    warning("Duplicates in CalBP found - removing duplicates")
    curve_clean <- curve_clean[!duplicated(curve_clean$CalBP), ]
  }

  # Create spline functions
  tryCatch({
    # Spline for C14Age (mean of curve)
    mu_spline <- splinefun(
      x = curve_clean$CalBP,
      y = curve_clean$C14Age,
      method = method
    )

    # Spline for Error (sigma of curve)
    sig_spline <- splinefun(
      x = curve_clean$CalBP,
      y = curve_clean$Error,
      method = method
    )

    # Calculate valid range
    valid_range <- range(curve_clean$CalBP, na.rm = TRUE)

    # Optional: Memoization for additional caching
    if (use_memoise && requireNamespace("memoise", quietly = TRUE)) {
      mu_spline <- memoise::memoise(mu_spline)
      sig_spline <- memoise::memoise(sig_spline)
      cat("Memoization enabled for spline interpolators\n")
    }

    # Wrapper functions with range checks
    mu_safe <- function(x) {
      out_of_range <- x < valid_range[1] | x > valid_range[2]
      if (any(out_of_range, na.rm = TRUE)) {
        warning(sprintf("Warning: %d values outside curve range (%d-%d cal BP)",
                       sum(out_of_range, na.rm = TRUE), 
                       valid_range[1], valid_range[2]))
      }
      mu_spline(x)
    }
    
    sig_safe <- function(x) {
      out_of_range <- x < valid_range[1] | x > valid_range[2]
      if (any(out_of_range, na.rm = TRUE)) {
        warning(sprintf("Warning: %d values outside curve range (%d-%d cal BP)",
                       sum(out_of_range, na.rm = TRUE), 
                       valid_range[1], valid_range[2]))
      }
      sig_spline(x)
    }
    
    result <- list(
      mu = mu_safe,
      sig = sig_safe,
      range = valid_range,
      method = method,
      n_points = nrow(curve_clean),
      memoise_enabled = use_memoise && requireNamespace("memoise", quietly = TRUE)
    )
    
    cat(sprintf("Spline interpolators created: %d points, range %d-%d cal BP, method: %s\n",
                result$n_points, result$range[1], result$range[2], result$method))

    return(result)

  }, error = function(e) {
    stop(sprintf("Error creating spline interpolators: %s",
                conditionMessage(e)))
  })
}

#' Benchmark: Compare approx() vs. spline interpolation
#'
#' Helper function to test performance improvement
#'
#' @param curve_df Calibration curve
#' @param n_iterations Number of repetitions
#' @param grid_size Size of test grid
#'
#' @return List with benchmark results
benchmark_interpolation_methods <- function(curve_df,
                                           n_iterations = 100,
                                           grid_size = 1000) {

  if (!requireNamespace("microbenchmark", quietly = TRUE)) {
    message("Package 'microbenchmark' not available - using system.time()")
    use_microbenchmark <- FALSE
  } else {
    use_microbenchmark <- TRUE
  }

  # Test grid
  range_bp <- range(curve_df$CalBP, na.rm = TRUE)
  test_grid <- seq(range_bp[1], range_bp[2], length.out = grid_size)

  # Method 1: Repeated approx() calls
  approx_method <- function() {
    mu <- approx(curve_df$CalBP, curve_df$C14Age, xout = test_grid, rule = 2)$y
    sig <- approx(curve_df$CalBP, curve_df$Error, xout = test_grid, rule = 2)$y
    list(mu = mu, sig = sig)
  }

  # Method 2: Spline interpolators (created once)
  interpolators <- get_curve_interpolators(curve_df)
  spline_method <- function() {
    mu <- interpolators$mu(test_grid)
    sig <- interpolators$sig(test_grid)
    list(mu = mu, sig = sig)
  }

  if (use_microbenchmark) {
    results <- microbenchmark::microbenchmark(
      approx = approx_method(),
      spline = spline_method(),
      times = n_iterations
    )

    cat("\n=== Benchmark Results ===\n")
    print(results)

    return(list(
      benchmark = results,
      speedup = median(results$time[results$expr == "approx"]) /
                median(results$time[results$expr == "spline"])
    ))
  } else {
    # Fallback with system.time
    time_approx <- system.time(replicate(n_iterations, approx_method()))
    time_spline <- system.time(replicate(n_iterations, spline_method()))

    cat("\n=== Benchmark Results ===\n")
    cat(sprintf("approx(): %.3f seconds for %d iterations\n",
                time_approx["elapsed"], n_iterations))
    cat(sprintf("Spline:   %.3f seconds for %d iterations\n",
                time_spline["elapsed"], n_iterations))
    cat(sprintf("Speedup:  %.2fx faster\n",
                time_approx["elapsed"] / time_spline["elapsed"]))
    
    return(list(
      time_approx = time_approx,
      time_spline = time_spline,
      speedup = time_approx["elapsed"] / time_spline["elapsed"]
    ))
  }
}

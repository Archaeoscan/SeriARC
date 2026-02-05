# ======================= helper_robust_outliers.R =======================
# Robust outlier detection across all dimension combinations
# Finds outliers that are problematic in MULTIPLE dimensions
# @param res FactoMineR CA result
# @param mat Data matrix (Sites × Types)
# @param meta_data Metadata (sites, types)
# @param settings List with cos2_quantile, contrib_quantile
# @param identify_supplementary Function for supplementary identification
# @param calculate_jackknife_influence Function for Jackknife analysis
# @return List with $sites and $types DataFrames (Entity, Count, Dimensions, Why_Critical)

compute_robust_outliers <- function(res, mat, meta_data, settings, identify_supplementary, calculate_jackknife_influence) {
  
  cat("[ROBUST DETECTION] Starting for 4 dimension combinations...\n")
  cat(sprintf("  Dataset: %.0f sites, %.0f types\n", nrow(mat), ncol(mat)))

  # Estimated duration (Jackknife is slow)
  estimated_seconds <- nrow(mat) * 0.4  # Roughly 0.4 seconds per site
  cat(sprintf("  Estimated duration: ~%.0f seconds\n\n", estimated_seconds))

  # Extract settings
  cos2_quantile <- settings$cos2_quantile %||% 0.1
  contrib_quantile <- settings$contrib_quantile %||% 0.05
  
  # Fixed thresholds as additional safety
  cos2_threshold_fixed <- 0.3
  contrib_threshold_fixed <- 3.0
  
  # All 4 dimension combinations
  dim_combinations <- list(
    list(dims = c(1, 2), label = "Dim1+Dim2"),
    list(dims = c(1, 3), label = "Dim1+Dim3"),
    list(dims = c(2, 3), label = "Dim2+Dim3"),
    list(dims = c(1, 2, 3), label = "Dim1+Dim2+Dim3")
  )
  
  # ===== INITIALISIERE SAMMLER-LISTEN =====
  all_sites_outliers <- list()
  all_types_outliers <- list()
  all_influence_sites <- list()
  all_influence_types <- list()
  
  for (i in seq_along(dim_combinations)) {
    combo <- dim_combinations[[i]]
    dims <- combo$dims
    combo_name <- combo$label
    
    cat(sprintf("[%.0f/4] %s...\n", i, combo_name))
    
    # Check if dimensions exist
    if (max(dims) > ncol(res$row$cos2)) {
      cat("  → Dimensions not available, skipping\n\n")
      next
    }
    
    # ===== 1. LOW COS² OUTLIER =====
    sites_low_cos2 <- character(0)
    types_low_cos2 <- character(0)
    
    if (!is.null(res$row$cos2)) {
      sites_cos2 <- rowSums(res$row$cos2[, dims, drop = FALSE])
      cos2_threshold <- quantile(sites_cos2, probs = cos2_quantile, na.rm = TRUE)
      sites_low_cos2 <- names(sites_cos2[sites_cos2 < cos2_threshold & sites_cos2 < cos2_threshold_fixed])
    }
    
    if (!is.null(res$col$cos2)) {
      types_cos2 <- rowSums(res$col$cos2[, dims, drop = FALSE])
      cos2_threshold <- quantile(types_cos2, probs = cos2_quantile, na.rm = TRUE)
      types_low_cos2 <- names(types_cos2[types_cos2 < cos2_threshold & types_cos2 < cos2_threshold_fixed])
    }
    
    cat(sprintf("  Low Cos²: %.0f Sites, %.0f Types\n", length(sites_low_cos2), length(types_low_cos2)))
    
    # ===== 2. HIGH INFLUENCE OUTLIER (JACKKNIFE) =====
    sites_high_influence <- character(0)
    types_high_influence <- character(0)
    
    tryCatch({
      jk_result <- calculate_jackknife_influence(
        mat = mat,
        meta_data = meta_data,
        threshold = 0.1,  # Standard-Threshold
        n_dims = length(dims)
      )
      
      sites_high_influence <- jk_result$influential_sites %||% character(0)
      types_high_influence <- jk_result$influential_types %||% character(0)
      
      cat(sprintf("  High Influence: %.0f Sites, %.0f Types\n", length(sites_high_influence), length(types_high_influence)))
    }, error = function(e) {
      cat("  [WARNING] Jackknife failed:", e$message, "\n")
    })
    
    # ===== 3. COMBINE INTO OUTLIER LIST =====
    # Sites: All with low Cos² (regardless of high influence)
    combined_sites <- unique(sites_low_cos2)
    
    if (length(combined_sites) > 0) {
      all_sites_outliers[[combo_name]] <- list(
        entities = combined_sites,
        reasons = sapply(combined_sites, function(s) {
          if (s %in% sites_high_influence) "Both" else "Cos2"
        }, USE.NAMES = FALSE)
      )
      
      # Console output with categorization
      n_critical <- sum(combined_sites %in% sites_high_influence)
      cat(sprintf("  -> Low Cos2 outliers: %.0f sites, %.0f types\n", length(combined_sites), length(types_low_cos2)))
      if (n_critical > 0) {
        cat(sprintf("     (of which CRITICAL with high influence: %.0f sites, 0 types)\n", n_critical))
      }
    }

    # Types: All with low Cos2
    combined_types <- unique(types_low_cos2)

    if (length(combined_types) > 0) {
      all_types_outliers[[combo_name]] <- list(
        entities = combined_types,
        reasons = sapply(combined_types, function(t) {
          if (t %in% types_high_influence) "Both" else "Cos2"
        }, USE.NAMES = FALSE)
      )

      n_critical_types <- sum(combined_types %in% types_high_influence)
      if (n_critical_types > 0) {
        cat(sprintf("     (of which CRITICAL with high influence: 0 sites, %.0f types)\n", n_critical_types))
      }
    }

    # ===== 4. COLLECT "INFLUENCE ONLY" OUTLIERS =====
    # Sites with high influence WITHOUT low Cos2
    influence_only_sites <- setdiff(sites_high_influence, sites_low_cos2)
    if (length(influence_only_sites) > 0) {
      all_influence_sites[[combo_name]] <- list(
        entities = influence_only_sites,
        reasons = rep("Influence", length(influence_only_sites))
      )
    }
    
    # Types with high influence WITHOUT low Cos²
    influence_only_types <- setdiff(types_high_influence, types_low_cos2)
    if (length(influence_only_types) > 0) {
      all_influence_types[[combo_name]] <- list(
        entities = influence_only_types,
        reasons = rep("Influence", length(influence_only_types))
      )
    }
    
    cat("\n")
  }
  
  # ===== COUNT HOW OFTEN EACH OUTLIER APPEARS =====
  # ONLY NOW, after all lists are filled!
  
  all_sites_flat <- c(
    unlist(lapply(all_sites_outliers, function(x) x$entities)),
    unlist(lapply(all_influence_sites, function(x) x$entities))
  )
  
  all_types_flat <- c(
    unlist(lapply(all_types_outliers, function(x) x$entities)),
    unlist(lapply(all_influence_types, function(x) x$entities))
  )
  
  sites_counts <- table(all_sites_flat)
  types_counts <- table(all_types_flat)
  
  # ===== CREATE RESULT TABLES =====
  robust_sites <- NULL
  if (length(sites_counts) > 0) {
    robust_sites <- data.frame(
      Entity = names(sites_counts),
      Count = as.integer(sites_counts),
      Dimensions = sapply(names(sites_counts), function(site) {
        # Find all combinations where this site occurs
        all_combos <- c(names(all_sites_outliers), names(all_influence_sites))
        dims_found <- character(0)
        
        for (combo_name in all_combos) {
          if (combo_name %in% names(all_sites_outliers)) {
            if (site %in% all_sites_outliers[[combo_name]]$entities) {
              dims_found <- c(dims_found, combo_name)
            }
          }
          if (combo_name %in% names(all_influence_sites)) {
            if (site %in% all_influence_sites[[combo_name]]$entities) {
              dims_found <- c(dims_found, combo_name)
            }
          }
        }
        
        paste(unique(dims_found), collapse = ", ")
      }, USE.NAMES = FALSE),
      Why_Critical = sapply(names(sites_counts), function(site) {
        # Collect reasons from all combinations
        reasons <- character(0)
        
        # From all_sites_outliers
        for (combo_name in names(all_sites_outliers)) {
          combo <- all_sites_outliers[[combo_name]]
          if (site %in% combo$entities) {
            idx <- which(combo$entities == site)
            reasons <- c(reasons, combo$reasons[idx])
          }
        }
        
        # From all_influence_sites
        for (combo_name in names(all_influence_sites)) {
          combo <- all_influence_sites[[combo_name]]
          if (site %in% combo$entities) {
            idx <- which(combo$entities == site)
            reasons <- c(reasons, combo$reasons[idx])
          }
        }
        
        # Count categories
        cos2_only <- sum(reasons == "Cos2")
        both <- sum(reasons == "Both")
        influence_only <- sum(reasons == "Influence")

        # Categorize
        if (both > 0) {
          sprintf("CRITICAL: Cos2+Influence (%.0fx)", both)
        } else if (influence_only > 0 && cos2_only > 0) {
          sprintf("Mixed (%.0fx Cos2, %.0fx Influence)", cos2_only, influence_only)
        } else if (influence_only > 0) {
          sprintf("Structural: Influence only (%.0fx)", influence_only)
        } else {
          sprintf("Marginal: Cos2 only (%.0fx)", cos2_only)
        }
      }, USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )
    
    # Sort by count (descending)
    robust_sites <- robust_sites[order(-robust_sites$Count), ]
  }
  
  # SAME LOGIC FOR TYPES
  robust_types <- NULL
  if (length(types_counts) > 0) {
    robust_types <- data.frame(
      Entity = names(types_counts),
      Count = as.integer(types_counts),
      Dimensions = sapply(names(types_counts), function(type) {
        all_combos <- c(names(all_types_outliers), names(all_influence_types))
        dims_found <- character(0)
        
        for (combo_name in all_combos) {
          if (combo_name %in% names(all_types_outliers)) {
            if (type %in% all_types_outliers[[combo_name]]$entities) {
              dims_found <- c(dims_found, combo_name)
            }
          }
          if (combo_name %in% names(all_influence_types)) {
            if (type %in% all_influence_types[[combo_name]]$entities) {
              dims_found <- c(dims_found, combo_name)
            }
          }
        }
        
        paste(unique(dims_found), collapse = ", ")
      }, USE.NAMES = FALSE),
      Why_Critical = sapply(names(types_counts), function(type) {
        reasons <- character(0)
        
        for (combo_name in names(all_types_outliers)) {
          combo <- all_types_outliers[[combo_name]]
          if (type %in% combo$entities) {
            idx <- which(combo$entities == type)
            reasons <- c(reasons, combo$reasons[idx])
          }
        }
        
        for (combo_name in names(all_influence_types)) {
          combo <- all_influence_types[[combo_name]]
          if (type %in% combo$entities) {
            idx <- which(combo$entities == type)
            reasons <- c(reasons, combo$reasons[idx])
          }
        }
        
        cos2_only <- sum(reasons == "Cos2")
        both <- sum(reasons == "Both")
        influence_only <- sum(reasons == "Influence")

        if (both > 0) {
          sprintf("CRITICAL: Cos2+Influence (%.0fx)", both)
        } else if (influence_only > 0 && cos2_only > 0) {
          sprintf("Mixed (%.0fx Cos2, %.0fx Influence)", cos2_only, influence_only)
        } else if (influence_only > 0) {
          sprintf("Structural: Influence only (%.0fx)", influence_only)
        } else {
          sprintf("Marginal: Cos2 only (%.0fx)", cos2_only)
        }
      }, USE.NAMES = FALSE),
      stringsAsFactors = FALSE
    )

    robust_types <- robust_types[order(-robust_types$Count), ]
  }

  # ===== CONSOLE OUTPUT: SUMMARY =====
  cat("[ROBUST DETECTION] Complete!\n")

  if (!is.null(robust_sites) && nrow(robust_sites) > 0) {
    # Count categories (only >= 2 dimensions)
    sites_filtered <- robust_sites[robust_sites$Count >= 2, ]
    n_critical <- sum(grepl("CRITICAL", sites_filtered$Why_Critical))
    n_marginal <- sum(grepl("Marginal", sites_filtered$Why_Critical))
    n_structural <- sum(grepl("Structural", sites_filtered$Why_Critical))

    cat(sprintf("  -> %.0f sites with low Cos2 (in >=2 combinations):\n", nrow(sites_filtered)))
    cat(sprintf("      - %.0f CRITICAL (also high influence) -> distorters!\n", n_critical))
    cat(sprintf("      - %.0f marginal (Cos2 only) -> outside plane\n", n_marginal))
    if (n_structural > 0) {
      cat(sprintf("      - %.0f structural (influence only) -> CA distorters!\n", n_structural))
    }
  }

  if (!is.null(robust_types) && nrow(robust_types) > 0) {
    types_filtered <- robust_types[robust_types$Count >= 2, ]
    n_critical <- sum(grepl("CRITICAL", types_filtered$Why_Critical))
    n_marginal <- sum(grepl("Marginal", types_filtered$Why_Critical))
    n_structural <- sum(grepl("Structural", types_filtered$Why_Critical))
    
    cat(sprintf("  -> %.0f types with low Cos2 (in >=2 combinations):\n", nrow(types_filtered)))
    cat(sprintf("      - %.0f CRITICAL (also high influence) -> distorters!\n", n_critical))
    cat(sprintf("      - %.0f marginal (Cos2 only) -> outside plane\n", n_marginal))
    if (n_structural > 0) {
      cat(sprintf("      - %.0f structural (influence only) -> CA distorters!\n", n_structural))
    }
  }
  
  cat("\n")
  
  # Return
  list(
    method = "robust",
    sites = robust_sites,
    types = robust_types
  )
}

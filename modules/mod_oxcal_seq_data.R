# mod_oxcal_seq_data.R
# Data preparation and CQL generation for OxCal Sequence module
# Modularized from mod_oxcal_seq.R
# UPDATED: Charcoal correction + Simplified outlier modes

# Helper functions are already loaded in global.R

# ===================== CQL SANITIZATION =====================

# Escape special characters in OxCal strings
# OxCal generates JSON output, so names must not contain quotes that break JSON parsing
.sanitize_oxcal_string <- function(s) {
  if (is.null(s) || is.na(s)) return("")
  s <- as.character(s)
  # Remove ALL types of quotes (they break JSON parsing of OxCal output)
  s <- gsub('[\u201C\u201D\u201E\u201F]', '', s)  # " " „ ‟ -> remove

  s <- gsub('[\u2018\u2019\u201A\u201B]', '', s)  # ' ' ‚ ‛ -> remove
  s <- gsub('["\']', '', s)  # " and ' -> remove
  # Remove other problematic characters for OxCal/JSON
  s <- gsub('[;{}\\[\\]]', '_', s)
  s
}

# ===================== DATA PREPARATION =====================

# Base data without range filtering
get_oxcal_base_data <- function(c14_table_reactive, chrono_curve_reactive) {
  df <- c14_table_reactive()
  if (!is.data.frame(df) || nrow(df) == 0) {
    showNotification("No 14C data found.", type="warning", duration=5)
    return(NULL)
  }
  
  chrono <- chrono_curve_reactive()
  if (!is.data.frame(chrono) || !all(c("site_id","t_idx") %in% names(chrono))) {
    showNotification("Chronology Curve (site_id, t_idx) missing. Calculate first in 'Chronology Curve' tab.", type="warning", duration=5)
    return(NULL)
  }
  
  # Spalten-Mapping
  site_col <- age_col <- error_col <- material_col <- NA
  for(col in c("Site","site_id","site","SITE","Site_ID","Fundstelle","fundstelle","FUNDSTELLE","Fundort","fundort")) {
    if(col %in% names(df)) { site_col <- col; break }
  }
  for(col in c("Age","age","bp","BP","C14_Age","c14_age","Alter","alter","ALTER")) {
    if(col %in% names(df)) { age_col <- col; break }
  }
  for(col in c("Error","error","std","STD","Std","C14_Error","c14_error","Fehler","fehler","FEHLER","Standardabweichung")) {
    if(col %in% names(df)) { error_col <- col; break }
  }
  for(col in c("Material","material","MATERIAL","Probe","probe","PROBE","Sample_Type","sample_type","Probentyp","probentyp")) {
    if(col %in% names(df)) { material_col <- col; break }
  }

  # LabID/LabNr column detection
  labid_col <- NA
  for(col in c("LabNr","LabID","Lab_Nr","Lab_ID","labnr","labid","lab_nr","lab_id","LABNR","LABID",
               "Laboratory_Number","laboratory_number","Lab_Code","lab_code","Sample_ID","sample_id")) {
    if(col %in% names(df)) { labid_col <- col; break }
  }

  if (is.na(site_col) || is.na(age_col) || is.na(error_col)) {
    showNotification("Columns not recognized. Required: Site, Age, Error", type="warning", duration=8)
    return(NULL)
  }
  
  # Data cleaning
  # Safe outlier handling: NA becomes FALSE
  outlier_vals <- if ("Outlier" %in% names(df)) {
    out <- as.logical(df$Outlier)
    out[is.na(out)] <- FALSE
    out
  } else {
    rep(FALSE, nrow(df))
  }

  df_clean <- data.frame(
    site_id = as.character(df[[site_col]]),
    bp      = as.numeric(df[[age_col]]),
    std     = as.numeric(df[[error_col]]),
    material = if(!is.na(material_col)) as.character(df[[material_col]]) else NA_character_,
    lab_id = if(!is.na(labid_col)) as.character(df[[labid_col]]) else NA_character_,
    outlier = outlier_vals,
    stringsAsFactors = FALSE
  )
  
  has_material <- !is.na(material_col)
  
  if (has_material) {
    # Mark charcoal for later correction
    # Recognized terms: Holzkohle, Charcoal, HK, Kohle, Holz, Wood, etc.
    charcoal_detected <- grepl("holzkohle|charcoal|\\bhk\\b|kohle|charbon|carbón|wood|holz|charbon de bois|carbon|carbono", 
                                df_clean$material, ignore.case = TRUE)
    df_clean$is_charcoal <- charcoal_detected
  } else {
    df_clean$is_charcoal <- FALSE
  }
  
  df_clean$site_id <- trimws(df_clean$site_id)
  chrono$site_id   <- trimws(chrono$site_id)

  df_clean <- df_clean[!is.na(df_clean$site_id) & !is.na(df_clean$bp) & !is.na(df_clean$std), ]

  if (nrow(df_clean) == 0) {
    showNotification("No valid 14C data after cleaning.", type="warning", duration=5)
    return(NULL)
  }

  # Merge with chronology (both sides now sanitized)
  df_merged <- merge(df_clean, chrono[, c("site_id","t_idx")], by="site_id", all.x=FALSE, all.y=FALSE)
  if (nrow(df_merged) == 0) {
    showNotification("No match between 14C sites and chronology sites found.", type="warning", duration=8)
    return(NULL)
  }

  # Label-Mapping - sanitize for OxCal output
  lab_map <- .build_label_map(df_merged$site_id)
  # Sanitize site names for OxCal (remove quotes that break JSON)
  df_merged$phase_pretty <- vapply(df_merged$site_id, .sanitize_oxcal_string, character(1))
  df_merged$phase_safe   <- lab_map$safe[match(df_merged$site_id, lab_map$pretty)]
  attr(df_merged, "label_map") <- lab_map
  
  # OxCal REQUIRES sequences ordered from OLDER to YOUNGER (higher BP first)
  # NOTE: The Chronology Curve module already auto-detects the correct direction
  # based on C14 data and adjusts t_idx accordingly. We just sort by t_idx here.

  # Sort by t_idx (order from Chronology Curve - already direction-corrected)
  df_sorted <- df_merged[order(df_merged$t_idx), ]

  # Re-rank t_idx to be 1, 2, 3, ... (keeping the determined order)
  site_order <- unique(df_sorted$site_id)
  site_rank <- setNames(seq_along(site_order), site_order)
  df_sorted$t_idx <- site_rank[df_sorted$site_id]

  attr(df_sorted, "label_map") <- lab_map

  df_sorted[, c("site_id","bp","std","phase_pretty","phase_safe","t_idx","outlier","material","is_charcoal","lab_id")]
}

# Range-filtered data
get_oxcal_filtered_data <- function(base_data, from_input, to_input) {
  if (is.null(base_data) || nrow(base_data) == 0) return(NULL)
  
  if (is.null(from_input) || is.null(to_input) || from_input == "---" || to_input == "---") {
    return(base_data)
  }
  
  # Determine which phases are in the selected range
  phases_ordered <- unique(base_data[order(base_data$t_idx), c("phase_safe", "t_idx")])
  
  # Determine start and end t_idx based on input
  if (from_input == "Start") {
    from_t_idx <- min(phases_ordered$t_idx)
  } else {
    from_phase <- .extract_phase_from_boundary(from_input)
    from_idx <- which(phases_ordered$phase_safe == from_phase)
    from_t_idx <- if (length(from_idx) > 0) phases_ordered$t_idx[from_idx[1]] else min(phases_ordered$t_idx)
  }
  
  if (to_input == "End") {
    to_t_idx <- max(phases_ordered$t_idx)
  } else {
    to_phase <- .extract_phase_from_boundary(to_input)
    to_idx <- which(phases_ordered$phase_safe == to_phase)
    to_t_idx <- if (length(to_idx) > 0) phases_ordered$t_idx[to_idx[1]] else max(phases_ordered$t_idx)
  }
  
  # Ensure sorting (from older to younger)
  if (from_t_idx > to_t_idx) {
    temp <- from_t_idx
    from_t_idx <- to_t_idx
    to_t_idx <- temp
  }
  
  # Filter data to selected range
  original_count <- nrow(base_data)
  df_filtered <- base_data[base_data$t_idx >= from_t_idx & base_data$t_idx <= to_t_idx, ]
  
  if (nrow(df_filtered) == 0) {
    showNotification(paste("No C14 data in selected range:", from_input, "to", to_input), type="warning", duration=6)
    return(base_data)  # Fallback to all data
  }
  
  df_filtered
}

# ===================== CQL-GENERIERUNG =====================

# Standard CQL-Generierung
build_oxcal_cql <- function(filtered_data, settings, cache = NULL, mapping = NULL, cluster_names = NULL) {
  if (is.null(filtered_data) || nrow(filtered_data) == 0) {
    return("# No data available or invalid range")
  }
  
  # Extract settings
  auto_boundaries <- settings$autoBoundaries
  boundary_strategy <- settings$boundary_strategy %||% "shared"
  add_span_phase <- settings$addSpanPhase
  add_span_seq <- settings$addSpanSeq

  # === PHASE GROUPING (Sites/Cluster/Groups) ===
  phase_mode <- settings$phase_mode %||% "sites"

  if (phase_mode == "clusters") {
    # Cluster mode: Group by cluster
    if (is.null(cache) || is.null(cache$kmeans_result) || is.null(cache$kmeans_result$data)) {
      showNotification("Cluster data missing. Switching to Sites mode.", type="warning", duration=5)
      phase_mode <- "sites"
    } else {
      cluster_data <- cache$kmeans_result$data
      # Mapping Site -> Cluster (ensure vectors)
      site_to_cluster <- setNames(as.vector(cluster_data$cluster), as.character(cluster_data$label))
      # paste0 with vector safety
      cluster_ids <- site_to_cluster[filtered_data$site_id]
      
      # VERWENDE BENUTZERDEFINIERTE CLUSTER-NAMEN
      if (!is.null(cluster_names) && is.function(cluster_names)) {
        # cluster_names() returns named character vector
        custom_names <- tryCatch(cluster_names(), error = function(e) NULL)
        if (!is.null(custom_names) && is.character(custom_names)) {
          # SANITIZE custom cluster names (may contain quotes!)
          custom_names_safe <- vapply(custom_names, .sanitize_oxcal_string, character(1))
          names(custom_names_safe) <- names(custom_names)
          # Replace cluster IDs with sanitized custom names
          filtered_data$phase_group <- custom_names_safe[as.character(cluster_ids)]
          # Fallback for unmapped (should not happen)
          missing_mask <- is.na(filtered_data$phase_group)
          if (any(missing_mask)) {
            filtered_data$phase_group[missing_mask] <- paste0("Cluster_", cluster_ids[missing_mask])
          }
        } else {
          # Fallback: default names
          filtered_data$phase_group <- paste0("Cluster_", as.character(cluster_ids))
        }
      } else {
        # Fallback: default names
        filtered_data$phase_group <- paste0("Cluster_", as.character(cluster_ids))
      }
      # Fallback for unmapped sites (use sanitized phase_pretty)
      filtered_data$phase_group[is.na(filtered_data$phase_group)] <- as.character(filtered_data$phase_pretty[is.na(filtered_data$phase_group)])
    }
  } else if (phase_mode == "groups") {
    if (is.null(mapping) || !is.data.frame(mapping) || !("group" %in% names(mapping))) {
      showNotification("Group column missing. Switching to Sites mode.", type="warning", duration=5)
      phase_mode <- "sites"
    } else {
      site_col <- if ("site_id" %in% names(mapping)) mapping$site_id else mapping$site
      # SANITIZE group names (may contain quotes!)
      group_names_safe <- vapply(as.character(mapping$group), .sanitize_oxcal_string, character(1))
      site_to_group <- setNames(group_names_safe, as.character(site_col))
      filtered_data$phase_group <- site_to_group[filtered_data$site_id]
      # Fallback: Sites without group keep their name (use sanitized phase_pretty)
      filtered_data$phase_group[is.na(filtered_data$phase_group)] <- as.character(filtered_data$phase_pretty[is.na(filtered_data$phase_group)])
    }
  }
  
  if (phase_mode == "sites") {
    # Use phase_pretty (sanitized version of site_id)
    filtered_data$phase_group <- as.character(filtered_data$phase_pretty)
  }
  
  # Ensure phase_group and t_idx are vectors (not lists)
  # FINAL SANITIZATION: Remove any remaining problematic characters
  filtered_data$phase_group <- vapply(as.character(filtered_data$phase_group), .sanitize_oxcal_string, character(1))
  filtered_data$t_idx <- as.numeric(filtered_data$t_idx)
  
  # Aggregate t_idx per phase_group (mean of sites in group)
  phase_t_idx <- aggregate(t_idx ~ phase_group, data = filtered_data, FUN = mean)
  filtered_data <- merge(filtered_data, phase_t_idx, by = "phase_group", suffixes = c("", "_group"))
  filtered_data$t_idx <- as.numeric(filtered_data$t_idx_group)
  filtered_data$t_idx_group <- NULL
  
  # === INSERT EMPTY PHASES (for clusters/groups) ===
  if (phase_mode %in% c("clusters", "groups")) {
    dated_phases <- unique(filtered_data$phase_group)
    
    # Determine ALL possible phases - SORTED BY CA-POSITION (not number!)
    all_phases <- if (phase_mode == "clusters") {
      cluster_data <- cache$kmeans_result$data
      
      # Get cluster centroids from CA (Dimension 1 = chronological order)
      if (!is.null(cache$kmeans_result$centers)) {
        # Centers: Matrix with [Cluster, Dimension]
        centroids <- cache$kmeans_result$centers[, 1]  # Nur Dim1
        cluster_ids <- as.numeric(rownames(cache$kmeans_result$centers))
        if (is.null(cluster_ids) || any(is.na(cluster_ids))) {
          cluster_ids <- seq_len(nrow(cache$kmeans_result$centers))
        }
        
        # Sort clusters by centroid position (Dim1)
        if (length(dated_phases) >= 2) {
          # Works with both "Cluster_1" and custom names
          phase_to_cluster_id <- if (!is.null(cluster_names) && is.function(cluster_names)) {
            custom <- tryCatch(cluster_names(), error = function(e) NULL)
            if (!is.null(custom) && is.character(custom)) {
              # Invert: Name -> ID
              setNames(as.numeric(names(custom)), as.character(custom))
            } else {
              # Fallback: extract from "Cluster_X"
              setNames(as.numeric(sub("Cluster_", "", dated_phases)), dated_phases)
            }
          } else {
            setNames(as.numeric(sub("Cluster_", "", dated_phases)), dated_phases)
          }

          dated_cluster_nums <- phase_to_cluster_id[dated_phases]
          # Filter NA values
          valid_mask <- !is.na(dated_cluster_nums)

          if (sum(valid_mask) >= 2) {
            dated_centroids <- centroids[dated_cluster_nums[valid_mask]]
            dated_t_idx <- sapply(dated_phases[valid_mask], function(p) {
              mean(filtered_data$t_idx[filtered_data$phase_group == p], na.rm = TRUE)
            })

            # Pearson correlation: positive = same direction, negative = reversed
            # Safe calculation with error handling
            correlation <- tryCatch(
              cor(dated_centroids, dated_t_idx, use = "complete.obs"),
              error = function(e) NA_real_,
              warning = function(w) NA_real_
            )

            reverse_order <- if (!is.na(correlation)) correlation < 0 else FALSE
          } else {
            reverse_order <- FALSE
          }
        } else {
          reverse_order <- FALSE  # Fallback
        }
        
        # Sort by centroid
        sorted_idx <- order(centroids, decreasing = reverse_order)
        sorted_cluster_ids <- cluster_ids[sorted_idx]

        # Use custom names if available
        if (!is.null(cluster_names) && is.function(cluster_names)) {
          custom <- tryCatch(cluster_names(), error = function(e) NULL)
          if (!is.null(custom) && is.character(custom)) {
            # Map sorted IDs to custom names
            custom[as.character(sorted_cluster_ids)]
          } else {
            paste0("Cluster_", sorted_cluster_ids)
          }
        } else {
          paste0("Cluster_", sorted_cluster_ids)
        }
      } else {
        # Fallback: numeric, but with custom names
        sorted_ids <- sort(unique(cluster_data$cluster))
        if (!is.null(cluster_names) && is.function(cluster_names)) {
          custom <- tryCatch(cluster_names(), error = function(e) NULL)
          if (!is.null(custom) && is.character(custom)) {
            custom[as.character(sorted_ids)]
          } else {
            paste0("Cluster_", sorted_ids)
          }
        } else {
          paste0("Cluster_", sorted_ids)
        }
      }
    } else {
      sort(unique(mapping$meta$group[!is.na(mapping$meta$group)]))
    }
    
    # empty_phases: difference between all_phases and dated_phases
    empty_phases <- setdiff(all_phases, dated_phases)
    
    if (length(empty_phases) > 0) {
      # Interpolate t_idx for empty phases based on neighboring phases
      # Create mapping: phase_group -> mean t_idx
      phase_order_map <- setNames(
        sapply(all_phases, function(p) {
          if (p %in% dated_phases) {
            mean(filtered_data$t_idx[filtered_data$phase_group == p], na.rm = TRUE)
          } else {
            # Interpolate between neighbors
            phase_idx <- which(all_phases == p)
            before_phases <- all_phases[1:(phase_idx-1)]
            after_phases <- all_phases[(phase_idx+1):length(all_phases)]

            t_before_vals <- unlist(sapply(before_phases[before_phases %in% dated_phases], function(bp) {
              mean(filtered_data$t_idx[filtered_data$phase_group == bp], na.rm = TRUE)
            }))
            t_before <- if (length(t_before_vals) > 0) max(t_before_vals, na.rm = TRUE) else -Inf

            t_after_vals <- unlist(sapply(after_phases[after_phases %in% dated_phases], function(ap) {
              mean(filtered_data$t_idx[filtered_data$phase_group == ap], na.rm = TRUE)
            }))
            t_after <- if (length(t_after_vals) > 0) min(t_after_vals, na.rm = TRUE) else Inf

            # Mean between neighbors (or fallback)
            if (is.finite(t_before) && is.finite(t_after)) {
              (t_before + t_after) / 2
            } else if (is.finite(t_before)) {
              t_before + 0.1
            } else if (is.finite(t_after)) {
              t_after - 0.1
            } else {
              phase_idx  # Fallback: position in array
            }
          }
        }),
        all_phases
      )
      
      # Insert empty phases (with dummy data, recognized later)
      for (empty_phase in empty_phases) {
        empty_row <- data.frame(
          phase_group = empty_phase,
          site_id = empty_phase,
          bp = NA_real_,
          std = NA_real_,
          material = NA_character_,
          outlier = FALSE,
          is_charcoal = FALSE,
          phase_pretty = empty_phase,
          phase_safe = gsub("[^A-Za-z0-9_]", "_", empty_phase),
          t_idx = phase_order_map[[empty_phase]],
          stringsAsFactors = FALSE
        )
        filtered_data <- rbind(filtered_data, empty_row)
      }
      
    }
  }
  
  # NEUE OUTLIER-PARAMETER
  outlier_mode <- settings$outlier_mode %||% "none"  # "none", "general", "individual", "combined"
  outlier_prior <- (settings$outlier_prior %||% 5) / 100  # % -> Dezimal
  
  # HOLZKOHLE-KORREKTUR
  charcoal_correction <- settings$charcoal_correction %||% FALSE
  charcoal_offset <- settings$charcoal_offset %||% 40
  
  from_input <- settings$from
  to_input <- settings$to
  
  # Data is already correctly sorted
  df_ordered <- filtered_data[order(filtered_data$t_idx), ]

  # Use phase_group (can be site_id, Cluster_X or group name)
  phase_order <- unique(df_ordered$phase_group)
  by_phase <- split(df_ordered, df_ordered$phase_group, drop = TRUE)
  
  # Phase-Chunks erstellen
  phase_chunks <- .build_phase_chunks(phase_order, by_phase, outlier_mode, outlier_prior, add_span_phase, charcoal_correction, charcoal_offset)
  
  # Sequenz-Body zusammenbauen
  if (boundary_strategy == "shared") {
    seq_body <- .build_sequence_body_shared(phase_chunks, auto_boundaries, from_input, to_input)
  } else {
    seq_body <- .build_sequence_body_explicit(phase_chunks, auto_boundaries, from_input, to_input)
  }
  
  # Add queries
  queries <- .build_queries(add_span_seq, auto_boundaries, from_input, to_input, phase_order)
  
  outlier_header <- if (outlier_mode == "general" || outlier_mode == "combined") {
    sprintf('Outlier_Model("General",T(%d),U(0,4),"t");\n', round(outlier_prior * 100))
  } else {
    ""
  }
  
  # MCMC export DISABLED
  mcmc_export <- ""
  
  sprintf('Plot(){\n%sSequence("SeriArcSequence"){\n%s\n%s\n};\n%s};',
          outlier_header, seq_body, if (nzchar(queries)) queries else "", mcmc_export)
}

# ===================== CQL HELPER FUNCTIONS =====================

.build_phase_chunks <- function(phase_order, by_phase, outlier_mode, outlier_prior, add_span_phase, charcoal_correction = FALSE, charcoal_offset = 40) {
  lapply(phase_order, function(ph){
    sub <- by_phase[[ph]]

    # Sanitize phase name for OxCal (escape special chars like ")
    ph_safe <- .sanitize_oxcal_string(ph)

    # === CHECK IF PHASE IS EMPTY (no 14C data) ===
    is_empty <- all(is.na(sub$bp)) || nrow(sub) == 0

    if (is_empty) {
      # EMPTY PHASE: Phase container without dates
      content <- sprintf('Phase("%s") {\n  // No 14C data - interpolated by boundaries\n};', ph_safe)
      return(list(
        phase_name = ph,
        content = content
      ))
    }

    # === NORMAL PHASE WITH DATA ===
    # Remove NA rows (if mixed)
    sub <- sub[!is.na(sub$bp), ]

    # Build lab names: Use LabID if available, otherwise Site_N format
    # NOTE: Do NOT use ":" as separator - OxCal treats it specially and breaks JSON output!
    if ("lab_id" %in% names(sub) && any(!is.na(sub$lab_id) & nchar(trimws(sub$lab_id)) > 0)) {
      labs <- sapply(seq_len(nrow(sub)), function(i) {
        lid <- sub$lab_id[i]
        if (!is.na(lid) && nchar(trimws(lid)) > 0) {
          # Use Site_LabID format (sanitized, underscore separator)
          paste0(ph_safe, "_", .sanitize_oxcal_string(trimws(lid)))
        } else {
          # Fallback to Site_N
          paste0(ph_safe, "_", i)
        }
      })
    } else {
      labs <- paste0(ph_safe, "_", seq_len(nrow(sub)))
    }
    
    # HOLZKOHLE-KORREKTUR ANWENDEN (vor Outlier-Behandlung)
    bp_values <- sub$bp
    if (charcoal_correction && "is_charcoal" %in% names(sub)) {
      charcoal_mask <- !is.na(sub$is_charcoal) & sub$is_charcoal
      if (any(charcoal_mask)) {
        bp_values[charcoal_mask] <- bp_values[charcoal_mask] - charcoal_offset
      }
    }
    
    # OUTLIER HANDLING by mode
    if (outlier_mode == "none") {
      # No outlier test
      body <- oxcAAR::R_Date(labs, bp_values, sub$std)
      
    } else if (outlier_mode == "general") {
      # General Model: No individual wrappers (global Outlier_Model())
      body <- oxcAAR::R_Date(labs, bp_values, sub$std)
      
    } else if (outlier_mode == "individual") {
      # Individual: Only marked dates get Outlier(p)
      body <- character(nrow(sub))
      for (i in seq_len(nrow(sub))) {
        base_date <- oxcAAR::R_Date(labs[i], bp_values[i], sub$std[i])
        if (isTRUE(sub$outlier[i])) {
          base_date_clean <- sub(';\\s*$', '', base_date)
          body[i] <- sprintf('%s { Outlier(%.3f); };', base_date_clean, outlier_prior)
        } else {
          body[i] <- base_date
        }
      }
      
    } else if (outlier_mode == "combined") {
      # Combined: General Model + increased prior for marked
      body <- character(nrow(sub))
      for (i in seq_len(nrow(sub))) {
        base_date <- oxcAAR::R_Date(labs[i], bp_values[i], sub$std[i])
        if (isTRUE(sub$outlier[i])) {
          # 2× prior for marked (max 0.9)
          individual_prior <- min(outlier_prior * 2, 0.9)
          base_date_clean <- sub(';\\s*$', '', base_date)
          body[i] <- sprintf('%s { Outlier(%.3f); };', base_date_clean, individual_prior)
        } else {
          body[i] <- base_date
        }
      }
      
    } else {
      # Fallback
      body <- oxcAAR::R_Date(labs, bp_values, sub$std)
    }
    
    # Add span
    if (add_span_phase && nrow(sub) >= 1) {
      body <- c(body, sprintf('Span("Span_%s");', ph_safe))
    }

    body_str <- paste(body, collapse="\n")

    # Phase only, no boundaries (boundaries handled separately)
    list(
      phase_name = ph,  # Keep original for reference
      phase_name_safe = ph_safe,  # Sanitized for CQL
      content = paste0('Phase("', ph_safe, '"){\n', body_str, '\n};')
    )
  })
}

# Sharp transitions (shared strategy)
.build_sequence_body_shared <- function(phase_chunks, auto_boundaries, from_input, to_input) {
  if (!auto_boundaries) {
    phase_contents <- sapply(phase_chunks, function(chunk) chunk$content)
    return(paste(phase_contents, collapse = "\n"))
  }

  # Use sanitized phase names for boundaries
  phase_names_safe <- sapply(phase_chunks, function(chunk) {
    if (!is.null(chunk$phase_name_safe)) chunk$phase_name_safe else .sanitize_oxcal_string(chunk$phase_name)
  })
  phase_contents <- sapply(phase_chunks, function(chunk) chunk$content)

  if (length(phase_names_safe) == 0) {
    return("# No phases available")
  }

  seq_parts <- character(0)

  # Start boundary for first phase
  first_phase <- phase_names_safe[1]
  seq_parts <- c(seq_parts, sprintf('Boundary("%s_Start");', first_phase))

  # Phases with intermediate boundaries
  for (i in seq_along(phase_contents)) {
    seq_parts <- c(seq_parts, phase_contents[i])
    if (i < length(phase_contents)) {
      # Intermediate boundaries: use _to_ format
      boundary_name <- sprintf('Boundary("%s_to_%s");', phase_names_safe[i], phase_names_safe[i+1])
      seq_parts <- c(seq_parts, boundary_name)
    }
  }

  # End boundary for last phase
  last_phase <- phase_names_safe[length(phase_names_safe)]
  seq_parts <- c(seq_parts, sprintf('Boundary("%s_End");', last_phase))

  paste(seq_parts, collapse = "\n")
}

# Explicit hiatuses (explicit strategy)
.build_sequence_body_explicit <- function(phase_chunks, auto_boundaries, from_input, to_input) {
  if (!auto_boundaries) {
    phase_contents <- sapply(phase_chunks, function(chunk) chunk$content)
    return(paste(phase_contents, collapse = "\n"))
  }

  # Use sanitized phase names for boundaries
  phase_names_safe <- sapply(phase_chunks, function(chunk) {
    if (!is.null(chunk$phase_name_safe)) chunk$phase_name_safe else .sanitize_oxcal_string(chunk$phase_name)
  })
  phase_contents <- sapply(phase_chunks, function(chunk) chunk$content)

  seq_parts <- character(0)

  if (from_input == "Start" || is.null(from_input)) {
    seq_parts <- c(seq_parts, 'Boundary("Start");')
  }

  for (i in seq_along(phase_contents)) {
    ph_name <- phase_names_safe[i]
    seq_parts <- c(seq_parts,
                   sprintf('Boundary("%s_Start");', ph_name),
                   phase_contents[i],
                   sprintf('Boundary("%s_End");', ph_name))
  }

  if (to_input == "End" || is.null(to_input)) {
    seq_parts <- c(seq_parts, 'Boundary("End");')
  }

  paste(seq_parts, collapse = "\n")
}

.build_queries <- function(add_span_seq, auto_boundaries, from_input, to_input, phase_names = NULL) {
  queries <- character(0)
  
  if (add_span_seq) {
    queries <- 'Span("Span_Sequence");'
  }
  
  queries
}

# ===================== RANGE SELECTION UI HELPER FUNCTION =====================

generate_interval_choices <- function(base_data, boundaries_enabled) {
  if (is.null(base_data) || nrow(base_data) == 0) {
    return(list(
      choices_list = c("---" = "---"),
      default_from = "---",
      default_to = "---",
      help_text = "Please load 14C & Chronology data."
    ))
  }
  
  phases_ordered <- unique(base_data[order(base_data$t_idx), c("phase_pretty","phase_safe")])
  mk <- function(pretty, safe, suffix) {
    stats::setNames(paste0(safe, suffix), paste0(pretty, " ", sub("^_", " ", suffix)))
  }
  
  if (boundaries_enabled) {
    choices_list <- c(
      stats::setNames("Start", "Start"),
      mk(phases_ordered$phase_pretty, phases_ordered$phase_safe, "_Start"),
      mk(phases_ordered$phase_pretty, phases_ordered$phase_safe, "_End"),
      stats::setNames("End", "End")
    )
    default_from <- "Start"
    default_to <- "End"
    help_text <- "With boundaries: Selected range will be calculated (better performance)"
  } else {
    choices_list <- stats::setNames(phases_ordered$phase_safe, phases_ordered$phase_pretty)
    default_from <- if(nrow(phases_ordered) > 0) phases_ordered$phase_safe[1] else "---"
    default_to   <- if(nrow(phases_ordered) > 0) phases_ordered$phase_safe[nrow(phases_ordered)] else "---"
    help_text <- "Without boundaries: Phase selection only, limited range filtering"
  }
  
  list(
    choices_list = choices_list,
    default_from = default_from,
    default_to = default_to,
    help_text = help_text
  )
}

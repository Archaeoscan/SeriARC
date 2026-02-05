# mod_oxcal_seq_phases.R
# Phase analysis from OxCal JS output (without MCMC CSV)

# ===================== PHASE DETECTION FROM TIDY =====================
.recognize_phases_from_tidy <- function(tidy_df) {
  # Find boundary rows (WITHOUT "Boundary" in name - oxcAAR removes it!)
  # Pattern: _Start, _End, _to_ (transitions)
  boundary_rows <- grep("_Start$|_End$|_to_", tidy_df$name)

  # === FALLBACK: Without boundaries - detect phases from R_Date prefixes ===
  if (length(boundary_rows) == 0) {
    return(.recognize_phases_from_dates(tidy_df))
  }
  
  phases <- list()
  
  for (idx in boundary_rows) {
    boundary_name <- tidy_df$name[idx]
    
    # STRATEGY 1: Transitions (Phase1_to_Phase2)
    if (grepl("_to_", boundary_name)) {
      parts <- strsplit(boundary_name, "_to_")[[1]]
      
      if (length(parts) >= 2) {
        phase1 <- parts[1]
        phase2 <- parts[length(parts)]
        
        if (!phase1 %in% names(phases)) phases[[phase1]] <- list()
        if (!phase2 %in% names(phases)) phases[[phase2]] <- list()
        
        phases[[phase1]]$end_idx <- idx
        phases[[phase2]]$start_idx <- idx
      }
    }
    
    # STRATEGY 2: Explicit Start/End (WITHOUT "Boundary" at end!)
    if (grepl("_Start$", boundary_name)) {
      phase_name <- gsub("_Start$", "", boundary_name)
      if (nzchar(phase_name)) {
        if (!phase_name %in% names(phases)) phases[[phase_name]] <- list()
        phases[[phase_name]]$start_idx <- idx
      }
    }
    
    if (grepl("_End$", boundary_name)) {
      phase_name <- gsub("_End$", "", boundary_name)
      if (nzchar(phase_name)) {
        if (!phase_name %in% names(phases)) phases[[phase_name]] <- list()
        phases[[phase_name]]$end_idx <- idx
      }
    }
  }
  
  # Only complete phases + SEQUENCE ORDER
  valid <- list()
  phase_order <- character(0)  # Order from boundaries

  # Extract sequence from _to_ boundaries
  for (idx in boundary_rows) {
    boundary_name <- tidy_df$name[idx]
    if (grepl("_to_", boundary_name)) {
      parts <- strsplit(boundary_name, "_to_")[[1]]
      if (length(parts) >= 2) {
        phase1 <- parts[1]
        phase2 <- parts[length(parts)]
        # Build sequence
        if (!phase1 %in% phase_order) phase_order <- c(phase_order, phase1)
        if (!phase2 %in% phase_order) phase_order <- c(phase_order, phase2)
      }
    }
  }
  
  # Add phases in sequence order
  for (pn in phase_order) {
    if (pn %in% names(phases) && 
        !is.null(phases[[pn]]$start_idx) && 
        !is.null(phases[[pn]]$end_idx)) {
      valid[[pn]] <- phases[[pn]]
    }
  }
  
  # If no _to_ boundaries: fallback to alphabetical
  if (length(valid) == 0) {
    for (pn in names(phases)) {
      if (!is.null(phases[[pn]]$start_idx) && !is.null(phases[[pn]]$end_idx)) {
        valid[[pn]] <- phases[[pn]]
      }
    }
  }
  
  valid
}

# ===================== PHASE DETECTION WITHOUT BOUNDARIES =====================
# Detects phases from R_Date names (format: "PhaseName_1", "PhaseName_2", etc.)
.recognize_phases_from_dates <- function(tidy_df) {
  # Find all R_Date rows (typically have "_" followed by number)
  # Extract phase prefix from names like "PF_1", "R I_2", "A_1"

  phases <- list()

  for (idx in seq_len(nrow(tidy_df))) {
    name <- tidy_df$name[idx]

    # Skip Span, Sequence, etc.
    if (grepl("^Span|^Sequence|^Plot", name, ignore.case = TRUE)) next

    # Extract phase prefix: everything before last "_number"
    # Pattern: "PhaseName_123" → "PhaseName"
    if (grepl("_[0-9]+$", name)) {
      phase_name <- sub("_[0-9]+$", "", name)

      if (!phase_name %in% names(phases)) {
        phases[[phase_name]] <- list(date_indices = c(), medians = c())
      }

      phases[[phase_name]]$date_indices <- c(phases[[phase_name]]$date_indices, idx)
      phases[[phase_name]]$medians <- c(phases[[phase_name]]$medians, tidy_df$median[idx])
    }
  }

  if (length(phases) == 0) {
    return(list())
  }

  # For each phase: oldest date = start, youngest = end
  # (In BP: higher median = older, lower = younger)
  valid <- list()

  for (phase_name in names(phases)) {
    indices <- phases[[phase_name]]$date_indices
    medians <- phases[[phase_name]]$medians

    if (length(indices) >= 1) {
      # Oldest (highest median) = Start, Youngest (lowest median) = End
      start_idx <- indices[which.max(medians)]
      end_idx <- indices[which.min(medians)]

      valid[[phase_name]] <- list(
        start_idx = start_idx,
        end_idx = end_idx,
        date_indices = indices,
        is_from_dates = TRUE  # Marker für alternative Berechnung
      )
    }
  }

  # Sort phases by mean median (oldest first)
  phase_means <- sapply(names(valid), function(p) mean(phases[[p]]$medians))
  sorted_names <- names(valid)[order(phase_means, decreasing = TRUE)]

  valid_sorted <- list()
  for (pn in sorted_names) {
    valid_sorted[[pn]] <- valid[[pn]]
  }

  cat(sprintf("Phases without boundaries detected: %s\n", paste(sorted_names, collapse = " -> ")))

  valid_sorted
}

# ===================== STATISTICS FROM TIDY =====================
.calculate_phase_stats_from_tidy <- function(tidy_df, phases_info) {
  results <- list()

  for (phase_name in names(phases_info)) {
    start_idx <- phases_info[[phase_name]]$start_idx
    end_idx   <- phases_info[[phase_name]]$end_idx

    # Extract boundary data
    start_row <- tidy_df[start_idx, ]
    end_row   <- tidy_df[end_idx, ]

    # Median, 68% CI and 95% CI
    start_median <- start_row$median
    start_68_lower <- if(!is.null(start_row$from_68)) start_row$from_68 else NA
    start_68_upper <- if(!is.null(start_row$to_68)) start_row$to_68 else NA
    start_95_lower <- start_row$from_95
    start_95_upper <- start_row$to_95

    end_median <- end_row$median
    end_68_lower <- if(!is.null(end_row$from_68)) end_row$from_68 else NA
    end_68_upper <- if(!is.null(end_row$to_68)) end_row$to_68 else NA
    end_95_lower <- end_row$from_95
    end_95_upper <- end_row$to_95

    # Duration (point estimate)
    duration_median <- end_median - start_median
    # Conservative CI estimate
    duration_95_lower <- end_95_lower - start_95_upper
    duration_95_upper <- end_95_upper - start_95_lower

    # === INTERPOLATION DETECTION: Count R_Dates in this phase ===
    # A phase is interpolated if it has NO R_Date entries
    # R_Dates lie between Start-Boundary and End-Boundary
    n_dates <- .count_dates_in_phase(tidy_df, phase_name, start_idx, end_idx)
    is_interpolated <- (n_dates == 0)

    results[[phase_name]] <- data.frame(
      Phase = phase_name,
      Start_Median = start_median,
      Start_68CI_Lower = start_68_lower,
      Start_68CI_Upper = start_68_upper,
      Start_95CI_Lower = start_95_lower,
      Start_95CI_Upper = start_95_upper,
      End_Median = end_median,
      End_68CI_Lower = end_68_lower,
      End_68CI_Upper = end_68_upper,
      End_95CI_Lower = end_95_lower,
      End_95CI_Upper = end_95_upper,
      Duration_Median = duration_median,
      Duration_95CI_Lower = duration_95_lower,
      Duration_95CI_Upper = duration_95_upper,
      N_Dates = n_dates,
      Is_Interpolated = is_interpolated,
      stringsAsFactors = FALSE
    )
  }

  do.call(rbind, results)
}

# Helper function: Count R_Dates between start and end boundary of a phase
.count_dates_in_phase <- function(tidy_df, phase_name, start_idx, end_idx) {
  # Method 1: Search for R_Date entries with phase name as prefix
  # Pattern: "PhaseName_1", "PhaseName_2", etc.
  phase_pattern <- paste0("^", gsub("([\\[\\]\\(\\)\\{\\}\\^\\$\\.\\|\\*\\+\\?\\\\])", "\\\\\\1", phase_name), "_[0-9]+$")
  matching_dates <- grep(phase_pattern, tidy_df$name, value = TRUE)

  if (length(matching_dates) > 0) {
    return(length(matching_dates))
  }

  # Method 2: Count all entries between start and end that are NOT boundaries/spans
  if (!is.na(start_idx) && !is.na(end_idx) && start_idx < end_idx) {
    between_rows <- tidy_df[(start_idx + 1):(end_idx - 1), ]
    if (nrow(between_rows) > 0) {
      # Filter out boundaries, spans, sequences
      is_date <- !grepl("_Start$|_End$|_to_|^Span|^Sequence|Boundary", between_rows$name, ignore.case = TRUE)
      return(sum(is_date))
    }
  }

  return(0)
}

.calculate_overlaps_from_tidy <- function(tidy_df, phases_info) {
  phase_names <- names(phases_info)
  n_phases <- length(phase_names)

  if (n_phases < 2) {
    return(data.frame(Info = "Need at least 2 phases for overlaps"))
  }

  results <- list()
  idx <- 1

  for (i in 1:(n_phases - 1)) {
    for (j in (i + 1):n_phases) {
      phase1 <- phase_names[i]
      phase2 <- phase_names[j]

      # Extract medians
      p1_start_idx <- phases_info[[phase1]]$start_idx
      p1_end_idx   <- phases_info[[phase1]]$end_idx
      p2_start_idx <- phases_info[[phase2]]$start_idx
      p2_end_idx   <- phases_info[[phase2]]$end_idx

      p1_start <- tidy_df[p1_start_idx, "median"]
      p1_end   <- tidy_df[p1_end_idx, "median"]
      p2_start <- tidy_df[p2_start_idx, "median"]
      p2_end   <- tidy_df[p2_end_idx, "median"]

      # BP convention: higher value = older
      # Phase-Start = oldest date (highest median)
      # Phase-End = youngest date (lowest median)
      # Overlap exists if: Phase1-End > Phase2-Start (in BP: Phase1 ends before Phase2 starts)
      # Correction: In BP ">" = older, so we need to check if ranges intersect

      # Normalize: start = older (higher), end = younger (lower)
      p1_older <- max(p1_start, p1_end)
      p1_younger <- min(p1_start, p1_end)
      p2_older <- max(p2_start, p2_end)
      p2_younger <- min(p2_start, p2_end)

      # Overlap: The ranges [older, younger] intersect
      # Intersection exists if: max(older1, older2) >= min(younger1, younger2)
      # In BP: older > younger, so overlap when older_max <= younger_min is FALSE
      # Correct: Overlap if p1_younger <= p2_older AND p2_younger <= p1_older
      overlap_exists <- (p1_younger <= p2_older) & (p2_younger <= p1_older)

      overlap_duration <- if (overlap_exists) {
        # Overlap range: from max(younger) to min(older)
        abs(min(p1_older, p2_older) - max(p1_younger, p2_younger))
      } else {
        0
      }

      results[[idx]] <- data.frame(
        Phase1 = phase1,
        Phase2 = phase2,
        Overlaps = ifelse(overlap_exists, "Yes", "No"),
        Overlap_Duration = round(overlap_duration, 0),
        stringsAsFactors = FALSE
      )
      idx <- idx + 1
    }
  }
  
  do.call(rbind, results)
}

.calculate_gaps_from_tidy <- function(tidy_df, phases_info) {
  phase_names <- names(phases_info)
  n_phases <- length(phase_names)

  if (n_phases < 2) {
    return(data.frame(Info = "Need at least 2 phases for gaps"))
  }

  # BP convention: higher value = older
  # Sort phases by start median (oldest first = highest value first)
  phase_starts <- sapply(phase_names, function(p) {
    # For each phase: the older date (higher median)
    start_med <- tidy_df[phases_info[[p]]$start_idx, "median"]
    end_med <- tidy_df[phases_info[[p]]$end_idx, "median"]
    max(start_med, end_med)  # Älteres Datum
  })
  phase_order <- phase_names[order(phase_starts, decreasing = TRUE)]  # Oldest first

  results <- list()

  for (i in 1:(n_phases - 1)) {
    phase1 <- phase_order[i]      # Older phase
    phase2 <- phase_order[i + 1]  # Younger phase

    # Extract data for both phases
    p1_start_med <- tidy_df[phases_info[[phase1]]$start_idx, "median"]
    p1_end_med <- tidy_df[phases_info[[phase1]]$end_idx, "median"]
    p2_start_med <- tidy_df[phases_info[[phase2]]$start_idx, "median"]
    p2_end_med <- tidy_df[phases_info[[phase2]]$end_idx, "median"]

    # Phase1: older->younger = max->min (BP)
    p1_younger <- min(p1_start_med, p1_end_med)  # Youngest date in Phase1
    # Phase2: older->younger = max->min (BP)
    p2_older <- max(p2_start_med, p2_end_med)    # Oldest date in Phase2

    # Gap = difference between Phase1-End and Phase2-Start (in BP years)
    # Positive = gap (Phase1 ends before Phase2 starts)
    # Negative = overlap
    gap_median <- p1_younger - p2_older

    # 95% CI for gap (approximate)
    p1_end_row <- tidy_df[phases_info[[phase1]]$end_idx, ]
    p2_start_row <- tidy_df[phases_info[[phase2]]$start_idx, ]

    # Sichere Extraktion der CI-Werte
    p1_to_95 <- if ("to_95" %in% names(p1_end_row)) p1_end_row$to_95 else p1_younger
    p1_from_95 <- if ("from_95" %in% names(p1_end_row)) p1_end_row$from_95 else p1_younger
    p2_to_95 <- if ("to_95" %in% names(p2_start_row)) p2_start_row$to_95 else p2_older
    p2_from_95 <- if ("from_95" %in% names(p2_start_row)) p2_start_row$from_95 else p2_older

    gap_95_lower <- min(p1_to_95, p1_from_95) - max(p2_to_95, p2_from_95)
    gap_95_upper <- max(p1_to_95, p1_from_95) - min(p2_to_95, p2_from_95)

    results[[i]] <- data.frame(
      After_Phase = phase1,
      Before_Phase = phase2,
      Gap_Median = round(gap_median, 0),
      Gap_95CI_Lower = round(min(gap_95_lower, gap_95_upper), 0),
      Gap_95CI_Upper = round(max(gap_95_lower, gap_95_upper), 0),
      Interpretation = ifelse(gap_median > 0, "Gap", ifelse(gap_median < 0, "Overlap", "Adjacent")),
      stringsAsFactors = FALSE
    )
  }
  
  do.call(rbind, results)
}

# ===================== PLOT FUNCTION =====================
.plot_phase_timeline <- function(phase_stats, show_68ci = FALSE, show_95ci = TRUE) {
  if (!requireNamespace("plotly", quietly = TRUE)) {
    stop("Plotly required for phase plot")
  }

  # Convert to cal BC (negative -> positive)
  phase_stats$Start_Median_BC <- abs(phase_stats$Start_Median)
  phase_stats$Start_95CI_Lower_BC <- abs(phase_stats$Start_95CI_Lower)
  phase_stats$Start_95CI_Upper_BC <- abs(phase_stats$Start_95CI_Upper)
  phase_stats$Start_68CI_Lower_BC <- abs(phase_stats$Start_68CI_Lower)
  phase_stats$Start_68CI_Upper_BC <- abs(phase_stats$Start_68CI_Upper)
  
  phase_stats$End_Median_BC <- abs(phase_stats$End_Median)
  phase_stats$End_95CI_Lower_BC <- abs(phase_stats$End_95CI_Lower)
  phase_stats$End_95CI_Upper_BC <- abs(phase_stats$End_95CI_Upper)
  phase_stats$End_68CI_Lower_BC <- abs(phase_stats$End_68CI_Lower)
  phase_stats$End_68CI_Upper_BC <- abs(phase_stats$End_68CI_Upper)
  
  # Keep sequence order (oldest on top) - do NOT sort by median!
  # Order already comes from .recognize_phases_from_tidy()
  phase_stats$Phase <- factor(phase_stats$Phase, levels = rev(phase_stats$Phase))

  # Create plot
  p <- plotly::plot_ly()

  # Iterate over phases for individual opacity
  for (i in seq_len(nrow(phase_stats))) {
    phase_row <- phase_stats[i, ]
    is_interpolated <- isTRUE(phase_row$Is_Interpolated)

    # Interpolated phases: dashed, transparent, with warning symbol
    line_opacity <- if(is_interpolated) 0.4 else 1.0
    marker_opacity <- if(is_interpolated) 0.5 else 1.0
    line_dash <- if(is_interpolated) "dash" else "solid"
    phase_label <- if(is_interpolated) paste0(phase_row$Phase, " (interpolated)") else as.character(phase_row$Phase)
    
    # 95% CI Balken (optional)
    if (show_95ci) {
      # 95% CI Balken (Start)
      p <- plotly::add_segments(p,
        x = phase_row$Start_95CI_Lower_BC,
        xend = phase_row$Start_95CI_Upper_BC,
        y = phase_row$Phase,
        yend = phase_row$Phase,
        line = list(color = "#3498DB", width = 6, dash = line_dash),
        opacity = line_opacity,
        name = "Start 95% CI",
        showlegend = (i == 1),  # Nur einmal in Legende
        legendgroup = "start_95",
        hovertemplate = paste0(
          "<b>", phase_label, "</b><br>",
          "Start: ", round(phase_row$Start_95CI_Lower_BC), "-",
          round(phase_row$Start_95CI_Upper_BC), " cal BC<br>",
          "95% CI Start<extra></extra>"
        )
      )

      # 95% CI Balken (End)
      p <- plotly::add_segments(p,
        x = phase_row$End_95CI_Lower_BC,
        xend = phase_row$End_95CI_Upper_BC,
        y = phase_row$Phase,
        yend = phase_row$Phase,
        line = list(color = "#E74C3C", width = 6, dash = line_dash),
        opacity = line_opacity,
        name = "End 95% CI",
        showlegend = (i == 1),
        legendgroup = "end_95",
        hovertemplate = paste0(
          "<b>", phase_label, "</b><br>",
          "End: ", round(phase_row$End_95CI_Lower_BC), "-",
          round(phase_row$End_95CI_Upper_BC), " cal BC<br>",
          "95% CI End<extra></extra>"
        )
      )
    }
    
    # 68% CI Balken (optional)
    if (show_68ci) {
      # 68% CI Balken (Start)
      p <- plotly::add_segments(p,
        x = phase_row$Start_68CI_Lower_BC,
        xend = phase_row$Start_68CI_Upper_BC,
        y = phase_row$Phase,
        yend = phase_row$Phase,
        line = list(color = "#5DADE2", width = 10, opacity = line_opacity),
        name = "Start 68% CI",
        showlegend = (i == 1),
        legendgroup = "start_68",
        hovertemplate = paste0(
          "<b>", phase_row$Phase, "</b><br>",
          "Start: ", round(phase_row$Start_68CI_Lower_BC), "-",
          round(phase_row$Start_68CI_Upper_BC), " cal BC<br>",
          "68% CI Start<extra></extra>"
        )
      )
      
      # 68% CI Balken (End)
      p <- plotly::add_segments(p,
        x = phase_row$End_68CI_Lower_BC,
        xend = phase_row$End_68CI_Upper_BC,
        y = phase_row$Phase,
        yend = phase_row$Phase,
        line = list(color = "#EC7063", width = 10, opacity = line_opacity),
        name = "End 68% CI",
        showlegend = (i == 1),
        legendgroup = "end_68",
        hovertemplate = paste0(
          "<b>", phase_row$Phase, "</b><br>",
          "End: ", round(phase_row$End_68CI_Lower_BC), "-",
          round(phase_row$End_68CI_Upper_BC), " cal BC<br>",
          "68% CI End<extra></extra>"
        )
      )
    }
    
    # Median marker (Start) - triangle pointing right (▶ = Play/Start)
    # Interpolated: open symbol, Normal: filled symbol
    start_symbol <- if(is_interpolated) "triangle-right-open" else "triangle-right"
    p <- plotly::add_markers(p,
      x = phase_row$Start_Median_BC,
      y = phase_row$Phase,
      marker = list(color = "#2C3E50", size = 10, symbol = start_symbol, opacity = marker_opacity),
      name = "Start Median",
      showlegend = (i == 1),
      legendgroup = "start_median",
      hovertemplate = paste0(
        "<b>", phase_label, "</b><br>",
        "Start Median: ", round(phase_row$Start_Median_BC), " cal BC<extra></extra>"
      )
    )

    # Median marker (End) - square (■ = Stop/End)
    # Interpolated: open symbol, Normal: filled symbol
    end_symbol <- if(is_interpolated) "square-open" else "square"
    p <- plotly::add_markers(p,
      x = phase_row$End_Median_BC,
      y = phase_row$Phase,
      marker = list(color = "#34495E", size = 10, symbol = end_symbol, opacity = marker_opacity),
      name = "End Median",
      showlegend = (i == 1),
      legendgroup = "end_median",
      hovertemplate = paste0(
        "<b>", phase_label, "</b><br>",
        "End Median: ", round(phase_row$End_Median_BC), " cal BC<extra></extra>"
      )
    )
  }
  
  # Layout
  p <- plotly::layout(p,
    title = "Phase Timeline (Oldest on top, Start left)",
    xaxis = list(
      title = "Calibrated Age (cal BC)",
      autorange = "reversed"  # Older (higher BC) left, younger right
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 12)
    ),
    hovermode = "closest",
    showlegend = TRUE,
    legend = list(x = 1.05, y = 1)
  )
  
  p
}

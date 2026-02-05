# server_status_outputs.R - Status and debug outputs
# Extracted from server.R for better maintainability

# TRANSPARENCY UI OUTPUTS

# Parameter status panel
output$parameter_status_panel <- renderUI({
  req(filtermod$parameter_status())
  status <- filtermod$parameter_status()
  
  tagList(
    h4(paste0("📊 ", tr("status.param.title"))),
    div(class = "alert alert-info",
      p(sprintf(paste0("✅ ", tr("status.param.sites")),
               status$active_sites, status$suppl_sites, status$mean_weight_sites)),
      p(sprintf(paste0("✅ ", tr("status.param.types")),
               status$active_types, status$suppl_types, status$mean_weight_types)),
      p(sprintf(paste0("🎯 ", tr("status.param.matrix")),
               status$matrix_rows, status$matrix_cols, status$weighted_points, status$weighted_sum))
    )
  )
})

# Weights overview
output$weights_overview <- DT::renderDataTable({
  req(filtermod$weights_table())
  weights_df <- filtermod$weights_table()
  
  DT::datatable(
    weights_df,
    options = list(
      pageLength = 10, 
      scrollX = TRUE,
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover',
    caption = tr("status.weights.caption")
  ) %>%
    DT::formatStyle(
      'Status',
      backgroundColor = DT::styleEqual(
        c('Aktiv', 'Supplementary'),
        c('#d4edda', '#fff3cd')
      )
    ) %>%
    DT::formatRound('Gewicht', digits = 2)
})

# Matrix transformation comparison
output$matrix_transformation <- renderText({
  req(filtermod$matrix_comparison())
  comp <- filtermod$matrix_comparison()
  
  sprintf(
    "📋 %s:\n\nOriginal: %d Sites × %d Types (%d %s, %s: %.1f)\n→ Selected: %d Sites × %d Types\n→ %s: %s = %.1f (%+.1f%%)\n→ Supplementary: %d %s\n✅ Final: %d %s",
    tr("status.transform.title"),
    comp$orig_sites, comp$orig_types, comp$orig_entries, tr("status.transform.entries"), tr("status.transform.sum"), comp$orig_sum,
    comp$final_sites, comp$final_types,
    tr("status.transform.weighted"), tr("status.transform.sum"), comp$final_sum, comp$weight_impact,
    comp$suppl_count, tr("status.transform.suppl.projected"),
    comp$final_points, tr("status.transform.final")
  )
})

# UI STATUS FLAGS
output$utm_detected <- reactive({
  !is.null(mapping_coord_info()) && !is.na(mapping_coord_info()$type) && identical(mapping_coord_info()$type, "utm")
})
outputOptions(output, "utm_detected", suspendWhenHidden = FALSE)

output$import_detection_msg <- renderUI({ 
  import$detection_status() 
})

output$file_uploaded <- reactive({
  tryCatch({
    df <- data_raw()
    return(!is.null(df) && nrow(df) > 0)
  }, error = function(e) {
    return(FALSE)
  })
})

# COORDINATE STATUS TEXT - ROBUST WITH ERROR HANDLING
output$coord_status_text <- renderText({
  tryCatch({
    info <- mapping_coord_info()
    mm <- mapping_meta()
    if (is.null(mm)) return(paste0("🔄 ", tr("coords.loading")))

    # Safe coordinate evaluation
    mm$lon <- suppressWarnings(as.numeric(mm$lon))
    mm$lat <- suppressWarnings(as.numeric(mm$lat))
    valid <- sum(!is.na(mm$lon) & !is.na(mm$lat) & is.finite(mm$lon) & is.finite(mm$lat))
    total <- nrow(mm)

    if (total == 0) return(paste0("📍 ", tr("coords.none.available")))

    if (is.null(info) || is.na(info$type)) {
      if (valid > 0) {
        sprintf(paste0("✅ ", tr("coords.valid.no.type")), valid, total)
      } else {
        paste0("❌ ", tr("coords.none.found"))
      }
    } else {
      ord <- if (!is.null(info$order) && !is.na(info$order)) paste0(" (", info$order, ")") else ""
      if (valid > 0) {
        sprintf(paste0("✅ ", tr("coords.valid.type")), valid, total, info$type, ord, valid/total*100)
      } else {
        sprintf(paste0("❌ ", tr("coords.type.none")), info$type, ord)
      }
    }
  }, error = function(e) {
    paste0("⚠️ ", tr("coords.status.error"), ": ", e$message)
  })
})

# TYPEMAP COORDINATE STATUS TEXT - duplicate for separate UI element
output$typemap_coord_status_text <- renderText({
  tryCatch({
    info <- mapping_coord_info()
    mm <- mapping_meta()
    if (is.null(mm)) return(paste0("🔄 ", tr("coords.loading")))

    # Safe coordinate evaluation
    mm$lon <- suppressWarnings(as.numeric(mm$lon))
    mm$lat <- suppressWarnings(as.numeric(mm$lat))
    valid <- sum(!is.na(mm$lon) & !is.na(mm$lat) & is.finite(mm$lon) & is.finite(mm$lat))
    total <- nrow(mm)

    if (total == 0) return(paste0("📍 ", tr("coords.none.available")))

    if (is.null(info) || is.na(info$type)) {
      if (valid > 0) {
        sprintf(paste0("✅ ", tr("coords.valid.no.type")), valid, total)
      } else {
        paste0("❌ ", tr("coords.none.found"))
      }
    } else {
      ord <- if (!is.null(info$order) && !is.na(info$order)) paste0(" (", info$order, ")") else ""
      if (valid > 0) {
        sprintf(paste0("✅ ", tr("coords.valid.type")), valid, total, info$type, ord, valid/total*100)
      } else {
        sprintf(paste0("❌ ", tr("coords.type.none")), info$type, ord)
      }
    }
  }, error = function(e) {
    paste0("⚠️ ", tr("coords.status.error"), ": ", e$message)
  })
})

outputOptions(output, 'file_uploaded', suspendWhenHidden = FALSE)

output$kmeans_available <- reactive({
  km_result <- kmeans_module$kmeans_result()
  return(!is.null(km_result) && !is.null(km_result$data))
})
outputOptions(output, 'kmeans_available', suspendWhenHidden = FALSE)

output$seriation_available <- reactive({
  return(!is.null(cache$seriation_result))
})
outputOptions(output, 'seriation_available', suspendWhenHidden = FALSE)

output$bootstrap_available <- reactive({
  return(!is.null(cache$bootstrap_result))
})
outputOptions(output, 'bootstrap_available', suspendWhenHidden = FALSE)

output$debug_file_status <- renderText({
  if (!is.null(file_status)) {
    file_status()
  } else {
    tr("status.none.available")
  }
})

output$data_quality <- renderText({
  req(data_raw())
  
  df <- data_raw()
  numeric_matrix <- as.matrix(df[, -1, drop = FALSE])
  
  total_cells <- length(numeric_matrix)
  na_count <- sum(is.na(numeric_matrix))
  zero_count <- sum(numeric_matrix == 0, na.rm = TRUE)
  positive_count <- sum(numeric_matrix > 0, na.rm = TRUE)
  
  na_percent <- round(na_count / total_cells * 100, 1)
  zero_percent <- round(zero_count / total_cells * 100, 1)
  positive_percent <- round(positive_count / total_cells * 100, 1)
  
  sparsity <- na_percent + zero_percent
  
  # ARCHAEOLOGICALLY CORRECT ASSESSMENT
  # Based on literature (Choulakian 2017, Peeples & Schachner 2012)
  quality_rating <- if (sparsity < 95) paste0("✅ ", tr("data.quality.excellent")) else
    if (sparsity < 98) paste0("⚠️ ", tr("data.quality.very.sparse")) else
      if (sparsity < 99.5) paste0("💡 ", tr("data.quality.extremely.sparse")) else paste0("🔬 ", tr("data.quality.critical"))

  explanation <- if (sparsity < 95) {
    tr("data.quality.standard.good")
  } else if (sparsity < 98) {
    tr("data.quality.sparse.ok")
  } else if (sparsity < 99.5) {
    tr("data.quality.extreme.taxicab")
  } else {
    tr("data.quality.critical.filter")
  }

  sprintf(
    "%s\n\n📊 %s:\n• %s: %d (%.1f%%)\n• %s: %d (%.1f%%)\n• %s: %d (%.1f%%)\n• %s: %.1f%%\n\n📖 %s",
    quality_rating, tr("data.quality.details"),
    tr("data.quality.missing"), na_count, na_percent,
    tr("data.quality.zero"), zero_count, zero_percent,
    tr("data.quality.positive"), positive_count, positive_percent,
    tr("data.quality.sparsity"), sparsity, explanation
  )
})

output$raw_data_preview <- DT::renderDataTable({
  req(data_raw())
  
  preview_data <- head(data_raw(), 10)
  
  DT::datatable(
    preview_data, 
    options = list(
      scrollX = TRUE, 
      pageLength = 5,
      dom = 'tip',
      columnDefs = list(list(className = 'dt-center', targets = "_all"))
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover'
  ) %>%
    formatRound(columns = 2:ncol(preview_data), digits = 2)
})

output$plot_parameter_indicator <- renderText({
  tryCatch({
    req(filtermod$parameter_status())
    status <- filtermod$parameter_status()

    sprintf(
      "⚖️ %d Sites (Ø %.2f), %d Types (Ø %.2f), %d %s",
      status$active_sites, status$mean_weight_sites,
      status$active_types, status$mean_weight_types,
      status$suppl_sites + status$suppl_types,
      tr("status.suppl.projected")
    )
  }, error = function(e) {
    tr("status.params.loading")
  })
})

# SUMMARIES
output$count_summary <- renderText({
  req(data_raw(), meta$data)
  
  original <- data_raw()
  
  tryCatch({
    filtered <- filtermod$filtered_data()
    
    active_sites <- sum(meta$data$sites$Selected & !meta$data$sites$Supplementary)
    suppl_sites <- sum(meta$data$sites$Selected & meta$data$sites$Supplementary)
    active_types <- sum(meta$data$types$Selected & !meta$data$types$Supplementary)
    suppl_types <- sum(meta$data$types$Selected & meta$data$types$Supplementary)
    
    coord_status <- if (!is.null(mapping$meta)) {
      valid_coords <- sum(!is.na(mapping$meta$lon) & !is.na(mapping$meta$lat))
      if (valid_coords > 0) {
        sprintf("\n🗺️ %s", sprintf(tr("data.summary.coords.localized"), valid_coords))
      } else {
        paste0("\n⚠️ ", tr("data.summary.coords.none"))
      }
    } else {
      paste0("\n📍 ", tr("data.summary.coords.loading"))
    }

    sprintf(
      "📈 %s: %d Sites × %d Types\n🔍 %s: %d Sites × %d Types\n\n✅ %s: %d Sites, %d Types\n🔗 %s: %d Sites, %d Types\n\n💾 %s: %s%s\n🏛️ SeriARC v.1.5 %s",
      tr("data.summary.original"), nrow(original), ncol(original) - 1,
      tr("data.summary.filtered"), nrow(filtered), ncol(filtered),
      tr("data.summary.active"), active_sites, active_types,
      tr("data.summary.supplementary"), suppl_sites, suppl_types,
      tr("data.summary.datapoints"), format(sum(filtered > 0, na.rm = TRUE), big.mark = " "),
      coord_status,
      tr("data.summary.ready")
    )
  }, error = function(e) {
    sprintf("📈 Original: %d Sites × %d Types\n⚠️ Filter error: %s",
            nrow(original), ncol(original) - 1, e$message)
  })
})

# 4 SEPARATE TEXT BOXES for filter effects (much cleaner!)
output$filter_summary_col1 <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    original <- data_raw()
    filtered <- filtermod$filtered_data()
    comparison <- filtermod$matrix_comparison()
    
    orig_sites <- nrow(original)
    orig_types <- ncol(original) - 1
    final_sites <- comparison$final_sites
    final_types <- comparison$final_types
    
    sprintf(
      "%s:\n%d Sites × %d Types\n\n%s:\n%d Sites × %d Types\n\n%s:\n-%d Sites, -%d Types",
      tr("filter.original"), orig_sites, orig_types,
      tr("filter.filtered"), final_sites, final_types,
      tr("filter.difference"), orig_sites - final_sites, orig_types - final_types
    )
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.error"), e$message)
  })
})

output$filter_summary_col2 <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    original <- data_raw()
    comparison <- filtermod$matrix_comparison()
    
    orig_sites <- nrow(original)
    orig_types <- ncol(original) - 1
    final_sites <- comparison$final_sites
    final_types <- comparison$final_types
    
    site_retention <- round((final_sites / orig_sites) * 100, 1)
    type_retention <- round((final_types / orig_types) * 100, 1)
    
    # Aggregation statistics
    hidden_sites <- sum(meta$data$sites$Hidden, na.rm = TRUE)
    hidden_types <- sum(meta$data$types$Hidden, na.rm = TRUE)
    
    sprintf(
      "Sites:\n%.1f%% %s\n(%d %s %d)\n%d %s\n\nTypes:\n%.1f%% %s\n(%d %s %d)\n%d %s",
      site_retention, tr("filter.retained"), final_sites, tr("filter.of"), orig_sites, hidden_sites, tr("filter.aggregated"),
      type_retention, tr("filter.retained"), final_types, tr("filter.of"), orig_types, hidden_types, tr("filter.aggregated")
    )
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.error"), e$message)
  })
})

output$filter_summary_col3 <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    filtered <- filtermod$filtered_data()
    comparison <- filtermod$matrix_comparison()
    
    matrix_points <- sum(filtered > 0, na.rm = TRUE)
    sparsity <- round((1 - matrix_points / (nrow(filtered) * ncol(filtered))) * 100, 1)
    
    sprintf(
      "%s:\n%s\n\n%s:\n%.1f%%\n\nSupplementary:\n%d %s",
      tr("filter.datapoints"), format(matrix_points, big.mark = " "),
      tr("filter.sparsity"), sparsity,
      comparison$suppl_count, tr("filter.suppl.projected")
    )
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.error"), e$message)
  })
})

output$filter_summary_col4 <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    sites_with_custom_weight <- sum(abs(meta$data$sites$Weight - 1.0) > 0.001, na.rm = TRUE)
    types_with_custom_weight <- sum(abs(meta$data$types$Weight - 1.0) > 0.001, na.rm = TRUE)
    total_sites <- nrow(meta$data$sites)
    total_types <- nrow(meta$data$types)
    
    avg_site_weight <- if(total_sites > 0) round(mean(meta$data$sites$Weight), 2) else 1.0
    avg_type_weight <- if(total_types > 0) round(mean(meta$data$types$Weight), 2) else 1.0
    
    sprintf(
      "Sites %s:\n%d %s %d\n\nTypes %s:\n%d %s %d\n\n%s:\nSites: %.2f, Types: %.2f",
      tr("filter.modified"), sites_with_custom_weight, tr("filter.of"), total_sites,
      tr("filter.modified"), types_with_custom_weight, tr("filter.of"), total_types,
      tr("filter.avg.weight"), avg_site_weight, avg_type_weight
    )
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.error"), e$message)
  })
})

output$filter_recommendation <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    original <- data_raw()
    filtered <- filtermod$filtered_data()
    comparison <- filtermod$matrix_comparison()
    
    orig_sites <- nrow(original)
    orig_types <- ncol(original) - 1
    final_sites <- comparison$final_sites
    final_types <- comparison$final_types
    
    site_retention <- round((final_sites / orig_sites) * 100, 1)
    type_retention <- round((final_types / orig_types) * 100, 1)
    avg_retention <- (site_retention + type_retention) / 2
    
    matrix_points <- sum(filtered > 0, na.rm = TRUE)
    sparsity <- round((1 - matrix_points / (nrow(filtered) * ncol(filtered))) * 100, 1)
    
    quality_icon <- if (avg_retention >= 85) "🟢" else if (avg_retention >= 70) "🟡" else "🔴"
    quality_text <- if (avg_retention >= 85) tr("filter.quality.verygood") else if (avg_retention >= 70) tr("filter.quality.good") else tr("filter.quality.sufficient")

    ca_ready <- final_sites >= 10 && final_types >= 10 && sparsity < 95
    recommendation <- if (ca_ready) paste0("✅ ", tr("filter.ready")) else paste0("⚠️ ", tr("filter.limited"))

    sprintf("%s %s • %s", quality_icon, quality_text, recommendation)
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.error"), e$message)
  })
})
output$filtered_summary <- renderText({
  req(data_raw(), meta$data)
  
  original <- data_raw()
  
  tryCatch({
    filtered <- filtermod$filtered_data()
    status <- filtermod$parameter_status()
    
    matrix_sum <- sum(filtered, na.rm = TRUE)
    matrix_points <- sum(filtered > 0, na.rm = TRUE)
    sparsity <- round((1 - matrix_points / (nrow(filtered) * ncol(filtered))) * 100, 1)
    
    sprintf(
      "📊 %s:\n\n🎯 %s: %d Sites × %d Types\n💾 %s: %s (%s: %.1f)\n📈 %s: %.1f%%\n\n⚖️ %s:\n• Sites: Ø %.2f\n• Types: Ø %.2f\n\n🔗 Supplementary: %d %s",
      tr("filter.summary.title"),
      tr("filter.summary.dimensions"), nrow(filtered), ncol(filtered),
      tr("filter.summary.datapoints"), format(matrix_points, big.mark = " "), tr("status.transform.sum"), matrix_sum,
      tr("filter.sparsity"), sparsity,
      tr("filter.summary.weighting"),
      status$mean_weight_sites, status$mean_weight_types,
      status$suppl_sites + status$suppl_types, tr("filter.suppl.projected")
    )
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.summary.error"), e$message)
  })
})

output$filter_quality <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    original <- data_raw()
    filtered <- filtermod$filtered_data()
    comparison <- filtermod$matrix_comparison()
    
    retention_sites <- round(comparison$final_sites / comparison$orig_sites * 100, 1)
    retention_types <- round(comparison$final_types / comparison$orig_types * 100, 1)
    weight_impact <- comparison$weight_impact
    
    quality_rating <- if (retention_sites >= 80 && retention_types >= 80) {
      paste0("🟢 ", tr("filter.quality.verygood"))
    } else if (retention_sites >= 60 && retention_types >= 60) {
      paste0("🟡 ", tr("filter.quality.good"))
    } else if (retention_sites >= 40 && retention_types >= 40) {
      paste0("🟠 ", tr("filter.quality.medium"))
    } else {
      paste0("🔴 ", tr("filter.quality.weak"))
    }

    sprintf(
      "%s\n\n📈 %s:\n• Sites: %.1f%% (%d %s %d)\n• Types: %.1f%% (%d %s %d)\n\n⚖️ %s:\n• %s: %+.1f%%\n• Supplementary: %d %s\n\n📊 %s: %s",
      quality_rating,
      tr("filter.quality.title"),
      retention_sites, comparison$final_sites, tr("filter.of"), comparison$orig_sites,
      retention_types, comparison$final_types, tr("filter.of"), comparison$orig_types,
      tr("filter.quality.weight.effect"),
      tr("filter.quality.data.change"), weight_impact,
      comparison$suppl_count, tr("filter.quality.elements"),
      tr("filter.quality.recommendation"),
      if (retention_sites >= 70 && retention_types >= 70) paste0("✅ ", tr("filter.optimal")) else
      if (retention_sites >= 50 && retention_types >= 50) paste0("⚠️ ", tr("filter.acceptable")) else paste0("❌ ", tr("filter.critical"))
    )
  }, error = function(e) {
    sprintf("⚠️ %s: %s", tr("filter.quality.error"), e$message)
  })
})

output$debug_filter_status <- renderText({
  req(data_raw(), meta$data)
  
  tryCatch({
    status <- filtermod$parameter_status()
    weights_df <- filtermod$weights_table()
    comparison <- filtermod$matrix_comparison()
    
    sprintf(
      "🔧 %s:\n\n📊 %s:\n• %s: %d (Ø %s: %.3f)\n• %s: %d (Ø %s: %.3f)\n• Supplementary Sites: %d\n• Supplementary Types: %d\n\n📈 %s:\n• Original: %d×%d (%s: %.1f)\n• %s: %d×%d (%s: %.1f)\n• %s: %+.1f%%\n\n⚖️ %s:\n• %s: %d\n• %s: %.3f\n• %s: %.3f",
      tr("debug.filter.title"),
      tr("debug.param.status"),
      tr("debug.active.sites"), status$active_sites, tr("debug.weight"), status$mean_weight_sites,
      tr("debug.active.types"), status$active_types, tr("debug.weight"), status$mean_weight_types,
      status$suppl_sites, status$suppl_types,
      tr("debug.matrix.transform"),
      comparison$orig_sites, comparison$orig_types, tr("status.transform.sum"), comparison$orig_sum,
      tr("debug.final"), comparison$final_sites, comparison$final_types, tr("status.transform.sum"), comparison$final_sum,
      tr("debug.weight.effect"), comparison$weight_impact,
      tr("debug.weights.table"),
      tr("debug.elements.weighted"), nrow(weights_df),
      tr("debug.avg.sites"), mean(weights_df$Gewicht[weights_df$Typ == "Site"], na.rm = TRUE),
      tr("debug.avg.types"), mean(weights_df$Gewicht[weights_df$Typ == "Type"], na.rm = TRUE)
    )
  }, error = function(e) {
    sprintf("❌ %s: %s", tr("debug.error"), e$message)
  })
})

output$filter_stats <- renderText({
  req(data_raw())
  
  original <- data_raw()
  
  sprintf(
    "📊 %s:\n\nSites: %d\nTypes: %d\n\n🎯 %s",
    tr("filter.stats.title"),
    nrow(original), ncol(original) - 1,
    tr("filter.stats.auto")
  )
})

# FILTER ANALYSIS OUTPUTS
output$filter_stats_table <- renderTable({
  req(filtered_data(), data_raw())
  
  df <- data.frame(
    Kategorie = c(tr("filter.stats.sites.total"), tr("filter.stats.sites.filtered"),
                  tr("filter.stats.types.total"), tr("filter.stats.types.filtered")),
    Anzahl = c(nrow(data_raw()), nrow(filtered_data()),
               ncol(data_raw())-1, ncol(filtered_data())-1)
  )
  names(df) <- c(tr("filter.stats.category"), tr("filter.stats.count"))
  df
}, striped = TRUE, bordered = TRUE)

output$filter_quality_info <- renderText({
  req(filtered_data())
  
  mat <- as.matrix(filtered_data()[,-1])
  sparsity <- sum(mat == 0, na.rm = TRUE) / length(mat) * 100
  non_zero <- sum(mat > 0, na.rm = TRUE)
  
  sprintf("%s:\n\n%s: %.1f%%\n%s: %d\n\n%s",
          tr("filter.info.overview"),
          tr("filter.sparsity"), sparsity,
          tr("filter.info.nonzero"), non_zero,
          tr("filter.info.suitable"))
})


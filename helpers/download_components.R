# DOWNLOAD COMPONENTS HELPER - SeriARC V1.5
# Reusable download handlers and export functions

# === STANDARD EXPORT FUNCTIONS ===

# Base info sheet for all Excel exports
create_seriarc_info_sheet <- function(export_type = "Data", additional_info = NULL) {
  info_df <- data.frame(
    Parameter = c("Software", "Version", "Export_Type", "Created_at", "User"),
    Value = c("SeriARC", "v1.0.0", export_type, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), Sys.info()[["user"]])
  )
  
  if (!is.null(additional_info)) {
    additional_df <- data.frame(
      Parameter = names(additional_info),
      Value = as.character(additional_info)
    )
    info_df <- rbind(info_df, additional_df)
  }
  
  info_df
}

matrix_to_excel_format <- function(matrix_data, entity_col_name = "Entity") {
  if (is.null(matrix_data)) return(data.frame())
  
  df <- data.frame(
    Entity = rownames(matrix_data),
    matrix_data,
    check.names = FALSE
  )
  colnames(df)[1] <- entity_col_name
  df
}

# Generate statistics sheet
create_statistics_sheet <- function(matrix_data, analysis_type = "Matrix") {
  if (is.null(matrix_data)) return(data.frame(Note = "No data available"))
  
  data.frame(
    Metric = c("Number of Sites", "Number of Types", "Total Data Points", "Non-Zero Values",
               "Sparsity (%)", "Mean", "Maximum", "Minimum"),
    Value = c(
      nrow(matrix_data),
      ncol(matrix_data),
      nrow(matrix_data) * ncol(matrix_data),
      sum(matrix_data > 0, na.rm = TRUE),
      round((1 - sum(matrix_data > 0, na.rm = TRUE) / (nrow(matrix_data) * ncol(matrix_data))) * 100, 1),
      round(mean(matrix_data, na.rm = TRUE), 3),
      round(max(matrix_data, na.rm = TRUE), 3),
      round(min(matrix_data, na.rm = TRUE), 3)
    ),
    Analysis = analysis_type
  )
}

# === PLOT EXPORT HANDLERS ===

# PNG export handler template (with real data when available)
# @param tr Optional translator function for i18n
create_png_download_handler <- function(plot_output_id, base_filename, session, plot_data = NULL, plot_generator_func = NULL, width_func = NULL, height_func = NULL, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  downloadHandler(
    filename = function() sprintf("%s_%s.png", base_filename, Sys.Date()),
    content = function(file) {
      showNotification(.tr("export.png.progress", "PNG export in progress..."), type = "message", duration = 3)
      
      tryCatch({
        data_value <- if (!is.null(plot_data) && is.reactive(plot_data)) plot_data() else NULL
        
        width_px <- 1200
        height_px <- 800
        res_dpi <- 300
        
        # If real plot data is available, use it
        if (!is.null(data_value) && !is.null(plot_generator_func)) {
          data <- data_value
          if (!is.null(data) && length(data) > 0 && !is.null(data$permuted_matrix)) {
            mat <- data$permuted_matrix
            n_sites <- nrow(mat)
            n_types <- ncol(mat)
            
            width_inch <- max(12, min(60, n_types * 0.25))
            height_inch <- max(8, min(40, n_sites * 0.25))
            
            # Bubble needs more space for legend
            if (!is.null(data$plot_type) && data$plot_type == "bubble") {
              width_inch <- width_inch + 4  # +4 inches
            }
            
            width_px <- width_inch * res_dpi
            height_px <- height_inch * res_dpi
            
            png(file, width = width_px, height = height_px, res = res_dpi)
            plot_generator_func(data)
            dev.off()
            showNotification(.tr("export.png.complete", "PNG export completed with data!"), type = "message", duration = 2)
            return()
          }
        }
        
        # Fallback: User-friendly hint for Plotly export
        png(file, width = width_px, height = height_px, res = 150)
        par(mar = c(5, 4, 4, 2))
        plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10), 
             xlab = "", ylab = "", main = "SeriARC PNG Export", axes = FALSE)
        
        # Main hint
        text(5, 7.5, .tr("export.png.hint.title", "For best PNG quality:"), cex = 1.8, font = 2, col = "#2c3e50")
        text(5, 6.5, .tr("export.png.hint.click", "Click directly on the plot"), cex = 1.4, col = "#3498db")
        text(5, 5.7, .tr("export.png.hint.camera", "Use the Plotly camera icon"), cex = 1.4, col = "#3498db")
        text(5, 4.9, .tr("export.png.hint.download", "Select 'Download plot as PNG'"), cex = 1.4, col = "#3498db")

        # Alternative hint
        text(5, 3.8, .tr("export.png.hint.pdf", "OR: Use PDF export for publications!"), cex = 1.3, font = 2, col = "#27ae60")
        text(5, 3.2, .tr("export.png.hint.pdf.data", "PDF contains all plot data"), cex = 1.1, col = "#27ae60")

        # Technical info
        text(5, 2.3, .tr("export.png.hint.plotly", "Plotly limitation: Browser export only"), cex = 1.0, col = "#e67e22")
        
        # SeriARC branding
        text(5, 1.2, "SeriARC v1.0.0 - Archaeological Analysis Suite", cex = 0.9, col = "gray")

        # Decorative elements
        points(c(1.5, 8.5), c(8.5, 8.5), pch = 16, cex = 2.5, col = "#3498db")
        points(c(1.5, 8.5), c(1.5, 1.5), pch = 17, cex = 2.5, col = "#e74c3c")
        
        dev.off()
        
        showNotification(.tr("export.png.hint.created", "PNG export hint created!"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(.tr("export.png.error", "PNG export error:"), e$message), type = "error", duration = 5)
      })
    }
  )
}

# SVG export handler template (with real data fallback)
# @param tr Optional translator function for i18n
create_svg_download_handler <- function(plot_output_id, base_filename, session, plot_data = NULL, plot_generator_func = NULL, width_func = NULL, height_func = NULL, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  downloadHandler(
    filename = function() sprintf("%s_%s.svg", base_filename, Sys.Date()),
    content = function(file) {
      showNotification(.tr("export.svg.progress", "SVG export in progress..."), type = "message", duration = 3)
      
      tryCatch({
        data_value <- if (!is.null(plot_data) && is.reactive(plot_data)) plot_data() else NULL
        
        width_inch <- 12
        height_inch <- 8
        
        # If real plot data is available, use it
        if (!is.null(data_value)) {
          data <- data_value
          if (!is.null(data) && !is.null(data$permuted_matrix) && !is.null(plot_generator_func)) {
            mat <- data$permuted_matrix
            n_sites <- nrow(mat)
            n_types <- ncol(mat)
            
            # Adaptive dimensions: ~0.25 inch per site/type, min 8x12, max 40x60
            width_inch <- max(12, min(60, n_types * 0.25))
            height_inch <- max(8, min(40, n_sites * 0.25))
            
            # Bubble needs more space for legend
            if (!is.null(data$plot_type) && data$plot_type == "bubble") {
              width_inch <- width_inch + 4  # +4 inches
            }
            
            # Export real data as SVG
            svg(file, width = width_inch, height = height_inch)
            plot_generator_func(data)
            dev.off()
            
            showNotification(.tr("export.svg.complete", "SVG export completed with data!"), type = "message", duration = 2)
            return()
          }
        }

        # Fallback: User-friendly hint
        svg(file, width = 12, height = 8)
        par(mar = c(5, 4, 4, 2))
        plot(0, 0, type = "n", xlim = c(0, 10), ylim = c(0, 10),
             xlab = "", ylab = "", main = "SeriARC SVG Export", axes = FALSE)

        # Main hint
        text(5, 7.5, .tr("export.svg.hint.title", "SVG Export: Plotly Limitation!"), cex = 1.8, font = 2, col = "#e74c3c")
        text(5, 6.5, .tr("export.svg.hint.png.only", "Plotly only supports PNG export"), cex = 1.4, col = "#e67e22")

        # Suggested solutions
        text(5, 5.5, .tr("export.svg.hint.vector", "For vector graphics:"), cex = 1.4, font = 2, col = "#2c3e50")
        text(5, 4.8, .tr("export.svg.hint.pdf", "Use PDF export (contains actual data)"), cex = 1.2, col = "#27ae60")
        text(5, 4.2, .tr("export.svg.hint.convert", "Export PNG + convert in Illustrator/Inkscape"), cex = 1.1, col = "#3498db")
        text(5, 3.6, .tr("export.svg.hint.ggplot", "Or: Use R script for ggplot2-SVG"), cex = 1.1, col = "#3498db")

        # Recommendation
        text(5, 2.5, .tr("export.svg.hint.recommend", "RECOMMENDED: PDF export for publications!"), cex = 1.3, font = 2, col = "#27ae60")
        
        # SeriARC branding
        text(5, 1, "SeriARC v1.0.0 - Archaeological Analysis Suite", cex = 0.9, col = "gray")

        # Decorative elements
        points(c(1.5, 8.5), c(8, 8), pch = 16, cex = 3, col = "#3498db")
        points(c(1.5, 8.5), c(2, 2), pch = 17, cex = 3, col = "#e74c3c")
        
        dev.off()
        
        showNotification(.tr("export.svg.done", "SVG export completed!"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(.tr("export.svg.error", "SVG export error:"), e$message), type = "error", duration = 5)
      })
    }
  )
}

# PDF export handler template (with ggplot2 fallback)
# @param tr Optional translator function for i18n
create_pdf_download_handler <- function(plot_data, base_filename, plot_generator_func = NULL, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  downloadHandler(
    filename = function() sprintf("%s_%s.pdf", base_filename, Sys.Date()),
    content = function(file) {
      showNotification(.tr("export.pdf.progress", "PDF export in progress..."), type = "message", duration = 3)
      
      tryCatch({
        data_value <- if (is.reactive(plot_data)) plot_data() else plot_data
        
        pdf_width <- 12
        pdf_height <- 8
        
        if (!is.null(data_value) && !is.null(data_value$permuted_matrix)) {
          mat <- data_value$permuted_matrix
          n_sites <- nrow(mat)
          n_types <- ncol(mat)
          
          # Adaptive dimensions: ~0.25 inch per site/type, min 8x12, max 40x60
          pdf_width <- max(12, min(60, n_types * 0.25))
          pdf_height <- max(8, min(40, n_sites * 0.25))
          
          # Bubble needs more space for legend
          if (!is.null(data_value$plot_type) && data_value$plot_type == "bubble") {
            pdf_width <- pdf_width + 4  # +4 incheses for legend (was +2)
          }
        }
        
        pdf(file, width = pdf_width, height = pdf_height)
        
        if (!is.null(plot_generator_func)) {
          # Custom plot function
          plot_generator_func(data_value)
        } else {
          # Standard plot
          if (!is.null(data_value) && nrow(data_value) > 0 && "x" %in% colnames(data_value) && "y" %in% colnames(data_value)) {
            # Colors based on type/element_type
            point_colors <- if ("type" %in% colnames(data_value)) {
              ifelse(data_value$type == "Site", "#3498db", "#e74c3c")
            } else "#3498db"
            
            point_shapes <- if ("type" %in% colnames(data_value)) {
              ifelse(data_value$type == "Site", 16, 17)
            } else 16
            
            plot(data_value$x, data_value$y,
                 col = point_colors,
                 pch = point_shapes,
                 cex = 1.2,
                 xlab = "Dimension 1", 
                 ylab = "Dimension 2",
                 main = paste("SeriARC", base_filename))
            
            grid(col = "lightgray", lty = "dotted")
            
            # Legend
            if ("type" %in% colnames(data_value)) {
              legend("topright", 
                     legend = c("Sites", "Types"),
                     col = c("#3498db", "#e74c3c"),
                     pch = c(16, 17),
                     cex = 0.8)
            }
          } else {
            plot(1, 1, type = "n", xlab = "", ylab = "", main = paste("SeriARC", base_filename))
            text(1, 1, .tr("export.pdf.nodata", "No plot data available"), cex = 1.5, col = "#e74c3c")
          }
        }

        dev.off()
        showNotification(.tr("export.pdf.complete", "PDF export completed!"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(.tr("export.pdf.error", "PDF export error:"), e$message), type = "error", duration = 5)
        if (file.exists(file)) file.remove(file)
      })
    }
  )
}

# === DATA EXPORT HANDLERS ===

# Excel data export handler template
# @param tr Optional translator function for i18n
create_excel_download_handler <- function(data_sheets_func, base_filename, export_type = "Data", tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  downloadHandler(
    filename = function() sprintf("SeriARC_%s_%s.xlsx", base_filename, Sys.Date()),
    content = function(file) {
      showNotification(.tr("export.excel.progress", "Excel export in progress..."), type = "message", duration = 3)
      
      tryCatch({
        # Generate data sheets
        data_list <- data_sheets_func()
        
        # Add info sheet
        if (!"SeriARC_Info" %in% names(data_list)) {
          data_list <- c(list("SeriARC_Info" = create_seriarc_info_sheet(export_type)), data_list)
        }
        
        writexl::write_xlsx(data_list, file)
        
        showNotification(.tr("export.excel.complete", "Excel export completed!"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(.tr("export.excel.error", "Excel export error:"), e$message), type = "error", duration = 5)
      })
    }
  )
}

# CSV export handler template
# @param tr Optional translator function for i18n
create_csv_download_handler <- function(data_func, base_filename, add_header = TRUE, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  downloadHandler(
    filename = function() sprintf("SeriARC_%s_%s.csv", base_filename, Sys.Date()),
    content = function(file) {
      showNotification(.tr("export.csv.progress", "CSV export in progress..."), type = "message", duration = 3)
      
      tryCatch({
        if (add_header) {
          header_lines <- c(
            "# SeriARC Archaeological Analysis Suite v1.0.0",
            sprintf("# Analysiert am: %s", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
            sprintf("# Datei: %s", base_filename),
            "# https://github.com/seriarc/seriarc",
            ""
          )
          writeLines(header_lines, file)
        }
        
        data <- data_func()
        if (!is.null(data) && nrow(data) > 0) {
          write.csv(data, file, row.names = FALSE, append = add_header)
        } else {
          warning_df <- data.frame(
            Note = .tr("export.csv.nodata", "No data available"),
            Info = .tr("export.csv.runfirst", "Please run an analysis first")
          )
          write.csv(warning_df, file, row.names = FALSE, append = add_header)
        }

        showNotification(.tr("export.csv.complete", "CSV export completed!"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(.tr("export.csv.error", "CSV export error:"), e$message), type = "error", duration = 5)
      })
    }
  )
}

# === SPECIALIZED EXPORT FUNCTIONS ===

# CA-specific Excel data sheets
create_ca_excel_sheets <- function(ca_result, filtered_data = NULL) {
  if (is.null(ca_result)) {
    return(list("Note" = data.frame(Info = "No CA results available")))
  }
  
  sheets <- list()
  
  # Eigenvalues
  if (!is.null(ca_result$eig)) {
    sheets[["CA_Eigenwerte"]] <- data.frame(
      Dimension = paste0("Dim", 1:nrow(ca_result$eig)),
      Eigenwert = round(ca_result$eig[, 1], 4),
      Varianz_Prozent = round(ca_result$eig[, 2], 2),
      Kumuliert_Prozent = round(ca_result$eig[, 3], 2)
    )
  }
  
  # Sites coordinates
  if (!is.null(ca_result$row) && !is.null(ca_result$row$coord)) {
    sheets[["CA_Sites_Koordinaten"]] <- data.frame(
      Site = rownames(ca_result$row$coord),
      ca_result$row$coord,
      check.names = FALSE
    )
  }
  
  # Types coordinates  
  if (!is.null(ca_result$col) && !is.null(ca_result$col$coord)) {
    sheets[["CA_Types_Koordinaten"]] <- data.frame(
      Type = rownames(ca_result$col$coord),
      ca_result$col$coord,
      check.names = FALSE
    )
  }
  
  # Sites contributions
  if (!is.null(ca_result$row) && !is.null(ca_result$row$contrib)) {
    sheets[["CA_Sites_Beitraege"]] <- data.frame(
      Site = rownames(ca_result$row$contrib),
      ca_result$row$contrib,
      check.names = FALSE
    )
  }
  
  # Types contributions
  if (!is.null(ca_result$col) && !is.null(ca_result$col$contrib)) {
    sheets[["CA_Types_Beitraege"]] <- data.frame(
      Type = rownames(ca_result$col$contrib),
      ca_result$col$contrib,
      check.names = FALSE
    )
  }
  
  # Original data
  if (!is.null(filtered_data)) {
    sheets[["Original_Matrix"]] <- matrix_to_excel_format(filtered_data, "Site")
    sheets[["Statistiken"]] <- create_statistics_sheet(filtered_data, "Korrespondenzanalyse")
  }
  
  sheets
}

# K-Means specific Excel data sheets
create_kmeans_excel_sheets <- function(kmeans_result, get_cluster_name_func = NULL, parameters = NULL) {
  if (is.null(kmeans_result)) {
    return(list("Note" = data.frame(Info = "No K-Means results available")))
  }

  sheets <- list()

  # REPRODUCIBILITY: Parameter metadata as first sheet
  if (!is.null(parameters)) {
    method <- parameters$method %||% "kmeans"
    method_label <- switch(method,
                          "kmeans" = "K-Means",
                          "hierarchical" = "Hierarchisch",
                          "fuzzy" = "Fuzzy K-Means",
                          "gmm" = "GMM (Gaussian Mixture)",
                          "K-Means")

    param_names <- c("Clustering-Methode", "X-Dimension (Visualisierung)", "Y-Dimension (Visualisierung)",
                     "Clustering-Dimensionen", "Clustere", "Anzahl Cluster")
    param_values <- c(
      method_label,
      as.character(parameters$x_dim %||% "Unbekannt"),
      as.character(parameters$y_dim %||% "Unbekannt"),
      paste0(parameters$n_dims %||% 2, "D"),
      as.character(parameters$cluster_on %||% "both"),
      as.character(parameters$num_clusters %||% "Unbekannt")
    )

    # Method-specific parameters
    if (method == "kmeans") {
      param_names <- c(param_names, "Seed fixiert", "Seed-Wert", "nstart", "iter.max")
      param_values <- c(param_values,
                       as.character(parameters$seed_fixed %||% FALSE),
                       if (isTRUE(parameters$seed_fixed)) as.character(parameters$seed_value %||% 123) else "Not fixed",
                       "50", "200")
    } else if (method == "hierarchical") {
      param_names <- c(param_names, "Linkage-Methode")
      param_values <- c(param_values, as.character(parameters$hclust_method %||% "average"))
    } else if (method == "fuzzy") {
      param_names <- c(param_names, "Fuzziness-Parameter (m)", "max. Iterationen")
      param_values <- c(param_values, as.character(parameters$fuzzy_m %||% 2), "500")
    } else if (method == "gmm") {
      param_names <- c(param_names, "Kovarianz-Modell")
      param_values <- c(param_values, as.character(parameters$gmm_model %||% "auto"))
    }

    param_names <- c(param_names, "Export-Zeitpunkt", "SeriARC Version")
    param_values <- c(param_values, format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "SeriARC 2024")

    param_df <- data.frame(
      Parameter = param_names,
      Wert = param_values,
      stringsAsFactors = FALSE
    )
    sheets[["0_Parameter"]] <- param_df
  }

  # Cluster assignments
  if (!is.null(kmeans_result$data)) {
    result_table <- kmeans_result$data[, c("label", "type", "element_type", "cluster")]
    
    # Add cluster names
    if (!is.null(get_cluster_name_func)) {
      result_table$cluster_name <- sapply(result_table$cluster, function(id) {
        tryCatch(get_cluster_name_func(id), error = function(e) paste("Cluster", id))
      })
    } else {
      result_table$cluster_name <- paste("Cluster", result_table$cluster)
    }
    
    colnames(result_table) <- c("Entity", "Element_Typ", "Status", "Cluster_Nr", "Cluster_Name")
    
    # Add cluster sizes
    cluster_sizes <- table(result_table$Cluster_Nr)
    result_table$Cluster_Groesse <- as.numeric(cluster_sizes[result_table$Cluster_Nr])
    
    sheets[["Cluster_Zuordnungen"]] <- result_table[order(result_table$Cluster_Nr, result_table$Status), ]
  }

  # Cluster centers
  if (!is.null(kmeans_result$centers)) {
    centers_df <- data.frame(
      Cluster = 1:nrow(kmeans_result$centers),
      kmeans_result$centers,
      check.names = FALSE
    )

    if (!is.null(get_cluster_name_func)) {
      centers_df$Cluster_Name <- sapply(1:nrow(kmeans_result$centers), function(id) {
        tryCatch(get_cluster_name_func(id), error = function(e) paste("Cluster", id))
      })
    }

    sheets[["Cluster_Zentren"]] <- centers_df
  }
  
  # Quality metrics (EXTENDED with CH, DB, Cophenetic, Dunn)
  quality_metrics <- c()
  interpretations <- c()

  if (!is.null(kmeans_result$silhouette)) {
    quality_metrics <- c(quality_metrics, "Silhouette Score" = round(kmeans_result$silhouette, 3))
    interpretations <- c(interpretations, "Higher = better (max 1.0)")
  }
  if (!is.null(kmeans_result$calinski_harabasz)) {
    quality_metrics <- c(quality_metrics, "Calinski-Harabasz Index" = round(kmeans_result$calinski_harabasz, 2))
    interpretations <- c(interpretations, "Higher = better (no maximum)")
  }
  if (!is.null(kmeans_result$davies_bouldin)) {
    quality_metrics <- c(quality_metrics, "Davies-Bouldin Index" = round(kmeans_result$davies_bouldin, 3))
    interpretations <- c(interpretations, "Lower = better (min 0.0)")
  }
  if (!is.null(kmeans_result$cophenetic_corr) && !is.na(kmeans_result$cophenetic_corr)) {
    quality_metrics <- c(quality_metrics, "Cophenetic Correlation" = round(kmeans_result$cophenetic_corr, 3))
    interpretations <- c(interpretations, "Higher = better (hierarchy quality)")
  }
  if (!is.null(kmeans_result$dunn_coeff) && !is.na(kmeans_result$dunn_coeff)) {
    quality_metrics <- c(quality_metrics, "Dunn Partition Coefficient" = round(kmeans_result$dunn_coeff, 3))
    interpretations <- c(interpretations, "Higher = sharper (fuzzy quality)")
  }
  if (!is.null(kmeans_result$gmm_bic) && !is.na(kmeans_result$gmm_bic)) {
    quality_metrics <- c(quality_metrics, "BIC (Bayesian Information Criterion)" = round(kmeans_result$gmm_bic, 2))
    interpretations <- c(interpretations, "Lower = better (GMM model quality)")
  }
  if (!is.null(kmeans_result$gmm_loglik) && !is.na(kmeans_result$gmm_loglik)) {
    quality_metrics <- c(quality_metrics, "Log-Likelihood" = round(kmeans_result$gmm_loglik, 2))
    interpretations <- c(interpretations, "Higher = better (GMM fit)")
  }
  if (!is.null(kmeans_result$tot_withinss)) {
    quality_metrics <- c(quality_metrics, "Within SS" = round(kmeans_result$tot_withinss, 2))
    interpretations <- c(interpretations, "Lower = better")
  }
  if (!is.null(kmeans_result$betweenss)) {
    quality_metrics <- c(quality_metrics, "Between SS" = round(kmeans_result$betweenss, 2))
    interpretations <- c(interpretations, "Higher = better")
  }

  if (length(quality_metrics) > 0) {
    explained_var <- if (!is.null(kmeans_result$betweenss) && !is.null(kmeans_result$tot_withinss)) {
      round(kmeans_result$betweenss / (kmeans_result$tot_withinss + kmeans_result$betweenss) * 100, 1)
    } else NA

    n_dims <- kmeans_result$n_dims %||% 2

    sheets[["Clustering_Qualitaet"]] <- data.frame(
      Metrik = c(names(quality_metrics), "Explained Variance %", "Clustering Dimensionen"),
      Wert = c(as.character(quality_metrics), ifelse(is.na(explained_var), "N/A", explained_var), paste0(n_dims, "D")),
      Interpretation = c(interpretations, "% variance between clusters", "Number of CA dimensions for clustering")
    )
  }

  # FUZZY CLUSTERING: Membership-Matrix
  if (!is.null(kmeans_result$clustering_result) && !is.null(kmeans_result$clustering_result$membership)) {
    membership_matrix <- kmeans_result$clustering_result$membership
    membership_df <- data.frame(
      Entity = rownames(membership_matrix),
      membership_matrix,
      check.names = FALSE
    )
    colnames(membership_df)[-1] <- paste0("Cluster_", 1:ncol(membership_matrix))
    sheets[["Fuzzy_Membership"]] <- membership_df
  }

  # GMM: Posterior probabilities
  if (!is.null(kmeans_result$clustering_result) && !is.null(kmeans_result$clustering_result$gmm_prob)) {
    gmm_prob_matrix <- kmeans_result$clustering_result$gmm_prob
    gmm_prob_df <- data.frame(
      Entity = rownames(gmm_prob_matrix),
      gmm_prob_matrix,
      check.names = FALSE
    )
    colnames(gmm_prob_df)[-1] <- paste0("Cluster_", 1:ncol(gmm_prob_matrix))
    sheets[["GMM_Posteriori"]] <- gmm_prob_df
  }

  # LEADING TYPES CHARACTERIZATION
  if (!is.null(kmeans_result$characterization) && !is.null(kmeans_result$characterization$leading_types)) {
    char_data <- kmeans_result$characterization
    num_clusters <- length(char_data$leading_types)

    char_rows <- list()
    for (k in 1:num_clusters) {
      leading_types <- char_data$leading_types[[k]]
      if (length(leading_types) > 0) {
        for (lt in leading_types) {
          # BACKWARDS COMPATIBILITY: Check if p_value_adj exists
          if (!is.null(lt$p_value_adj)) {
            # NEW VERSION: With FDR correction
            char_rows[[length(char_rows) + 1]] <- data.frame(
              Cluster = k,
              Type = lt$type,
              Anzahl = lt$count,
              Ueberrepraesentation_Prozent = round(lt$overrep * 100, 1),
              P_Wert = round(lt$p_value, 4),
              P_Wert_FDR_adj = round(lt$p_value_adj, 4),
              Signifikant_FDR = ifelse(lt$p_value_adj < 0.05, "Ja", "Nein"),
              stringsAsFactors = FALSE
            )
          } else {
            # OLD VERSION: Without FDR (for existing results)
            char_rows[[length(char_rows) + 1]] <- data.frame(
              Cluster = k,
              Type = lt$type,
              Anzahl = lt$count,
              Ueberrepraesentation_Prozent = round(lt$overrep * 100, 1),
              P_Wert = round(lt$p_value, 4),
              P_Wert_FDR_adj = NA,
              Signifikant_FDR = "Recalculation needed",
              stringsAsFactors = FALSE
            )
          }
        }
      }
    }

    if (length(char_rows) > 0) {
      char_df <- do.call(rbind, char_rows)
      sheets[["Leittypen"]] <- char_df[order(char_df$Cluster, -char_df$Ueberrepraesentation_Prozent), ]
    }
  }

  sheets
}

# Seriation-specific Excel data sheets
create_seriation_excel_sheets <- function(seriation_result) {
  if (is.null(seriation_result)) {
    return(list("Note" = data.frame(Info = "No seriation results available")))
  }
  
  sheets <- list()
  
  # Seriated matrix
  if (!is.null(seriation_result$permuted_matrix)) {
    permuted_mat <- as.matrix(seriation_result$permuted_matrix)
    sheets[["Seriation_Matrix"]] <- matrix_to_excel_format(permuted_mat, "Site_chronologisch")
    sheets[["Seriation_Statistiken"]] <- create_statistics_sheet(permuted_mat, "Seriation")
  }
  
  # Seriation order
  if (!is.null(seriation_result$permuted_matrix)) {
    permuted_mat <- as.matrix(seriation_result$permuted_matrix)
    max_length <- max(nrow(permuted_mat), ncol(permuted_mat))
    
    sheets[["Seriation_Ordnung"]] <- data.frame(
      Position = 1:max_length,
      Site_chronologisch = c(rownames(permuted_mat), rep(NA, max(0, max_length - nrow(permuted_mat)))),
      Type_chronologisch = c(colnames(permuted_mat), rep(NA, max(0, max_length - ncol(permuted_mat))))
    )
  }
  
  # Seriation parameters
  method_info <- if (!is.null(seriation_result$method_name)) seriation_result$method_name else "Unbekannt"
  transformation_info <- if (!is.null(seriation_result$transformation)) seriation_result$transformation else "Keine"
  
  sheets[["Seriation_Parameter"]] <- data.frame(
    Parameter = c("Methode", "Transformation", "Algorithmus", "Software"),
    Wert = c(method_info, transformation_info, "seriation R package", "SeriARC v1.0.0")
  )
  
  sheets
}

# === DOWNLOAD BUTTON SETS ===

# Standard plot download buttons (for UI)
plot_download_buttons_ui <- function(ns_prefix = "") {
  ns_func <- if (ns_prefix != "") function(x) paste0(ns_prefix, x) else function(x) x
  
  div(class = "download-buttons-group",
    h5("📥 Plot Export:", class = "mt-3"),
    div(class = "btn-group btn-group-sm", role = "group",
      downloadButton(ns_func("download_plot_png"), "PNG", class = "btn btn-outline-primary btn-sm", 
                     title = "PNG Export (1200x800px)"),
      downloadButton(ns_func("download_plot_svg"), "SVG", class = "btn btn-outline-success btn-sm",
                     title = "SVG Export (vector-based)"),
      downloadButton(ns_func("download_plot_pdf"), "PDF", class = "btn btn-outline-danger btn-sm",
                     title = "PDF Export (print-optimized)")
    )
  )
}

# Standard data download buttons (for UI)
data_download_buttons_ui <- function(ns_prefix = "", include_csv = TRUE) {
  ns_func <- if (ns_prefix != "") function(x) paste0(ns_prefix, x) else function(x) x
  
  buttons <- list(
    downloadButton(ns_func("download_data_excel"), "📊 Excel", class = "btn btn-success btn-sm",
                   title = "Complete Excel export with metadata")
  )
  
  if (include_csv) {
    buttons <- append(buttons, 
      downloadButton(ns_func("download_data_csv"), "📄 CSV", class = "btn btn-info btn-sm",
                     title = "CSV Export (compatible)"))
  }
  
  div(class = "download-buttons-group",
    h5("📥 Data Export:", class = "mt-3"),
    div(class = "btn-group btn-group-sm", role = "group", buttons)
  )
}

# Combined plot + data download buttons
combined_download_buttons_ui <- function(ns_prefix = "", include_csv = TRUE, show_plots = TRUE, show_data = TRUE) {
  tagList(
    if (show_plots) plot_download_buttons_ui(ns_prefix) else NULL,
    if (show_data) data_download_buttons_ui(ns_prefix, include_csv) else NULL
  )
}

# === DETRENDED CA SPECIFIC EXPORT FUNCTIONS ===

# DCA-specific Excel data sheets (PAST compatible)
create_dca_excel_sheets <- function(dca_result, filtered_data = NULL) {
  if (is.null(dca_result)) {
    return(list("Note" = data.frame(Info = "No DCA results available")))
  }
  
  sheets <- list()
  
  # ===== SHEET 1: DCA KOORDINATEN (Sites & Types) =====
  if (!is.null(dca_result$row) && !is.null(dca_result$row$coord) &&
      !is.null(dca_result$col) && !is.null(dca_result$col$coord)) {
    
    # Sites coordinates
    sites_df <- data.frame(
      Element = rownames(dca_result$row$coord),
      Element_Type = "Site",
      Status = "Active",
      dca_result$row$coord,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Types coordinates  
    types_df <- data.frame(
      Element = rownames(dca_result$col$coord),
      Element_Type = "Type", 
      Status = "Active",
      dca_result$col$coord,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    # Add supplementary sites (if available)
    if (!is.null(dca_result$row.sup) && !is.null(dca_result$row.sup$coord)) {
      suppl_sites_df <- data.frame(
        Element = rownames(dca_result$row.sup$coord),
        Element_Type = "Site",
        Status = "Supplementary",
        dca_result$row.sup$coord,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      sites_df <- rbind(sites_df, suppl_sites_df)
    }
    
    # Add supplementary types (if available)
    if (!is.null(dca_result$col.sup) && !is.null(dca_result$col.sup$coord)) {
      suppl_types_df <- data.frame(
        Element = rownames(dca_result$col.sup$coord),
        Element_Type = "Type",
        Status = "Supplementary",
        dca_result$col.sup$coord,
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      types_df <- rbind(types_df, suppl_types_df)
    }
    
    # Combine sites and types
    all_coords <- rbind(sites_df, types_df)
    sheets[["DCA_Koordinaten"]] <- all_coords[order(all_coords$Element_Type, all_coords$Status, all_coords$Element), ]
  }
  
  # ===== SHEET 2: DCA EIGENWERTE =====
  if (!is.null(dca_result$eig)) {
    eigenvalues_df <- data.frame(
      Dimension = paste0("DCA", 1:nrow(dca_result$eig)),
      Eigenwert = round(dca_result$eig[, 1], 6),
      Varianz_Prozent = round(dca_result$eig[, 2], 3),
      Kumuliert_Prozent = round(dca_result$eig[, 3], 3),
      stringsAsFactors = FALSE
    )
    
    # Add total inertia (if available)
    if (!is.null(dca_result$call) && !is.null(dca_result$call$X)) {
      eigenvalues_df$Anmerkung <- c(
        "Erste DCA-Dimension (detrended)",
        "Zweite DCA-Dimension (arch-effect entfernt)", 
        rep("Weitere DCA-Dimension", max(0, nrow(eigenvalues_df) - 2))
      )
    }
    
    sheets[["DCA_Eigenwerte"]] <- eigenvalues_df
  }
  
  # ===== SHEET 3: DCA METHODIK & PARAMETER =====
  method_name <- dca_result$dca_method %||% "linear"
  n_segments <- dca_result$dca_segments %||% NA
  
  # Dimension information
  n_dims <- if (!is.null(dca_result$row$coord)) ncol(dca_result$row$coord) else 0
  n_sites <- if (!is.null(dca_result$row$coord)) nrow(dca_result$row$coord) else 0
  n_types <- if (!is.null(dca_result$col$coord)) nrow(dca_result$col$coord) else 0
  n_suppl_sites <- if (!is.null(dca_result$row.sup$coord)) nrow(dca_result$row.sup$coord) else 0
  n_suppl_types <- if (!is.null(dca_result$col.sup$coord)) nrow(dca_result$col.sup$coord) else 0
  
  methodology_df <- data.frame(
    Parameter = c(
      "Detrending_Methode",
      "Segmente_Anzahl", 
      "Algorithmus",
      "Referenz_1",
      "Referenz_2",
      "Dimensionen_Anzahl",
      "Sites_Aktiv",
      "Types_Aktiv",
      "Sites_Supplementary", 
      "Types_Supplementary",
      "Software_Version",
      "Erstellt_am",
      "R_Package"
    ),
    Wert = c(
      switch(method_name,
        "linear" = "Linear (Residualregression)",
        "nonlinear" = "Non-linear (Segmentweise)",
        method_name
      ),
      if (!is.na(n_segments)) as.character(n_segments) else "N/A (only for non-linear)",
      "Detrended Correspondence Analysis",
      "Hill & Gauch (1980)",
      "ter Braak & Prentice (1988)",
      as.character(n_dims),
      as.character(n_sites),
      as.character(n_types),
      as.character(n_suppl_sites),
      as.character(n_suppl_types),
      "SeriARC v1.0.0",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      "FactoMineR (Basis-CA) + SeriARC (Detrending)"
    ),
    Description = c(
      "Method for arch effect correction",
      "Number of segments for non-linear detrending",
      "Statistical procedure",
      "Fundamental DCA publication",
      "Methodological advancement",
      "Number of extracted DCA dimensions",
      "Number of active sites in analysis",
      "Number of active types in analysis",
      "Number of projected sites",
      "Number of projected types",
      "Software version used",
      "Calculation timestamp",
      "R packages used"
    ),
    stringsAsFactors = FALSE
  )
  sheets[["DCA_Methodik"]] <- methodology_df
  
  # ===== SHEET 4: KORRELATIONS-ANALYSE (Original vs Detrended) =====
  if (!is.null(dca_result$dca_original_coords) && 
      !is.null(dca_result$row$coord) && !is.null(dca_result$col$coord)) {
    
    orig_sites <- dca_result$dca_original_coords$row
    detr_sites <- dca_result$row$coord
    orig_types <- dca_result$dca_original_coords$col  
    detr_types <- dca_result$col$coord
    
    correlation_data <- data.frame(
      Element_Type = character(),
      Dimension_Original = character(),
      Dimension_DCA = character(), 
      Korrelation = numeric(),
      R_Quadrat = numeric(),
      Interpretation = character(),
      stringsAsFactors = FALSE
    )
    
    max_dims <- min(ncol(orig_sites), ncol(detr_sites))
    for (i in 1:max_dims) {
      cor_val <- cor(orig_sites[, i], detr_sites[, i], use = "complete.obs")
      r_squared <- cor_val^2
      
      interpretation <- if (i == 1) {
        "First dimension: Same direction expected"
      } else if (i == 2) {
        if (abs(cor_val) < 0.1) "Excellent arch effect removal"
        else if (abs(cor_val) < 0.3) "Good removal"
        else if (abs(cor_val) < 0.5) "Moderate removal"
        else "Weak removal"
      } else {
        "Further dimension"
      }
      
      correlation_data <- rbind(correlation_data, data.frame(
        Element_Type = "Sites",
        Dimension_Original = paste0("CA", i),
        Dimension_DCA = paste0("DCA", i),
        Korrelation = round(cor_val, 4),
        R_Quadrat = round(r_squared, 4),
        Interpretation = interpretation,
        stringsAsFactors = FALSE
      ))
    }
    
    max_dims <- min(ncol(orig_types), ncol(detr_types))
    for (i in 1:max_dims) {
      cor_val <- cor(orig_types[, i], detr_types[, i], use = "complete.obs")
      r_squared <- cor_val^2
      
      interpretation <- if (i == 1) {
        "First dimension: Same direction expected"
      } else if (i == 2) {
        if (abs(cor_val) < 0.1) "Excellent arch effect removal"
        else if (abs(cor_val) < 0.3) "Good removal"
        else if (abs(cor_val) < 0.5) "Moderate removal"
        else "Weak removal"
      } else {
        "Further dimension"
      }
      
      correlation_data <- rbind(correlation_data, data.frame(
        Element_Type = "Types",
        Dimension_Original = paste0("CA", i),
        Dimension_DCA = paste0("DCA", i),
        Korrelation = round(cor_val, 4),
        R_Quadrat = round(r_squared, 4),
        Interpretation = interpretation,
        stringsAsFactors = FALSE
      ))
    }
    
    sheets[["DCA_Korrelationsanalyse"]] <- correlation_data
  }
  
  # ===== SHEET 5: SITES CONTRIBUTIONS =====
  if (!is.null(dca_result$row) && !is.null(dca_result$row$contrib)) {
    sites_contrib_df <- data.frame(
      Site = rownames(dca_result$row$contrib),
      Status = "Active",
      dca_result$row$contrib,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    contrib_cols <- colnames(dca_result$row$contrib)
    for (col in contrib_cols) {
      total_contrib <- sum(dca_result$row$contrib[, col], na.rm = TRUE)
      sites_contrib_df[[paste0(col, "_Prozent")]] <- round(
        (dca_result$row$contrib[, col] / total_contrib) * 100, 2
      )
    }
    
    sheets[["DCA_Sites_Beitraege"]] <- sites_contrib_df[order(-sites_contrib_df[[contrib_cols[1]]]), ]
  }
  
  # ===== SHEET 6: TYPES CONTRIBUTIONS =====
  if (!is.null(dca_result$col) && !is.null(dca_result$col$contrib)) {
    types_contrib_df <- data.frame(
      Type = rownames(dca_result$col$contrib),
      Status = "Active",
      dca_result$col$contrib,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    
    contrib_cols <- colnames(dca_result$col$contrib)
    for (col in contrib_cols) {
      total_contrib <- sum(dca_result$col$contrib[, col], na.rm = TRUE)
      types_contrib_df[[paste0(col, "_Prozent")]] <- round(
        (dca_result$col$contrib[, col] / total_contrib) * 100, 2
      )
    }
    
    sheets[["DCA_Types_Beitraege"]] <- types_contrib_df[order(-types_contrib_df[[contrib_cols[1]]]), ]
  }
  
  # ===== SHEET 7: ORIGINAL MATRIX (if available) =====
  if (!is.null(filtered_data)) {
    sheets[["Original_Matrix"]] <- matrix_to_excel_format(filtered_data, "Site")
    
    # Additional matrix statistics for DCA
    dca_stats <- create_statistics_sheet(filtered_data, "Detrended Correspondence Analysis")
    
    # Add DCA-specific statistics
    if (!is.null(dca_result$eig)) {
      total_inertia <- sum(dca_result$eig[, 1])
      first_two_dims <- sum(dca_result$eig[1:min(2, nrow(dca_result$eig)), 1])
      explained_by_first_two <- (first_two_dims / total_inertia) * 100
      
      additional_stats <- data.frame(
        Metric = c(
          "Total Inertia (DCA)",
          "Explained by DCA1+DCA2 (%)",
          "Detrending Method",
          "Arch effect removed"
        ),
        Wert = c(
          round(total_inertia, 4),
          round(explained_by_first_two, 1),
          dca_result$dca_method %||% "linear",
          "Ja (Hill & Gauch 1980)"
        ),
        Analyse = rep("Detrended Correspondence Analysis", 4),
        stringsAsFactors = FALSE
      )
      
      dca_stats <- rbind(dca_stats, additional_stats)
    }
    
    sheets[["Matrix_Statistiken"]] <- dca_stats
  }
  
  # ===== SHEET 8: LITERATUR & HINWEISE =====
  literature_df <- data.frame(
    Kategorie = c(
      "Grundlagen", "Grundlagen", "Methodik", "Methodik", "Software", "Software", "Anwendung", "Anwendung"
    ),
    Referenz = c(
      "Hill, M.O. & Gauch, H.G. (1980)",
      "ter Braak, C.J.F. & Prentice, I.C. (1988)", 
      "Gauch, H.G. (1982)",
      "Prentice, I.C. (1977)",
      "Hammer, Ø. & Harper, D.A.T. (2001)",
      "SeriARC Development Team (2024)",
      "Baxter, M.J. (1994)",
      "Shennan, S. (1997)"
    ),
    Titel = c(
      "Detrended correspondence analysis: an improved ordination technique",
      "A theory of gradient analysis",
      "Multivariate Analysis in Community Ecology",
      "Non-linear methods in multivariate analysis", 
      "PAST: Paleontological Statistics Software Package",
      "SeriARC: Archaeological Analysis Suite v1.0.0",
      "Exploratory Multivariate Analysis in Archaeology",
      "Quantifying Archaeology (2nd ed.)"
    ),
    Zeitschrift_Verlag = c(
      "Vegetatio 42: 47-58",
      "Advances in Ecological Research 18: 271-317",
      "Cambridge University Press",
      "Vegetatio 33: 135-152",
      "Acta Palaeontologica Polonica 46: 515-516",
      "Archaeological Computing Research",
      "Edinburgh University Press", 
      "Edinburgh University Press"
    ),
    Relevance = c(
      "Original DCA method publication",
      "Theoretical foundations of gradient analysis",
      "Standard reference for multivariate ecology",
      "Early non-linear ordination methods",
      "PAST Software (DCA reference implementation)",
      "This software implementation",
      "Multivariate methods in archaeology",
      "Quantitative archaeology methods"
    ),
    stringsAsFactors = FALSE
  )
  sheets[["Literatur_Hinweise"]] <- literature_df
  
  return(sheets)
}

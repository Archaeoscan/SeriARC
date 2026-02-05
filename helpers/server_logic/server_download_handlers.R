# server_download_handlers.R - Download Handlers
# Extracted from server.R for better maintainability
# Selection tables moved to server_selection_tables.R

source("helpers/download_components.R", local = TRUE)

# FILTER-MATRIX EXPORT
output$download_filtered_matrix <- downloadHandler(
  filename = function() {
    sprintf("SeriARC_Filtered_Matrix_%s.xlsx", Sys.Date())
  },
  content = function(file) {
    req(data_raw(), meta$data)

    tryCatch({
      filtered_matrix <- filtermod$filtered_data()

      info_df <- data.frame(
        Parameter = c(
          "Software", "Version", "Exported_at", "Original_Sites", "Original_Types",
          "Filtered_Sites", "Filtered_Types", "Active_Sites", "Active_Types",
          "Supplementary_Sites", "Supplementary_Types", "Weighting_applied"
        ),
        Value = c(
          "SeriARC", "v1.0.0", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
          nrow(data_raw()), ncol(data_raw()) - 1,
          nrow(filtered_matrix), ncol(filtered_matrix),
          sum(meta$data$sites$Selected & !meta$data$sites$Supplementary),
          sum(meta$data$types$Selected & !meta$data$types$Supplementary),
          sum(meta$data$sites$Selected & meta$data$sites$Supplementary),
          sum(meta$data$types$Selected & meta$data$types$Supplementary),
          "Yes"
        )
      )

      matrix_df <- data.frame(
        Site = rownames(filtered_matrix),
        as.data.frame(filtered_matrix),
        check.names = FALSE
      )

      sites_info <- meta$data$sites
      sites_info$Status <- ifelse(
        sites_info$Supplementary, "Supplementary",
        ifelse(sites_info$Selected, "Active", "Disabled")
      )

      types_info <- meta$data$types
      types_info$Status <- ifelse(
        types_info$Supplementary, "Supplementary",
        ifelse(types_info$Selected, "Active", "Disabled")
      )

      data_list <- list(
        "Info" = info_df,
        "Filtered_Matrix" = matrix_df,
        "Sites_Settings" = sites_info,
        "Types_Settings" = types_info
      )

      if (!is.null(mapping$meta)) {
        coord_df <- data.frame(
          Site = mapping$meta$site,
          Group = mapping$meta$group,
          Longitude = mapping$meta$lon,
          Latitude = mapping$meta$lat
        )
        coord_df <- coord_df[!is.na(coord_df$Longitude) & !is.na(coord_df$Latitude), ]
        if (nrow(coord_df) > 0) {
          data_list[["Coordinates"]] <- coord_df
        }
      }

      writexl::write_xlsx(data_list, file)

      showNotification(
        sprintf("✅ Filtered matrix exported: %d Sites x %d Types",
                nrow(filtered_matrix), ncol(filtered_matrix)),
        type = "default", duration = 5
      )

    }, error = function(e) {
      showNotification(
        sprintf("❌ Export error: %s", e$message),
        type = "error", duration = 5
      )
      stop("Export failed")
    })
  }
)

# TYPE MAPPING DOWNLOADS
output$download_type_map_html <- downloadHandler(
  filename = function() paste0("SeriARC_Type_Mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
  content  = function(file) {
    tryCatch({
      m <- typenkartierung_module$build_export_type_map()
      htmlwidgets::saveWidget(m, file = file, selfcontained = TRUE)
    }, error = function(e) {
      stop("Export failed")
    })
  }
)

output$download_type_map_png <- downloadHandler(
  filename = function() paste0("SeriARC_Type_Mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png"),
  content  = function(file) {
    tryCatch({
      m <- typenkartierung_module$build_export_type_map()
      tmp_html <- tempfile(fileext = ".html")
      htmlwidgets::saveWidget(m, file = tmp_html, selfcontained = TRUE)

      if (requireNamespace("webshot2", quietly = TRUE)) {
        webshot2::webshot(url = tmp_html, file = file, vwidth = 1400, vheight = 900, zoom = 2)
      } else if (requireNamespace("mapview", quietly = TRUE)) {
        mapview::mapshot(m, file = file)
      } else {
        stop("PNG export requires 'webshot2' (or alternatively 'mapview').")
      }
    }, error = function(e) {
      stop("Export failed")
    })
  }
)

output$download_type_map_svg <- downloadHandler(
  filename = function() paste0("SeriARC_Type_Mapping_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".svg"),
  content  = function(file) {
    tryCatch({
      p <- ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = 0, y = 0), alpha = 0) +
        ggplot2::labs(title = "Type Mapping SVG Export",
                     subtitle = "Note: Export full interactive map as HTML") +
        ggplot2::theme_minimal()

      if (svg_support$svglite) {
        svglite::svglite(file, width = 12, height = 8)
        print(p)
        dev.off()
      } else {
        ggplot2::ggsave(file, p, device = "svg", width = 12, height = 8)
      }
    }, error = function(e) {
      stop("SVG export failed")
    })
  }
)



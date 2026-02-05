# SERVER SELECTION TABLES - rhandsontables Implementation
# Focus problem fixed by switching from renderUI to rhandsontables

# SITES SELECTION TABLE using rhandsontables
output$sites_selection_table <- renderRHandsontable({
  req(meta$data, meta$data$sites)

  # Display ALL sites (Hidden only relevant for aggregation logic)
  sites_df <- meta$data$sites

  # Add additional information
  sites_df$Group <- ""
  sites_df$Coordinates <- "❌"

  if (!is.null(mapping$meta)) {
    for (i in 1:nrow(sites_df)) {
      site_match <- match(sites_df$Entity[i], mapping$meta$site)
      if (!is.na(site_match)) {
        sites_df$Group[i] <- ifelse(is.na(mapping$meta$group[site_match]), "",
                                    mapping$meta$group[site_match])

        if (!is.na(mapping$meta$lon[site_match]) && !is.na(mapping$meta$lat[site_match])) {
          sites_df$Coordinates[i] <- "✅"
        }
      }
    }
  }

  # Add statistics - ONLY over type columns (not group, lon, lat!)
  sites_df$Types_Count <- 0
  sites_df$Max_Value <- 0

  if (!is.null(data_raw()) && !is.null(meta$data$types)) {
    df <- data_raw()

    type_names <- meta$data$types$Entity
    available_types <- intersect(type_names, colnames(df))

    if (length(available_types) > 0) {
      # Extract only type columns
      type_matrix <- as.matrix(df[, available_types, drop = FALSE])
      rownames(type_matrix) <- df$Entity

      for (i in 1:nrow(sites_df)) {
        row_match <- match(sites_df$Entity[i], rownames(type_matrix))
        if (!is.na(row_match)) {
          site_row <- type_matrix[row_match, ]
          sites_df$Types_Count[i] <- sum(site_row > 0, na.rm = TRUE)
          sites_df$Max_Value[i] <- max(site_row, na.rm = TRUE)
          if (is.infinite(sites_df$Max_Value[i])) sites_df$Max_Value[i] <- 0
        }
      }
    }
  }

  # Order columns for display
  display_df <- data.frame(
    Site = sites_df$Entity,
    Selected = sites_df$Selected,
    Weight = round(sites_df$Weight, 2),
    Supplementary = sites_df$Supplementary,
    Label = sites_df$ShowLabel,
    Group = sites_df$Group,
    Types = sites_df$Types_Count,
    Max_Value = round(sites_df$Max_Value, 1),
    Coordinates = sites_df$Coordinates,
    stringsAsFactors = FALSE
  )

  rht <- rhandsontable(display_df,
                       width = "100%",
                       height = 400,
                       rowHeaders = FALSE,
                       stretchH = "all",
                       renderAllRows = TRUE,
                       fixedColumnsLeft = 1) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(colWidths = c(180, 60, 60, 80, 60, 100, 60, 70, 70)) %>%

    # Site names - not editable
    hot_col("Site", readOnly = TRUE, renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.backgroundColor = '#f8f9fa';
        td.style.fontWeight = 'bold';
      }") %>%

    # Selected - Checkbox
    hot_col("Selected", type = "checkbox", readOnly = FALSE) %>%

    # Weight - Numeric with validation
    hot_col("Weight", type = "numeric", format = "0.00",
            readOnly = FALSE, allowInvalid = FALSE) %>%

    # Supplementary - Checkbox
    hot_col("Supplementary", type = "checkbox", readOnly = FALSE) %>%

    # Label - Checkbox
    hot_col("Label", type = "checkbox", readOnly = FALSE) %>%

    # Group, Types, Max_Value, Coordinates - display only
    hot_col("Group", readOnly = TRUE, renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.backgroundColor = '#f8f9fa';
        td.style.fontStyle = 'italic';
      }") %>%
    hot_col("Types", readOnly = TRUE, type = "numeric", renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.backgroundColor = '#e8f4fd';
        td.style.textAlign = 'center';
      }") %>%
    hot_col("Max_Value", readOnly = TRUE, type = "numeric", format = "0.0", renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.backgroundColor = '#e8f4fd';
        td.style.textAlign = 'center';
      }") %>%
    hot_col("Coordinates", readOnly = TRUE, renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.backgroundColor = '#f8f9fa';
        td.style.textAlign = 'center';
        if (value === '✅') {
          td.style.color = '#28a745';
        } else {
          td.style.color = '#dc3545';
        }
      }")

  return(rht)
})

# TYPES SELECTION TABLE using rhandsontables
output$types_selection_table <- renderRHandsontable({
  req(meta$data, meta$data$types)

  # Display ALL types (Hidden only relevant for aggregation logic)
  types_df <- meta$data$types

  # Add statistics - ONLY over type columns
  types_df$Sites_Count <- 0
  types_df$Max_Value <- 0
  types_df$Total_Value <- 0

  if (!is.null(data_raw())) {
    df <- data_raw()

    type_names <- meta$data$types$Entity
    available_types <- intersect(type_names, colnames(df))

    if (length(available_types) > 0) {
      # Extract only type columns
      type_matrix <- as.matrix(df[, available_types, drop = FALSE])

      for (i in 1:nrow(types_df)) {
        col_match <- match(types_df$Entity[i], colnames(type_matrix))
        if (!is.na(col_match)) {
          type_col <- type_matrix[, col_match]
          types_df$Sites_Count[i] <- sum(type_col > 0, na.rm = TRUE)
          types_df$Max_Value[i] <- max(type_col, na.rm = TRUE)
          types_df$Total_Value[i] <- sum(type_col, na.rm = TRUE)

          if (is.infinite(types_df$Max_Value[i])) types_df$Max_Value[i] <- 0
          if (is.infinite(types_df$Total_Value[i])) types_df$Total_Value[i] <- 0
        }
      }
    }
  }

  # Order columns for display
  display_df <- data.frame(
    Type = types_df$Entity,
    Selected = types_df$Selected,
    Weight = round(types_df$Weight, 2),
    Supplementary = types_df$Supplementary,
    Label = types_df$ShowLabel,
    Sites = types_df$Sites_Count,
    Max_Value = round(types_df$Max_Value, 1),
    Total = round(types_df$Total_Value, 1),
    stringsAsFactors = FALSE
  )

  rht <- rhandsontable(display_df,
                       width = "100%",
                       height = 400,
                       rowHeaders = FALSE,
                       stretchH = "all",
                       renderAllRows = TRUE,
                       fixedColumnsLeft = 1) %>%
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE) %>%
    hot_cols(colWidths = c(180, 60, 60, 80, 60, 60, 70, 70)) %>%

    # Type names - not editable
    hot_col("Type", readOnly = TRUE, renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.TextRenderer.apply(this, arguments);
        td.style.backgroundColor = '#f8f9fa';
        td.style.fontWeight = 'bold';
      }") %>%

    # Selected - Checkbox
    hot_col("Selected", type = "checkbox", readOnly = FALSE) %>%

    # Weight - Numeric with validation
    hot_col("Weight", type = "numeric", format = "0.00",
            readOnly = FALSE, allowInvalid = FALSE) %>%

    # Supplementary - Checkbox
    hot_col("Supplementary", type = "checkbox", readOnly = FALSE) %>%

    # Label - Checkbox
    hot_col("Label", type = "checkbox", readOnly = FALSE) %>%

    # Statistics - display only
    hot_col("Sites", readOnly = TRUE, type = "numeric", renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.backgroundColor = '#e8f4fd';
        td.style.textAlign = 'center';
      }") %>%
    hot_col("Max_Value", readOnly = TRUE, type = "numeric", format = "0.0", renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.backgroundColor = '#e8f4fd';
        td.style.textAlign = 'center';
      }") %>%
    hot_col("Total", readOnly = TRUE, type = "numeric", format = "0.0", renderer = "
      function(instance, td, row, col, prop, value, cellProperties) {
        Handsontable.renderers.NumericRenderer.apply(this, arguments);
        td.style.backgroundColor = '#e8f4fd';
        td.style.textAlign = 'center';
      }")

  return(rht)
})

# OBSERVER FOR rhandsontables - Replaces the 200+ individual observers

# Sites Table Observer
observeEvent(input$sites_selection_table, {
  req(input$sites_selection_table, meta$data, meta$data$sites)

  tryCatch({
    new_data <- hot_to_r(input$sites_selection_table)

    for (i in 1:nrow(new_data)) {
      site_name <- new_data$Site[i]
      meta_idx <- which(meta$data$sites$Entity == site_name)

      if (length(meta_idx) == 1) {
        if (!is.na(new_data$Selected[i]) &&
            !identical(meta$data$sites$Selected[meta_idx], new_data$Selected[i])) {
          meta$data$sites$Selected[meta_idx] <- new_data$Selected[i]
        }

        if (!is.na(new_data$Weight[i]) && new_data$Weight[i] > 0 && new_data$Weight[i] <= 10) {
          if (!identical(round(meta$data$sites$Weight[meta_idx], 2), round(new_data$Weight[i], 2))) {
            meta$data$sites$Weight[meta_idx] <- new_data$Weight[i]
          }
        }

        if (!is.na(new_data$Supplementary[i]) &&
            !identical(meta$data$sites$Supplementary[meta_idx], new_data$Supplementary[i])) {
          meta$data$sites$Supplementary[meta_idx] <- new_data$Supplementary[i]
        }

        if (!is.na(new_data$Label[i]) &&
            !identical(meta$data$sites$ShowLabel[meta_idx], new_data$Label[i])) {
          meta$data$sites$ShowLabel[meta_idx] <- new_data$Label[i]
        }
      }
    }

    # Success notification (optional, debug mode only)
    if (exists("DEBUG_MODE") && DEBUG_MODE) {
      showNotification("🏢 Sites selection updated", type = "default", duration = 1)
    }

  }, error = function(e) {
    if (exists("DEBUG_MODE") && DEBUG_MODE) {
      showNotification(paste("⚠️ Sites update error:", e$message), type = "error", duration = 3)
    }
  })
}, ignoreInit = TRUE)

# Types Table Observer
observeEvent(input$types_selection_table, {
  req(input$types_selection_table, meta$data, meta$data$types)

  tryCatch({
    new_data <- hot_to_r(input$types_selection_table)

    for (i in 1:nrow(new_data)) {
      type_name <- new_data$Type[i]
      meta_idx <- which(meta$data$types$Entity == type_name)

      if (length(meta_idx) == 1) {
        if (!is.na(new_data$Selected[i]) &&
            !identical(meta$data$types$Selected[meta_idx], new_data$Selected[i])) {
          meta$data$types$Selected[meta_idx] <- new_data$Selected[i]
        }

        if (!is.na(new_data$Weight[i]) && new_data$Weight[i] > 0 && new_data$Weight[i] <= 10) {
          if (!identical(round(meta$data$types$Weight[meta_idx], 2), round(new_data$Weight[i], 2))) {
            meta$data$types$Weight[meta_idx] <- new_data$Weight[i]
          }
        }

        if (!is.na(new_data$Supplementary[i]) &&
            !identical(meta$data$types$Supplementary[meta_idx], new_data$Supplementary[i])) {
          meta$data$types$Supplementary[meta_idx] <- new_data$Supplementary[i]
        }

        if (!is.na(new_data$Label[i]) &&
            !identical(meta$data$types$ShowLabel[meta_idx], new_data$Label[i])) {
          meta$data$types$ShowLabel[meta_idx] <- new_data$Label[i]
        }
      }
    }

    # Success notification (optional, debug mode only)
    if (exists("DEBUG_MODE") && DEBUG_MODE) {
      showNotification("🏺 Types selection updated", type = "default", duration = 1)
    }

  }, error = function(e) {
    if (exists("DEBUG_MODE") && DEBUG_MODE) {
      showNotification(paste("⚠️ Types update error:", e$message), type = "error", duration = 3)
    }
  })
}, ignoreInit = TRUE)

# BULK ACTIONS for rhandsontables

# Bulk actions remain the same, as they directly manipulate meta$data
observeEvent(input$select_all_sites, {
  meta$data$sites$Selected <- TRUE
  showNotification("✅ All sites selected", type = "default", duration = 2)
})

observeEvent(input$deselect_all_sites, {
  meta$data$sites$Selected <- FALSE
  showNotification("❌ All sites deselected", type = "default", duration = 2)
})

observeEvent(input$select_all_types, {
  meta$data$types$Selected <- TRUE
  showNotification("✅ All types selected", type = "default", duration = 2)
})

observeEvent(input$deselect_all_types, {
  meta$data$types$Selected <- FALSE
  showNotification("❌ All types deselected", type = "default", duration = 2)
})

observeEvent(input$reset_weights, {
  meta$data$sites$Weight <- 1.0
  meta$data$types$Weight <- 1.0
  showNotification("🔄 All weights reset", type = "default", duration = 2)
})

observeEvent(input$reset_weights_sites, {
  meta$data$sites$Weight <- 1.0
  showNotification("🔄 Site weights reset", type = "default", duration = 2)
})

# Label Bulk Actions
observeEvent(input$label_all_sites, {
  meta$data$sites$ShowLabel <- TRUE
  showNotification("✅ All site labels enabled", type = "default", duration = 2)
})

observeEvent(input$label_none_sites, {
  meta$data$sites$ShowLabel <- FALSE
  showNotification("❌ All site labels disabled", type = "default", duration = 2)
})

observeEvent(input$label_all_types, {
  meta$data$types$ShowLabel <- TRUE
  showNotification("✅ All type labels enabled", type = "default", duration = 2)
})

observeEvent(input$label_none_types, {
  meta$data$types$ShowLabel <- FALSE
  showNotification("❌ All type labels disabled", type = "default", duration = 2)
})

# mod_data_filter.R

mod_data_filter_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  tagList(
    div(class = "filter-controls",
        sliderInput(ns("min_types_per_site"),
                   label = tags$span(tr("filter.min.types.site"),
                                   tags$i(class = "fa fa-info-circle",
                                         title = tr("filter.min.types.tooltip"))),
                   min = 1, max = 8, value = 2, step = 1),
        sliderInput(ns("min_sites_per_type"),
                   label = tags$span(tr("filter.min.sites.type"),
                                   tags$i(class = "fa fa-info-circle",
                                         title = tr("filter.min.sites.tooltip"))),
                   min = 1, max = 8, value = 2, step = 1),

        div(class = "alert alert-warning", style = "margin-top: 10px; padding: 8px; font-size: 0.9em;",
            icon("exclamation-triangle"),
            strong(paste0(" ", tr("filter.warning.title"), ": ")),
            tr("filter.slider.warning")
        ),

        uiOutput(ns("filter_status"))
    )
  )
}

mod_data_filter_server <- function(id, data_raw, sites_selection, types_selection, meta = NULL, filter_trigger = NULL, tr = function(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    original_counts <- reactiveValues(sites = 0, types = 0)

    observeEvent(data_raw(), {
      req(data_raw(), meta)

      df <- data_raw()

      meta$data <- list(
        sites = data.frame(
          Entity = df$Entity,
          Selected = TRUE,
          Weight = 1.0,
          Supplementary = FALSE,
          ShowLabel = TRUE,
          Hidden = FALSE,
          AggregatedInto = NA_character_,
          stringsAsFactors = FALSE
        ),
        types = data.frame(
          Entity = colnames(df)[-1],
          Selected = TRUE,
          Weight = 1.0,
          Supplementary = FALSE,
          ShowLabel = TRUE,
          Hidden = FALSE,
          AggregatedInto = NA_character_,
          stringsAsFactors = FALSE
        )
      )

    }, priority = 100)

    observe({
      req(data_raw(), meta, meta$data, meta$data$types)

      df <- data_raw()
      all_types <- colnames(df)[-1]
      existing_types <- meta$data$types$Entity
      missing_types <- setdiff(all_types, existing_types)

      if (length(missing_types) > 0) {
        new_rows <- data.frame(
          Entity = missing_types,
          Selected = TRUE,
          Weight = 1.0,
          Supplementary = FALSE,
          ShowLabel = TRUE,
          Hidden = FALSE,
          AggregatedInto = NA_character_,
          stringsAsFactors = FALSE
        )
        meta$data$types <- rbind(meta$data$types, new_rows)
      }
    })

    observeEvent(data_raw(), {
      req(data_raw())
      df <- data_raw()
      original_counts$sites <- nrow(df)
      original_counts$types <- ncol(df) - 1
    })

    observeEvent(data_raw(), {
      req(data_raw(), meta, meta$data, meta$data$sites, meta$data$types)

      min_types <- if (!is.null(input$min_types_per_site)) input$min_types_per_site else 2
      min_sites <- if (!is.null(input$min_sites_per_type)) input$min_sites_per_type else 2

      df <- data_raw()
      mat <- as.matrix(df[, -1, drop = FALSE])
      rownames(mat) <- df$Entity

      site_type_counts <- rowSums(mat > 0, na.rm = TRUE)
      type_site_counts <- colSums(mat > 0, na.rm = TRUE)

      sites_meeting_criteria <- names(site_type_counts[site_type_counts >= min_types])
      types_meeting_criteria <- names(type_site_counts[type_site_counts >= min_sites])

      meta$data$sites$Selected <- meta$data$sites$Entity %in% sites_meeting_criteria
      meta$data$types$Selected <- meta$data$types$Entity %in% types_meeting_criteria

      if (!is.null(filter_trigger)) {
        filter_trigger(filter_trigger() + 1)
      }

    }, priority = 50)

    observeEvent(list(input$min_types_per_site, input$min_sites_per_type), {
      req(data_raw(), meta, meta$data, meta$data$sites, meta$data$types)

      df <- data_raw()
      mat <- as.matrix(df[, -1, drop = FALSE])
      rownames(mat) <- df$Entity

      site_type_counts <- rowSums(mat > 0, na.rm = TRUE)
      type_site_counts <- colSums(mat > 0, na.rm = TRUE)

      sites_meeting_criteria <- names(site_type_counts[site_type_counts >= input$min_types_per_site])
      types_meeting_criteria <- names(type_site_counts[type_site_counts >= input$min_sites_per_type])

      meta$data$sites$Selected <- meta$data$sites$Entity %in% sites_meeting_criteria
      meta$data$types$Selected <- meta$data$types$Entity %in% types_meeting_criteria

      if (!is.null(filter_trigger)) {
        filter_trigger(filter_trigger() + 1)
      }

    }, ignoreInit = TRUE)

    output$filter_status <- renderUI({
      req(data_raw(), meta, meta$data)

      current_sites <- sum(meta$data$sites$Selected, na.rm = TRUE)
      current_types <- sum(meta$data$types$Selected, na.rm = TRUE)

      div(class = "alert alert-info",
          style = "margin-top: 15px; padding: 10px;",
          HTML(sprintf(tr("filter.status.sites"), current_sites, original_counts$sites)),
          tags$br(),
          HTML(sprintf(tr("filter.status.types"), current_types, original_counts$types))
      )
    })

    filtered <- reactive({
      req(data_raw(), meta, meta$data, meta$data$sites, meta$data$types)
      filter_trigger()

      df <- data_raw()
      all_types <- colnames(df)[-1]
      existing_types <- meta$data$types$Entity
      missing_types <- setdiff(all_types, existing_types)

      if (length(missing_types) > 0) {
        new_rows <- data.frame(
          Entity = missing_types,
          Selected = TRUE,
          Weight = 1.0,
          Supplementary = FALSE,
          ShowLabel = TRUE,
          Hidden = FALSE,
          AggregatedInto = NA_character_,
          stringsAsFactors = FALSE
        )
        meta$data$types <- rbind(meta$data$types, new_rows)
      }

      mat <- as.matrix(df[, -1, drop = FALSE])
      rownames(mat) <- df$Entity

      s_df <- meta$data$sites
      t_df <- meta$data$types

      sel_sites <- s_df$Entity[s_df$Selected]
      sel_types <- t_df$Entity[t_df$Selected]

      validate(need(length(sel_sites) >= 2, tr("filter.error.sites")))
      validate(need(length(sel_types) >= 2, tr("filter.error.types")))

      mat <- mat[rownames(mat) %in% sel_sites, colnames(mat) %in% sel_types, drop = FALSE]

      validate(need(nrow(mat) >= 2 && ncol(mat) >= 2, tr("filter.error.matrix")))

      w_sites <- setNames(s_df$Weight, s_df$Entity)
      w_types <- setNames(t_df$Weight, t_df$Entity)

      site_weights <- w_sites[rownames(mat)]
      type_weights <- w_types[colnames(mat)]

      site_weights[is.na(site_weights)] <- 1
      type_weights[is.na(type_weights)] <- 1

      mat <- sweep(mat, 1, site_weights, "*")
      mat <- sweep(mat, 2, type_weights, "*")

      mat[!is.finite(mat)] <- 0
      mat[is.na(mat)] <- 0

      validate(need(sum(mat, na.rm = TRUE) > 0, tr("filter.error.novalues")))

      mat
    })

    parameter_status <- reactive({
      req(sites_selection(), types_selection(), filtered())

      s_df <- sites_selection()
      t_df <- types_selection()
      filtered_mat <- filtered()

      active_sites <- sum(s_df$Selected & !s_df$Supplementary)
      suppl_sites <- sum(s_df$Selected & s_df$Supplementary)
      active_types <- sum(t_df$Selected & !t_df$Supplementary)
      suppl_types <- sum(t_df$Selected & t_df$Supplementary)

      active_site_weights <- s_df$Weight[s_df$Selected & !s_df$Supplementary]
      active_type_weights <- t_df$Weight[t_df$Selected & !t_df$Supplementary]

      mean_weight_sites <- if(length(active_site_weights) > 0) mean(active_site_weights) else 1.0
      mean_weight_types <- if(length(active_type_weights) > 0) mean(active_type_weights) else 1.0

      list(
        active_sites = active_sites,
        suppl_sites = suppl_sites,
        active_types = active_types,
        suppl_types = suppl_types,
        mean_weight_sites = mean_weight_sites,
        mean_weight_types = mean_weight_types,
        matrix_rows = nrow(filtered_mat),
        matrix_cols = ncol(filtered_mat),
        weighted_sum = sum(filtered_mat, na.rm = TRUE),
        weighted_points = sum(filtered_mat > 0, na.rm = TRUE)
      )
    })

    weights_table <- reactive({
      req(sites_selection(), types_selection())

      s_df <- sites_selection()
      t_df <- types_selection()

      create_weight_info <- function(df, typ) {
        selected <- df[df$Selected, ]
        if (nrow(selected) == 0) return(data.frame())

        data.frame(
          Element = selected$Entity,
          Typ = typ,
          Gewicht = selected$Weight,
          Status = ifelse(selected$Supplementary, "Supplementary", tr("filter.status.active")),
          Auswirkung = case_when(
            selected$Supplementary ~ tr("filter.impact.projected"),
            selected$Weight > 1.5 ~ sprintf(tr("filter.impact.increased"), (selected$Weight - 1) * 100),
            selected$Weight < 0.8 ~ sprintf(tr("filter.impact.decreased"), (1 - selected$Weight) * 100),
            TRUE ~ tr("filter.impact.normal")
          ),
          stringsAsFactors = FALSE
        )
      }

      rbind(
        create_weight_info(s_df, "Site"),
        create_weight_info(t_df, "Type")
      )
    })

    matrix_comparison <- reactive({
      req(data_raw(), sites_selection(), types_selection(), filtered())

      orig_df <- data_raw()
      orig_mat <- as.matrix(orig_df[, -1, drop = FALSE])
      orig_sum <- sum(orig_mat, na.rm = TRUE)

      status <- parameter_status()

      weight_impact <- if (orig_sum > 0) {
        round((status$weighted_sum / orig_sum - 1) * 100, 1)
      } else {
        0
      }

      list(
        orig_sites = nrow(orig_mat),
        orig_types = ncol(orig_mat),
        orig_entries = sum(orig_mat > 0, na.rm = TRUE),
        orig_sum = orig_sum,
        final_sites = status$active_sites,
        final_types = status$active_types,
        final_sum = status$weighted_sum,
        final_points = status$weighted_points,
        suppl_count = status$suppl_sites + status$suppl_types,
        weight_impact = weight_impact
      )
    })

    return(list(
      filtered_data = filtered,
      meta_data = reactive({ list(sites = sites_selection(), types = types_selection()) }),
      parameter_status = parameter_status,
      weights_table = weights_table,
      matrix_comparison = matrix_comparison
    ))
  })
}

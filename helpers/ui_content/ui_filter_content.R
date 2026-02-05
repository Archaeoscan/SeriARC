# ui_filter_content.R - Filter Tab UI Content

# FILTER ANALYSIS UI
filter_analysis_ui_simple <- function(tr = function(x) x) {
  tagList(
    # Statistics panel
    div(class = "seriarc-panel",
      h4(tr("filter.analysis.title"), class = "mt-0"),

      div(class = "seriarc-info-box",
        h5(tr("filter.dataset.overview")),
        tableOutput("filter_stats_table")
      ),

      tags$hr(),

      div(class = "seriarc-info-box",
        h5(tr("filter.quality")),
        verbatimTextOutput("filter_quality_info")
      )
    ),

    # Sites table
    div(class = "seriarc-panel", style = "margin-top: 20px;",
      h4(tr("filter.sites.select.title"), class = "mt-0"),
      p(tr("filter.sites.select.desc")),

      # Bulk Actions
      div(class = "btn-group", style = "margin-bottom: 10px;",
        actionButton("select_all_sites", tr("filter.btn.all"), class = "btn btn-sm btn-success"),
        actionButton("deselect_all_sites", tr("filter.btn.none"), class = "btn btn-sm btn-danger"),
        actionButton("label_all_sites", tr("filter.btn.labels.all"), class = "btn btn-sm btn-info"),
        actionButton("label_none_sites", tr("filter.btn.labels.none"), class = "btn btn-sm btn-warning"),
        actionButton("reset_weights_sites", tr("filter.btn.reset.weights"), class = "btn btn-sm btn-secondary")
      ),

      rHandsontableOutput("sites_selection_table")
    ),

    # Types table
    div(class = "seriarc-panel", style = "margin-top: 20px;",
      h4(tr("filter.types.select.title"), class = "mt-0"),
      p(tr("filter.types.select.desc")),

      # Bulk Actions
      div(class = "btn-group", style = "margin-bottom: 10px;",
        actionButton("select_all_types", tr("filter.btn.all"), class = "btn btn-sm btn-success"),
        actionButton("deselect_all_types", tr("filter.btn.none"), class = "btn btn-sm btn-danger"),
        actionButton("label_all_types", tr("filter.btn.labels.all"), class = "btn btn-sm btn-info"),
        actionButton("label_none_types", tr("filter.btn.labels.none"), class = "btn btn-sm btn-warning"),
        actionButton("reset_weights", tr("filter.btn.reset.weights"), class = "btn btn-sm btn-secondary")
      ),

      rHandsontableOutput("types_selection_table")
    )
  )
}

# Helper function for safe sprintf
safe_sprintf <- function(fmt, ...) {
  args <- list(...)
  args <- args[!sapply(args, function(x) is.null(x) || (length(x) == 1 && is.na(x)))]

  placeholders <- gregexpr('%[sd%]', fmt)[[1]]
  n_placeholders <- ifelse(placeholders[1] == -1, 0, length(placeholders))

  while(length(args) < n_placeholders) {
    args <- c(args, "")
  }

  tryCatch({
    do.call(sprintf, c(list(fmt), args[1:n_placeholders]))
  }, error = function(e) {
    paste("Formatting error / Formatierungsfehler:", fmt)
  })
}

# FILTER TAB UI-MODULE
output$filter_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0
  
  if (!file_loaded) {
    welcome_filter_ui(tr = tr)
  } else {
    tagList(
      fluidRow(
        column(3,
          div(class = "seriarc-panel seriarc-panel-success",
            h4(tr("filter.settings.title"), class = "mt-0"),
            p(tr("filter.settings.desc")),

            mod_data_filter_ui("filter", tr = tr),

            tags$hr(),
            h5(tr("filter.export.title")),
            downloadButton("download_filtered_matrix", tr("filter.export.matrix"),
                          class = "btn btn-success", style = "width: 100%; margin-bottom: 10px;")
          )
        ),
        
        column(9,
          filter_analysis_ui_simple(tr = tr)
        )
      )
    )
  }
})

# FILTER TAB MAIN CONTENT

# UI COMPONENTS HELPER - IMPROVED VERSION
# Adapted layout for Import Welcome Screen
# EXTENDED: Loads additional helper components
# i18n: All functions accept tr as parameter

source("helpers/plot_components.R")      # Plot configurations, colors, hover texts
source("helpers/download_components.R")   # Download handler templates
source("helpers/input_components.R")      # Input groups and UI controls

# === WELCOME UI COMPONENTS ===

# Main welcome panel for all tabs
welcome_base_ui <- function(icon = "file-excel", title = "Load Data", subtitle = "Load Excel file (.xlsx/.xls) to start") {
  div(class = "seriarc-welcome-panel",
    div(class = "seriarc-welcome-center",
      shiny::icon(icon, class = "fa-3x"),
      h2(paste0("📊 ", title)),
      h4(subtitle)
    )
  )
}

# IMPROVED Tab-specific Welcome UI for Import
# Without large icon/title - directly the format info
welcome_data_ui <- function(tr = function(x) x) {
  tagList(
    # FORMAT-INFO - Three columns side by side (without welcome_base_ui)
    div(class = "seriarc-format-panel",
      h3(tr("welcome.format.title"), style = "text-align: center; margin-bottom: 20px;"),

      # Grid with three equal-width columns
      div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr; gap: 20px; margin-bottom: 30px;",

        # Column 1: Basic structure
        div(class = "seriarc-format-box",
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff;",
          h4(tr("welcome.structure.title"), style = "color: #007bff; margin-top: 0;"),
          tags$ul(style = "margin: 0; padding-left: 20px;",
            tags$li(tags$strong(tr("welcome.structure.col1")), " ", tr("welcome.structure.col1.desc"), " ", tags$span(paste0("(", tr("welcome.structure.required"), ")"), style = "color: #dc3545;")),
            tags$li(tags$strong(tr("welcome.structure.col2")), " ", tr("welcome.structure.col2.desc")),
            tags$li(tags$strong(tr("welcome.structure.col5")), " ", tr("welcome.structure.col5.desc")),
            tags$li(tr("welcome.structure.empty"))
          )
        ),

        # Column 2: Coordinates
        div(class = "seriarc-format-box",
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745;",
          h4(tr("welcome.coords.title"), style = "color: #28a745; margin-top: 0;"),
          tags$ul(style = "margin: 0; padding-left: 20px;",
            tags$li(tags$strong(tr("welcome.coords.wgs84")), " 48.1351, 11.5820"),
            tags$li(tags$strong(tr("welcome.coords.utm")), " 691234, 5334567"),
            tags$li(tr("welcome.coords.auto")),
            tags$li(tr("welcome.coords.optional"))
          )
        ),

        # Column 3: Quality
        div(class = "seriarc-format-box",
            style = "background: #f8f9fa; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107;",
          h4(tr("welcome.quality.title"), style = "color: #ffc107; margin-top: 0;"),
          tags$ul(style = "margin: 0; padding-left: 20px;",
            tags$li(tags$strong(tr("welcome.quality.min")), " ", tr("welcome.quality.min.desc")),
            tags$li(tags$strong(tr("welcome.quality.optimal")), " ", tr("welcome.quality.optimal.desc")),
            tags$li(tr("welcome.quality.positive")),
            tags$li(tr("welcome.quality.sparse")),
            tags$li(tr("welcome.quality.consistent"))
          )
        )
      ),

      # EXAMPLE TABLE - LARGER AND MORE PROMINENT
      div(class = "seriarc-example-panel",
          style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                   padding: 25px;
                   border-radius: 12px;
                   box-shadow: 0 4px 6px rgba(0,0,0,0.1);
                   margin-top: 20px;",
        h3(tr("welcome.example.title"),
           style = "color: white;
                    text-align: center;
                    margin-top: 0;
                    margin-bottom: 20px;
                    font-size: 24px;"),

        div(class = "seriarc-example-table-wrapper",
            style = "background: white;
                     padding: 20px;
                     border-radius: 8px;
                     overflow-x: auto;",

          # Larger example table
          tags$table(class = "seriarc-example-table",
                    style = "width: 100%;
                             border-collapse: collapse;
                             font-size: 15px;",
            tags$thead(
              tags$tr(style = "background: #f8f9fa; border-bottom: 2px solid #dee2e6;",
                tags$th(style = "padding: 12px; text-align: left; font-weight: 600; color: #495057;", "Site"),
                tags$th(style = "padding: 12px; text-align: left; font-weight: 600; color: #495057;", "Phase"),
                tags$th(style = "padding: 12px; text-align: center; font-weight: 600; color: #495057;", "X_UTM"),
                tags$th(style = "padding: 12px; text-align: center; font-weight: 600; color: #495057;", "Y_UTM"),
                tags$th(style = "padding: 12px; text-align: center; font-weight: 600; color: #007bff;", "Ceramic_A"),
                tags$th(style = "padding: 12px; text-align: center; font-weight: 600; color: #007bff;", "Ceramic_B"),
                tags$th(style = "padding: 12px; text-align: center; font-weight: 600; color: #28a745;", "Metal"),
                tags$th(style = "padding: 12px; text-align: center; font-weight: 600; color: #dc3545;", "Glass")
              )
            ),
            tags$tbody(
              tags$tr(style = "border-bottom: 1px solid #dee2e6;",
                tags$td(style = "padding: 10px; font-weight: 500;", "Trench_A1"),
                tags$td(style = "padding: 10px;", "Phase_1"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "691234.5"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "5334567.8"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "15"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "8"),
                tags$td(style = "padding: 10px; text-align: center; background: #e8f5e9; font-weight: 600;", "3"),
                tags$td(style = "padding: 10px; text-align: center; background: #ffebee; font-weight: 600;", "0")
              ),
              tags$tr(style = "border-bottom: 1px solid #dee2e6;",
                tags$td(style = "padding: 10px; font-weight: 500;", "Pit_B2"),
                tags$td(style = "padding: 10px;", "Phase_2"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "691456.7"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "5334789.1"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "22"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "12"),
                tags$td(style = "padding: 10px; text-align: center; background: #e8f5e9; font-weight: 600;", "7"),
                tags$td(style = "padding: 10px; text-align: center; background: #ffebee; font-weight: 600;", "2")
              ),
              tags$tr(style = "border-bottom: 1px solid #dee2e6;",
                tags$td(style = "padding: 10px; font-weight: 500;", "Site_C3"),
                tags$td(style = "padding: 10px;", "Phase_1"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "691789.2"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "5335012.4"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "5"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "18"),
                tags$td(style = "padding: 10px; text-align: center; background: #e8f5e9; font-weight: 600;", "1"),
                tags$td(style = "padding: 10px; text-align: center; background: #ffebee; font-weight: 600;", "4")
              ),
              tags$tr(
                tags$td(style = "padding: 10px; font-weight: 500;", "Settlement_D4"),
                tags$td(style = "padding: 10px;", "Phase_3"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "692012.6"),
                tags$td(style = "padding: 10px; text-align: center; color: #6c757d;", "5335234.9"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "0"),
                tags$td(style = "padding: 10px; text-align: center; background: #e3f2fd; font-weight: 600;", "3"),
                tags$td(style = "padding: 10px; text-align: center; background: #e8f5e9; font-weight: 600;", "11"),
                tags$td(style = "padding: 10px; text-align: center; background: #ffebee; font-weight: 600;", "6")
              )
            )
          ),

          # Explanation below the table
          div(style = "margin-top: 15px; padding: 10px; background: #f8f9fa; border-radius: 6px;",
            p(style = "margin: 0; color: #495057; font-size: 14px;",
              tags$strong(tr("welcome.example.note.title")), " ",
              tr("welcome.example.note.text")
            )
          )
        )
      )

      # Download button for example data REMOVED - not needed
    )
  )
}

# Other welcome UIs with tr parameter
welcome_filter_ui <- function(tr = function(x) x) {
  welcome_base_ui("filter", tr("welcome.filter"), tr("welcome.filter.hint"))
}

welcome_analysis_ui <- function(tr = function(x) x) {
  welcome_base_ui("chart-line", tr("welcome.ca"), tr("welcome.filter.hint"))
}

welcome_bootstrap_ui <- function(tr = function(x) x) {
  welcome_base_ui("refresh", tr("welcome.bootstrap"), tr("welcome.filter.hint"))
}

welcome_3d_ui <- function(tr = function(x) x) {
  welcome_base_ui("cube", tr("welcome.3d"), tr("welcome.filter.hint"))
}

welcome_cluster_ui <- function(tr = function(x) x) {
  welcome_base_ui("bullseye", tr("welcome.cluster"), tr("welcome.filter.hint"))
}

welcome_seriation_ui <- function(tr = function(x) x) {
  welcome_base_ui("building", tr("welcome.seriation"), tr("welcome.filter.hint"))
}

welcome_battleship_ui <- function(tr = function(x) x) {
  welcome_base_ui("ship", tr("welcome.battleship"), tr("welcome.filter.hint"))
}

welcome_mapping_ui <- function(tr = function(x) x) {
  welcome_base_ui("map", tr("welcome.mapping"), tr("welcome.filter.hint"))
}

welcome_typemap_ui <- function(tr = function(x) x) {
  welcome_base_ui("map-marker", tr("welcome.typemap"), tr("welcome.filter.hint"))
}

welcome_export_ui <- function(tr = function(x) x) {
  welcome_base_ui("download", tr("welcome.export"), tr("welcome.filter.hint"))
}

# === ANALYSIS UI COMPONENTS ===

# Data summary (replaces multiple conditionalPanels) - EQUAL HEIGHT FOR TEXT BOXES
data_summary_ui <- function(tr = function(x) x) {
  tagList(
    # EQUAL HEIGHT TEXT BOXES with CSS-Grid
    div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 20px; margin-bottom: 20px;",

      # Data summary box
      div(class = "seriarc-summary-box seriarc-summary-data",
          style = "background: #e8f5e8; padding: 20px; border-radius: 8px; border-left: 4px solid #28a745; min-height: 200px; display: flex; flex-direction: column;",
        h4(tr("summary.title"),
           style = "margin-top: 0; margin-bottom: 15px; color: #155724; font-weight: 600;"),
        div(style = "flex: 1; overflow-y: auto;",
          verbatimTextOutput("count_summary")
        )
      ),

      # Data quality box
      div(class = "seriarc-summary-box seriarc-summary-quality",
          style = "background: #fff3cd; padding: 20px; border-radius: 8px; border-left: 4px solid #ffc107; min-height: 200px; display: flex; flex-direction: column;",
        h4(tr("summary.quality.title"),
           style = "margin-top: 0; margin-bottom: 15px; color: #856404; font-weight: 600;"),
        div(style = "flex: 1; overflow-y: auto;",
          verbatimTextOutput("data_quality")
        )
      )
    ),

    # Data preview panel
    div(class = "seriarc-panel",
      h4(tr("summary.preview"), class = "mt-0"),
      DT::dataTableOutput("raw_data_preview")
    )
  )
}

# Filter-Analysis UI - 4 SEPARATE TEXT BOXES side by side - ALL EQUAL HEIGHT
filter_analysis_ui_simple <- function(tr = function(x) x) {
  tagList(
    # 4 EQUAL HEIGHT TEXT BOXES with CSS-Grid instead of fluidRow
    div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 15px; margin-bottom: 20px;",

      # Filter effects box
      div(class = "seriarc-filter-box seriarc-filter-primary",
          style = "background: #e3f2fd; padding: 15px; border-radius: 8px; border-left: 4px solid #007bff; min-height: 180px; display: flex; flex-direction: column;",
        h5(tr("filter.title.summary"),
           style = "margin-top: 0; margin-bottom: 12px; color: #004085; font-weight: 600;"),
        div(style = "flex: 1; overflow-y: auto;",
          verbatimTextOutput("filter_summary_col1")
        )
      ),

      # Data retention box
      div(class = "seriarc-filter-box seriarc-filter-info",
          style = "background: #d1ecf1; padding: 15px; border-radius: 8px; border-left: 4px solid #17a2b8; min-height: 180px; display: flex; flex-direction: column;",
        h5(tr("filter.title.retention"),
           style = "margin-top: 0; margin-bottom: 12px; color: #0c5460; font-weight: 600;"),
        div(style = "flex: 1; overflow-y: auto;",
          verbatimTextOutput("filter_summary_col2")
        )
      ),

      # Matrix-Status Box
      div(class = "seriarc-filter-box seriarc-filter-success",
          style = "background: #d4edda; padding: 15px; border-radius: 8px; border-left: 4px solid #28a745; min-height: 180px; display: flex; flex-direction: column;",
        h5(tr("filter.title.matrix"),
           style = "margin-top: 0; margin-bottom: 12px; color: #155724; font-weight: 600;"),
        div(style = "flex: 1; overflow-y: auto;",
          verbatimTextOutput("filter_summary_col3")
        )
      ),

      # Weighting box - NOW EXACTLY EQUAL HEIGHT
      div(class = "seriarc-filter-box seriarc-filter-warning",
          style = "background: #fff3cd; padding: 15px; border-radius: 8px; border-left: 4px solid #ffc107; min-height: 180px; display: flex; flex-direction: column;",
        h5(tr("filter.title.weights"),
           style = "margin-top: 0; margin-bottom: 12px; color: #856404; font-weight: 600;"),
        div(style = "flex: 1; overflow-y: auto;",
          verbatimTextOutput("filter_summary_col4")
        )
      )
    ),

    # Assessment as separate box
    fluidRow(
      column(12,
        div(class = "seriarc-panel",
          verbatimTextOutput("filter_recommendation")
        )
      )
    ),
    br(),
    fluidRow(
      column(6,
        div(class = "seriarc-panel",
          h4(tr("filter.sites.title"), class = "mt-0"),
          div(style = "margin-bottom: 10px;",
            div(class = "btn-group btn-group-sm", style = "margin-right: 10px;",
              actionButton("select_all_sites", tr("filter.btn.select.all"), class = "btn btn-success btn-sm"),
              actionButton("deselect_all_sites", tr("filter.btn.select.none"), class = "btn btn-danger btn-sm")
            ),
            div(class = "btn-group btn-group-sm",
              actionButton("label_all_sites", tr("filter.btn.label.all"), class = "btn btn-info btn-sm"),
              actionButton("label_none_sites", tr("filter.btn.label.none"), class = "btn btn-secondary btn-sm")
            )
          ),
          rHandsontableOutput("sites_selection_table")
        )
      ),
      column(6,
        div(class = "seriarc-panel",
          h4(tr("filter.types.title"), class = "mt-0"),
          div(style = "margin-bottom: 10px;",
            div(class = "btn-group btn-group-sm", style = "margin-right: 10px;",
              actionButton("select_all_types", tr("filter.btn.select.all"), class = "btn btn-success btn-sm"),
              actionButton("deselect_all_types", tr("filter.btn.select.none"), class = "btn btn-danger btn-sm"),
              actionButton("reset_weights", tr("filter.btn.reset.weights"), class = "btn btn-warning btn-sm")
            ),
            div(class = "btn-group btn-group-sm",
              actionButton("label_all_types", tr("filter.btn.label.all"), class = "btn btn-info btn-sm"),
              actionButton("label_none_types", tr("filter.btn.label.none"), class = "btn btn-secondary btn-sm")
            )
          ),
          rHandsontableOutput("types_selection_table")
        )
      )
    )
  )
}



# === HELPER FUNCTIONS ===

debug_panel_ui <- function(debug_content_id, debug_enabled = FALSE) {
  if (!debug_enabled) return(NULL)

  div(class = "seriarc-debug-panel",
    h4("🔧 Debug Information"),
    verbatimTextOutput(debug_content_id)
  )
}

# Conditional UTM Settings (modular)
utm_settings_ui <- function(ns, utm_detected = FALSE, tr = function(x) x) {
  if (!utm_detected) return(NULL)

  div(class = "seriarc-panel-warning",
    h5(tr("tooltip.info")),
    fluidRow(
      column(6, numericInput(ns("utm_zone"), tr("utm.zone.label"), min=1, max=60, value=32, step=1)),
      column(6, selectInput(ns("utm_hemi"), tr("utm.hemisphere.label"),
                           choices = setNames(c("N","S"), c(tr("import.utm.north"), tr("import.utm.south"))),
                           selected="N"))
    )
  )
}

# Tooltip-Helper
tooltip_info <- function(text) {
  tags$i(class = "fa fa-info-circle",
         style = "margin-left: 5px; color: #6c757d; cursor: help;",
         title = text)
}

# === EXTENDED DATA-TABLE CONFIGURATIONS ===

# Standard DataTable configuration for SeriARC
# tr parameter for i18n
seriarc_datatable_config <- function(data, caption_text = "SeriARC Data Analysis",
                                    page_length = 15, scroll_x = TRUE, tr = function(x) x) {
  DT::datatable(
    data,
    options = list(
      pageLength = page_length,
      scrollX = scroll_x,
      columnDefs = list(list(className = 'dt-center', targets = "_all")),
      language = list(
        search = tr("datatable.search"),
        lengthMenu = tr("datatable.show"),
        info = tr("datatable.info"),
        paginate = list(
          "first" = tr("datatable.first"),
          "last" = tr("datatable.last"),
          "next" = tr("datatable.next"),
          "previous" = tr("datatable.previous")
        )
      )
    ),
    rownames = FALSE,
    class = 'cell-border stripe hover',
    caption = caption_text
  )
}

# Cluster table with color background
cluster_datatable_with_colors <- function(data, cluster_col = "Cluster_Nr", caption_text = "Cluster Assignments", tr = function(x) x) {
  if (!cluster_col %in% colnames(data)) {
    return(seriarc_datatable_config(data, caption_text, tr = tr))
  }

  unique_clusters <- unique(data[[cluster_col]])
  base_colors <- c("#e3f2fd", "#ffebee", "#e8f5e8", "#fff3e0", "#f3e5f5", "#e0f2f1")
  cluster_colors <- rep(base_colors, length.out = length(unique_clusters))
  names(cluster_colors) <- as.character(unique_clusters)

  dt <- seriarc_datatable_config(data, caption_text, tr = tr)

  if (length(cluster_colors) == length(unique_clusters)) {
    dt <- dt %>% DT::formatStyle(
      cluster_col,
      backgroundColor = DT::styleEqual(unique_clusters, cluster_colors[as.character(unique_clusters)])
    )
  }

  # Status formatting if available
  if ("Status" %in% colnames(data)) {
    dt <- dt %>% DT::formatStyle(
      'Status',
      backgroundColor = DT::styleEqual(c('Active', 'Supplementary'), c('#ffffff', '#f8f9fa')),
      fontWeight = DT::styleEqual('Supplementary', 'italic')
    )
  }

  dt
}

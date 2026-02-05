# mod_meta_editor.R
# Entity management for Sites/Types (Supplementary classification according to Greenacre 2007)

mod_meta_editor_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      type = "tabs",
      tabPanel(
        tr("meta.sites.title"),
        br(),
        div(style="background:#e3f2fd; padding:10px; border-radius:5px; margin-bottom:10px;",
            p(tr("meta.sites.hint"),
              style="margin:0; font-size:12px; color:#1976d2;")),
        fluidRow(
          column(3, actionButton(ns("clear_suppl_sites"), tr("meta.sites.all.active"), class = "btn btn-warning btn-sm", style="width:100%;")),
          column(3, actionButton(ns("auto_suppl_sites"),  tr("meta.sites.auto.suppl"), class = "btn btn-info btn-sm", style="width:100%;")),
          column(6, div(style="padding-top:6px; font-weight:bold;", textOutput(ns("sites_selection_info"))))
        ),
        br(),
        rHandsontableOutput(ns("site_table"))
      ),
      tabPanel(
        tr("meta.types.title"),
        br(),
        div(style="background:#e8f5e8; padding:10px; border-radius:5px; margin-bottom:10px;",
            p(tr("meta.types.hint"),
              style="margin:0; font-size:12px; color:#27ae60;")),
        fluidRow(
          column(3, actionButton(ns("clear_suppl_types"), tr("meta.types.all.active"), class = "btn btn-warning btn-sm", style="width:100%;")),
          column(3, actionButton(ns("auto_suppl_types"),  tr("meta.sites.auto.suppl"), class = "btn btn-info btn-sm", style="width:100%;")),
          column(6, div(style="padding-top:6px; font-weight:bold;", textOutput(ns("types_selection_info"))))
        ),
        br(),
        rHandsontableOutput(ns("type_table"))
      )
    )
  )
}

mod_meta_editor_server <- function(id, meta, data_raw, tr = function(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # Handsontable-Rendering (Entity-Klassifikation)
    render_entity_table <- function(data, output_id) {
      req(data)
      n_rows <- nrow(data)
      height <- min(1200, max(450, n_rows * 30 + 150))
      if (n_rows > 30) height <- n_rows * 30 + 150

      rhandsontable(data, height = height, stretchH = "all") %>%
        hot_col("Entity", readOnly = TRUE, width = 150) %>%
        hot_col("Selected", type = "checkbox", width = 80) %>%
        hot_col("Weight", type = "numeric", format = "0.00", width = 80) %>%
        hot_col("Supplementary", type = "checkbox", width = 100) %>%
        hot_table(highlightCol = TRUE, highlightRow = TRUE)
    }

    output$site_table <- renderRHandsontable({ render_entity_table(meta$data$sites, "site") })
    output$type_table <- renderRHandsontable({ render_entity_table(meta$data$types, "type") })

    # Handsontable-Updates
    observeEvent(input$site_table, { if (!is.null(input$site_table)) meta$data$sites <- hot_to_r(input$site_table) })
    observeEvent(input$type_table, { if (!is.null(input$type_table)) meta$data$types <- hot_to_r(input$type_table) })

    # Button-Handler (Supplementary-Management)
    observeEvent(input$clear_suppl_sites, {
      req(meta$data$sites)
      meta$data$sites$Supplementary <- FALSE
      showNotification(tr("meta.status.all.sites"), type = "default", duration = 2)
    })

    observeEvent(input$clear_suppl_types, {
      req(meta$data$types)
      meta$data$types$Supplementary <- FALSE
      showNotification(tr("meta.status.all.types"), type = "default", duration = 2)
    })

    observeEvent(input$auto_suppl_sites, {
      req(meta$data$sites, data_raw())
      df <- data_raw(); mat <- as.matrix(df[, -1, drop = FALSE]); rownames(mat) <- df$Entity
      site_counts <- rowSums(mat > 0, na.rm = TRUE)
      rare_sites <- names(site_counts[site_counts <= quantile(site_counts, 0.2, na.rm = TRUE)])
      meta$data$sites$Supplementary <- meta$data$sites$Entity %in% rare_sites
      showNotification(sprintf(tr("meta.status.auto.sites"), length(rare_sites)), type = "default", duration = 3)
    })

    observeEvent(input$auto_suppl_types, {
      req(meta$data$types, data_raw())
      df <- data_raw(); mat <- as.matrix(df[, -1, drop = FALSE])
      type_counts <- colSums(mat > 0, na.rm = TRUE)
      rare_types <- names(type_counts[type_counts <= quantile(type_counts, 0.2, na.rm = TRUE)])
      meta$data$types$Supplementary <- meta$data$types$Entity %in% rare_types
      showNotification(sprintf(tr("meta.status.auto.types"), length(rare_types)), type = "default", duration = 3)
    })

    # Selection-Info
    output$sites_selection_info <- renderText({
      req(meta$data$sites)
      selected <- sum(meta$data$sites$Selected)
      suppl <- sum(meta$data$sites$Selected & meta$data$sites$Supplementary)
      sprintf(tr("meta.status.summary"), selected - suppl, suppl)
    })

    output$types_selection_info <- renderText({
      req(meta$data$types)
      selected <- sum(meta$data$types$Selected)
      suppl <- sum(meta$data$types$Selected & meta$data$types$Supplementary)
      sprintf(tr("meta.status.summary"), selected - suppl, suppl)
    })

    # Reaktive Outputs
    sites_selection <- reactive({ req(meta$data$sites); meta$data$sites })
    types_selection <- reactive({ req(meta$data$types); meta$data$types })
    selection_stats <- reactive({
      req(meta$data)
      list(
        active_sites = sum(meta$data$sites$Selected & !meta$data$sites$Supplementary),
        suppl_sites  = sum(meta$data$sites$Selected &  meta$data$sites$Supplementary),
        active_types = sum(meta$data$types$Selected & !meta$data$types$Supplementary),
        suppl_types  = sum(meta$data$types$Selected &  meta$data$types$Supplementary)
      )
    })

    return(list(
      sites_selection = sites_selection,
      types_selection = types_selection,
      selection_stats = selection_stats
    ))
  })
}

# UI CONTENT - AGGREGATION TAB

# Aggregation content UI
aggregation_content_ui <- function(tr = function(x) x) {
  tagList(
    # ==== SITE-AGGREGATION ====
    fluidRow(
      column(12,
        div(class = "seriarc-panel",
          h3(tr("agg.site.title"), class = "mt-0"),
          p(tr("agg.site.desc")),

          # Action Buttons
          div(style = "margin-bottom: 20px;",
            actionButton("add_site_aggregation",
                        tr("agg.site.add"),
                        class = "btn btn-primary btn-lg",
                        icon = icon("plus")),
            actionButton("reset_site_aggregations",
                        tr("agg.reset.all"),
                        class = "btn btn-danger",
                        icon = icon("trash"),
                        style = "margin-left: 10px;")
          )
        )
      )
    ),

    # Site-Aggregations-Liste
    fluidRow(
      column(12,
        uiOutput("site_aggregations_list_ui")
      )
    ),

    # Trennlinie
    fluidRow(
      column(12,
        tags$hr(style = "margin: 40px 0; border-top: 2px solid #dee2e6;")
      )
    ),

    # ==== TYPE-AGGREGATION ====
    fluidRow(
      column(12,
        div(class = "seriarc-panel",
          h3(tr("agg.type.title"), class = "mt-0"),
          p(tr("agg.type.desc")),

          # Action Buttons
          div(style = "margin-bottom: 20px;",
            actionButton("add_type_aggregation",
                        tr("agg.type.add"),
                        class = "btn btn-primary btn-lg",
                        icon = icon("plus")),
            actionButton("reset_type_aggregations",
                        tr("agg.reset.all"),
                        class = "btn btn-danger",
                        icon = icon("trash"),
                        style = "margin-left: 10px;")
          )
        )
      )
    ),

    # Type-Aggregations-Liste
    fluidRow(
      column(12,
        uiOutput("type_aggregations_list_ui")
      )
    ),

    # Summary-Box
    fluidRow(
      column(12,
        div(class = "seriarc-panel",
          style = "background: #e8f5e9; border-left: 4px solid #28a745;",
          h4(tr("agg.status"), class = "mt-0"),
          verbatimTextOutput("aggregation_summary")
        )
      )
    )
  )
}

# Modal for new Site Aggregation
site_aggregation_modal_ui <- function(available_sites, tr = function(x) x) {
  modalDialog(
    title = tr("agg.modal.site.title"),
    size = "l",

    fluidRow(
      column(12,
        textInput("site_agg_name",
                 tr("agg.modal.site.name"),
                 placeholder = tr("agg.modal.site.placeholder"),
                 width = "100%")
      )
    ),

    fluidRow(
      column(12,
        h4(tr("agg.modal.site.select")),
        p(tr("agg.modal.site.hint"),
          style = "color: #6c757d; font-size: 0.9em;")
      )
    ),

    fluidRow(
      column(12,
        div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #dee2e6; padding: 15px; border-radius: 4px;",
          checkboxGroupInput("site_agg_sites_selected",
                           label = NULL,
                           choices = available_sites,
                           width = "100%")
        )
      )
    ),

    footer = tagList(
      actionButton("site_agg_cancel", tr("agg.btn.cancel"), class = "btn btn-secondary"),
      actionButton("site_agg_confirm", tr("agg.btn.create"), class = "btn btn-success")
    )
  )
}

# Modal for new Type Aggregation
type_aggregation_modal_ui <- function(available_types, tr = function(x) x) {
  modalDialog(
    title = tr("agg.modal.type.title"),
    size = "l",

    fluidRow(
      column(12,
        textInput("type_agg_name",
                 tr("agg.modal.type.name"),
                 placeholder = tr("agg.modal.type.placeholder"),
                 width = "100%")
      )
    ),

    fluidRow(
      column(12,
        h4(tr("agg.modal.type.select")),
        p(tr("agg.modal.type.hint"),
          style = "color: #6c757d; font-size: 0.9em;")
      )
    ),

    fluidRow(
      column(12,
        div(style = "max-height: 400px; overflow-y: auto; border: 1px solid #dee2e6; padding: 15px; border-radius: 4px;",
          checkboxGroupInput("type_agg_types_selected",
                           label = NULL,
                           choices = available_types,
                           width = "100%")
        )
      )
    ),

    footer = tagList(
      actionButton("type_agg_cancel", tr("agg.btn.cancel"), class = "btn btn-secondary"),
      actionButton("type_agg_confirm", tr("agg.btn.create"), class = "btn btn-success")
    )
  )
}

# Einzelne Site-Aggregations-Box
site_aggregation_box_ui <- function(agg_id, agg_name, sites_list, tr = function(x) x) {
  div(class = "seriarc-panel",
      id = paste0("site_agg_box_", agg_id),
      style = "background: #f8f9fa; border-left: 4px solid #007bff; margin-bottom: 15px;",

    fluidRow(
      column(10,
        h4(style = "margin-top: 0; color: #007bff;",
           icon("layer-group"), " ", agg_name)
      ),
      column(2,
        div(style = "text-align: right;",
          actionButton(paste0("delete_site_agg_", agg_id),
                      tr("agg.box.delete"),
                      class = "btn btn-danger btn-sm",
                      onclick = paste0("Shiny.setInputValue('delete_site_agg_id', '", agg_id, "', {priority: 'event'});"))
        )
      )
    ),

    fluidRow(
      column(12,
        h5(tr("agg.box.sites"), style = "color: #495057; margin-top: 10px;"),
        tags$ul(style = "margin-bottom: 0;",
          lapply(sites_list, function(site) {
            tags$li(style = "color: #6c757d;", site)
          })
        )
      )
    )
  )
}

# Einzelne Type-Aggregations-Box
type_aggregation_box_ui <- function(agg_id, agg_name, types_list, tr = function(x) x) {
  div(class = "seriarc-panel",
      id = paste0("type_agg_box_", agg_id),
      style = "background: #f8f9fa; border-left: 4px solid #6f42c1; margin-bottom: 15px;",

    fluidRow(
      column(10,
        h4(style = "margin-top: 0; color: #6f42c1;",
           icon("layer-group"), " ", agg_name)
      ),
      column(2,
        div(style = "text-align: right;",
          actionButton(paste0("delete_type_agg_", agg_id),
                      tr("agg.box.delete"),
                      class = "btn btn-danger btn-sm",
                      onclick = paste0("Shiny.setInputValue('delete_type_agg_id', '", agg_id, "', {priority: 'event'});"))
        )
      )
    ),

    fluidRow(
      column(12,
        h5(tr("agg.box.types"), style = "color: #495057; margin-top: 10px;"),
        tags$ul(style = "margin-bottom: 0;",
          lapply(types_list, function(type) {
            tags$li(style = "color: #6c757d;", type)
          })
        )
      )
    )
  )
}

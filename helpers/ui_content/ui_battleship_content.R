# ui_battleship_content.R - Battleship Tab UI Content

# BATTLESHIP MAIN CONTENT
output$battleship_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_battleship_ui(tr = tr)
  } else {
    tagList(
      # JavaScript for Bootstrap collapse chevron animations
      tags$script(HTML("
        $(document).on('shown.bs.collapse', '.battleship-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-down').addClass('glyphicon-chevron-up');
        });
        $(document).on('hidden.bs.collapse', '.battleship-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-up').addClass('glyphicon-chevron-down');
        });
      ")),

      fluidRow(
        column(4,
          div(class = "seriarc-panel",
            h4(tr("battleship.settings"), class="mt-0", style = "margin-bottom: 10px;"),

            # ===================== PANEL 0: TYPE SELECTION (open by default) =====================
            div(class = "panel panel-success", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#battleship_panel_types",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("battleship.panel.types")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "battleship_panel_types", class = "panel-collapse collapse in battleship-collapse",
                div(class = "panel-body", style = "padding: 12px;",
                  uiOutput("bship_types_ui")
                )
              )
            ),

            # ===================== PANEL 1: CURVE STYLE (open by default) =====================
            div(class = "panel panel-primary", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#battleship_panel_curve",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("battleship.panel.curve")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "battleship_panel_curve", class = "panel-collapse collapse in battleship-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  checkboxInput("bship_curve_smooth", tr("battleship.curve.smooth"), TRUE),
                  selectInput("bship_curve_method", tr("battleship.curve.method"),
                              choices = c("loess", "spline", "gaussian"), selected = "loess"),
                  sliderInput("bship_curve_param", tr("battleship.curve.smoothing"),
                              min = 0.05, max = 1.0, value = 0.3, step = 0.05),
                  checkboxInput("bship_norm",
                               HTML(paste0(tr("battleship.normalize"),
                                    ' <span style="color:#3498db; cursor:help;" title="',
                                    tr("battleship.normalize.tooltip"), '">ⓘ</span>')),
                               TRUE),

                  # Aggregation section
                  div(class = "seriarc-panel-success", style = "margin-top: 10px;",
                    h6(tr("battleship.aggregation"), style = "margin-top: 0; margin-bottom: 8px;"),
                    checkboxInput("bship_aggregate_clusters", tr("battleship.agg.sites.clusters"), FALSE),
                    checkboxInput("bship_aggregate_types", tr("battleship.agg.types.clusters"), FALSE),
                    checkboxInput("bship_aggregate_type_groups", tr("battleship.agg.sites.groups"), FALSE)
                  )
                )
              )
            ),

            # ===================== PANEL 2: DISPLAY OPTIONS (open by default) =====================
            div(class = "panel panel-info", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#battleship_panel_display",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("battleship.panel.display")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "battleship_panel_display", class = "panel-collapse collapse in battleship-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  checkboxInput("bship_flip_axes", tr("battleship.flip.axes"), FALSE),
                  sliderInput("bship_halfwidth", tr("battleship.width"),
                              min = 0.2, max = 2.0, value = 0.8, step = 0.05),
                  sliderInput("bship_alpha", tr("battleship.alpha"),
                              min = 0.1, max = 1.0, value = 0.6, step = 0.05),
                  sliderInput("bship_label_chars", tr("battleship.label.chars"),
                              min = 3, max = 30, value = 20, step = 1)
                )
              )
            )
          )
        ),
        column(8,
          div(class = "seriarc-panel", plotlyOutput("battleship_plot", height = "720px")),
          br(),
          fluidRow(
            column(3, downloadButton("download_battleship_png", "PNG", class="btn btn-info btn-sm", style="width:100%;")),
            column(3, downloadButton("download_battleship_svg", "SVG", class="btn btn-secondary btn-sm", style="width:100%;")),
            column(3, downloadButton("download_battleship_pdf", "PDF", class="btn btn-danger btn-sm", style="width:100%;")),
            column(3, downloadButton("download_battleship_html", "HTML", class="btn btn-success btn-sm", style="width:100%;"))
          ),
          br(),
          fluidRow(
            column(6, downloadButton("download_battleship_data", "Excel", class="btn btn-warning btn-sm", style="width:100%;")),
            column(6, downloadButton("download_battleship_data_csv", "CSV", class="btn btn-info btn-sm", style="width:100%;"))
          )
        )
      )
    )
  }
})

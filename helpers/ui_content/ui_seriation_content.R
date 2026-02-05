# ui_seriation_content.R - Seriation Tab UI Content

# SERIATION MAIN CONTENT
output$seriation_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_seriation_ui(tr = tr)
  } else {
    tagList(
      fluidRow(
        column(3,
          div(class = "seriarc-panel",
            div(style="display: flex; align-items: center; gap: 5px;",
              h4(tr("seriation.settings.title"), class="mt-0"),
              tags$span(
                icon("info-circle"),
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                title = tr("seriation.tooltip.methods"),
                style = "cursor: help; color: #3498db; font-size: 16px;"
              )
            ),

            selectInput("seriation_method", tr("seriation.method"),
              choices = setNames(
                c("CA", "RA"),
                c(tr("seriation.method.ca"), tr("seriation.method.ra"))
              ), selected = "CA"),
            
            div(style="display: flex; align-items: center; gap: 5px;",
              h5(tr("seriation.preprocessing")),
              tags$span(
                icon("info-circle"),
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                title = tr("seriation.tooltip.transform"),
                style = "cursor: help; color: #3498db; font-size: 14px;"
              )
            ),
            selectInput("seriation_transform", NULL,
              choices = setNames(
                c("none", "binary", "relative", "log"),
                c(tr("seriation.transform.none"), tr("seriation.transform.binary"), tr("seriation.transform.relative"), tr("seriation.transform.log"))
              ), selected = "none"),

            conditionalPanel(
              condition = "input.seriation_transform == 'binary'",
              numericInput("binary_threshold", tr("seriation.binary.threshold"), value = 0, min = 0, step = 0.1)
            ),

            div(style="display: flex; align-items: center; gap: 5px;",
              h5(tr("seriation.visualization")),
              tags$span(
                icon("info-circle"),
                `data-toggle` = "tooltip",
                `data-placement` = "right",
                title = tr("seriation.tooltip.plottype"),
                style = "cursor: help; color: #3498db; font-size: 14px;"
              )
            ),
            selectInput("seriation_plot_type", tr("seriation.plot.type"),
              choices = setNames(
                c("ford", "bubble", "heatmap", "battleship"),
                c(tr("seriation.plot.ford"), tr("seriation.plot.bubble"), tr("seriation.plot.heatmap"), tr("seriation.plot.battleship"))
              ),
              selected = "ford"),

            conditionalPanel(
              condition = "input.seriation_plot_type == 'heatmap'",
              selectInput("heatmap_palette", tr("seriation.heatmap.palette"),
                choices = HEATMAP_PALETTES,
                selected = "viridis")
            ),

            conditionalPanel(
              condition = "input.seriation_plot_type == 'bubble'",
              checkboxInput("bubble_show_legend", tr("seriation.bubble.legend"), TRUE)
            ),

            conditionalPanel(
              condition = "input.seriation_plot_type == 'bubble' || input.seriation_plot_type == 'ford' || input.seriation_plot_type == 'heatmap'",
              checkboxInput("transpose_matrix", tr("seriation.transpose"), FALSE)
            ),

            checkboxInput("show_site_names", tr("seriation.show.sites"), FALSE),
            checkboxInput("show_type_names", tr("seriation.show.types"), FALSE),
            sliderInput("ser_label_chars", tr("seriation.label.chars"),
                        min = MIN_LABEL_CHARS, max = MAX_LABEL_CHARS,
                        value = DEFAULT_LABEL_CHARS, step = 1),

            actionButton("refresh2", tr("seriation.btn.calculate"), class = "btn btn-warning", width = "100%")
          )
        ),
        
        column(9,
          div(class = "seriarc-panel", style="padding: 0;",
            div(style="height: 800px; overflow-y: auto; overflow-x: auto; padding: 15px;",
              plotlyOutput("seriation_plotly")
            )
          ),
          fluidRow(
            column(3, downloadButton("download_seriation_plot_png", "📱 PNG", class="btn btn-info btn-sm", style="width:100%;")),
            column(3, downloadButton("download_seriation_plot_svg", "📄 SVG", class="btn btn-secondary btn-sm", style="width:100%;")),
            column(3, downloadButton("download_seriation_plot_pdf", "📄 PDF", class="btn btn-danger btn-sm", style="width:100%;")),
            column(3, downloadButton("download_seriation_plot_html", "🌐 HTML", class="btn btn-success btn-sm", style="width:100%;"))
          ),
          br(),
          fluidRow(
            column(12, downloadButton("download_seriation_report", "📋 Excel Report", class="btn btn-warning btn-sm", style="width:100%;"))
          )
        )
      ),
      br(),
      fluidRow(
        column(6, div(class="seriarc-panel",
          h4(tr("seriation.order.title"), class="mt-0"),
          checkboxInput("show_full_seriation_order", tr("seriation.order.full"), FALSE),
          div(style = "font-size: 14px; line-height: 1.6;",
            textOutput("seriation_order")
          ))),
        column(6, div(class="seriarc-panel",
          h4(tr("seriation.metrics.title"), class="mt-0"),
          div(style = "font-size: 14px; line-height: 1.6;",
            textOutput("seriation_metrics")
          )))
      )
    )
  }
})

# ui_detrended_ca_content.R - Detrended CA Tab UI Content

# DETRENDED CA TAB UI MODULE
output$detrended_ca_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0
  
  if (!file_loaded) {
    welcome_analysis_ui(tr = tr)
  } else {
    tagList(
      fluidRow(
        column(4,
          div(class = "seriarc-panel",
            h4(tr("dca.settings"), class = "mt-0"),

            div(class = "seriarc-analysis-focus",
              h5(tr("dca.analysis.focus"), tooltip_info(tr("dca.analysis.focus.tooltip"))),
              selectInput("dca_analysis_focus", NULL,
                choices = setNames(
                  c("both", "sites_only", "types_only"),
                  c(tr("dca.focus.both"), tr("dca.focus.sites.only"), tr("dca.focus.types.only"))
                ),
                selected = "both"
              )
            ),

            div(class = "seriarc-detrending-method",
              h5(tr("dca.method.title"), tooltip_info(tr("dca.method.tooltip"))),
              radioButtons("dca_method", NULL,
                choices = setNames(
                  c("linear", "nonlinear"),
                  c(tr("dca.method.linear"), tr("dca.method.nonlinear"))
                ),
                selected = "linear", inline = FALSE
              ),

              conditionalPanel(
                condition = "input.dca_method == 'nonlinear'",
                numericInput("dca_segments",
                           div(tr("dca.segments"), tooltip_info(tr("dca.segments.tooltip"))),
                           value = 26, min = 5, max = 100, step = 1)
              )
            ),

            h5(tr("dca.dimensions"), tooltip_info(tr("dca.dimensions.tooltip"))),
            uiOutput("dca_dim_select"),

            h5(tr("dca.display")),
            checkboxInput("dca_color_by_group", tr("dca.color.by.group"), FALSE),
            checkboxInput("dca_show_labels", tr("dca.show.labels"), TRUE),
            conditionalPanel(
              condition = "input.dca_show_labels",
              sliderInput("dca_label_chars", tr("dca.label.chars"), 3, 25, 12, step = 1)
            ),
            sliderInput("dca_point_size", tr("dca.point.size"), 1, 8, 3, step = 0.5),

            tags$hr(),
            actionButton("dca_refresh", tr("dca.btn.calculate"), class = "btn btn-success", width = "100%")
          )
        ),
        
        column(8,
          div(class = "seriarc-panel",
            plotlyOutput("dca_plotly", height = "600px")
          ),
          br(),
          fluidRow(
            column(3, downloadButton("download_dca_plot_png", "📱 PNG", class = "btn btn-info btn-sm", style="width:100%;")),
            column(3, downloadButton("download_dca_plot_svg", "📄 SVG", class = "btn btn-secondary btn-sm", style="width:100%;")),
            column(3, downloadButton("download_dca_plot_pdf", "📄 PDF", class = "btn btn-danger btn-sm", style="width:100%;")),
            column(3, downloadButton("download_dca_plot_html", "🌐 HTML", class = "btn btn-success btn-sm", style="width:100%;"))
          ),
          br(),
          fluidRow(
            column(6, downloadButton("download_dca_data", "📊 Excel Data", class = "btn btn-warning btn-sm", style="width:100%;")),
            column(6, downloadButton("download_dca_data_csv", "📄 CSV Data", class = "btn btn-info btn-sm", style="width:100%;"))
          )
        )
      ),
      br(),
      
      # Collapsible method guide
      fluidRow(
        column(12,
          div(class = "seriarc-panel",
            tags$details(
              tags$summary(
                style = "cursor: pointer; font-weight: bold; font-size: 15px; padding: 12px; background-color: #e8f4f8; border-radius: 4px; margin-bottom: 0;",
                HTML(paste0("▶ <span style='margin-left: 8px;'>", tr("dca.method.help.title"), "</span> <span style='color: #666; font-size: 13px; font-weight: normal; margin-left: 10px;'>", tr("dca.method.help.click"), "</span>"))
              ),
              div(style = "margin-top: 20px; padding: 15px; background-color: #f8f9fa; border-radius: 4px;",

                h5(tr("dca.method.help.dca"), style = "margin-top: 0;"),
                p(tr("dca.method.help.dca.desc"), style = "text-align: justify;"),

                tags$hr(style = "border-top: 1px solid #dee2e6; margin: 20px 0;"),

                h5(tr("dca.method.title2")),
                tags$ul(
                  tags$li(strong(tr("dca.method.linear"), ":"), " ", tr("dca.method.linear.desc")),
                  tags$li(strong(tr("dca.method.nonlinear"), ":"), " ", tr("dca.method.nonlinear.desc"))
                ),

                tags$hr(style = "border-top: 1px solid #dee2e6; margin: 20px 0;"),

                h5(tr("dca.interpretation")),
                tags$dl(
                  tags$dt(strong(tr("dca.dca1"))),
                  tags$dd(tr("dca.dca1.desc")),

                  tags$dt(strong(tr("dca.dca2"))),
                  tags$dd(tr("dca.dca2.desc")),

                  tags$dt(strong(tr("dca.horizontal"))),
                  tags$dd(tr("dca.horizontal.desc")),

                  tags$dt(strong(tr("dca.remaining"))),
                  tags$dd(tr("dca.remaining.desc"))
                ),

                tags$hr(style = "border-top: 1px solid #dee2e6; margin: 20px 0;"),

                h5(tr("dca.stats.title")),
                tags$ul(
                  tags$li(strong(tr("dca.stats.r1")), " ", tr("dca.stats.r1.desc")),
                  tags$li(strong(tr("dca.stats.r2low")), " ", tr("dca.stats.r2low.desc")),
                  tags$li(strong(tr("dca.stats.r2mid")), " ", tr("dca.stats.r2mid.desc")),
                  tags$li(strong(tr("dca.stats.r2high")), " ", tr("dca.stats.r2high.desc"))
                ),

                tags$hr(style = "border-top: 1px solid #dee2e6; margin: 20px 0;"),

                h5(tr("dca.warning.title")),
                div(style = "background-color: #fff3cd; border-left: 5px solid #ffc107; padding: 15px; margin: 15px 0; border-radius: 4px;",
                  p(style = "margin: 0;", tr("dca.warning.text"))
                ),

                h5(tr("dca.recommendation"), style = "margin-top: 20px;"),
                div(style = "display: flex; gap: 20px; margin-top: 10px;",
                  div(style = "flex: 1; background-color: #d4edda; border-left: 4px solid #28a745; padding: 12px; border-radius: 4px;",
                    strong(tr("dca.recommended.dca")),
                    tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px;",
                      tags$li(tr("dca.recommended.dca.1")),
                      tags$li(tr("dca.recommended.dca.2")),
                      tags$li(tr("dca.recommended.dca.3"))
                    )
                  ),
                  div(style = "flex: 1; background-color: #cce5ff; border-left: 4px solid #004085; padding: 12px; border-radius: 4px;",
                    strong(tr("dca.recommended.ca")),
                    tags$ul(style = "margin: 8px 0 0 0; padding-left: 20px;",
                      tags$li(tr("dca.recommended.ca.1")),
                      tags$li(tr("dca.recommended.ca.2")),
                      tags$li(tr("dca.recommended.ca.3"))
                    )
                  )
                ),

                tags$hr(style = "border-top: 1px solid #dee2e6; margin: 20px 0;"),

                h5(tr("dca.literature")),
                tags$ul(style = "font-size: 13px;",
                  tags$li("Hill, M. O. & Gauch, H. G. (1980). Detrended correspondence analysis: An improved ordination technique. ",
                          tags$em("Vegetatio"), " 42, 47–58."),
                  tags$li("ter Braak, C. J. F. & Prentice, I. C. (1988). A theory of gradient analysis. ",
                          tags$em("Advances in Ecological Research"), " 18, 271–317."),
                  tags$li("Guttman, L. (1941). The quantification of a class of attributes: A theory and method of scale construction. ",
                          tags$em("The Social Sciences"), ".")
                )
              )
            )
          )
        )
      ),
      br(),

      # Statistics panel
      fluidRow(
        column(12, div(class="seriarc-panel",
          h4(tr("dca.stats"), class="mt-0"),
          verbatimTextOutput("dca_stats")))
      )
    )
  }
})

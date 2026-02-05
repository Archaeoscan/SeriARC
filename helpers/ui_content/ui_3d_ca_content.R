# ui_3d_ca_content.R - 3D-CA Tab UI Content

# 3D-CA MAIN CONTENT
output$ca_3d_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_3d_ui(tr = tr)
  } else {
    tagList(
      fluidRow(
        # ===================== LEFT SIDE: SETTINGS (narrower) =====================
        column(3,
          div(class = "seriarc-panel",
            h4(tr("3d.settings.title"), class="mt-0"),

            div(class = "seriarc-panel-success",
              h5(tr("3d.dimensions")),
              uiOutput("ca_3d_dim_select")
            ),

            div(class = "seriarc-panel-info",
              h5(tr("3d.plot.content")),
              selectInput("ca_3d_plot_elements", NULL,
                choices = setNames(
                  c("both", "sites_only", "types_only"),
                  c(tr("3d.both"), tr("3d.sites.only"), tr("3d.types.only"))
                ),
                selected = "both")
            ),

            h5(tr("3d.display")),
            radioButtons("ca_3d_color_by", tr("3d.coloring"),
              choices = setNames(c("type", "group"), c(tr("3d.color.type"), tr("3d.color.group"))), selected = "type"),

            sliderInput("point_size_3d", tr("3d.point.size"), 1, 10, 5, step = 0.5),
            checkboxInput("show_3d_planes", tr("3d.coord.planes"), FALSE),

            conditionalPanel(
              condition = "output.kmeans_available",
              div(class = "seriarc-panel-success",
                h5(tr("3d.cluster.ellipsoids")),
                checkboxInput("show_cluster_ellipsoids", tr("3d.show.ellipsoids"), FALSE)
              )
            ),

            tags$hr(),
            # 3D Statistics box - vertical scroll only, no horizontal
            div(class = "quality-excellent",
              h5(tr("3d.stats"), class="mt-0"),
              div(style = "overflow-x: hidden; white-space: pre-wrap; word-wrap: break-word;",
                verbatimTextOutput("ca_3d_stats")
              )
            )
          )
        ),

        # ===================== RIGHT SIDE: PLOT + EXPORT + EIGENVALUE TABLE =====================
        column(9,
          # Plot
          div(class = "seriarc-panel seriarc-plotly-container",
            plotlyOutput("ca_3d_plotly", height = "650px")
          ),
          br(),
          # Reset view button
          actionButton("reset_3d_view", tr("3d.reset.view"), class = "btn btn-warning btn-sm", style = "width: 100%;"),
          br(),
          br(),
          # Export buttons
          fluidRow(
            column(6, downloadButton("download_plot_png", "📱 PNG", class="btn btn-info btn-sm", style="width:100%;")),
            column(6, downloadButton("download_data_excel", "📊 Excel", class="btn btn-success btn-sm", style="width:100%;"))
          ),
          br(),
          # Eigenvalue table (moved from bottom to right side)
          div(class="seriarc-panel",
            h5(tr("3d.dimensions.overview"), class="mt-0"),
            tableOutput("ca_3d_eigen")
          )
        )
      )
    )
  }
})

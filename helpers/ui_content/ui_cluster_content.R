# ui_cluster_content.R - Cluster Tab UI Content

# === DYNAMIC METHOD SELECTION (SERVER-SIDE) ===
output$clustering_method_ui <- renderUI({
  expert_mode <- !is.null(input$expert_clustering_mode) && input$expert_clustering_mode

  if (expert_mode) {
    selectInput("clustering_method", tr("cluster.method"),
                choices = setNames(
                  c("fuzzy", "gmm"),
                  c(tr("cluster.method.fuzzy"), tr("cluster.method.gmm"))
                ),
                selected = "fuzzy")
  } else {
    selectInput("clustering_method", tr("cluster.method"),
                choices = setNames(
                  c("kmeans", "hierarchical"),
                  c(tr("cluster.method.kmeans"), tr("cluster.method.hierarchical"))
                ),
                selected = "kmeans")
  }
})

# CLUSTER MAIN CONTENT
output$cluster_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_cluster_ui(tr = tr)
  } else {
    tagList(
      # JavaScript for Bootstrap collapse chevron animations
      tags$script(HTML("
        $(document).on('shown.bs.collapse', '.cluster-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-down').addClass('glyphicon-chevron-up');
        });
        $(document).on('hidden.bs.collapse', '.cluster-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-up').addClass('glyphicon-chevron-down');
        });
      ")),

      fluidRow(
        column(4,
          div(class = "seriarc-panel",
            div(style="display:flex; justify-content:space-between; align-items:center; margin-bottom: 10px;",
              h4(tr("cluster.settings.title"), class="mt-0 mb-0"),
              actionButton("show_cluster_help", tr("cluster.help"), class="btn btn-info btn-sm")
            ),

            # ===================== PANEL 1: CLUSTERING OPTIONS (open by default) =====================
            div(class = "panel panel-primary", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#cluster_panel_options",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("cluster.panel.options")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "cluster_panel_options", class = "panel-collapse collapse in cluster-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  checkboxInput("expert_clustering_mode", tr("cluster.expert.mode"), FALSE),
                  uiOutput("clustering_method_ui"),
                  selectInput("cluster_on", tr("cluster.what"),
                    choices = setNames(
                      c("both", "rows", "cols"),
                      c(tr("cluster.both"), tr("cluster.rows"), tr("cluster.cols"))
                    )),
                  conditionalPanel(
                    condition = "input.cluster_on == 'both'",
                    div(class = "alert alert-warning", style="padding:8px; margin-top:-10px; margin-bottom:10px;",
                      tags$small(tr("cluster.both.warning"))
                    )
                  ),

                  actionButton("auto_suggest_clusters", tr("cluster.auto.suggest"),
                               class = "btn btn-info", style = "width:100%; margin-bottom:15px;"),

                  sliderInput("num_clusters", tr("cluster.num"), 2, 10, 3, step = 1),

                  # === DIMENSIONS FOR CLUSTERING (WITH INFO BUTTON) ===
                  div(style="display:flex; align-items:center; gap:5px; margin-bottom:5px;",
                    tags$label(tr("cluster.dimensions"), style="margin:0; flex-grow:1;"),
                    actionButton("info_clustering_dims", icon("info-circle"), class="btn btn-info btn-sm",
                                style="padding:2px 6px; font-size:11px;")
                  ),
                  sliderInput("kmeans_n_dims", NULL, 1, 5, 2, step = 1,
                              post = " Dim"),
                  tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 10px;",
                             tr("cluster.dim.tip")),

                  conditionalPanel(
                    condition = "input.clustering_method == 'hierarchical'",
                    selectInput("hclust_method", tr("cluster.hclust.method"),
                      choices = setNames(
                        c("ward.D2", "complete", "average", "single"),
                        c(tr("cluster.hclust.ward"), tr("cluster.hclust.complete"), tr("cluster.hclust.average"), tr("cluster.hclust.single"))
                      )),
                    checkboxInput("show_cluster_blocks", tr("cluster.show.blocks"), TRUE)
                  ),

                  conditionalPanel(
                    condition = "input.clustering_method == 'fuzzy'",
                    sliderInput("fuzzy_m", tr("cluster.fuzzy.m"), 1.1, 3, 2, step = 0.1),
                    tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 10px;",
                               tr("cluster.fuzzy.hint"))
                  ),

                  conditionalPanel(
                    condition = "input.clustering_method == 'gmm'",
                    selectInput("gmm_model_type", tr("cluster.gmm.model"),
                      choices = setNames(
                        c("VVV", "EEE", "VEV", "auto"),
                        c(tr("cluster.gmm.vvv"), tr("cluster.gmm.eee"), tr("cluster.gmm.vev"), tr("cluster.gmm.auto"))
                      )),
                    tags$small(class = "text-muted", style = "display: block; margin-top: -10px; margin-bottom: 10px;",
                               tr("cluster.gmm.hint"))
                  )
                )
              )
            ),

            # Calculate Button (always visible, between panels)
            actionButton("refresh3", tr("cluster.btn.calculate"), class = "btn btn-success btn-lg", width = "100%", style = "margin: 10px 0;"),

            # ===================== PANEL 2: DISPLAY OPTIONS (open by default) =====================
            div(class = "panel panel-info", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#cluster_panel_display",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("cluster.panel.display")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "cluster_panel_display", class = "panel-collapse collapse in cluster-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  checkboxInput("show_labels_km", tr("cluster.show.labels"), TRUE),
                  conditionalPanel(
                    condition = "input.show_labels_km",
                    sliderInput("label_chars_km", tr("cluster.label.chars"), 3, 25, 12, step = 1)
                  ),
                  sliderInput("point_size_km", tr("cluster.point.size"), 1, 8, 3, step = 0.5),

                  tags$hr(style = "margin: 10px 0;"),

                  h6(tr("cluster.features"), style = "margin-bottom: 8px;"),
                  checkboxInput("show_cluster_centers", tr("cluster.show.centers"), FALSE),
                  div(style="display:flex; align-items:center; gap:5px;",
                    checkboxInput("show_confidence_ellipses", tr("cluster.show.ellipses"), FALSE),
                    actionButton("info_confidence_ellipses", icon("info-circle"), class="btn btn-info btn-sm",
                                style="padding:2px 6px; font-size:11px; margin-left:-10px;")
                  )
                )
              )
            ),

            # ===================== PANEL 3: CLUSTER NAMES (closed by default) =====================
            conditionalPanel(
              condition = "output.kmeans_available",
              div(class = "panel panel-success", style = "margin-bottom: 10px;",
                div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                    `data-toggle` = "collapse", `data-target` = "#cluster_panel_names",
                  div(style = "display: flex; justify-content: space-between; align-items: center;",
                    tags$span(style = "font-weight: bold;", tr("cluster.panel.names")),
                    tags$span(class = "glyphicon glyphicon-chevron-down collapse-chevron")
                  )
                ),
                div(id = "cluster_panel_names", class = "panel-collapse collapse cluster-collapse",
                  div(class = "panel-body", style = "padding: 12px;",
                    uiOutput("cluster_name_inputs"),
                    actionButton("reset_cluster_names", tr("cluster.reset.names.btn"), class = "btn btn-secondary btn-sm", style="width:100%; margin-top: 10px;")
                  )
                )
              )
            )
          )
        ),

        column(8,
          div(class = "seriarc-panel",
            tabsetPanel(
              id = "cluster_viz_tabs",
              tabPanel(
                tr("cluster.tab.scatter"),
                value = "scatter",
                br(),
                plotlyOutput("kmeans_plotly", height = "600px"),
                br(),
                fluidRow(
                  column(3, downloadButton("download_kmeans_plot_png", "PNG", class="btn btn-info btn-sm", style="width:100%;")),
                  column(3, downloadButton("download_kmeans_plot_svg", "SVG", class="btn btn-secondary btn-sm", style="width:100%;")),
                  column(3, downloadButton("download_kmeans_plot_pdf", "PDF", class="btn btn-danger btn-sm", style="width:100%;")),
                  column(3, downloadButton("download_kmeans_data", "Data", class="btn btn-success", style="width:100%;"))
                ),
                # Cluster Quality Box (moved from left panel)
                conditionalPanel(
                  condition = "output.kmeans_available",
                  tags$hr(),
                  div(class = "quality-good",
                    h5(tr("cluster.quality"), class="mt-0"),
                    verbatimTextOutput("cluster_quality")
                  )
                )
              ),
              tabPanel(
                tr("cluster.tab.dendrogram"),
                value = "dendrogram",
                br(),
                plotlyOutput("dendrogram_plotly", height = "700px"),
                br(),
                fluidRow(
                  column(4, downloadButton("download_dendrogram_png", "PNG", class="btn btn-info btn-sm", style="width:100%;")),
                  column(4, downloadButton("download_dendrogram_svg", "SVG", class="btn btn-secondary btn-sm", style="width:100%;")),
                  column(4, downloadButton("download_dendrogram_pdf", "PDF", class="btn btn-danger btn-sm", style="width:100%;"))
                )
              ),
              tabPanel(
                tr("cluster.tab.table"),
                value = "table",
                br(),
                conditionalPanel(
                  condition = "input.cluster_on == 'both'",
                  radioButtons("cluster_table_filter", tr("cluster.table.filter"),
                             choices = setNames(
                               c("both", "Site", "Type"),
                               c(tr("cluster.table.all"), tr("cluster.table.sites"), tr("cluster.table.types"))
                             ),
                             selected = "both", inline = TRUE)
                ),
                DT::dataTableOutput("kmeans_table"),
                br()
              ),
              tabPanel(
                tr("cluster.tab.characterization"),
                value = "characterization",
                br(),
                conditionalPanel(
                  condition = "output.characterization_available",
                  uiOutput("cluster_characterization_ui")
                ),
                conditionalPanel(
                  condition = "!output.characterization_available",
                  div(class = "alert alert-warning",
                    tags$p(tr("cluster.char.unavailable"))
                  )
                )
              )
            )
          )
        )
      )
    )
  }
})

# === HELPER FOR WELCOME UI ===
welcome_cluster_ui <- function(tr = function(x) x) {
  div(class = "seriarc-panel text-center", style = "padding: 80px 40px;",
    tags$h2(tr("cluster.welcome.title"), class = "text-primary"),
    tags$p(class = "lead", tr("cluster.welcome.desc")),
    tags$hr(),
    div(class = "alert alert-info",
      tags$h4(tr("cluster.welcome.nodata"), class = "mt-0"),
      tags$p(tr("cluster.welcome.hint"))
    ),
    tags$hr(),
    tags$h4(tr("cluster.what.title")),
    tags$ul(class = "text-left", style = "max-width: 600px; margin: 0 auto;",
      tags$li(tags$strong("K-Means:"), " ", tr("cluster.what.kmeans")),
      tags$li(tags$strong("Hierarchical:"), " ", tr("cluster.what.hierarchical")),
      tags$li(tags$strong("Fuzzy K-Means:"), " ", tr("cluster.what.fuzzy")),
      tags$li(tags$strong("GMM:"), " ", tr("cluster.what.gmm"))
    ),
    tags$p(class = "text-muted", "SeriARC v1.0.0 - Clustering Module")
  )
}

# ui_ca_content.R - Correspondence Analysis Tab UI Content

# CORRESPONDENCE ANALYSIS TAB UI MODULE
output$ca_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_analysis_ui(tr = tr)
  } else {
    tagList(
      # JavaScript for Bootstrap collapse chevron animations
      tags$script(HTML("
        $(document).on('shown.bs.collapse', '.ca-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-down').addClass('glyphicon-chevron-up');
        });
        $(document).on('hidden.bs.collapse', '.ca-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-up').addClass('glyphicon-chevron-down');
        });
      ")),

      fluidRow(
        column(4,
          div(class = "seriarc-panel",
            h4(tr("ca.settings.title"), class = "mt-0"),

            # ===================== PANEL 1: TRANSFORMATIONS (open by default) =====================
            div(class = "panel panel-primary", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#ca_panel_transform",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("ca.panel.transform")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "ca_panel_transform", class = "panel-collapse collapse in ca-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  # Analysis Focus
                  h6(tr("ca.analysis.focus"), tooltip_info(tr("ca.analysis.focus.tooltip")), style = "margin-top: 0;"),
                  selectInput("ca_analysis_focus", NULL,
                    choices = setNames(
                      c("both", "sites_only", "types_only"),
                      c(tr("ca.focus.both"), tr("ca.focus.sites.only"), tr("ca.focus.types.only"))
                    ),
                    selected = "both"
                  ),

                  tags$hr(style = "margin: 10px 0;"),

                  # Transformations
                  h6(tr("ca.transform.title"),
                     actionLink("show_transform_help", tr("ca.transform.help"), style = "margin-left: 8px; font-size: 0.85em;"),
                     style = "margin-bottom: 8px;"
                  ),

                  tags$small(tr("ca.transform.types"), style = "color: #666;"),
                  radioButtons("ca_transform_types", NULL,
                    choices = setNames(
                      c("none", "normalize", "log"),
                      c(tr("ca.transform.none"), tr("ca.transform.normalize"), tr("ca.transform.log"))
                    ),
                    selected = "none", inline = TRUE
                  ),

                  tags$small(tr("ca.transform.sites"), style = "color: #666;"),
                  radioButtons("ca_transform_sites", NULL,
                    choices = setNames(
                      c("none", "normalize", "log"),
                      c(tr("ca.transform.none"), tr("ca.transform.normalize"), tr("ca.transform.log"))
                    ),
                    selected = "none", inline = TRUE
                  ),

                  tags$hr(style = "margin: 10px 0;"),

                  # Biplot Type
                  h6(tr("ca.biplot.title"), tooltip_info(tr("ca.biplot.tooltip")), style = "margin-bottom: 8px;"),
                  radioButtons("ca_biplot_type", NULL,
                    choices = setNames(
                      c("symmetric", "rowprincipal", "colprincipal"),
                      c(tr("ca.biplot.symmetric"), tr("ca.biplot.rowprincipal"), tr("ca.biplot.colprincipal"))
                    ),
                    selected = "symmetric",
                    width = "100%"
                  ),

                  # Biplot type explanations (conditional)
                  conditionalPanel(
                    condition = "input.ca_biplot_type == 'symmetric'",
                    div(class = "alert alert-info", style = "font-size: 0.8em; padding: 6px 8px; margin: 5px 0 0 0;",
                      tags$b(tr("ca.biplot.symmetric.title")), br(),
                      tr("ca.biplot.symmetric.desc"), br(),
                      tags$b(tr("ca.biplot.symmetric.interp")), br(),
                      tr("ca.biplot.symmetric.dist1"), br(),
                      tr("ca.biplot.symmetric.dist2"), br(),
                      tr("ca.biplot.symmetric.dist3")
                    )
                  ),
                  conditionalPanel(
                    condition = "input.ca_biplot_type == 'rowprincipal'",
                    div(class = "alert alert-warning", style = "font-size: 0.8em; padding: 6px 8px; margin: 5px 0 0 0;",
                      tags$b(tr("ca.biplot.rowprincipal.title")), br(),
                      tr("ca.biplot.rowprincipal.desc"), br(),
                      tags$b(tr("ca.biplot.symmetric.interp")), br(),
                      tr("ca.biplot.rowprincipal.interp1"), br(),
                      tr("ca.biplot.rowprincipal.interp2"), br(),
                      tr("ca.biplot.rowprincipal.interp3"), br(),
                      tags$small(style = "color: #856404;", tr("ca.biplot.rowprincipal.source"))
                    )
                  ),
                  conditionalPanel(
                    condition = "input.ca_biplot_type == 'colprincipal'",
                    div(class = "alert alert-success", style = "font-size: 0.8em; padding: 6px 8px; margin: 5px 0 0 0;",
                      tags$b(tr("ca.biplot.colprincipal.title")), br(),
                      tr("ca.biplot.colprincipal.desc"), br(),
                      tags$b(tr("ca.biplot.symmetric.interp")), br(),
                      tr("ca.biplot.colprincipal.interp1"), br(),
                      tr("ca.biplot.colprincipal.interp2"), br(),
                      tr("ca.biplot.colprincipal.interp3"), br(),
                      tags$small(style = "color: #155724;", tr("ca.biplot.colprincipal.source"))
                    )
                  )
                )
              )
            ),

            # Calculate Button (always visible, between panels)
            actionButton("refresh", tr("ca.btn.calculate"), class = "btn btn-primary btn-lg", width = "100%", style = "margin: 10px 0;"),

            # ===================== PANEL 2: DISPLAY OPTIONS (open by default) =====================
            div(class = "panel panel-info", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#ca_panel_display",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("ca.panel.display")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "ca_panel_display", class = "panel-collapse collapse in ca-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  # Dimension Selection
                  h6(tr("ca.dimensions"), tooltip_info(tr("ca.dimensions.tooltip")), style = "margin-top: 0;"),
                  uiOutput("ca_dim_select"),

                  tags$hr(style = "margin: 10px 0;"),

                  # Display Options
                  h6(tr("ca.display"), style = "margin-bottom: 8px;"),
                  checkboxInput("ca_color_by_group", tr("ca.color.by.group"), FALSE),
                  checkboxInput("show_group_centroids", tr("ca.show.centroids"), FALSE),
                  div(style = "display: flex; align-items: center; gap: 5px;",
                    checkboxInput("show_confidence_ellipses", tr("ca.show.ellipses"), FALSE),
                    tooltip_info(tr("ca.ellipses.tooltip"))
                  ),

                  tags$hr(style = "margin: 10px 0;"),

                  # Labels
                  checkboxInput("show_labels", tr("ca.show.labels"), TRUE),
                  conditionalPanel(
                    condition = "input.show_labels",
                    div(style = "padding-left: 10px; border-left: 2px solid #5bc0de;",
                      selectInput("label_selection", tr("ca.label.selection"),
                        choices = setNames(c("all", "selected"), c(tr("ca.label.all"), tr("ca.label.selected"))),
                        selected = "all"
                      ),
                      selectInput("label_type", tr("ca.label.for"),
                        choices = setNames(
                          c("both", "sites", "types"),
                          c(tr("ca.label.both"), tr("ca.label.sites"), tr("ca.label.types"))
                        ),
                        selected = "both"
                      ),
                      sliderInput("label_chars", tr("ca.label.chars"), 3, 60, 12, step = 1),
                      sliderInput("label_size", tr("ca.label.size"), 8, 20, 12, step = 1),
                      checkboxInput("label_colored", tr("ca.label.colored"), FALSE)
                    )
                  ),

                  sliderInput("point_size", tr("ca.point.size"), 1, 8, 3, step = 0.5)
                )
              )
            ),

            # ===================== PANEL 3: OVERLAYS (closed by default) =====================
            div(class = "panel panel-success", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#ca_panel_overlay",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("ca.panel.overlay")),
                  tags$span(class = "glyphicon glyphicon-chevron-down collapse-chevron")
                )
              ),
              div(id = "ca_panel_overlay", class = "panel-collapse collapse ca-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  # 14C Overlay (from dedicated module)
                  mod_ca_c14_overlay_ui("c14_overlay", tr = tr)

                  # Future: Clustering overlay options could go here too
                )
              )
            )

          )
        ),
        
        column(8,
          div(class = "seriarc-panel",
            # === TABS FOR VISUALIZATIONS ===
            tabsetPanel(
              id = "ca_viz_tabs",
              
              # Tab: Biplot
              tabPanel(tr("ca.tab.biplot"),
                br(),
                plotlyOutput("ca_plotly", height = "600px"),
                br(),
                fluidRow(
                  column(3, downloadButton("download_ca_plot_png", "📱 PNG", class = "btn btn-info btn-sm", style="width:100%;")),
                  column(3, downloadButton("download_ca_plot_svg", "📄 SVG", class = "btn btn-secondary btn-sm", style="width:100%;")),
                  column(3, downloadButton("download_ca_plot_pdf", "📄 PDF", class = "btn btn-danger btn-sm", style="width:100%;")),
                  column(3, downloadButton("download_ca_plot_html", "🌐 HTML", class = "btn btn-success btn-sm", style="width:100%;"))
                ),
                br(),
                fluidRow(
                  column(6, downloadButton("download_ca_data", tr("ca.download.excel"), class = "btn btn-warning btn-sm", style="width:100%;")),
                  column(6, downloadButton("download_ca_data_csv", tr("ca.download.csv"), class = "btn btn-info btn-sm", style="width:100%;"))
                ),
                br(),
                # Quick Statistics (moved from left panel)
                div(class = "quality-excellent", style = "margin-top: 10px;",
                  h5(tr("ca.quick.stats"), class = "mt-0"),
                  tableOutput("ca_quick_stats")
                )
              ),
              
              # Tab: Contributions
              tabPanel(tr("ca.tab.contrib"),
                br(),
                h5(tr("ca.contrib.title")),
                p(tr("ca.contrib.desc"),
                  style="color: #6c757d; font-size: 0.9em;"),
                hr(),
                # Auswahlleiste
                fluidRow(
                  column(6,
                    uiOutput("ca_contrib_dim_selector")
                  ),
                  column(6,
                    radioButtons("contrib_element", tr("ca.contrib.element"),
                                choices = setNames(c("row", "col"), c(tr("ca.contrib.sites"), tr("ca.contrib.types"))),
                                selected = "row", inline = TRUE)
                  )
                ),
                # One large plot
                plotlyOutput("ca_contrib_selected", height = "700px"),
                br(),
                # Export button
                downloadButton("download_contrib_excel", tr("ca.contrib.export"), class = "btn btn-success", style = "width: 200px;")
              ),
              
              # Tab: Quality (Cos²)
              tabPanel(tr("ca.tab.quality"),
                br(),
                h5(tr("ca.cos2.title")),
                div(class="alert alert-info", style="font-size: 0.9em; margin-bottom: 15px;",
                  tags$b(tr("ca.cos2.what")), br(),
                  tr("ca.cos2.desc"), br(),
                  tags$ul(style="margin-bottom: 5px;",
                    tags$li(tags$b(tr("ca.cos2.high")), " ", tr("ca.cos2.high.desc")),
                    tags$li(tags$b(tr("ca.cos2.low")), " ", tr("ca.cos2.low.desc"))
                  ),
                  tags$b(tr("ca.cos2.meaning")), br(),
                  tr("ca.cos2.meaning.desc")
                ),
                hr(),
                # Auswahlleiste
                fluidRow(
                  column(12,
                    radioButtons("cos2_element", tr("ca.contrib.element"),
                                choices = setNames(c("row", "col"), c(tr("ca.contrib.sites"), tr("ca.contrib.types"))),
                                selected = "row", inline = TRUE)
                  )
                ),
                # One large plot
                plotlyOutput("ca_cos2_selected", height = "700px"),
                # INFO-BOX with quartile explanation
                uiOutput("ca_cos2_info"),
                br(),
                # Export-Button
                downloadButton("download_cos2_excel", tr("ca.contrib.export"), class = "btn btn-success", style = "width: 200px;")
              ),
              
              # Tab: Outlier-Detection
              tabPanel(tr("ca.tab.outlier"),
                br(),
                
                # Collapsible explanation with clear indicator
                div(class="panel panel-warning", style="margin-bottom: 20px;",
                  div(class="panel-heading", style="cursor: pointer; background-color: #fcf8e3; border-color: #faebcc; padding: 10px 15px;",
                      onclick="$('#outlier_explanation').collapse('toggle');",
                      div(style="display: flex; justify-content: space-between; align-items: center;",
                        div(
                          tags$b(tr("ca.outlier.exp.title")),
                          tags$span(style="margin-left: 10px; color: #8a6d3b; font-size: 0.9em;",
                            tr("ca.outlier.exp.toggle"))
                        ),
                        tags$span(id="toggle_icon", class="glyphicon glyphicon-chevron-down",
                                  style="color: #8a6d3b;")
                      )
                  ),
                  div(id="outlier_explanation", class="panel-collapse collapse",
                    div(class="panel-body", style="font-size: 0.95em; background-color: #fcf8e3;",
                      tr("ca.outlier.exp.intro"), br(), br(),

                      tags$b(tr("ca.outlier.exp.quantile.title")), br(),
                      tags$ul(style="margin-bottom: 10px;",
                        tags$li(tags$b(tr("ca.outlier.exp.quantile.cos2")), " ", tr("ca.outlier.exp.quantile.cos2.desc")),
                        tags$li(tags$b(tr("ca.outlier.exp.quantile.contrib")), " ", tr("ca.outlier.exp.quantile.contrib.desc"))
                      ),

                      tags$b(tr("ca.outlier.exp.jackknife.title")), br(),
                      tags$ul(style="margin-bottom: 10px;",
                        tags$li(tags$b(tr("ca.outlier.exp.jackknife.loo")), " ", tr("ca.outlier.exp.jackknife.loo.desc")),
                        tags$li(tags$b(tr("ca.outlier.exp.jackknife.score")), " ", tr("ca.outlier.exp.jackknife.score.desc")),
                        tags$li(tags$b(tr("ca.outlier.exp.jackknife.high")), " ", tr("ca.outlier.exp.jackknife.high.desc"))
                      ),

                      tags$b(tr("ca.outlier.exp.combined.title")), br(),
                      tags$ul(style="margin-bottom: 10px;",
                        tags$li("🔴 ", tags$b(tr("ca.outlier.exp.combined.critical")), " ", tr("ca.outlier.exp.combined.critical.desc")),
                        tags$li("🟡 ", tags$b(tr("ca.outlier.exp.combined.marginal")), " ", tr("ca.outlier.exp.combined.marginal.desc")),
                        tags$li("🟠 ", tags$b(tr("ca.outlier.exp.combined.structural")), " ", tr("ca.outlier.exp.combined.structural.desc")),
                        tags$li("✅ ", tags$b(tr("ca.outlier.exp.combined.ok")), " ", tr("ca.outlier.exp.combined.ok.desc"))
                      ),

                      tags$b(tr("ca.outlier.exp.robust.title")), br(),
                      tr("ca.outlier.exp.robust.desc"), br(),
                      tags$ul(style="margin-bottom: 10px;",
                        tags$li(tr("ca.outlier.exp.robust.dims")),
                        tags$li(tags$b(tr("ca.outlier.exp.robust.shows")), " ", tr("ca.outlier.exp.robust.threshold"), " ", tags$b(tr("ca.outlier.exp.robust.threshold2"))),
                        tags$li("🔴 ", tr("ca.outlier.exp.robust.critical")),
                        tags$li("🟡 ", tr("ca.outlier.exp.robust.marginal")),
                        tags$li("🟠 ", tr("ca.outlier.exp.robust.structural")),
                        tags$li(tags$b(tr("ca.outlier.exp.robust.advantage")), " ", tr("ca.outlier.exp.robust.advantage.desc"))
                      ),

                      hr(style="margin: 10px 0;"),

                      tags$b(tr("ca.outlier.exp.interp.title")), br(),
                      tags$ul(style="margin-bottom: 5px;",
                        tags$li(tags$b(tr("ca.outlier.exp.interp.cos2")), " ", tr("ca.outlier.exp.interp.cos2.desc")),
                        tags$li(tags$b(tr("ca.outlier.exp.interp.contrib")), " ", tr("ca.outlier.exp.interp.contrib.desc")),
                        tags$li(tags$b(tr("ca.outlier.exp.interp.influence")), " ", tr("ca.outlier.exp.interp.influence.desc"))
                      ),

                      tags$b(tr("ca.outlier.exp.warning")), " ", tr("ca.outlier.exp.warning.desc"), br(), br(),
                      tags$b(tr("ca.outlier.exp.sources")), br(),
                      tags$small(
                        tr("ca.outlier.exp.source1"), br(),
                        tr("ca.outlier.exp.source2"), br(),
                        tr("ca.outlier.exp.source3")
                      )
                    )
                  )
                ),
                
                # JavaScript for Chevron animation
                tags$script("
                  $('#outlier_explanation').on('shown.bs.collapse', function() {
                    $('#toggle_icon').removeClass('glyphicon-chevron-down').addClass('glyphicon-chevron-up');
                  });
                  $('#outlier_explanation').on('hidden.bs.collapse', function() {
                    $('#toggle_icon').removeClass('glyphicon-chevron-up').addClass('glyphicon-chevron-down');
                  });
                "),
                
                fluidRow(
                  column(6,
                    div(class="seriarc-panel",
                      h5(tr("ca.outlier.settings"), class="mt-0"),
                      
                      # Method selection (only when NOT Robust selected)
                      conditionalPanel(
                        condition = "input.outlier_dimensions != 'robust'",
                        radioButtons("outlier_detection_method",
                          tr("ca.outlier.method"),
                          choices = setNames(
                            c("quantile", "jackknife", "combined"),
                            c(tr("ca.outlier.method.quantile"), tr("ca.outlier.method.jackknife"), tr("ca.outlier.method.combined"))
                          ),
                          selected = "quantile"
                        )
                      ),
                      
                      # Info when Robust selected
                      conditionalPanel(
                        condition = "input.outlier_dimensions == 'robust'",
                        div(class="alert alert-success", style="font-size: 0.85em; padding: 8px; margin-bottom: 10px;",
                          tags$b(tr("ca.outlier.robust.active")), br(),
                          tr("ca.outlier.robust.desc")
                        )
                      ),
                      
                      # NEU: Dimensionsauswahl inkl. Robust-Option
                      radioButtons("outlier_dimensions",
                        div(tr("ca.outlier.dim.label"),
                            tooltip_info(tr("ca.tooltip.outlier.dims"))),
                        choices = setNames(
                          c("12", "13", "23", "123", "robust"),
                          c(tr("ca.outlier.dim.12"), tr("ca.outlier.dim.13"), tr("ca.outlier.dim.23"), tr("ca.outlier.dim.123"), tr("ca.outlier.dim.robust"))
                        ),
                        selected = "12"
                      ),
                      
                      # Info for Robust option
                      conditionalPanel(
                        condition = "input.outlier_dimensions == 'robust'",
                        div(class="alert alert-success", style="font-size: 0.85em; padding: 8px; margin-top: 5px;",
                          tags$b(tr("ca.outlier.robust.title")), br(),
                          tr("ca.outlier.robust.checks"), br(),
                          tags$ul(style="margin-bottom: 5px; padding-left: 20px;",
                            tags$li("Dim1+Dim2 (2D)"),
                            tags$li("Dim1+Dim3 (2D)"),
                            tags$li("Dim2+Dim3 (2D)"),
                            tags$li("Dim1+Dim2+Dim3 (3D)")
                          ),
                          tags$b(tr("ca.outlier.robust.result.label")), " ", tr("ca.outlier.robust.result")
                        )
                      ),
                      
                      # Button for (Re)calculation
                      actionButton("outlier_recalculate",
                        tr("ca.outlier.btn.recalc"),
                        class = "btn btn-primary",
                        style = "width: 100%; margin-bottom: 10px;"
                      ),

                      div(class="alert alert-info", style="font-size: 0.85em; padding: 8px; margin-bottom: 10px;",
                        tags$b(tr("ca.outlier.tip.label")), " ", tr("ca.outlier.tip")
                      ),

                      hr(),

                      h6(tr("ca.outlier.thresholds")),

                      sliderInput("outlier_cos2_quantile",
                        div(tr("ca.outlier.cos2.label"),
                            tooltip_info(tr("ca.tooltip.cos2.quantile"))),
                        min = 5, max = 20, value = 10, step = 1,
                        post = "%"
                      ),

                      sliderInput("outlier_contrib_quantile",
                        div(tr("ca.outlier.contrib.label"),
                            tooltip_info(tr("ca.tooltip.contrib.quantile"))),
                        min = 0, max = 15, value = 5, step = 1,
                        post = "%"
                      ),

                      # Jackknife threshold (only for Jackknife/Combined)
                      conditionalPanel(
                        condition = "input.outlier_detection_method != 'quantile'",
                        sliderInput("outlier_jackknife_threshold",
                          div(tr("ca.outlier.jackknife.label"),
                              tooltip_info(tr("ca.tooltip.jackknife"))),
                          min = 0.05, max = 0.3, value = 0.1, step = 0.01
                        )
                      ),
                      
                      hr(),

                      div(style="max-height: 400px; overflow-y: auto; overflow-x: hidden; background: #f8f9fa; padding: 12px; border-radius: 4px;",
                        tags$style(HTML("#ca_outlier_summary { white-space: pre-wrap !important; word-wrap: break-word !important; word-break: break-word !important; font-size: 0.85em; line-height: 1.4; }")),
                        verbatimTextOutput("ca_outlier_summary")
                      )
                    )
                  ),
                  
                  column(6,
                    div(class="seriarc-panel",
                      h5(tr("ca.outlier.found.title"), class="mt-0"),

                      # Status display for excluded outliers
                      uiOutput("ca_excluded_outliers_status"),

                      # Filter: Sites vs Types
                      radioButtons("outlier_filter",
                        tr("ca.outlier.filter.label"),
                        choices = setNames(
                          c("sites", "types", "all"),
                          c(tr("ca.outlier.filter.sites"), tr("ca.outlier.filter.types"), tr("ca.outlier.filter.all"))
                        ),
                        selected = "all",
                        inline = TRUE
                      ),

                      # Ampel-Legende
                      div(class="alert alert-info", style="font-size: 0.85em; padding: 8px; margin-bottom: 10px;",
                        tags$b(tr("ca.outlier.legend")), br(),
                        tr("ca.outlier.legend.critical"), " | ",
                        tr("ca.outlier.legend.structural"), " | ",
                        tr("ca.outlier.legend.marginal")
                      ),

                      # Checkbox list with scroll
                      div(style="max-height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px; padding: 10px; background: #f8f9fa;",
                        uiOutput("ca_outlier_checkboxes")
                      ),

                      br(),

                      actionButton("ca_remove_outliers",
                        tr("ca.outlier.btn.exclude"),
                        class = "btn btn-warning",
                        style = "width: 100%;"),

                      br(),

                      actionButton("ca_reset_outliers",
                        tr("ca.outlier.btn.include"),
                        class = "btn btn-secondary",
                        style = "width: 100%;")
                    )
                  )
                ),
                
                br(),
                
                div(class="alert alert-info", style="font-size: 0.85em;",
                  tags$b(tr("ca.outlier.interp.title")), br(), br(),

                  tags$b(tr("ca.outlier.interp.cos2.title")), br(),
                  tr("ca.outlier.interp.cos2.desc"), br(),
                  tags$b(tr("ca.outlier.interp.causes")), br(),
                  tags$ul(style="margin-bottom: 8px;",
                    tags$li(tr("ca.outlier.interp.cos2.cause1")),
                    tags$li(tr("ca.outlier.interp.cos2.cause2")),
                    tags$li(tr("ca.outlier.interp.cos2.cause3"))
                  ),

                  tags$b(tr("ca.outlier.interp.contrib.title")), br(),
                  tr("ca.outlier.interp.contrib.desc"), br(),
                  tags$b(tr("ca.outlier.interp.causes")), br(),
                  tags$ul(style="margin-bottom: 8px;",
                    tags$li(tr("ca.outlier.interp.contrib.cause1")),
                    tags$li(tr("ca.outlier.interp.contrib.cause2")),
                    tags$li(tr("ca.outlier.interp.contrib.cause3"))
                  ),

                  tags$b(tr("ca.outlier.interp.influence.title")), br(),
                  tr("ca.outlier.interp.influence.desc"), br(),
                  tags$b(tr("ca.outlier.interp.causes")), br(),
                  tags$ul(style="margin-bottom: 8px;",
                    tags$li(tr("ca.outlier.interp.influence.cause1")),
                    tags$li(tr("ca.outlier.interp.influence.cause2")),
                    tags$li(tr("ca.outlier.interp.influence.cause3"))
                  ),

                  tags$b(tr("ca.outlier.interp.combined.title")), br(),
                  tr("ca.outlier.interp.combined.desc"), br(), br(),

                  hr(style="margin: 10px 0;"),

                  tags$b(tr("ca.outlier.interp.procedure.title")), br(),
                  tr("ca.outlier.interp.procedure.step1"), " ", tags$b(tr("ca.outlier.interp.procedure.quantile")), " ", tr("ca.outlier.interp.procedure.on"), br(),
                  tr("ca.outlier.interp.procedure.step2"), br(),
                  tr("ca.outlier.interp.procedure.step3"), br(),
                  tr("ca.outlier.interp.procedure.step4"), " ", tags$b(tr("ca.outlier.interp.procedure.jackknife")), " ", tr("ca.outlier.interp.procedure.check"), br(),
                  tr("ca.outlier.interp.procedure.step5"), " ", tags$b(tr("ca.outlier.interp.procedure.critical")), " ", tr("ca.outlier.interp.procedure.remove"), br(),
                  tr("ca.outlier.interp.procedure.step6"), br(), br(),

                  tags$b(tr("ca.outlier.interp.literature")), br(),
                  tags$small(
                    "Greenacre, M. (2007). ", tags$em("Correspondence Analysis in Practice"), ", 3rd ed. Chapman & Hall/CRC.", br(),
                    "Baxter, M. (1994). ", tags$em("Exploratory Multivariate Analysis in Archaeology"), ". Edinburgh University Press.", br(),
                    "Ihm, P. (1978). Statistik in der Archäologie. ", tags$em("Archaeo-Physika"), " 7."
                  )
                )
              ),
              
              # Tab: Eigenwerte
              tabPanel(tr("ca.tab.eigenvalues"),
                br(),
                div(style="display: flex; justify-content: space-between; align-items: center;",
                  h5(tr("ca.eigen.variance.title")),
                  actionButton("help_eigenwerte", tr("ca.eigen.help"), class="btn-info btn-sm")
                ),
                br(),
                fluidRow(
                  column(6,
                    div(class = "seriarc-panel",
                      h4(tr("ca.eigen.title"), class="mt-0"),
                      div(style="max-height: 400px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px;",
                        tableOutput("ca_eigen")
                      )
                    )
                  ),
                  column(6,
                    div(class="seriarc-panel",
                      h4(tr("ca.eigen.stats.title"), class="mt-0"),
                      div(style="max-height: 400px; overflow-y: auto; overflow-x: hidden; background: #f8f9fa; padding: 12px; border-radius: 4px;",
                        tags$style(HTML("#ca_stats { white-space: pre-wrap !important; word-wrap: break-word !important; word-break: break-word !important; font-size: 0.85em; line-height: 1.5; margin: 0; }")),
                        verbatimTextOutput("ca_stats")
                      )
                    )
                  )
                ),
                br(),
                # Scree-Plot
                div(class = "seriarc-panel",
                  div(style="display: flex; align-items: center; gap: 8px;",
                    h4(tr("ca.eigen.scree.title"), class="mt-0 mb-0"),
                    tooltip_info(tr("ca.eigen.scree.tooltip"))
                  ),
                  p(tr("ca.eigen.scree.desc"),
                    style="color: #6c757d; font-size: 0.9em; margin-top: 5px;"),
                  plotlyOutput("ca_screeplot", height = "450px")
                ),
                br(),
                # Malinvaud-Test
                div(class = "seriarc-panel",
                  div(style="display: flex; align-items: center; gap: 8px;",
                    h4(tr("ca.eigen.malinvaud.title"), class="mt-0 mb-0"),
                    tooltip_info(tr("ca.eigen.malinvaud.tooltip"))
                  ),
                  p(tr("ca.eigen.malinvaud.desc"),
                    style="color: #6c757d; font-size: 0.9em; margin-top: 5px;"),
                  tableOutput("ca_malinvaud"),
                  uiOutput("ca_malinvaud_warning")
                ),
                br(),
                # Between-Group Inertia
                div(class = "seriarc-panel",
                  h4(tr("ca.eigen.between.title"), class="mt-0"),
                  p(tr("ca.eigen.between.desc"),
                    style="color: #6c757d; font-size: 0.9em;"),
                  verbatimTextOutput("ca_between_group")
                )
              )
            )
          )
        )
      )
    )
  }
})

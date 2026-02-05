# ui_bootstrap_content.R - Bootstrap Tab UI Content

output$bootstrap_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_bootstrap_ui(tr = tr)
  } else {
    tagList(
      # JavaScript for Bootstrap collapse chevron animations
      tags$script(HTML("
        $(document).on('shown.bs.collapse', '.bootstrap-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-down').addClass('glyphicon-chevron-up');
        });
        $(document).on('hidden.bs.collapse', '.bootstrap-collapse', function() {
          $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-up').addClass('glyphicon-chevron-down');
        });
      ")),

      fluidRow(
        column(4,
               div(class = "seriarc-panel",
                   div(style = "display: flex; justify-content: space-between; align-items: center; margin-bottom: 10px;",
                       h4(tr("bootstrap.settings"), class = "mt-0"),
                       actionButton(
                         "bootstrap_help", "",
                         icon = icon("question-circle"),
                         class = "btn-link",
                         style = "font-size: 1.5em; color: #3498db; padding: 0; border: none;"
                       )
                   ),

                   # ===================== PANEL 1: PARAMETERS (open by default) =====================
                   div(class = "panel panel-primary", style = "margin-bottom: 10px;",
                     div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                         `data-toggle` = "collapse", `data-target` = "#bootstrap_panel_params",
                       div(style = "display: flex; justify-content: space-between; align-items: center;",
                         tags$span(style = "font-weight: bold;", tr("bootstrap.panel.parameters")),
                         tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                       )
                     ),
                     div(id = "bootstrap_panel_params", class = "panel-collapse collapse in bootstrap-collapse",
                       div(class = "panel-body", style = "padding: 12px;",

                         sliderInput("bootstrap_iterations", tr("bootstrap.iterations"), min = 50, max = 5000, value = 999, step = 50),
                         helpText(tr("bootstrap.iterations.help")),

                         div(style = "padding: 10px; background: #e3f2fd; border-radius: 4px; margin: 10px 0;",
                             p(style = "margin: 0; font-size: 0.9em;",
                               strong(tr("bootstrap.resampling.model")),
                               br(),
                               tr("bootstrap.resampling.desc")
                             )
                         ),

                         sliderInput("bootstrap_confidence", tr("bootstrap.confidence"), min = 90, max = 99, value = 95, step = 1),
                         helpText(tr("bootstrap.confidence.help")),

                         tags$hr(style = "margin: 10px 0;"),

                         h6(tr("bootstrap.dimensions"), style = "margin-bottom: 8px;"),
                         uiOutput("bootstrap_dim_select")
                       )
                     )
                   ),

                   # Run Button (always visible, between panels)
                   actionButton("run_bootstrap", tr("bootstrap.btn.start"), class = "btn btn-success btn-lg", style = "width: 100%; margin: 10px 0;"),

                   # ===================== PANEL 2: DISPLAY OPTIONS (open by default) =====================
                   div(class = "panel panel-info", style = "margin-bottom: 10px;",
                     div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                         `data-toggle` = "collapse", `data-target` = "#bootstrap_panel_display",
                       div(style = "display: flex; justify-content: space-between; align-items: center;",
                         tags$span(style = "font-weight: bold;", tr("bootstrap.panel.display")),
                         tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                       )
                     ),
                     div(id = "bootstrap_panel_display", class = "panel-collapse collapse in bootstrap-collapse",
                       div(class = "panel-body", style = "padding: 12px;",

                         selectInput(
                           "bootstrap_elements", tr("bootstrap.elements"),
                           choices = setNames(
                             c("both", "sites", "types"),
                             c(tr("bootstrap.elements.both"), tr("bootstrap.elements.sites"), tr("bootstrap.elements.types"))
                           ),
                           selected = "both"
                         ),

                         checkboxInput("highlight_stability", tr("bootstrap.highlight"), TRUE),
                         helpText(tr("bootstrap.highlight.help")),

                         checkboxInput("show_confidence_ellipses_bootstrap", tr("bootstrap.ellipses"), FALSE),
                         helpText(tr("bootstrap.ellipses.help")),

                         checkboxInput("show_bootstrap_labels", tr("bootstrap.labels"), TRUE),
                         conditionalPanel(
                           condition = "input.show_bootstrap_labels",
                           sliderInput("bootstrap_label_chars", tr("bootstrap.label.chars"), 3, 25, 12, step = 1)
                         )
                       )
                     )
                   )
               )
        ),

        column(8,
               div(class = "seriarc-panel",
                   plotlyOutput("bootstrap_plotly", height = "600px")
               ),
               br(),
               div(class = "seriarc-help-text",
                   tr("bootstrap.plot.help")
               ),
               br(),
               fluidRow(
                 column(4, downloadButton("download_plot_png", "📱 PNG", class = "btn btn-info btn-sm", style = "width:100%;")),
                 column(4, downloadButton("download_plot_svg", "📄 SVG", class = "btn btn-secondary btn-sm", style = "width:100%;")),
                 column(4, downloadButton("download_data_excel", "📊 Excel", class = "btn btn-success btn-sm", style = "width:100%;"))
               )
        )
      ),

      br(),

      fluidRow(
        column(6, div(class = "seriarc-panel",
                      h4(tr("bootstrap.stability"), class = "mt-0"),
                      helpText(tr("bootstrap.stability.help")),
                      DT::dataTableOutput("bootstrap_stats_table")
        )),
        column(6, div(class = "seriarc-panel",
                      h4(tr("bootstrap.critical"), class = "mt-0"),
                      helpText(tr("bootstrap.critical.help")),
                      DT::dataTableOutput("bootstrap_critical_elements")
        ))
      ),

      br(),

      fluidRow(
        column(12, div(class = "seriarc-panel",
                       h4(tr("bootstrap.quality"), class = "mt-0"),
                       helpText(tr("bootstrap.quality.help")),
                       div(
                         style = "max-height: 400px; overflow-y: scroll; overflow-x: hidden; background: #f8f9fa; padding: 10px; border-radius: 4px; white-space: pre-wrap; font-family: monospace; font-size: 12px;",
                         verbatimTextOutput("bootstrap_quality_stats")
                       )
        ))
      )
    )
  }
})

# HELP MODAL with user-facing text
observeEvent(input$bootstrap_help, {
  showModal(modalDialog(
    title = div(icon("graduation-cap"), " Bootstrap Analysis - Help & Interpretation"),
    size = "l",
    easyClose = TRUE,
    footer = modalButton("Close"),

    div(style = "max-height: 70vh; overflow-y: auto;",

        # === 1. FUNDAMENTALS ===
        h4(icon("question-circle"), " What is Bootstrapping?"),
        p(
          "Bootstrapping is a resampling technique. It helps assess how ",
          strong("stable"),
          " a result is when the data varies slightly (e.g., due to sampling effects, recording, fragmentation)."
        ),
        p(
          "Many 'pseudo-datasets' are generated from the existing matrix (resampling). ",
          "The analysis is recalculated for each. If results fluctuate strongly, the solution is sensitive; ",
          "if they remain similar, it is robust."
        ),

        h5("What does this module do specifically?"),
        p("This module performs bootstrap resampling of a correspondence analysis (CA)."),
        p(strong("The procedure is:")),
        tags$ol(
          tags$li("Starting point is a contingency table with count values (e.g., sites × types)."),
          tags$li("Cells are resampled many times via Poisson resampling. Both cell values and totals may vary - suitable for archaeological data without fixed find counts."),
          tags$li("A CA is calculated for each resampled matrix."),
          tags$li(
            "For each element (site or type), the following is evaluated:",
            tags$ul(
              tags$li("how much its coordinates scatter across all bootstrap runs,"),
              tags$li("how large the uncertainty is in the space of selected dimensions.")
            )
          )
        ),
        p(strong("The result is not a new CA, but an uncertainty assessment of the existing CA solution.")),

        hr(),

        # === 2. INTERPRETATION OF THE DISPLAY ===
        h4(icon("chart-scatter"), " Interpretation of the Display"),

        h5("What do the points in the plot show?"),
        p("The points are the CA coordinates from the original analysis (not the mean of bootstrap runs)."),
        p("They continue to show:"),
        tags$ul(
          tags$li("the relative position of sites and types,"),
          tags$li("the structure along the selected dimensions.")
        ),

        h5("What do the confidence ellipses mean?"),
        p("The ellipses show how much the estimated position of an element fluctuates across many bootstrap replications."),

        div(class = "alert alert-warning",
            p(strong("Important:")),
            tags$ul(
              tags$li("The ellipses are ", strong("not classical confidence intervals"), " in the strict inferential statistical sense."),
              tags$li(
                "They describe the ",
                strong("stability of position"),
                " of a point under resampling - not the 'truth' of an unknown parameter."
              )
            )
        ),

        tags$dl(
          tags$dt(icon("circle", style = "color:#27ae60;"), " Small ellipse:"),
          tags$dd("The element's position is ", strong("robust"), " against data variation."),

          tags$dt(icon("circle", style = "color:#e74c3c;"), " Large ellipse:"),
          tags$dd("The position is ", strong("sensitive"), " to sampling fluctuations (often with rare types, heterogeneous contexts, or peripheral positions).")
        ),

        h5("How should stability be interpreted?"),
        p("The stability measures (e.g., scatter/region) are ", strong("relative quantities"), "."),
        p("This means:"),
        tags$ul(
          tags$li("'Stable' means: stable ", strong("compared to other elements"), " within the same analysis."),
          tags$li(
            "Absolute thresholds are not universal and depend on:",
            tags$ul(
              tags$li("data size,"),
              tags$li("dimensionality,"),
              tags$li("structure/sparsity of the matrix.")
            )
          )
        ),

        div(class = "alert alert-info",
            p(strong("Practical note:")),
            p(
              "Particularly important are elements with ",
              strong("large ellipse and high contribution to the axis"),
              ": They strongly influence the interpretation but are uncertainly positioned."
            )
        ),

        hr(),

        # === 3. IMPORTANT NOTES ===
        h4(icon("exclamation-triangle"), " Important Notes on Interpretation"),

        tags$ul(
          tags$li("The analysis assumes that the matrix contains ", strong("count values"), " (not normalized profiles)."),
          tags$li("The results apply to the ", strong("selected dimensions"), ". A point may appear stable in 2D but be unstable in higher dimensions."),
          tags$li(strong("Supplementary elements"), " are not bootstrapped and therefore not directly comparable with active elements.")
        ),

        h5("What does this analysis answer - and what not?"),

        div(class = "alert alert-success",
            h6("✅ Suitable for:"),
            tags$ul(
              tags$li("Assessing the robustness of CA structures"),
              tags$li("Identifying unstable sites or types"),
              tags$li("Comparing relative stability within a dataset")
            )
        ),

        div(class = "alert alert-danger",
            h6("❌ Not suitable for:"),
            tags$ul(
              tags$li("formal hypothesis tests"),
              tags$li("p-values or significance in the classical sense"),
              tags$li("comparing uncertainties between completely different datasets")
            )
        ),

        hr(),

        # === 4. PARAMETERS & FUNCTIONS ===
        h4(icon("sliders-h"), " Parameters & Functions"),

        tags$dl(
          tags$dt(icon("calculator"), " Iterations"),
          tags$dd(
            tags$ul(
              tags$li(strong("999:"), " For initial exploration / tests"),
              tags$li(strong("2,000-5,000:"), " For robust statements and documentation")
            )
          ),

          tags$dt(icon("bullseye"), " Confidence Region"),
          tags$dd(
            tags$ul(
              tags$li(strong("95% (default):"), " Contains 95% of bootstrap replicates"),
              tags$li(strong("90%:"), " Narrower region"),
              tags$li(strong("99%:"), " More conservative, wider ellipses")
            )
          ),

          tags$dt(icon("star"), " Top-10 Critical Elements"),
          tags$dd(
            p("Combines ", strong("Contribution"), " (influence on CA axes) with ", strong("Instability"), " (uncertainty)."),
            p(strong("Criticality Score:"), " Contribution × Instability (both normalized)."),
            p("→ These elements are often the most important 'levers' for interpretation - and at the same time the most uncertain.")
          )
        ),

        hr(),

        # === 5. EXTENDED INFORMATION (collapsed) ===
        tags$details(
          tags$summary(
            style = "cursor: pointer; font-size: 1.1em; font-weight: bold; padding: 10px; background: #f8f9fa; border-radius: 4px;",
            icon("caret-right"), " Extended Information (click to expand)"
          ),

          div(style = "padding: 15px;",

              # Eigenvalue diagnostics
              h5(icon("exclamation-triangle"), " Eigenvalue Diagnostics & Axis Instability"),

              div(class = "alert alert-danger",
                  h6("🔴 Critical: Close Eigenvalues"),
                  p(
                    "When eigenvalues are close together (e.g., ratio λ1/λ2 < 1.2), bootstrap replicates may ",
                    strong("swap or rotate axes"),
                    ". Then ellipses become difficult to interpret."
                  ),
                  tags$ul(
                    tags$li(strong("Problem:"), " Axis 1 in Rep. A does not necessarily correspond to Axis 1 in Rep. B"),
                    tags$li(strong("Consequence:"), " Ellipses may appear artificially large"),
                    tags$li(strong("Tip:"), " Check quality panel and interpret results cautiously")
                  )
              ),

              # Data model validation
              h5(icon("database"), " Data Model: Is the data suitable?"),

              p(
                strong("Bootstrap assumes:"),
                " The matrix contains ",
                strong("counts"),
                " or at least frequencies."
              ),
              tags$ul(
                tags$li(strong("✅ Suitable:"), " Raw counts, frequencies"),
                tags$li(strong("⚠️ Problematic:"), " Normalized data (row profiles, TF-IDF)"),
                tags$li(strong("❌ Unsuitable:"), " Log-transformed, negative values, already standardized")
              ),
              p("→ Warnings appear at bootstrap start if problems are detected."),

              # Best Practices
              h5(icon("lightbulb"), " Best Practices Workflow"),

              tags$ol(
                tags$li(strong("Check data:"), " Quality panel → note warnings"),
                tags$li(strong("Start:"), " 999 iterations, 95% confidence (Poisson fixed)"),
                tags$li(strong("Eigenvalues:"), " Ratio λ1/λ2 < 1.2? → Interpret ellipses cautiously"),
                tags$li(strong("Screening:"), " Sort table by uncertainty"),
                tags$li(strong("Top-10:"), " Review critical elements substantively"),
                tags$li(strong("Visualization:"), " Enable ellipses for few elements"),
                tags$li(strong("Publication:"), " 2000+ iterations, save Excel export")
              ),

              # Technical details
              h5(icon("cogs"), " Technical Details & References"),

              p(class = "text-muted small",
                strong("R package used:"), tags$br(),
                tags$code("cabootcrs"), " - Bootstrap Confidence Regions for Correspondence Analysis", tags$br(),
                "Ringrose, T. J. (2022). cabootcrs: Bootstrap Confidence Regions for Simple and Multiple Correspondence Analysis. ",
                "R package version 2.1.0. ",
                tags$a(
                  href = "https://CRAN.R-project.org/package=cabootcrs",
                  target = "_blank",
                  icon("external-link-alt"), " CRAN"
                )
              ),

              p(class = "text-muted small",
                strong("References (selection):"), tags$br(),
                "Ringrose, T. J. (2012). Bootstrap confidence regions for correspondence analysis. ",
                tags$em("Journal of Statistical Computation and Simulation, 82"), "(10), 1397-1413.", tags$br(),
                "Greenacre, M. (2007). ", tags$em("Correspondence Analysis in Practice"), " (2nd ed.). Chapman & Hall/CRC."
              )
          )
        )
    )
  ))
})

# mod_oxcal_seq_ui.R
# UI components for OxCal sequence module
# EXTENDED: Material-based outlier assignment + MCMC parameters + phase analysis

# ===================== MAIN UI =====================
mod_oxcal_seq_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  tagList(
    # JavaScript variables for enhanced features
    tags$script(sprintf("window.ENHANCED_CQL_SUPPORT = %s;",
                       if(exists("ENHANCED_CQL_SUPPORT")) tolower(ENHANCED_CQL_SUPPORT) else "false")),

    # JavaScript for Bootstrap collapse chevron animations (same as CA tab)
    tags$script(HTML("
      $(document).on('shown.bs.collapse', '.oxcal-collapse', function() {
        $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-down').addClass('glyphicon-chevron-up');
      });
      $(document).on('hidden.bs.collapse', '.oxcal-collapse', function() {
        $(this).parent().find('.collapse-chevron').removeClass('glyphicon-chevron-up').addClass('glyphicon-chevron-down');
      });
    ")),

    h3(tr("oxcal.title")),

    # WORKFLOW WARNING: Chronology Curve required
    uiOutput(ns("workflow_warning")),

    # ONLY SHOW WHEN DATA AVAILABLE
    conditionalPanel(
      condition = sprintf("output['%s']", ns("data_available")),
      fluidRow(
        # ===================== LEFT SIDEBAR =====================
        column(4,
          div(class = "seriarc-panel",

            # ==================== PANEL 1: PHASE MODE (primary, open by default) ====================
            div(class = "panel panel-primary", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#oxcal-panel-phasemode",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("oxcal.panel.phasemode")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "oxcal-panel-phasemode", class = "panel-collapse collapse in oxcal-collapse",
                div(class = "panel-body", style = "padding: 12px;",
                  # Phase Mode Selection
                  radioButtons(ns("phase_mode"), tr("oxcal.phase.mode"),
                    choices = setNames(
                      c("sites", "clusters", "groups"),
                      c(tr("oxcal.phase.sites"), tr("oxcal.phase.clusters"), tr("oxcal.phase.groups"))
                    ), selected = "sites", inline = FALSE
                  ),
                  uiOutput(ns("phase_mode_info"))
                )
              )
            ),

            # ==================== PANEL 2: MODEL OPTIONS (info, open by default) ====================
            div(class = "panel panel-info", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#oxcal-panel-model",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("oxcal.panel.model")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "oxcal-panel-model", class = "panel-collapse collapse in oxcal-collapse",
                div(class = "panel-body", style = "padding: 12px;",
                  # Auto Boundaries
                  checkboxInput(ns("autoBoundaries"), tr("oxcal.boundaries.auto"), FALSE),
                  # Boundary strategy selection
                  conditionalPanel(
                    condition = sprintf("input['%s']", ns("autoBoundaries")),
                    radioButtons(ns("boundary_strategy"), tr("oxcal.boundary.strategy"),
                      choices = setNames(
                        c("shared", "explicit"),
                        c(tr("oxcal.boundary.shared"), tr("oxcal.boundary.explicit"))
                      ), selected = "shared", inline = FALSE
                    ),
                    div(class = "alert alert-info", style = "padding: 6px; margin: 5px 0; font-size: 0.85em;",
                      HTML(tr("oxcal.boundary.info"))
                    )
                  ),
                  # Span options
                  checkboxInput(ns("addSpanPhase"), tr("oxcal.span.phase"), FALSE),
                  checkboxInput(ns("addSpanSeq"), tr("oxcal.span.seq"), TRUE)
                )
              )
            ),

            # ==================== PANEL 3: OUTLIER & CORRECTION (warning, closed by default) ====================
            div(class = "panel panel-warning", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#oxcal-panel-outlier",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("oxcal.panel.outlier")),
                  tags$span(class = "glyphicon glyphicon-chevron-down collapse-chevron")
                )
              ),
              div(id = "oxcal-panel-outlier", class = "panel-collapse collapse oxcal-collapse",
                div(class = "panel-body", style = "padding: 12px;",
                  # Outlier-Test Konfiguration
                  checkboxInput(ns("test_outlier"), tr("oxcal.outlier.model"), FALSE),
                  conditionalPanel(
                    condition = sprintf("input['%s']", ns("test_outlier")),
                    .ui_outlier_options(ns, tr)
                  )
                )
              )
            ),

            # ==================== PANEL 4: DISPLAY (success, closed by default) ====================
            div(class = "panel panel-success", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#oxcal-panel-display",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("oxcal.panel.display")),
                  tags$span(class = "glyphicon glyphicon-chevron-down collapse-chevron")
                )
              ),
              div(id = "oxcal-panel-display", class = "panel-collapse collapse oxcal-collapse",
                div(class = "panel-body", style = "padding: 12px;",
                  # Display-Optionen
                  radioButtons(ns("ci_level"), tr("oxcal.ci.level"),
                    choices = setNames(
                      c("68", "95", "both"),
                      c(tr("oxcal.ci.68"), tr("oxcal.ci.95"), tr("oxcal.ci.both"))
                    ), selected = "both"
                  ),
                  checkboxInput(ns("show_likelihood"), tr("oxcal.show.likelihood"), FALSE),
                  # Enhanced CQL Option (conditional auf ENHANCED_CQL_SUPPORT)
                  conditionalPanel(
                    condition = "typeof(window.ENHANCED_CQL_SUPPORT) !== 'undefined' && window.ENHANCED_CQL_SUPPORT",
                    checkboxInput(ns("enhanced_cql"), tr("oxcal.enhanced.cql"), FALSE),
                    div(class = "alert alert-info", style = "padding: 4px; margin: 3px 0; font-size: 0.8em;",
                      HTML(tr("oxcal.enhanced.cql.info"))
                    )
                  )
                )
              )
            ),

            # ==================== RANGE SELECTION (outside panels) ====================
            uiOutput(ns("interval_from_to")),

            # ==================== OXCAL STATUS (outside panels) ====================
            hr(),
            .ui_oxcal_status(ns, tr),

            # ==================== ACTIONS (outside panels) ====================
            hr(),
            .ui_actions(ns, tr)

          ) # end seriarc-panel
        ), # end column(4)

        # ===================== RIGHT SIDE: RESULTS =====================
        column(8,
          .ui_results(ns, tr)
        )
      ),
      hr(),
      # CQL Vorschau
      .ui_cql_preview(ns, tr)
    )
  )
}

# ===================== UI-KOMPONENTEN =====================

.ui_outlier_options <- function(ns, tr = function(x) x) {
  tagList(
    # CHARCOAL CORRECTION
    div(style = "background: #e8f5e9; padding: 10px; border-radius: 4px; margin: 5px 0;",
      h6(tr("oxcal.charcoal.title")),
      checkboxInput(ns("charcoal_correction"), tr("oxcal.charcoal.auto"), FALSE),
      conditionalPanel(
        condition = sprintf("input['%s']", ns("charcoal_correction")),
        numericInput(ns("charcoal_offset"), tr("oxcal.charcoal.offset"),
          value = 40, min = 0, max = 200, step = 20
        ),
        div(class = "alert alert-info", style = "padding: 5px; margin: 5px 0; font-size: 0.75em;",
          HTML(tr("oxcal.charcoal.info"))
        )
      )
    ),

    # OUTLIER MODEL
    div(style = "background: #fff3cd; padding: 10px; border-radius: 4px; margin: 10px 0 5px 0;",
      h6(tr("oxcal.outlier.title")),
      radioButtons(ns("outlier_mode"), tr("oxcal.outlier.mode.label"),
        choices = setNames(
          c("none", "general", "individual", "combined"),
          c(tr("oxcal.outlier.none"), tr("oxcal.outlier.general"),
            tr("oxcal.outlier.individual"), tr("oxcal.outlier.combined"))
        ), selected = "none"
      ),
      conditionalPanel(
        condition = sprintf("input['%s'] != 'none'", ns("outlier_mode")),
        numericInput(ns("outlier_prior"), tr("oxcal.outlier.prior"),
          value = 5, min = 0, max = 100, step = 1
        ),
        # MODE EXPLANATION
        div(class = "alert alert-info", style = "padding: 8px; margin: 8px 0; font-size: 0.8em;",
          uiOutput(ns("outlier_mode_explanation"))
        )
      )
    )
  )
}

.ui_oxcal_status <- function(ns, tr = function(x) x) {
  # Cloud mode: Show hint instead of OxCal setup
  if (exists("RUNNING_ON_CLOUD") && RUNNING_ON_CLOUD) {
    div(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; border: 1px solid #ffc107;",
      h5(HTML(tr("oxcal.cloud.title"))),
      p(style = "font-size: 0.9em; margin-bottom: 8px;",
        tr("oxcal.cloud.desc")
      ),
      div(class = "alert alert-info", style = "padding: 8px; margin: 5px 0; font-size: 0.85em;",
        HTML(tr("oxcal.cloud.howto"))
      )
    )
  } else {
    div(style = "background-color: #f8f9fa; padding: 10px; border-radius: 5px;",
      h5(tr("oxcal.status.title")),
      verbatimTextOutput(ns("oxcal_status")),
      uiOutput(ns("oxcal_setup_ui"))
    )
  }
}

.ui_actions <- function(ns, tr = function(x) x) {
  # Cloud mode: Button disabled, CQL download only
  if (exists("RUNNING_ON_CLOUD") && RUNNING_ON_CLOUD) {
    tagList(
      # Disabled button with tooltip explanation
      div(title = tr("oxcal.btn.run.disabled.tooltip"),
        actionButton(ns("run"), tr("oxcal.btn.run"), class = "btn btn-secondary", disabled = TRUE)
      ),
      tags$small(class = "text-muted", tr("oxcal.cloud.unavailable")),
      br(), br(),
      downloadButton(ns("dl_cql"), tr("oxcal.btn.dl.cql")),
      tags$small(class = "text-muted d-block mt-1",
        tr("oxcal.cloud.copy.hint")
      )
    )
  } else {
    tagList(
      actionButton(ns("run"), tr("oxcal.btn.run"), class = "btn btn-primary"),
      br(), br(),
      downloadButton(ns("dl_cql"), tr("oxcal.btn.dl.cql")),
      downloadButton(ns("dl_js"), tr("oxcal.btn.dl.js"))
    )
  }
}

.ui_results <- function(ns, tr = function(x) x) {
  tagList(
    # Globale Agreement-Zusammenfassung
    tags$div(
      style = "margin:6px 0 8px 0; padding: 8px; background-color: #f8f9fa; border-radius: 4px;",
      uiOutput(ns("agreements_summary"))
    ),

    h5(tr("oxcal.results.title")),

    # Extended help texts with detailed explanations
    tags$div(
      style = "font-size:0.85em;color:#666;margin-bottom:8px;",
      HTML(paste0('<strong>', tr("oxcal.legend"), '</strong> ')),
      HTML(paste0('<span title="', tr("oxcal.legend.agreement.tooltip"), '" style="cursor:help;">', tr("oxcal.legend.agreement"), '</span> · ')),
      HTML(paste0('<span title="', tr("oxcal.legend.outlier.tooltip"), '" style="cursor:help;">', tr("oxcal.legend.outlier"), '</span>')),
      br(),
      HTML(paste0('<strong>', tr("oxcal.legend.global"), '</strong> ')),
      HTML(paste0('<span title="', tr("oxcal.legend.aoverall.tooltip"), '" style="cursor:help;">', tr("oxcal.legend.aoverall"), '</span> · ')),
      HTML(paste0('<span title="', tr("oxcal.legend.amodel.tooltip"), '" style="cursor:help;">', tr("oxcal.legend.amodel"), '</span> · ')),
      HTML(paste0('<span title="', tr("oxcal.legend.acomb.tooltip"), '" style="cursor:help;">', tr("oxcal.legend.acomb"), '</span>'))
    ),

    # TAB STRUCTURE: Table and plot separated
    tabsetPanel(
      type = "tabs",

      # TAB 1: Zusammenfassungstabelle
      tabPanel(
        title = tr("oxcal.tab.summary"),
        value = "summary_tab",
        br(),
        DT::dataTableOutput(ns("tbl_summary"))
      ),

      # TAB 2: Posterior-Plot
      tabPanel(
        title = tr("oxcal.tab.posterior"),
        value = "posterior_tab",
        br(),
        tags$div(
          style = "font-size:0.9em;color:#666;margin-bottom:10px;",
          HTML(tr("oxcal.posterior.desc"))
        ),
        plotlyOutput(ns("plt_posterior"), height = "450px")
      ),

      # TAB 3: Phasen-Analyse
      tabPanel(
        title = tr("oxcal.tab.phases"),
        value = "phases_tab",
        br(),
        tags$div(
          style = "font-size:0.9em;color:#666;margin-bottom:10px;",
          HTML(tr("oxcal.phases.desc"))
        ),

        # Sub-tabs for different aspects
        tabsetPanel(
          type = "pills",

          tabPanel(
            title = tr("oxcal.phases.timeline"),
            br(),
            plotlyOutput(ns("plt_phases"), height = "600px"),
            # Export buttons for timeline
            div(class = "mt-3",
              h6(tr("oxcal.phases.export")),
              div(class = "btn-group btn-group-sm", role = "group",
                downloadButton(ns("dl_phases_png"), "PNG", class = "btn btn-outline-primary btn-sm"),
                downloadButton(ns("dl_phases_svg"), "SVG", class = "btn btn-outline-success btn-sm"),
                downloadButton(ns("dl_phases_html"), tr("oxcal.phases.html"), class = "btn btn-outline-info btn-sm")
              ),
              tags$small(class = "text-muted d-block mt-1",
                tr("oxcal.phases.export.hint")
              )
            )
          ),

          tabPanel(
            title = tr("oxcal.phases.table"),
            br(),
            DT::dataTableOutput(ns("tbl_phases"))
          )
        )
      )
    )
  )
}

.ui_cql_preview <- function(ns, tr = function(x) x) {
  tagList(
    h5(tr("oxcal.cql.preview")),
    verbatimTextOutput(ns("log"))
  )
}

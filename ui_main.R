# SeriARC v1.0.0 - UI Definition
# i18n: UI is built per session from ?lang= (function(request) in app.R)

source("helpers/ui_components.R")

# Helper function for tooltips with info symbol
tooltip_info <- function(text) {
  tags$span(
    class = "seriarc-info-symbol",
    title = text,
    "ℹ️"
  )
}

# Language selector links (DE/EN) - triggers actual browser reload
# Position is controlled via CSS (seriarc-styles.css)
language_selector <- function(current_lang) {
  div(
    class = "seriarc-lang-selector",
    tags$a(
      href = "?lang=de",
      class = if (current_lang == "de") "lang-active" else "lang-inactive",
      style = paste0(
        "padding: 4px 10px; text-decoration: none; border-radius: 4px; font-size: 13px;",
        if (current_lang == "de") " background: #007bff; color: white; font-weight: bold;" else " background: rgba(255,255,255,0.2); color: white;"
      ),
      "DE"
    ),
    tags$a(
      href = "?lang=en",
      class = if (current_lang == "en") "lang-active" else "lang-inactive",
      style = paste0(
        "padding: 4px 10px; text-decoration: none; border-radius: 4px; font-size: 13px;",
        if (current_lang == "en") " background: #007bff; color: white; font-weight: bold;" else " background: rgba(255,255,255,0.2); color: white;"
      ),
      "EN"
    )
  )
}

# UI definition with tr() function as parameter
# IMPORTANT: tr must be passed, default is only for fallback
create_ui <- function(tr = NULL, lang = NULL) {
  # Use "de" as hardcoded fallback if DEFAULT_LANGUAGE doesn't exist yet
  if (is.null(lang)) lang <- if (exists("DEFAULT_LANGUAGE")) DEFAULT_LANGUAGE else "de"
  if (is.null(tr)) tr <- if (exists("make_tr")) make_tr(lang) else function(key) key

  tagList(
    # Loading Screen (only for cloud mode)
    if (exists("RUNNING_ON_CLOUD") && RUNNING_ON_CLOUD) {
      tags$div(
        id = "seriarc-loading-screen",
        tags$div(class = "seriarc-loader",
          tags$h2("📊 SeriARC"),
          tags$p(if (lang == "de") "Wird geladen..." else "Loading..."),
          tags$div(class = "seriarc-spinner")
        )
      )
    },
    
    navbarPage(
      title = tr("app.title"),
      id = "mainnav",
      header = tagList(
        useShinyjs(),
        tags$head(
          tags$link(rel = "stylesheet", type = "text/css", href = paste0("seriarc-styles.css?v=", format(Sys.time(), "%Y%m%d%H%M%S"))),
          tags$script(src = "tooltips.js"),
          # JavaScript to move subtitle and language selector into navbar-header
          tags$script(HTML("
            $(document).ready(function() {
              setTimeout(function() {
                // Move elements into navbar-header
                var subtitle = $('.seriarc-navbar-subtitle').detach();
                var langSelector = $('.seriarc-lang-selector').detach();
                $('.navbar-header').append(subtitle).append(langSelector);
              }, 50);
              
              // Show loading spinner IMMEDIATELY when file is selected
              // Use event delegation since module loads after this script
              $(document).on('change', '#import-file', function() {
                if (this.files && this.files.length > 0) {
                  $('#welcome-screen-import').hide();
                  $('#data-loading-indicator').show();
                }
              });
            });
          ")),
          # Hide loading screen when Shiny is ready
          if (exists("RUNNING_ON_CLOUD") && RUNNING_ON_CLOUD) {
            tags$script(HTML("
              $(document).on('shiny:connected', function() {
                setTimeout(function() {
                  $('#seriarc-loading-screen').addClass('loaded');
                  setTimeout(function() {
                    $('#seriarc-loading-screen').remove();
                  }, 500);
                }, 300);
              });
            "))
          }
        ),
        div(class = "watermark", paste("SeriARC v", APP_VERSION)),
        div(class = "seriarc-navbar-subtitle", tr("app.subtitle")),
        language_selector(lang)
    ),

    # Footer with branding
    footer = div(class = "seriarc-footer",
      hr(style = "margin-top: 50px;"),
      div(style = "text-align: center; padding: 20px; background-color: #f8f9fa; color: #6c757d;",
        div(style = "margin-bottom: 10px;",
          strong(tr("app.title")), " - ", tr("app.footer")
        ),
        div(style = "font-size: 0.9em; margin-bottom: 10px;",
          "\u00a9 ", format(Sys.Date(), "%Y"), " Daniel Meixner (University of Regensburg)"
        ),
        div(style = "font-size: 0.8em;",
          a(href = "mailto:Daniel.Meixner@geschichte.uni-regensburg.de", "Daniel.Meixner@geschichte.uni-regensburg.de")
        )
      )
    ),

    # TAB: Data
    tabPanel(tr("nav.data"),
      tabsetPanel(
        id = "data_subtabs",

        # SUBTAB: Import
        tabPanel(tr("nav.data.import"),
          fluidPage(
            fluidRow(
              column(4,
                div(class = "seriarc-panel seriarc-panel-primary",
                  h4(tr("import.title"), class = "mt-0"),
                  mod_data_import_ui("import", tr = tr),

                  uiOutput("debug_panel_import")
                )
              ),

              column(8,
                # Static welcome screen (fast loading, hidden when data loads)
                div(id = "welcome-screen-import",
                  welcome_data_ui(tr)
                ),
                # Loading indicator (shown while processing)
                div(id = "data-loading-indicator",
                  div(class = "spinner"),
                  h3(if (lang == "de") "Daten werden verarbeitet..." else "Processing data..."),
                  tags$p(if (lang == "de") "Bitte warten Sie einen Moment" else "Please wait a moment")
                ),
                # Dynamic content after data load
                uiOutput("import_main_content")
              )
            )
          )
        ),

        # SUBTAB: Filter
        tabPanel(tr("nav.data.filter"),
          fluidPage(
            uiOutput("filter_main_content")
          )
        ),

        # SUBTAB: Aggregation
        tabPanel(tr("nav.data.aggregation"),
          fluidPage(
            uiOutput("aggregation_main_content")
          )
        )
      )
    ),

    # TAB: Correspondence Analysis
    tabPanel(tr("nav.ca"),
      tabsetPanel(
        id = "ca_subtabs",

        # SUBTAB: Standard CA
        tabPanel(tr("nav.ca.standard"),
          fluidPage(
            uiOutput("ca_main_content")
          )
        ),

        # SUBTAB: Detrended CA
        tabPanel(tr("nav.ca.detrended"),
          fluidPage(
            uiOutput("detrended_ca_main_content")
          )
        )
      )
    ),

    # TAB: Bootstrap
    tabPanel(tr("nav.bootstrap"),
      fluidPage(
        uiOutput("bootstrap_main_content")
      )
    ),

    # TAB: 3D-CA
    tabPanel(tr("nav.3d"),
      fluidPage(
        uiOutput("ca_3d_main_content")
      )
    ),

    # TAB: Cluster
    tabPanel(tr("nav.cluster"),
      fluidPage(
        uiOutput("cluster_main_content")
      )
    ),

    # TAB: Seriation
    tabPanel(tr("nav.seriation"),
      fluidPage(
        uiOutput("seriation_main_content")
      )
    ),

    # TAB: Battleship
    tabPanel(tr("nav.battleship"),
      fluidPage(
        uiOutput("battleship_main_content")
      )
    ),

    # TAB: Mapping
    tabPanel(tr("nav.mapping"),
      fluidPage(
        uiOutput("mapping_main_content")
      )
    ),

    # TAB: Typenkartierung
    tabPanel(tr("nav.typemap"),
      fluidPage(
        uiOutput("typemap_main_content")
      )
    ),

    # TAB: 14C Integration
    tabPanel(tr("nav.c14"),
      fluidPage(
        uiOutput("chronology_main_content")
      )
    )
  ) # end navbarPage
  ) # end tagList
}

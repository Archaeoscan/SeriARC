# server_observers.R - Event handlers and observers
# Extracted from server.R for better maintainability

# Mapping reactive connection
observe({
  tryCatch({
    mm <- mapping_meta()
    if (!is.null(mm)) {
      mapping$meta <- mm
    }
  }, error = function(e) {
    # Ignore mapping errors
  })

  tryCatch({
    ci <- mapping_coord_info()
    if (!is.null(ci)) mapping$coord_info <- ci
  }, error = function(e) {
    # Ignore coord_info errors
  })

  tryCatch({
    mr <- mapping_raw()
    if (!is.null(mr)) mapping$raw <- mr
  }, error = function(e) {
    # Ignore raw errors
  })
})

# Meta-data creation - DISABLED!
# Redundant with initialization in mod_data_filter.R
# This observer triggered 4x and overwrote the correct initialization!
# observeEvent(data_raw(), {
#   req(data_raw())
#   cat("\n🏭 SERVER_OBSERVERS: Creating meta$data with make_meta()\n")
#   cat("   data_raw() has", ncol(data_raw())-1, "Types\n")
#   meta$data <- make_meta(data_raw())
#   cat("   AFTER make_meta(): meta$data$types has", nrow(meta$data$types), "rows\n")
#   showNotification(sprintf("📊 Meta-data created: %d Sites, %d Types loaded",
#                            nrow(meta$data$sites), nrow(meta$data$types)),
#                    type = "default", duration = 3)
#   for(name in names(cache)) {
#     cache[[name]] <- NULL
#   }
# })

# File Loaded Status
observe({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  output$file_uploaded <- reactive(file_loaded)
  outputOptions(output, "file_uploaded", suspendWhenHidden = FALSE)
})

# Reactive connections for Sites/Types - DISABLED
# This observer triggered unwanted re-evaluations
# observe({
#   sites_df <- sites_selection_reactive()
#   types_df <- types_selection_reactive()
# }, priority = 100)

# Matrix Transformation Help Modal
observeEvent(input$show_transform_help, {
  showModal(modalDialog(
    title = paste0("⚖️ ", tr("transform.help.title")),
    size = "l",
    easyClose = TRUE,

    div(style="padding: 15px; max-height: 60vh; overflow-y: scroll;",

      p(style="font-size: 1.05em;", tr("transform.help.intro")),

      div(class="alert alert-warning", style="margin: 15px 0; font-size: 1.05em;",
        tags$b(paste0("⚠️ ", tr("transform.help.recommendation.title"))), br(),
        HTML(tr("transform.help.recommendation.text"))
      ),

      tags$hr(),

      # 1. No Transformation
      h4(paste0("1️⃣ ", tr("transform.help.none.title"))),

      p(tags$b(tr("transform.help.what")), " ", tr("transform.help.none.what")),

      p(tags$b(tr("transform.help.when"))),
      tags$ul(
        tags$li(tr("transform.help.none.when1")),
        tags$li(tr("transform.help.none.when2")),
        tags$li(tr("transform.help.none.when3"))
      ),

      p(tags$b(tr("transform.help.advantages"))),
      tags$ul(
        tags$li(tr("transform.help.none.adv1")),
        tags$li(tr("transform.help.none.adv2")),
        tags$li(tr("transform.help.none.adv3"))
      ),

      div(class="alert alert-info", style="margin: 10px 0;",
        tags$b(paste0("👉 ", tr("transform.help.rule"))), " ", tr("transform.help.none.rule")
      ),

      tags$hr(),

      # 2. Normalization
      h4(paste0("2️⃣ ", tr("transform.help.norm.title"))),

      p(tags$b(tr("transform.help.what")), " ", tr("transform.help.norm.what")),

      p(tags$b(tr("transform.help.when"))),
      tags$ul(
        tags$li(tags$b(tr("transform.help.sites")), " ", tr("transform.help.norm.when1")),
        tags$li(tags$b(tr("transform.help.types")), " ", tr("transform.help.norm.when2")),
        tags$li(tr("transform.help.norm.when3"))
      ),

      p(tags$b(tr("transform.help.advantages"))),
      tags$ul(
        tags$li(tr("transform.help.norm.adv1")),
        tags$li(tr("transform.help.norm.adv2")),
        tags$li(tr("transform.help.norm.adv3"))
      ),

      p(tags$b(tr("transform.help.disadvantage"))),
      tags$ul(
        tags$li(tr("transform.help.norm.disadv1"))
      ),

      div(class="alert alert-success", style="margin: 10px 0;",
        tags$b(paste0("👉 ", tr("transform.help.rule"))), " ", tr("transform.help.norm.rule")
      ),

      tags$hr(),

      # 3. Log Transformation
      h4(paste0("3️⃣ ", tr("transform.help.log.title"))),

      p(tags$b(tr("transform.help.what")), " ", HTML(tr("transform.help.log.what"))),

      div(class="alert alert-info", style="margin: 10px 0;",
        tags$b(paste0("ℹ️ ", tr("transform.help.log.technical.title"))), " ", HTML(tr("transform.help.log.technical"))
      ),

      p(tags$b(tr("transform.help.log.example")), style="font-family: monospace;"),
      tags$ul(style="font-family: monospace;",
        tags$li(HTML(tr("transform.help.log.ex0"))),
        tags$li(HTML(tr("transform.help.log.ex1"))),
        tags$li(HTML(tr("transform.help.log.ex2"))),
        tags$li(HTML(tr("transform.help.log.ex10"))),
        tags$li(HTML(tr("transform.help.log.ex100")))
      ),

      div(class="alert alert-warning", style="margin: 10px 0;",
        tags$b(tr("transform.help.log.effect.title")), " ", tr("transform.help.log.effect")
      ),

      p(tags$b(tr("transform.help.log.when.title"))),
      tags$ul(
        tags$li(tags$b(tr("transform.help.sites")), " ", tr("transform.help.log.when1")),
        tags$li(tags$b(tr("transform.help.types")), " ", tr("transform.help.log.when2")),
        tags$li(HTML(tr("transform.help.log.when3")))
      ),

      p(tags$b(tr("transform.help.log.why.title"))),
      tags$ul(
        tags$li(tr("transform.help.log.why1")),
        tags$li(tr("transform.help.log.why2")),
        tags$li(tr("transform.help.log.why3")),
        tags$li(tr("transform.help.log.why4"))
      ),

      div(class="alert alert-danger", style="margin: 10px 0;",
        HTML(tr("transform.help.log.rule"))
      ),

      tags$hr(),

      h4(paste0("⚠️ ", tr("transform.help.weight.title"))),

      p(HTML(tr("transform.help.weight.text1"))),

      tags$ul(
        tags$li(tags$b(tr("transform.help.weight.either")), " ", tr("transform.help.weight.weights")),
        tags$li(tags$b(tr("transform.help.weight.or")), " ", tr("transform.help.weight.transform"))
      ),

      p(tr("transform.help.weight.text2")),

      tags$hr(),

      # Testing Strategy
      h4(paste0("🧪 ", tr("transform.help.strategy.title"))),

      tags$ol(
        tags$li(HTML(tr("transform.help.strategy.step1"))),
        tags$li(HTML(tr("transform.help.strategy.step2"))),
        tags$li(HTML(tr("transform.help.strategy.step3"))),
        tags$li(HTML(tr("transform.help.strategy.step4"))),
        tags$li(tr("transform.help.strategy.step5"))
      ),

      tags$hr(),

      # Summary
      h4(paste0("✔️ ", tr("transform.help.summary.title"))),

      tags$table(class="table table-bordered table-striped", style="font-size: 0.9em;",
        tags$thead(
          tags$tr(
            tags$th(tr("transform.help.summary.situation")),
            tags$th(tr("transform.help.summary.recommendation"))
          )
        ),
        tags$tbody(
          tags$tr(
            tags$td(tr("transform.help.summary.sit1")),
            tags$td(tags$b(tr("transform.help.summary.rec1")))
          ),
          tags$tr(
            tags$td(tr("transform.help.summary.sit2")),
            tags$td(tags$b(tr("transform.help.summary.rec2")))
          ),
          tags$tr(
            tags$td(tr("transform.help.summary.sit3")),
            tags$td(tags$b(tr("transform.help.summary.rec3")))
          ),
          tags$tr(
            tags$td(tr("transform.help.summary.sit4")),
            tags$td(tags$b(tr("transform.help.summary.rec4")))
          ),
          tags$tr(
            tags$td(tr("transform.help.summary.sit5")),
            tags$td(tags$b(tr("transform.help.summary.rec5")))
          ),
          tags$tr(style="background-color: #f8d7da;",
            tags$td(tr("transform.help.summary.sit6")),
            tags$td(tags$b(tr("transform.help.summary.rec6")))
          )
        )
      )
    ),

    footer = modalButton(tr("term.close"))
  ))
})

# ui_chronology_content.R - SeriARC v1.0.0 14C Integration UI Content

output$chronology_main_content <- renderUI({
  fluidPage(
    # 14C integration workflow
    tabsetPanel(
      id = "c14_workflow_tabs",

      # TAB 1: IMPORT & MAPPING (with hero banner)
      tabPanel(tr("chrono.tab.import"),
        fluidRow(
          column(12,
            div(class = "seriarc-hero",
              h2(tr("chrono.tab.title")),
              p(tr("chrono.tab.desc"))
            )
          )
        ),
        mod_c14_ui("chronology", tr = tr)
      ),

      # TAB 2: CALIBRATION & EXPORT
      tabPanel(tr("chrono.tab.calib"),
        div(class = "seriarc-panel",
          mod_c14_calibration_ui("c14cal", tr = tr)
        )
      ),

      # TAB 3: CHRONOLOGY BRIDGE
      tabPanel(tr("chrono.tab.curve"),
        div(class = "seriarc-panel",
          mod_chronology_curve_ui("chrono", tr = tr)
        )
      ),

      # TAB 4: OXCAL SEQUENCE
      tabPanel(tr("chrono.tab.oxcal"),
        div(class = "seriarc-panel",
          mod_oxcal_seq_ui("oxseq", tr = tr)
        )
      )
    )
  )
})

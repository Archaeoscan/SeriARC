# ui_poly_content.R - Polynomial Projection Tab UI Content (Standalone)

output$poly_main_content <- renderUI({
  ca_ready <- !is.null(cache$ca_result)

  if (!ca_ready) {
    div(class = "alert alert-info", style = "margin: 30px; font-size: 1em;",
      tags$span(class = "glyphicon glyphicon-info-sign", style = "margin-right: 8px;"),
      tr("ca.poly.no.result")
    )
  } else {
    tagList(
      fluidRow(
        # ── LEFT SIDEBAR: Einstellungen (CA-Stil) ────────────────────────────
        column(3,
          div(class = "seriarc-panel",
            h4(tr("ca.poly.settings.title"), class = "mt-0"),

            # ── PANEL 1: Fitting & Projektion (aufgeklappt) ───────────────────
            div(class = "panel panel-primary", style = "margin-bottom: 10px;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#poly_panel_fitting",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("ca.poly.panel.fitting")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "poly_panel_fitting", class = "panel-collapse collapse in ca-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  h6(tr("ca.poly.fit.on"), tooltip_info(tr("ca.poly.fit.on.tooltip")),
                     style = "margin-top: 0;"),
                  radioButtons("poly_fit_on", NULL,
                    choices = c(
                      " Sites"         = "sites",
                      " Types"         = "types",
                      " Sites + Types" = "both"
                    ),
                    selected = "sites"
                  ),

                  # Passive Gruppe (nur wenn nicht "both")
                  conditionalPanel(
                    condition = "input.poly_fit_on != 'both'",
                    div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
                      checkboxInput("poly_show_passive", tr("ca.poly.show.passive"), value = TRUE),
                      tooltip_info(tr("ca.poly.show.passive.tooltip"))
                    )
                  ),

                  tags$hr(style = "margin: 8px 0;"),

                  # Types im Zeitstrahl (nur wenn Types projiziert)
                  conditionalPanel(
                    condition = "input.poly_fit_on != 'sites'",
                    div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
                      checkboxInput("poly_show_types_timeline", tr("ca.poly.show.types.timeline"), value = FALSE),
                      tooltip_info(tr("ca.poly.show.types.timeline.tooltip"))
                    )
                  )
                )
              )
            ),

            # ── PANEL 2: Anzeige-Optionen (aufgeklappt) ───────────────────────
            div(class = "panel panel-info", style = "margin-bottom: 0;",
              div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                  `data-toggle` = "collapse", `data-target` = "#poly_panel_display",
                div(style = "display: flex; justify-content: space-between; align-items: center;",
                  tags$span(style = "font-weight: bold;", tr("ca.panel.display")),
                  tags$span(class = "glyphicon glyphicon-chevron-up collapse-chevron")
                )
              ),
              div(id = "poly_panel_display", class = "panel-collapse collapse in ca-collapse",
                div(class = "panel-body", style = "padding: 12px;",

                  # --- Biplot-spezifisch ---
                  tags$small(tags$b("Biplot"), style = "color: #888; display: block; margin-bottom: 4px;"),
                  div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
                    checkboxInput("poly_show_projlines", tr("ca.poly.show.projlines"), value = FALSE),
                    tooltip_info(tr("ca.poly.show.projlines.tooltip"))
                  ),
                  div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
                    checkboxInput("poly_show_projonly", tr("ca.poly.show.projonly"), value = FALSE),
                    tooltip_info(tr("ca.poly.show.projonly.tooltip"))
                  ),

                  # --- Zeitstrahl-spezifisch (nur wenn Zeitstrahl-Tab aktiv) ---
                  conditionalPanel(
                    condition = "input.poly_viz_tabs == 'timeline'",
                    tags$hr(style = "margin: 8px 0;"),
                    tags$small(tags$b("Zeitstrahl"), style = "color: #888; display: block; margin-bottom: 4px;"),
                    div(style = "display: flex; align-items: center; gap: 5px; margin-bottom: 5px;",
                      checkboxInput("poly_show_timeline_lines", tr("ca.poly.show.timeline.lines"), value = FALSE),
                      tooltip_info(tr("ca.poly.show.timeline.lines.tooltip"))
                    )
                  ),

                  tags$hr(style = "margin: 8px 0;"),

                  # Labels
                  checkboxInput("poly_show_labels", tr("ca.show.labels"), value = TRUE),
                  conditionalPanel(
                    condition = "input.poly_show_labels",
                    div(style = "padding-left: 10px; border-left: 2px solid #5bc0de;",
                      sliderInput("poly_label_chars", tr("ca.label.chars"),
                                  min = 3, max = 40, value = 12, step = 1, width = "100%"),
                      sliderInput("poly_label_size",  tr("ca.label.size"),
                                  min = 8, max = 20, value = 11, step = 1, width = "100%")
                    )
                  ),

                  # Punktgröße
                  sliderInput("poly_point_size", tr("ca.point.size"),
                              min = 1, max = 8, value = 3, step = 0.5, width = "100%")
                )
              )
            )
          )
        ),

        # ── RIGHT: Sub-Tabs ──────────────────────────────────────────────────
        column(9,
          div(class = "seriarc-panel",
            tabsetPanel(
              id = "poly_viz_tabs",

              # Sub-Tab 1: Biplot
              tabPanel(tr("poly.tab.biplot"), value = "biplot",
                br(),
                div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
                  h5(tr("ca.poly.plot.title"), style = "margin: 0;"),
                  tooltip_info(tr("ca.poly.plot.tooltip"))
                ),

                # ── Erklärungstext (zugeklappt, über dem Plot) ───────────────
                div(class = "panel panel-success", style = "margin-bottom: 12px;",
                  div(class = "panel-heading", style = "cursor: pointer; padding: 8px 12px;",
                      `data-toggle` = "collapse", `data-target` = "#poly_explanation",
                    div(style = "display: flex; justify-content: space-between; align-items: center;",
                      tags$span(style = "font-weight: bold;", tr("ca.poly.exp.title")),
                      tags$span(class = "glyphicon glyphicon-chevron-down collapse-chevron")
                    )
                  ),
                  div(id = "poly_explanation", class = "panel-collapse collapse ca-collapse",
                    div(class = "panel-body", style = "font-size: 0.85em; line-height: 1.5;",

                      # ── Was ist Polynomprojektion? ──────────────────────────
                      tags$p("Die Polynomprojektion ist eine Methode, um die Ergebnisse einer Korrespondenzanalyse besser als Abfolge darzustellen."),
                      tags$p("Normalerweise werden Fundkomplexe in der CA nur entlang der ersten Achse (Dim1) angeordnet. Diese Achse zeigt den wichtigsten Entwicklungstrend im Material."),
                      tags$p("Oft verläuft dieser Trend im Diagramm aber nicht gerade, sondern gebogen \u2013 meist parabelförmig. In diesem Fall bildet Dim1 den Verlauf nur vereinfacht ab."),
                      tags$p("Die Polynomprojektion passt deshalb eine Kurve an die Punktwolke an und bestimmt die Position jedes Fundkomplexes entlang dieser Kurve. Diese Position (gemessen als Weg entlang der Kurve) wird als Ordnungs- und Abstandsmaß verwendet."),
                      tags$p("So lässt sich der tatsächliche Verlauf der Entwicklung genauer abbilden."),
                      div(class = "alert alert-info",
                          style = "padding: 5px 10px; margin: 4px 0 8px 0; font-size: 0.95em;",
                        tags$b("Wichtig:\u00a0"),
                        "Die Methode erzeugt keine neuen Informationen. Sie nutzt nur die vorhandene Struktur der CA-Darstellung besser aus."
                      ),

                      tags$hr(style = "margin: 8px 0;"),

                      # ── Algorithmus ─────────────────────────────────────────
                      tags$b("Algorithmus (vereinfacht)"), br(),
                      tags$ol(style = "margin: 4px 0 8px 0;",
                        tags$li("Eine Kurve wird an die Punktwolke im CA-Diagramm angepasst."),
                        tags$li("Jeder Fundkomplex wird auf den nächstgelegenen Punkt dieser Kurve projiziert."),
                        tags$li("Die Position entlang der Kurve wird als eindimensionale Ordnungsskala berechnet.")
                      ),

                      tags$hr(style = "margin: 8px 0;"),

                      # ── Wann sinnvoll? ──────────────────────────────────────
                      tags$b("Wann ist die Methode sinnvoll?"), br(),
                      tags$p(style = "margin-top: 4px;",
                        "Die Polynomprojektion ist besonders hilfreich, wenn der CA-Plot deutlich gebogen oder parabelförmig aussieht."),
                      tags$p("In solchen Fällen werden bei einer reinen Dim1-Sortierung die zeitlichen Abstände an den Rändern künstlich zusammengedrückt."),
                      tags$p("Die Kurvenprojektion \u201eentfaltet\u201c diese Verzerrung und bildet den Entwicklungsverlauf gleichmäßiger ab."),
                      tags$p(style = "margin-bottom: 2px;", "Wenn die Punktwolke dagegen"),
                      tags$ul(style = "margin: 0 0 4px 0;",
                        tags$li("nahezu gerade verläuft oder"),
                        tags$li("klar zwei unabhängige Strukturen zeigt,")
                      ),
                      tags$p("bringt die Methode meist keinen zusätzlichen Nutzen."),

                      tags$hr(style = "margin: 8px 0;"),

                      # ── Methodisch ──────────────────────────────────────────
                      tags$b("Worum geht es methodisch?"), br(),
                      tags$p(style = "margin-top: 4px;",
                        "Die CA zeigt den wichtigsten Entwicklungstrend im Material \u2013 den sogenannten Hauptgradienten."),
                      tags$p("Dim1 stellt diesen Trend als Gerade dar. Die Polynomprojektion folgt dagegen dem tatsächlichen, oft gebogenen Verlauf."),
                      tags$p("Beide beschreiben also denselben Trend \u2013 aber auf unterschiedliche Weise."),
                      tags$p(style = "margin-bottom: 2px;", "Je nach Form der Punktwolke können sich dadurch"),
                      tags$ul(style = "margin: 0 0 4px 0;",
                        tags$li("die Reihenfolge einzelner Fundkomplexe und"),
                        tags$li("die Abstände zwischen ihnen")
                      ),
                      tags$p("unterscheiden."),

                      tags$hr(style = "margin: 8px 0;"),

                      # ── Praktischer Hinweis ─────────────────────────────────
                      tags$b("Praktischer Hinweis"), br(),
                      tags$p(style = "margin-top: 4px;",
                        "In der Praxis ist es sinnvoll, beide Darstellungen auszuprobieren:"),
                      tags$ul(style = "margin: 0 0 4px 0;",
                        tags$li("Sortierung entlang Dim1"),
                        tags$li("Sortierung entlang der Kurve (Polynomprojektion)")
                      ),
                      tags$p("Anschließend sollte man prüfen, welche Variante besser zu den archäologischen Erwartungen passt \u2013 zum Beispiel zur Stratigraphie, zu bekannten Phasenabfolgen oder zu unabhängigen Datierungen."),
                      tags$p("Die Methode ist also kein Automatismus, sondern ein Werkzeug, das man bewusst einsetzen und prüfen sollte."),

                      tags$hr(style = "margin: 8px 0;"),

                      # ── Literatur ───────────────────────────────────────────
                      tags$b("Literatur"), br(),
                      tags$small(style = "line-height: 1.6;",
                        "Weninger, B. & Krauß, R. (2021). Pottery Dating by Correspondence Analysis \u2013 ",
                        "Time Series Analysis. In: Wissing, A. et al. (eds.), ",
                        tags$em("Von der Tongrube bis zur Abfallgrube"),
                        ". Tübinger Schriften zur Ur- und Frühgeschichtlichen Archäologie, pp. 31\u201356.", br(),
                        "Leibovici, C. (2014). Arc length of general polynomial. ",
                        tags$a(href = "https://math.stackexchange.com/questions/922098/arc-length-of-general-polynomial",
                               target = "_blank",
                               "math.stackexchange.com"),
                        ". Last visited: 26 Sep 2023.", br(),
                        "Greenacre, M. (2017). ",
                        tags$em("Correspondence Analysis in Practice"), ", 3rd ed. CRC Press."
                      )
                    )
                  )
                ),

                plotlyOutput("poly_biplot", height = "540px"),

                # ── Export-Button unter dem Biplot ───────────────────────────
                div(style = "margin-top: 10px;",
                  div(style = "display: flex; align-items: center; gap: 8px;",
                    downloadButton("download_poly_scores", tr("ca.poly.export"),
                                   class = "btn btn-success btn-sm",
                                   style = "min-width: 220px;"),
                    tooltip_info(tr("ca.poly.export.tooltip"))
                  )
                )
              ),

              # Sub-Tab 2: Zeitstrahl
              tabPanel(tr("poly.tab.timeline"), value = "timeline",
                br(),
                div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
                  h5(tr("ca.poly.timeline.title"), style = "margin: 0;"),
                  tooltip_info(tr("ca.poly.timeline.tooltip"))
                ),
                div(class = "alert alert-info",
                    style = "font-size: 0.82em; padding: 6px 10px; margin-bottom: 10px;",
                  tags$span(class = "glyphicon glyphicon-info-sign",
                            style = "margin-right: 4px;"),
                  tr("ca.poly.timeline.hint")
                ),

                # ── Zeitskala-Controls ──────────────────────────────────────
                div(class = "well well-sm",
                    style = "padding: 8px 12px; margin-bottom: 10px; background: #f9f9f9;",

                  # Toggle – kompakte einzelne Zeile
                  div(style = "display: flex; align-items: center; gap: 6px;",
                    checkboxInput("poly_timescale_on", NULL, value = FALSE),
                    tags$b(tr("ca.poly.timescale.show"), style = "font-size: 0.88em;"),
                    tooltip_info(tr("ca.poly.timescale.show.tooltip"))
                  ),

                  # Datum-Eingaben + Optionen (nur wenn aktiv)
                  conditionalPanel(
                    condition = "input.poly_timescale_on == true",
                    hr(style = "margin: 6px 0;"),
                    fluidRow(
                      column(3,
                        numericInput("poly_ts_date_left",
                          tr("ca.poly.timescale.left"),
                          value = 3950, step = 1, width = "100%")
                      ),
                      column(3,
                        numericInput("poly_ts_date_right",
                          tr("ca.poly.timescale.right"),
                          value = 455, step = 1, width = "100%")
                      ),
                      # Einheit: nur sichtbar wenn NICHT BC/AD-Format
                      column(2,
                        conditionalPanel(
                          condition = "input.poly_ts_bcad == false",
                          textInput("poly_ts_unit",
                            tr("ca.poly.timescale.unit"),
                            value = "BP", width = "100%")
                        ),
                        conditionalPanel(
                          condition = "input.poly_ts_bcad == true",
                          div(style = "padding-top: 24px; text-align: center;",
                            tags$small(tags$b("BC / AD"), style = "color: #888;")
                          )
                        )
                      ),
                      # BC/AD-Toggle + Flip-Button
                      column(4,
                        div(style = "display: flex; flex-direction: column; gap: 4px; padding-top: 4px;",
                          div(style = "display: flex; align-items: center; gap: 4px;",
                            checkboxInput("poly_ts_bcad", NULL, value = FALSE),
                            div(
                              tags$small(tags$b(tr("ca.poly.timescale.bcad"))),
                              tooltip_info(tr("ca.poly.timescale.bcad.tooltip"))
                            )
                          ),
                          actionButton("poly_ts_flip",
                            tr("ca.poly.timescale.flip"),
                            class = "btn btn-default btn-sm",
                            style = "width: 100%;")
                        )
                      )
                    ),
                    # Hinweise
                    conditionalPanel(
                      condition = "input.poly_ts_bcad == false",
                      div(class = "alert alert-warning",
                          style = "font-size: 0.78em; padding: 4px 8px; margin-top: 6px; margin-bottom: 2px;",
                        tags$span(class = "glyphicon glyphicon-transfer",
                                  style = "margin-right: 4px;"),
                        tr("ca.poly.timescale.hint")
                      )
                    ),
                    conditionalPanel(
                      condition = "input.poly_ts_bcad == true",
                      div(class = "alert alert-info",
                          style = "font-size: 0.78em; padding: 4px 8px; margin-top: 6px; margin-bottom: 2px;",
                        tags$span(class = "glyphicon glyphicon-info-sign",
                                  style = "margin-right: 4px;"),
                        tr("ca.poly.timescale.bcad.hint")
                      )
                    ),
                    # Edge-Scaling-Warnung
                    div(class = "alert alert-danger",
                        style = "font-size: 0.76em; padding: 4px 8px; margin-top: 2px; margin-bottom: 0;",
                      tags$span(class = "glyphicon glyphicon-alert",
                                style = "margin-right: 4px;"),
                      tr("ca.poly.timescale.edge.warning")
                    ),

                    # ── ¹⁴C-Ankerpunkte ────────────────────────────────────
                    hr(style = "margin: 8px 0;"),
                    div(style = "display: flex; align-items: center; gap: 6px;",
                      checkboxInput("poly_ts_anchors_on", NULL, value = FALSE),
                      tags$b(tr("ca.poly.ts.anchors.on"), style = "font-size: 0.88em;"),
                      tooltip_info(tr("ca.poly.ts.anchors.tooltip"))
                    ),
                    conditionalPanel(
                      condition = "input.poly_ts_anchors_on == true",
                      div(style = "margin-top: 6px; margin-bottom: 2px;",
                        tags$small(tags$b(tr("ca.poly.ts.anchor.source"),
                                          style = "color: #555;")),
                        radioButtons("poly_ts_anchor_source", NULL,
                          choices = c(
                            " Unmodelliert (IntCal20)"  = "unmodelled",
                            " Modelliert (OxCal-Seq.)"  = "modelled"
                          ),
                          selected = "unmodelled",
                          inline   = FALSE
                        )
                      ),
                      uiOutput("poly_anchor_info_ui")
                    )
                  )
                ),
                # ────────────────────────────────────────────────────────────

                plotlyOutput("poly_timeline", height = "380px")
              ),

              # Sub-Tab 3: Rangwechsel (Slope Graph)
              tabPanel(tr("poly.tab.ranking"), value = "ranking",
                br(),
                div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
                  h5(tr("ca.poly.bump.title"), style = "margin: 0;"),
                  tooltip_info(tr("ca.poly.bump.tooltip"))
                ),
                plotlyOutput("poly_bump_chart", height = "560px")
              ),

              # Sub-Tab 4: Rang-Tabelle
              tabPanel(tr("poly.tab.table"), value = "table",
                br(),
                div(style = "display: flex; align-items: center; gap: 8px; margin-bottom: 8px;",
                  h5(tr("ca.poly.table.title"), style = "margin: 0;"),
                  tooltip_info(tr("ca.poly.table.tooltip"))
                ),
                DT::dataTableOutput("poly_order_table")
              )
            )
          )
        )
      )
    )
  }
})

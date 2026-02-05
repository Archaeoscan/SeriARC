# ui_typemap_content.R - Type Mapping Tab UI Content

# TYPEMAP MAIN CONTENT
output$typemap_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_typemap_ui(tr = tr)
  } else {
    tagList(
      fluidRow(
        # ===================== LEFT SIDEBAR =====================
        column(4,
          div(class = "seriarc-panel",
            h4(tr("typemap.title"), class = "mt-0"),

            # Coordinate Status (always visible at top)
            div(class = "seriarc-coord-status", style = "margin-bottom: 10px; padding: 8px; background: #e8f4fd; border-radius: 4px; border-left: 3px solid #2196f3;",
              tags$small(
                tags$strong(tr("typemap.status")), ": ",
                textOutput("typemap_coord_status_text", inline = TRUE)
              )
            ),

            # ==================== PANEL 1: TYPE SELECTION ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#typemap-panel-selection",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("typemap.panel.selection")),
                  span(class = "chevron-icon rotate-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "typemap-panel-selection", class = "panel-collapse collapse in",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Type Selection UI (rendered from server)
                  uiOutput("type_selection_ui")
                )
              )
            ),

            # ==================== PANEL 2: BASEMAP ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#typemap-panel-basemap",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("typemap.panel.basemap")),
                  span(class = "chevron-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "typemap-panel-basemap", class = "panel-collapse collapse",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Basemap Selection
                  h5(tr("typemap.basemap")),
                  radioButtons("type_map_basemap", NULL,
                    choices = setNames(
                      c("OpenTopoMap_Color", "OpenTopoMap_Gray", "MapsForFree_Color", "MapsForFree_Gray"),
                      c(tr("typemap.basemap.topo.color"), tr("typemap.basemap.topo.gray"),
                        tr("typemap.basemap.free.color"), tr("typemap.basemap.free.gray"))
                    ),
                    selected = "MapsForFree_Gray"
                  ),
                  # Water Overlay
                  checkboxInput("type_map_show_water", tr("typemap.water.overlay"), TRUE)
                )
              )
            ),

            # ==================== PANEL 3: DISPLAY ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#typemap-panel-display",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("typemap.panel.display")),
                  span(class = "chevron-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "typemap-panel-display", class = "panel-collapse collapse",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Standardization with info icon
                  div(style = "display: flex; align-items: center; gap: 6px;",
                    h5(tr("typemap.standardization"), style = "margin: 0;"),
                    tags$span(
                      class = "info-icon",
                      style = "cursor: help; color: #5bc0de; font-size: 14px;",
                      title = tr("typemap.std.info"),
                      HTML("&#9432;")  # Unicode info circle
                    )
                  ),
                  selectInput("type_map_standardization", NULL,
                    choices = setNames(
                      c("none", "percent_site", "percent_type"),
                      c(tr("typemap.std.none"), tr("typemap.std.percent.site"), tr("typemap.std.percent.type"))
                    ),
                    selected = "none"
                  ),
                  # Display Mode
                  h5(tr("typemap.display")),
                  radioButtons("type_map_display_mode", NULL,
                    choices = setNames(c("abundance", "presence"),
                      c(tr("typemap.display.abundance"), tr("typemap.display.presence"))),
                    selected = "presence"
                  ),
                  # Null Display
                  radioButtons("type_map_null_display", tr("typemap.null.display"),
                    choices = setNames(c("hide", "show_gray"),
                      c(tr("typemap.null.hide"), tr("typemap.null.gray"))),
                    selected = "hide"
                  ),
                  # Marker Style
                  h5(tr("typemap.marker.style")),
                  radioButtons("type_map_marker_style", NULL,
                    choices = setNames(c("pins", "circles"),
                      c(tr("typemap.marker.pins"), tr("typemap.marker.circles"))),
                    selected = "pins"
                  )
                )
              )
            ),

            # ==================== PANEL 4: MARKER OPTIONS ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#typemap-panel-marker",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("typemap.panel.marker")),
                  span(class = "chevron-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "typemap-panel-marker", class = "panel-collapse collapse",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Size Scaling
                  h5(tr("typemap.size.title")),
                  radioButtons("type_map_size_method", NULL,
                    choices = setNames(c("linear", "log"),
                      c(tr("typemap.size.linear"), tr("typemap.size.log"))),
                    selected = "linear"
                  ),
                  sliderInput("type_map_marker_scale", tr("typemap.scale"),
                    min = 0.5, max = 3.0, value = 1.0, step = 0.1),
                  # Overlap Handling
                  h5(tr("typemap.overlap")),
                  radioButtons("type_map_overlap_handling", NULL,
                    choices = setNames(c("distribute", "stack"),
                      c(tr("typemap.overlap.distribute"), tr("typemap.overlap.stack"))),
                    selected = "distribute"
                  ),
                  conditionalPanel(
                    condition = "input.type_map_overlap_handling == 'distribute'",
                    sliderInput("type_map_distribution_radius", tr("typemap.distribution.radius"),
                      min = 0.001, max = 0.01, value = 0.003, step = 0.0005),
                    sliderInput("type_map_coordinate_tolerance", tr("typemap.tolerance"),
                      min = 0.002, max = 0.05, value = 0.01, step = 0.001)
                  ),
                  # Legend and Auto-Zoom
                  checkboxInput("type_map_show_legend", tr("typemap.show.legend"), TRUE),
                  checkboxInput("type_map_auto_zoom", tr("typemap.auto.zoom"), TRUE),
                  # UTM Zone (conditional)
                  conditionalPanel(
                    condition = "output.utm_detected == true",
                    h5(tr("typemap.utm.zone")),
                    fluidRow(
                      column(6, numericInput("type_utm_zone", tr("typemap.utm.zone.label"),
                        min = 1, max = 60, value = 32, step = 1)),
                      column(6, selectInput("type_utm_hemi", tr("typemap.utm.hemisphere"),
                        choices = setNames(c("N", "S"),
                          c(tr("typemap.utm.north"), tr("typemap.utm.south"))),
                        selected = "N"))
                    )
                  )
                )
              )
            )

          ) # end seriarc-panel
        ), # end column(4)

        # ===================== RIGHT SIDE: MAP + EXPORT =====================
        column(8,
          # Map
          div(class = "seriarc-panel",
            leaflet::leafletOutput("type_map", height = "700px")
          ),
          br(),
          # Export Buttons
          div(class = "seriarc-panel",
            h5(tr("typemap.export"), class = "mt-0"),
            fluidRow(
              column(4, downloadButton("download_type_map_html", "🌐 HTML", class = "btn btn-info", style = "width: 100%;")),
              column(4, downloadButton("download_type_map_png", "🖼️ PNG", class = "btn btn-success", style = "width: 100%;")),
              column(4, downloadButton("download_type_map_svg", "📄 SVG", class = "btn btn-warning", style = "width: 100%;"))
            )
          ),
          br(),
          # Help Text
          div(class = "seriarc-help-text", tr("typemap.help"))
        ) # end column(8)
      ), # end fluidRow
      br(),
      # Type Statistics Table
      div(class = "seriarc-panel",
        h4(tr("typemap.stats"), class = "mt-0"),
        DT::dataTableOutput("type_statistics_table")
      )
    )
  }
})

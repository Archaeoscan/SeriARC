# ui_mapping_content.R - Mapping Tab UI Content

# MAPPING MAIN CONTENT
output$mapping_main_content <- renderUI({
  file_loaded <- !is.null(data_raw()) && nrow(data_raw()) > 0

  if (!file_loaded) {
    welcome_mapping_ui(tr = tr)
  } else {
    tagList(
      fluidRow(
        # ===================== LEFT SIDEBAR =====================
        column(4,
          div(class = "seriarc-panel",
            h4(tr("mapping.settings"), class = "mt-0"),

            # Coordinate Status (always visible at top)
            div(class = "seriarc-coord-status", style = "margin-bottom: 10px; padding: 8px; background: #e8f4fd; border-radius: 4px; border-left: 3px solid #2196f3;",
              tags$small(
                tags$strong(tr("mapping.coord.status")), ": ",
                textOutput("coord_status_text", inline = TRUE)
              )
            ),

            # ==================== PANEL 1: BASEMAP ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#mapping-panel-basemap",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("mapping.panel.basemap")),
                  span(class = "chevron-icon rotate-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "mapping-panel-basemap", class = "panel-collapse collapse in",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Basemap Selection
                  h5(tr("mapping.basemap")),
                  radioButtons("map_basemap_selection", NULL,
                    choices = setNames(
                      c("OpenTopoMap_Color", "OpenTopoMap_Gray", "MapsForFree_Color", "MapsForFree_Gray"),
                      c(tr("mapping.basemap.topo.color"), tr("mapping.basemap.topo.gray"),
                        tr("mapping.basemap.free.color"), tr("mapping.basemap.free.gray"))
                    ),
                    selected = "OpenTopoMap_Color"
                  ),
                  # Water Overlay
                  checkboxInput("map_show_water_overlay", tr("mapping.water.overlay"), FALSE)
                )
              )
            ),

            # ==================== PANEL 2: GROUP/CLUSTER ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#mapping-panel-groupcluster",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("mapping.panel.groupcluster")),
                  span(class = "chevron-icon rotate-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "mapping-panel-groupcluster", class = "panel-collapse collapse in",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Color By Selection
                  radioButtons("map_color_by", tr("mapping.color.by"),
                    choices = setNames(c("group", "cluster"), c(tr("mapping.color.group"), tr("mapping.color.cluster"))),
                    selected = "group"
                  ),
                  # Cluster Selection (conditional)
                  conditionalPanel(
                    condition = "input.map_color_by == 'cluster' && output.kmeans_available == true",
                    h5(tr("mapping.cluster.selection")),
                    radioButtons("map_cluster_display_mode", tr("mapping.display.mode"),
                      choices = setNames(c("hide", "gray"), c(tr("mapping.display.hide"), tr("mapping.display.gray"))),
                      selected = "hide", inline = TRUE
                    ),
                    uiOutput("map_cluster_checkboxes"),
                    fluidRow(
                      column(4, actionButton("map_select_all_clusters", tr("mapping.btn.all"),
                        class = "btn btn-success btn-sm", style = "width: 100%;")),
                      column(4, actionButton("map_select_no_clusters", tr("mapping.btn.none"),
                        class = "btn btn-warning btn-sm", style = "width: 100%;")),
                      column(4, actionButton("map_invert_clusters", tr("mapping.btn.invert"),
                        class = "btn btn-info btn-sm", style = "width: 100%;"))
                    ),
                    textOutput("map_cluster_info")
                  ),
                  # Group Selection (conditional)
                  conditionalPanel(
                    condition = "input.map_color_by == 'group'",
                    h5(tr("mapping.group.selection")),
                    radioButtons("map_group_display_mode", tr("mapping.display.mode"),
                      choices = setNames(c("hide", "gray"), c(tr("mapping.display.hide"), tr("mapping.display.gray"))),
                      selected = "hide", inline = TRUE
                    ),
                    uiOutput("map_group_checkboxes"),
                    fluidRow(
                      column(4, actionButton("map_select_all_groups", tr("mapping.btn.all"),
                        class = "btn btn-success btn-sm", style = "width: 100%;")),
                      column(4, actionButton("map_select_no_groups", tr("mapping.btn.none"),
                        class = "btn btn-warning btn-sm", style = "width: 100%;")),
                      column(4, actionButton("map_invert_groups", tr("mapping.btn.invert"),
                        class = "btn btn-info btn-sm", style = "width: 100%;"))
                    ),
                    textOutput("map_group_info")
                  )
                )
              )
            ),

            # ==================== PANEL 3: DISPLAY OPTIONS ====================
            div(class = "panel panel-default",
              div(class = "panel-heading",
                style = "padding: 8px 12px; cursor: pointer;",
                `data-toggle` = "collapse",
                `data-target` = "#mapping-panel-display",
                div(class = "panel-title",
                  style = "display: flex; justify-content: space-between; align-items: center;",
                  span(tr("mapping.panel.display")),
                  span(class = "chevron-icon", style = "transition: transform 0.2s;", HTML("&#9660;"))
                )
              ),
              div(id = "mapping-panel-display", class = "panel-collapse collapse",
                div(class = "panel-body", style = "padding: 10px 12px;",
                  # Labels
                  h5(tr("mapping.labels")),
                  radioButtons("map_label_mode", NULL,
                    choices = setNames(c("permanent", "none"), c(tr("mapping.labels.permanent"), tr("mapping.labels.none"))),
                    selected = "none"
                  ),
                  conditionalPanel(
                    condition = "input.map_label_mode == 'permanent'",
                    sliderInput("map_label_chars", tr("mapping.label.chars"), min = 3, max = 30, value = 12, step = 1),
                    sliderInput("map_label_size", tr("mapping.label.size"), min = 8, max = 24, value = 12, step = 1)
                  ),
                  # Point Size
                  sliderInput("map_point_size", tr("mapping.point.size"), min = 3, max = 20, value = 8, step = 1),
                  # Legend and Auto-Zoom
                  checkboxInput("map_show_legend", tr("mapping.show.legend"), TRUE),
                  checkboxInput("map_auto_zoom", tr("mapping.auto.zoom"), TRUE),
                  # UTM Zone (conditional)
                  conditionalPanel(
                    condition = "output.utm_detected == true",
                    div(class = "seriarc-panel-warning",
                      h5(tr("mapping.utm.zone")),
                      fluidRow(
                        column(6, numericInput("utm_zone", tr("utm.zone.label"), min = 1, max = 60, value = 32, step = 1)),
                        column(6, selectInput("utm_hemi", tr("utm.hemisphere.label"),
                          choices = setNames(c("N", "S"), c(tr("import.utm.north"), tr("import.utm.south"))),
                          selected = "N"))
                      )
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
            leaflet::leafletOutput("map", height = "650px")
          ),
          br(),
          # Export Buttons
          div(class = "seriarc-panel",
            h5(tr("mapping.export"), class = "mt-0"),
            fluidRow(
              column(6, downloadButton("download_map_png", "🖼️ PNG", class = "btn btn-success", style = "width: 100%;")),
              column(6, downloadButton("download_map_html", "🌐 HTML", class = "btn btn-info", style = "width: 100%;"))
            )
          ),
          br(),
          # Help Text
          div(class = "seriarc-help-text", tr("mapping.help"))
        ) # end column(8)
      ) # end fluidRow
    )
  }
})

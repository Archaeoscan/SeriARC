# modules/mod_c14.R - SeriARC v1.0.0 14C Integration
# Import, name matching, exclusion checkboxes and saving

# === UI COMPONENT ===
mod_c14_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)

  tagList(
    fluidRow(
      # === SIDEBAR: DATA & SETTINGS ===
      column(4,
             div(class = "seriarc-panel seriarc-panel-primary",
                 h4(tr("c14.panel.title"), class = "mt-0"),

                 # C14-Datenimport
                 fileInput(ns("c14_file"), tr("c14.import.label"),
                           accept = c(".xlsx", ".xls"),
                           placeholder = tr("c14.import.placeholder")
                 ),

                 downloadButton(ns("download_c14_template"), tr("c14.btn.template"),
                                class = "btn-outline-primary btn-sm"),

                 br(), br(),

                 # Status
                 conditionalPanel(
                   condition = sprintf("output['%s'] == true", ns("c14_data_loaded")),
                   div(class = "alert alert-success",
                       tr("c14.status.loaded")
                   )
                 ),

                 # Filter-Status
                 conditionalPanel(
                   condition = sprintf("output['%s'] == true", ns("c14_data_loaded")),
                   hr(),
                   h5(tr("c14.filter.title")),
                   uiOutput(ns("c14_filter_status"))
                 )
             )
      ),

      # === HAUPTBEREICH: INTEGRATION ===
      column(8,
             div(class = "seriarc-panel",
                 tabsetPanel(
                   id = ns("main_tabs"),

                   # === TAB: 14C-INTEGRATION ===
                   tabPanel(tr("c14.tab.integration"),

                            # Name-Mapping Panel
                            uiOutput(ns("name_mapping_panel")),

                            br(),

                            # 14C data table with checkboxes
                            conditionalPanel(
                              condition = sprintf("output['%s'] == true", ns("c14_data_loaded")),
                              div(class = "seriarc-panel",
                                  h5(tr("c14.table.title")),
                                  p(tr("c14.table.desc"),
                                    style = "color: #666; font-size: 0.9em;"),
                                  div(class = "alert alert-info", style = "padding: 8px; margin: 10px 0;",
                                      HTML(paste0("ℹ️ <strong>", tr("c14.table.note.title"), ":</strong> ", tr("c14.table.note.text")))
                                  ),
                                  rHandsontableOutput(ns("c14_selection_table"), height = "400px"),
                                  br(),
                                  downloadButton(ns("download_c14_data"), tr("c14.btn.download.table"),
                                                class = "btn btn-primary btn-sm")
                              )
                            ),

                            br(),

                            # Save section
                            conditionalPanel(
                              condition = sprintf("output['%s'] == true", ns("mapping_applied")),
                              div(class = "seriarc-panel seriarc-panel-success",
                                  h5(tr("c14.integration.complete.title")),
                                  p(tr("c14.integration.complete.desc")),
                                  downloadButton(ns("download_matched_data"), tr("c14.btn.download.integrated"),
                                                class = "btn btn-success")
                              )
                            )
                   )
                 )
             )
      )
    )
  )
}

# === SERVER COMPONENT ===
mod_c14_server <- function(id, filtered_data, meta_data, rv = NULL, phases_tbl = NULL, curve_df = NULL, tr = function(x) x) {

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # === REACTIVE VALUES ===
    c14_data <- reactiveVal(NULL)
    c14_meta_data <- reactiveVal(NULL)
    table_refresh_trigger <- reactiveVal(0)
    name_mapping_applied <- reactiveVal(FALSE)
    last_imported_file <- reactiveVal(NULL)

    # === HELPER FUNCTIONS ===
    .notify_error <- function(prefix, e, duration = 10) {
      msg <- tryCatch(conditionMessage(e), error = function(e) "Unbekannter Fehler")
      if (is.null(msg) || !nzchar(msg)) msg <- "Unbekannter Fehler"
      showNotification(paste0(prefix, " ", msg), type = "error", duration = duration)
    }

    # Robust conversion of various inputs to logical
    .to_logical_robust <- function(col, default_val, n_rows) {
      if (is.null(col) || length(col) == 0) return(rep(default_val, n_rows))
      col_char <- as.character(col)
      result <- rep(default_val, length(col_char))
      result[tolower(col_char) %in% c("true", "wahr", "1", "yes", "ja")] <- TRUE
      result[tolower(col_char) %in% c("false", "falsch", "0", "no", "nein")] <- FALSE
      return(result)
    }

    # Initialize meta data from imported C14 data
    .init_c14_meta <- function(c14_df, preserve_existing = FALSE) {
      n_rows <- nrow(c14_df)

      # Build columns with proper NA handling
      material_col <- if ("Material" %in% names(c14_df) && !is.null(c14_df$Material)) {
        mat <- as.character(c14_df$Material)
        mat[is.na(mat) | mat == ""] <- "Unknown"
        mat
      } else rep("Unknown", n_rows)

      context_col <- if ("Context" %in% names(c14_df) && !is.null(c14_df$Context)) {
        ctx <- as.character(c14_df$Context)
        ctx[is.na(ctx)] <- ""
        ctx
      } else rep("", n_rows)

      # Handle Selected/Outlier columns
      selected_col <- if ("Selected" %in% names(c14_df) && length(c14_df$Selected) == n_rows) {
        sel <- if (is.logical(c14_df$Selected)) c14_df$Selected else as.logical(c14_df$Selected)
        sel[is.na(sel)] <- TRUE
        sel
      } else rep(TRUE, n_rows)

      outlier_col <- if ("Outlier" %in% names(c14_df) && length(c14_df$Outlier) == n_rows) {
        out <- if (is.logical(c14_df$Outlier)) c14_df$Outlier else as.logical(c14_df$Outlier)
        out[is.na(out)] <- FALSE
        out
      } else rep(FALSE, n_rows)

      new_meta <- data.frame(
        LabNr = c14_df$LabNr,
        Fundstelle = c14_df$Fundstelle,
        Age = c14_df$Age,
        Error = c14_df$Error,
        Material = material_col,
        Context = context_col,
        Selected = selected_col,
        Outlier = outlier_col,
        stringsAsFactors = FALSE
      )

      # Preserve existing Selected/Outlier settings if re-importing
      if (preserve_existing && !is.null(c14_meta_data()) && nrow(c14_meta_data()) > 0) {
        old_meta <- c14_meta_data()
        for (i in seq_len(nrow(new_meta))) {
          match_idx <- which(old_meta$LabNr == new_meta$LabNr[i])
          if (length(match_idx) > 0) {
            new_meta$Selected[i] <- old_meta$Selected[match_idx[1]]
            new_meta$Outlier[i] <- old_meta$Outlier[match_idx[1]]
          }
        }
      }

      c14_meta_data(new_meta)
    }

    # Find best matching CA site for a C14 site name
    .find_best_match <- function(c14_site, ca_sites) {
      if (c14_site %in% ca_sites) return(c14_site)

      c14_normalized <- trimws(gsub("^[0-9]+\\s*", "", c14_site))

      for (ca_site in ca_sites) {
        ca_normalized <- trimws(gsub("^[0-9]+\\s*", "", ca_site))
        if (tolower(c14_normalized) == tolower(ca_normalized)) return(ca_site)
        if (nchar(c14_normalized) > 3 && nchar(ca_normalized) > 3) {
          # Use tolower for case-insensitive matching (fixed=TRUE ignores ignore.case)
          if (grepl(tolower(c14_normalized), tolower(ca_site), fixed = TRUE) ||
              grepl(tolower(ca_normalized), tolower(c14_site), fixed = TRUE)) {
            return(ca_site)
          }
        }
      }
      return(NA)
    }

    # === C14 DATA IMPORT ===
    observeEvent(input$c14_file, {
      req(input$c14_file)

      # Prevent duplicate imports
      current_file <- input$c14_file$datapath
      if (!is.null(last_imported_file()) && identical(current_file, last_imported_file())) return()
      last_imported_file(current_file)

      tryCatch({
        c14_df <- as.data.frame(readxl::read_excel(current_file))

        # Check required columns
        required_cols <- c("LabNr", "Fundstelle", "Age", "Error")
        missing_cols <- setdiff(required_cols, names(c14_df))
        if (length(missing_cols) > 0) {
          stop(paste("Fehlende Spalten:", paste(missing_cols, collapse = ", ")))
        }

        # Add optional columns if missing
        if (!"Material" %in% names(c14_df)) c14_df$Material <- "Unknown"
        if (!"Context" %in% names(c14_df)) c14_df$Context <- ""

        c14_df$Age <- as.numeric(c14_df$Age)
        c14_df$Error <- as.numeric(c14_df$Error)

        # Handle Selected/Outlier columns
        if ("Selected" %in% names(c14_df)) {
          c14_df$Selected <- .to_logical_robust(c14_df$Selected, TRUE, nrow(c14_df))
        }
        if ("Outlier" %in% names(c14_df)) {
          c14_df$Outlier <- .to_logical_robust(c14_df$Outlier, FALSE, nrow(c14_df))
        }

        has_existing <- !is.null(c14_meta_data()) && nrow(c14_meta_data()) > 0
        c14_data(c14_df)
        .init_c14_meta(c14_df, preserve_existing = has_existing)

        # Check mapping status
        if (!is.null(filtered_data())) {
          ca_sites <- rownames(filtered_data())
          c14_sites <- unique(c14_df$Fundstelle)
          name_mapping_applied(length(setdiff(c14_sites, ca_sites)) == 0)
        } else {
          name_mapping_applied(FALSE)
        }

        showNotification(tr("c14.notify.loaded"), type = "message", id = "c14_import_notify")

      }, error = function(e) {
        .notify_error("Import error:", e)
      })
    })

    # === NAME MAPPING PANEL ===
    output$name_mapping_panel <- renderUI({
      c14_df <- tryCatch(c14_data(), error = function(e) NULL)
      filtered_df <- tryCatch(filtered_data(), error = function(e) NULL)

      if (is.null(c14_df) || is.null(filtered_df)) {
        return(div(class = "alert alert-info", tr("c14.mapping.info")))
      }

      tryCatch({
        ca_sites <- rownames(filtered_df)
        if (is.null(ca_sites) || length(ca_sites) == 0) {
          return(div(class = "alert alert-info", tr("c14.mapping.info")))
        }

        c14_sites <- unique(c14_df$Fundstelle)
        missing_in_ca <- setdiff(c14_sites, ca_sites)

        if (length(missing_in_ca) > 0) {
          # Find suggestions for each missing site
          suggested_matches <- vapply(missing_in_ca, function(site) {
            result <- .find_best_match(site, ca_sites)
            if (is.null(result) || length(result) == 0) NA_character_ else as.character(result)
          }, FUN.VALUE = character(1), USE.NAMES = TRUE)

          div(class = "seriarc-panel seriarc-panel-warning",
              h5(tr("c14.mapping.title")),
              p(tr("c14.mapping.desc")),

              lapply(seq_along(missing_in_ca), function(i) {
                c14_site <- missing_in_ca[i]
                suggested <- if (i <= length(suggested_matches)) suggested_matches[i] else NA_character_
                selected_val <- if (!is.na(suggested) && nchar(suggested) > 0) suggested else ""

                fluidRow(
                  column(5,
                         div(style = "padding-top: 8px;",
                             strong(c14_site),
                             if (!is.na(suggested) && nchar(suggested) > 0) {
                               tags$small(class = "text-success", style = "display: block;",
                                         paste0("→ ", tr("c14.mapping.suggestion")))
                             }
                         )
                  ),
                  column(1, div(style = "padding-top: 8px; text-align: center;", "→")),
                  column(6,
                         selectInput(ns(paste0("mapping_", i)), NULL,
                                     choices = c(setNames("", tr("c14.mapping.noassign")), ca_sites),
                                     selected = selected_val)
                  )
                )
              }),

              br(),
              actionButton(ns("apply_name_mapping"), tr("c14.btn.apply.mapping"),
                          class = "btn-warning btn-sm")
          )
        } else {
          div(class = "seriarc-panel seriarc-panel-success",
              h5(tr("c14.mapping.compatible.title")),
              p(tr("c14.mapping.compatible.desc"))
          )
        }
      }, error = function(e) {
        div(class = "alert alert-warning", tr("c14.mapping.error"))
      })
    })

    # === APPLY NAME MAPPING ===
    observeEvent(input$apply_name_mapping, {
      req(c14_data(), filtered_data())

      tryCatch({
        c14_df <- c14_data()
        ca_sites <- rownames(filtered_data())
        c14_sites <- unique(c14_df$Fundstelle)
        missing_in_ca <- setdiff(c14_sites, ca_sites)

        if (length(missing_in_ca) == 0) {
          name_mapping_applied(TRUE)
          showNotification(tr("notify.c14.mapping.none.needed"), type = "message")
          return()
        }

        # Apply mappings
        mappings_applied <- 0
        for (i in seq_along(missing_in_ca)) {
          mapping_input <- input[[paste0("mapping_", i)]]
          if (!is.null(mapping_input) && nzchar(mapping_input)) {
            old_name <- missing_in_ca[i]
            c14_df$Fundstelle[c14_df$Fundstelle == old_name] <- mapping_input
            mappings_applied <- mappings_applied + 1
          }
        }

        c14_data(c14_df)

        # Update meta data with mappings
        if (!is.null(c14_meta_data()) && nrow(c14_meta_data()) > 0) {
          updated_meta <- c14_meta_data()
          for (i in seq_along(missing_in_ca)) {
            mapping_input <- input[[paste0("mapping_", i)]]
            if (!is.null(mapping_input) && nzchar(mapping_input)) {
              old_name <- missing_in_ca[i]
              updated_meta$Fundstelle[updated_meta$Fundstelle == old_name] <- mapping_input
            }
          }
          c14_meta_data(updated_meta)
        }

        table_refresh_trigger(isolate(table_refresh_trigger()) + 1)
        name_mapping_applied(TRUE)

        showNotification(sprintf(tr("notify.c14.mapping.applied"), mappings_applied), type = "message")

      }, error = function(e) {
        .notify_error("Mapping error:", e)
      })
    })

    # Auto-set mapping_applied if all sites are compatible
    observe({
      tryCatch({
        c14_df <- c14_data()
        filtered_df <- filtered_data()

        if (!is.null(c14_df) && !is.null(filtered_df) && nrow(c14_df) > 0 && nrow(filtered_df) > 0) {
          ca_sites <- rownames(filtered_df)
          c14_sites <- unique(c14_df$Fundstelle)

          if (length(ca_sites) > 0 && length(c14_sites) > 0 && length(setdiff(c14_sites, ca_sites)) == 0) {
            name_mapping_applied(TRUE)
          }
        }
      }, error = function(e) NULL)
    })

    # === OUTPUT STATUS ===
    output$c14_data_loaded <- reactive({ !is.null(c14_data()) })
    outputOptions(output, "c14_data_loaded", suspendWhenHidden = FALSE)

    output$mapping_applied <- reactive({ isTRUE(name_mapping_applied()) })
    outputOptions(output, "mapping_applied", suspendWhenHidden = FALSE)

    # === C14 SELECTION TABLE ===
    output$c14_selection_table <- renderRHandsontable({
      trigger_val <- table_refresh_trigger()
      meta_data <- c14_meta_data()
      req(meta_data, nrow(meta_data) > 0)
      req(all(c("LabNr", "Fundstelle", "Age", "Error", "Selected", "Outlier") %in% names(meta_data)))

      # Prepare display data
      selected_vals <- meta_data$Selected
      selected_vals[is.na(selected_vals)] <- TRUE
      outlier_vals <- meta_data$Outlier
      outlier_vals[is.na(outlier_vals)] <- FALSE

      display_df <- data.frame(
        LabNr = as.character(meta_data$LabNr),
        Fundstelle = as.character(meta_data$Fundstelle),
        Age_BP = paste0(meta_data$Age, " ± ", meta_data$Error),
        Material = as.character(ifelse(is.na(meta_data$Material), "Unknown", meta_data$Material)),
        Context = as.character(ifelse(is.na(meta_data$Context), "", meta_data$Context)),
        Selected = selected_vals,
        Outlier = outlier_vals,
        stringsAsFactors = FALSE
      )

      tryCatch({
        rht <- rhandsontable(display_df,
                             width = "100%", height = 380,
                             rowHeaders = FALSE,
                             colHeaders = c(tr("term.labnr"), tr("term.fundstelle"), tr("term.age.bp"),
                                           tr("term.material"), tr("term.context"), tr("term.selected"), tr("term.outlier")),
                             stretchH = "all", copyPaste = TRUE)

        rht <- hot_context_menu(rht,
          allowRowEdit = FALSE, allowColEdit = FALSE,
          customOpts = list(
            copy = list(name = "Copy", callback = htmlwidgets::JS(
              "function(key, selection, clickEvent) { this.copyPaste.copy(); }"
            ))
          )
        )

        rht <- hot_cols(rht, colWidths = c(120, 150, 120, 100, 120, 80, 80))

        # Set column types using indices
        rht <- hot_col(rht, 1, readOnly = TRUE)
        rht <- hot_col(rht, 2, readOnly = TRUE)
        rht <- hot_col(rht, 3, readOnly = TRUE)
        rht <- hot_col(rht, 4, readOnly = TRUE)
        rht <- hot_col(rht, 5, readOnly = TRUE)
        rht <- hot_col(rht, 6, type = "checkbox")
        rht <- hot_col(rht, 7, type = "checkbox")

        rht
      }, error = function(e) {
        rhandsontable(display_df, width = "100%", height = 380)
      })
    })

    # === TABLE OBSERVER ===
    observeEvent(input$c14_selection_table, {
      req(input$c14_selection_table, c14_meta_data())

      tryCatch({
        new_data <- hot_to_r(input$c14_selection_table)
        current_meta <- c14_meta_data()

        if (nrow(new_data) == nrow(current_meta)) {
          changes_made <- FALSE

          for (i in 1:nrow(new_data)) {
            if (!is.na(new_data$Selected[i]) && !identical(current_meta$Selected[i], new_data$Selected[i])) {
              current_meta$Selected[i] <- new_data$Selected[i]
              changes_made <- TRUE
            }
            if (!is.na(new_data$Outlier[i]) && !identical(current_meta$Outlier[i], new_data$Outlier[i])) {
              current_meta$Outlier[i] <- new_data$Outlier[i]
              changes_made <- TRUE
            }
          }

          if (changes_made) c14_meta_data(current_meta)
        }
      }, error = function(e) NULL)
    }, ignoreInit = TRUE)

    # === FILTER STATUS ===
    output$c14_filter_status <- renderUI({
      meta <- c14_meta_data()
      req(meta, nrow(meta) > 0)

      sel <- meta$Selected
      sel[is.na(sel)] <- TRUE
      out <- meta$Outlier
      out[is.na(out)] <- FALSE

      div(class = "alert alert-info", style = "padding: 8px; margin: 5px 0;",
          HTML(sprintf(tr("c14.filter.selected"), sum(sel), nrow(meta))),
          HTML(sprintf(tr("c14.filter.outlier"), sum(sel & out))),
          HTML(sprintf(tr("c14.filter.available"), sum(sel)))
      )
    })

    # === DOWNLOAD HANDLERS ===
    output$download_c14_template <- downloadHandler(
      filename = "c14_template.xlsx",
      content = function(file) {
        template <- data.frame(
          LabNr = c("Beta-1234", "Beta-1235", "AA-6789"),
          Fundstelle = c("Site001", "Site001", "Site002"),
          Age = c(2450, 2380, 2200),
          Error = c(30, 35, 40),
          Material = c("Charcoal", "Bone", "Charcoal"),
          Context = c("Hearth", "Pit", "Posthole")
        )
        writexl::write_xlsx(template, file)
      }
    )

    output$download_c14_data <- downloadHandler(
      filename = function() paste0("c14_data_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(c14_meta_data())
        writexl::write_xlsx(c14_meta_data(), file)
      }
    )

    output$download_matched_data <- downloadHandler(
      filename = function() paste0("c14_integrated_", Sys.Date(), ".xlsx"),
      content = function(file) {
        req(c14_data())

        c14_df <- c14_data()

        if (!is.null(filtered_data())) {
          ca_sites <- rownames(filtered_data())
          c14_df$In_CA_Matrix <- c14_df$Fundstelle %in% ca_sites
        }

        summary_data <- data.frame(
          Info = c("Anzahl 14C-Datierungen", "Anzahl Sites", "Integrierte Sites", "Export-Zeitpunkt"),
          Wert = c(
            nrow(c14_df),
            length(unique(c14_df$Fundstelle)),
            sum(c14_df$In_CA_Matrix %||% rep(TRUE, length(unique(c14_df$Fundstelle)))),
            as.character(Sys.time())
          )
        )

        writexl::write_xlsx(list("C14_Data" = c14_df, "Summary" = summary_data), file)
      }
    )

    # === FILTERED C14 DATA (for downstream modules) ===
    filtered_c14_data <- reactive({
      meta <- c14_meta_data()
      req(meta, nrow(meta) > 0)

      selected_mask <- meta$Selected
      selected_mask[is.na(selected_mask)] <- TRUE

      active_data <- meta[selected_mask, , drop = FALSE]
      if (is.null(active_data) || nrow(active_data) == 0) return(NULL)

      data.frame(
        LabNr = active_data$LabNr,
        Site = active_data$Fundstelle,
        Age = active_data$Age,
        Error = active_data$Error,
        Material = active_data$Material,
        Context = active_data$Context,
        Outlier = active_data$Outlier,
        stringsAsFactors = FALSE
      )
    })

    # === RETURN VALUES ===
    return(list(
      c14_data = filtered_c14_data,
      c14_raw_data = c14_data,
      c14_meta_data = reactive({ c14_meta_data() }),
      mapping_applied = name_mapping_applied
    ))
  })
}

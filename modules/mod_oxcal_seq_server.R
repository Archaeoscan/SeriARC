# mod_oxcal_seq_server.R
# Server logic for OxCal Sequence module (ArchaeoPhases removed)

# Load phase analysis functions
source("modules/mod_oxcal_seq_phases.R", local = TRUE)

# ---------- kleine Utils ----------
`%||%` <- function(x, y) {
  if (is.null(x)) return(y)
  if (length(x) == 0) return(y)
  if (is.logical(x) && length(x) == 1 && is.na(x)) return(y)
  x
}

.ensure_oxcal <- function() {
  # Try to get existing OxCal path, or setup if not found
  # Note: Main initialization logic is now in .initialize_oxcal()
  tryCatch({
    p <- tryCatch(oxcAAR::getOxcalExecutablePath(), error = function(e) NULL)
    if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
    # Try quick setup
    oxcAAR::quickSetupOxcal()
    p <- tryCatch(oxcAAR::getOxcalExecutablePath(), error = function(e) NULL)
    if (!is.null(p) && nzchar(p) && file.exists(p)) return(p)
    NULL
  }, error = function(e) {
    NULL
  })
}

# ===================== HAUPTSERVER =====================
mod_oxcal_seq_server <- function(id, c14_table_reactive, chrono_curve_reactive,
                                   cache = NULL, mapping = NULL, rv = NULL,
                                   cluster_names = NULL, cluster_colors = NULL,
                                   tr = function(x) x) {
  moduleServer(id, function(input, output, session){
    ns <- session$ns
    
    # Verhindere automatische Rplots.pdf-Erstellung
    pdf(NULL)
    
    # ===================== OXCAL SETUP =====================
    oxcal_setup <- .initialize_oxcal()
    oxcal_ready_val <- reactiveVal(oxcal_setup$ready)
    
    # OxCal Setup Event Handlers
    .setup_oxcal_handlers(input, output, oxcal_ready_val, ns)
    
    oxcal_ready <- reactive({ oxcal_ready_val() })
    
    # ===================== WORKFLOW WARNING =====================
    # Check if Chronology Curve is available
    output$workflow_warning <- renderUI({
      chrono <- tryCatch(chrono_curve_reactive(), error = function(e) NULL)
      
      # Check: Missing or invalid?
      is_invalid <- is.null(chrono) || 
                    !is.data.frame(chrono) || 
                    nrow(chrono) == 0 || 
                    !all(c("site_id", "t_idx") %in% names(chrono))
      
      if (is_invalid) {
        div(class = "alert alert-danger", style = "margin: 20px 0;",
            h4(paste0("⚠️ ", tr("oxcal.seq.nocurve.title")), class = "mt-0"),
            p(tr("oxcal.seq.nocurve.intro")),
            tags$ol(
              tags$li(strong(tr("oxcal.seq.nocurve.step1a")), " ", tr("oxcal.seq.nocurve.step1b")),
              tags$li(tr("oxcal.seq.nocurve.step2"), code("t_idx"), ")"),
              tags$li(tr("oxcal.seq.nocurve.step3"))
            ),
            p(class = "text-danger",
              strong(paste0("❌ ", tr("oxcal.seq.nocurve.warn"))),
              " ", tr("oxcal.seq.nocurve.warn.text"))
        )
      } else {
        NULL
      }
    })
    
    # Status output for conditionalPanel
    output$data_available <- reactive({
      chrono <- tryCatch(chrono_curve_reactive(), error = function(e) NULL)
      !is.null(chrono) && 
        is.data.frame(chrono) && 
        nrow(chrono) > 0 && 
        all(c("site_id", "t_idx") %in% names(chrono))
    })
    outputOptions(output, "data_available", suspendWhenHidden = FALSE)
    
    # ===================== PHASEN-MODUS INFO =====================
    output$phase_mode_info <- renderUI({
      mode <- input$phase_mode
      
      if (mode == "clusters") {
        # Check if clusters exist
        has_clusters <- !is.null(cache) && !is.null(cache$kmeans_result) && 
                        !is.null(cache$kmeans_result$data) &&
                        "cluster" %in% names(cache$kmeans_result$data)
        
        if (!has_clusters) {
          div(class="alert alert-warning", style="padding:8px; margin:8px 0; font-size:0.85em;",
              HTML(paste0("⚠️ <strong>", tr("oxcal.phase.clusters.missing"), "</strong>")))
        } else {
          n_clusters <- length(unique(cache$kmeans_result$data$cluster))
          div(class="alert alert-success", style="padding:8px; margin:8px 0; font-size:0.85em;",
              HTML(sprintf("✅ <strong>%d %s</strong> - %s", n_clusters, tr("oxcal.phase.clusters.available"), tr("oxcal.phase.clusters.desc"))))
        }
      } else if (mode == "groups") {
        # Check if groups exist
        mapping_data <- if (is.function(mapping)) mapping() else mapping

        # mapping_meta is a data.frame with 'group' column
        has_groups <- !is.null(mapping_data) && is.data.frame(mapping_data) &&
                      "group" %in% names(mapping_data) &&
                      any(!is.na(mapping_data$group))

        if (!has_groups) {
          div(class="alert alert-warning", style="padding:8px; margin:8px 0; font-size:0.85em;",
              HTML(paste0("⚠️ <strong>", tr("oxcal.phase.groups.missing"), "</strong>")))
        } else {
          groups <- unique(mapping_data$group[!is.na(mapping_data$group)])
          div(class="alert alert-success", style="padding:8px; margin:8px 0; font-size:0.85em;",
              HTML(sprintf("✅ <strong>%d %s:</strong> %s",
                          length(groups), tr("oxcal.phase.groups.available"), paste(head(groups, 5), collapse=", "))))
        }
      } else {
        # Sites-Modus (Standard)
        div(class="alert alert-info", style="padding:8px; margin:8px 0; font-size:0.85em;",
            HTML(paste0("ℹ️ <strong>", tr("oxcal.phase.sites.info"), "</strong>")))
      }
    })
    
    # ===================== UI-REAKTIVE FUNKTIONEN =====================
    base_data <- reactive({
      get_oxcal_base_data(c14_table_reactive, chrono_curve_reactive)
    })
    
    filtered_data <- reactive({
      df <- base_data()
      from_input <- input$from
      to_input   <- input$to
      get_oxcal_filtered_data(df, from_input, to_input)
    })
    
    # ===================== OUTLIER MODE EXPLANATION =====================
    output$outlier_mode_explanation <- renderUI({
      mode <- input$outlier_mode
      if (is.null(mode) || mode == "none") return(NULL)
      
      explanation <- switch(mode,
        "general" = tagList(
          HTML("<strong>🎯 General Model:</strong><br>"),
          HTML("• <strong>What:</strong> OxCal applies global <code>Outlier_Model()</code> to ALL data<br>"),
          HTML("• <strong>OxCal decides:</strong> Which date is likely an outlier<br>"),
          HTML("• <strong>Good for:</strong> General robustness without manual marking<br>"),
          HTML("<br><strong>CQL example:</strong><br><code>Outlier_Model(\"General\", T(5), U(0,4), \"t\");<br>Sequence() { R_Date(...); }</code>")
        ),
        "individual" = tagList(
          HTML("<strong>🎯 Individual Model:</strong><br>"),
          HTML("• <strong>What:</strong> Only manually marked data (Outlier column = TRUE) get <code>Outlier(p)</code><br>"),
          HTML("• <strong>You decide:</strong> Which data are suspicious<br>"),
          HTML("• <strong>Good for:</strong> Targeted treatment of known problem data<br>"),
          HTML("<br><strong>CQL example:</strong><br><code>R_Date(\"Site1\", 3500, 30) { Outlier(0.05); };<br>R_Date(\"Site2\", 3200, 25); // normal</code>")
        ),
        "combined" = tagList(
          HTML("<strong>🎯 Combined Model:</strong><br>"),
          HTML("• <strong>What:</strong> General Model for all + increased prior for marked data<br>"),
          HTML("• <strong>Double protection:</strong> General (all) + Individual (marked with 2× prior)<br>"),
          HTML("• <strong>Good for:</strong> Maximum robustness for known problem cases<br>"),
          HTML("<br><strong>CQL example:</strong><br><code>Outlier_Model(\"General\", T(5), U(0,4), \"t\");<br>Sequence() {<br>  R_Date(\"Site1\", ...) { Outlier(0.10); }; // marked: 2×<br>  R_Date(\"Site2\", ...); // has General protection<br>}</code>")
        ),
        NULL
      )
      
      explanation
    })
    
    # Range selection UI
    output$interval_from_to <- renderUI({
      boundaries_enabled <- input$autoBoundaries
      df <- base_data()
      choices_info <- generate_interval_choices(df, boundaries_enabled, tr)
      
      if (is.null(df) || nrow(df) == 0) {
        return(tagList(
          div(class="alert alert-warning", choices_info$help_text),
          selectInput(ns("from"), "Von", choices = choices_info$choices_list),
          selectInput(ns("to"), "Bis", choices = choices_info$choices_list)
        ))
      }
      
      tagList(
        div(class="text-muted", style="font-size: 0.85em; margin-bottom: 8px;", choices_info$help_text),
        selectInput(ns("from"), "Bereich: Von", choices = choices_info$choices_list, selected = choices_info$default_from),
        selectInput(ns("to"),   "… Bis",       choices = choices_info$choices_list, selected = choices_info$default_to)
      )
    })
    
    # ===================== CQL-GENERIERUNG =====================
    build_cql <- reactive({
      df <- filtered_data()
      if (is.null(df) || nrow(df) == 0) {
        return("# No data available or invalid range")
      }
      
      # Evaluiere reactive Parameter
      mapping_data <- if (is.function(mapping)) mapping() else mapping
      
      settings <- list(
        autoBoundaries = input$autoBoundaries,
        boundary_strategy = input$boundary_strategy %||% "shared",
        addSpanPhase = input$addSpanPhase,
        addSpanSeq = input$addSpanSeq,
        
        # PHASEN-MODUS
        phase_mode = input$phase_mode %||% "sites",  # "sites", "clusters", "groups"
        
        # OUTLIER-PARAMETER
        outlier_mode = input$outlier_mode %||% "none",  # "none", "general", "individual", "combined"
        outlier_prior = input$outlier_prior %||% 5,  # Prozent (wird in data.R zu dezimal)
        
        # HOLZKOHLE-KORREKTUR
        charcoal_correction = input$charcoal_correction %||% FALSE,
        charcoal_offset = input$charcoal_offset %||% 40,
        
        # REST
        enhanced_cql = input$enhanced_cql %||% FALSE,
        from = input$from,
        to = input$to
      )
      
      build_oxcal_cql(df, settings, cache, mapping_data, cluster_names)
    })
    
    # ===================== OXCAL EXECUTION =====================
    res <- eventReactive(input$run, {
      .execute_oxcal_model(build_cql())
    }, ignoreInit = TRUE)
    
    # ===================== PHASEN-ANALYSE =====================
    # Direct from JS output (no MCMC CSV required)
    phases_parsed <- reactive({
      r <- res()
      req(r, r$tidy)
      
      tidy_df <- r$tidy
      
      # Identify phases from boundary lines
      phases_info <- .recognize_phases_from_tidy(tidy_df)
      
      if (length(phases_info) == 0) return(NULL)

      list(
        tidy = tidy_df,
        phases = names(phases_info),
        boundaries = phases_info
      )
    })
    
    phase_stats <- reactive({
      m <- phases_parsed()
      req(m)
      .calculate_phase_stats_from_tidy(m$tidy, m$boundaries)
    })
    
    phase_overlaps <- reactive({
      m <- phases_parsed()
      req(m)
      .calculate_overlaps_from_tidy(m$tidy, m$boundaries)
    })

    phase_gaps <- reactive({
      m <- phases_parsed()
      req(m)
      .calculate_gaps_from_tidy(m$tidy, m$boundaries)
    })

    # Phasen-Analyse Outputs
    output$tbl_phases <- DT::renderDataTable({
      df <- phase_stats()
      if (is.null(df)) {
        return(data.frame(Info = "No phase data available"))
      }

      # Interpolation status based on N_Dates (no longer on CI width!)
      df$Interpoliert <- ifelse(
        df$Is_Interpolated,
        "⚠️ Ja (keine 14C-Daten)",
        "Nein"
      )

      # === CONVERSION: BP → BC (absolute, without sign) ===
      # BP values are negative (e.g. -4500), BC are positive (4500 BC)
      bp_cols <- c("Start_Median", "Start_68CI_Lower", "Start_68CI_Upper",
                   "Start_95CI_Lower", "Start_95CI_Upper",
                   "End_Median", "End_68CI_Lower", "End_68CI_Upper",
                   "End_95CI_Lower", "End_95CI_Upper")
      for (col in bp_cols) {
        if (col %in% names(df)) {
          df[[col]] <- abs(df[[col]])
        }
      }

      # Duration stays as years (positive)
      duration_cols <- c("Duration_Median", "Duration_95CI_Lower", "Duration_95CI_Upper")
      for (col in duration_cols) {
        if (col %in% names(df)) {
          df[[col]] <- abs(df[[col]])
        }
      }

      # Remove internal column, keep N_Dates for display
      df$Is_Interpolated <- NULL

      # Sort columns: Phase, N_Dates, Interpolated, then rest
      cols <- c("Phase", "N_Dates", "Interpoliert", setdiff(names(df), c("Phase", "N_Dates", "Interpoliert")))
      df <- df[, cols]

      # Round numeric columns (except N_Dates, which is already integer)
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- lapply(df[num_cols], round, 0)

      # Column names for display (cal BC instead of cal BP!)
      col_names <- c(
        "Phase",
        "Anzahl 14C",
        "Interpoliert",
        "Start Median (cal BC)",
        "Start 68% CI Min",
        "Start 68% CI Max",
        "Start 95% CI Min",
        "Start 95% CI Max",
        "Ende Median (cal BC)",
        "Ende 68% CI Min",
        "Ende 68% CI Max",
        "Ende 95% CI Min",
        "Ende 95% CI Max",
        "Dauer Median (Jahre)",
        "Dauer 95% CI Min",
        "Dauer 95% CI Max"
      )

      DT::datatable(df,
        colnames = col_names,
        options = list(pageLength = 10, scrollX = TRUE, dom = 'frtip'),
        rownames = FALSE
      ) %>%
        DT::formatStyle(
          'Interpoliert',
          target = 'row',
          backgroundColor = DT::styleEqual(
            c("⚠️ Ja (keine 14C-Daten)", "Nein"),
            c('#fff3cd', 'white')  # Yellow for interpolated phases
          )
        )
    })
    
    output$tbl_overlaps <- DT::renderDataTable({
      df <- phase_overlaps()
      if (is.null(df)) {
        return(data.frame(Info = "No overlap data available"))
      }
      # Round numeric columns to 0 decimal places
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- lapply(df[num_cols], round, 0)

      # Column names
      col_names <- c(
        "Phase 1",
        "Phase 2",
        "Overlap?",
        "Overlap Duration (years)"
      )

      DT::datatable(df, colnames = col_names, options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'frtip'
      ), rownames = FALSE)
    })

    output$tbl_gaps <- DT::renderDataTable({
      df <- phase_gaps()
      if (is.null(df)) {
        return(data.frame(Info = "No gap data available"))
      }
      num_cols <- sapply(df, is.numeric)
      df[num_cols] <- lapply(df[num_cols], round, 0)

      # Column names
      col_names <- c(
        "After Phase",
        "Before Phase",
        "Gap Median (years)",
        "Gap 95% CI Min",
        "Gap 95% CI Max",
        "Interpretation"
      )
      
      DT::datatable(df, colnames = col_names, options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "400px",
        dom = 'frtip'
      ), rownames = FALSE)
    })

    output$plt_phases <- renderPlotly({
      df <- phase_stats()
      req(df)
      # Use ci_level from left panel
      ci_mode <- input$ci_level %||% "both"
      show_68 <- ci_mode %in% c("68", "both")
      show_95 <- ci_mode %in% c("95", "both")
      .plot_phase_timeline(df, show_68ci = show_68, show_95ci = show_95)
    })
    
    # ===================== UI OUTPUTS =====================
    output$agreements_summary <- renderUI({
      r <- res()
      if (is.null(r) || is.null(r$global_A)) return(NULL)
      render_agreements_summary(r$global_A, tr)
    })
    
    output$tbl_summary <- DT::renderDataTable({
      r <- res()
      if (is.null(r) || !is.data.frame(r$tidy)) {
        return(data.frame(Info = tr("term.no.data")))
      }
      show_like <- isTRUE(input$show_likelihood)
      render_summary_table(r$tidy, r$like_intervals, show_like, tr)
    }, options = list(
      fixedHeader = TRUE,
      scrollX = TRUE,
      scrollY = "400px",
      paging = FALSE,
      dom = 'frti'
    ), rownames = FALSE)
    
    output$plt_posterior <- renderPlotly({
      r <- res()
      if (is.null(r) || !is.data.frame(r$tidy)) {
        return(plot_ly() %>% layout(title = tr("plot.oxcal.no.data")))
      }
      settings <- list(
        ci_level = input$ci_level,
        plot_type = "intervals",  # Fixed to intervals (density option removed)
        show_likelihood = input$show_likelihood
      )
      render_posterior_plot(r$tidy, r$densities, r$like_densities, r$like_intervals, settings, tr)
    })
    
    output$plt_calibration_curve <- renderPlotly({
      r <- res()
      if (is.null(r)) {
        return(plot_ly() %>% layout(title = tr("plot.oxcal.run.first")))
      }
      render_calibration_curve_plot(r, tr)
    })
    
    # ===================== DOWNLOADS =====================
    .setup_download_handlers(output, res, ns, build_cql)

    # ===================== PHASEN-PLOT EXPORTS =====================
    .setup_phases_plot_exports(output, phase_stats, input, ns)

    # ===================== STATUS OUTPUTS =====================
    .setup_status_outputs(output, oxcal_ready, oxcal_setup, ns, tr)
    
    # ===================== CQL PREVIEW =====================
    output$log <- renderText({
      .generate_cql_preview(filtered_data(), input, build_cql())
    })

    # ===================== RETURN MODULE INTERFACE =====================
    # Expose results for other modules (e.g., 14C overlay)
    return(list(
      # Get OxCal results (tidy data frame with posteriors)
      get_results = res,

      # Check if results are available
      has_results = reactive({
        r <- tryCatch(res(), error = function(e) NULL)
        !is.null(r) && !is.null(r$tidy) && nrow(r$tidy) > 0
      }),

      # Get tidy data frame directly
      get_tidy = reactive({
        r <- tryCatch(res(), error = function(e) NULL)
        if (!is.null(r) && !is.null(r$tidy)) r$tidy else NULL
      })
    ))
  })
}

# ===================== OXCAL SETUP FUNKTIONEN =====================
.initialize_oxcal <- function() {
  # Cloud mode: OxCal cannot run on shinyapps.io
  if (exists("RUNNING_ON_CLOUD") && RUNNING_ON_CLOUD) {
    return(list(ready = FALSE, path = NULL, cloud = TRUE))
  }

  # First try to get existing OxCal path
  ox_path <- tryCatch({
    p <- oxcAAR::getOxcalExecutablePath()
    if (!is.null(p) && nzchar(p) && file.exists(p)) p else NULL
  }, error = function(e) NULL)

  # If no path found, try quickSetup
  if (is.null(ox_path)) {
    ox_path <- tryCatch({
      oxcAAR::quickSetupOxcal()
      p <- oxcAAR::getOxcalExecutablePath()
      if (!is.null(p) && nzchar(p) && file.exists(p)) p else NULL
    }, error = function(e) NULL)
  }

  if (is.null(ox_path)) {
    return(list(ready = FALSE, path = NULL))
  }

  # Path exists - try a quick test execution
  ok <- TRUE
  msg <- NULL
  test_cql <- 'R_Date("test", 5000, 25);'
  tryCatch({
    invisible(oxcAAR::executeOxcalScript(test_cql))
  }, error = function(e) {
    ok <<- FALSE
    msg <<- e$message
  })

  if (!ok) {
    # Test failed, but path exists - could be temporary issue
    # Don't show error notification, just report status
    message("OxCal test execution failed (path exists): ", msg)
    # Still mark as ready if path exists - user can try running manually
    return(list(ready = TRUE, path = ox_path, warning = msg))
  }

  list(ready = TRUE, path = ox_path)
}

.setup_oxcal_handlers <- function(input, output, oxcal_ready_val, ns) {
  # Cloud mode: no setup handlers needed
  if (exists("RUNNING_ON_CLOUD") && RUNNING_ON_CLOUD) return(invisible(NULL))

  observeEvent(input$set_path, {
    path <- input$oxcal_path_input
    if(!is.null(path) && nchar(path) > 0) {
      tryCatch({
        if(file.exists(path)) {
          oxcAAR::setOxcalExecutablePath(path)
          oxcal_ready_val(TRUE)
          showNotification(paste("✅ OxCal path successfully set:", basename(path)), type = "message")
        } else {
          showNotification(paste("⚠️ File not found:", path), type = "error")
        }
      }, error = function(e) {
        showNotification(paste("⚠️ Error setting path:", e$message), type = "error")
      })
    } else {
      showNotification("⚠️ Please enter a path", type = "warning")
    }
  })
  
  observeEvent(input$setup_oxcal, {
    withProgress(message = "Setting up OxCal...", value = 0.5, {
      tryCatch({
        oxcAAR::quickSetupOxcal()
        oxcal_ready_val(TRUE)
        showNotification("✅ OxCal successfully set up!", type = "message")
      }, error = function(e) {
        showNotification(paste("⚠️ OxCal setup failed:", e$message), type = "error")
      })
    })
  })
}

# ===================== OXCAL EXECUTION =====================
.execute_oxcal_model <- function(cql) {
  if (!nzchar(cql) || identical(cql, "# No data available or invalid range")) {
    showNotification("CQL cannot be created - check data", type="warning", duration=5)
    return(NULL)
  }
  
  withProgress(message = "OxCal running (MCMC)...", value = 0.1, {
    if (grepl("Outlier\\(", cql, perl = TRUE)) {
      incProgress(0.05, detail = "🔬 Outlier Model - 2–15 Min")
    } else {
      incProgress(0.05, detail = "⏩ Standard Sequenz-Modell")
    }
    js_path <- tryCatch({
      oxcAAR::executeOxcalScript(cql)
    }, error = function(e) {
      showNotification(paste("OxCal execution failed:", e$message), type="error", duration=10)
      return(NULL)
    })
    if (is.null(js_path) || !file.exists(js_path)) {
      showNotification("OxCal did not return a JS file.", type="error", duration=8)
      return(NULL)
    }
    incProgress(0.4)

    lines <- tryCatch({
      oxcAAR::readOxcalOutput(js_path)
    }, error = function(e) {
      showNotification(paste("JS read error:", e$message), type="error", duration=8)
      return(character(0))
    })
    if (length(lines) == 0) {
      showNotification("Empty OxCal output.", type="error", duration=8)
      return(NULL)
    }
    incProgress(0.3)
    
    result <- .process_oxcal_output(cql, js_path, lines)
    result
  })
}

.process_oxcal_output <- function(cql, js_path, lines) {
  # Globale Agreements
  global_A <- .parse_global_agreements(lines)

  # Convergence check
  zero_convergence <- any(grepl("\\bConvergence\\s*$", lines)) && any(grepl("\\s0\\.0\\s*$", lines))
  if (zero_convergence) {
    showNotification("OxCal: MCMC Convergence 0%. Model too complex or contradictory.", type="error", duration=10)
    minimal_tidy <- .parse_oxcal_js_fallback(lines)
    return(list(cql=cql, js_path=js_path, tidy=minimal_tidy, converged=FALSE, global_A=global_A))
  }

  # Hybrid-Parser
  parsed_full <- tryCatch({ oxcAAR::parseFullOxcalOutput(lines) }, error = function(e) NULL)

  if (!is.null(parsed_full)) {
    tidy <- .extract_posterior_df_hull(parsed_full, lines)
  } else {
    tidy <- .parse_oxcal_js_fallback(lines)
  }

  simple_parsed <- tryCatch({ oxcAAR::parseOxcalOutput(lines) }, error = function(e) NULL)
  
  tidy <- .merge_model_proportional(tidy, lines)
  densities        <- .extract_posterior_densities(lines, tidy$name)
  like_densities   <- .extract_likelihood_densities(lines, tidy$name)
  like_intervals   <- .extract_likelihood_intervals(lines, tidy$name)
  tidy             <- .map_ci_names(tidy)
  
  .report_extraction_success(tidy, global_A)
  
  list(
    cql = cql,
    js_path = js_path,
    tidy = tidy,
    converged = TRUE,
    global_A = global_A,
    densities = densities,
    like_densities = like_densities,
    like_intervals = like_intervals,
    simple_parsed = simple_parsed
  )
}

.report_extraction_success <- function(tidy, global_A) {
  has_data <- NROW(tidy) > 0
  has_intervals <- has_data && any(!is.na(tidy$from_68) | !is.na(tidy$from_95))
  has_outlier <- has_data && "p_outlier" %in% names(tidy) && any(!is.na(tidy$p_outlier))
  has_agreement <- has_data && "A" %in% names(tidy) && any(!is.na(tidy$A))
  
  if (!has_data) {
    showNotification("No posterior data extracted. Check OxCal syntax.", type="warning", duration=8)
  } else if (!has_intervals) {
    showNotification("Data found, but no CI intervals.", type="warning", duration=6)
  } else {
    msg_parts <- c("✅ Posterior intervals successfully extracted!")
    if (has_outlier) {
      outlier_high  <- sum(!is.na(tidy$p_outlier) & tidy$p_outlier > 0.5, na.rm = TRUE)
      outlier_total <- sum(!is.na(tidy$p_outlier))
      if (outlier_high > 0) msg_parts <- c(msg_parts, sprintf("🔴 %d/%d Outliers (p>0.5)", outlier_high, outlier_total))
    }
    if (has_agreement) {
      low_agreement  <- sum(!is.na(tidy$A) & tidy$A < 60, na.rm = TRUE)
      agreement_total<- sum(!is.na(tidy$A))
      if (low_agreement > 0) msg_parts <- c(msg_parts, sprintf("⚠️ %d/%d low Agreement (<60%%)", low_agreement, agreement_total))
    }
    showNotification(paste(msg_parts, collapse=" "), type="message", duration=5)
  }
}

# ===================== DOWNLOAD HANDLERS =====================
.setup_download_handlers <- function(output, res, ns, build_cql_reactive = NULL) {
  output$dl_cql <- downloadHandler(
    filename = function() "SeriArcSequence.oxcal",
    content  = function(file) {
      # Priority 1: CQL from OxCal result (if model has run)
      r <- tryCatch(res(), error = function(e) NULL)
      if (!is.null(r) && !is.null(r$cql)) {
        writeLines(r$cql, file)
        return()
      }

      # Priority 2: CQL from build_cql reactive (cloud mode, without OxCal execution)
      if (!is.null(build_cql_reactive)) {
        cql <- tryCatch(build_cql_reactive(), error = function(e) NULL)
        if (!is.null(cql) && nzchar(cql) && !grepl("^#", cql)) {
          writeLines(cql, file)
          return()
        }
      }

      writeLines("# No CQL available - please load data first", file)
    }
  )
  
  output$dl_js <- downloadHandler(
    filename = function() "SeriArcSequence.js",
    content  = function(file) {
      r <- res()
      if (is.null(r) || is.null(r$js_path) || !file.exists(r$js_path)) {
        writeLines("// No JS file available", file); return()
      }
      file.copy(r$js_path, file, overwrite = TRUE)
      # Keine automatische Kopie mehr ins Projektverzeichnis -
    }
  )
}

# ===================== STATUS OUTPUTS =====================
.setup_status_outputs <- function(output, oxcal_ready, oxcal_setup, ns, tr = function(x) x) {
  output$oxcal_status <- renderText({
    status <- oxcal_ready()
    if (status) {
      # Get current path (may have been updated after initial setup)
      current_path <- tryCatch({
        p <- oxcAAR::getOxcalExecutablePath()
        if (!is.null(p) && nzchar(p)) p else oxcal_setup$path
      }, error = function(e) oxcal_setup$path)

      warning_msg <- if (!is.null(oxcal_setup$warning)) {
        paste0("\n\n", tr("oxcal.status.warning.prefix"), oxcal_setup$warning)
      } else ""

      paste0(
        tr("oxcal.status.ready"),
        "\n\n", tr("oxcal.status.found"),
        "\n", tr("oxcal.status.path"), " ", if(!is.null(current_path)) current_path else tr("oxcal.status.unknown"),
        "\n", tr("oxcal.status.ready.detail"),
        warning_msg
      )
    } else {
      paste0(
        tr("oxcal.status.not.configured"),
        "\n\n", tr("oxcal.status.requires"),
        "\n\n", tr("oxcal.status.options"),
        "\n", tr("oxcal.status.option.manual"),
        "\n", tr("oxcal.status.option.auto"),
        "\n\n", tr("oxcal.status.console.hint")
      )
    }
  })

  output$oxcal_setup_ui <- renderUI({
    if(!oxcal_ready()) {
      tagList(
        br(),
        fluidRow(
          column(6, actionButton(ns("setup_oxcal"), tr("oxcal.btn.auto.setup"), class = "btn btn-warning btn-sm")),
          column(6, actionButton(ns("manual_path"), tr("oxcal.btn.set.path"), class = "btn btn-secondary btn-sm"))
        ),
        br(),
        conditionalPanel(
          condition = sprintf("input['%s'] %% 2 == 1", ns("manual_path")),
          div(class = "well well-sm",
              h6(tr("oxcal.manual.path.title")),
              fluidRow(
                column(9, textInput(ns("oxcal_path_input"), NULL,
                                    placeholder = tr("oxcal.manual.path.placeholder"), value = "")),
                column(3, actionButton(ns("set_path"), tr("oxcal.btn.set"), class = "btn btn-success btn-sm", style = "margin-top: 0px;"))
              )
          )
        )
      )
    } else {
      div(class = "alert alert-success", style = "font-size: 0.9em;",
          strong(tr("oxcal.status.ready.badge")), " ", tr("oxcal.status.ready.msg"))
    }
  })
}

# ===================== CQL PREVIEW =====================
.generate_cql_preview <- function(filtered_data, input, cql) {
  status_txt <- paste0(
    "# === AKTUELLE EINSTELLUNGEN ===\n",
    "# Boundaries automatisch: ", if(isTRUE(input$autoBoundaries)) "✅ JA" else "❌ NEIN", "\n",
    "# Span je Site (Phase): ", if(isTRUE(input$addSpanPhase)) "✅ JA" else "❌ NEIN", "\n",
    "# Span der Sequenz: ", if(isTRUE(input$addSpanSeq)) "✅ JA" else "❌ NEIN", "\n",
    "# Holzkohle-Korrektur: ", if(isTRUE(input$charcoal_correction))
      paste0("🌲 AKTIV (", input$charcoal_offset %||% 40, " Jahre)") else "❌ AUS", "\n",
    "# Outlier-Modell: ", input$outlier_mode %||% "none",
      if((input$outlier_mode %||% "none") != "none") paste0(" (", input$outlier_prior %||% 5, "%)") else "", "\n",
    "# Konfidenzintervall: ", input$ci_level %||% "both", "\n",
    "# Bereich Von: ", input$from %||% "---", "\n",
    "# Bereich Bis: ", input$to %||% "---", "\n\n"
  )
  
  ord_txt <- ""
  if (!is.null(filtered_data) && nrow(filtered_data) > 0) {
    phases_ordered <- unique(filtered_data[order(filtered_data$t_idx), "phase_pretty", drop=FALSE])$phase_pretty
    filter_info <- if (!is.null(input$from) && !is.null(input$to) && input$from != "---" && input$to != "---") {
      sprintf(" [filtered: %s → %s]", input$from, input$to)
    } else {
      " [all phases]"
    }
    ord_txt <- paste("# Order (oldest → youngest):", paste(phases_ordered, collapse=" → "), filter_info, "\n\n")
  }
  
  cql_txt <- tryCatch(cql, error = function(e) {
    paste("# CQL cannot be created yet:", e$message,
          "\n\n# Please ensure that:\n",
          "# 1. 14C data is imported\n",
          "# 2. Chronology Curve has been calculated\n",
          "# 3. At least one site matches")
  })
  
  paste0(status_txt, ord_txt, cql_txt)
}

# ===================== PHASE PLOT EXPORT HANDLERS =====================
.setup_phases_plot_exports <- function(output, phase_stats_reactive, input, ns) {

  # Helper function: Generate phase plot (reusable for all exports)
  generate_phases_plot <- function() {
    df <- tryCatch(phase_stats_reactive(), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) return(NULL)

    ci_mode <- input$ci_level %||% "both"
    show_68 <- ci_mode %in% c("68", "both")
    show_95 <- ci_mode %in% c("95", "both")

    .plot_phase_timeline(df, show_68ci = show_68, show_95ci = show_95)
  }

  # PNG Export
 output$dl_phases_png <- downloadHandler(
    filename = function() sprintf("SeriARC_Phasen_Zeitstrahl_%s.png", Sys.Date()),
    content = function(file) {
      showNotification("📸 Creating PNG Export...", type = "message", duration = 3)

      tryCatch({
        p <- generate_phases_plot()
        if (is.null(p)) {
          png(file, width = 800, height = 400, res = 150)
          plot.new()
          text(0.5, 0.5, "No phase data available.\nPlease calculate OxCal model with Boundaries first.",
               cex = 1.2, col = "gray50")
          dev.off()
          return()
        }

        # Use orca/kaleido if available, otherwise show hint
        if (requireNamespace("webshot2", quietly = TRUE) || requireNamespace("webshot", quietly = TRUE)) {
          tmp_html <- tempfile(fileext = ".html")
          htmlwidgets::saveWidget(p, tmp_html, selfcontained = TRUE)

          if (requireNamespace("webshot2", quietly = TRUE)) {
            webshot2::webshot(tmp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
          } else {
            webshot::webshot(tmp_html, file = file, vwidth = 1200, vheight = 800, zoom = 2)
          }
          unlink(tmp_html)
        } else {
          # Fallback without webshot: hint image
          png(file, width = 1200, height = 800, res = 150)
          plot.new()
          text(0.5, 0.7, "PNG export requires the 'webshot2' package", cex = 1.5, font = 2)
          text(0.5, 0.5, "Alternatives:", cex = 1.2)
          text(0.5, 0.4, "1. Export HTML and take a screenshot", cex = 1.1)
          text(0.5, 0.3, "2. Use the Plotly camera icon in the plot", cex = 1.1)
          text(0.5, 0.2, "3. Run install.packages('webshot2')", cex = 1.1, col = "blue")
          dev.off()
        }

        showNotification("✅ PNG Export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("⚠️ PNG Export Error:", e$message), type = "error", duration = 5)
      })
    }
  )

  # SVG Export
  output$dl_phases_svg <- downloadHandler(
    filename = function() sprintf("SeriARC_Phasen_Zeitstrahl_%s.svg", Sys.Date()),
    content = function(file) {
      showNotification("📄 Creating SVG Export...", type = "message", duration = 3)

      tryCatch({
        df <- tryCatch(phase_stats_reactive(), error = function(e) NULL)
        if (is.null(df) || nrow(df) == 0) {
          svg(file, width = 10, height = 6)
          plot.new()
          text(0.5, 0.5, "No phase data available", cex = 1.2, col = "gray50")
          dev.off()
          return()
        }

        # Create SVG with ggplot2 (better than Plotly for vector graphics)
        svg(file, width = 12, height = max(6, nrow(df) * 0.5))

        # Simple Base-R Plot as SVG
        par(mar = c(5, 10, 4, 2))

        df$Start_BC <- abs(df$Start_Median)
        df$End_BC <- abs(df$End_Median)

        # Sort by start (oldest on top)
        df <- df[order(-df$Start_BC), ]

        n <- nrow(df)
        y_pos <- n:1

        # Empty plot
        x_range <- range(c(df$Start_BC, df$End_BC), na.rm = TRUE)
        plot(NULL, xlim = rev(x_range), ylim = c(0.5, n + 0.5),
             xlab = "Calibrated Age (cal BC)", ylab = "",
             yaxt = "n", main = "Phase Timeline (OxCal)")

        # Y-axis with phase names
        axis(2, at = y_pos, labels = df$Phase, las = 1, cex.axis = 0.8)

        # Draw bars for each phase
        for (i in 1:n) {
          is_interp <- isTRUE(df$Is_Interpolated[i])
          col_bar <- if (is_interp) "gray80" else "steelblue"
          border_col <- if (is_interp) "gray60" else "darkblue"
          lty_val <- if (is_interp) 2 else 1

          # Bar from start to end
          rect(df$Start_BC[i], y_pos[i] - 0.3, df$End_BC[i], y_pos[i] + 0.3,
               col = col_bar, border = border_col, lty = lty_val, lwd = 1.5)

          # Start symbol (triangle)
          points(df$Start_BC[i], y_pos[i], pch = if(is_interp) 2 else 17, col = "darkgreen", cex = 1.2)

          # End symbol (square)
          points(df$End_BC[i], y_pos[i], pch = if(is_interp) 0 else 15, col = "darkred", cex = 1.2)
        }

        # Legend
        legend("topright",
               legend = c("Start (▶)", "End (■)", "Normal Phase", "Interpolated Phase"),
               pch = c(17, 15, NA, NA),
               fill = c(NA, NA, "steelblue", "gray80"),
               border = c(NA, NA, "darkblue", "gray60"),
               col = c("darkgreen", "darkred", NA, NA),
               cex = 0.8, bg = "white")

        dev.off()

        showNotification("✅ SVG Export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("⚠️ SVG Export Error:", e$message), type = "error", duration = 5)
      })
    }
  )

  # HTML Export (interactive)
  output$dl_phases_html <- downloadHandler(
    filename = function() sprintf("SeriARC_Phasen_Zeitstrahl_%s.html", Sys.Date()),
    content = function(file) {
      showNotification("🌐 Creating HTML Export...", type = "message", duration = 3)

      tryCatch({
        p <- generate_phases_plot()
        if (is.null(p)) {
          writeLines("<html><body><h1>No Phase Data</h1><p>Please calculate OxCal model with Boundaries first.</p></body></html>", file)
          return()
        }

        htmlwidgets::saveWidget(
          p,
          file = file,
          selfcontained = TRUE,
          title = paste("SeriARC Phase Timeline -", Sys.Date())
        )

        showNotification("✅ HTML Export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("⚠️ HTML Export Error:", e$message), type = "error", duration = 5)
      })
    }
  )
}

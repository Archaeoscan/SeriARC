# ============================= mod_c14_calibration.R =============================
# 14C calibration with local IntCal20.14c for SeriARC (robust version)
# - Uses local intcal20.14c file instead of external packages
# - Custom calibration implementation (Monte Carlo based)
# - Multi-peak HPD intervals (1σ/2σ) correctly handled
# - Row-by-row try/catch (no total aborts)
# - Unified cal BC/AD formatting, intervals older to younger
# - Table in input order; sorting via column click possible
# - Excel export with complete interval strings

# === UI COMPONENT ===
mod_c14_calibration_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  
  tagList(
    h4(
      div(
        tr("c14.calib.title"),
        tags$span("ℹ️", style="margin-left:10px; cursor:help;",
                  title=tr("c14.calib.tooltip"))
      ),
      class = "mt-0"
    ),
    
    tabsetPanel(
      id = ns("calib_tabs"),
      type = "tabs",
      
      # === TAB 1: BATCH CALIBRATION ===
      tabPanel(
        tr("c14.calib.tab.batch"),
        br(),
        div(class = "seriarc-panel seriarc-panel-primary",
            fluidRow(
              column(6,
                     div(
                       h5(tr("c14.calib.curve.title")),
                       div(class = "alert alert-info",
                           "ℹ️ ", strong(tr("c14.calib.curve.local")), " ", tr("c14.calib.curve.used"),
                           br(),
                           tr("c14.calib.curve.info"),
                           br(), br(),
                           tr("c14.calib.curve.standard"),
                           br(),
                           tags$small(class = "text-muted",
                             tr("c14.calib.curve.outlier.note"))
                       )
                     )
              ),
              column(6,
                     div(style = "margin-top: 25px;",
                         actionButton(
                           ns("calibrate_c14"),
                           tr("c14.calib.btn.calibrate"),
                           class = "btn-primary btn-lg",
                           style = "width: 100%;"
                         )
                     )
              )
            ),
            
            br(),
            
            conditionalPanel(
              condition = sprintf("output['%s'] == true", ns("calibration_running")),
              div(class = "alert alert-info",
                  tr("c14.calib.running"),
                  br(),
                  div(id = ns("progress_container"))
              )
            ),
            
            conditionalPanel(
              condition = sprintf("output['%s'] == true", ns("calibration_completed")),
              div(class = "alert alert-success",
                  tr("c14.calib.success"),
                  br(),
                  textOutput(ns("calibration_summary"))
              )
            )
        ),
        
        # === CALIBRATED DATA TABLE ===
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("calibration_completed")),
          br(),
          div(class = "seriarc-panel",
              h4(tr("c14.calib.results.title"), class = "mt-0"),
              
              # Table at full width
              DT::dataTableOutput(ns("calibrated_table")),

              br(), br(),

              # Export block below table, full width
              div(class = "seriarc-panel seriarc-panel-info",
                  h5(tr("c14.calib.export.title")),
                  p(tr("c14.calib.export.desc")),
                  downloadButton(
                    ns("download_calibrated_excel"),
                    tr("c14.calib.export.btn"),
                    class = "btn-success btn-lg"
                  ),
                  br(), br(),
                  h6(tr("c14.calib.export.hints.title")),
                  tags$ul(
                    tags$li(tr("c14.calib.export.hints.1")),
                    tags$li(tr("c14.calib.export.hints.2")),
                    tags$li(tr("c14.calib.export.hints.3")),
                    tags$li(tr("c14.calib.export.hints.4"))
                  )
              )
          )
        )
      ),
      
      # === TAB 2: SINGLE DATE CALIBRATION ===
      tabPanel(
        tr("c14.calib.tab.single"),
        br(),
        # Info-Box: Works independently
        div(class = "alert alert-info", style = "margin-bottom: 15px;",
            HTML(paste0("<i class='fa fa-info-circle'></i> <strong>",
                        tr("c14.calib.single.standalone.title"), "</strong> ",
                        tr("c14.calib.single.standalone.text")))
        ),
        div(class = "seriarc-panel seriarc-panel-primary",
            h5(tr("c14.calib.single.dataselect")),
            fluidRow(
              column(12,
                     selectInput(
                       ns("single_data_source"),
                       tr("c14.calib.single.select.label"),
                       choices = setNames("manual", tr("c14.calib.single.manual")),
                       width = "100%"
                     ),
                     tags$small(class = "text-muted",
                               tr("c14.calib.single.select.hint"))
              )
            ),
            br(),
            h5(tr("c14.calib.single.input")),
            fluidRow(
              column(4,
                     textInput(
                       ns("single_labid"),
                       tr("c14.calib.single.labid"),
                       value = "",
                       placeholder = tr("c14.calib.single.labid.placeholder")
                     )
              ),
              column(4,
                     numericInput(
                       ns("single_age"),
                       tr("c14.calib.single.age"),
                       value = NA,
                       min = 0,
                       max = 55000
                     )
              ),
              column(4,
                     numericInput(
                       ns("single_error"),
                       tr("c14.calib.single.error"),
                       value = NA,
                       min = 1,
                       max = 1000
                     )
              )
            ),
            br(),
            actionButton(
              ns("calibrate_single"),
              tr("c14.calib.btn.single"),
              class = "btn-primary btn-lg"
            )
        ),
        
        # RESULTS IN TABS
        conditionalPanel(
          condition = sprintf("output['%s'] == true", ns("single_calibrated")),
          br(),
          tabsetPanel(
            type = "tabs",

            # Tab 2.1: Calibration plot
            tabPanel(
              tr("c14.calib.single.tab.plot"),
              br(),
              # Plot in container with limited width, centered
              div(style = "max-width: 700px; margin: 0 auto;",
                  plotOutput(ns("single_plot"), height = "512px", width = "100%")
              ),
              br(),
              div(style = "text-align: center;",
                  downloadButton(
                    ns("dl_single_plot"),
                    tr("c14.calib.single.btn.save"),
                    class = "btn-success"
                  )
              )
            ),

            # Tab 2.2: Calibrated date (values)
            tabPanel(
              tr("c14.calib.single.tab.result"),
              br(),
              div(class = "seriarc-panel seriarc-panel-info",
                  htmlOutput(ns("single_result"))
              )
            )
          )
        )
      )
    )
  )
}

# === SERVER COMPONENT ===
mod_c14_calibration_server <- function(id, c14_raw_reactive, mapping_applied_reactive, tr = function(x) x) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # === REACTIVE VALUES ===
    calibrated_data <- reactiveVal(NULL)
    calibration_in_progress <- reactiveVal(FALSE)
    calibration_complete <- reactiveVal(FALSE)
    
    # Saves significant performance on repeated calibrations
    curve_interpolators_cache <- reactiveVal(NULL)

    # === HELPER: Use central functions from mod_c14_functions.R ===
    # (calbp_to_year_ce, format_year_label, format_interval_calbc, intervals_to_string_calbc)
    # (hpd_intervals_grid - new robust HPD calculation)

    # Wrapper for HPD calculation (uses central function)
    .calculate_hpd_intervals <- function(age_bp, dens, prob_level = 0.683) {
      # Use the new robust HPD function from mod_c14_functions.R
      result <- hpd_intervals_grid(t = age_bp, p = dens, level = prob_level)

      # Add mass column (if needed for compatibility)
      if (!is.null(result) && nrow(result) > 0 && !all(is.na(result$low_bp))) {
        result$mass <- sapply(1:nrow(result), function(i) {
          mask <- age_bp >= result$low_bp[i] & age_bp <= result$high_bp[i]
          if (!any(mask)) return(0)
          
          # Diskrete Integration
          dx <- median(diff(age_bp), na.rm = TRUE)
          if (!is.finite(dx) || dx <= 0) dx <- 1
          sum(dens[mask] * dx) / sum(dens * dx)
        })
      }
      
      return(result)
    }
    
    # Weighted Median from Grid (robust)
    .grid_weighted_median_bp <- function(age_bp, dens) {
      o <- order(age_bp); x <- age_bp[o]; f <- pmax(dens[o], 0)
      dx <- c(diff(x), stats::median(diff(x), na.rm = TRUE))
      w  <- f / sum(f * dx)
      cw <- cumsum(w * dx)
      x[which(cw >= 0.5)[1]]
    }

    # Normalize curve names (for documentation only, as we only use local IntCal20)
    .normalize_curve <- function(x) {
      x <- tolower(trimws(ifelse(is.null(x), "", x)))
      ok <- c("intcal20", "shcal20", "marine20")
      x[!(x %in% ok)] <- "intcal20"
      # Warning for non-IntCal20 curves
      if (any(x != "intcal20")) {
        showNotification(tr("notify.c14.intcal.warning"),
                         type = "warning", duration = 6)
      }
      rep("intcal20", length(x))  # Always use IntCal20
    }

    # === LOCAL CALIBRATION FUNCTION ===
    .calibrate_single_local <- function(c14_age, c14_error, curve_data, interpolators = NULL) {
      # Simple Monte-Carlo calibration with local curve
      # Standard calibration WITHOUT outlier model
      # (Outlier handling only in OxCal with stratigraphic constraints)

      # Validate inputs
      if (is.na(c14_age) || is.na(c14_error) || c14_error <= 0) {
        stop("Invalid 14C data: Age=", c14_age, ", Error=", c14_error)
      }

      if (is.null(curve_data) || nrow(curve_data) == 0) {
        stop("Calibration curve is empty or not available")
      }

      # Check if required columns exist
      required_cols <- c("CalBP", "C14Age", "Error")
      if (!all(required_cols %in% names(curve_data))) {
        stop("Calibration curve has missing columns: ",
             paste(setdiff(required_cols, names(curve_data)), collapse = ", "))
      }

      curve_range <- range(curve_data$CalBP, na.rm = TRUE)
      age_min <- max(0, curve_range[1] - 1000)
      age_max <- min(55000, curve_range[2] + 1000)
      
      # Focus grid range around expected calibration
      # Rough estimate: C14 age ± 3σ for rough range
      rough_range <- c14_age + c(-3 * c14_error, 3 * c14_error) * 10  # Factor 10 for cal BP uncertainty
      focus_min <- max(age_min, rough_range[1] - 1000)
      focus_max <- min(age_max, rough_range[2] + 1000)

      # ⚠️ EXTRAPOLATION CHECK: Warning for dates outside curve range
      if (focus_min < curve_range[1] || focus_max > curve_range[2]) {
        extrapolation_warning <- sprintf(
          "⚠️ Date (%d ± %d BP) is partially outside curve range (%d-%d cal BP). Interpret results with caution!",
          c14_age, c14_error, curve_range[1], curve_range[2]
        )
        warning(extrapolation_warning)

        # Clip grid to curve range (no extrapolation)
        focus_min <- max(focus_min, curve_range[1])
        focus_max <- min(focus_max, curve_range[2])
      }

      age_grid <- seq(focus_min, focus_max, by = 1)

      if (length(age_grid) == 0) {
        stop("Could not create valid age grid")
      }

      tryCatch({
        # ⚡ OPTIMIZATION: Use spline interpolators if available
        if (!is.null(interpolators)) {
          # Fast spline interpolation
          curve_c14 <- interpolators$mu(age_grid)
          curve_err <- interpolators$sig(age_grid)
        } else {
          # Fallback: Standard approx()
          curve_c14 <- approx(curve_data$CalBP, curve_data$C14Age, xout = age_grid, rule = 2)$y
          curve_err <- approx(curve_data$CalBP, curve_data$Error, xout = age_grid, rule = 2)$y
        }

        # Check interpolation results
        if (any(is.na(curve_c14)) || any(is.na(curve_err))) {
          warning("Interpolation of calibration curve resulted in NA values")
          # Remove NA values
          valid_idx <- !is.na(curve_c14) & !is.na(curve_err)
          if (sum(valid_idx) == 0) {
            stop("No valid interpolation values")
          }
          age_grid <- age_grid[valid_idx]
          curve_c14 <- curve_c14[valid_idx]
          curve_err <- curve_err[valid_idx]
        }

        # Combined uncertainty: sqrt(c14_error^2 + curve_error^2)
        total_error <- sqrt(c14_error^2 + curve_err^2)

        # ⚡ NUMERICAL STABILITY: Log-space likelihood + Log-Sum-Exp
        # Prevents underflow for very small probabilities
        loglik_core <- dnorm(c14_age, mean = curve_c14, sd = total_error, log = TRUE)

        # Log-Sum-Exp trick for stability
        m <- max(loglik_core[is.finite(loglik_core)])  # Maximum for stability

        if (!is.finite(m)) {
          # All log-likelihoods are -Inf or NA
          likelihood <- rep(1/length(age_grid), length(age_grid))
          warning("Log-likelihood was entirely -Inf, using uniform prior")
        } else {
          # Standard likelihood (no outlier mixture)
          likelihood <- exp(loglik_core - m)

          # Normalization
          s <- sum(likelihood)

          if (s <= 0 || !is.finite(s)) {
            # Fallback: uniform prior
            likelihood <- rep(1/length(age_grid), length(age_grid))
            warning("Normalization failed, using uniform prior")
          } else {
            # Successfully normalized
            likelihood <- likelihood / s
          }
        }

        return(list(
          age_grid = age_grid,
          density = likelihood
        ))

      }, error = function(e) {
        stop("Calibration error: ", conditionMessage(e))
      })
    }

    # === VALIDATION ===
    .validate_c14_data <- function(c14_df) {
      # Robust column detection (like in rank-prior module)
      colmap <- list(
        LabNr = c("LabNr", "Lab", "LabID", "ID", "Probennummer"),
        Site = c("Site", "Fundstelle", "site", "Locality", "Location"),
        Age = c("Age", "C14Age", "Age_BP", "AgeBP", "Alter", "Datum"),
        Error = c("Error", "C14Error", "Sigma", "Std", "sd", "Fehler", "StdDev")
      )

      # Normalize column names
      for (target_name in names(colmap)) {
        for (alt in colmap[[target_name]]) {
          if (alt %in% names(c14_df)) {
            names(c14_df)[names(c14_df) == alt] <- target_name
            break
          }
        }
      }

      req_cols <- c("LabNr", "Site", "Age", "Error")
      if (!all(req_cols %in% colnames(c14_df))) {
        missing_cols <- setdiff(req_cols, colnames(c14_df))
        available_cols <- names(c14_df)
        stop(sprintf("Missing columns in 14C data: %s\nAvailable columns: %s\nExpected: LabNr, Site/Fundstelle, Age, Error (or variants)",
                     paste(missing_cols, collapse = ", "),
                     paste(available_cols, collapse = ", ")))
      }
      if (!nrow(c14_df)) stop("No 14C data available for calibration")

      # Force numeric (decimal comma -> point)
      to_num <- function(v) suppressWarnings(as.numeric(gsub(",", ".", v)))
      c14_df$Age   <- to_num(c14_df$Age)
      c14_df$Error <- to_num(c14_df$Error)

      bad_age   <- is.na(c14_df$Age)   | c14_df$Age   <= 0
      bad_error <- is.na(c14_df$Error) | c14_df$Error <= 0
      if (any(bad_age))   stop("Invalid age values (NA/<=0) in ", sum(bad_age), " row(s)")
      if (any(bad_error)) stop("Invalid error values (NA/<=0) in ", sum(bad_error), " row(s)")

      if (any(c14_df$Age > 55000, na.rm = TRUE)) {
        showNotification(tr("notify.c14.age.old"),
                         type = "warning", duration = 8)
      }
      if (any(c14_df$Age < 150, na.rm = TRUE)) {
        showNotification(tr("notify.c14.age.young"),
                         type = "warning", duration = 8)
      }

      # Return the normalized DataFrame
      return(c14_df)
    }

    # === MAIN: Local IntCal20 calibration (robust, row-wise) ===
    .calibrate_with_local_intcal <- function(c14_df, default_curve, use_spline = TRUE) {
      if (!exists("ensure_intcal20_df")) {
        stop("IntCal20 loader not available. Ensure helpers/c14_curve_loader.R is loaded.")
      }

      local_curve <- ensure_intcal20_df()

      curve_interpolators <- curve_interpolators_cache()
      
      if (is.null(curve_interpolators) && use_spline) {
        tryCatch({
          curve_interpolators <- get_curve_interpolators(
            curve_df = local_curve,
            method = "monoH.FC",
            use_memoise = FALSE
          )
          curve_interpolators_cache(curve_interpolators)
        }, error = function(e) {
          warning(sprintf("Spline creation failed, using approx(): %s",
                         conditionMessage(e)))
          curve_interpolators <- NULL
        })
      }

      # Per row curve from column 'Curve' (if present), otherwise default
      curve_vec <- if ("Curve" %in% names(c14_df)) {
        .normalize_curve(c14_df$Curve)
      } else {
        rep(.normalize_curve(default_curve), nrow(c14_df))
      }

      n <- nrow(c14_df)
      rows <- vector("list", n)

      withProgress(message = "Local IntCal20 calibration...", value = 0, {
        extrapolation_detected <- FALSE  # Flag for extrapolation warning

        for (i in seq_len(n)) {
          incProgress(1/n, detail = sprintf("Date %d of %d", i, n))

          age   <- c14_df$Age[i]
          error <- c14_df$Error[i]
          lab   <- c14_df$LabNr[i]
          site  <- c14_df$Site[i]  # Now consistently use "Site"
          curve <- curve_vec[i]

          res <- try({
            # Local calibration with own function (standard WITHOUT outlier model)
            result <- .calibrate_single_local(
              c14_age = age,
              c14_error = error,
              curve_data = local_curve,
              interpolators = curve_interpolators
            )
            ag  <- result$age_grid
            den <- result$density

            dx  <- c(diff(ag), stats::median(diff(ag), na.rm = TRUE))
            den <- pmax(den, 0)
            den <- den / sum(den * dx)  # Normalization

            med   <- .grid_weighted_median_bp(ag, den)
            hpd68 <- .calculate_hpd_intervals(ag, den, 0.683)
            hpd95 <- .calculate_hpd_intervals(ag, den, 0.954)

            # ✅ ROBUST DATA.FRAME CREATION
            # Ensure all components are correct
            if (is.null(hpd68) || nrow(hpd68) == 0) {
              hpd68 <- data.frame(low_bp = NA_real_, high_bp = NA_real_, mass = NA_real_)
            }
            if (is.null(hpd95) || nrow(hpd95) == 0) {
              hpd95 <- data.frame(low_bp = NA_real_, high_bp = NA_real_, mass = NA_real_)
            }
            if (length(ag) == 0) ag <- NA_real_
            if (length(den) == 0) den <- NA_real_
            if (is.na(med) || !is.finite(med)) med <- NA_real_

            data.frame(
              Site            = as.character(site),
              LabID           = as.character(lab),
              Age_BP          = as.numeric(age),
              Error_BP        = as.numeric(error),
              Curve           = as.character(curve),
              cal_median_BP   = as.numeric(med),
              hpd68_intervals = I(list(hpd68)),
              hpd95_intervals = I(list(hpd95)),
              age_grid        = I(list(ag)),
              density         = I(list(den)),
              stringsAsFactors = FALSE
            )
          }, silent = TRUE)

          if (inherits(res, "try-error")) {
            # ✅ ROBUST FALLBACK CREATION
            fallback_hpd68 <- data.frame(low_bp = NA_real_, high_bp = NA_real_, mass = NA_real_)
            fallback_hpd95 <- data.frame(low_bp = NA_real_, high_bp = NA_real_, mass = NA_real_)

            rows[[i]] <- data.frame(
              Site            = as.character(site),
              LabID           = as.character(lab),
              Age_BP          = as.numeric(age),
              Error_BP        = as.numeric(error),
              Curve           = as.character(curve),
              cal_median_BP   = NA_real_,
              hpd68_intervals = I(list(fallback_hpd68)),
              hpd95_intervals = I(list(fallback_hpd95)),
              age_grid        = I(list(NA_real_)),
              density         = I(list(NA_real_)),
              stringsAsFactors = FALSE
            )
            
            err_msg <- conditionMessage(attr(res, "condition"))

            # Check if extrapolation was the cause
            if (grepl("outside curve range", err_msg, ignore.case = TRUE)) {
              extrapolation_detected <<- TRUE
            }

            showNotification(sprintf(tr("notify.c14.calib.failed.row"), site, lab),
                             type = "warning", duration = 6)
          } else {
            rows[[i]] <- res
          }
        }

        # ⚠️ EXTRAPOLATION WARNING: Show notification if detected
        if (extrapolation_detected) {
          showNotification(
            tr("notify.c14.extrapolation"),
            type = "warning",
            duration = 10
          )
        }
      })
      do.call(rbind, rows)
    }

    # === MAIN CALIBRATION LOGIC ===
    observeEvent(input$calibrate_c14, {
      req(c14_raw_reactive())

      # NOTE: Mapping check removed - calibration works independently of CA matrix mapping
      # The calibration only needs valid C14 data (Age, Error), not site name matching

      tryCatch({
        c14_df <- c14_raw_reactive()
        c14_df <- .validate_c14_data(c14_df)  # Also normalizes column names

        calibration_in_progress(TRUE)
        calibration_complete(FALSE)

        # Standard calibration WITHOUT outlier model
        result <- .calibrate_with_local_intcal(
          c14_df = c14_df,
          default_curve = "intcal20",
          use_spline = TRUE
        )

        calibrated_data(result)
        calibration_in_progress(FALSE)
        calibration_complete(TRUE)

        showNotification(
          sprintf(tr("notify.c14.calib.complete"), nrow(result)),
          type = "message", duration = 5
        )

      }, error = function(e) {
        # User-friendly error message
        showNotification(paste(tr("notify.c14.calib.error"), conditionMessage(e)),
                         type = "error", duration = 10)
        calibration_in_progress(FALSE)
      })
    })

    # === OUTPUT STATUS ===
    output$calibration_running <- reactive({ isTRUE(calibration_in_progress()) })
    outputOptions(output, "calibration_running", suspendWhenHidden = FALSE)

    output$calibration_completed <- reactive({
      isTRUE(calibration_complete()) && !is.null(calibrated_data())
    })
    outputOptions(output, "calibration_completed", suspendWhenHidden = FALSE)

    output$calibration_summary <- renderText({
      req(calibrated_data())
      data <- calibrated_data()
      n_dates <- nrow(data)
      n_sites <- length(unique(data$Site))

      age_range_bp <- range(data$cal_median_BP, na.rm = TRUE)
      y_old <- calbp_to_year_ce(max(age_range_bp, na.rm = TRUE))  # oldest = largest BP
      y_yng <- calbp_to_year_ce(min(age_range_bp, na.rm = TRUE))  # youngest = smallest BP

      sprintf("📊 %d dates from %d sites • 🕰️ Age range: %d–%d cal BP (%s – %s) • 📈 Curve: Local IntCal20.14c",
              n_dates, n_sites,
              round(age_range_bp[1]), round(age_range_bp[2]),
              format_year_label(y_old), format_year_label(y_yng))
    })

    # Helper: Format HPD with probabilities
    .format_hpd_with_prob <- function(hpd_df) {
      if (is.null(hpd_df) || nrow(hpd_df) == 0 || all(is.na(hpd_df$low_bp))) {
        return("—")
      }
      
      # Sort older → younger
      hpd_df <- hpd_df[order(-hpd_df$high_bp), ]
      
      parts <- vapply(seq_len(nrow(hpd_df)), function(i) {
        low_ce <- calbp_to_year_ce(hpd_df$low_bp[i])
        high_ce <- calbp_to_year_ce(hpd_df$high_bp[i])
        prob <- round(hpd_df$mass[i] * 100)
        
        sprintf("%s–%s (%d%%)", 
                format_year_label(high_ce),
                format_year_label(low_ce),
                prob)
      }, character(1))
      
      paste(parts, collapse = ", ")
    }
    
    # === TABLE DISPLAY ===
    output$calibrated_table <- DT::renderDataTable({
      req(calibrated_data())
      data <- calibrated_data()
      
      display_data <- data.frame(
        Site   = data$Site,
        LabID  = data$LabID,
        "Roh (BP)" = paste0(data$Age_BP, " ± ", data$Error_BP),
        check.names = FALSE,
        stringsAsFactors = FALSE
      )
      
      # Median (cal BC/AD)
      display_data[["Median (cal BC/AD)"]] <- format_year_label(calbp_to_year_ce(data$cal_median_BP))
      
      # 68% / 95% HPD with probabilities
      display_data[["68% HPD (cal BC/AD)"]] <- vapply(
        data$hpd68_intervals, .format_hpd_with_prob, character(1)
      )
      display_data[["95% HPD (cal BC/AD)"]] <- vapply(
        data$hpd95_intervals, .format_hpd_with_prob, character(1)
      )
      
      DT::datatable(
        display_data,
        options = list(
          paging = FALSE,
          searching = FALSE,
          scrollX = TRUE,
          scrollY = "400px",
          dom = 't'
        ),
        rownames = FALSE,
        caption = HTML(
          paste0(
            "<div style='text-align:left;'>",
            "<strong>", tr("c14.calib.table.caption"), "</strong><br>",
            "<small>",
            "<span style='cursor:help;' title='", tr("c14.calib.table.bcad.tooltip"), "'>",
            tr("c14.calib.table.bcad"), "</span> | ",
            "<span style='cursor:help;' title='", tr("c14.calib.table.interval.tooltip"), "'>",
            tr("c14.calib.table.interval"), "</span> | ",
            "<span style='cursor:help;' title='", tr("c14.calib.table.prob.tooltip"), "'>",
            tr("c14.calib.table.prob"), "</span>",
            "</small>",
            "</div>"
          )
        )
      )
    })
    
    # === EXCEL DOWNLOAD HANDLER ===
    output$download_calibrated_excel <- downloadHandler(
      filename = function() sprintf("SeriARC_calibrated_c14_%s.xlsx", Sys.Date()),
      content = function(file) {
        req(calibrated_data())
        if (!requireNamespace("writexl", quietly = TRUE)) {
          showNotification(tr("notify.c14.writexl.missing"),
                           type = "error", duration = 8)
          return()
        }
        
        data <- calibrated_data()
        
        # === EXTENDED EXPORT: All parameters documented ===

        # 1. MAIN SHEET: Detailed calibration results
        main_data <- data.frame(
          Site        = data$Site,
          LabID       = data$LabID,
          Age_BP      = data$Age_BP,
          Error_BP    = data$Error_BP,
          Curve       = data$Curve,
          
          # Median
          cal_median_BP  = round(data$cal_median_BP),
          cal_median_cal = format_year_label(calbp_to_year_ce(data$cal_median_BP)),
          
          # HPD-Intervalle als Strings
          HPD_68_cal     = vapply(data$hpd68_intervals, intervals_to_string_calbc, character(1)),
          HPD_95_cal     = vapply(data$hpd95_intervals, intervals_to_string_calbc, character(1)),
          
          # Mode (Maximum of posterior)
          Mode_BP = sapply(seq_len(nrow(data)), function(i) {
            if (length(data$age_grid[[i]]) > 1 && length(data$density[[i]]) > 1) {
              idx <- which.max(data$density[[i]])
              round(data$age_grid[[i]][idx])
            } else {
              NA_real_
            }
          }),
          Mode_cal = sapply(seq_len(nrow(data)), function(i) {
            if (length(data$age_grid[[i]]) > 1 && length(data$density[[i]]) > 1) {
              idx <- which.max(data$density[[i]])
              format_year_label(calbp_to_year_ce(data$age_grid[[i]][idx]))
            } else {
              NA_character_
            }
          }),
          
          stringsAsFactors = FALSE,
          check.names = FALSE
        )
        
        # 2. PARAMETER SHEET: Documentation of calibration settings
        params_data <- data.frame(
          Parameter = c(
            "Calibration Method",
            "Curve Used",
            "Curve File",
            "Outlier Model",
            "Spline Interpolation",
            "Number of Dates",
            "Number of Sites",
            "Oldest Date (Median)",
            "Youngest Date (Median)",
            "Calibration Timestamp",
            "SeriARC Version"
          ),
          Value = c(
            "Custom implementation with local IntCal20.14c",
            "IntCal20",
            "intcal20.14c (local)",
            "No (standard, only in OxCal with constraints)",
            "Yes (monotone splines with caching)",
            nrow(data),
            length(unique(data$Site)),
            sprintf("%d cal BP (%s)",
                   round(max(data$cal_median_BP, na.rm = TRUE)),
                   format_year_label(calbp_to_year_ce(max(data$cal_median_BP, na.rm = TRUE)))),
            sprintf("%d cal BP (%s)",
                   round(min(data$cal_median_BP, na.rm = TRUE)),
                   format_year_label(calbp_to_year_ce(min(data$cal_median_BP, na.rm = TRUE)))),
            as.character(Sys.time()),
            APP_VERSION %||% "1.0.0"
          ),
          stringsAsFactors = FALSE
        )

        # 3. NOTES SHEET: Explanations for users
        notes_data <- data.frame(
          Note = c(
            "BC/AD Convention",
            "Interval Order",
            "HPD Intervals",
            "Median vs. Mode",
            "Outlier Model",
            "Curve Range",
            "Extrapolation",
            "Citation"
          ),
          Explanation = c(
            "BC = Before Christ, AD = Anno Domini. Year 0 does not exist! From 1 BC it goes directly to 1 AD.",
            "Intervals are sorted from older (left) to younger (right). Example: '3000-2800 cal BC' means from 3000 BC to 2800 BC.",
            "HPD = Highest Posterior Density. 68% HPD corresponds to ~1σ, 95% HPD corresponds to ~2σ. Multi-modal distributions may have several separate intervals.",
            "Median = Middle value (50% quantile). Mode = Most frequent value (maximum of posterior). For symmetric distributions both are equal.",
            "No outlier model in batch calibration. Outlier treatment only meaningful in OxCal with stratigraphic constraints.",
            "IntCal20 valid from 0-55000 cal BP. Dates outside this range were clipped and generate warnings.",
            "If dates are outside the curve range, they were restricted to the valid range. Check the console for warnings.",
            "When using this data please cite SeriARC and IntCal20: Reimer et al. (2020) Radiocarbon 62."
          ),
          stringsAsFactors = FALSE
        )

        sheets <- list(
          "Calibrated_Data" = main_data,
          "Parameters" = params_data,
          "Notes" = notes_data
        )
        
        writexl::write_xlsx(sheets, path = file)
        showNotification(tr("notify.c14.excel.complete"),
                         type = "message", duration = 3)
      }
    )
    
    # === SINGLE DATE CALIBRATION ===
    single_calibrated <- reactiveVal(FALSE)
    single_result_data <- reactiveVal(NULL)

    observe({
      # Get raw data - with safety checks
      raw_data <- tryCatch(c14_raw_reactive(), error = function(e) NULL)

      req(raw_data, nrow(raw_data) > 0)

      # Validate and normalize data
      c14_data <- tryCatch(.validate_c14_data(raw_data), error = function(e) NULL)

      if (!is.null(c14_data) && nrow(c14_data) > 0) {
        choices <- setNames("manual", tr("c14.calib.single.manual"))

        for (i in seq_len(nrow(c14_data))) {
          # Safe access with fallbacks
          lab_nr <- if ("LabNr" %in% names(c14_data)) c14_data$LabNr[i] else paste0("Sample_", i)
          site <- if ("Site" %in% names(c14_data)) c14_data$Site[i] else "Unknown"
          age <- if ("Age" %in% names(c14_data)) c14_data$Age[i] else NA
          error <- if ("Error" %in% names(c14_data)) c14_data$Error[i] else NA

          if (!is.na(age) && !is.na(error)) {
            label <- sprintf("%s (%s, %d±%d BP)", lab_nr, site, as.integer(age), as.integer(error))
            choices[label] <- as.character(i)
          }
        }

        updateSelectInput(session, "single_data_source", choices = choices)
      }
    })

    # Fill fields on dropdown selection
    observeEvent(input$single_data_source, {
      if (input$single_data_source == "manual") {
        # Manual input: leave fields empty
        updateTextInput(session, "single_labid", value = "")
        updateNumericInput(session, "single_age", value = NA)
        updateNumericInput(session, "single_error", value = NA)
      } else {
        # Date from import
        req(c14_raw_reactive())
        c14_data <- tryCatch({
          .validate_c14_data(c14_raw_reactive())
        }, error = function(e) NULL)

        if (!is.null(c14_data)) {
          idx <- as.integer(input$single_data_source)

          if (idx > 0 && idx <= nrow(c14_data)) {
            updateTextInput(session, "single_labid",
                           value = as.character(c14_data$LabNr[idx]))
            updateNumericInput(session, "single_age",
                              value = c14_data$Age[idx])
            updateNumericInput(session, "single_error",
                              value = c14_data$Error[idx])
          }
        }
      }
    })


    observeEvent(input$calibrate_single, {
      # Validation
      req(input$single_labid, input$single_age, input$single_error)

      if (nchar(trimws(input$single_labid)) == 0) {
        showNotification(tr("notify.c14.labid.required"), type = "warning", duration = 3)
        return()
      }

      if (is.na(input$single_age) || input$single_age <= 0) {
        showNotification(tr("notify.c14.age.invalid"), type = "warning", duration = 3)
        return()
      }

      if (is.na(input$single_error) || input$single_error <= 0) {
        showNotification(tr("notify.c14.error.invalid"), type = "warning", duration = 3)
        return()
      }

      tryCatch({
        local_curve <- ensure_intcal20_df()

        curve_interpolators <- curve_interpolators_cache()
        if (is.null(curve_interpolators)) {
          curve_interpolators <- tryCatch({
            get_curve_interpolators(
              curve_df = local_curve,
              method = "monoH.FC",
              use_memoise = FALSE
            )
          }, error = function(e) NULL)
          if (!is.null(curve_interpolators)) {
            curve_interpolators_cache(curve_interpolators)
          }
        }

        # Perform calibration (standard WITHOUT outlier model)
        result <- .calibrate_single_local(
          c14_age = input$single_age,
          c14_error = input$single_error,
          curve_data = local_curve,
          interpolators = curve_interpolators
        )
        
        ag  <- result$age_grid
        den <- result$density

        # Normalization
        dx  <- c(diff(ag), stats::median(diff(ag), na.rm = TRUE))
        den <- pmax(den, 0)
        den <- den / sum(den * dx)

        med   <- .grid_weighted_median_bp(ag, den)
        hpd68 <- .calculate_hpd_intervals(ag, den, 0.683)
        hpd95 <- .calculate_hpd_intervals(ag, den, 0.954)

        single_result_data(list(
          labid = input$single_labid,
          age = input$single_age,
          error = input$single_error,
          age_grid = ag,
          density = den,
          median_bp = med,
          hpd68 = hpd68,
          hpd95 = hpd95
        ))
        
        single_calibrated(TRUE)
        showNotification(tr("notify.c14.single.success"), type = "message", duration = 3)

      }, error = function(e) {
        showNotification(paste(tr("notify.c14.single.error"), conditionMessage(e)),
                         type = "error", duration = 8)
        single_calibrated(FALSE)
      })
    })
    
    # Status for conditionalPanel
    output$single_calibrated <- reactive({ isTRUE(single_calibrated()) })
    outputOptions(output, "single_calibrated", suspendWhenHidden = FALSE)

    # Status: Has imported data?
    output$has_imported_data <- reactive({
      c14_data <- c14_raw_reactive()
      !is.null(c14_data) && nrow(c14_data) > 0
    })
    outputOptions(output, "has_imported_data", suspendWhenHidden = FALSE)
    
    # === OXCAL-STYLE SINGLE PLOT (ONE SOURCE OF TRUTH) ===
    # Single drawing function for screen + export -> guarantees identical appearance.
    .plot_single_oxcal_style <- function(single_data, local_curve, main = NULL) {
      stopifnot(is.list(single_data))
      stopifnot(is.data.frame(local_curve))

      # ---- Prepare data ----
      ag_bp  <- single_data$age_grid
      den    <- single_data$density

      if (length(ag_bp) < 2 || length(den) < 2 || all(is.na(ag_bp)) || all(is.na(den))) {
        plot.new()
        text(0.5, 0.5, "No valid posterior data", cex = 1.0)
        return(invisible(NULL))
      }

      # Normalization (robust)
      o <- order(ag_bp)
      ag_bp <- ag_bp[o]
      den   <- pmax(den[o], 0)
      dx <- c(diff(ag_bp), stats::median(diff(ag_bp), na.rm = TRUE))
      if (!all(is.finite(dx)) || any(dx <= 0)) dx <- rep(1, length(ag_bp))
      s <- sum(den * dx)
      if (!is.finite(s) || s <= 0) den <- rep(1 / length(den), length(den)) else den <- den / s

      # ---- X-Limits: based on 95% HPD (OxCal-like), with small buffer ----
      if (!is.null(single_data$hpd95) && nrow(single_data$hpd95) > 0 && !all(is.na(single_data$hpd95$low_bp))) {
        hpd_range_bp <- range(c(single_data$hpd95$low_bp, single_data$hpd95$high_bp), na.rm = TRUE)
        xlim_bp <- hpd_range_bp + c(-100, 100)
      } else {
        med_bp <- single_data$median_bp
        sigma_est <- single_data$error * 1.5
        xlim_bp <- c(med_bp - 3 * sigma_est, med_bp + 3 * sigma_est)
      }

      # Clip to curve range
      curve_rng <- range(local_curve$CalBP, na.rm = TRUE)
      xlim_bp[1] <- max(xlim_bp[1], curve_rng[1])
      xlim_bp[2] <- min(xlim_bp[2], curve_rng[2])

      # In calibrated calendar years (CE; negative = BC)
      xlim_cal <- calbp_to_year_ce(xlim_bp)
      xlim_cal <- sort(xlim_cal)

      # ---- Curve subset exactly to edges (incl. forced edge points) ----
      # Extend slightly so interpolation remains stable
      xlim_bp_ext <- xlim_bp + c(-100, 100)
      xlim_bp_ext[1] <- max(xlim_bp_ext[1], curve_rng[1])
      xlim_bp_ext[2] <- min(xlim_bp_ext[2], curve_rng[2])

      curve_subset <- local_curve[
        local_curve$CalBP >= min(xlim_bp_ext) & local_curve$CalBP <= max(xlim_bp_ext),
      ]

      left_point <- data.frame(
        CalBP = xlim_bp[1],
        C14Age = approx(local_curve$CalBP, local_curve$C14Age, xout = xlim_bp[1], rule = 2)$y,
        Error  = approx(local_curve$CalBP, local_curve$Error,  xout = xlim_bp[1], rule = 2)$y
      )
      right_point <- data.frame(
        CalBP = xlim_bp[2],
        C14Age = approx(local_curve$CalBP, local_curve$C14Age, xout = xlim_bp[2], rule = 2)$y,
        Error  = approx(local_curve$CalBP, local_curve$Error,  xout = xlim_bp[2], rule = 2)$y
      )

      curve_subset <- rbind(left_point, curve_subset, right_point)
      curve_subset <- curve_subset[order(curve_subset$CalBP), ]

      curve_cal <- calbp_to_year_ce(curve_subset$CalBP)

      # ---- Y-Limits: OxCal-like (no asp=1) + space below for posterior+brackets ----
      curve_range <- range(curve_subset$C14Age, na.rm = TRUE)
      curve_buffer <- diff(curve_range) * 0.06
      if (!is.finite(curve_buffer) || curve_buffer <= 0) curve_buffer <- 50

      # Extra space below: posterior + brackets + margin
      ylim_top <- curve_range[2] + curve_buffer * 0.6
      ylim_bot <- curve_range[1] - curve_buffer * 3.0
      ylim_bp  <- c(ylim_bot, ylim_top)

      # ---- Plot base: OxCal style (minimal decoration, no gridlines) ----
      op <- par(no.readonly = TRUE)
      on.exit(par(op), add = TRUE)

      par(mar = c(2.2, 3.0, 1.2, 0.6), xaxs = "i", yaxs = "i")  # Tight margins, not 0

      plot(NA,
           xlim = xlim_cal,
           ylim = ylim_bp,
           xlab = "Calibrated date (calBC/AD)",
           ylab = "Radiocarbon determination (BP)",
           main = if (is.null(main)) "" else main,
           las = 1,
           xaxt = "n",
           yaxt = "n")

      # ---- Axes: OxCal-like ticks (major/minor/tiny), no grid ----
      # X: 100s major + 50s minor + 10s tiny
      xr <- xlim_cal
      x_major <- seq(floor(xr[1] / 100) * 100, ceiling(xr[2] / 100) * 100, by = 100)
      x_minor <- seq(floor(xr[1] / 50) * 50,  ceiling(xr[2] / 50) * 50,  by = 50)
      x_minor <- x_minor[!x_minor %in% x_major]
      x_tiny <- seq(floor(xr[1] / 10) * 10,  ceiling(xr[2] / 10) * 10,  by = 10)
      x_tiny <- x_tiny[!x_tiny %in% c(x_major, x_minor)]

      axis(1, at = x_major, labels = x_major, cex.axis = 1.0, font = 2)  # Bold
      axis(1, at = x_minor, labels = FALSE, tcl = -0.25)
      axis(1, at = x_tiny, labels = FALSE, tcl = -0.15)  # 10s ticks

      # Y: 100s major + 50s minor + 10s tiny
      yr <- ylim_bp
      y_major <- seq(floor(yr[1] / 100) * 100, ceiling(yr[2] / 100) * 100, by = 100)
      y_minor <- seq(floor(yr[1] / 50) * 50,  ceiling(yr[2] / 50) * 50,  by = 50)
      y_minor <- y_minor[!y_minor %in% y_major]
      y_tiny <- seq(floor(yr[1] / 10) * 10,  ceiling(yr[2] / 10) * 10,  by = 10)
      y_tiny <- y_tiny[!y_tiny %in% c(y_major, y_minor)]

      axis(2, at = y_major, labels = y_major, las = 1, cex.axis = 1.0, font = 2)  # Bold
      axis(2, at = y_minor, labels = FALSE, tcl = -0.25)
      axis(2, at = y_tiny, labels = FALSE, tcl = -0.15)  # 10s ticks

      box()

      # ---- 1) Calibration curve (blue) + uncertainty band (1σ) ----
      polygon(c(curve_cal, rev(curve_cal)),
              c(curve_subset$C14Age - curve_subset$Error,
                rev(curve_subset$C14Age + curve_subset$Error)),
              col = rgb(0.68, 0.85, 0.90, 0.70),
              border = NA)

      lines(curve_cal, curve_subset$C14Age, col = "#4169E1", lwd = 2)

      # ---- 2) Uncalibrated 14C distribution (red) on left axis - TALLER ----
      age <- single_data$age
      err <- single_data$error

      gauss_bp   <- seq(age - 4 * err, age + 4 * err, length.out = 200)  # TALLER: ±3σ->±4σ
      gauss_dens <- stats::dnorm(gauss_bp, mean = age, sd = err)
      max_gauss  <- max(gauss_dens, na.rm = TRUE)
      if (!is.finite(max_gauss) || max_gauss <= 0) max_gauss <- 1

      gauss_width  <- diff(xlim_cal) * 0.12  # NARROWER: 0.18->0.12
      gauss_x0     <- xlim_cal[1]
      gauss_x      <- gauss_x0 + (gauss_dens / max_gauss) * gauss_width

      polygon(c(rep(gauss_x0, length(gauss_bp)), rev(gauss_x)),
              c(gauss_bp, rev(gauss_bp)),
              col = rgb(1, 0.60, 0.60, 0.50),
              border = "red", lwd = 1.2)

      points(gauss_x0, age, pch = 16, col = "red", cex = 1.0)

      # ---- 3) Posterior anchored at plot bottom (OxCal-like) ----
      cal_years <- calbp_to_year_ce(ag_bp)

      # BP value along curve for each cal position (so density "sits on the curve")
      bp_at_cal <- approx(curve_cal, curve_subset$C14Age, xout = cal_years, rule = 2)$y

      max_dens <- max(den, na.rm = TRUE)
      if (!is.finite(max_dens) || max_dens <= 0) max_dens <- 1

      # Baseline: EVEN CLOSER to lower plot edge
      baseline_bp <- ylim_bp[1] + diff(ylim_bp) * 0.06  # 0.10->0.06: Even closer
      dens_height <- diff(ylim_bp) * 0.22
      dens_scaled <- (den / max_dens) * dens_height

      valid <- is.finite(cal_years) & is.finite(dens_scaled)
      if (sum(valid) > 2) {
        polygon(c(cal_years[valid], rev(cal_years[valid])),
                c(baseline_bp + dens_scaled[valid], rep(baseline_bp, sum(valid))),
                col = "gray60", border = NA)
      }

      # ---- 4) HPD brackets below baseline - EVEN CLOSER TO EDGE ----
      bracket_h  <- diff(ylim_bp) * 0.010
      b68_y      <- baseline_bp - diff(ylim_bp) * 0.010  # 0.015->0.010: Even closer
      b95_y      <- baseline_bp - diff(ylim_bp) * 0.030  # 0.040->0.030: Even closer

      .draw_brackets <- function(hpd_df, y0) {
        if (is.null(hpd_df) || nrow(hpd_df) == 0 || all(is.na(hpd_df$low_bp))) return(invisible(NULL))
        # older -> younger: in cal CE "older" is usually smaller/negative; we just draw each segment
        for (i in seq_len(nrow(hpd_df))) {
          if (is.na(hpd_df$low_bp[i]) || is.na(hpd_df$high_bp[i])) next
          x1 <- calbp_to_year_ce(hpd_df$high_bp[i])  # older
          x2 <- calbp_to_year_ce(hpd_df$low_bp[i])   # younger
          xr <- sort(c(x1, x2))
          segments(xr[1], y0, xr[2], y0, lwd = 3, col = "black")
          segments(xr[1], y0 - bracket_h, xr[1], y0 + bracket_h, lwd = 3, col = "black")
          segments(xr[2], y0 - bracket_h, xr[2], y0 + bracket_h, lwd = 3, col = "black")
        }
      }

      .draw_brackets(single_data$hpd68, b68_y)
      .draw_brackets(single_data$hpd95, b95_y)

      # ---- 5) Median marker on X-axis ----
      med_cal <- calbp_to_year_ce(single_data$median_bp)
      # Triangle pointing up at lower plot edge
      points(med_cal, ylim_bp[1], pch = 17, col = "black", cex = 1.3)

      # ---- 6) Text block top right (OxCal-like, compact) - LARGER FONT ----
      tx <- xlim_cal[2] - diff(xlim_cal) * 0.02
      ty <- ylim_bp[2] - diff(ylim_bp) * 0.06
      lh <- diff(ylim_bp) * 0.055

      text(tx, ty, sprintf("Date(%s,%d±%d)", single_data$labid, age, err),
           adj = c(1, 0.5), cex = 0.85, font = 2)  # Bold
      ty <- ty - lh

      med_str <- format_year_label(calbp_to_year_ce(single_data$median_bp))
      text(tx, ty, sprintf("Median: %s", med_str),
           adj = c(1, 0.5), cex = 0.83, font = 2)  # Bold
      ty <- ty - lh

      # 68/95 Text: use existing formatter if available in module
      if (exists(".format_hpd_with_prob", mode = "function")) {
        h68 <- .format_hpd_with_prob(single_data$hpd68)
        h95 <- .format_hpd_with_prob(single_data$hpd95)
        text(tx, ty, sprintf("68%% HPD: %s", h68), adj = c(1, 0.5), cex = 0.81, font = 2); ty <- ty - lh  # Bold
        text(tx, ty, sprintf("95%% HPD: %s", h95), adj = c(1, 0.5), cex = 0.81, font = 2)  # Bold
      } else {
        # Fallback: without probability shares
        text(tx, ty, "68% HPD", adj = c(1, 0.5), cex = 0.81, font = 2); ty <- ty - lh  # Bold
        text(tx, ty, "95% HPD", adj = c(1, 0.5), cex = 0.81, font = 2)  # Bold
      }

      invisible(NULL)
    }
    
    # Plot of posterior distribution (OxCal-style using central function)
    output$single_plot <- renderPlot({
      req(single_result_data())
      single_data <- single_result_data()
      local_curve <- ensure_intcal20_df()
      
      .plot_single_oxcal_style(
        single_data = single_data,
        local_curve = local_curve,
        main = NULL
      )
    })
    
    # Result text with HPD intervals
    output$single_result <- renderUI({
      req(single_result_data())
      data <- single_result_data()
      
      med_str <- format_year_label(calbp_to_year_ce(data$median_bp))
      hpd68_str <- .format_hpd_with_prob(data$hpd68)
      hpd95_str <- .format_hpd_with_prob(data$hpd95)
      
      HTML(sprintf(
        "<p><strong>%s:</strong> %s</p>
        <p><strong>%s:</strong> %d ± %d BP</p>
        <hr>
        <p><strong>%s:</strong> %s</p>
        <p><strong>68%% HPD:</strong><br>%s</p>
        <p><strong>95%% HPD:</strong><br>%s</p>
        <hr>
        <p><small>ℹ️ %s</small></p>",
        tr("c14.calib.single.result.labid"), data$labid,
        tr("c14.calib.single.result.age"), data$age, data$error,
        tr("c14.calib.single.result.median"), med_str,
        hpd68_str, hpd95_str,
        tr("c14.calib.single.result.probhint")
      ))
    })
    
    # Download handler for single calibration plot (uses central function)
    output$dl_single_plot <- downloadHandler(
      filename = function() {
        labid <- input$single_labid
        if (is.null(labid) || nchar(trimws(labid)) == 0) {
          labid <- "calibration"
        } else {
          labid <- gsub("[^a-zA-Z0-9_-]", "_", labid)
        }
        paste0("SeriARC_cal_", labid, "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".png")
      },
      content = function(file) {
        req(single_result_data())
        single_data <- single_result_data()
        local_curve <- ensure_intcal20_df()
        
        # PNG with high resolution (another 20% smaller: 960x640 -> 768x512)
        png(file, width = 768, height = 512, res = 150)
        
        .plot_single_oxcal_style(
          single_data = single_data,
          local_curve = local_curve,
          main = NULL
        )
        
        dev.off()
      }
    )
    
    # === RETURN VALUES ===
    return(list(
      get_calibrated = reactive({ calibrated_data() })
    ))
  })
}


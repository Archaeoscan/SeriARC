# mod_polynomial_projection.R - POLYNOMIAL PROJECTION (Arc Length)
# SeriARC: Projects CA sites onto a fitted polynomial curve and computes arc length
# as an alternative chronological time scale.
#
# Method based on:
#   Weninger, B. & Krauss, R. (2021). Pottery Dating by Correspondence Analysis -
#   Time Series Analysis. In: Wissing, A. et al. (eds.), Von der Tongrube bis zur
#   Abfallgrube. Tubinger Schriften zur Ur- und Fruhgeschichtlichen Archaologie, pp. 31-56.
#
# Arc length formula based on:
#   Leibovici, C. (2014). Arc length of general polynomial.
#   https://math.stackexchange.com/questions/922098/arc-length-of-general-polynomial
#   Last visited: 26 Sep 2023.
#   [Trapezoidal approximation: sum(sqrt(dx^2 + dy^2)) over 2000 grid points]
#
# Algorithm:
#   1. Fit polynomial of chosen degree to site/type/combined scores (Dim1, Dim2)
#   2. Project each point orthogonally onto nearest point on curve
#   3. Compute arc length from curve start to each projected point (normalized 0-1)

mod_polynomial_projection_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL
}

mod_polynomial_projection_server <- function(
    filtered_data, cache, meta_data = NULL,
    get_site_group = NULL, get_element_details = NULL,
    c14_calibrated = NULL,
    oxcal_results  = NULL,
    input, output, session, tr = function(x) x) {

  source("helpers/plot_components.R", local = TRUE)
  source("helpers/download_components.R", local = TRUE)

  # ============================================================
  # HELPER FUNCTIONS
  # ============================================================

  # Evaluate polynomial at x values
  # coef: named vector from lm (Intercept, poly1, poly2, ...)
  eval_poly <- function(coef, x) {
    coef <- as.numeric(coef)          # strip names/attributes → plain numeric
    x    <- as.numeric(x)
    degree <- length(coef) - 1
    y <- rep(coef[1], length(x))
    for (d in seq_len(degree)) {
      y <- y + coef[d + 1] * x^d
    }
    y
  }

  # Fit polynomial of given degree to (dim1, dim2) coordinates
  fit_polynomial <- function(dim1, dim2, degree = 2) {
    if (length(dim1) < degree + 2) {
      degree <- max(1, length(dim1) - 2)
    }
    formula_str <- paste0("dim2 ~ poly(dim1, ", degree, ", raw = TRUE)")
    fit <- lm(as.formula(formula_str))
    coef(fit)
  }

  # For each point (px, py), find x on curve that minimizes distance (unweighted Euclidean).
  project_point_to_curve <- function(px, py, poly_coef, x_range, n = 2000,
                                     w1 = 1, w2 = 1) {
    px <- as.numeric(px)[[1L]]        # ensure plain scalar (strips names/dim)
    py <- as.numeric(py)[[1L]]
    w1 <- as.numeric(w1)[[1L]]
    w2 <- as.numeric(w2)[[1L]]
    xs <- seq(x_range[1], x_range[2], length.out = n)
    ys <- eval_poly(poly_coef, xs)    # already returns plain numeric via eval_poly
    dists <- sqrt((w1 * (xs - px))^2 + (w2 * (ys - py))^2)
    xs[which.min(dists)]
  }

  # Compute arc length from x_start to x_end along polynomial (trapezoidal integration)
  arc_length_between <- function(poly_coef, x_start, x_end, n = 2000) {
    if (abs(x_end - x_start) < .Machine$double.eps) return(0)
    xs <- seq(x_start, x_end, length.out = n)
    ys <- eval_poly(poly_coef, xs)
    dx <- diff(xs)
    dy <- diff(ys)
    sum(sqrt(dx^2 + dy^2))
  }

  # Full arc length of curve across x_range
  total_arc_length <- function(poly_coef, x_range, n = 2000) {
    arc_length_between(poly_coef, x_range[1], x_range[2], n)
  }

  # Project a group of points onto the curve; return data.frame
  # Returns columns: Name, Dim1, Dim2, ProjX, ProjY, ArcLength
  project_group <- function(dim1, dim2, names, poly_coef, x_range, x_min, total_len,
                             w1 = 1, w2 = 1) {
    proj_xs <- vapply(seq_along(dim1), function(i) {
      project_point_to_curve(dim1[i], dim2[i], poly_coef, x_range, w1 = w1, w2 = w2)
    }, numeric(1))
    proj_ys    <- eval_poly(poly_coef, proj_xs)
    arc_lengths <- vapply(proj_xs, function(px) {
      arc_length_between(poly_coef, x_min, px)
    }, numeric(1))
    data.frame(
      Name        = names,
      Dim1        = dim1,
      Dim2        = dim2,
      ProjX       = proj_xs,
      ProjY       = proj_ys,
      ArcLength   = arc_lengths / total_len,
      DistToCurve = sqrt((dim1 - proj_xs)^2 + (dim2 - proj_ys)^2),
      stringsAsFactors = FALSE
    )
  }

  # ── ¹⁴C-Aggregations-Helfer ────────────────────────────────────────────────

  # Berechnet den Median der Summen-Posteriori-Verteilung (SPD) aus einer Liste
  # kalibrierter ¹⁴C-Daten (age_grid + density). Korrekte Methode für Sites mit
  # mehreren Daten. Fallback auf Mittelwert der cal_median_BP wenn Verteilungen
  # nicht verfügbar.
  poly_spd_median <- function(ag_list, dn_list, fallback_medians) {
    # Nur Einträge mit gültigen Verteilungen verwenden
    valid <- which(
      !sapply(ag_list, is.null) &
      !sapply(dn_list, is.null) &
      sapply(ag_list, length) > 1
    )
    if (length(valid) == 0) return(mean(fallback_medians, na.rm = TRUE))

    if (length(valid) == 1) {
      # Einzelnes Datum: Median direkt aus Dichteverteilung
      ag <- ag_list[[valid[1]]]
      dn <- dn_list[[valid[1]]]
    } else {
      # Mehrere Daten: SPD auf gemeinsames Alters-Grid interpolieren und summieren
      all_ages <- sort(unique(unlist(ag_list[valid])))
      spd      <- numeric(length(all_ages))
      for (i in valid) {
        ag <- ag_list[[i]]; dn <- dn_list[[i]]
        s  <- sum(dn, na.rm = TRUE)
        if (s == 0 || is.na(s)) next
        dn <- dn / s                                          # normieren
        iv <- stats::approx(ag, dn, xout = all_ages, rule = 2)$y
        iv[is.na(iv)] <- 0
        spd <- spd + iv
      }
      ag <- all_ages
      dn <- spd
    }

    # Median: CDF bilden (aufsteigend cal BP = jünger zuerst) → 50. Perzentil
    ord  <- order(ag)
    ag_s <- ag[ord]; dn_s <- dn[ord]
    s    <- sum(dn_s, na.rm = TRUE)
    if (s == 0 || is.na(s)) return(mean(fallback_medians, na.rm = TRUE))
    cum  <- cumsum(dn_s / s)
    idx  <- which(cum >= 0.5)[1]
    if (is.na(idx)) return(mean(fallback_medians, na.rm = TRUE))
    ag_s[idx]
  }

  # Aggregiert kalibrierte 14C-Daten zu einer Zeile pro Site.
  # Verwendet SPD-Median (aus age_grid/density) wenn vorhanden,
  # sonst Mittelwert der cal_median_BP Werte.
  aggregate_c14_by_site <- function(cal_df) {
    sites <- unique(cal_df$Site)
    result <- lapply(sites, function(s) {
      sub <- cal_df[cal_df$Site == s, , drop = FALSE]
      n   <- nrow(sub)
      has_dist <- all(c("age_grid", "density") %in% names(sub)) &&
                  !is.null(sub$age_grid[[1]])
      spd_med <- if (has_dist) {
        poly_spd_median(
          ag_list          = sub$age_grid,
          dn_list          = sub$density,
          fallback_medians = sub$cal_median_BP
        )
      } else {
        mean(sub$cal_median_BP, na.rm = TRUE)
      }
      data.frame(
        Site          = s,
        cal_median_BP = spd_med,
        n_dates       = n,
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, result)
  }

  # Aggregiert OxCal-Posterior-Daten zu einer Zeile pro Site.
  # Extrahiert Site-Namen aus dem OxCal-tidy-Format ("SiteName_LabID"),
  # filtert auf R_Date-Einträge und gibt cal_median_BP zurück.
  aggregate_oxcal_anchor <- function(oxcal_data) {
    if (is.null(oxcal_data) || is.null(oxcal_data$tidy)) return(NULL)
    tidy <- oxcal_data$tidy
    if (nrow(tidy) == 0 || !"type" %in% names(tidy)) return(NULL)

    # Nur R_Date-Einträge (keine Boundaries, Phasen, Spans usw.)
    dates_df <- tidy[grepl("R_Date", tidy$type, fixed = TRUE), , drop = FALSE]
    if (nrow(dates_df) == 0 || !"median" %in% names(dates_df)) return(NULL)

    # Site-Namen extrahieren: "SiteName_LabID" → "SiteName"
    # Selbe Regex wie in mod_ca_c14_overlay.R
    dates_df$Site <- sapply(dates_df$name, function(n) {
      cleaned <- gsub("^R_Date[\\s:(]*", "", n, perl = TRUE)
      cleaned <- gsub("[\\);]+$",         "", cleaned)
      if (grepl("_[A-Za-z]+-\\d+$", cleaned) || grepl("_[A-Za-z]+\\d+$", cleaned)) {
        return(trimws(sub("_[A-Za-z]+-?\\d+$", "", cleaned)))
      }
      if (grepl("_\\d+$", cleaned)) return(trimws(sub("_\\d+$", "", cleaned)))
      trimws(cleaned)
    })

    # Aggregiere: Median der Posterior-Mediane pro Site (OxCal-Werte bereits modelliert)
    sites  <- unique(dates_df$Site)
    result <- lapply(sites, function(s) {
      sub <- dates_df[dates_df$Site == s, , drop = FALSE]
      data.frame(
        Site          = s,
        cal_median_BP = median(sub$median, na.rm = TRUE),
        n_dates       = nrow(sub),
        stringsAsFactors = FALSE
      )
    })
    do.call(rbind, result)
  }

  # ============================================================
  # DISPLAY TRIGGER: force all plots to re-render on any display-setting change
  # ============================================================
  # observe() is always active (unlike a lazy reactive), so it reliably tracks
  # all display inputs even when they are created dynamically inside renderUI.
  # Incrementing poly_viz_trigger() inside isolate() avoids a reactive loop.

  poly_viz_trigger <- reactiveVal(0L)

  observe({
    v1 <- input$poly_show_labels
    v2 <- input$poly_show_projonly
    v3 <- input$poly_label_size
    v4 <- input$poly_label_chars
    v5 <- input$poly_point_size
    isolate(poly_viz_trigger(poly_viz_trigger() + 1L))
  })

  # ============================================================
  # REACTIVE: Compute polynomial projection
  # ============================================================

  poly_result <- reactive({
    req(cache$ca_result)
    ca_obj <- cache$ca_result

    # Get active site coordinates
    req(!is.null(ca_obj$row) && !is.null(ca_obj$row$coord))
    active_site_coords <- ca_obj$row$coord
    req(nrow(active_site_coords) >= 3)

    # Active type (column) coordinates
    active_type_coords <- if (!is.null(ca_obj$col) && !is.null(ca_obj$col$coord))
      ca_obj$col$coord else NULL

    # ── Projection options ──────────────────────────────────────
    fit_target <- input$poly_fit_on %||% "sites"

    if (fit_target == "sites") {
      fit_dim1 <- active_site_coords[, 1]
      fit_dim2 <- active_site_coords[, 2]
    } else if (fit_target == "types") {
      req(!is.null(active_type_coords))
      req(nrow(active_type_coords) >= 3)
      fit_dim1 <- active_type_coords[, 1]
      fit_dim2 <- active_type_coords[, 2]
    } else {                                          # "both"
      req(!is.null(active_type_coords))
      fit_dim1 <- c(active_site_coords[, 1], active_type_coords[, 1])
      fit_dim2 <- c(active_site_coords[, 2], active_type_coords[, 2])
    }
    req(length(fit_dim1) >= 3)

    degree <- 2L
    poly_coef <- tryCatch(
      fit_polynomial(fit_dim1, fit_dim2, degree),
      error = function(e) NULL
    )
    req(!is.null(poly_coef))

    # ── Curve range: cover all displayed points ──────────────────
    all_dim1 <- active_site_coords[, 1]
    if (!is.null(active_type_coords)) all_dim1 <- c(all_dim1, active_type_coords[, 1])

    x_min <- min(all_dim1) - 0.05 * diff(range(all_dim1))
    x_max <- max(all_dim1) + 0.05 * diff(range(all_dim1))
    x_range <- c(x_min, x_max)

    # Dense curve for visualization
    curve_xs <- seq(x_min, x_max, length.out = 500)
    curve_ys <- eval_poly(poly_coef, curve_xs)

    total_len <- total_arc_length(poly_coef, x_range)

    # ── Project active sites ─────────────────────────────────────
    active_sites <- project_group(
      active_site_coords[, 1], active_site_coords[, 2],
      rownames(active_site_coords), poly_coef, x_range, x_min, total_len
    )
    active_sites$SiteType <- "active"

    # ── Project supplementary sites ──────────────────────────────
    supp_sites <- NULL
    if (!is.null(ca_obj$row.sup) && !is.null(ca_obj$row.sup$coord)) {
      sc <- ca_obj$row.sup$coord
      supp_sites <- project_group(
        sc[, 1], sc[, 2], rownames(sc), poly_coef, x_range, x_min, total_len
      )
      supp_sites$SiteType <- "supplementary"
    }

    sites_df <- if (!is.null(supp_sites)) rbind(active_sites, supp_sites) else active_sites
    names(sites_df)[names(sites_df) == "Name"]     <- "Site"
    names(sites_df)[names(sites_df) == "SiteType"] <- "Type"

    # Add ranks (sites only; types do not have a seriation rank)
    sites_df$Dim1Rank <- rank(sites_df$Dim1,       ties.method = "min")
    sites_df$ALRank   <- rank(sites_df$ArcLength,  ties.method = "min")
    sites_df$RankDiff <- abs(sites_df$Dim1Rank - sites_df$ALRank)

    # ── Project active types ─────────────────────────────────────
    types_df <- NULL
    if (!is.null(active_type_coords)) {
      types_df <- project_group(
        active_type_coords[, 1], active_type_coords[, 2],
        rownames(active_type_coords), poly_coef, x_range, x_min, total_len
      )
      types_df$TypeCat <- "active"
    }

    # ── Project supplementary types ──────────────────────────────
    supp_types_df <- NULL
    if (!is.null(ca_obj$col.sup) && !is.null(ca_obj$col.sup$coord)) {
      stc <- ca_obj$col.sup$coord
      supp_types_df <- project_group(
        stc[, 1], stc[, 2], rownames(stc), poly_coef, x_range, x_min, total_len
      )
      supp_types_df$TypeCat <- "supplementary"
    }

    # ── Write to cache so battleship / clustering can use it ─────
    cache$polynomial_result <- list(
      arc_length       = sites_df[, c("Site", "ArcLength")],
      arc_length_types = if (!is.null(types_df)) types_df[, c("Name", "ArcLength")] else NULL,
      poly_coef   = poly_coef,
      poly_degree = degree,
      curve_x     = curve_xs,
      curve_y     = curve_ys,
      x_range     = x_range,
      total_len   = total_len
    )

    list(
      sites_df      = sites_df,
      types_df      = types_df,
      supp_types_df = supp_types_df,
      curve_xs      = curve_xs,
      curve_ys      = curve_ys,
      poly_coef     = poly_coef,
      x_range       = x_range,
      fit_target    = fit_target
    )
  })

  # ============================================================
  # OUTPUT: CA Biplot with polynomial curve
  # ============================================================

  output$poly_biplot <- plotly::renderPlotly({
    req(poly_result())
    res <- poly_result()
    df  <- res$sites_df

    active_df <- df[df$Type == "active", ]
    supp_df   <- df[df$Type == "supplementary", ]

    fit_target   <- res$fit_target
    show_passive <- isTRUE(input$poly_show_passive %||% TRUE)

    # Which group is passive (projected but not fitted on)?
    # "sites" → types are passive
    # "types" → sites are passive
    # "both"  → no passive group
    sites_are_passive <- (fit_target == "types")
    types_are_passive <- (fit_target == "sites")

    show_sites <- !(sites_are_passive && !show_passive)
    show_types <- !(types_are_passive && !show_passive)

    # Display-Einstellungen – via Trigger-Counter (immer aktiv via observe)
    use_group_color <- isTRUE(input$ca_color_by_group)
    poly_viz_trigger()
    show_labels   <- isTRUE(input$poly_show_labels   %||% TRUE)
    show_projonly <- isTRUE(input$poly_show_projonly  %||% FALSE)
    lbl_size      <- as.numeric(input$poly_label_size  %||% 11)
    lbl_chars     <- as.integer(input$poly_label_chars %||% 12)
    pt_size       <- as.numeric(input$poly_point_size  %||% 3) * 3

    # Truncate labels
    truncate_label <- function(x) substr(x, 1, lbl_chars)

    # Helper: bedingt Text-Attribute in Trace aufnehmen.
    # Gibt leere Liste zurück wenn Labels aus → verhindert, dass Plotly.react()
    # alte textposition/-font im Hot-Update stehen lässt (bekannter Plotly-Bug).
    txt <- function(labels, pos = "top center", col = NULL) {
      if (!show_labels) return(list())
      f <- list(size = lbl_size)
      if (!is.null(col)) f$color <- col
      list(text = truncate_label(labels), textposition = pos, textfont = f)
    }

    # Site colors
    get_color <- function(site_name) {
      if (use_group_color && !is.null(get_site_group)) {
        grp <- tryCatch(get_site_group(site_name), error = function(e) NULL)
        if (is.list(grp) && !is.null(grp$color)) return(as.character(grp$color))
      }
      "#3498db"
    }
    active_colors <- vapply(active_df$Site, get_color, character(1))

    # ── Label-Positionen: außerhalb der Parabel ───────────────────
    # Vergleiche originale Dim2-Position jedes Punktes mit dem Kurvenwert
    # an derselben Dim1-Position. Punkte ÜBER der Kurve → Label oben,
    # Punkte DARUNTER → Label unten. Gilt auch im projonly-Modus
    # (projizierte Punkte liegen auf der Kurve, aber der originale
    # Abstand bestimmt die "Außenseite").
    curve_y_active   <- eval_poly(res$poly_coef, active_df$Dim1)
    active_text_pos  <- ifelse(active_df$Dim2 >= curve_y_active,
                               "top center", "bottom center")

    supp_text_pos <- if (nrow(supp_df) > 0) {
      curve_y_supp <- eval_poly(res$poly_coef, supp_df$Dim1)
      ifelse(supp_df$Dim2 >= curve_y_supp, "top center", "bottom center")
    } else "top center"

    site_mode <- if (show_labels) "markers+text" else "markers"
    type_mode <- if (show_labels) "markers+text" else "markers"

    p <- plotly::plot_ly()

    # ── Polynomial curve line ────────────────────────────────────
    p <- plotly::add_trace(p,
      x = res$curve_xs, y = res$curve_ys,
      type = "scatter", mode = "lines",
      line = list(color = "#e74c3c", width = 2, dash = "solid"),
      name = "Polynom (Grad 2)",
      hoverinfo = "none"
    )

    # ── Projection lines for sites (nur im normalen Modus) ───────
    if (isTRUE(input$poly_show_projlines) && !show_projonly &&
        show_sites && nrow(active_df) > 0) {
      for (i in seq_len(nrow(active_df))) {
        p <- plotly::add_trace(p,
          x = c(active_df$Dim1[i], active_df$ProjX[i]),
          y = c(active_df$Dim2[i], active_df$ProjY[i]),
          type = "scatter", mode = "lines",
          line = list(color = "rgba(150,150,150,0.4)", width = 1),
          showlegend = FALSE, hoverinfo = "none"
        )
      }
    }

    # ── Projection lines for types (nur im normalen Modus) ───────
    if (isTRUE(input$poly_show_projlines) && !show_projonly &&
        show_types && !is.null(res$types_df)) {
      tdf_lines <- res$types_df
      for (i in seq_len(nrow(tdf_lines))) {
        p <- plotly::add_trace(p,
          x = c(tdf_lines$Dim1[i], tdf_lines$ProjX[i]),
          y = c(tdf_lines$Dim2[i], tdf_lines$ProjY[i]),
          type = "scatter", mode = "lines",
          line = list(color = "rgba(230,126,34,0.3)", width = 1),
          showlegend = FALSE, hoverinfo = "none"
        )
      }
    }

    # ── Projected site points on curve (Dreiecke) ────────────────
    # Projonly-Modus: größer (1.3×), volle Deckkraft, Labels wie Kreise
    # Normaler Modus: kleiner (0.8×), halbtransparent, kein Label-Text
    if (show_sites && nrow(active_df) > 0) {
      if (show_projonly) {
        p <- do.call(plotly::add_trace, c(list(p,
          x          = active_df$ProjX, y = active_df$ProjY,
          type       = "scatter", mode = site_mode,
          marker     = list(symbol = "triangle-up", size = pt_size * 1.3,
                            color = active_colors, opacity = 1.0,
                            line = list(color = "white", width = 1)),
          customdata = paste0("<b>", active_df$Site, "</b><br>Bogenlänge: ",
                              round(active_df$ArcLength, 3),
                              "<br>Rang (AL): ", active_df$ALRank),
          hovertemplate = "%{customdata}<extra></extra>",
          name       = "Projektion auf Kurve",
          showlegend = TRUE), txt(active_df$Site, pos = active_text_pos)))
      } else {
        p <- plotly::add_trace(p,
          x         = active_df$ProjX, y = active_df$ProjY,
          type      = "scatter", mode = "markers",
          marker    = list(symbol = "triangle-up", size = pt_size * 0.8,
                           color = active_colors, opacity = 0.65,
                           line = list(color = "white", width = 1)),
          text      = paste0(active_df$Site,
                             "<br>Bogenlänge: ", round(active_df$ArcLength, 3),
                             "<br>Rang (AL): ", active_df$ALRank),
          hoverinfo = "text",
          name      = "Projektion auf Kurve",
          showlegend = TRUE
        )
      }
    }

    # ── Active sites: Kreise (ausgeblendet in projonly-Modus) ────
    if (!show_projonly && show_sites && nrow(active_df) > 0) {
      p <- do.call(plotly::add_trace, c(list(p,
        x = active_df$Dim1, y = active_df$Dim2,
        type = "scatter", mode = site_mode,
        marker = list(size = pt_size, color = active_colors,
                      line = list(color = "white", width = 1)),
        customdata = paste0("<b>", active_df$Site, "</b><br>Dim1: ",
                            round(active_df$Dim1, 3),
                            "<br>Dim2: ", round(active_df$Dim2, 3),
                            "<br>Bogenlänge: ", round(active_df$ArcLength, 3)),
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Sites (aktiv)",
        showlegend = TRUE), txt(active_df$Site, pos = active_text_pos)))
    }

    # ── Supplementary sites ───────────────────────────────────────
    # Projonly: offene Dreiecke auf Kurve; Normal: offene Kreise an CA-Pos.
    if (show_sites && nrow(supp_df) > 0) {
      if (show_projonly) {
        p <- do.call(plotly::add_trace, c(list(p,
          x          = supp_df$ProjX, y = supp_df$ProjY,
          type       = "scatter", mode = site_mode,
          marker     = list(symbol = "triangle-up-open", size = pt_size * 1.1,
                            color = "#95a5a6", opacity = 0.9,
                            line = list(color = "#95a5a6", width = 2)),
          customdata = paste0("<b>", supp_df$Site, "</b><br>Bogenlänge: ",
                              round(supp_df$ArcLength, 3),
                              "<br>Rang (AL): ", supp_df$ALRank),
          hovertemplate = "%{customdata}<extra></extra>",
          name       = "Sites (supplementary)",
          showlegend = TRUE), txt(supp_df$Site, pos = supp_text_pos, col = "#95a5a6")))
      } else {
        p <- do.call(plotly::add_trace, c(list(p,
          x = supp_df$Dim1, y = supp_df$Dim2,
          type = "scatter", mode = site_mode,
          marker = list(size = pt_size, symbol = "circle-open", color = "#95a5a6",
                        line = list(color = "#95a5a6", width = 2)),
          customdata = paste0("<b>", supp_df$Site, "</b><br>Dim1: ",
                              round(supp_df$Dim1, 3),
                              "<br>Dim2: ", round(supp_df$Dim2, 3),
                              "<br>Bogenlänge: ", round(supp_df$ArcLength, 3)),
          hovertemplate = "%{customdata}<extra></extra>",
          name = "Sites (supplementary)",
          showlegend = TRUE), txt(supp_df$Site, pos = supp_text_pos, col = "#95a5a6")))
      }
    }

    # ── Active types (diamonds) ──────────────────────────────────
    if (show_types && !is.null(res$types_df)) {
      tdf <- res$types_df
      curve_y_types  <- eval_poly(res$poly_coef, tdf$Dim1)
      types_text_pos <- ifelse(tdf$Dim2 >= curve_y_types, "top center", "bottom center")
      p <- do.call(plotly::add_trace, c(list(p,
        x = tdf$Dim1, y = tdf$Dim2,
        type = "scatter", mode = type_mode,
        marker = list(size = pt_size * 0.85, symbol = "diamond",
                      color = "#e67e22",
                      line = list(color = "white", width = 1)),
        customdata = paste0("<b>", tdf$Name, "</b><br>Dim1: ",
                            round(tdf$Dim1, 3),
                            "<br>Dim2: ", round(tdf$Dim2, 3),
                            "<br>Bogenlänge: ", round(tdf$ArcLength, 3)),
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Types (aktiv)",
        showlegend = TRUE), txt(tdf$Name, pos = types_text_pos, col = "#e67e22")))
    }

    # ── Supplementary types (open diamonds) ──────────────────────
    if (show_types && !is.null(res$supp_types_df)) {
      stdf <- res$supp_types_df
      curve_y_stypes  <- eval_poly(res$poly_coef, stdf$Dim1)
      stypes_text_pos <- ifelse(stdf$Dim2 >= curve_y_stypes, "top center", "bottom center")
      p <- do.call(plotly::add_trace, c(list(p,
        x = stdf$Dim1, y = stdf$Dim2,
        type = "scatter", mode = type_mode,
        marker = list(size = pt_size * 0.85, symbol = "diamond-open",
                      color = "#f39c12",
                      line = list(color = "#f39c12", width = 2)),
        customdata = paste0("<b>", stdf$Name, "</b><br>Dim1: ",
                            round(stdf$Dim1, 3),
                            "<br>Dim2: ", round(stdf$Dim2, 3),
                            "<br>Bogenlänge: ", round(stdf$ArcLength, 3)),
        hovertemplate = "%{customdata}<extra></extra>",
        name = "Types (supplementary)",
        showlegend = TRUE), txt(stdf$Name, pos = stypes_text_pos, col = "#f39c12")))
    }

    p <- plotly::layout(p,
      xaxis = list(title = "Dim 1", zeroline = TRUE, zerolinecolor = "#ddd"),
      yaxis = list(title = "Dim 2", zeroline = TRUE, zerolinecolor = "#ddd",
                   scaleanchor = "x", scaleratio = 1),
      legend = list(orientation = "h", y = -0.15),
      hovermode = "closest",
      margin = list(t = 30, b = 80),
      dragmode = "pan"
    )

    plotly::config(p,
      displayModeBar = TRUE,
      scrollZoom = TRUE,
      modeBarButtonsToRemove = c("lasso2d", "select2d"),
      displaylogo = FALSE
    )
  })

  # ============================================================
  # OUTPUT: Rank comparison table (sites only)
  # ============================================================

  output$poly_order_table <- DT::renderDataTable({
    req(poly_result())
    df <- poly_result()$sites_df

    display_df <- data.frame(
      Site           = df$Site,
      Typ            = df$Type,
      Dim1           = round(df$Dim1, 4),
      `Bogenlänge`   = round(df$ArcLength, 4),
      `Rang (Dim1)`  = df$Dim1Rank,
      `Rang (AL)`    = df$ALRank,
      `|Differenz|`  = df$RankDiff,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )

    # Sort by arc length by default
    display_df <- display_df[order(display_df[["Rang (AL)"]]), ]

    DT::datatable(
      display_df,
      options = list(
        paging      = FALSE,
        scrollX     = TRUE,
        scrollY     = "380px",
        scrollCollapse = TRUE,
        fixedHeader = TRUE,
        order       = list(list(6, "desc")),  # Sort by |Differenz| descending
        columnDefs  = list(list(className = "dt-center", targets = 2:6)),
        dom         = "ft"  # nur Filter + Tabelle, kein Paging-Control
      ),
      rownames = FALSE,
      class = "display compact"
    ) |>
      DT::formatStyle(
        "|Differenz|",
        background = DT::styleColorBar(c(0, max(df$RankDiff, 1)), "#fadbd8"),
        backgroundSize = "100% 80%",
        backgroundRepeat = "no-repeat",
        backgroundPosition = "center"
      )
  })

  # ============================================================
  # OUTPUT: Download CSV
  # ============================================================

  output$download_poly_scores <- downloadHandler(
    filename = function() {
      paste0("polynomial_projection_", format(Sys.Date(), "%Y-%m-%d"), ".csv")
    },
    content = function(file) {
      req(poly_result())
      df <- poly_result()$sites_df

      export_df <- data.frame(
        Site         = df$Site,
        Type         = df$Type,
        Dim1         = round(df$Dim1, 6),
        Dim2         = round(df$Dim2, 6),
        ProjX_curve  = round(df$ProjX, 6),
        ArcLength    = round(df$ArcLength, 6),
        Rank_Dim1    = df$Dim1Rank,
        Rank_ArcLength = df$ALRank,
        Rank_Diff    = df$RankDiff,
        stringsAsFactors = FALSE
      )
      export_df <- export_df[order(export_df$Rank_ArcLength), ]
      write.csv(export_df, file, row.names = FALSE)
    }
  )

  # ============================================================
  # Flip-Button: tauscht Datum-links ↔ Datum-rechts
  # ============================================================
  observeEvent(input$poly_ts_flip, {
    l <- input$poly_ts_date_left
    r <- input$poly_ts_date_right
    updateNumericInput(session, "poly_ts_date_left",  value = r)
    updateNumericInput(session, "poly_ts_date_right", value = l)
  })

  # ============================================================
  # OUTPUT: Ankerpunkt-Info-UI (kalibrierte ¹⁴C-Stützstellen)
  # ============================================================
  output$poly_anchor_info_ui <- renderUI({

    anchor_source <- input$poly_ts_anchor_source %||% "unmodelled"

    # ── Daten je nach Quelle laden & aggregieren ───────────────────────────
    if (anchor_source == "modelled") {
      # OxCal-Posterior-Mediane
      if (is.null(oxcal_results)) {
        return(div(class = "alert alert-warning",
                   style = "font-size: 0.8em; padding: 4px 8px; margin-top: 4px;",
          tags$span(class = "glyphicon glyphicon-warning-sign",
                    style = "margin-right: 4px;"),
          tr("ca.poly.ts.anchors.oxcal.none")
        ))
      }
      oxcal_data <- tryCatch(oxcal_results(), error = function(e) NULL)
      agg <- aggregate_oxcal_anchor(oxcal_data)
      if (is.null(agg) || nrow(agg) == 0) {
        return(div(class = "alert alert-warning",
                   style = "font-size: 0.8em; padding: 4px 8px; margin-top: 4px;",
          tags$span(class = "glyphicon glyphicon-warning-sign",
                    style = "margin-right: 4px;"),
          tr("ca.poly.ts.anchors.oxcal.none")
        ))
      }
    } else {
      # Unmodelliert: IntCal20-Kalibrierung aus Chronologie-Tab
      if (is.null(c14_calibrated)) {
        return(div(class = "alert alert-warning",
                   style = "font-size: 0.8em; padding: 4px 8px; margin-top: 4px;",
          tags$span(class = "glyphicon glyphicon-warning-sign",
                    style = "margin-right: 4px;"),
          tr("ca.poly.ts.anchors.none")
        ))
      }
      cal_df <- tryCatch(c14_calibrated(), error = function(e) NULL)
      if (is.null(cal_df) || nrow(cal_df) == 0 ||
          !all(c("Site", "cal_median_BP") %in% names(cal_df))) {
        return(div(class = "alert alert-warning",
                   style = "font-size: 0.8em; padding: 4px 8px; margin-top: 4px;",
          tags$span(class = "glyphicon glyphicon-warning-sign",
                    style = "margin-right: 4px;"),
          tr("ca.poly.ts.anchors.none")
        ))
      }
      # SPD-Median (aus age_grid/density, korrekte Aggregation bei mehreren Daten)
      agg <- aggregate_c14_by_site(cal_df)
    }

    # Poly-Ergebnis nötig für ArcLength-Normierung
    poly_res <- tryCatch(poly_result(), error = function(e) NULL)
    if (is.null(poly_res)) return(NULL)

    # Join mit Poly-Sites (ArcLength → normieren)
    all_sites <- poly_res$sites_df
    al_min    <- min(all_sites$ArcLength, na.rm = TRUE)
    al_max    <- max(all_sites$ArcLength, na.rm = TRUE)
    al_rng    <- max(al_max - al_min, .Machine$double.eps)

    merged <- merge(all_sites[, c("Site", "ArcLength")], agg, by = "Site")
    if (nrow(merged) == 0) {
      return(div(class = "alert alert-warning",
                 style = "font-size: 0.8em; padding: 4px 8px; margin-top: 4px;",
        tags$span(class = "glyphicon glyphicon-warning-sign",
                  style = "margin-right: 4px;"),
        tr("ca.poly.ts.anchors.none")
      ))
    }
    merged$ArcLengthNorm <- (merged$ArcLength - al_min) / al_rng
    merged <- merged[order(merged$ArcLengthNorm), ]
    n_anch <- nrow(merged)

    tagList(
      div(class = "alert alert-success",
          style = "font-size: 0.8em; padding: 4px 8px; margin-top: 6px; margin-bottom: 4px;",
        tags$span(class = "glyphicon glyphicon-map-marker",
                  style = "margin-right: 4px;"),
        paste0(n_anch, " ", tr("ca.poly.ts.anchors.info"))
      ),
      div(style = "font-size: 0.78em; max-height: 160px; overflow-y: auto; margin-bottom: 4px;",
        tags$table(class = "table table-condensed table-striped",
          style = "margin-bottom: 0;",
          tags$thead(
            tags$tr(
              tags$th(style = "padding: 2px 4px;", tr("ca.poly.ts.anchors.col.site")),
              tags$th(style = "padding: 2px 4px;", tr("ca.poly.ts.anchors.col.date")),
              tags$th(style = "padding: 2px 4px;", tr("ca.poly.ts.anchors.col.n"))
            )
          ),
          tags$tbody(
            lapply(seq_len(n_anch), function(i) {
              tag_lbl <- if (anchor_source == "modelled") {
                tags$span(" OxCal", style = "color:#31708f; font-size:0.85em;")
              } else if (merged$n_dates[i] > 1) {
                tags$span(" SPD",   style = "color:#888;    font-size:0.85em;")
              } else {
                NULL
              }
              date_cell <- tagList(
                paste0(round(merged$cal_median_BP[i]), " cal BP"),
                tag_lbl
              )
              tags$tr(
                tags$td(style = "padding: 2px 4px;", merged$Site[i]),
                tags$td(style = "padding: 2px 4px;", date_cell),
                tags$td(style = "padding: 2px 4px;", merged$n_dates[i])
              )
            })
          )
        )
      ),
      div(class = "alert alert-info",
          style = "font-size: 0.76em; padding: 3px 6px; margin: 2px 0;",
        tags$span(class = "glyphicon glyphicon-transfer",
                  style = "margin-right: 3px;"),
        tr("ca.poly.ts.anchors.extrap.warn")
      )
    )
  })

  # ============================================================
  # OUTPUT: Zeitstrahl – Dim1 (normiert) vs. Bogenlänge [0,1]
  # ============================================================

  output$poly_timeline <- plotly::renderPlotly({
    req(poly_result())
    res <- poly_result()
    df  <- res$sites_df

    poly_viz_trigger()
    show_labels <- isTRUE(input$poly_show_labels   %||% TRUE)
    lbl_chars   <- as.integer(input$poly_label_chars %||% 12)
    lbl_size    <- as.numeric(input$poly_label_size  %||% 11)
    pt_size     <- as.numeric(input$poly_point_size  %||% 3) * 3
    trunc_lbl   <- function(x) substr(x, 1, lbl_chars)
    pt_mode     <- if (show_labels) "markers+text" else "markers"

    # Helper: bedingt Text-Attribute hinzufügen (s. poly_biplot für Erklärung)
    txt <- function(labels, pos = "top center", col = NULL) {
      if (!show_labels) return(list())
      f <- list(size = lbl_size)
      if (!is.null(col)) f$color <- col
      list(text = trunc_lbl(labels), textposition = pos, textfont = f)
    }

    # Normiere Dim1 → [0,1]  (min-max auf Basis der tatsächlichen Sites)
    d1_min <- min(df$Dim1, na.rm = TRUE)
    d1_max <- max(df$Dim1, na.rm = TRUE)
    d1_rng <- max(d1_max - d1_min, .Machine$double.eps)
    df$Dim1Norm <- (df$Dim1 - d1_min) / d1_rng

    # Normiere ArcLength → [0,1] auf Basis der tatsächlichen Site-Werte.
    # Die ArcLength-Werte im Modul sind relativ zur GESAMTEN Kurve normiert
    # (inkl. 5%-Überstand über die Sites hinaus). Bei steiler Parabel startet
    # der erste Site dadurch bei ArcLength > 0. Für den Zeitstrahl-Vergleich
    # normieren wir beide Skalen auf denselben [0,1]-Raum der Site-Wertemenge.
    al_min <- min(df$ArcLength, na.rm = TRUE)
    al_max <- max(df$ArcLength, na.rm = TRUE)
    al_rng <- max(al_max - al_min, .Machine$double.eps)
    df$ArcLengthNorm <- (df$ArcLength - al_min) / al_rng

    # ── Zeitskala-Mapping (optional) ──────────────────────────────────────
    use_ts   <- isTRUE(input$poly_timescale_on)
    use_bcad <- isTRUE(input$poly_ts_bcad)
    ts_left  <- if (use_ts) as.numeric(input$poly_ts_date_left  %||% 3950) else NA_real_
    ts_right <- if (use_ts) as.numeric(input$poly_ts_date_right %||%  455) else NA_real_
    ts_unit  <- if (use_ts && !use_bcad) trimws(as.character(input$poly_ts_unit %||% "BP")) else ""

    # ── ¹⁴C-Ankerpunkt-Interpolation (stückweise linear) ──────────────────
    # Aktiviert wenn: Zeitskala AN + Ankerpunkte AN + mind. eine Datenquelle verfügbar
    use_anchors   <- isTRUE(input$poly_ts_anchors_on) && use_ts &&
                     (!is.null(c14_calibrated) || !is.null(oxcal_results))
    anchor_source <- input$poly_ts_anchor_source %||% "unmodelled"
    anch_map      <- NULL

    if (use_anchors) {
      agg <- NULL

      if (anchor_source == "modelled" && !is.null(oxcal_results)) {
        # OxCal-modellierte Posterior-Mediane
        oxcal_data <- tryCatch(oxcal_results(), error = function(e) NULL)
        agg        <- aggregate_oxcal_anchor(oxcal_data)

      } else if (!is.null(c14_calibrated)) {
        # Unmodelliert: SPD-Median aus IntCal20-Kalibrierung
        cal_df <- tryCatch(c14_calibrated(), error = function(e) NULL)
        if (!is.null(cal_df) && nrow(cal_df) > 0 &&
            all(c("Site", "cal_median_BP") %in% names(cal_df))) {
          agg <- aggregate_c14_by_site(cal_df)[, c("Site", "cal_median_BP")]
        }
      }

      if (!is.null(agg) && nrow(agg) >= 1 &&
          all(c("Site", "cal_median_BP") %in% names(agg))) {
        # Join mit df (hat ArcLengthNorm bereits berechnet)
        merged <- merge(df[, c("Site", "ArcLengthNorm")],
                        agg[, c("Site", "cal_median_BP")], by = "Site")
        if (nrow(merged) >= 2) {
          # Datum in der gewählten Einheit umrechnen
          merged$date_val <- if (use_bcad) {
            1950 - merged$cal_median_BP   # year CE: negativ = BC, positiv = AD
          } else {
            merged$cal_median_BP          # cal BP direkt
          }
          anch_map <- merged[order(merged$ArcLengthNorm), ]
        }
      }
    }

    # Mapping-Funktion: normierte Position [0,1] → Datumswert
    # Ankerpunkt-basiert (stückweise linear) oder global linear
    norm_to_date <- if (!is.null(anch_map)) {
      function(x) {
        stats::approx(
          x      = anch_map$ArcLengthNorm,
          y      = anch_map$date_val,
          xout   = x,
          rule   = 2,       # Randwert-Extrapolation für Sites außerhalb
          method = "linear"
        )$y
      }
    } else {
      function(x) ts_left + x * (ts_right - ts_left)
    }

    # Formatter: Datumswert → lesbarer String (BP oder BC/AD)
    fmt_date_val <- function(x) {
      xr <- round(x)
      if (use_bcad) {
        ifelse(xr < 0,  paste0(abs(xr), "\u00a0BC"),
        ifelse(xr == 0, "0",
                        paste0(xr, "\u00a0AD")))
      } else {
        paste0(xr, "\u00a0", ts_unit)
      }
    }

    # Aktive Datumsskala: linear ODER ankerbasiert
    has_date_scale <- (!is.null(anch_map)) || (use_ts && !anyNA(c(ts_left, ts_right)))

    # x-Achsen-Titel
    xaxis_title <- if (!has_date_scale) {
      tr("ca.poly.timeline.xaxis")
    } else if (use_bcad) {
      "BC / AD"
    } else if (nchar(ts_unit) > 0) {
      ts_unit
    } else {
      tr("ca.poly.timeline.xaxis")
    }

    # x-Achse: bei Zeitskala werden Tick-Positionen mit Datumslabels versehen
    tick_pos <- seq(0, 1, by = 0.1)
    if (has_date_scale) {
      xaxis_def <- list(
        title      = xaxis_title,
        range      = c(-0.03, 1.03),
        zeroline   = FALSE,
        showgrid   = TRUE,
        tickvals   = tick_pos,
        ticktext   = fmt_date_val(norm_to_date(tick_pos))
      )
    } else {
      xaxis_def <- list(
        title      = tr("ca.poly.timeline.xaxis"),
        range      = c(-0.03, 1.03),
        zeroline   = FALSE,
        showgrid   = TRUE,
        tickformat = ".2f",
        tickvals   = tick_pos
      )
    }

    max_diff <- max(df$RankDiff, 1L)

    # Farbskala: RankDiff → grau (0) bis rot (max)
    make_drift_color <- function(rd) {
      t <- rd / max_diff
      r <- as.integer(160 + 95 * t)
      g <- as.integer(160 - 160 * t)
      b <- as.integer(160 - 160 * t)
      sprintf("rgb(%d,%d,%d)", r, g, b)
    }

    p <- plotly::plot_ly()

    # Verbindungslinien (nur wenn Toggle aktiviert)
    if (isTRUE(input$poly_show_timeline_lines)) {
      for (i in seq_len(nrow(df))) {
        p <- plotly::add_trace(p,
          x = c(df$Dim1Norm[i], df$ArcLengthNorm[i]),
          y = c(2L, 1L),
          type = "scatter", mode = "lines",
          line = list(
            color = make_drift_color(df$RankDiff[i]),
            width = 1 + df$RankDiff[i] / max_diff * 2
          ),
          showlegend = FALSE, hoverinfo = "none"
        )
      }
    }

    # Punkte: Dim1-Reihe (y = 2)
    date_suffix_d1 <- if (has_date_scale) {
      paste0("<br>≈\u00a0", fmt_date_val(norm_to_date(df$Dim1Norm)))
    } else rep("", nrow(df))

    p <- do.call(plotly::add_trace, c(list(p,
      x    = df$Dim1Norm,
      y    = rep(2L, nrow(df)),
      type = "scatter", mode = pt_mode,
      marker = list(
        size   = pt_size,
        color  = "#3498db",
        line   = list(color = "white", width = 1)
      ),
      name = tr("ca.poly.timeline.row.dim1"),
      hovertemplate = paste0(
        "<b>", df$Site, "</b><br>",
        tr("ca.poly.timeline.row.dim1"), ": %{x:.3f}<br>",
        "Dim1: ", round(df$Dim1, 3), "<br>",
        "Rang: ", df$Dim1Rank,
        date_suffix_d1,
        "<extra></extra>"
      )), txt(df$Site)))

    # Punkte: Bogenlängen-Reihe (y = 1)
    date_suffix_al <- if (has_date_scale) {
      paste0("<br>≈\u00a0", fmt_date_val(norm_to_date(df$ArcLengthNorm)))
    } else rep("", nrow(df))

    p <- do.call(plotly::add_trace, c(list(p,
      x    = df$ArcLengthNorm,
      y    = rep(1L, nrow(df)),
      type = "scatter", mode = pt_mode,
      marker = list(
        size   = pt_size,
        color  = "#e74c3c",
        line   = list(color = "white", width = 1)
      ),
      name = tr("ca.poly.timeline.row.arclength"),
      hovertemplate = paste0(
        "<b>", df$Site, "</b><br>",
        tr("ca.poly.timeline.row.arclength"), ": ", round(df$ArcLength, 3), "<br>",
        "Rang: ", df$ALRank,
        date_suffix_al,
        "<extra></extra>"
      )), txt(df$Site, pos = "bottom center")))

    # Optional: Types auf beiden Reihen
    if (isTRUE(input$poly_show_types_timeline) && !is.null(res$types_df)) {
      tdf <- res$types_df
      tdf$Dim1Norm <- (tdf$Dim1 - d1_min) / d1_rng
      tdf$Dim1Norm <- pmax(0, pmin(1, tdf$Dim1Norm))
      if ("ArcLength" %in% names(tdf)) {
        tdf$ArcLengthNorm <- (tdf$ArcLength - al_min) / al_rng
        tdf$ArcLengthNorm <- pmax(0, pmin(1, tdf$ArcLengthNorm))
      }

      # Types auf Dim1-Reihe
      p <- plotly::add_trace(p,
        x    = tdf$Dim1Norm,
        y    = rep(2L, nrow(tdf)),
        type = "scatter", mode = "markers",
        marker = list(
          size   = pt_size * 0.8,
          symbol = "diamond",
          color  = "#e67e22",
          line   = list(color = "white", width = 1)
        ),
        name = paste0(tr("ca.poly.timeline.row.dim1"), " (Types)"),
        hovertemplate = paste0(
          "<b>%{text}</b><br>",
          tr("ca.poly.timeline.row.dim1"), ": %{x:.3f}<extra></extra>"
        ),
        text = trunc_lbl(tdf$Name)
      )

      # Types auf Bogenlängen-Reihe
      if ("ArcLength" %in% names(tdf)) {
        p <- plotly::add_trace(p,
          x    = tdf$ArcLengthNorm,
          y    = rep(1L, nrow(tdf)),
          type = "scatter", mode = "markers",
          marker = list(
            size   = pt_size * 0.8,
            symbol = "diamond",
            color  = "#e67e22",
            line   = list(color = "white", width = 1)
          ),
          name = paste0(tr("ca.poly.timeline.row.arclength"), " (Types)"),
          hovertemplate = paste0(
            "<b>%{text}</b><br>",
            tr("ca.poly.timeline.row.arclength"), ": %{x:.3f}<extra></extra>"
          ),
          text = trunc_lbl(tdf$Name),
          showlegend = FALSE
        )
      }
    }

    p <- plotly::layout(p,
      xaxis = xaxis_def,
      yaxis = list(
        tickvals     = c(1L, 2L),
        ticktext     = c(
          tr("ca.poly.timeline.row.arclength"),
          tr("ca.poly.timeline.row.dim1")
        ),
        range        = c(0.4, 2.8),
        showgrid     = FALSE,
        zeroline     = FALSE,
        fixedrange   = TRUE,
        tickfont     = list(size = 12, color = "#333")
      ),
      hovermode = "closest",
      margin    = list(t = 20, b = 70, l = 140, r = 20),
      showlegend = TRUE,
      legend    = list(orientation = "h", y = -0.18)
    )

    plotly::config(p,
      displayModeBar          = TRUE,
      scrollZoom              = FALSE,
      modeBarButtonsToRemove  = c("lasso2d", "select2d"),
      displaylogo             = FALSE
    )
  })

  # ============================================================
  # OUTPUT: Rangwechsel – Slope Graph (Dim1-Rang vs. AL-Rang)
  # ============================================================

  output$poly_bump_chart <- plotly::renderPlotly({
    req(poly_result())
    df <- poly_result()$sites_df
    n  <- nrow(df)

    poly_viz_trigger()
    show_labels <- isTRUE(input$poly_show_labels   %||% TRUE)
    lbl_chars   <- as.integer(input$poly_label_chars %||% 12)
    lbl_size    <- as.numeric(input$poly_label_size  %||% 11)
    pt_size     <- as.numeric(input$poly_point_size  %||% 3) * 3
    trunc_lbl   <- function(x) substr(x, 1, lbl_chars)

    # Color by |RankDiff|: grey (small) → red (large)
    max_diff <- max(df$RankDiff, 1L)
    pt_colors <- sapply(df$RankDiff, function(d) {
      t <- d / max_diff
      sprintf("rgb(%d,%d,%d)",
        as.integer(160 + 95 * t),
        as.integer(160 - 160 * t),
        as.integer(160 - 160 * t))
    })

    # Labels only for top outliers (all if n <= 30, else top 12 by RankDiff)
    n_label <- if (n <= 30) n else 12L
    thresh  <- sort(df$RankDiff, decreasing = TRUE)[min(n_label, n)]
    labels  <- ifelse(show_labels & df$RankDiff >= thresh & df$RankDiff > 0,
                      trunc_lbl(df$Site), "")

    has_labels <- any(nzchar(labels))
    pad <- n * 0.04  # small margin around axes

    p <- plotly::plot_ly() |>
      # ── Diagonal reference (= perfect agreement) ──────────────
      plotly::add_trace(
        x         = c(1 - pad, n + pad),
        y         = c(1 - pad, n + pad),
        type      = "scatter", mode = "lines",
        line      = list(color = "rgba(80,80,80,0.25)", width = 1.5, dash = "dot"),
        showlegend = FALSE, hoverinfo = "skip"
      ) |>
      # ── Data points ────────────────────────────────────────────
      plotly::add_trace(
        x             = df$Dim1Rank,
        y             = df$ALRank,
        type          = "scatter",
        mode          = if (has_labels) "markers+text" else "markers",
        marker        = list(size  = pt_size,
                             color = pt_colors,
                             line  = list(color = "white", width = 0.5)),
        text          = labels,
        textposition  = "top center",
        textfont      = list(size = lbl_size),
        hovertemplate = paste0(
          "<b>", df$Site, "</b><br>",
          tr("ca.poly.bump.xlab.dim1"), ": ", df$Dim1Rank, "<br>",
          tr("ca.poly.bump.xlab.al"),   ": ", df$ALRank,   "<br>",
          "|\u0394|: <b>", df$RankDiff, "</b>",
          "<extra></extra>"
        ),
        showlegend = FALSE
      )

    plotly::layout(p,
      xaxis = list(
        title     = tr("ca.poly.bump.xlab.dim1"),
        showgrid  = TRUE,
        gridcolor = "rgba(200,200,200,0.4)",
        zeroline  = FALSE,
        range     = c(0.5 - pad, n + 0.5 + pad)
      ),
      yaxis = list(
        title     = tr("ca.poly.bump.xlab.al"),
        showgrid  = TRUE,
        gridcolor = "rgba(200,200,200,0.4)",
        zeroline  = FALSE,
        range     = c(0.5 - pad, n + 0.5 + pad)
      ),
      annotations = list(list(
        text      = tr("ca.poly.scatter.diag.note"),
        x = 0.01, y = 1.0, xref = "paper", yref = "paper",
        xanchor = "left", yanchor = "top",
        showarrow = FALSE,
        font = list(size = 10, color = "rgba(100,100,100,0.8)")
      )),
      hovermode = "closest",
      margin    = list(t = 30, b = 60, l = 70, r = 20)
    ) |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  # ── Diagnose-Plot 1: Abstand zur Parabel pro Position ───────────────────────
  output$poly_diag_distance <- plotly::renderPlotly({
    req(poly_result())
    df <- poly_result()$sites_df
    req("DistToCurve" %in% names(df))

    show_labels <- isTRUE(input$poly_show_labels %||% TRUE)
    lbl_chars   <- as.integer(input$poly_label_chars %||% 12)
    lbl_size    <- as.numeric(input$poly_label_size  %||% 11)
    pt_size     <- as.numeric(input$poly_point_size  %||% 3) * 3
    trunc_lbl   <- function(x) substr(x, 1, lbl_chars)

    max_dist  <- max(df$DistToCurve, 1e-6)
    pt_colors <- sapply(df$DistToCurve, function(d) {
      t <- d / max_dist
      sprintf("rgb(%d,%d,%d)",
        as.integer(160 + 95 * t),
        as.integer(160 - 160 * t),
        as.integer(160 - 160 * t))
    })

    p <- plotly::plot_ly(
      x             = df$Dim1Rank,
      y             = df$DistToCurve,
      type          = "scatter",
      mode          = if (show_labels) "markers+text" else "markers",
      marker        = list(size = pt_size, color = pt_colors,
                           line = list(color = "white", width = 0.5)),
      text          = if (show_labels) trunc_lbl(df$Site) else NULL,
      textposition  = "top center",
      textfont      = list(size = lbl_size),
      hovertemplate = paste0(
        "<b>", df$Site, "</b><br>",
        tr("ca.poly.diag.dist.xlab"), ": ", df$Dim1Rank, "<br>",
        tr("ca.poly.diag.dist.ylab"), ": %{y:.3f}",
        "<extra></extra>"
      ),
      showlegend = FALSE
    )

    plotly::layout(p,
      xaxis     = list(title = tr("ca.poly.diag.dist.xlab"),
                       showgrid = TRUE, gridcolor = "rgba(200,200,200,0.4)", zeroline = FALSE),
      yaxis     = list(title = tr("ca.poly.diag.dist.ylab"),
                       rangemode = "tozero",
                       showgrid = TRUE, gridcolor = "rgba(200,200,200,0.4)", zeroline = FALSE),
      hovermode = "closest",
      margin    = list(t = 20, b = 50, l = 70, r = 20)
    ) |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

  # ── Diagnose-Plot 2: Rangverschiebung pro Position ───────────────────────────
  output$poly_diag_rankshift <- plotly::renderPlotly({
    req(poly_result())
    df <- poly_result()$sites_df

    show_labels <- isTRUE(input$poly_show_labels %||% TRUE)
    lbl_chars   <- as.integer(input$poly_label_chars %||% 12)
    lbl_size    <- as.numeric(input$poly_label_size  %||% 11)
    pt_size     <- as.numeric(input$poly_point_size  %||% 3) * 3
    trunc_lbl   <- function(x) substr(x, 1, lbl_chars)

    max_diff  <- max(df$RankDiff, 1L)
    pt_colors <- sapply(df$RankDiff, function(d) {
      t <- d / max_diff
      sprintf("rgb(%d,%d,%d)",
        as.integer(160 + 95 * t),
        as.integer(160 - 160 * t),
        as.integer(160 - 160 * t))
    })

    p <- plotly::plot_ly(
      x             = df$Dim1Rank,
      y             = df$RankDiff,
      type          = "scatter",
      mode          = if (show_labels) "markers+text" else "markers",
      marker        = list(size = pt_size, color = pt_colors,
                           line = list(color = "white", width = 0.5)),
      text          = if (show_labels) trunc_lbl(df$Site) else NULL,
      textposition  = "top center",
      textfont      = list(size = lbl_size),
      hovertemplate = paste0(
        "<b>", df$Site, "</b><br>",
        tr("ca.poly.diag.rankshift.xlab"), ": ", df$Dim1Rank, "<br>",
        tr("ca.poly.diag.rankshift.ylab"), ": %{y}",
        "<extra></extra>"
      ),
      showlegend = FALSE
    )

    plotly::layout(p,
      xaxis     = list(title = tr("ca.poly.diag.rankshift.xlab"),
                       showgrid = TRUE, gridcolor = "rgba(200,200,200,0.4)", zeroline = FALSE),
      yaxis     = list(title = tr("ca.poly.diag.rankshift.ylab"),
                       rangemode = "tozero", dtick = 1,
                       showgrid = TRUE, gridcolor = "rgba(200,200,200,0.4)", zeroline = FALSE),
      hovermode = "closest",
      margin    = list(t = 20, b = 50, l = 70, r = 20)
    ) |> plotly::config(displayModeBar = FALSE, displaylogo = FALSE)
  })

}

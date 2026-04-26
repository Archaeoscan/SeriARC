mod_group_validation_server <- function(
  filtered_data, ca_result, get_site_group,
  input, output, session, tr
) {

  # ===== DATA PREPARATION =====

  group_data <- reactive({
    req(filtered_data())
    mat   <- filtered_data()
    sites <- rownames(mat)
    grps  <- get_site_group(sites)
    df    <- data.frame(site = sites, group = grps, stringsAsFactors = FALSE)
    df[!is.na(df$group) & nchar(trimws(df$group)) > 0, ]
  })

  has_groups <- reactive({
    gd <- group_data()
    !is.null(gd) && nrow(gd) >= 4 && length(unique(gd$group)) >= 2
  })

  # ===== PERMANOVA =====

  permanova_result <- reactive({
    req(has_groups())
    mat     <- filtered_data()
    gd      <- group_data()
    mat_sub <- mat[gd$site, , drop = FALSE]
    mat_sub <- mat_sub[, colSums(mat_sub) > 0, drop = FALSE]
    if (ncol(mat_sub) < 2) return(NULL)
    mat_hell <- vegan::decostand(mat_sub, method = "hellinger")
    tryCatch(
      vegan::adonis2(mat_hell ~ group, data = gd, permutations = 999, method = "euclidean"),
      error = function(e) NULL
    )
  })

  # ===== SILHOUETTE =====

  silhouette_data <- reactive({
    req(has_groups(), ca_result())
    res   <- ca_result()
    gd    <- group_data()
    if (is.null(res$row$coord)) return(NULL)
    avail <- intersect(gd$site, rownames(res$row$coord))
    if (length(avail) < 4) return(NULL)
    gd     <- gd[gd$site %in% avail, ]
    n_dims <- min(4, ncol(res$row$coord))
    coords <- res$row$coord[gd$site, 1:n_dims, drop = FALSE]
    g_fac  <- factor(gd$group)
    g_int  <- as.integer(g_fac)
    if (length(unique(g_int)) < 2) return(NULL)
    sil <- cluster::silhouette(g_int, dist(coords))
    data.frame(
      site      = gd$site,
      group     = gd$group,
      sil_width = round(sil[, "sil_width"], 3),
      neighbor  = levels(g_fac)[sil[, "neighbor"]],
      stringsAsFactors = FALSE
    )
  })

  # ===== OUTPUT: OVERVIEW PANEL =====

  output$groupval_overview_ui <- renderUI({
    if (!has_groups()) {
      return(div(class = "alert alert-warning",
        tags$b(tr("groupval.no.groups")), br(),
        tr("groupval.no.groups.desc")
      ))
    }

    gd       <- group_data()
    perm     <- permanova_result()
    sil      <- silhouette_data()
    n_sites  <- nrow(gd)
    n_groups <- length(unique(gd$group))

    perm_block <- if (!is.null(perm)) {
      r2     <- round(perm$R2[1] * 100, 1)
      pv     <- perm$`Pr(>F)`[1]
      pv_txt <- if (!is.na(pv) && pv < 0.001) "< 0.001" else as.character(round(pv, 3))
      sig_cls <- if (!is.na(pv) && pv < 0.05) "success" else "warning"
      sig_lbl <- if (!is.na(pv) && pv < 0.05) tr("groupval.significant") else tr("groupval.not.significant")
      div(class = paste0("alert alert-", sig_cls), style = "margin-bottom: 10px;",
        tags$b(tr("groupval.permanova.title")), br(),
        tags$span(
          tr("groupval.r2"), ": ", tags$b(paste0(r2, "%")), " | ",
          "p = ", tags$b(pv_txt), " — ",
          tags$span(class = paste0("label label-", sig_cls), sig_lbl)
        ), br(),
        tags$small(style = "color: #555;", sprintf(tr("groupval.r2.desc"), r2))
      )
    } else {
      div(class = "alert alert-warning", tr("groupval.permanova.failed"))
    }

    sil_block <- if (!is.null(sil)) {
      avg_sil <- round(mean(sil$sil_width), 3)
      n_poor  <- sum(sil$sil_width < 0)
      sil_cls <- if (avg_sil > 0.5) "success" else if (avg_sil > 0.25) "warning" else "danger"
      sil_lbl <- if (avg_sil > 0.5) tr("groupval.sil.good") else if (avg_sil > 0.25) tr("groupval.sil.moderate") else tr("groupval.sil.poor")
      div(class = "alert alert-info", style = "margin-bottom: 10px;",
        tags$b(tr("groupval.sil.title")), br(),
        tags$span(
          tr("groupval.sil.avg"), ": ", tags$b(avg_sil), " — ",
          tags$span(class = paste0("label label-", sil_cls), sil_lbl)
        ), br(),
        if (n_poor > 0)
          tags$small(style = "color: #c0392b;", sprintf(tr("groupval.sil.poor.sites"), n_poor))
        else
          tags$small(style = "color: #27ae60;", tr("groupval.sil.all.fit"))
      )
    } else NULL

    tagList(
      div(class = "alert alert-info", style = "padding: 8px 12px; margin-bottom: 10px;",
        tags$b(sprintf(tr("groupval.summary"), n_sites, n_groups))
      ),
      perm_block,
      sil_block,
      h6(tr("groupval.group.sizes"), style = "margin-top: 12px;"),
      DT::dataTableOutput("groupval_groupsizes")
    )
  })

  output$groupval_groupsizes <- DT::renderDataTable({
    req(has_groups())
    gd  <- group_data()
    out <- as.data.frame(table(Gruppe = gd$group))
    colnames(out) <- c(tr("groupval.col.group"), "n")
    DT::datatable(out, rownames = FALSE,
      options = list(pageLength = 15, dom = "t", ordering = FALSE))
  }, server = FALSE)

  # ===== OUTPUT: BIPLOT =====

  output$groupval_biplot <- renderPlotly({
    req(has_groups(), ca_result())
    res   <- ca_result()
    gd    <- group_data()
    sil   <- silhouette_data()
    if (is.null(res$row$coord)) return(NULL)
    avail <- intersect(gd$site, rownames(res$row$coord))
    if (length(avail) < 2) return(NULL)
    gd <- gd[gd$site %in% avail, ]
    x  <- res$row$coord[gd$site, 1]
    y  <- res$row$coord[gd$site, 2]

    show_labels <- isTRUE(input$groupval_show_labels %||% TRUE)
    pt_size     <- as.numeric(input$groupval_point_size %||% 10)

    groups  <- factor(gd$group)
    n_grp   <- nlevels(groups)
    # 12-color qualitative palette, distinct and accessible
    base_pal <- c("#4e79a7","#f28e2b","#59a14f","#e15759","#76b7b2",
                  "#edc948","#b07aa1","#ff9da7","#9c755f","#bab0ac",
                  "#d37295","#499894")
    palette <- if (n_grp <= length(base_pal)) {
      base_pal[seq_len(n_grp)]
    } else {
      colorRampPalette(base_pal)(n_grp)
    }
    col_map   <- setNames(palette, levels(groups))
    opacities <- if (!is.null(sil)) {
      sil_sub <- sil[match(gd$site, sil$site), ]
      pmax(0.3, pmin(1, 0.5 + sil_sub$sil_width * 0.5))
    } else rep(0.88, nrow(gd))

    sil_sub <- if (!is.null(sil)) sil[match(gd$site, sil$site), ] else NULL
    htxt <- paste0(
      "<b>", gd$site, "</b><br>",
      tr("groupval.group"), ": ", gd$group,
      if (!is.null(sil_sub))
        paste0("<br>", tr("groupval.sil.score"), ": ", round(sil_sub$sil_width, 3),
               ifelse(sil_sub$sil_width < 0,
                 paste0(" (", tr("groupval.closer.to"), " ", sil_sub$neighbor, ")"), ""))
      else ""
    )

    p <- plot_ly()
    for (grp in levels(groups)) {
      idx <- which(as.character(groups) == grp)
      p <- add_trace(p,
        x = x[idx], y = y[idx],
        type = "scatter", mode = if (show_labels) "markers+text" else "markers",
        text = gd$site[idx], textposition = "top center",
        textfont = list(size = 10),
        marker = list(color = col_map[grp], size = pt_size, opacity = opacities[idx],
                      line = list(color = "white", width = 1.5)),
        hovertext = htxt[idx],
        hovertemplate = "%{hovertext}<extra></extra>",
        name = grp
      )
    }

    eig   <- res$eig
    x_pct <- if (!is.null(eig) && nrow(eig) >= 1) round(eig[1, 2], 1) else ""
    y_pct <- if (!is.null(eig) && nrow(eig) >= 2) round(eig[2, 2], 1) else ""

    p %>% layout(
      title  = list(text = tr("groupval.biplot.title"), font = list(size = 14)),
      xaxis  = list(title = paste0("Dim 1 (", x_pct, "%)"),
                    zeroline = TRUE, zerolinecolor = "#ddd", gridcolor = "#eee"),
      yaxis  = list(title = paste0("Dim 2 (", y_pct, "%)"),
                    zeroline = TRUE, zerolinecolor = "#ddd", gridcolor = "#eee"),
      legend = list(orientation = "h", y = -0.18),
      plot_bgcolor = "#fafafa", paper_bgcolor = "white",
      margin = list(t = 50, b = 80)
    ) %>% standard_plotly_config("2d")
  })

  # ===== OUTPUT: SILHOUETTE TABLE =====

  output$groupval_table <- DT::renderDataTable({
    sil <- silhouette_data()
    if (is.null(sil)) return(NULL)
    sil <- sil[order(sil$sil_width), ]
    out <- data.frame(
      Site     = sil$site,
      Group    = sil$group,
      Score    = sil$sil_width,
      Neighbor = ifelse(sil$sil_width < 0, sil$neighbor, "\u2014"),
      stringsAsFactors = FALSE
    )
    colnames(out) <- c(tr("groupval.col.site"), tr("groupval.col.group"),
                       tr("groupval.col.score"), tr("groupval.col.neighbor"))
    max_abs <- max(abs(out$Score), na.rm = TRUE)
    DT::datatable(out, rownames = FALSE,
      options = list(pageLength = 20, scrollX = TRUE,
                     order = list(list(2L, "asc")))
    ) %>% DT::formatStyle(
      "Score",
      background = DT::styleColorBar(c(-max_abs, max_abs), "#f0c27f"),
      backgroundSize = "100% 80%", backgroundRepeat = "no-repeat",
      backgroundPosition = "center"
    ) %>% DT::formatStyle(
      "Score",
      color = DT::styleInterval(0, c("#c0392b", "#27ae60")),
      fontWeight = "bold"
    )
  }, server = FALSE)

}

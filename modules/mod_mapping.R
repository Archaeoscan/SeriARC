# mod_mapping.R - Geographic Visualization (Siegmund 2020)
# === LOAD HELPER COMPONENTS ===
source("helpers/download_components.R", local = TRUE)

mod_mapping_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL
}

mod_mapping_server <- function(meta_data, data_raw, mapping_meta, site_clusters, cluster_names = NULL, cluster_colors = NULL, input, output, session, tr = function(x) x) {
  
  # HILFSFUNKTIONEN: Kartenerstellung
  add_scale <- function(map_obj) {
    tryCatch(
      leaflet::addScaleBar(map_obj, position = "bottomleft", 
                          options = leaflet::scaleBarOptions(metric = TRUE, imperial = FALSE, maxWidth = 200)),
      error = function(e) map_obj)
  }
  
  add_tiles_with_gray <- function(map_obj, url_template, gray = FALSE, group = "base", attribution) {
    leaflet::addTiles(
      map = map_obj, urlTemplate = url_template,
      options = leaflet::tileOptions(
        className = if (isTRUE(gray)) "tile-gray-watch" else NULL,
        attribution = attribution),
      group = group)
  }
  
  add_gray_mutation_observer <- function(widget) {
    htmlwidgets::onRender(widget, "
      function(el, x) {
        var pane = el.querySelector('.leaflet-tile-pane');
        if (!pane) return;
        function setGray(img) {
          try { img.style.filter = 'grayscale(100%) contrast(1.1)'; } catch(e) {}
        }
        function seedContainer(container) {
          container.querySelectorAll('img.leaflet-tile').forEach(setGray);
          var obs = new MutationObserver(function(m) {
            m.forEach(function(mu) {
              mu.addedNodes.forEach(function(nn) {
                if (nn && nn.tagName === 'IMG') setGray(nn);
                if (nn && nn.querySelectorAll) nn.querySelectorAll('img.leaflet-tile').forEach(setGray);
              });
            });
          });
          obs.observe(container, {childList:true, subtree:true});
        }
        var mainObs = new MutationObserver(function(muts) {
          muts.forEach(function(m) {
            m.addedNodes.forEach(function(n) {
              if (!(n instanceof Element)) return;
              if (n.classList && n.classList.contains('tile-gray-watch')) seedContainer(n);
              else if (n.tagName === 'IMG' && n.closest('.tile-gray-watch')) setGray(n);
              else { var grayContainers = n.querySelectorAll ? n.querySelectorAll('.tile-gray-watch') : [];
                     grayContainers.forEach(seedContainer); }
            });
          });
        });
        mainObs.observe(pane, {childList:true, subtree:true});
        Array.prototype.slice.call(pane.querySelectorAll('.tile-gray-watch')).forEach(seedContainer);
      }")
  }
  
  # HILFSFUNKTIONEN: Layout & Farben
  create_checkbox_layout <- function(checkbox_list) {
    n <- length(checkbox_list)
    if (n <= 3) {
      fluidRow(lapply(1:n, function(i) column(12/n, checkbox_list[[i]])))
    } else if (n <= 6) {
      tagList(
        fluidRow(lapply(1:min(3, n), function(i) column(4, checkbox_list[[i]]))),
        if (n > 3) fluidRow(lapply(4:n, function(i) column(4, checkbox_list[[i]])))
      )
    } else {
      n_per_row <- ceiling(n / 3)
      rows <- split(1:n, ceiling(seq_along(1:n) / n_per_row))
      lapply(rows, function(row_indices) {
        fluidRow(lapply(row_indices, function(i) column(12/length(row_indices), checkbox_list[[i]])))
      })
    }
  }
  
  get_group_colors_for_checkboxes <- function(available_groups) {
    n <- length(available_groups)
    if (n > 0) {
      cols <- RColorBrewer::brewer.pal(n = min(max(3, n), 8), name = "Set2")
      if (n > 8) cols <- grDevices::colorRampPalette(cols)(n)
      cols <- cols[1:n]  # Ensure same length as available_groups
      names(cols) <- as.character(available_groups)
      return(cols)
    }
    return(NULL)
  }

  get_cluster_colors_for_checkboxes <- function() {
    if (!is.null(cluster_colors) && is.function(cluster_colors)) {
      kmeans_colors <- tryCatch(cluster_colors(), error = function(e) NULL)
      if (!is.null(kmeans_colors) && length(kmeans_colors) > 0) return(kmeans_colors)
    }

    cluster_data <- site_clusters()
    if (!is.null(cluster_data)) {
      available_clusters <- sort(unique(na.omit(cluster_data)))
      n <- length(available_clusters)
      if (n > 0) {
        cols <- RColorBrewer::brewer.pal(n = min(max(3, n), 8), name = "Dark2")
        if (n > 8) cols <- grDevices::colorRampPalette(cols)(n)
        cols <- cols[1:n]  # Ensure same length as available_clusters
        names(cols) <- as.character(available_clusters)
        return(cols)
      }
    }
    return(NULL)
  }
  
  get_cluster_name <- function(cluster_id) {
    cluster_name_mapping <- if (!is.null(cluster_names) && is.function(cluster_names)) {
      tryCatch(cluster_names(), error = function(e) NULL)
    } else NULL
    
    if (!is.null(cluster_name_mapping) && !is.na(cluster_name_mapping[as.character(cluster_id)]) && cluster_name_mapping[as.character(cluster_id)] != "") {
      return(cluster_name_mapping[as.character(cluster_id)])
    } else {
      return(paste("Cluster", cluster_id))
    }
  }
  
  get_selected_groups_from_checkboxes <- function() {
    mapping_data_current <- mapping_data()
    if (is.null(mapping_data_current) || !"group" %in% names(mapping_data_current$meta)) return(NULL)

    available_groups <- sort(unique(na.omit(mapping_data_current$meta$group)))
    if (length(available_groups) == 0) return(NULL)

    # Check if any checkbox has been rendered yet
    first_checkbox_id <- paste0("map_group_", gsub("[^A-Za-z0-9_]", "_", available_groups[1]))
    if (is.null(input[[first_checkbox_id]])) {
      # Checkboxes not yet rendered - return all groups as selected (default)
      return(available_groups)
    }

    selected <- c()
    for (group_name in available_groups) {
      checkbox_id <- paste0("map_group_", gsub("[^A-Za-z0-9_]", "_", group_name))
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) selected <- c(selected, group_name)
    }
    return(selected)
  }

  get_selected_clusters_from_checkboxes <- function() {
    cluster_data <- site_clusters()
    if (is.null(cluster_data)) return(NULL)

    available_clusters <- sort(unique(na.omit(cluster_data)))
    if (length(available_clusters) == 0) return(NULL)

    # Check if any checkbox has been rendered yet
    first_checkbox_id <- paste0("map_cluster_", available_clusters[1])
    if (is.null(input[[first_checkbox_id]])) {
      # Checkboxes not yet rendered - return all clusters as selected (default)
      return(available_clusters)
    }

    selected <- c()
    for (cluster_id in available_clusters) {
      checkbox_id <- paste0("map_cluster_", cluster_id)
      if (!is.null(input[[checkbox_id]]) && input[[checkbox_id]]) selected <- c(selected, cluster_id)
    }
    return(selected)
  }
  
  get_uniform_gray_color <- function() "#A0A0A0"
  
  # PREPARE DATA
  mapping_data <- reactive({
    if (is.null(meta_data$data) || is.null(data_raw()) || is.null(mapping_meta())) return(NULL)
    
    m <- if (!is.null(meta_data$sites) && "Selected" %in% names(meta_data$sites)) {
      sel_sites <- meta_data$sites$Entity[meta_data$sites$Selected]
      mapping_meta()[mapping_meta()$site %in% sel_sites, , drop = FALSE]
    } else mapping_meta()
    
    if (nrow(m) == 0) return(NULL)
    
    m$lon <- suppressWarnings(as.numeric(m$lon))
    m$lat <- suppressWarnings(as.numeric(m$lat))
    valid_coords <- sum(is.finite(m$lon) & is.finite(m$lat))
    
    list(meta = m, types = as.matrix(data_raw()[, -1, drop = FALSE]), 
         coord_info = m, 
         status = if(valid_coords == 0) "no_coords" else "ready")
  })
  
  # KMEANS AVAILABILITY
  output$kmeans_available <- reactive({
    cl <- site_clusters()
    !is.null(cl) && length(na.omit(cl)) > 0
  })
  outputOptions(output, "kmeans_available", suspendWhenHidden = FALSE)
  
  output$map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
      leaflet::setView(lng = 12.5, lat = 48.8, zoom = 8) %>%
      add_tiles_with_gray("https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png", 
                         gray = FALSE, group = "base",
                         attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a> contributors, <a href="https://opentopomap.org/">OpenTopoMap</a>') %>%
      add_gray_mutation_observer() %>%
      add_scale()
    m
  })
  
  # BASEMAP AKTUALISIEREN
  observe({
    basemap_choice <- input$map_basemap_selection %||% "OpenTopoMap_Color"
    proxy <- leaflet::leafletProxy("map") %>% leaflet::clearGroup("base")
    
    if (basemap_choice == "OpenTopoMap_Color") {
      proxy <- add_tiles_with_gray(proxy, "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png", 
                                  gray = FALSE, group = "base",
                                  attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a> contributors, <a href="https://opentopomap.org/">OpenTopoMap</a>')
    } else if (basemap_choice == "OpenTopoMap_Gray") {
      proxy <- add_tiles_with_gray(proxy, "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png", 
                                  gray = TRUE, group = "base",
                                  attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a> contributors, <a href="https://opentopomap.org/">OpenTopoMap</a>')
    } else if (basemap_choice == "MapsForFree_Color") {
      proxy <- add_tiles_with_gray(proxy, "https://maps-for-free.com/layer/relief/z{z}/row{y}/{z}_{x}-{y}.jpg", 
                                  gray = FALSE, group = "base",
                                  attribution = 'Relief &copy; <a href="https://maps-for-free.com/">maps-for-free.com</a> (CC0)')
    } else if (basemap_choice == "MapsForFree_Gray") {
      proxy <- add_tiles_with_gray(proxy, "https://maps-for-free.com/layer/relief/z{z}/row{y}/{z}_{x}-{y}.jpg", 
                                  gray = TRUE, group = "base",
                                  attribution = 'Relief &copy; <a href="https://maps-for-free.com/">maps-for-free.com</a> (CC0)')
    }
  })
  
  # WASSER-OVERLAY
  observe({
    show_water <- isTRUE(input$map_show_water_overlay)
    proxy <- leaflet::leafletProxy("map") %>% leaflet::clearGroup("water_overlay")
    
    if (show_water) {
      proxy <- proxy %>% leaflet::addTiles(
        urlTemplate = "https://maps-for-free.com/layer/water/z{z}/row{y}/{z}_{x}-{y}.gif",
        group = "water_overlay",
        options = leaflet::tileOptions(attribution = "Water © maps-for-free.com (CC0)", opacity = 0.7))
    }
  })
  
  # GRUPPEN-CHECKBOXES ERSTELLEN
  output$map_group_checkboxes <- renderUI({
    mapping_data_current <- mapping_data()
    if (is.null(mapping_data_current) || input$map_color_by != "group") return(NULL)
    
    mm <- mapping_data_current$meta
    if (!"group" %in% names(mm)) return(div("No group column available", style = "color: #dc3545; font-style: italic;"))

    available_groups <- sort(unique(na.omit(mm$group)))
    if (length(available_groups) == 0) return(div("No groups available", style = "color: #dc3545; font-style: italic;"))
    
    # Group colors (Goldmann 1972: Chronological grouping)
    group_color_mapping <- get_group_colors_for_checkboxes(available_groups)
    
    checkbox_list <- lapply(available_groups, function(x) {
      group_color <- if (!is.null(group_color_mapping) && !is.na(group_color_mapping[as.character(x)])) {
        group_color_mapping[as.character(x)]
      } else "#2196f3"
      
      color_dot <- sprintf('<span class="group-color-dot" style="display:inline-block; width:12px; height:12px; border-radius:50%%; background-color:%s; margin-right:8px; vertical-align:middle; border:1px solid #ccc;"></span>', group_color)
      
      div(class = "group-checkbox-container", style = "margin-bottom: 8px;",
          checkboxInput(inputId = paste0("map_group_", gsub("[^A-Za-z0-9_]", "_", x)),
                       label = HTML(paste0(color_dot, as.character(x))), value = TRUE))
    })
    
    create_checkbox_layout(checkbox_list)
  })
  
  # CLUSTER-CHECKBOXES ERSTELLEN
  output$map_cluster_checkboxes <- renderUI({
    cluster_data <- site_clusters()
    if (is.null(cluster_data) || input$map_color_by != "cluster") return(NULL)
    
    available_clusters <- sort(unique(na.omit(cluster_data)))
    if (length(available_clusters) == 0) return(div("No clusters available", style = "color: #dc3545; font-style: italic;"))
    
    # Cluster names and colors (Bock 1974: Automatic Classification)
    cluster_name_mapping <- if (!is.null(cluster_names) && is.function(cluster_names)) {
      tryCatch(cluster_names(), error = function(e) NULL)
    } else NULL
    cluster_color_mapping <- get_cluster_colors_for_checkboxes()
    
    checkbox_list <- lapply(available_clusters, function(x) {
      label_text <- if (!is.null(cluster_name_mapping) && !is.na(cluster_name_mapping[as.character(x)]) && cluster_name_mapping[as.character(x)] != "") {
        paste("Cluster", x, "-", cluster_name_mapping[as.character(x)])
      } else paste("Cluster", x)
      
      cluster_color <- if (!is.null(cluster_color_mapping) && !is.na(cluster_color_mapping[as.character(x)])) {
        cluster_color_mapping[as.character(x)]
      } else "#666666"
      
      color_dot <- sprintf('<span class="cluster-color-dot" style="display:inline-block; width:12px; height:12px; border-radius:50%%; background-color:%s; margin-right:8px; vertical-align:middle; border:1px solid #ccc;"></span>', cluster_color)
      
      div(class = "cluster-checkbox-container", style = "margin-bottom: 8px;",
          checkboxInput(inputId = paste0("map_cluster_", x),
                       label = HTML(paste0(color_dot, label_text)), value = TRUE))
    })
    
    create_checkbox_layout(checkbox_list)
  })
  
  # QUICK SELECTION BUTTONS
  observeEvent(input$map_select_all_groups, {
    mapping_data_current <- mapping_data()
    if (!is.null(mapping_data_current) && "group" %in% names(mapping_data_current$meta)) {
      all_groups <- sort(unique(na.omit(mapping_data_current$meta$group)))
      for (group_name in all_groups) {
        checkbox_id <- paste0("map_group_", gsub("[^A-Za-z0-9_]", "_", group_name))
        updateCheckboxInput(session, checkbox_id, value = TRUE)
      }
    }
  })
  
  observeEvent(input$map_select_no_groups, {
    mapping_data_current <- mapping_data()
    if (!is.null(mapping_data_current) && "group" %in% names(mapping_data_current$meta)) {
      all_groups <- sort(unique(na.omit(mapping_data_current$meta$group)))
      for (group_name in all_groups) {
        checkbox_id <- paste0("map_group_", gsub("[^A-Za-z0-9_]", "_", group_name))
        updateCheckboxInput(session, checkbox_id, value = FALSE)
      }
    }
  })
  
  observeEvent(input$map_invert_groups, {
    mapping_data_current <- mapping_data()
    if (!is.null(mapping_data_current) && "group" %in% names(mapping_data_current$meta)) {
      all_groups <- sort(unique(na.omit(mapping_data_current$meta$group)))
      for (group_name in all_groups) {
        checkbox_id <- paste0("map_group_", gsub("[^A-Za-z0-9_]", "_", group_name))
        current_value <- input[[checkbox_id]] %||% TRUE
        updateCheckboxInput(session, checkbox_id, value = !current_value)
      }
    }
  })
  
  observeEvent(input$map_select_all_clusters, {
    cluster_data <- site_clusters()
    if (!is.null(cluster_data)) {
      all_clusters <- sort(unique(na.omit(cluster_data)))
      for (cluster_id in all_clusters) {
        checkbox_id <- paste0("map_cluster_", cluster_id)
        updateCheckboxInput(session, checkbox_id, value = TRUE)
      }
    }
  })
  
  observeEvent(input$map_select_no_clusters, {
    cluster_data <- site_clusters()
    if (!is.null(cluster_data)) {
      all_clusters <- sort(unique(na.omit(cluster_data)))
      for (cluster_id in all_clusters) {
        checkbox_id <- paste0("map_cluster_", cluster_id)
        updateCheckboxInput(session, checkbox_id, value = FALSE)
      }
    }
  })
  
  observeEvent(input$map_invert_clusters, {
    cluster_data <- site_clusters()
    if (!is.null(cluster_data)) {
      all_clusters <- sort(unique(na.omit(cluster_data)))
      for (cluster_id in all_clusters) {
        checkbox_id <- paste0("map_cluster_", cluster_id)
        current_value <- input[[checkbox_id]] %||% TRUE
        updateCheckboxInput(session, checkbox_id, value = !current_value)
      }
    }
  })
  
  # INFO TEXTS
  output$map_group_info <- renderText({
    mapping_data_current <- mapping_data()
    if (is.null(mapping_data_current) || !"group" %in% names(mapping_data_current$meta)) {
      return("No group data available")
    }

    mm <- mapping_data_current$meta
    selected_groups <- get_selected_groups_from_checkboxes()
    display_mode <- input$map_group_display_mode %||% "hide"
    total_sites <- nrow(mm)

    if (is.null(selected_groups) || length(selected_groups) == 0) {
      if (display_mode == "hide") return("📄 No groups selected - no sites visible")
      else return(sprintf("📄 All %d sites shown grayed out", total_sites))
    }

    selected_sites <- sum(mm$group %in% selected_groups, na.rm = TRUE)
    total_groups <- length(unique(na.omit(mm$group)))

    if (display_mode == "hide") {
      sprintf("📄 %d of %d sites visible (%d of %d groups selected)",
              selected_sites, total_sites, length(selected_groups), total_groups)
    } else {
      unselected_sites <- total_sites - selected_sites
      sprintf("📄 %d sites normal, %d sites grayed out (%d of %d groups selected)",
              selected_sites, unselected_sites, length(selected_groups), total_groups)
    }
  })

  output$map_cluster_info <- renderText({
    cluster_data <- site_clusters()
    if (is.null(cluster_data)) return("No cluster data available")

    selected_clusters <- get_selected_clusters_from_checkboxes()
    display_mode <- input$map_cluster_display_mode %||% "hide"
    total_sites <- length(cluster_data)

    if (is.null(selected_clusters) || length(selected_clusters) == 0) {
      if (display_mode == "hide") return("📊 No clusters selected - no sites visible")
      else return(sprintf("📊 All %d sites shown grayed out", total_sites))
    }
    
    selected_sites <- sum(cluster_data %in% selected_clusters, na.rm = TRUE)
    total_clusters <- length(unique(na.omit(cluster_data)))
    
    if (display_mode == "hide") {
      sprintf("📊 %d of %d sites visible (%d of %d clusters selected)",
              selected_sites, total_sites, length(selected_clusters), total_clusters)
    } else {
      unselected_sites <- total_sites - selected_sites
      sprintf("📊 %d sites normal, %d sites grayed out (%d of %d clusters selected)",
              selected_sites, unselected_sites, length(selected_clusters), total_clusters)
    }
  })
  
  # MAIN MAP OBSERVER (SPATIAL ANALYSIS - Siegmund 2020)
  observe({
    dat <- mapping_data()
    if (is.null(dat)) {
      leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% leaflet::clearControls()
      return()
    }
    
    mm <- dat$meta
    has_coords <- is.finite(mm$lon) & is.finite(mm$lat)
    if (!any(has_coords)) {
      leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% leaflet::clearControls()
      return()
    }
    mm <- mm[has_coords, , drop = FALSE]
    
    # Filter anwenden
    sites_to_gray <- NULL
    
    if (identical(input$map_color_by, "group") && "group" %in% names(mm)) {
      selected_groups <- get_selected_groups_from_checkboxes()
      if (is.null(selected_groups)) selected_groups <- c()  # NULL normalisieren
      display_mode <- input$map_group_display_mode %||% "hide"
      
      if (display_mode == "hide") {
        if (length(selected_groups) > 0) mm <- mm[mm$group %in% selected_groups, , drop = FALSE]
        else mm <- mm[FALSE, , drop = FALSE]
      } else if (display_mode == "gray") {
        if (length(selected_groups) > 0) sites_to_gray <- mm$site[!mm$group %in% selected_groups]
        else sites_to_gray <- mm$site
      }
    }
    
    if (identical(input$map_color_by, "cluster")) {
      cl_all <- site_clusters()
      selected_clusters <- get_selected_clusters_from_checkboxes()
      if (is.null(selected_clusters)) selected_clusters <- c()  # NULL normalisieren
      display_mode <- input$map_cluster_display_mode %||% "hide"
      
      if (!is.null(cl_all)) {
        if (display_mode == "hide") {
          if (length(selected_clusters) > 0) {
            selected_sites <- names(cl_all)[cl_all %in% selected_clusters]
            mm <- mm[mm$site %in% selected_sites, , drop = FALSE]
          } else mm <- mm[FALSE, , drop = FALSE]
        } else if (display_mode == "gray") {
          if (length(selected_clusters) > 0) sites_to_gray <- names(cl_all)[!cl_all %in% selected_clusters]
          else sites_to_gray <- names(cl_all)
        }
      }
    }
    
    if (nrow(mm) == 0) {
      leaflet::leafletProxy("map") %>% leaflet::clearMarkers() %>% leaflet::clearControls()
      return()
    }
    
    # Coloring
    col_by <- input$map_color_by %||% "group"
    col_vals <- NULL
    
    if (identical(col_by, "group") && "group" %in% names(mm)) {
      grp <- as.factor(mm$group)
      n <- max(3, length(levels(grp)))
      cols <- RColorBrewer::brewer.pal(n = min(n, 8), name = "Set2")
      if (n > 8) cols <- grDevices::colorRampPalette(cols)(n)
      pal <- leaflet::colorFactor(palette = cols, domain = levels(grp))
      col_vals <- pal(grp)
      
      if (!is.null(sites_to_gray) && length(sites_to_gray) > 0) {
        gray_indices <- mm$site %in% sites_to_gray
        col_vals[gray_indices] <- get_uniform_gray_color()
      }
    } else if (identical(col_by, "cluster")) {
      cl_all <- site_clusters()
      if (!is.null(cl_all)) {
        cl <- cl_all[mm$site]
        cl <- factor(cl, levels = sort(unique(na.omit(cl))))
        
        if (!is.null(cluster_colors) && is.function(cluster_colors)) {
          kmeans_colors <- tryCatch(cluster_colors(), error = function(e) NULL)
          if (!is.null(kmeans_colors) && length(kmeans_colors) > 0) {
            cols <- kmeans_colors[as.character(levels(cl))]
            cols <- cols[!is.na(cols)]
            if (length(cols) == length(levels(cl))) {
              pal <- leaflet::colorFactor(palette = cols, domain = levels(cl))
              col_vals <- pal(cl)
            }
          }
        }
        
        if (is.null(col_vals)) {
          n <- max(3, length(levels(cl)))
          cols <- RColorBrewer::brewer.pal(n = min(n, 8), name = "Dark2")
          if (n > 8) cols <- grDevices::colorRampPalette(cols)(n)
          pal <- leaflet::colorFactor(palette = cols, domain = levels(cl))
          col_vals <- pal(cl)
        }
        
        if (!is.null(sites_to_gray) && length(sites_to_gray) > 0) {
          gray_indices <- mm$site %in% sites_to_gray
          col_vals[gray_indices] <- get_uniform_gray_color()
        }
      }
    }
    
    # Labels and popups
    label_mode <- input$map_label_mode %||% "none"
    display_title <- NULL; display_val <- NULL
    
    if (identical(col_by, "cluster") && !is.null(site_clusters())) {
      cl_all <- site_clusters()
      display_title <- "Cluster"
      raw_cluster_vals <- cl_all[mm$site]
      
      cluster_name_mapping <- if (!is.null(cluster_names) && is.function(cluster_names)) {
        tryCatch(cluster_names(), error = function(e) NULL)
      } else NULL
      
      if (!is.null(cluster_name_mapping) && length(cluster_name_mapping) > 0) {
        display_val <- sapply(raw_cluster_vals, function(x) {
          if (is.na(x)) return("Keine")
          custom_name <- cluster_name_mapping[as.character(x)]
          if (!is.na(custom_name) && custom_name != "") return(custom_name)
          else return(paste("Cluster", x))
        })
      } else display_val <- raw_cluster_vals
    } else if ("group" %in% names(mm)) {
      display_title <- "Gruppe"; display_val <- mm$group
    }
    
    base_label_content <- if (!is.null(display_title) && !is.null(display_val)) {
      sprintf("%s (%s: %s)", mm$site, display_title, ifelse(is.na(display_val), "Keine", as.character(display_val)))
    } else mm$site
    
    if (!is.null(input$map_label_chars)) {
      lablen <- as.integer(input$map_label_chars)
      base_label_content <- vapply(base_label_content, function(x){
        if (nchar(x) > lablen) paste0(substr(x,1,lablen), "…") else x
      }, character(1))
    }
    
    popup_content <- if (!is.null(display_title) && !is.null(display_val)) {
      sprintf("<b>%s</b><br/>%.5f, %.5f<br/>%s: %s", mm$site, mm$lat, mm$lon, display_title, ifelse(is.na(display_val), "Keine", as.character(display_val)))
    } else sprintf("<b>%s</b><br/>%.5f, %.5f", mm$site, mm$lat, mm$lon)
    
    # Add markers
    proxy <- leaflet::leafletProxy("map") %>%
      leaflet::clearMarkers() %>%
      leaflet::clearControls() %>%
      leaflet::addCircleMarkers(
        data = mm, lng = ~lon, lat = ~lat, popup = popup_content,
        radius = max(3, input$map_point_size %||% 3), fillOpacity = 0.9, stroke = TRUE, 
        weight = 2, color = "#FFFFFF", 
        fillColor = if (!is.null(col_vals)) col_vals else "#FF0000", group = "Sites")
    
    # Permanente Labels
    if (label_mode == "permanent") {
      n_sites <- nrow(mm)
      if (n_sites > 1) {
        angles <- seq(0, 2*pi, length.out = n_sites + 1)[1:n_sites]
        offset_distance <- (input$map_point_size %||% 8) + 15
        
        for (i in 1:n_sites) {
          dx <- cos(angles[i]) * offset_distance
          dy <- sin(angles[i]) * offset_distance
          
          proxy <- proxy %>%
            leaflet::addLabelOnlyMarkers(
              lng = mm$lon[i], lat = mm$lat[i], label = htmltools::HTML(base_label_content[i]),
              labelOptions = leaflet::labelOptions(
                noHide = TRUE, direction = "auto", textsize = paste0(input$map_label_size %||% 12, "px"),
                offset = c(dx, dy), style = list("font-weight" = "bold", "padding" = "3px 8px",
                                                 "background" = "rgba(255,255,255,0.9)", "border" = "1px solid #999",
                                                 "border-radius" = "4px", "font-size" = paste0(input$map_label_size %||% 12, "px"))))
        }
      } else {
        proxy <- proxy %>%
          leaflet::addLabelOnlyMarkers(
            lng = mm$lon, lat = mm$lat, label = htmltools::HTML(base_label_content),
            labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "auto", textsize = paste0(input$map_label_size %||% 12, "px")))
      }
    }
    
    proxy <- add_scale(proxy)
    
    # Add legend (requirement: Legend option like in Type Mapping)
    if (!is.null(input$map_show_legend) && input$map_show_legend) {
      if (identical(col_by, "group") && "group" %in% names(mm)) {
        grp <- as.factor(mm$group)
        unique_groups <- levels(grp)
        if (length(unique_groups) > 0) {
          n <- length(unique_groups)
          # brewer.pal needs at least 3, so get enough colors then subset
          cols <- RColorBrewer::brewer.pal(n = min(max(3, n), 8), name = "Set2")
          if (n > 8) cols <- grDevices::colorRampPalette(cols)(n)
          cols <- cols[1:n]  # Ensure same length as unique_groups
          proxy <- proxy %>% leaflet::addLegend(
            position = "bottomright",
            colors = cols,
            labels = unique_groups,
            title = "Gruppen",
            opacity = 0.8
          )
        }
      } else if (identical(col_by, "cluster") && !is.null(site_clusters())) {
        cl_all <- site_clusters()
        if (!is.null(cl_all)) {
          unique_clusters <- sort(unique(na.omit(cl_all)))
          if (length(unique_clusters) > 0) {
            cluster_color_mapping <- get_cluster_colors_for_checkboxes()
            if (!is.null(cluster_color_mapping)) {
              legend_colors <- cluster_color_mapping[as.character(unique_clusters)]
              legend_colors[is.na(legend_colors)] <- "#666666"  # NA-Farben ersetzen
              legend_labels <- sapply(unique_clusters, get_cluster_name)
              proxy <- proxy %>% leaflet::addLegend(
                position = "bottomright", 
                colors = legend_colors, 
                labels = legend_labels,
                title = "Cluster", 
                opacity = 0.8
              )
            }
          }
        }
      }
    }
    
    # Auto-Zoom auf Gesamtausdehnung
    if (isTRUE(input$map_auto_zoom)) {
      dat_all <- mapping_data()
      if (!is.null(dat_all) && !is.null(dat_all$meta)) {
        mm_all <- dat_all$meta
        mm_all$lon <- suppressWarnings(as.numeric(mm_all$lon))
        mm_all$lat <- suppressWarnings(as.numeric(mm_all$lat))
        has_coords_all <- is.finite(mm_all$lon) & is.finite(mm_all$lat)
        
        if (any(has_coords_all)) {
          mm_zoom <- mm_all[has_coords_all, , drop = FALSE]
          bbox <- c(min(mm_zoom$lon, na.rm = TRUE), min(mm_zoom$lat, na.rm = TRUE),
                    max(mm_zoom$lon, na.rm = TRUE), max(mm_zoom$lat, na.rm = TRUE))
          
          if (all(is.finite(bbox)) && bbox[1] < bbox[3] && bbox[2] < bbox[4]) {
            lon_padding <- (bbox[3] - bbox[1]) * 0.1
            lat_padding <- (bbox[4] - bbox[2]) * 0.1
            bbox_padded <- c(bbox[1] - lon_padding, bbox[2] - lat_padding, bbox[3] + lon_padding, bbox[4] + lat_padding)
            
            proxy <- proxy %>% leaflet::fitBounds(bbox_padded[1], bbox_padded[2], bbox_padded[3], bbox_padded[4], options = list(maxZoom = 12))
          }
        }
      }
    }
  })
  
  # EXPORT FUNCTIONS
  build_export_map <- function(base_choice = NULL) {
    if (is.null(base_choice)) base_choice <- input$map_basemap_selection %||% "OpenTopoMap_Color"
    
    dat <- mapping_data()
    meta_map <- if (!is.null(dat)) dat$meta else NULL
    
    m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
      add_gray_mutation_observer()
    
    if (is.null(meta_map) || nrow(meta_map) == 0) {
      return(add_scale(m) %>% leaflet::setView(lng = 11.5, lat = 48.7, zoom = 6))
    }
    
    meta_map$lon <- suppressWarnings(as.numeric(meta_map$lon))
    meta_map$lat <- suppressWarnings(as.numeric(meta_map$lat))
    has_coords <- is.finite(meta_map$lon) & is.finite(meta_map$lat)
    if (!any(has_coords)) return(add_scale(m) %>% leaflet::setView(lng = 11.5, lat = 48.7, zoom = 6))
    mm <- meta_map[has_coords, , drop = FALSE]
    
    # Basemap for export
    if (base_choice == "OpenTopoMap_Color") {
      m <- add_tiles_with_gray(m, "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png", gray = FALSE, group = "base",
                              attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a> contributors, <a href="https://opentopomap.org/">OpenTopoMap</a>')
    } else if (base_choice == "OpenTopoMap_Gray") {
      m <- add_tiles_with_gray(m, "https://{s}.tile.opentopomap.org/{z}/{x}/{y}.png", gray = TRUE, group = "base",
                              attribution = '&copy; <a href="https://www.openstreetmap.org/copyright">OSM</a> contributors, <a href="https://opentopomap.org/">OpenTopoMap</a>')
    } else if (base_choice == "MapsForFree_Color") {
      m <- add_tiles_with_gray(m, "https://maps-for-free.com/layer/relief/z{z}/row{y}/{z}_{x}-{y}.jpg", gray = FALSE, group = "base",
                              attribution = 'Relief &copy; <a href="https://maps-for-free.com/">maps-for-free.com</a> (CC0)')
    } else if (base_choice == "MapsForFree_Gray") {
      m <- add_tiles_with_gray(m, "https://maps-for-free.com/layer/relief/z{z}/row{y}/{z}_{x}-{y}.jpg", gray = TRUE, group = "base",
                              attribution = 'Relief &copy; <a href="https://maps-for-free.com/">maps-for-free.com</a> (CC0)')
    }
    
    # Water overlay in export
    if (isTRUE(input$map_show_water_overlay)) {
      m <- m %>% leaflet::addTiles(
        urlTemplate = "https://maps-for-free.com/layer/water/z{z}/row{y}/{z}_{x}-{y}.gif",
        options = leaflet::tileOptions(attribution = "Water © maps-for-free.com (CC0)", opacity = 0.7),
        group = "water_overlay")
    }
    
    # Markers for export
    m <- m %>% leaflet::addCircleMarkers(
      data = mm, lng = ~lon, lat = ~lat,
      popup = sprintf("<b>%s</b><br/>%.5f, %.5f", mm$site, mm$lat, mm$lon),
      radius = input$map_point_size %||% 8, fillOpacity = 0.9, stroke = TRUE, 
      weight = 2, color = "white", fillColor = "#FF0000")
    
    # Labels in export
    if (input$map_label_mode == "permanent") {
      m <- m %>% leaflet::addLabelOnlyMarkers(
        lng = mm$lon, lat = mm$lat, label = mm$site,
        labelOptions = leaflet::labelOptions(noHide = TRUE, direction = "auto", textsize = paste0(input$map_label_size %||% 12, "px")))
    }
    
    # Zoom to data extent
    bbox <- c(min(mm$lon, na.rm = TRUE), min(mm$lat, na.rm = TRUE), max(mm$lon, na.rm = TRUE), max(mm$lat, na.rm = TRUE))
    if (all(is.finite(bbox)) && bbox[1] < bbox[3] && bbox[2] < bbox[4]) {
      m <- leaflet::fitBounds(m, bbox[1], bbox[2], bbox[3], bbox[4])
    } else m <- leaflet::setView(m, lng = 11.5, lat = 48.7, zoom = 6)
    
    m <- add_scale(m)
    m
  }
  
  # DOWNLOAD HANDLER
  output$download_map_html <- downloadHandler(
    filename = function() paste0("SeriARC_Map_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content  = function(file) {
      m <- build_export_map()
      htmlwidgets::saveWidget(m, file = file, selfcontained = TRUE)
    })
  
  output$download_map_png <- create_png_download_handler(
    "map", "SeriARC_Map", session,
    plot_data = reactive({ mapping_data() }),
    plot_generator_func = function(data) {
      if (!is.null(data) && !is.null(data$meta) && nrow(data$meta) > 0) {
        # Use real mapping data for PNG
        meta_map <- data$meta
        meta_map$lon <- suppressWarnings(as.numeric(meta_map$lon))
        meta_map$lat <- suppressWarnings(as.numeric(meta_map$lat))
        has_coords <- is.finite(meta_map$lon) & is.finite(meta_map$lat)
        
        if (any(has_coords)) {
          mm <- meta_map[has_coords, , drop = FALSE]
          
          par(mar = c(4, 4, 4, 2))
          
          # Simple point map for PNG export
          plot(mm$lon, mm$lat, 
               pch = 16, cex = (input$map_point_size %||% 8) * 0.2,
               col = "#e74c3c", 
               xlab = "Longitude", ylab = "Latitude",
               main = "SeriARC Mapping")
          
          # Add labels if enabled
          if (!is.null(input$map_label_mode) && input$map_label_mode == "permanent") {
            text(mm$lon, mm$lat, mm$site, 
                 pos = 3, cex = 0.7, col = "#2c3e50")
          }
          
          # Grid for orientation
          grid(col = "lightgray", lty = "dotted")
          
          # Info-Text
          mtext(sprintf("PNG Export | %d Sites | %s", nrow(mm), format(Sys.time(), "%Y-%m-%d %H:%M")), 
                side = 1, line = 3, cex = 0.7, col = "gray")
        }
      }
    }
  )
  
  return(list(mapping_data = mapping_data, build_export_map = build_export_map))
}

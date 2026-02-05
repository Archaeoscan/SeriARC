# mod_typenkartierung.R - Spatial artifact distribution (Ihm 1983)
# === LOAD HELPER COMPONENTS ===
source("helpers/download_components.R", local = TRUE)

mod_typenkartierung_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL
}

mod_typenkartierung_server <- function(meta_data, data_raw, mapping_meta, site_clusters, cluster_names = NULL, cluster_colors = NULL, input, output, session, tr = function(x) x) {
  
  # HILFSFUNKTIONEN KONSOLIDIERT
  add_scale <- function(map_obj) {
    tryCatch(leaflet::addScaleBar(map_obj, position = "bottomleft", 
                                 options = leaflet::scaleBarOptions(metric = TRUE, imperial = FALSE, maxWidth = 200)),
             error = function(e) map_obj)
  }
  
  add_tiles_with_gray <- function(map_obj, url_template, gray = FALSE, group = "base", attribution) {
    leaflet::addTiles(map = map_obj, urlTemplate = url_template,
                     options = leaflet::tileOptions(className = if (isTRUE(gray)) "tile-gray-watch" else NULL, attribution = attribution),
                     group = group)
  }
  
  add_gray_mutation_observer <- function(widget) {
    htmlwidgets::onRender(widget, "
      function(el, x) {
        var pane = el.querySelector('.leaflet-tile-pane');
        if (!pane) return;
        function setGray(img) { try { img.style.filter = 'grayscale(100%) contrast(1.1)'; } catch(e) {} }
        function seedContainer(container) {
          container.querySelectorAll('img.leaflet-tile').forEach(setGray);
          var obs = new MutationObserver(function(m) {
            m.forEach(function(mu) { mu.addedNodes.forEach(function(nn) {
              if (nn && nn.tagName === 'IMG') setGray(nn);
              if (nn && nn.querySelectorAll) nn.querySelectorAll('img.leaflet-tile').forEach(setGray);
            }); });
          });
          obs.observe(container, {childList:true, subtree:true});
        }
        var mainObs = new MutationObserver(function(muts) {
          muts.forEach(function(m) { m.addedNodes.forEach(function(n) {
            if (!(n instanceof Element)) return;
            if (n.classList && n.classList.contains('tile-gray-watch')) seedContainer(n);
            else if (n.tagName === 'IMG' && n.closest('.tile-gray-watch')) setGray(n);
            else { var grayContainers = n.querySelectorAll ? n.querySelectorAll('.tile-gray-watch') : [];
                   grayContainers.forEach(seedContainer); }
          }); });
        });
        mainObs.observe(pane, {childList:true, subtree:true});
        Array.prototype.slice.call(pane.querySelectorAll('.tile-gray-watch')).forEach(seedContainer);
      }")
  }
  
  # TYPEN-SPEZIFISCHE FUNKTIONEN
  get_type_colors <- function(n_types) {
    if (n_types <= 0) return(character(0))
    if (n_types <= 8) {
      colors <- RColorBrewer::brewer.pal(min(8, max(3, n_types)), "Dark2")[1:n_types]
    } else if (n_types <= 12) {
      colors <- RColorBrewer::brewer.pal(min(12, max(3, n_types)), "Paired")[1:n_types]
    } else {
      base_colors <- c("#d62728", "#2ca02c", "#1f77b4", "#ff7f0e", "#9467bd", "#8c564b", "#e377c2", "#7f7f7f", "#bcbd22", "#17becf")
      colors <- grDevices::colorRampPalette(base_colors)(n_types)
    }
    return(colors)
  }
  
  # EISWAFFEL-PIN FUNKTIONEN - Konstante Waffel + variable Eiskugel
  create_pushpin_icon <- function(color, size_factor = 1.0, marker_scale = 1.0) {
    # Eingabe-Validierung
    size_factor <- as.numeric(size_factor)
    marker_scale <- as.numeric(marker_scale)
    if (is.na(size_factor)) size_factor <- 1.0
    if (is.na(marker_scale)) marker_scale <- 1.0
    
    # Cone stays constant, only ice cream scoop varies
    base_size <- 20 * marker_scale  # Base size of the cone
    cone_width <- round(base_size * 0.6)
    cone_height <- round(base_size * 0.8)
    
    # Ice cream scoop size varies with abundance
    ice_radius <- max(4, min(15, round(8 * size_factor * marker_scale)))
    
    # Adjust total size
    total_width <- max(cone_width, ice_radius * 2 + 2)
    total_height <- cone_height + ice_radius + 2
    
    # Positionen berechnen
    center_x <- total_width / 2
    ice_center_y <- ice_radius + 1
    cone_top_y <- ice_center_y + ice_radius - 2
    
    # Eiswaffel-SVG: Gelbe Waffel + farbige Eiskugel
    waffel_color <- "#D4A574"  # Waffelbraun/beige
    svg_content <- paste0(
      '<svg width="', total_width, '" height="', total_height, '" xmlns="http://www.w3.org/2000/svg">',
      # Cone (triangle) - always same size
      '<path d="M', center_x - cone_width/2, ',', cone_top_y, ' ',
      'L', center_x, ',', total_height - 1, ' ',
      'L', center_x + cone_width/2, ',', cone_top_y, ' Z" ',
      'fill="', waffel_color, '" stroke="#B8956A" stroke-width="1"/>',
      # Waffel-Muster (optional)
      '<line x1="', center_x - cone_width/4, '" y1="', cone_top_y + cone_height/3, '" ',
      'x2="', center_x + cone_width/4, '" y2="', cone_top_y + cone_height/3, '" ',
      'stroke="#B8956A" stroke-width="0.5"/>',
      '<line x1="', center_x - cone_width/6, '" y1="', cone_top_y + cone_height*2/3, '" ',
      'x2="', center_x + cone_width/6, '" y2="', cone_top_y + cone_height*2/3, '" ',
      'stroke="#B8956A" stroke-width="0.5"/>',
      # Ice cream scoop - size varies with abundance
      '<circle cx="', center_x, '" cy="', ice_center_y, '" r="', ice_radius, '" ',
      'fill="', color, '" stroke="white" stroke-width="1"/>',
      '</svg>'
    )
    
    # SVG zu Data-URI konvertieren
    svg_encoded <- utils::URLencode(svg_content, reserved = TRUE)
    data_uri <- paste0("data:image/svg+xml;charset=utf-8,", svg_encoded)
    
    # Icon erstellen
    tryCatch({
      leaflet::makeIcon(
        iconUrl = data_uri,
        iconWidth = total_width,
        iconHeight = total_height,
        iconAnchorX = total_width/2,
        iconAnchorY = total_height - 1  # Anker an der Waffel-Spitze
      )
    }, error = function(e) {
      return(NULL)  # Fallback auf Circle-Marker
    })
  }
  
  calculate_marker_sizes <- function(values, method = "linear", min_size = 0.4, max_size = 2.5) {
    if (all(is.na(values)) || all(values == 0, na.rm = TRUE)) return(rep(min_size, length(values)))
    
    valid_values <- values[!is.na(values) & values > 0]
    if (length(valid_values) == 0) return(rep(min_size, length(values)))
    
    if (method == "linear") {
      range_vals <- range(valid_values, na.rm = TRUE)
      if (range_vals[1] == range_vals[2]) {
        sizes <- rep((min_size + max_size) / 2, length(values))
      } else {
        normalized <- (values - range_vals[1]) / (range_vals[2] - range_vals[1])
        sizes <- min_size + normalized * (max_size - min_size)
      }
    } else if (method == "log") {
      log_vals <- log1p(values)
      range_log <- range(log_vals[!is.na(log_vals) & log_vals > 0], na.rm = TRUE)
      if (range_log[1] == range_log[2]) {
        sizes <- rep((min_size + max_size) / 2, length(values))
      } else {
        normalized <- (log_vals - range_log[1]) / (range_log[2] - range_log[1])
        sizes <- min_size + normalized * (max_size - min_size)
      }
    }
    
    sizes[is.na(values) | values == 0] <- 0
    return(sizes)
  }
  
  standardize_values <- function(data_matrix, method = "none") {
    if (method == "none") return(data_matrix)
    
    result <- data_matrix
    if (method == "minmax") {
      for (j in 1:ncol(result)) {
        col_vals <- result[, j]
        range_vals <- range(col_vals, na.rm = TRUE)
        if (range_vals[1] != range_vals[2]) result[, j] <- (col_vals - range_vals[1]) / (range_vals[2] - range_vals[1])
      }
    } else if (method == "zscore") {
      for (j in 1:ncol(result)) {
        col_vals <- result[, j]
        col_mean <- mean(col_vals, na.rm = TRUE)
        col_sd <- sd(col_vals, na.rm = TRUE)
        if (col_sd > 0) result[, j] <- (col_vals - col_mean) / col_sd
      }
    } else if (method == "percent_site") {
      row_sums <- rowSums(result, na.rm = TRUE)
      row_sums[row_sums == 0] <- 1
      result <- result / row_sums * 100
    } else if (method == "percent_type") {
      col_sums <- colSums(result, na.rm = TRUE)
      col_sums[col_sums == 0] <- 1
      result <- sweep(result, 2, col_sums, "/") * 100
    }
    return(result)
  }
  
  distribute_markers_circular <- function(coords, radius = 0.001) {
    tolerance <- 0.00001
    coord_groups <- list()
    used <- rep(FALSE, nrow(coords))
    
    for (i in 1:nrow(coords)) {
      if (used[i]) next
      nearby <- which(!used & abs(coords$lon - coords$lon[i]) < tolerance & abs(coords$lat - coords$lat[i]) < tolerance)
      if (length(nearby) > 1) {
        coord_groups[[length(coord_groups) + 1]] <- nearby
        used[nearby] <- TRUE
      } else used[i] <- TRUE
    }
    
    result_coords <- coords
    for (group in coord_groups) {
      n_markers <- length(group)
      if (n_markers <= 1) next
      
      center_lon <- mean(coords$lon[group])
      center_lat <- mean(coords$lat[group])
      dynamic_radius <- radius * sqrt(n_markers)
      angles <- seq(0, 2*pi, length.out = n_markers + 1)[1:n_markers]
      
      for (j in 1:n_markers) {
        result_coords$lon[group[j]] <- center_lon + dynamic_radius * cos(angles[j])
        result_coords$lat[group[j]] <- center_lat + dynamic_radius * sin(angles[j])
      }
    }
    return(result_coords)
  }
  
  # PREPARE DATA
  typenkartierung_data <- reactive({
    if (is.null(meta_data$data) || is.null(data_raw()) || is.null(mapping_meta())) return(NULL)
    
    # Site-Filter
    m <- if (!is.null(meta_data$sites) && "Selected" %in% names(meta_data$sites)) {
      sel_sites <- meta_data$sites$Entity[meta_data$sites$Selected]
      mapping_meta()[mapping_meta()$site %in% sel_sites, , drop = FALSE]
    } else mapping_meta()
    
    if (nrow(m) == 0) return(NULL)
    
    # Koordinaten validieren
    m$lon <- suppressWarnings(as.numeric(m$lon))
    m$lat <- suppressWarnings(as.numeric(m$lat))
    valid_coords <- sum(is.finite(m$lon) & is.finite(m$lat))
    
    if (valid_coords == 0) {
      types_matrix_no_coords <- as.matrix(data_raw()[, -1, drop = FALSE])
      rownames(types_matrix_no_coords) <- data_raw()[, 1]
      return(list(meta = m, types_matrix = types_matrix_no_coords,
                  available_types = colnames(data_raw())[-1], coord_info = m, status = "no_coords"))
    }
    
    # Typ-Matrix vorbereiten
    types_matrix <- as.matrix(data_raw()[, -1, drop = FALSE])
    rownames(types_matrix) <- data_raw()[, 1]
    
    # Typ-Statistiken
    available_types <- colnames(types_matrix)
    type_stats <- data.frame(
      type = available_types,
      total_abundance = colSums(types_matrix, na.rm = TRUE),
      presence_count = colSums(types_matrix > 0, na.rm = TRUE),
      max_value = apply(types_matrix, 2, max, na.rm = TRUE),
      stringsAsFactors = FALSE)
    type_stats <- type_stats[order(type_stats$total_abundance, decreasing = TRUE), ]
    
    list(meta = m, types_matrix = types_matrix, available_types = available_types,
         type_stats = type_stats, coord_info = m, status = "ready")
  })
  
  # UI FOR TYPE SELECTION
  output$type_selection_ui <- renderUI({
    data_current <- typenkartierung_data()
    if (is.null(data_current) || data_current$status != "ready") {
      return(div(tr("typenkartierung.no.data"), style = "color: red;"))
    }
    
    type_stats <- data_current$type_stats
    all_types <- type_stats$type
    top5 <- head(type_stats$type, 5)
    flop5 <- tail(type_stats$type, 5)
    
    tagList(
      h5(tr("typenkartierung.type.selection.title"), style = "margin-top: 0; color: #1976d2; font-weight: bold;"),
      div(style = "margin-bottom: 10px;",
          fluidRow(
            column(3, actionButton("type_select_all", tr("typenkartierung.btn.all"), class = "btn-success btn-sm", style = "width: 100%;")),
            column(3, actionButton("type_select_none", tr("typenkartierung.btn.none"), class = "btn-warning btn-sm", style = "width: 100%;")),
            column(3, actionButton("type_select_top5", tr("typenkartierung.btn.top5"), class = "btn-info btn-sm", style = "width: 100%;")),
            column(3, actionButton("type_select_flop5", tr("typenkartierung.btn.flop5"), class = "btn-secondary btn-sm", style = "width: 100%;")))),
      selectInput("selected_types", tr("typenkartierung.select.types"),
                  choices = setNames(all_types, paste0(all_types, " (", tr("typenkartierung.sum"), "=", type_stats$total_abundance, ", Sites=", type_stats$presence_count, ")")),
                  selected = character(0), multiple = TRUE, size = 10, selectize = FALSE),
      div(style = "margin-top: 10px; padding: 8px; background: #e3f2fd; border-radius: 4px; border-left: 3px solid #2196f3;",
          textOutput("type_selection_info")))
  })
  
  # SCHNELLAUSWAHL EVENT HANDLER
  observeEvent(input$type_select_all, {
    data_current <- typenkartierung_data()
    if (!is.null(data_current) && data_current$status == "ready") {
      all_types <- head(data_current$type_stats$type, 10)
      updateSelectInput(session, "selected_types", selected = all_types)
    }
  })
  
  observeEvent(input$type_select_none, {
    updateSelectInput(session, "selected_types", selected = character(0))
  })
  
  observeEvent(input$type_select_top5, {
    data_current <- typenkartierung_data()
    if (!is.null(data_current) && data_current$status == "ready") {
      top5 <- head(data_current$type_stats$type, 5)
      updateSelectInput(session, "selected_types", selected = top5)
    }
  })
  
  observeEvent(input$type_select_flop5, {
    data_current <- typenkartierung_data()
    if (!is.null(data_current) && data_current$status == "ready") {
      flop5 <- tail(data_current$type_stats$type, 5)
      updateSelectInput(session, "selected_types", selected = flop5)
    }
  })
  
  # INFO TEXT FOR TYPE SELECTION
  output$type_selection_info <- renderText({
    selected_count <- length(input$selected_types %||% character(0))
    if (selected_count == 0) return(tr("typenkartierung.no.types.selected"))
    else if (selected_count > 10) return(paste(tr("typenkartierung.too.many.types"), selected_count))
    else return(sprintf(tr("typenkartierung.types.selected"), selected_count))
  })
  
  # KARTE INITIALISIEREN
  output$type_map <- leaflet::renderLeaflet({
    m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = FALSE, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
      leaflet::setView(lng = 12.5, lat = 48.8, zoom = 8) %>%
      add_tiles_with_gray("https://maps-for-free.com/layer/relief/z{z}/row{y}/{z}_{x}-{y}.jpg", 
                         gray = TRUE, group = "base", 
                         attribution = 'Relief &copy; <a href="https://maps-for-free.com/">maps-for-free.com</a> (CC0)') %>%
      leaflet::addTiles(urlTemplate = "https://maps-for-free.com/layer/water/z{z}/row{y}/{z}_{x}-{y}.gif",
                       group = "water_overlay",
                       options = leaflet::tileOptions(attribution = "Water © maps-for-free.com (CC0)", opacity = 0.7)) %>%
      add_gray_mutation_observer() %>%
      add_scale()
    m
  })
  
  # BASEMAP AKTUALISIEREN
  observe({
    basemap_choice <- input$type_map_basemap %||% "OpenTopoMap_Color"
    proxy <- leaflet::leafletProxy("type_map") %>% leaflet::clearGroup("base")
    
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
    show_water <- isTRUE(input$type_map_show_water)
    proxy <- leaflet::leafletProxy("type_map") %>% leaflet::clearGroup("water_overlay")
    
    if (show_water) {
      proxy <- proxy %>% leaflet::addTiles(
        urlTemplate = "https://maps-for-free.com/layer/water/z{z}/row{y}/{z}_{x}-{y}.gif",
        group = "water_overlay",
        options = leaflet::tileOptions(attribution = "Water © maps-for-free.com (CC0)", opacity = 0.7))
    }
  })
  
  # Main observer for updating type markers on map
  observe({
    # Explicitly depend on all relevant inputs to ensure reactivity
    data_current <- typenkartierung_data()
    selected_types <- input$selected_types
    # Also track inputs from closed panels (they use %||% defaults below)
    std_mode <- input$type_map_standardization
    disp_mode <- input$type_map_display_mode
    null_disp <- input$type_map_null_display
    size_meth <- input$type_map_size_method
    marker_sc <- input$type_map_marker_scale
    show_leg <- input$type_map_show_legend
    auto_zm <- input$type_map_auto_zoom

    if (is.null(data_current) || data_current$status != "ready" || is.null(selected_types) || length(selected_types) == 0) {
      leaflet::leafletProxy("type_map") %>% leaflet::clearGroup("type_markers") %>% leaflet::clearControls()
      return()
    }

    selected_types <- unique(selected_types)

    if (length(selected_types) > 10) {
      selected_types <- head(selected_types, 10)
      updateSelectInput(session, "selected_types", selected = selected_types)
    }

    mm <- data_current$meta
    types_matrix <- data_current$types_matrix
    has_coords <- is.finite(mm$lon) & is.finite(mm$lat)
    if (!any(has_coords)) {
      leaflet::leafletProxy("type_map") %>% leaflet::clearGroup("type_markers") %>% leaflet::clearControls()
      return()
    }
    mm <- mm[has_coords, , drop = FALSE]

    common_sites <- intersect(mm$site, rownames(types_matrix))

    mm <- mm[mm$site %in% common_sites, , drop = FALSE]
    if (nrow(mm) == 0) {
      leaflet::leafletProxy("type_map") %>% leaflet::clearGroup("type_markers") %>% leaflet::clearControls()
      return()
    }
    types_matrix_filtered <- types_matrix[mm$site, selected_types, drop = FALSE]
    
    # Standardization and display mode
    standardization <- input$type_map_standardization %||% "none"
    if (standardization != "none") types_matrix_filtered <- standardize_values(types_matrix_filtered, standardization)
    
    display_mode <- input$type_map_display_mode %||% "presence"
    if (display_mode == "presence") types_matrix_filtered[types_matrix_filtered > 0] <- 1
    
    null_display <- input$type_map_null_display %||% "hide"
    size_method <- input$type_map_size_method %||% "linear"
    marker_scale <- input$type_map_marker_scale %||% 1.0
    min_size <- 0.6; max_size <- 2.0
    
    # Colors and proxy
    type_colors <- get_type_colors(length(selected_types))
    names(type_colors) <- selected_types
    proxy <- leaflet::leafletProxy("type_map") %>% leaflet::clearGroup("type_markers") %>% leaflet::clearControls()
    all_markers <- data.frame()
    
    # Generate marker data
    for (i in 1:length(selected_types)) {
      type_name <- selected_types[i]
      type_color <- type_colors[type_name]
      type_values <- types_matrix_filtered[, type_name]
      has_type <- !is.na(type_values) & type_values > 0
      
      if (!any(has_type) && null_display == "hide") next
      
      if (null_display == "show_gray") {
        marker_sites <- mm
        marker_values <- type_values
        marker_values[is.na(marker_values)] <- 0
      } else {
        marker_sites <- mm[has_type, , drop = FALSE]
        marker_values <- type_values[has_type]
      }
      
      if (nrow(marker_sites) == 0) next
      
      sizes <- if (display_mode == "presence") rep(1.0, length(marker_values)) else calculate_marker_sizes(marker_values, size_method, min_size, max_size)
      if (null_display == "show_gray") sizes[marker_values == 0] <- 0.4
      
      for (j in 1:nrow(marker_sites)) {
        if (null_display == "hide" && (is.na(marker_values[j]) || marker_values[j] == 0)) next
        
        marker_color <- if (null_display == "show_gray" && marker_values[j] == 0) "#CCCCCC" else type_color
        
        all_markers <- rbind(all_markers, data.frame(
          lon = marker_sites$lon[j], lat = marker_sites$lat[j], site = marker_sites$site[j],
          type = type_name, value = marker_values[j], size = sizes[j], color = marker_color, stringsAsFactors = FALSE))
      }
    }
    
    if (nrow(all_markers) == 0) {
      proxy <- add_scale(proxy)
      return()
    }
    
    # Handle overlap
    overlap_handling <- input$type_map_overlap_handling %||% "distribute"
    if (overlap_handling == "distribute") all_markers[c("lon", "lat")] <- distribute_markers_circular(all_markers[c("lon", "lat")])
    
    # Add markers - EXTENDED with teardrop pins
    for (i in 1:nrow(all_markers)) {
      marker <- all_markers[i, ]
      hover_text <- paste0("<b>", marker$site, "</b><br/>", tr("term.type"), ": ", marker$type, "<br/>", tr("term.value"), ": ", round(marker$value, 2),
                          "<br/>", tr("term.coordinates"), ": ", round(marker$lat, 5), ", ", round(marker$lon, 5))
      
      # PUSHPIN MARKERS WITH DEBUG
      marker_style <- input$type_map_marker_style %||% "pins"
      
      if (marker_style %in% c("pins", "pushpins")) {
        # Stecknadel-Pins versuchen
        pushpin_icon <- create_pushpin_icon(marker$color, marker$size, marker_scale)
        
        if (!is.null(pushpin_icon)) {
          # SVG-Pin erfolgreich erstellt
          proxy <- proxy %>% leaflet::addMarkers(
            lng = marker$lon, 
            lat = marker$lat,
            icon = pushpin_icon,
            popup = hover_text,
            group = "type_markers"
          )
        } else {
          # SVG fehlgeschlagen - Circle-Marker als Fallback
          radius <- 8 + (marker$size * 10 * marker_scale)
          proxy <- proxy %>% leaflet::addCircleMarkers(
            lng = marker$lon, lat = marker$lat, radius = radius,
            fillColor = marker$color, color = "white", weight = 2,
            fillOpacity = 0.9, opacity = 1, popup = hover_text, group = "type_markers"
          )
        }
      }
      else {
        # Circle markers directly (when "circles" selected)
        radius <- 8 + (marker$size * 8 * marker_scale)
        proxy <- proxy %>% leaflet::addCircleMarkers(
          lng = marker$lon, lat = marker$lat, radius = radius,
          fillColor = marker$color, color = "white", weight = 2,
          fillOpacity = 0.9, opacity = 1, popup = hover_text, group = "type_markers"
        )
      }
    }
    
    # Legende
    if (isTRUE(input$type_map_show_legend)) {
      legend_colors <- type_colors[selected_types]
      proxy <- proxy %>% leaflet::addLegend(position = "bottomright", colors = legend_colors, labels = selected_types,
                                           title = tr("typenkartierung.legend.title"), opacity = 0.8)
    }
    
    # Auto-Zoom
    if (isTRUE(input$type_map_auto_zoom)) {
      bbox <- c(min(all_markers$lon, na.rm = TRUE), min(all_markers$lat, na.rm = TRUE),
                max(all_markers$lon, na.rm = TRUE), max(all_markers$lat, na.rm = TRUE))
      
      if (all(is.finite(bbox)) && bbox[1] < bbox[3] && bbox[2] < bbox[4]) {
        padding <- 0.1
        lon_padding <- (bbox[3] - bbox[1]) * padding
        lat_padding <- (bbox[4] - bbox[2]) * padding
        bbox_padded <- c(bbox[1] - lon_padding, bbox[2] - lat_padding, bbox[3] + lon_padding, bbox[4] + lat_padding)
        proxy <- proxy %>% leaflet::fitBounds(bbox_padded[1], bbox_padded[2], bbox_padded[3], bbox_padded[4], options = list(maxZoom = 12))
      }
    }
    
    proxy <- add_scale(proxy)
  })
  
  # EXPORT FUNCTIONS
  build_export_type_map <- function(base_choice = NULL) {
    # Select basemap for export
    if (is.null(base_choice)) base_choice <- input$type_map_basemap %||% "OpenTopoMap_Color"
    
    data_current <- typenkartierung_data()
    selected_types <- input$selected_types
    
    m <- leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE, zoomSnap = 0.1, zoomDelta = 0.1)) %>%
      add_gray_mutation_observer()
    
    # Add basemap
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
    
    m <- add_scale(m) %>% leaflet::setView(lng = 12.5, lat = 48.8, zoom = 8)
    
    # ADD MARKER DATA FOR EXPORT
    if (!is.null(data_current) && data_current$status == "ready" && !is.null(selected_types) && length(selected_types) > 0) {
      selected_types_limited <- head(selected_types, 10)
      
      mm <- data_current$meta
      types_matrix <- data_current$types_matrix
      has_coords <- is.finite(mm$lon) & is.finite(mm$lat)
      
      if (any(has_coords)) {
        mm <- mm[has_coords, , drop = FALSE]
        
                common_sites <- intersect(mm$site, rownames(types_matrix))
        mm <- mm[mm$site %in% common_sites, , drop = FALSE]
        
        if (nrow(mm) > 0) {
          types_matrix_filtered <- types_matrix[mm$site, selected_types_limited, drop = FALSE]
          # Get parameters from input
          standardization <- input$type_map_standardization %||% "none"
          if (standardization != "none") types_matrix_filtered <- standardize_values(types_matrix_filtered, standardization)
          
          display_mode <- input$type_map_display_mode %||% "presence"
          if (display_mode == "presence") types_matrix_filtered[types_matrix_filtered > 0] <- 1
          
          marker_scale <- input$type_map_marker_scale %||% 1.0
          size_method <- input$type_map_size_method %||% "linear"
          
          # Generate colors and markers
          type_colors <- get_type_colors(length(selected_types_limited))
          names(type_colors) <- selected_types_limited
          
          all_markers <- data.frame()
          
          for (i in 1:length(selected_types_limited)) {
            type_name <- selected_types_limited[i]
            type_color <- type_colors[type_name]
            type_values <- types_matrix_filtered[, type_name]
            has_type <- !is.na(type_values) & type_values > 0
            
            if (any(has_type)) {
              marker_sites <- mm[has_type, , drop = FALSE]
              marker_values <- type_values[has_type]
              
              sizes <- if (display_mode == "presence") rep(1.0, length(marker_values)) else calculate_marker_sizes(marker_values, size_method, 0.6, 2.0)
              
              for (j in 1:nrow(marker_sites)) {
                all_markers <- rbind(all_markers, data.frame(
                  lon = marker_sites$lon[j], lat = marker_sites$lat[j], site = marker_sites$site[j],
                  type = type_name, value = marker_values[j], size = sizes[j], color = type_color, stringsAsFactors = FALSE
                ))
              }
            }
          }
          
          # Add markers to map
          if (nrow(all_markers) > 0) {
            for (k in 1:nrow(all_markers)) {
              marker <- all_markers[k, ]
              hover_text <- paste0("<b>", marker$site, "</b><br/>", tr("term.type"), ": ", marker$type, "<br/>", tr("term.value"), ": ", round(marker$value, 2))
              
              # Ice cream cone pins for export
              pushpin_icon <- create_pushpin_icon(marker$color, marker$size, marker_scale)
              
              if (!is.null(pushpin_icon)) {
                m <- m %>% leaflet::addMarkers(
                  lng = marker$lon, lat = marker$lat,
                  icon = pushpin_icon, popup = hover_text, group = "type_markers"
                )
              } else {
                # Fallback: Circle-Marker
                radius <- 8 + (marker$size * 10 * marker_scale)
                m <- m %>% leaflet::addCircleMarkers(
                  lng = marker$lon, lat = marker$lat, radius = radius,
                  fillColor = marker$color, color = "white", weight = 2,
                  fillOpacity = 0.9, opacity = 1, popup = hover_text, group = "type_markers"
                )
              }
            }
            
            # Add legend to export
            legend_colors <- type_colors[selected_types_limited]
            m <- m %>% leaflet::addLegend(
              position = "bottomright", colors = legend_colors, labels = selected_types_limited,
              title = tr("typenkartierung.legend.title"), opacity = 0.8
            )
          }
        }
      }
    }
    
    return(m)
  }
  
  # TYP-STATISTIKEN TABELLE
  output$type_statistics_table <- DT::renderDataTable({
    data_current <- typenkartierung_data()
    if (is.null(data_current) || data_current$status != "ready") return(data.frame(Info = tr("typenkartierung.no.data")))

    type_stats <- data_current$type_stats
    DT::datatable(type_stats, options = list(pageLength = 15, scrollX = TRUE, columnDefs = list(list(className = 'dt-center', targets = "_all"))),
                  rownames = FALSE, colnames = c(tr("term.type"), tr("typenkartierung.table.total"), tr("typenkartierung.table.sites"), tr("typenkartierung.table.max")),
                  class = 'cell-border stripe hover') %>% DT::formatRound(c('total_abundance', 'max_value'), digits = 2)
  })
  
  # DOWNLOAD HANDLER: HTML EXPORT (Interactive Leaflet Map)
  output$download_type_map_html <- downloadHandler(
    filename = function() paste0("SeriARC_Typenkartierung_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".html"),
    content = function(file) {
      showNotification("🗺 Creating HTML Export...", type = "message", duration = 3)
      
      tryCatch({
        m <- build_export_type_map()
        htmlwidgets::saveWidget(m, file = file, selfcontained = TRUE)
        showNotification("✅ HTML Export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("⚠ HTML Export Error:", e$message), type = "error", duration = 5)
      })
    }
  )
  
  # PNG EXPORT with template and real data
  output$download_type_map_png <- create_png_download_handler(
    "type_map", "SeriARC_Typenkartierung", session,
    plot_data = reactive({ 
      data_current <- typenkartierung_data()
      selected_types <- input$selected_types
      list(data = data_current, selected_types = selected_types)
    }),
    plot_generator_func = function(export_data) {
      if (!is.null(export_data$data) && !is.null(export_data$selected_types) && 
          export_data$data$status == "ready" && length(export_data$selected_types) > 0) {
        
        # Use real type mapping data for PNG
        data_current <- export_data$data
        selected_types <- head(export_data$selected_types, 10)
        
        mm <- data_current$meta
        mm$lon <- suppressWarnings(as.numeric(mm$lon))
        mm$lat <- suppressWarnings(as.numeric(mm$lat))
        has_coords <- is.finite(mm$lon) & is.finite(mm$lat)
        
        if (any(has_coords)) {
          mm <- mm[has_coords, , drop = FALSE]
          types_matrix <- data_current$types_matrix[mm$site, selected_types, drop = FALSE]
          
          par(mar = c(4, 4, 4, 2))
          
          # Simple type distribution map for PNG export
          type_colors <- get_type_colors(length(selected_types))

          # Base plot
          plot(mm$lon, mm$lat, type = "n",
               xlab = "Longitude", ylab = "Latitude",
               main = paste("SeriARC Type Mapping (", length(selected_types), "Types)"))
          
          # Add points for each type
          for (i in 1:length(selected_types)) {
            type_name <- selected_types[i]
            type_values <- types_matrix[, type_name]
            has_type <- !is.na(type_values) & type_values > 0
            
            if (any(has_type)) {
              type_sites <- mm[has_type, ]
              point_sizes <- pmax(0.5, pmin(2.5, sqrt(type_values[has_type]) * 0.8))
              
              points(type_sites$lon, type_sites$lat, 
                     col = type_colors[i], pch = 16, 
                     cex = point_sizes)
            }
          }
          
          # Legende
          legend("topright", legend = selected_types, 
                 col = type_colors[1:length(selected_types)], 
                 pch = 16, cex = 0.8, bg = "white")
          
          # Grid for orientation
          grid(col = "lightgray", lty = "dotted")
          
          # Info-Text
          mtext(sprintf("PNG Export | %d Sites, %d Types | %s", 
                       nrow(mm), length(selected_types), format(Sys.time(), "%Y-%m-%d %H:%M")), 
                side = 1, line = 3, cex = 0.7, col = "gray")
        }
      }
    }
  )
  
  # SVG Export with template
  output$download_type_map_svg <- create_svg_download_handler(
    "type_map", "SeriARC_Typenkartierung", session,
    plot_data = reactive({ 
      data_current <- typenkartierung_data()
      selected_types <- input$selected_types
      list(data = data_current, selected_types = selected_types)
    }),
    plot_generator_func = function(export_data) {
      if (!is.null(export_data$data) && !is.null(export_data$selected_types) && 
          export_data$data$status == "ready" && length(export_data$selected_types) > 0) {
        
        # SVG export with real data
        data_current <- export_data$data
        selected_types <- head(export_data$selected_types, 10)
        
        mm <- data_current$meta
        mm$lon <- suppressWarnings(as.numeric(mm$lon))
        mm$lat <- suppressWarnings(as.numeric(mm$lat))
        has_coords <- is.finite(mm$lon) & is.finite(mm$lat)
        
        if (any(has_coords)) {
          mm <- mm[has_coords, , drop = FALSE]
          types_matrix <- data_current$types_matrix[mm$site, selected_types, drop = FALSE]
          
          par(mar = c(4, 4, 4, 2))
          
          # SVG type mapping
          type_colors <- get_type_colors(length(selected_types))
          
          plot(mm$lon, mm$lat, type = "n",
               xlab = "Longitude", ylab = "Latitude",
               main = paste("SeriARC Type Mapping - SVG (", length(selected_types), "Types)"))
          
          for (i in 1:length(selected_types)) {
            type_name <- selected_types[i]
            type_values <- types_matrix[, type_name]
            has_type <- !is.na(type_values) & type_values > 0
            
            if (any(has_type)) {
              type_sites <- mm[has_type, ]
              point_sizes <- pmax(0.8, pmin(2.0, sqrt(type_values[has_type]) * 1.0))
              
              points(type_sites$lon, type_sites$lat, 
                     col = type_colors[i], pch = 16, 
                     cex = point_sizes)
            }
          }
          
          legend("topright", legend = selected_types, 
                 col = type_colors[1:length(selected_types)], 
                 pch = 16, cex = 0.9, bg = "white")
          
          grid(col = "lightgray", lty = "dotted")
          
          mtext(sprintf("SVG Export: %d Sites, %d Typen", 
                       nrow(mm), length(selected_types)), 
                side = 1, line = 3, cex = 0.8, col = "gray")
        }
      }
    }
  )
  
  # DOWNLOAD HANDLER: EXCEL EXPORT (Type coordinates and statistics)
  output$download_type_data_excel <- downloadHandler(
    filename = function() paste0("SeriARC_Typenkartierung_Daten_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx"),
    content = function(file) {
      showNotification("📋 Creating Excel Export...", type = "message", duration = 3)
      
      tryCatch({
        data_current <- typenkartierung_data()
        selected_types <- input$selected_types %||% character(0)
        
        if (is.null(data_current) || data_current$status != "ready") {
          # Fallback: Empty data
          data_list <- list(
            "SeriARC_Info" = data.frame(
              Parameter = c("Software", "Version", "Export_Type", "Status", "Created_at"),
              Value = c("SeriARC", "v1.0.0", "Type Mapping", "No data available",
                      format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
            ),
            "Note" = data.frame(Info = "No type mapping data available")
          )
        } else {
          # Prepare Excel data
          data_list <- list()

          # SeriARC Info Sheet
          additional_info <- list(
            "Analysis_Type" = "Spatial Artifact Distribution",
            "Selected_Types" = if(length(selected_types) > 0) paste(head(selected_types, 10), collapse = ", ") else "None",
            "Type_Count" = length(selected_types),
            "Standardization" = input$type_map_standardization %||% "none",
            "Display_Mode" = input$type_map_display_mode %||% "presence",
            "Coordinates_Status" = paste(sum(is.finite(data_current$meta$lon) & is.finite(data_current$meta$lat)), "of", nrow(data_current$meta), "Sites")
          )
          
          data_list[["SeriARC_Info"]] <- data.frame(
            Parameter = c("Software", "Version", "Export_Type", "Created_at", names(additional_info)),
            Value = c("SeriARC", "v1.0.0", "Type Mapping", format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                    as.character(additional_info))
          )
          
          # Typ-Statistiken
          if (!is.null(data_current$type_stats)) {
            type_stats_export <- data_current$type_stats
            type_stats_export$selected <- type_stats_export$type %in% selected_types
            data_list[["Typ_Statistiken"]] <- type_stats_export[order(type_stats_export$total_abundance, decreasing = TRUE), ]
          }
          
          # Site coordinates with type data (selected types only)
          if (length(selected_types) > 0 && nrow(data_current$meta) > 0) {
            coords_with_types <- data_current$meta[c("site", "lon", "lat")]
            
                        coords_with_types <- coords_with_types[coords_with_types$site %in% rownames(data_current$types_matrix), , drop = FALSE]
            
            selected_types_limited <- head(selected_types, 10)
            
            # Add type values
            for (type_name in selected_types_limited) {
              if (type_name %in% colnames(data_current$types_matrix)) {
                type_values <- data_current$types_matrix[coords_with_types$site, type_name]
                coords_with_types[[type_name]] <- ifelse(is.na(type_values), 0, type_values)
              }
            }
            
            colnames(coords_with_types) <- c("Site", "Longitude", "Latitude", selected_types_limited)
            data_list[["Site_Koordinaten_Typen"]] <- coords_with_types
          }
          
          # Koordinaten-Statistiken
          mm <- data_current$meta
          mm$lon <- suppressWarnings(as.numeric(mm$lon))
          mm$lat <- suppressWarnings(as.numeric(mm$lat))
          valid_coords <- sum(is.finite(mm$lon) & is.finite(mm$lat))
          
          data_list[["Coordinates_Status"]] <- data.frame(
            Metric = c("Total Sites", "Sites with valid coordinates", "Sites without coordinates",
                      "Valid Coordinates %", "Longitude Min", "Longitude Max", "Latitude Min", "Latitude Max"),
            Value = c(
              nrow(mm),
              valid_coords,
              nrow(mm) - valid_coords,
              round(valid_coords / nrow(mm) * 100, 1),
              if(valid_coords > 0) round(min(mm$lon, na.rm = TRUE), 5) else "N/A",
              if(valid_coords > 0) round(max(mm$lon, na.rm = TRUE), 5) else "N/A",
              if(valid_coords > 0) round(min(mm$lat, na.rm = TRUE), 5) else "N/A",
              if(valid_coords > 0) round(max(mm$lat, na.rm = TRUE), 5) else "N/A"
            )
          )
        }
        
        writexl::write_xlsx(data_list, file)

        showNotification("✅ Excel Export completed!", type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste("⚠ Excel Export Error:", e$message), type = "error", duration = 5)
      })
    }
  )
  
  return(list(typenkartierung_data = typenkartierung_data, build_export_type_map = build_export_type_map))
}

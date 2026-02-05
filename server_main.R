# server.R - SeriARC v1.0.0 - Archaeological Seriation and Correspondence Analysis

server <- function(input, output, session) {

  # === CORE SETUP ===
  session$allowReconnect(TRUE)

  # === LANGUAGE SWITCHING (i18n) ===
  # Since language switch triggers a reload, tr is non-reactive
  query <- parseQueryString(isolate(session$clientData$url_search))
  current_lang <- if (!is.null(query$lang) && query$lang %in% AVAILABLE_LANGUAGES) {
    query$lang
  } else {
    DEFAULT_LANGUAGE
  }
  session$userData$lang <- current_lang

  # tr function for this session (non-reactive since language switch = reload)
  tr <- make_tr(current_lang)

  message("Server Session: lang=", current_lang)

  source("helpers/server_logic/server_reactive_values.R", local = TRUE)
  source("helpers/server_logic/server_helpers.R", local = TRUE)
  
  # === DATA MANAGEMENT MODULES ===
  import <- mod_data_import_server("import", tr = tr)
  data_raw <- import$data_raw
  mapping_meta <- import$mapping_meta
  mapping_coord_info <- import$mapping_coord_info
  mapping_raw <- import$mapping_raw
  mapping_type_groups <- import$mapping_type_groups
  file_status <- import$file_status
  is_example_data <- import$is_example_data
  
  filtermod <- mod_data_filter_server("filter",
                                      data_raw = data_raw,
                                      sites_selection = sites_selection_reactive,
                                      types_selection = types_selection_reactive,
                                      meta = meta,
                                      filter_trigger = filter_trigger,
                                      tr = tr)
  
  filtered_data <- filtermod$filtered_data
  filtered_meta <- filtermod$meta_data
  
  # === AGGREGATION WRAPPER ===
  # Applies site and type aggregations to filtered data
  aggregated_data <- reactive({
    req(filtered_data())
    
    site_agg_list <- site_aggregations$list
    type_agg_list <- type_aggregations$list
    
    result <- filtered_data()
    
    if (!is.null(site_agg_list) && length(site_agg_list) > 0) {
      result <- apply_aggregations(result, site_agg_list)
    }

    if (!is.null(type_agg_list) && length(type_agg_list) > 0) {
      result <- apply_type_aggregations(result, type_agg_list)
    }
    
    return(result)
  })
  
  aggregated_meta_reactive <- reactive({
    req(meta$data)
    filter_trigger()
    return(meta$data)
  })

  aggregated_meta <- reactiveValues(data = NULL)
  observe({
    aggregated_meta$data <- aggregated_meta_reactive()
  }, priority = 200)
  
  # === ANALYSIS MODULES ===
  # Use aggregated_meta for ALL modules!
  # aggregated_meta always contains the current metadata:
  # - WITHOUT aggregations: meta$data (immediate selected updates)
  # - WITH aggregations: aggregated metadata

  # Create a reactive reference for 14C data that will be populated later
  # This allows the CA module to access 14C data even though it's initialized first
  c14_calibrated_ref <- reactiveVal(NULL)
  oxcal_results_ref <- reactiveVal(NULL)

  ca_module <- mod_correspondence_analysis_server(
    filtered_data = aggregated_data,
    meta_data = aggregated_meta,  # Immer aggregated_meta - enthält aktuelle Daten!
    cache = cache,
    get_site_group = get_site_group,
    get_element_details = get_element_details,
    c14_calibrated_reactive = reactive({ c14_calibrated_ref() }),  # Reactive reference, updated by observer below
    oxcal_results_reactive = reactive({ oxcal_results_ref() }),  # OxCal results for modelled overlay
    input = input, output = output, session = session,
    tr = tr
  )

  # Note: c14_calibrated_ref and oxcal_results_ref are updated by observers after modules are created (see below)
  
  # Detrended CA module
  detrended_ca_module <- mod_detrended_ca_server(
    filtered_data = aggregated_data,
    meta_data = aggregated_meta,
    cache = cache,
    get_site_group = get_site_group,
    get_element_details = get_element_details,
    input = input, output = output, session = session,
    tr = tr
  )
  
  bootstrap_module <- mod_bootstrap_server(
    filtered_data = aggregated_data,
    meta_data = aggregated_meta,
    cache = cache,
    get_site_group = get_site_group,
    get_element_details = get_element_details,
    ca_result = ca_module$ca_result,
    input = input, output = output, session = session,
    tr = tr
  )
  
  kmeans_module <- mod_kmeans_clustering_server(
    ca_result = ca_module$ca_result,
    cache = cache,
    get_site_group = get_site_group,
    get_element_details = get_element_details,
    meta_data = aggregated_meta,
    filtered_data = aggregated_data,  # Original raw data for type characterization analysis
    input = input, output = output, session = session,
    tr = tr
  )
  
  ca_3d_module <- mod_3d_ca_server(
    ca_result = ca_module$ca_result,
    cache = cache,
    get_site_group = get_site_group,
    cluster_names = kmeans_module$cluster_names,
    cluster_colors = kmeans_module$cluster_colors,
    get_element_details = get_element_details,
    input = input, output = output, session = session,
    tr = tr
  )
  
  seriation_module <- mod_seriation_server(
    filtered_data = aggregated_data,
    cache = cache,
    get_site_group = get_site_group,
    get_element_details = get_element_details,
    input = input, output = output, session = session,
    tr = tr
  )
  
  battleship_module <- mod_battleship_server(
    filtered_data = aggregated_data,
    cache = cache,
    cluster_names = kmeans_module$cluster_names,
    cluster_colors = kmeans_module$cluster_colors,
    get_site_group = get_site_group,
    get_element_details = get_element_details,
    input = input, output = output, session = session,
    tr = tr
  )
  
  # === MAPPING MODULES ===
  mapping_module <- mod_mapping_server(
    meta_data = aggregated_meta,
    data_raw = data_raw,
    mapping_meta = mapping_meta,
    site_clusters = site_clusters,
    cluster_names = kmeans_module$cluster_names,
    cluster_colors = kmeans_module$cluster_colors,
    input = input, output = output, session = session,
    tr = tr
  )
  
  # === TYPE MAPPING MODULE ===
  typenkartierung_module <- mod_typenkartierung_server(
    meta_data = aggregated_meta,
    data_raw = data_raw,
    mapping_meta = mapping_meta,
    site_clusters = site_clusters,
    cluster_names = kmeans_module$cluster_names,
    cluster_colors = kmeans_module$cluster_colors,
    input = input, output = output, session = session,
    tr = tr
  )
  
  # === EXPORT MODULE ===
  export_module <- mod_export_server(
    filtered_data = aggregated_data,
    meta_data = aggregated_meta,
    ca_result = ca_module$ca_result,
    kmeans_result = kmeans_module$kmeans_result,
    seriation_result = seriation_module$seriation_result,
    input = input, output = output, session = session,
    tr = tr
  )
  
  # === UI CONTENT MODULES ===
  source("helpers/ui_content/ui_import_content.R", local = TRUE)
  source("helpers/ui_content/ui_filter_content.R", local = TRUE)
  source("helpers/ui_content/ui_aggregation_content.R", local = TRUE)
  source("helpers/ui_content/ui_ca_content.R", local = TRUE)
  source("helpers/ui_content/ui_detrended_ca_content.R", local = TRUE)
  source("helpers/ui_content/ui_bootstrap_content.R", local = TRUE)
  source("helpers/ui_content/ui_3d_ca_content.R", local = TRUE)
  source("helpers/ui_content/ui_cluster_content.R", local = TRUE)
  source("helpers/ui_content/ui_cluster_help_modal.R", local = TRUE)  # Clustering help modal
  source("helpers/ui_content/ui_seriation_content.R", local = TRUE)
  source("helpers/ui_content/ui_battleship_content.R", local = TRUE)
  source("helpers/ui_content/ui_mapping_content.R", local = TRUE)
  source("helpers/ui_content/ui_typemap_content.R", local = TRUE)
  source("helpers/ui_content/ui_chronology_content.R", local = TRUE)
  
  # === 14C INTEGRATION MODULES ===

  # Import & Mapping module
  chronology_module <- mod_c14_server("chronology",
                                      filtered_data = filtered_data,
                                      meta_data = meta,
                                      tr = tr
  )
  
  # Calibration module
  c14_cal <- mod_c14_calibration_server(
    "c14cal",
    c14_raw_reactive = chronology_module$c14_data,
    mapping_applied_reactive = chronology_module$mapping_applied,
    tr = tr
  )

  # Update the reactive reference when calibration data is available
  # This bridges the gap between module initialization order
  observe({
    cal_data <- tryCatch(c14_cal$get_calibrated(), error = function(e) NULL)
    c14_calibrated_ref(cal_data)
  })
  
  # Chronology bridge
  chronology_curve_module <- mod_chronology_curve_server(
    "chrono",
    ca_scores_reactive = ca_module$get_ca_scores,
    c14_data_reactive = chronology_module$c14_data,
    tr = tr
  )
  
  # OxCal Sequence module
  oxcal_module <- mod_oxcal_seq_server(
    "oxseq",
    c14_table_reactive = chronology_module$c14_data,
    chrono_curve_reactive = chronology_curve_module$get_chrono_curve,
    rv = NULL,
    cache = cache,
    mapping = mapping_meta,
    cluster_names = kmeans_module$cluster_names,
    cluster_colors = kmeans_module$cluster_colors,
    tr = tr
  )

  # Update the OxCal results reference when results are available
  observe({
    oxcal_data <- tryCatch({
      if (!is.null(oxcal_module) && !is.null(oxcal_module$get_results)) {
        oxcal_module$get_results()
      } else {
        NULL
      }
    }, error = function(e) NULL)
    oxcal_results_ref(oxcal_data)
  })
  
  # === 14C OVERLAY MODULE FOR CA ===
  # Separate module for 14C label overlay on CA plot
  # Uses direct reference to calibration module's reactive
  c14_overlay_module <- mod_ca_c14_overlay_server(
    "c14_overlay",
    c14_calibrated_reactive = c14_cal$get_calibrated,  # Direct reactive reference
    oxcal_results_reactive = reactive({
      # Get OxCal results if available (via module interface)
      tryCatch({
        if (!is.null(oxcal_module) && !is.null(oxcal_module$get_results)) {
          oxcal_module$get_results()
        } else {
          NULL
        }
      }, error = function(e) NULL)
    }),
    plot_data_reactive = ca_module$ca_plot_data,
    parent_input = input,  # Pass parent input for show_c14_overlay access
    tr = tr
  )
  
  # === SERVER LOGIC MODULES ===
  source("helpers/server_logic/server_observers.R", local = TRUE)
  source("helpers/server_logic/server_status_outputs.R", local = TRUE)
  source("helpers/server_logic/server_download_handlers.R", local = TRUE)
  source("helpers/server_logic/server_aggregation.R", local = TRUE)
  
  # === SELECTION TABLE MODULE ===
  source("helpers/server_logic/server_selection_tables.R", local = TRUE)
  
  # === UI MODULES AND HELPER COMPONENTS ===
  source("helpers/ui_components.R", local = TRUE)
}

# INPUT COMPONENTS HELPER - SeriARC V1.5
# Reusable input groups and UI control elements

# === DIMENSION SELECTION COMPONENTS ===

# Standard CA dimensions selection UI
# @param tr Optional translator function for i18n
ca_dimensions_ui <- function(ns, ca_result_reactive, max_dims = 10, include_info = TRUE, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  # Info button for scientific explanation
  info_button <- if (include_info) {
    actionButton(
      paste0(ns(""), "info_dimensions"),
      label = "",
      icon = icon("info-circle"),
      style = "background: none; border: none; color: #3498db; padding: 0; margin-left: 5px;",
      onclick = sprintf("Shiny.setInputValue('%s', Math.random());", paste0(ns(""), "show_info_dimensions"))
    )
  } else NULL

  tagList(
    h4(paste0("\U0001F4D0 ", .tr("input.dim.selection", "Dimension Selection")), info_button),
    uiOutput(paste0(ns(""), "dim_select_ui"))
  )
}

# Server logic for dimension selection
ca_dimensions_server <- function(output, ca_result_reactive, ns_prefix = "", max_dims = 10, default_z = TRUE) {
  ns_func <- if (ns_prefix != "") function(x) paste0(ns_prefix, x) else function(x) x
  
  output[[paste0(ns_prefix, "dim_select_ui")]] <- renderUI({
    req(ca_result_reactive())
    res <- ca_result_reactive()
    n_dims <- min(nrow(res$eig), max_dims)
    
    # Choices with variance information
    dim_choices <- setNames(
      paste0("Dim", 1:n_dims),
      sprintf("Dim%d (%.1f%%)", 1:n_dims, res$eig[1:n_dims, 2])
    )
    
    inputs <- list(
      selectInput(
        ns_func("x_dim"), 
        "X-Achse:", 
        choices = dim_choices, 
        selected = "Dim1"
      ),
      selectInput(
        ns_func("y_dim"), 
        "Y-Achse:", 
        choices = dim_choices, 
        selected = ifelse(n_dims >= 2, "Dim2", "Dim1")
      )
    )
    
    # Z-axis for 3D plots
    if (default_z && n_dims >= 3) {
      inputs <- append(inputs, list(
        selectInput(
          ns_func("z_dim"), 
          "Z-Achse:", 
          choices = dim_choices, 
          selected = "Dim3"
        )
      ))
    }
    
    do.call(tagList, inputs)
  })
}

# === PLOT-DARSTELLUNGS-OPTIONEN ===

# Standard Plot-Anzeigeoptionen UI
# @param tr Optional translator function for i18n
plot_display_options_ui <- function(ns, include_labels = TRUE, include_colors = TRUE, include_elements = TRUE, include_size = TRUE, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  options <- list()

  if (include_elements) {
    options <- append(options, list(
      h5(paste0("\U0001F3AF ", .tr("input.display", "Display:"))),
      selectInput(
        paste0(ns(""), "plot_elements"),
        .tr("input.elements", "Elements:"),
        choices = c(
          setNames("both", .tr("input.sites.types", "Sites & Types")),
          setNames("sites_only", .tr("input.sites.only", "Sites only")),
          setNames("types_only", .tr("input.types.only", "Types only"))
        ),
        selected = "both"
      )
    ))
  }

  if (include_colors) {
    options <- append(options, list(
      checkboxInput(
        paste0(ns(""), "color_by_group"),
        .tr("input.color.by.group", "Color by groups"),
        value = FALSE
      )
    ))
  }

  if (include_labels) {
    options <- append(options, list(
      checkboxInput(
        paste0(ns(""), "show_labels"),
        paste0("\U0001F3F7\UFE0F ", .tr("input.show.labels", "Show labels")),
        value = TRUE
      ),
      conditionalPanel(
        condition = sprintf("input['%s']", paste0(ns(""), "show_labels")),
        sliderInput(
          paste0(ns(""), "label_chars"),
          .tr("input.label.length", "Label length:"),
          min = 3, max = 20, value = 12, step = 1
        )
      )
    ))
  }

  if (include_size) {
    options <- append(options, list(
      sliderInput(
        paste0(ns(""), "point_size"),
        .tr("input.point.size", "Point size:"),
        min = 1, max = 8, value = 3, step = 1
      )
    ))
  }

  div(class = "plot-options-panel", options)
}

# === ADVANCED PLOT OPTIONS ===

# Advanced CA plot options UI
# @param tr Optional translator function for i18n
ca_advanced_options_ui <- function(ns, include_ellipses = TRUE, include_contributions = TRUE, include_transformations = FALSE, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  options <- list(h5(.tr("input.advanced.options", "Advanced Options:")))

  if (include_ellipses) {
    options <- append(options, list(
      checkboxInput(
        paste0(ns(""), "show_confidence_ellipses"),
        .tr("input.show.ellipses", "Show 95% confidence ellipses"),
        value = FALSE
      )
    ))
  }

  if (include_contributions) {
    options <- append(options, list(
      checkboxInput(
        paste0(ns(""), "highlight_contributions"),
        .tr("input.highlight.contributions", "Highlight high contributions"),
        value = FALSE
      )
    ))
  }

  if (include_transformations) {
    options <- append(options, list(
      h5(.tr("input.matrix.transform", "Matrix Transformation:")),
      selectInput(
        paste0(ns(""), "transform_sites"),
        "Sites:",
        choices = c(
          setNames("none", .tr("input.transform.none", "None")),
          setNames("normalize", .tr("input.transform.normalize", "Normalize")),
          setNames("log", .tr("input.transform.log", "Log-Transform"))
        ),
        selected = "none"
      ),
      selectInput(
        paste0(ns(""), "transform_types"),
        "Types:",
        choices = c(
          setNames("none", .tr("input.transform.none", "None")),
          setNames("normalize", .tr("input.transform.normalize", "Normalize")),
          setNames("log", .tr("input.transform.log", "Log-Transform"))
        ),
        selected = "none"
      )
    ))
  }

  div(class = "advanced-options-panel", options)
}

# === K-MEANS SPECIFIC OPTIONS ===

# K-Means cluster options UI
# @param tr Optional translator function for i18n
kmeans_options_ui <- function(ns, max_clusters = 10, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  tagList(
    h4(.tr("input.clustering.params", "Clustering Parameters")),

    fluidRow(
      column(6,
        numericInput(
          paste0(ns(""), "num_clusters"),
          .tr("input.num.clusters", "Number of clusters:"),
          min = 2, max = max_clusters, value = 3, step = 1
        )
      ),
      column(6,
        selectInput(
          paste0(ns(""), "cluster_on"),
          .tr("input.cluster.on", "Cluster on:"),
          choices = c(
            setNames("both", .tr("input.sites.types", "Sites & Types")),
            setNames("rows", .tr("input.sites.only", "Sites only")),
            setNames("cols", .tr("input.types.only", "Types only"))
          ),
          selected = "both"
        )
      )
    ),

    h5(.tr("input.display.options", "Display Options:")),
    checkboxInput(
      paste0(ns(""), "show_cluster_centers"),
      .tr("input.show.cluster.centers", "Show cluster centers"),
      value = TRUE
    ),

    checkboxInput(
      paste0(ns(""), "show_confidence_ellipses"),
      .tr("input.show.confidence.ellipses", "Show confidence ellipses"),
      value = TRUE
    ),

    # Cluster name inputs (dynamically generated)
    h5(.tr("input.cluster.names", "Cluster Names:")),
    div(id = paste0(ns(""), "cluster_names_container"),
        uiOutput(paste0(ns(""), "cluster_name_inputs"))
    ),

    div(class = "btn-group btn-group-sm",
      actionButton(
        paste0(ns(""), "reset_cluster_names"),
        .tr("input.reset.names", "Reset names"),
        class = "btn btn-outline-warning btn-sm"
      )
    )
  )
}

# === BOOTSTRAP SPECIFIC OPTIONS ===

# Bootstrap analysis options UI
# @param tr Optional translator function for i18n
bootstrap_options_ui <- function(ns, default_iterations = 999, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  tagList(
    h4(.tr("input.bootstrap.params", "Bootstrap Parameters")),

    fluidRow(
      column(6,
        numericInput(
          paste0(ns(""), "bootstrap_iterations"),
          .tr("input.iterations", "Iterations:"),
          min = 99, max = 9999, value = default_iterations, step = 100
        )
      ),
      column(6,
        numericInput(
          paste0(ns(""), "bootstrap_confidence"),
          .tr("input.confidence", "Confidence (%):"),
          min = 80, max = 99, value = 95, step = 1
        )
      )
    ),

    selectInput(
      paste0(ns(""), "bootstrap_resampling"),
      .tr("input.resampling.method", "Resampling method:"),
      choices = c(
        "Poisson" = "poisson",
        "Multinomial" = "multinomial"
      ),
      selected = "poisson"
    ),

    h5(.tr("input.display.elements", "Display Elements:")),
    selectInput(
      paste0(ns(""), "bootstrap_elements"),
      .tr("input.bootstrap.for", "Bootstrap for:"),
      choices = c(
        setNames("both", .tr("input.sites.types", "Sites & Types")),
        setNames("sites", .tr("input.sites.only", "Sites only")),
        setNames("types", .tr("input.types.only", "Types only"))
      ),
      selected = "both"
    ),

    checkboxInput(
      paste0(ns(""), "show_confidence_ellipses_bootstrap"),
      .tr("input.show.confidence.ellipses", "Show confidence ellipses"),
      value = TRUE
    ),

    checkboxInput(
      paste0(ns(""), "highlight_stability"),
      .tr("input.highlight.stability", "Highlight stability"),
      value = TRUE
    )
  )
}

# === SERIATION OPTIONS ===

# Seriation options UI
# @param tr Optional translator function for i18n
seriation_options_ui <- function(ns, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  tagList(
    h4(.tr("input.seriation.params", "Seriation Parameters")),

    selectInput(
      paste0(ns(""), "seriation_method"),
      .tr("input.seriation.method", "Seriation method:"),
      choices = c(
        "Barycenter (Ihm)" = "Barycenter",
        setNames("CA", .tr("input.ca.based", "CA-based")),
        setNames("PCA_angle", .tr("input.pca.based", "PCA-based")),
        "ARSA" = "ARSA"
      ),
      selected = "Barycenter"
    ),

    selectInput(
      paste0(ns(""), "seriation_criterion"),
      .tr("input.optimization.criterion", "Optimization criterion:"),
      choices = c(
        "Stress" = "stress",
        "Gradient" = "gradient",
        "Inertia" = "inertia"
      ),
      selected = "stress"
    ),

    h5(.tr("input.matrix.transform", "Matrix Transformation:")),
    selectInput(
      paste0(ns(""), "seriation_transform"),
      .tr("input.transformation", "Transformation:"),
      choices = c(
        setNames("none", .tr("input.transform.none", "None")),
        setNames("pa", .tr("input.presence.absence", "Presence/Absence")),
        setNames("log", .tr("input.transform.log", "Log-Transform")),
        setNames("percent", .tr("input.percentage", "Percentage"))
      ),
      selected = "none"
    ),

    h5(.tr("input.visualization", "Visualization:")),
    selectInput(
      paste0(ns(""), "seriation_scale"),
      .tr("input.scaling", "Scaling:"),
      choices = c(
        setNames("linear", .tr("input.linear", "Linear")),
        setNames("log", .tr("input.logarithmic", "Logarithmic")),
        setNames("sqrt", .tr("input.square.root", "Square root"))
      ),
      selected = "linear"
    ),

    checkboxInput(
      paste0(ns(""), "show_seriation_order"),
      .tr("input.show.order.index", "Show order index"),
      value = TRUE
    )
  )
}

# === FILTER CONTROL OPTIONS ===

# Standard filter control UI
# @param tr Optional translator function for i18n
filter_controls_ui <- function(ns, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  tagList(
    h4(.tr("input.filter.controls", "Filter Controls")),

    fluidRow(
      column(6,
        h5(.tr("input.sites.selection", "Sites Selection:")),
        div(class = "btn-group btn-group-sm mb-2", style = "width: 100%;",
          actionButton(
            paste0(ns(""), "select_all_sites"),
            .tr("input.all", "All"),
            class = "btn btn-success btn-sm",
            style = "flex: 1;"
          ),
          actionButton(
            paste0(ns(""), "deselect_all_sites"),
            .tr("input.none", "None"),
            class = "btn btn-danger btn-sm",
            style = "flex: 1;"
          )
        )
      ),
      column(6,
        h5(.tr("input.types.selection", "Types Selection:")),
        div(class = "btn-group btn-group-sm mb-2", style = "width: 100%;",
          actionButton(
            paste0(ns(""), "select_all_types"),
            .tr("input.all", "All"),
            class = "btn btn-success btn-sm",
            style = "flex: 1;"
          ),
          actionButton(
            paste0(ns(""), "deselect_all_types"),
            .tr("input.none", "None"),
            class = "btn btn-danger btn-sm",
            style = "flex: 1;"
          ),
          actionButton(
            paste0(ns(""), "reset_weights"),
            "Weights",
            class = "btn btn-warning btn-sm",
            style = "flex: 1;"
          )
        )
      )
    ),

    h5(.tr("input.data.quality.filter", "Data Quality Filter:")),
    fluidRow(
      column(6,
        numericInput(
          paste0(ns(""), "min_site_count"),
          .tr("input.min.sites.per.type", "Min. sites per type:"),
          min = 1, max = 50, value = 2, step = 1
        )
      ),
      column(6,
        numericInput(
          paste0(ns(""), "min_type_count"),
          .tr("input.min.types.per.site", "Min. types per site:"),
          min = 1, max = 50, value = 2, step = 1
        )
      )
    ),

    sliderInput(
      paste0(ns(""), "max_sparsity"),
      .tr("input.max.sparsity", "Max. sparsity (%):"),
      min = 0, max = 100, value = 80, step = 5
    )
  )
}

# === EXPORT OPTIONS ===

# Plot export options UI
# @param tr Optional translator function for i18n
plot_export_options_ui <- function(ns, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  tagList(
    h5(paste0("\U0001F4D0 ", .tr("input.export.settings", "Export Settings:"))),
    fluidRow(
      column(6,
        numericInput(
          paste0(ns(""), "plot_width"),
          .tr("input.width.px", "Width (px):"),
          min = 600, max = 3000, value = 1200, step = 100
        )
      ),
      column(6,
        numericInput(
          paste0(ns(""), "plot_height"),
          .tr("input.height.px", "Height (px):"),
          min = 400, max = 2000, value = 800, step = 100
        )
      )
    ),

    selectInput(
      paste0(ns(""), "export_format"),
      .tr("input.format", "Format:"),
      choices = c(
        "PNG (Web)" = "png",
        "SVG (Vector)" = "svg",
        "PDF (Print)" = "pdf"
      ),
      selected = "png"
    ),

    numericInput(
      paste0(ns(""), "export_dpi"),
      "DPI (PDF/PNG):",
      min = 72, max = 600, value = 300, step = 50
    )
  )
}

# === KOMBINIERTE OPTIONEN-PANELS ===

# Standard CA-Analyse-Panel (komplett)
# @param tr Optional translator function for i18n
ca_analysis_panel_ui <- function(ns, include_advanced = TRUE, include_export = FALSE, tr = NULL) {
  panels <- list(
    # Basis-Optionen
    plot_display_options_ui(ns,
                           include_labels = TRUE,
                           include_colors = TRUE,
                           include_elements = TRUE,
                           include_size = TRUE,
                           tr = tr)
  )

  if (include_advanced) {
    panels <- append(panels, list(
      br(),
      ca_advanced_options_ui(ns,
                            include_ellipses = TRUE,
                            include_contributions = TRUE,
                            include_transformations = TRUE,
                            tr = tr)
    ))
  }

  if (include_export) {
    panels <- append(panels, list(
      br(),
      plot_export_options_ui(ns, tr = tr)
    ))
  }

  do.call(tagList, panels)
}

# Standard K-Means-Analyse-Panel (komplett)
# @param tr Optional translator function for i18n
kmeans_analysis_panel_ui <- function(ns, max_clusters = 10, include_export = FALSE, tr = NULL) {
  panels <- list(
    kmeans_options_ui(ns, max_clusters, tr = tr),
    br(),
    plot_display_options_ui(ns,
                           include_labels = TRUE,
                           include_colors = FALSE,
                           include_elements = FALSE,
                           include_size = TRUE,
                           tr = tr)
  )

  if (include_export) {
    panels <- append(panels, list(
      br(),
      plot_export_options_ui(ns, tr = tr)
    ))
  }

  do.call(tagList, panels)
}

# === UTILITY FUNCTIONS ===

# Standard input validation for dimension selection
validate_dimension_inputs <- function(x_dim, y_dim, z_dim = NULL, max_dims) {
  errors <- c()

  if (is.null(x_dim) || x_dim == "") {
    errors <- c(errors, "X-Dimension must be selected")
  }

  if (is.null(y_dim) || y_dim == "") {
    errors <- c(errors, "Y-Dimension must be selected")
  }

  if (!is.null(z_dim) && z_dim == "") {
    errors <- c(errors, "Z-Dimension must be selected")
  }

  # Check for unique dimensions
  dims <- c(x_dim, y_dim, z_dim)
  dims <- dims[!is.null(dims) & dims != ""]

  if (length(dims) != length(unique(dims))) {
    errors <- c(errors, "Dimensions must be different")
  }

  # Check dimension range
  dim_numbers <- as.numeric(gsub("Dim", "", dims))
  if (any(dim_numbers > max_dims, na.rm = TRUE)) {
    errors <- c(errors, sprintf("Dimensions > %d not available", max_dims))
  }
  
  if (length(errors) > 0) {
    validate(need(FALSE, paste(errors, collapse = " | ")))
  }
  
  TRUE
}

# Input-Extraktion Helper
extract_dimension_indices <- function(x_dim, y_dim, z_dim = NULL) {
  x_idx <- as.numeric(gsub("Dim", "", x_dim %||% "1"))
  y_idx <- as.numeric(gsub("Dim", "", y_dim %||% "2"))
  z_idx <- if (!is.null(z_dim)) as.numeric(gsub("Dim", "", z_dim)) else NULL
  
  list(x = x_idx, y = y_idx, z = z_idx)
}

# Scientifically sound default values
get_analysis_defaults <- function(analysis_type = "CA") {
  defaults <- list(
    "CA" = list(
      point_size = 3,
      label_chars = 12,
      show_labels = TRUE,
      color_by_group = FALSE,
      show_confidence_ellipses = FALSE,
      plot_width = 1200,
      plot_height = 800
    ),
    "KMeans" = list(
      num_clusters = 3,
      point_size = 3,
      label_chars = 12,
      show_labels = TRUE,
      show_cluster_centers = TRUE,
      show_confidence_ellipses = TRUE,
      cluster_on = "both"
    ),
    "Bootstrap" = list(
      bootstrap_iterations = 999,
      bootstrap_confidence = 95,
      bootstrap_resampling = "poisson",
      show_confidence_ellipses_bootstrap = TRUE,
      highlight_stability = TRUE
    ),
    "Seriation" = list(
      seriation_method = "Barycenter",
      seriation_criterion = "stress", 
      seriation_transform = "none",
      seriation_scale = "linear",
      show_seriation_order = TRUE
    )
  )
  
  defaults[[analysis_type]] %||% defaults[["CA"]]
}

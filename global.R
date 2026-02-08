# SeriARC v1.0.0 - Archaeological Seriation and Correspondence Analysis
# Author: Daniel Meixner (University of Regensburg)
# License: GPL-3.0

source("config.R", local = TRUE)
source("helpers/error_handling.R", local = TRUE)
source("helpers/logging.R", local = TRUE)

APP_VERSION <- "1.0.0"
APP_NAME <- "SeriARC"

# Cloud environment detection (shinyapps.io, Posit Connect)
detect_cloud_environment <- function() {
  # 1) Env variables set by shinyapps.io / Posit Connect
  is_shinyapps <- nzchar(Sys.getenv("SHINYAPPS_ACCOUNT")) ||
                  nzchar(Sys.getenv("SHINYAPPS_APPLICATION"))
  is_connect <- nzchar(Sys.getenv("CONNECT_SERVER")) ||
                nzchar(Sys.getenv("RSTUDIO_CONNECT_HASTE"))
  # 2) Path-based: shinyapps.io runs under /srv/connect/ or similar
  wd <- getwd()
  is_cloud_path <- grepl("/srv/|/opt/connect|/home/shiny|shinyapps", wd, ignore.case = TRUE)
  # 3) SeriARC runs locally only on Windows — shinyapps.io is always Linux
  is_not_windows <- .Platform$OS.type != "windows"

  is_shinyapps || is_connect || is_cloud_path || is_not_windows
}

RUNNING_ON_CLOUD <- detect_cloud_environment()

# Required packages
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(rhandsontable)
library(readxl)
library(sf)
library(leaflet)
library(ggplot2)
library(ggrepel)
library(FactoMineR)
library(RColorBrewer)
library(dplyr)
library(shinyjs)
library(writexl)
library(reshape2)
library(viridis)
library(cluster)
library(khroma)
library(purrr)
# Internationalization (i18n) Setup - Custom Implementation

.translations <- tryCatch({
  csv_path <- "translations/translations.csv"
  if (!file.exists(csv_path)) {
    message("i18n: CSV file not found: ", csv_path)
    return(list())
  }

  df <- read.csv(csv_path,
                 stringsAsFactors = FALSE,
                 fileEncoding = "UTF-8",
                 check.names = FALSE,
                 sep = ";")

  trans_list <- list()
  for (i in seq_len(nrow(df))) {
    key <- as.character(df[i, "key"])
    trans_list[[key]] <- list(
      de = as.character(df[i, "de"]),
      en = as.character(df[i, "en"])
    )
  }

  message("i18n: ", length(trans_list), " translations loaded")
  trans_list
}, error = function(e) {
  message("i18n: Error loading translations: ", e$message)
  list()
})

# Global language selection
AVAILABLE_LANGUAGES <- c("de", "en")
DEFAULT_LANGUAGE <- "de"

# make_tr() - Factory function for translations
# Creates a tr() function bound to a specific language
# Usage: tr <- make_tr("en"); tr("nav.data") -> "Data"
make_tr <- function(lang) {
  if (!lang %in% AVAILABLE_LANGUAGES) lang <- DEFAULT_LANGUAGE

  function(key) {
    if (!is.null(.translations[[key]]) && !is.null(.translations[[key]][[lang]])) {
      return(.translations[[key]][[lang]])
    }
    # Fallback: Mark key with [[]] for visible error detection
    paste0("[[", key, "]]")
  }
}

# tr_server() - For server context with session$userData$lang
# Usage: tr_server(session, "key")
tr_server <- function(session, key) {
  lang <- session$userData$lang %||% DEFAULT_LANGUAGE
  if (!is.null(.translations[[key]]) && !is.null(.translations[[key]][[lang]])) {
    return(.translations[[key]][[lang]])
  }
  paste0("[[", key, "]]")
}

# data.table thread safety for Shiny
if (requireNamespace("data.table", quietly = TRUE)) {
  data.table::setDTthreads(1)
}

# OxCal setup (optional - only for local installations)
oxcal_setup_success <- FALSE

if (!RUNNING_ON_CLOUD && requireNamespace("oxcAAR", quietly = TRUE)) {
  OXCAL_LOCAL_PATH <- "C:\\Program Files\\OxCal\\bin\\OxCalWin.exe"
  
  if (file.exists(OXCAL_LOCAL_PATH)) {
    tryCatch({
      oxcAAR::setOxcalExecutablePath(OXCAL_LOCAL_PATH)
      oxcal_setup_success <- TRUE
    }, error = function(e) NULL)
  }
  
  if (!oxcal_setup_success && nzchar(Sys.getenv("OXCAL_PATH"))) {
    tryCatch({
      oxcAAR::setOxcalExecutablePath(Sys.getenv("OXCAL_PATH"))
      oxcal_setup_success <- TRUE
    }, error = function(e) NULL)
  }
  
  # Skip quickSetupOxcal - too slow for cloud deployment
}

# Optional 14C packages
if (requireNamespace("princurve", quietly = TRUE)) {
  library(princurve)
}

if (requireNamespace("rcarbon", quietly = TRUE)) {
  library(rcarbon)
}

# SVG support detection
detect_svg_support <- function() {
  svglite_available <- requireNamespace("svglite", quietly = TRUE)
  cairo_available <- capabilities("cairo")
  list(
    svglite = svglite_available,
    cairo = cairo_available,
    ggplot2 = TRUE,
    available = svglite_available || cairo_available || TRUE
  )
}

svg_support <- detect_svg_support()
if (svg_support$svglite) library(svglite)

# Seriation helper function
compute_seriation <- function(mat, method = "ca_combined", transform = "none", binary_threshold = 0) {
  mat_clean <- as.matrix(mat)
  mat_clean[is.na(mat_clean)] <- 0
  mat_clean[!is.finite(mat_clean)] <- 0

  if (transform == "binary") {
    threshold <- if (is.null(binary_threshold) || is.na(binary_threshold)) 0 else binary_threshold
    mat_transformed <- ifelse(mat_clean > threshold, 1, 0)
  } else if (transform == "relative") {
    row_sums <- rowSums(mat_clean, na.rm = TRUE)
    row_sums[row_sums == 0] <- 1
    mat_transformed <- sweep(mat_clean, 1, row_sums, FUN = "/")
  } else if (transform == "log") {
    mat_transformed <- log(mat_clean + 1)
  } else {
    mat_transformed <- mat_clean
  }

  ca_res <- FactoMineR::CA(mat_transformed, graph = FALSE)
  row_scores <- ca_res$row$coord[, 1]
  col_scores <- ca_res$col$coord[, 1]
  row_order_idx <- order(row_scores, decreasing = FALSE, na.last = NA)
  col_order_idx <- order(col_scores, decreasing = FALSE, na.last = NA)
  permuted_mat <- mat_transformed[row_order_idx, col_order_idx, drop = FALSE]

  list(
    permuted_mat = permuted_mat,
    permuted_matrix = permuted_mat,
    row_order = rownames(permuted_mat),
    col_order = colnames(permuted_mat),
    method_name = {
      method_names <- sapply(SERIATION_METHODS, identity)
      found_name <- names(method_names)[method_names == method]
      if (length(found_name) > 0) found_name[1] else "Seriation"
    },
    transformation = transform
  )
}

# Shiny options
options(
  shiny.maxRequestSize = MAX_FILE_SIZE_MB * 1024^2,
  shiny.usecairo = TRUE,
  shiny.trace = DEBUG_MODE,
  shiny.autoreload = DEBUG_MODE,
  digits = 4,
  warn = if(DEBUG_MODE) 2 else 1
)

# Color palette utilities (colorblind-safe with khroma)
seri_arc_make_palette <- function(n, use_khroma = TRUE) {
  if (use_khroma && requireNamespace("khroma", quietly = TRUE)) {
    tryCatch({
      n_safe <- min(n, 8L)
      if (n_safe <= 8) {
        pal <- as.character(khroma::colour("okabeito")(n_safe))
        if (n > 8) {
          extra_colors <- as.character(khroma::colour("muted")(n - 8))
          pal <- c(pal, extra_colors)
        }
        return(pal)
      }
      n_bright <- min(n, 7L)
      return(as.character(khroma::colour("bright")(n_bright)))
    }, error = function(e) NULL)
  }

  if (requireNamespace("RColorBrewer", quietly = TRUE)) {
    pals <- c("Set2", "Set3", "Dark2", "Paired")
    for (p in pals) {
      maxn <- tryCatch(RColorBrewer::brewer.pal.info[p, "maxcolors"], error = function(e) NA)
      if (!is.na(maxn) && is.finite(maxn) && n <= maxn) {
        return(RColorBrewer::brewer.pal(n, p))
      }
    }
  }

  grDevices::rainbow(n)
}

seri_arc_factor_colors <- function(fct, use_khroma = TRUE) {
  lv <- sort(unique(stats::na.omit(as.character(fct))))
  if (length(lv) == 0) return(list(fct = factor(fct), cols = character(0)))
  cols <- seri_arc_make_palette(length(lv), use_khroma)
  fct2 <- factor(fct, levels = lv)
  list(fct = fct2, cols = stats::setNames(cols, lv))
}

seri_arc_add_overlay_points <- function(p, df, aes_x, aes_y, cluster_col,
                                        show_labels = FALSE, label_col = NULL,
                                        size = 7, symbol_active = "circle", symbol_suppl = "x",
                                        color_na = "#888888") {
  cc <- seri_arc_factor_colors(df[[cluster_col]])
  clust_fct <- cc$fct
  cols_map <- cc$cols
  act <- df[isFALSE(df$is_suppl) | is.na(df$is_suppl), ]
  if (nrow(act)) {
    act$col <- cols_map[as.character(factor(act[[cluster_col]], levels = levels(clust_fct)))]
    p <- plotly::add_markers(
      p, data = act,
      x = act[[aes_x]], y = act[[aes_y]],
      marker = list(size = size, symbol = symbol_active),
      color = I(act$col),
      hoverinfo = "text",
      text = act$hover
    )
    if (show_labels && !is.null(label_col) && label_col %in% names(act)) {
      p <- plotly::add_text(
        p, data = act,
        x = act[[aes_x]], y = act[[aes_y]],
        text = act[[label_col]],
        mode = "text",
        textposition = "top center",
        hoverinfo = "none", showlegend = FALSE
      )
    }
  }
  sup <- df[isTRUE(df$is_suppl), ]
  if (nrow(sup)) {
    p <- plotly::add_markers(
      p, data = sup,
      x = sup[[aes_x]], y = sup[[aes_y]],
      marker = list(size = max(1, size - 2), symbol = symbol_suppl, line = list(width = 1)),
      color = I(color_na),
      hoverinfo = "text",
      text = sup$hover,
      showlegend = FALSE
    )
  }
  if (length(cols_map)) {
    legend_df <- data.frame(
      x = NA_real_, y = NA_real_, lab = names(cols_map), col = unname(cols_map),
      stringsAsFactors = FALSE
    )
    p <- plotly::add_markers(
      p, data = legend_df,
      x = legend_df$x, y = legend_df$y,
      marker = list(size = 0.1),
      name = ~lab,
      color = I(legend_df$col),
      hoverinfo = "none",
      showlegend = TRUE
    )
  }
  p
}

# Coordinate utilities
coerce_num <- function(x) {
  if (is.numeric(x)) return(x)
  x <- as.character(x)
  x <- gsub("\\s+", "", x)
  x <- gsub(",", ".", x, fixed = TRUE)
  x <- gsub("[^0-9.\\-]", "", x)
  suppressWarnings(as.numeric(x))
}

is_numeric_like <- function(x) {
  nx <- coerce_num(x)
  sum(!is.na(nx)) >= max(1, floor(length(nx) * 0.7))
}

detect_coord_type <- function(x, y) {
  xx <- coerce_num(x); yy <- coerce_num(y)
  if (all(is.na(xx)) || all(is.na(yy))) return(list(type = NA, order = NA))
  wgs_lonlat <- mean(!is.na(xx) & xx >= -180 & xx <= 180) >= 0.7 &&
    mean(!is.na(yy) & yy >= -90 & yy <= 90) >= 0.7
  wgs_latlon <- mean(!is.na(xx) & xx >= -90 & xx <= 90) >= 0.7 &&
    mean(!is.na(yy) & yy >= -180 & yy <= 180) >= 0.7
  if (wgs_lonlat || wgs_latlon) {
    ord <- if (wgs_lonlat) "lonlat" else "latlon"
    return(list(type = "wgs84", order = ord))
  }
  easting_x <- mean(!is.na(xx) & xx >= 100000 & xx <= 900000) >= 0.7
  northing_y <- mean(!is.na(yy) & yy >= 0 & yy <= 10000000) >= 0.7
  easting_y <- mean(!is.na(yy) & yy >= 100000 & yy <= 900000) >= 0.7
  northing_x <- mean(!is.na(xx) & xx >= 0 & xx <= 10000000) >= 0.7
  if ((easting_x && northing_y) || (easting_y && northing_x)) {
    ord <- if (easting_x && northing_y) "en" else "ne"
    return(list(type = "utm", order = ord))
  }
  list(type = NA, order = NA)
}

utm_to_wgs84 <- function(easting, northing, zone, hemisphere = "N") {
  if (!requireNamespace("sf", quietly = TRUE)) {
    warning("sf package not available")
    return(data.frame(lon = as.numeric(easting), lat = as.numeric(northing)))
  }

  tryCatch({
    utm_crs <- paste0("+proj=utm +zone=", zone,
                      ifelse(hemisphere == "S", " +south", ""),
                      " +datum=WGS84 +units=m +no_defs")

    easting_clean <- as.numeric(easting)
    northing_clean <- as.numeric(northing)
    valid_idx <- !is.na(easting_clean) & !is.na(northing_clean)

    if (!any(valid_idx)) {
      return(data.frame(lon = as.numeric(easting), lat = as.numeric(northing)))
    }

    utm_points <- sf::st_as_sf(
      data.frame(x = easting_clean[valid_idx], y = northing_clean[valid_idx]),
      coords = c("x", "y"),
      crs = utm_crs
    )

    wgs84_points <- sf::st_transform(utm_points, crs = 4326)
    coords <- sf::st_coordinates(wgs84_points)

    result_lon <- rep(NA_real_, length(easting))
    result_lat <- rep(NA_real_, length(northing))
    result_lon[valid_idx] <- coords[, 1]
    result_lat[valid_idx] <- coords[, 2]

    data.frame(lon = result_lon, lat = result_lat)
  }, error = function(e) {
    data.frame(lon = as.numeric(easting), lat = as.numeric(northing))
  })
}

# Package check
required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly", "rhandsontable",
  "readxl", "leaflet", "ggplot2", "ggrepel", "FactoMineR",
  "RColorBrewer", "dplyr", "shinyjs", "writexl", "reshape2",
  "viridis", "cluster", "sf", "khroma", "purrr", "princurve", "oxcAAR"
)
optional_packages <- c("base64enc", "webshot2", "mapview")

missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
if (length(missing_packages) > 0) {
  message("Missing packages: ", paste(missing_packages, collapse = ", "))
}

# Default dataset: Michelsberg
DEFAULT_DATASET <- NULL
DATASET_SOURCE <- NULL

if (requireNamespace("archdata", quietly = TRUE)) {
  tryCatch({
    data("Michelsberg", package = "archdata", envir = environment())
    Michelsberg_with_entity <- data.frame(
      Entity = rownames(Michelsberg),
      Michelsberg,
      stringsAsFactors = FALSE
    )
    rownames(Michelsberg_with_entity) <- NULL
    DEFAULT_DATASET <- Michelsberg_with_entity
    DATASET_SOURCE <- "archdata_package"
  }, error = function(e) NULL)
}

if (is.null(DEFAULT_DATASET) && file.exists("data/Michelsberg_archdata.xlsx")) {
  tryCatch({
    DEFAULT_DATASET <- readxl::read_excel("data/Michelsberg_archdata.xlsx")
    DATASET_SOURCE <- "local_xlsx"
  }, error = function(e) NULL)
}

if (!is.null(DEFAULT_DATASET) && DATASET_SOURCE == "archdata_package") {
  if (!file.exists("data/Michelsberg_archdata.xlsx")) {
    tryCatch({
      writexl::write_xlsx(DEFAULT_DATASET, "data/Michelsberg_archdata.xlsx")
    }, error = function(e) NULL)
  }
}

# Pipe compatibility for R < 4.1
if (getRversion() < "4.1.0") {
  if (!requireNamespace("magrittr", quietly = TRUE)) {
    stop("magrittr required for %>% operator on R < 4.1")
  } else {
    library(magrittr)
  }
}

all_modules <- c(
  "mod_data_import.R",
  "mod_data_filter.R",
  "mod_meta_editor.R",
  "mod_correspondence_analysis.R",
  "mod_detrended_ca.R",
  "mod_bootstrap.R",
  "mod_3d_ca.R",
  "mod_kmeans_clustering.R",
  "mod_seriation.R",
  "mod_battleship.R",
  "mod_mapping.R",
  "mod_typenkartierung.R",
  "mod_export.R"
)

helper_components <- c(
  "helpers/plot_components.R",
  "helpers/download_components.R",
  "helpers/input_components.R",
  "helpers/ui_components.R",
  "helpers/c14_curve_loader.R",
  "helpers/helper_aggregation.R",
  "helpers/helper_robust_outliers.R"
)

for (helper in helper_components) {
  if (file.exists(helper)) source(helper, local = TRUE)
}

for (mod in all_modules) {
  module_path <- file.path("modules", mod)
  if (file.exists(module_path)) source(module_path, local = TRUE)
}

# 14C modules
c14_modules <- c(
  "mod_c14.R",
  "mod_c14_functions.R",
  "mod_c14_calibration.R",
  "mod_ca_c14_overlay.R",  # 14C overlay for CA plot
  "mod_oxcal_seq_utils.R",
  "mod_oxcal_seq_data.R",
  "mod_oxcal_seq_plots.R",
  "mod_oxcal_seq_ui.R",
  "mod_oxcal_seq_server.R"
)

for (c14_mod in c14_modules) {
  c14_module_path <- file.path("modules", c14_mod)
  if (file.exists(c14_module_path)) source(c14_module_path, local = TRUE)
}

# Chronology curve module
chrono_module_path <- file.path("modules", "mod_chronology_curve.R")
if (file.exists(chrono_module_path)) source(chrono_module_path, local = TRUE)

# Startup message
message("\n", APP_NAME, " v", APP_VERSION, " initialized")
if (RUNNING_ON_CLOUD) message("  Running in cloud environment (OxCal execution not available)")
if (oxcal_setup_success) message("  OxCal: available")
if (!is.null(DEFAULT_DATASET)) message("  Example dataset: Michelsberg (archdata)")

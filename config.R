# SeriARC Configuration File
# Central configuration for better maintainability

# VERSION & METADATA

SERIARC_VERSION <- "1.0.0"
SERIARC_BUILD_DATE <- "2025-08-27"
SERIARC_AUTHOR <- "Daniel Meixner"

# FILE & IMPORT SETTINGS

# Supported file formats (currently implemented)
SUPPORTED_FORMATS <- c(".xlsx", ".xls")

# Planned formats (not yet implemented)
PLANNED_FORMATS <- c(".csv", ".txt", ".tsv")

# All theoretically supported formats
ALL_FORMATS <- c(SUPPORTED_FORMATS, PLANNED_FORMATS)

# Maximum file size (in MB)
MAX_FILE_SIZE_MB <- 100

# Default separators for CSV
DEFAULT_CSV_SEPARATOR <- ","
DEFAULT_DECIMAL_SEPARATOR <- "."

# Maximum number of rows/columns
MAX_ROWS <- 10000
MAX_COLS <- 5000

# SERIATION SETTINGS (CA only)

# Default transformations
DEFAULT_TRANSFORM <- "none"
BINARY_DEFAULT_THRESHOLD <- 0

# VISUALIZATION SETTINGS

# Default plot type
DEFAULT_PLOT_TYPE <- "ford"

# Available plot types
PLOT_TYPES <- list(
  "Ford Diagram (Matrix)" = "ford",
  "Abundance Heatmap" = "heatmap",
  "Battleship Curve" = "battleship"
)

# Heatmap color palettes
HEATMAP_PALETTES <- list(
  "Viridis" = "viridis",
  "Plasma" = "plasma",
  "Inferno" = "inferno",
  "Grayscale" = "greys"
)

# Default plot dimensions
PLOT_HEIGHT <- "700px"
PLOT_WIDTH <- "100%"

# Default label settings
DEFAULT_LABEL_CHARS <- 12
MIN_LABEL_CHARS <- 3
MAX_LABEL_CHARS <- 30

# CLUSTER OVERLAY SETTINGS

# Standard cluster colors (scientifically optimized)
CLUSTER_COLORS <- c(
  "#3498db", "#e74c3c", "#2ecc71", "#f39c12", "#9b59b6",
  "#1abc9c", "#e67e22", "#95a5a6", "#34495e", "#f1c40f",
  "#16a085", "#c0392b", "#27ae60", "#d35400", "#8e44ad"
)

# Overlay styles
OVERLAY_STYLES <- list(
  "Axis Bars" = "axis_bars",
  "Transparent Bands" = "transparent_bands",
  "Diagonal Blocks" = "diagonal_blocks"
)

# Default transparency values
DEFAULT_OVERLAY_ALPHA <- 0.25
MIN_OVERLAY_ALPHA <- 0.0
MAX_OVERLAY_ALPHA <- 0.35

# Default diagonal band width (%)
DEFAULT_DIAGONAL_BAND_WIDTH <- 6
MIN_DIAGONAL_BAND_WIDTH <- 1
MAX_DIAGONAL_BAND_WIDTH <- 20

# PERFORMANCE & DEBUGGING

DEBUG_MODE <- FALSE

# Logging level (0=None, 1=Error, 2=Warning, 3=Info, 4=Debug)
LOG_LEVEL <- 2

# Cache settings
ENABLE_CACHE <- TRUE
CACHE_MAX_SIZE <- 1000  # Maximum number of cached objects

# Timeout for long calculations (seconds)
CALCULATION_TIMEOUT <- 300  # 5 minutes

# EXPORT SETTINGS

# Default export formats
EXPORT_FORMATS <- c("PNG", "SVG", "PDF", "Excel")

# Default image resolution
DEFAULT_DPI <- 300
DEFAULT_PNG_WIDTH <- 1200
DEFAULT_PNG_HEIGHT <- 800

# Excel export settings
EXCEL_SHEET_NAMES <- list(
  info = "SeriARC_Info",
  matrix = "Seriated_Matrix",
  sites = "Sites_Order",
  types = "Types_Order"
)

# UI SETTINGS

# CSS classes
CSS_PANEL_CLASS <- "seriarc-panel"
CSS_WARNING_CLASS <- "seriarc-panel-warning"
CSS_INFO_CLASS <- "seriarc-panel-info"
CSS_HELP_CLASS <- "seriarc-help-text"

# Default button classes
BUTTON_PRIMARY <- "btn btn-primary"
BUTTON_WARNING <- "btn btn-warning"
BUTTON_SUCCESS <- "btn btn-success"
BUTTON_INFO <- "btn btn-info"
BUTTON_SECONDARY <- "btn btn-secondary"

# SCIENTIFIC REFERENCES

SCIENTIFIC_REFERENCES <- list(
  goldmann = "Goldmann, K. (1972): Zwei Methoden chronologischer Gruppierung",
  ihm = "Ihm, P. (1983): Statistik in der Archäologie: Probleme der Anwendung, allgemeine Methoden, Seriation und Klassifikation"
)

# HELPER FUNCTIONS

load_config <- function() {
  cat(paste("SeriARC", SERIARC_VERSION, "configuration loaded\n"))

  if (DEBUG_MODE) {
    cat("DEBUG MODE is enabled\n")
  }

  return(TRUE)
}

# Version information
get_version_info <- function() {
  return(paste("SeriARC", SERIARC_VERSION, "from", SERIARC_BUILD_DATE))
}

load_config()

# SeriARC Logging System
# Intelligent logging for debugging and monitoring

# LOGGING FUNCTIONS

#' Main logging function
#' @param level Logging level (0-4: None, Error, Warning, Info, Debug)
#' @param category Category (e.g. "seriation", "import", "cluster")
#' @param message Log message
#' @param details Additional details (optional)
log_message <- function(level, category, message, details = "") {

  # Only log if level is enabled
  if (level > LOG_LEVEL) return(invisible())

  # Timestamp
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")

  # Level names
  level_names <- c("", "ERROR", "WARN", "INFO", "DEBUG")
  level_name <- level_names[level + 1]

  # Emoji for better readability
  level_emoji <- switch(as.character(level),
    "1" = "❌", "2" = "⚠️", "3" = "ℹ️", "4" = "🔧", ""
  )
  
  # Format log entry
  log_entry <- sprintf("%s [%s] %s %s: %s", 
                       timestamp, level_name, level_emoji, 
                       toupper(category), message)
  
  if (nchar(details) > 0) {
    log_entry <- paste(log_entry, "| Details:", details)
  }
  
  # Output
  if (DEBUG_MODE) {
    cat(log_entry, "\n")
  }
  
  if (exists("LOG_TO_FILE") && LOG_TO_FILE) {
    log_file <- "seriarc.log"
    write(log_entry, file = log_file, append = TRUE)
  }
  
  invisible(log_entry)
}

# CONVENIENCE FUNCTIONS

#' Error Logging
#' @param category Category
#' @param message Message
#' @param details Details
log_error <- function(category, message, details = "") {
  log_message(1, category, message, details)
}

#' Warning Logging
#' @param category Category
#' @param message Message
#' @param details Details
log_warning <- function(category, message, details = "") {
  log_message(2, category, message, details)
}

#' Info Logging
#' @param category Category
#' @param message Message
#' @param details Details
log_info <- function(category, message, details = "") {
  log_message(3, category, message, details)
}

#' Debug Logging
#' @param category Category
#' @param message Message
#' @param details Details
log_debug <- function(category, message, details = "") {
  log_message(4, category, message, details)
}

# SPECIFIC LOGGERS FOR SERIARC COMPONENTS

#' Seriation-specific logging
#' @param action Action (e.g. "start", "success", "error")
#' @param method Seriation method
#' @param details Additional details
log_seriation <- function(action, method = "", details = "") {

  message <- switch(action,
    "start" = paste("Seriation started with method:", method),
    "success" = paste("Seriation completed successfully:", method),
    "error" = paste("Seriation error with method:", method),
    "validation_failed" = "Data validation for seriation failed",
    paste("Seriation action:", action)
  )
  
  level <- switch(action,
    "start" = 3,      # Info
    "success" = 3,    # Info  
    "error" = 1,      # Error
    "validation_failed" = 2,  # Warning
    3                 # Default: Info
  )
  
  log_message(level, "seriation", message, details)
}

#' Import-specific logging
#' @param action Action
#' @param filename Filename
#' @param details Details
log_import <- function(action, filename = "", details = "") {

  message <- switch(action,
    "start" = paste("Import started:", filename),
    "success" = paste("Import successful:", filename),
    "error" = paste("Import error:", filename),
    "format_detected" = paste("File format detected:", filename),
    paste("Import action:", action)
  )
  
  level <- switch(action,
    "start" = 3, "success" = 3, "error" = 1, "format_detected" = 3, 3
  )
  
  log_message(level, "import", message, details)
}

#' Cluster-specific logging
#' @param action Action
#' @param k Number of clusters
#' @param details Details
log_clustering <- function(action, k = "", details = "") {

  message <- switch(action,
    "start" = paste("Clustering started with k =", k),
    "success" = paste("Clustering successful with", k, "clusters"),
    "error" = paste("Clustering error with k =", k),
    paste("Clustering action:", action)
  )
  
  level <- switch(action, "start" = 3, "success" = 3, "error" = 1, 3)
  
  log_message(level, "clustering", message, details)
}

#' Performance Logging
#' @param operation Operation
#' @param duration Duration in seconds
#' @param details Details
log_performance <- function(operation, duration, details = "") {

  message <- sprintf("Performance: %s took %.2f seconds", operation, duration)

  # Warning for slow operations
  level <- if (duration > 30) 2 else if (duration > 10) 3 else 4
  
  log_message(level, "performance", message, details)
}

#' UI-Event Logging
#' @param event Event-Name
#' @param details Details
log_ui_event <- function(event, details = "") {
  log_message(4, "ui", paste("UI-Event:", event), details)
}

# PERFORMANCE MONITORING

#' Time measurement for operations
#' @param operation Name of the operation
#' @param expr R expression to execute
#' @return Result of the expression
time_operation <- function(operation, expr) {

  log_debug("timing", paste("Starting time measurement for:", operation))
  
  start_time <- Sys.time()
  
  result <- tryCatch({
    eval(expr, envir = parent.frame())
  }, error = function(e) {
    end_time <- Sys.time()
    duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
    log_performance(paste(operation, "(ERROR)"), duration, e$message)
    stop(e)
  })
  
  end_time <- Sys.time()
  duration <- as.numeric(difftime(end_time, start_time, units = "secs"))
  
  log_performance(operation, duration)
  
  return(result)
}

# SYSTEM INFO LOGGING

#' Log system information
log_system_info <- function() {

  if (LOG_LEVEL < 3) return(invisible())  # Only at Info level or higher

  log_info("system", paste("SeriARC Version:", SERIARC_VERSION))
  log_info("system", paste("R Version:", R.version.string))
  log_info("system", paste("Platform:", R.version$platform))
  log_info("system", paste("Debug mode:", DEBUG_MODE))
  log_info("system", paste("Log level:", LOG_LEVEL))

  # Memory info
  if (requireNamespace("utils", quietly = TRUE)) {
    mem_info <- utils::memory.size()
    log_info("system", sprintf("Memory usage: %.1f MB", mem_info))
  }

  # Package versions (important ones)
  important_packages <- c("shiny", "plotly", "FactoMineR", "dplyr")
  for (pkg in important_packages) {
    if (requireNamespace(pkg, quietly = TRUE)) {
      version <- as.character(packageVersion(pkg))
      log_debug("system", paste("Package", pkg, "Version:", version))
    }
  }
}

#' Session start logging
log_session_start <- function() {
  log_info("session", "=== SeriARC Session started ===")
  log_system_info()
}

#' Session end logging
log_session_end <- function() {
  log_info("session", "=== SeriARC Session ended ===")
}

# LOG ANALYSIS HELPERS

#' Show log statistics (debug mode only)
show_log_stats <- function() {

  if (!DEBUG_MODE) return(invisible())

  cat("\n📊 SeriARC Logging Statistics:\n")
  cat("Debug mode:", DEBUG_MODE, "\n")
  cat("Log level:", LOG_LEVEL, "\n")
  cat("Configuration loaded:", exists("SERIARC_VERSION"), "\n")
  cat("Error handling loaded:", exists("safe_seriation"), "\n")
  cat("\n")
}

# INITIALIZATION

if (DEBUG_MODE) {
  log_session_start()
  
  # Cleanup bei R-Session Ende registrieren
  reg.finalizer(globalenv(), function(e) {
    log_session_end()
  }, onexit = TRUE)
}

if (DEBUG_MODE) {
  cat("SeriARC Logging System loaded\n")
  show_log_stats()
}

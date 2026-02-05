# SeriARC Error Handling System
# Robust error handling for better user experience

# SAFE SERIATION FUNCTIONS

#' Safe seriation with error handling
#' @param data Matrix or data frame with archaeological data
#' @param method Seriation method (see config.R)
#' @param transform Transformation (see config.R)
#' @param binary_threshold Threshold for binarization
#' @param tr Optional translator function for i18n
#' @return List with success (logical) and either data or error
safe_seriation <- function(data, method = DEFAULT_SERIATION_METHOD,
                          transform = DEFAULT_TRANSFORM, binary_threshold = 0, tr = NULL) {

  # Input validation
  validation_result <- validate_seriation_data(data)
  if (!validation_result$valid) {
    return(list(
      success = FALSE,
      error = paste("Data validation failed:",
                    paste(validation_result$errors, collapse = "; ")),
      error_type = "validation"
    ))
  }

  # Safe computation
  tryCatch({

    if (DEBUG_MODE) {
      cat("Seriation starting:", method, "with", transform, "transformation\n")
    }

    result <- compute_seriation(data, method, transform, binary_threshold)

    # Result validation
    if (is.null(result) || is.null(result$permuted_matrix)) {
      return(list(
        success = FALSE,
        error = "Seriation algorithm returned no valid result",
        error_type = "algorithm"
      ))
    }

    if (DEBUG_MODE) {
      cat("Seriation successful:", nrow(result$permuted_matrix), "x", ncol(result$permuted_matrix), "\n")
    }

    return(list(
      success = TRUE,
      data = result,
      message = paste("Seriation successful with", method)
    ))

  }, error = function(e) {

    # Detailed error classification
    error_msg <- as.character(e$message)
    error_type <- classify_error(error_msg)
    user_msg <- create_user_friendly_error(error_msg, error_type, tr = tr)

    if (DEBUG_MODE) {
      cat("Seriation error:", error_msg, "\n")
    }

    return(list(
      success = FALSE,
      error = user_msg,
      error_type = error_type,
      technical_error = error_msg  # For debug purposes
    ))
  })
}

#' Safe cluster computation
#' @param data Numeric matrix
#' @param k Number of clusters
#' @param method Clustering method
#' @return List with success and result
safe_clustering <- function(data, k, method = "kmeans") {

  # Input validation
  if (is.null(data) || !is.numeric(as.matrix(data))) {
    return(list(
      success = FALSE,
      error = "Data must be numeric",
      error_type = "validation"
    ))
  }

  if (k < 2 || k > nrow(data)) {
    return(list(
      success = FALSE,
      error = sprintf("Number of clusters must be between 2 and %d", nrow(data)),
      error_type = "validation"
    ))
  }

  tryCatch({

    if (method == "kmeans") {
      result <- kmeans(data, centers = k, nstart = 25)
    } else {
      stop("Unknown clustering method: ", method)
    }

    return(list(
      success = TRUE,
      data = result,
      message = paste("Clustering successful with", k, "clusters")
    ))

  }, error = function(e) {
    return(list(
      success = FALSE,
      error = paste("Clustering error:", e$message),
      error_type = "algorithm"
    ))
  })
}

# DATA VALIDATION

#' Validates archaeological data for seriation
#' @param data Matrix or data frame
#' @return List with valid (logical) and errors (character vector)
validate_seriation_data <- function(data) {

  errors <- c()

  # Basic checks
  if (is.null(data)) {
    errors <- c(errors, "No data present")
    return(list(valid = FALSE, errors = errors))
  }

  # Check dimensions
  if (nrow(data) < 3) {
    errors <- c(errors, "At least 3 sites required for seriation")
  }

  if (ncol(data) < 2) {
    errors <- c(errors, "At least 2 types required for seriation")
  }

  # Performance limits
  if (nrow(data) > MAX_ROWS) {
    errors <- c(errors, sprintf("Too many sites (%d). Maximum: %d", nrow(data), MAX_ROWS))
  }

  if (ncol(data) > MAX_COLS) {
    errors <- c(errors, sprintf("Too many types (%d). Maximum: %d", ncol(data), MAX_COLS))
  }

  # Check numeric data
  numeric_mat <- tryCatch({
    as.matrix(data)
  }, error = function(e) NULL)

  if (is.null(numeric_mat)) {
    errors <- c(errors, "Data cannot be converted to numeric matrix")
    return(list(valid = FALSE, errors = errors))
  }

  # Check negative values (archaeologically implausible)
  if (any(numeric_mat < 0, na.rm = TRUE)) {
    errors <- c(errors, "Negative values found in archaeological data")
  }

  # Check empty matrix
  if (all(numeric_mat == 0, na.rm = TRUE)) {
    errors <- c(errors, "All values are zero - no analysis possible")
  }

  # Too many missing values
  na_percentage <- sum(is.na(numeric_mat)) / length(numeric_mat) * 100
  if (na_percentage > 50) {
    errors <- c(errors, sprintf("Too many missing values (%.1f%%)", na_percentage))
  }

  return(list(
    valid = length(errors) == 0,
    errors = errors,
    warnings = get_data_warnings(numeric_mat)
  ))
}

#' Data warnings for user
#' @param data Numeric matrix
#' @return Character vector with warnings
get_data_warnings <- function(data) {
  
  warnings <- c()
  
  # Check sparsity
  non_zero_percentage <- sum(data > 0, na.rm = TRUE) / length(data) * 100
  if (non_zero_percentage < 20) {
    warnings <- c(warnings, sprintf("Very sparse matrix (%.1f%% non-zero values)", non_zero_percentage))
  }

  # Unbalanced Sites/Types
  site_sums <- rowSums(data, na.rm = TRUE)
  type_sums <- colSums(data, na.rm = TRUE)

  if (max(site_sums, na.rm = TRUE) / median(site_sums, na.rm = TRUE) > 10) {
    warnings <- c(warnings, "Very unbalanced site frequencies found")
  }

  if (max(type_sums, na.rm = TRUE) / median(type_sums, na.rm = TRUE) > 10) {
    warnings <- c(warnings, "Very unbalanced type frequencies found")
  }
  
  return(warnings)
}

# ERROR CLASSIFICATION & USER-FRIENDLY MESSAGES

#' Classifies error by type
#' @param error_msg Error message
#' @return Character string with error type
classify_error <- function(error_msg) {
  
  if (grepl("memory|size|allocation", error_msg, ignore.case = TRUE)) {
    return("memory")
  }
  
  if (grepl("singular|rank|eigenvalue", error_msg, ignore.case = TRUE)) {
    return("mathematical")
  }
  
  if (grepl("dimnames|subscript|index", error_msg, ignore.case = TRUE)) {
    return("data_structure")
  }
  
  if (grepl("convergence|iteration", error_msg, ignore.case = TRUE)) {
    return("convergence")
  }
  
  return("general")
}

#' Creates user-friendly error messages
#' @param technical_error Technical error message
#' @param error_type Error type
#' @param tr Optional translator function for i18n
#' @return Character string with user-friendly message
create_user_friendly_error <- function(technical_error, error_type, tr = NULL) {
  # Helper for tr with fallback
  .tr <- function(key, default) if (!is.null(tr)) tr(key) else default

  base_msg <- switch(error_type,
    "memory" = .tr("error.memory", "Not enough memory. Try smaller data or close other programs."),
    "mathematical" = .tr("error.mathematical", "Mathematical problem with the data. The data may not be suitable for this analysis."),
    "data_structure" = .tr("error.data.structure", "Problem with data structure. Check column and row names."),
    "convergence" = .tr("error.convergence", "Algorithm could not converge. Try different parameters or methods."),
    "general" = .tr("error.general", "Unexpected error during analysis.")
  )

  solutions <- switch(error_type,
    "memory" = .tr("error.solution.memory", "Suggestions: Filter data, restart R Studio, close other programs."),
    "mathematical" = .tr("error.solution.mathematical", "Suggestions: Choose different seriation method, transform data, remove outliers."),
    "data_structure" = .tr("error.solution.data.structure", "Suggestions: Re-import data, rename columns/rows, check format."),
    "convergence" = .tr("error.solution.convergence", "Suggestions: Choose different method, adjust parameters, increase iterations."),
    "general" = .tr("error.solution.general", "Suggestions: Check data, restart SeriARC, contact support.")
  )

  if (DEBUG_MODE) {
    return(paste(base_msg, solutions, "\n\nTechnical error:", technical_error))
  } else {
    return(paste(base_msg, solutions))
  }
}

# NOTIFICATION HELPERS

#' Zeigt benutzerfreundliche Erfolgsmeldung
#' @param message Erfolgsmeldung
#' @param duration Anzeigedauer in Sekunden
show_success_notification <- function(message, duration = 3) {
  showNotification(
    paste("✅", message), 
    type = "default", 
    duration = duration
  )
}

#' Zeigt benutzerfreundliche Fehlermeldung
#' @param message Fehlermeldung  
#' @param duration Anzeigedauer in Sekunden
show_error_notification <- function(message, duration = 8) {
  showNotification(
    paste("❌", message),
    type = "error",
    duration = duration
  )
}

#' Shows warning
#' @param message Warning message
#' @param duration Display duration in seconds
show_warning_notification <- function(message, duration = 5) {
  showNotification(
    paste("⚠️", message),
    type = "warning", 
    duration = duration
  )
}

# SAFE FILE OPERATIONS

#' Safe file detection
#' @param file_path Path to file
#' @return List with information about the file
safe_file_detection <- function(file_path) {
  
  if (!file.exists(file_path)) {
    return(list(
      success = FALSE,
      error = "File not found",
      error_type = "file_not_found"
    ))
  }

  # Check file size
  file_info <- file.info(file_path)
  size_mb <- file_info$size / (1024^2)

  if (size_mb > MAX_FILE_SIZE_MB) {
    return(list(
      success = FALSE,
      error = sprintf("File too large (%.1f MB). Maximum: %d MB", size_mb, MAX_FILE_SIZE_MB),
      error_type = "file_too_large"
    ))
  }
  
  # Detect file format
  extension <- tools::file_ext(file_path)
  if (!paste0(".", tolower(extension)) %in% SUPPORTED_FORMATS) {
    planned_msg <- if (paste0(".", tolower(extension)) %in% PLANNED_FORMATS) {
      " (Format is planned but not yet implemented)"
    } else ""

    return(list(
      success = FALSE,
      error = paste0("File format not supported: ", extension, planned_msg,
                    "\nCurrently supported: ", paste(SUPPORTED_FORMATS, collapse = ", "),
                    "\nPlanned: ", paste(PLANNED_FORMATS, collapse = ", ")),
      error_type = "unsupported_format"
    ))
  }

  return(list(
    success = TRUE,
    format = extension,
    size_mb = size_mb,
    message = sprintf("File detected: %s (%.1f MB)", extension, size_mb)
  ))
}

if (DEBUG_MODE) {
  cat("SeriARC Error Handling System loaded\n")
}

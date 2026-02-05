# HELPER: MATRIX AGGREGATION
# Applies site aggregations to data matrix

#' Aggregates sites in a data matrix
#'
#' @param data_matrix Data matrix with Entity column and numeric values
#' @param aggregations_list List of aggregations: list(id = list(name="...", sites=c(...)))
#' @return Aggregated data matrix
apply_aggregations <- function(data_matrix, aggregations_list) {

  # Validierung: data_matrix muss data.frame sein
  if (!is.data.frame(data_matrix)) {
    if (!"Entity" %in% colnames(data_matrix) && !is.null(rownames(data_matrix))) {
      data_matrix <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
      data_matrix$Entity <- rownames(data_matrix)
      data_matrix <- data_matrix[, c("Entity", setdiff(names(data_matrix), "Entity"))]
    } else {
      data_matrix <- as.data.frame(data_matrix, stringsAsFactors = FALSE)
    }
  }

  if (!"Entity" %in% names(data_matrix)) {
    warning("apply_aggregations: Entity column missing")
    return(data_matrix)
  }

  if (is.null(aggregations_list) || length(aggregations_list) == 0) {
    return(data_matrix)
  }

  result <- data_matrix

  for (agg_id in names(aggregations_list)) {
    agg <- aggregations_list[[agg_id]]
    agg_name <- agg$name
    agg_sites <- agg$sites

    existing_sites <- agg_sites[agg_sites %in% result$Entity]

    if (length(existing_sites) < 2) {
      warning(paste0("Aggregation '", agg_name, "': Less than 2 sites found. Skipping."))
      next
    }

    site_rows <- which(result$Entity %in% existing_sites)
    if (length(site_rows) == 0) next

    numeric_cols <- setdiff(names(result), "Entity")

    aggregated_row <- result[site_rows[1], , drop = FALSE]
    aggregated_row$Entity <- agg_name

    for (col in numeric_cols) {
      if (is.numeric(result[[col]])) {
        aggregated_row[[col]] <- sum(result[site_rows, col], na.rm = TRUE)
      }
    }

    result <- result[-site_rows, ]
    result <- rbind(result, aggregated_row)
  }

  rownames(result) <- result$Entity
  result$Entity <- NULL

  return(result)
}

#' Aggregates types in a data matrix (column aggregation)
#'
#' @param data_matrix Data matrix (matrix or data.frame) with types as columns
#' @param type_aggregations_list List of type aggregations: list(id = list(name="...", types=c(...)))
#' @return Aggregated data matrix
apply_type_aggregations <- function(data_matrix, type_aggregations_list) {

  if (is.data.frame(data_matrix)) {
    if ("Entity" %in% names(data_matrix)) {
      rownames(data_matrix) <- data_matrix$Entity
      data_matrix$Entity <- NULL
    }
    data_matrix <- as.matrix(data_matrix)
  }

  if (is.null(type_aggregations_list) || length(type_aggregations_list) == 0) {
    return(data_matrix)
  }

  result <- data_matrix

  for (agg_id in names(type_aggregations_list)) {
    agg <- type_aggregations_list[[agg_id]]
    agg_name <- agg$name
    agg_types <- agg$types

    existing_types <- agg_types[agg_types %in% colnames(result)]

    if (length(existing_types) < 2) {
      warning(paste0("Type aggregation '", agg_name, "': Less than 2 types found. Skipping."))
      next
    }

    type_cols <- which(colnames(result) %in% existing_types)
    if (length(type_cols) == 0) next

    aggregated_col <- rowSums(result[, type_cols, drop = FALSE], na.rm = TRUE)
    result <- result[, -type_cols, drop = FALSE]
    result <- cbind(result, aggregated_col)
    colnames(result)[ncol(result)] <- agg_name
  }

  return(result)
}

#' Applies aggregations to meta$data (sites + types remain)
#'
#' @param meta_data meta$data object with sites, types, etc.
#' @param aggregations_list List of aggregations
#' @return Updated meta$data with aggregated sites
apply_aggregations_to_meta <- function(meta_data, aggregations_list) {

  if (is.null(aggregations_list) || length(aggregations_list) == 0) {
    return(meta_data)
  }

  result <- meta_data

  for (agg_id in names(aggregations_list)) {
    agg <- aggregations_list[[agg_id]]
    agg_name <- agg$name
    agg_sites <- agg$sites

    existing_sites <- agg_sites[agg_sites %in% result$sites$Entity]

    if (length(existing_sites) < 2) next

    site_rows <- which(result$sites$Entity %in% existing_sites)
    if (length(site_rows) == 0) next

    agg_site_row <- result$sites[site_rows[1], , drop = FALSE]
    agg_site_row$Entity <- agg_name

    if ("Weight" %in% names(agg_site_row)) {
      agg_site_row$Weight <- mean(result$sites$Weight[site_rows], na.rm = TRUE)
    }

    if ("Selected" %in% names(agg_site_row)) {
      agg_site_row$Selected <- any(result$sites$Selected[site_rows])
    }

    if ("Supplementary" %in% names(agg_site_row)) {
      agg_site_row$Supplementary <- FALSE
    }

    if ("ShowLabel" %in% names(agg_site_row)) {
      agg_site_row$ShowLabel <- TRUE
    }

    result$sites <- result$sites[-site_rows, ]
    result$sites <- rbind(result$sites, agg_site_row)
  }

  return(result)
}

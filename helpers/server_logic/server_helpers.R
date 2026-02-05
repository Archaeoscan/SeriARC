# server_helpers.R - Server helper functions

get_site_group <- function(site_names) {
  if (is.null(mapping$meta)) return(rep(NA_character_, length(site_names)))
  mm <- mapping$meta
  mm <- mm[match(site_names, mm$site), , drop = FALSE]
  mm$group
}

get_type_group <- function(type_names) {
  if (is.null(mapping$type_groups) || length(mapping$type_groups) == 0) {
    return(rep(NA_character_, length(type_names)))
  }
  mapping$type_groups[type_names]
}

# Helper function for extended hover texts
get_element_details <- function(element_name, element_type) {
  req(filtermod$weights_table())
  weights_df <- filtermod$weights_table()

  element_info <- weights_df[weights_df$Element == element_name & weights_df$Typ == element_type, ]

  if (nrow(element_info) > 0) {
    list(
      weight = element_info$Gewicht[1],
      status = element_info$Status[1],
      impact = element_info$Auswirkung[1]
    )
  } else {
    list(
      weight = 1.0,
      status = "Unknown",
      impact = "No info"
    )
  }
}

# Site Clusters reactive for Mapping
site_clusters <- reactive({
  if (is.null(cache$kmeans_result)) return(NULL)
  df <- cache$kmeans_result$data
  if (is.null(df)) return(NULL)
  sites_df <- subset(df, type == 'Site')
  if (!("cluster" %in% names(sites_df))) return(NULL)
  cl <- sites_df$cluster
  names(cl) <- sites_df$label
  cl
})

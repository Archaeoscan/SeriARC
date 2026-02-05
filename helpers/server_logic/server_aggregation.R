# SERVER LOGIC - AGGREGATION
# Manages site aggregations

# Initialize reactive values for aggregations (call in server.R)
# site_aggregations <- reactiveValues(
#   list = list(),  # List of site aggregations: list(id = list(name="...", sites=c(...)))
#   counter = 0     # ID counter
# )
# type_aggregations <- reactiveValues(
#   list = list(),  # List of type aggregations: list(id = list(name="...", types=c(...)))
#   counter = 0     # ID counter
# )

# Main content for aggregation tab
output$aggregation_main_content <- renderUI({
  if (is.null(data_raw())) {
    return(welcome_base_ui("link", tr("nav.data.aggregation"),
                          tr("welcome.filter.hint")))
  }
  
  aggregation_content_ui(tr = tr)
})

# ==== SITE AGGREGATION ====

# Display site aggregation list
output$site_aggregations_list_ui <- renderUI({
  if (length(site_aggregations$list) == 0) {
    return(
      div(class = "seriarc-panel",
          style = "background: #fff3cd; border-left: 4px solid #ffc107; text-align: center; padding: 40px;",
        icon("info-circle", class = "fa-3x", style = "color: #856404; margin-bottom: 15px;"),
        h4(tr("agg.empty.sites.title"), style = "color: #856404;"),
        p(tr("agg.empty.sites.hint"), style = "color: #856404;")
      )
    )
  }

  # Render aggregation boxes
  tagList(
    lapply(names(site_aggregations$list), function(agg_id) {
      agg <- site_aggregations$list[[agg_id]]
      site_aggregation_box_ui(agg_id, agg$name, agg$sites, tr = tr)
    })
  )
})

# Button: New site aggregation opens modal
observeEvent(input$add_site_aggregation, {
  req(data_raw(), meta, meta$data, meta$data$sites)
  
  # Available sites: Only original sites (Hidden=FALSE) and not already aggregated
  visible_sites <- meta$data$sites$Entity[!meta$data$sites$Hidden]
  
  # Sites that are already in aggregations
  used_sites <- character(0)
  if (length(site_aggregations$list) > 0) {
    used_sites <- unique(unlist(lapply(site_aggregations$list, function(x) x$sites)))
  }
  
  available_sites <- setdiff(visible_sites, used_sites)
  
  if (length(available_sites) < 2) {
    showNotification(
      paste0("⚠️ ", tr("agg.notify.sites.notenough")),
      type = "warning",
      duration = 5
    )
    return()
  }

  showModal(site_aggregation_modal_ui(available_sites, tr = tr))
})

# Modal: Cancel
observeEvent(input$site_agg_cancel, {
  removeModal()
})

observeEvent(input$site_agg_confirm, {
  req(input$site_agg_name, input$site_agg_sites_selected, meta, meta$data, meta$data$sites)
  
  # Validation
  if (nchar(trimws(input$site_agg_name)) == 0) {
    showNotification(paste0("⚠️ ", tr("agg.notify.name.required")), type = "warning", duration = 3)
    return()
  }

  if (length(input$site_agg_sites_selected) < 2) {
    showNotification(paste0("⚠️ ", tr("agg.notify.sites.min2")), type = "warning", duration = 3)
    return()
  }
  
  agg_name <- trimws(input$site_agg_name)
  agg_sites <- input$site_agg_sites_selected
  
  # Add new aggregation
  site_aggregations$counter <- site_aggregations$counter + 1
  new_id <- paste0("site_agg_", site_aggregations$counter)
  
  site_aggregations$list[[new_id]] <- list(
    name = agg_name,
    sites = agg_sites
  )
  
  # HIDDEN LOGIC: Hide original sites
  for (site in agg_sites) {
    idx <- which(meta$data$sites$Entity == site)
    if (length(idx) == 1) {
      meta$data$sites$Hidden[idx] <- TRUE
      meta$data$sites$AggregatedInto[idx] <- agg_name
          }
  }
  
  # Add new aggregated site to meta$data$sites
  # Take values from first original site as basis
  first_site_idx <- which(meta$data$sites$Entity == agg_sites[1])[1]
  
  new_site_row <- data.frame(
    Entity = agg_name,
    Selected = TRUE,
    Weight = meta$data$sites$Weight[first_site_idx],
    Supplementary = FALSE,
    ShowLabel = TRUE,
    Hidden = FALSE,
    AggregatedInto = NA_character_,
    stringsAsFactors = FALSE
  )
  
  meta$data$sites <- rbind(meta$data$sites, new_site_row)
    
  cache$ca_result <- NULL
  cache$kmeans_result <- NULL
  cache$seriation_result <- NULL
  cache$bootstrap_result <- NULL
  
  removeModal()
  
  showNotification(
    paste0("✅ ", sprintf(tr("agg.notify.created"), agg_name, length(agg_sites), "Sites")),
    type = "message",
    duration = 3
  )
})

# Delete site aggregation (via JS trigger)
observeEvent(input$delete_site_agg_id, {
  req(meta, meta$data, meta$data$sites)
  
  agg_id <- input$delete_site_agg_id
  
  if (!is.null(agg_id) && agg_id %in% names(site_aggregations$list)) {
    agg <- site_aggregations$list[[agg_id]]
    agg_name <- agg$name
    agg_sites <- agg$sites
    
    for (site in agg_sites) {
      idx <- which(meta$data$sites$Entity == site)
      if (length(idx) == 1) {
        meta$data$sites$Hidden[idx] <- FALSE
        meta$data$sites$AggregatedInto[idx] <- NA_character_
              }
    }
    
    # Remove aggregated site from meta$data$sites
    agg_site_idx <- which(meta$data$sites$Entity == agg_name)
    if (length(agg_site_idx) == 1) {
      meta$data$sites <- meta$data$sites[-agg_site_idx, ]
          }
    
    # Remove from aggregations$list
    site_aggregations$list[[agg_id]] <- NULL
    
    # Clear cache
    cache$ca_result <- NULL
    cache$kmeans_result <- NULL
    cache$seriation_result <- NULL
    cache$bootstrap_result <- NULL
    
    showNotification(
      paste0("🗑️ ", sprintf(tr("agg.notify.deleted"), agg_name)),
      type = "default",
      duration = 3
    )
  }
})

# Reset all site aggregations
observeEvent(input$reset_site_aggregations, {
  req(meta, meta$data, meta$data$sites)
  
  if (length(site_aggregations$list) == 0) {
    showNotification(paste0("ℹ️ ", tr("agg.notify.sites.none")), type = "default", duration = 2)
    return()
  }
  
  hidden_sites <- meta$data$sites$Hidden
  if (any(hidden_sites)) {
    meta$data$sites$Hidden[hidden_sites] <- FALSE
    meta$data$sites$AggregatedInto[hidden_sites] <- NA_character_
      }
  
  # Remove all aggregated sites from meta$data$sites
  # (Sites that are not in data_raw())
  original_sites <- data_raw()$Entity
  aggregated_site_idx <- which(!meta$data$sites$Entity %in% original_sites)
  
  if (length(aggregated_site_idx) > 0) {
    meta$data$sites <- meta$data$sites[-aggregated_site_idx, ]
  }
  
  # Reset aggregation list
  site_aggregations$list <- list()
  site_aggregations$counter <- 0
  
  # Clear cache
  cache$ca_result <- NULL
  cache$kmeans_result <- NULL
  cache$seriation_result <- NULL
  cache$bootstrap_result <- NULL
  
  showNotification(paste0("🗑️ ", tr("agg.notify.sites.reset")), type = "default", duration = 3)
})

# ==== TYPE AGGREGATION ====

# Display type aggregation list
output$type_aggregations_list_ui <- renderUI({
  if (length(type_aggregations$list) == 0) {
    return(
      div(class = "seriarc-panel",
          style = "background: #fff3cd; border-left: 4px solid #ffc107; text-align: center; padding: 40px;",
        icon("info-circle", class = "fa-3x", style = "color: #856404; margin-bottom: 15px;"),
        h4(tr("agg.empty.types.title"), style = "color: #856404;"),
        p(tr("agg.empty.types.hint"), style = "color: #856404;")
      )
    )
  }

  # Render aggregation boxes
  tagList(
    lapply(names(type_aggregations$list), function(agg_id) {
      agg <- type_aggregations$list[[agg_id]]
      type_aggregation_box_ui(agg_id, agg$name, agg$types, tr = tr)
    })
  )
})

# Button: New type aggregation opens modal
observeEvent(input$add_type_aggregation, {
  req(data_raw(), meta, meta$data, meta$data$types)
  
  # Available types: Only original types (Hidden=FALSE) and not already aggregated
  visible_types <- meta$data$types$Entity[!meta$data$types$Hidden]
  
  # Types that are already in aggregations
  used_types <- character(0)
  if (length(type_aggregations$list) > 0) {
    used_types <- unique(unlist(lapply(type_aggregations$list, function(x) x$types)))
  }
  
  available_types <- setdiff(visible_types, used_types)
  
  if (length(available_types) < 2) {
    showNotification(
      paste0("⚠️ ", tr("agg.notify.types.notenough")),
      type = "warning",
      duration = 5
    )
    return()
  }

  showModal(type_aggregation_modal_ui(available_types, tr = tr))
})

# Modal: Cancel
observeEvent(input$type_agg_cancel, {
  removeModal()
})

observeEvent(input$type_agg_confirm, {
  req(input$type_agg_name, input$type_agg_types_selected, meta, meta$data, meta$data$types)
  
  # Validation
  if (nchar(trimws(input$type_agg_name)) == 0) {
    showNotification(paste0("⚠️ ", tr("agg.notify.name.required")), type = "warning", duration = 3)
    return()
  }

  if (length(input$type_agg_types_selected) < 2) {
    showNotification(paste0("⚠️ ", tr("agg.notify.types.min2")), type = "warning", duration = 3)
    return()
  }
  
  agg_name <- trimws(input$type_agg_name)
  agg_types <- input$type_agg_types_selected
  
  # Add new aggregation
  type_aggregations$counter <- type_aggregations$counter + 1
  new_id <- paste0("type_agg_", type_aggregations$counter)
  
  type_aggregations$list[[new_id]] <- list(
    name = agg_name,
    types = agg_types
  )
  
  # HIDDEN LOGIC: Hide original types
  for (type in agg_types) {
    idx <- which(meta$data$types$Entity == type)
    if (length(idx) == 1) {
      meta$data$types$Hidden[idx] <- TRUE
      meta$data$types$AggregatedInto[idx] <- agg_name
          }
  }
  
  # Add new aggregated type to meta$data$types
  first_type_idx <- which(meta$data$types$Entity == agg_types[1])[1]
  
  new_type_row <- data.frame(
    Entity = agg_name,
    Selected = TRUE,
    Weight = meta$data$types$Weight[first_type_idx],
    Supplementary = FALSE,
    ShowLabel = TRUE,
    Hidden = FALSE,
    AggregatedInto = NA_character_,
    stringsAsFactors = FALSE
  )
  
  meta$data$types <- rbind(meta$data$types, new_type_row)
    
  # Clear cache
  cache$ca_result <- NULL
  cache$kmeans_result <- NULL
  cache$seriation_result <- NULL
  cache$bootstrap_result <- NULL
  
  removeModal()
  
  showNotification(
    paste0("✅ ", sprintf(tr("agg.notify.created"), agg_name, length(agg_types), "Types")),
    type = "message",
    duration = 3
  )
})

# Delete type aggregation (via JS trigger)
observeEvent(input$delete_type_agg_id, {
  req(meta, meta$data, meta$data$types)
  
  agg_id <- input$delete_type_agg_id
  
  if (!is.null(agg_id) && agg_id %in% names(type_aggregations$list)) {
    agg <- type_aggregations$list[[agg_id]]
    agg_name <- agg$name
    agg_types <- agg$types
    
    for (type in agg_types) {
      idx <- which(meta$data$types$Entity == type)
      if (length(idx) == 1) {
        meta$data$types$Hidden[idx] <- FALSE
        meta$data$types$AggregatedInto[idx] <- NA_character_
              }
    }
    
    # Remove aggregated type from meta$data$types
    agg_type_idx <- which(meta$data$types$Entity == agg_name)
    if (length(agg_type_idx) == 1) {
      meta$data$types <- meta$data$types[-agg_type_idx, ]
          }
    
    # Remove from type_aggregations$list
    type_aggregations$list[[agg_id]] <- NULL
    
    # Clear cache
    cache$ca_result <- NULL
    cache$kmeans_result <- NULL
    cache$seriation_result <- NULL
    cache$bootstrap_result <- NULL
    
    showNotification(
      paste0("🗑️ ", sprintf(tr("agg.notify.deleted"), agg_name)),
      type = "default",
      duration = 3
    )
  }
})

# Reset all type aggregations
observeEvent(input$reset_type_aggregations, {
  req(meta, meta$data, meta$data$types)
  
  if (length(type_aggregations$list) == 0) {
    showNotification(paste0("ℹ️ ", tr("agg.notify.types.none")), type = "default", duration = 2)
    return()
  }
  
  hidden_types <- meta$data$types$Hidden
  if (any(hidden_types)) {
    meta$data$types$Hidden[hidden_types] <- FALSE
    meta$data$types$AggregatedInto[hidden_types] <- NA_character_
      }
  
  # Remove all aggregated types from meta$data$types
  original_types <- colnames(data_raw())[-1]  # Without Entity column
  aggregated_type_idx <- which(!meta$data$types$Entity %in% original_types)
  
  if (length(aggregated_type_idx) > 0) {
    meta$data$types <- meta$data$types[-aggregated_type_idx, ]
  }
  
  # Reset aggregation list
  type_aggregations$list <- list()
  type_aggregations$counter <- 0
  
  # Clear cache
  cache$ca_result <- NULL
  cache$kmeans_result <- NULL
  cache$seriation_result <- NULL
  cache$bootstrap_result <- NULL
  
  showNotification(paste0("🗑️ ", tr("agg.notify.types.reset")), type = "default", duration = 3)
})

# ==== SUMMARY ====

# Output summary
output$aggregation_summary <- renderText({
  if (is.null(data_raw())) {
    return(tr("agg.summary.nodata"))
  }

  total_sites <- nrow(data_raw())
  total_types <- ncol(data_raw()) - 1

  if (length(site_aggregations$list) == 0 && length(type_aggregations$list) == 0) {
    return(paste0(
      "📊 ", tr("agg.summary.status"), ":\n",
      "  • ", tr("agg.summary.original.sites"), ": ", total_sites, "\n",
      "  • ", tr("agg.summary.original.types"), ": ", total_types, "\n",
      "  • ", tr("agg.summary.aggregations"), ": 0\n",
      "  • ", tr("agg.summary.final.matrix"), ": ", total_sites, " × ", total_types, " (", tr("agg.summary.no.change"), ")"
    ))
  }

  # Count aggregated sites
  aggregated_sites <- unique(unlist(lapply(site_aggregations$list, function(x) x$sites)))
  n_aggregated_sites <- length(aggregated_sites)
  n_site_aggregations <- length(site_aggregations$list)

  final_sites <- total_sites - n_aggregated_sites + n_site_aggregations

  # Count aggregated types
  aggregated_types <- unique(unlist(lapply(type_aggregations$list, function(x) x$types)))
  n_aggregated_types <- length(aggregated_types)
  n_type_aggregations <- length(type_aggregations$list)

  final_types <- total_types - n_aggregated_types + n_type_aggregations

  paste0(
    "📊 ", tr("agg.summary.status"), ":\n",
    "  • ", tr("agg.summary.original.sites"), ": ", total_sites, "\n",
    "  • ", tr("agg.summary.site.agg"), ": ", n_site_aggregations, "\n",
    "  • ", tr("agg.summary.aggregated.sites"), ": ", n_aggregated_sites, "\n",
    "  • ", tr("agg.summary.final.sites"), ": ", final_sites, " (",
    ifelse(final_sites < total_sites,
           paste0("-", total_sites - final_sites),
           paste0("+", final_sites - total_sites)), ")\n\n",
    "  • ", tr("agg.summary.original.types"), ": ", total_types, "\n",
    "  • ", tr("agg.summary.type.agg"), ": ", n_type_aggregations, "\n",
    "  • ", tr("agg.summary.aggregated.types"), ": ", n_aggregated_types, "\n",
    "  • ", tr("agg.summary.final.types"), ": ", final_types, " (",
    ifelse(final_types < total_types,
           paste0("-", total_types - final_types),
           paste0("+", final_types - total_types)), ")\n\n",
    "  • ", tr("agg.summary.final.matrix"), ": ", final_sites, " × ", final_types, "\n\n",
    "💡 ", tr("agg.summary.hint")
  )
})

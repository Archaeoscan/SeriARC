# server_reactive_values.R - Reactive values and core setup

# REACTIVE VALUES AND CACHE
meta <- reactiveValues(data = NULL)
cache <- reactiveValues(
  ca_result = NULL, 
  kmeans_result = NULL, 
  seriation_result = NULL,
  bootstrap_result = NULL
)

# AGGREGATIONS REACTIVE VALUES
site_aggregations <- reactiveValues(
  list = list(),  # List of site aggregations: list(id = list(name="...", sites=c(...)))
  counter = 0     # ID counter
)

type_aggregations <- reactiveValues(
  list = list(),  # List of type aggregations: list(id = list(name="...", types=c(...)))
  counter = 0     # ID counter
)

filter_trigger <- reactiveVal(0)

# Session cleanup
session$onSessionEnded(function() {
  tryCatch({
    cache_names <- ls(cache)
    for(name in cache_names) {
      cache[[name]] <- NULL
    }
  }, error = function(e) {
    # Silent error during cleanup
  })
  gc()
})

# HELPER FUNCTIONS
mapping <- reactiveValues(meta = NULL, coord_info = NULL, raw = NULL)

# Explicit reactive functions for sites and types
# Depend on filter_trigger() -> invalidate on filter changes
sites_selection_reactive <- reactive({
  filter_trigger()  # Register dependency first
  req(meta$data, meta$data$sites)  # Then validate

  sites_df <- meta$data$sites
  changed_weights <- sum(sites_df$Weight != 1.0)
  cat(sprintf("Sites-Selection Update (Trigger #%d): %d Sites, %d with Weight != 1.0\n",
              filter_trigger(), nrow(sites_df), changed_weights))
  sites_df
})

types_selection_reactive <- reactive({
  filter_trigger()  # Register dependency first
  req(meta$data, meta$data$types)  # Then validate

  types_df <- meta$data$types

  # WORKAROUND: Add missing types from data_raw()
  # If meta$data$types is incomplete (69 instead of 72), add missing ones
  tryCatch({
    if (exists("data_raw") && is.function(data_raw)) {
      raw_data <- data_raw()
      if (!is.null(raw_data)) {
        all_types <- colnames(raw_data)[-1]  # All types from raw data
        existing_types <- types_df$Entity
        missing_types <- setdiff(all_types, existing_types)

        if (length(missing_types) > 0) {
          cat(sprintf("types_selection_reactive: Adding %d missing types\n", length(missing_types)))
          new_rows <- data.frame(
            Entity = missing_types,
            Selected = TRUE,
            Weight = 1.0,
            Supplementary = FALSE,
            ShowLabel = TRUE,
            Hidden = FALSE,
            AggregatedInto = NA_character_,
            stringsAsFactors = FALSE
          )
          types_df <- rbind(types_df, new_rows)
        }
      }
    }
  }, error = function(e) {
    # Silent error - use types_df as is
  })

  changed_weights <- sum(types_df$Weight != 1.0)
  cat(sprintf("Types-Selection Update (Trigger #%d): %d Types, %d with Weight != 1.0\n",
              filter_trigger(), nrow(types_df), changed_weights))
  types_df
})

# METADATA MANAGEMENT
make_meta <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  
  list(
    sites = data.frame(
      Entity = df$Entity, 
      Selected = TRUE, 
      Weight = 1.0,
      Supplementary = FALSE,
      ShowLabel = TRUE,
      stringsAsFactors = FALSE
    ),
    types = data.frame(
      Entity = colnames(df)[-1], 
      Selected = TRUE, 
      Weight = 1.0,
      Supplementary = FALSE,
      ShowLabel = TRUE,
      stringsAsFactors = FALSE
    )
  )
}

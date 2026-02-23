# mod_kmeans_clustering.R
# K-Means Clustering with CA Integration

source("helpers/plot_components.R", local = TRUE)
source("helpers/download_components.R", local = TRUE)

mod_kmeans_clustering_ui <- function(id, tr = function(x) x) {
  ns <- NS(id)
  NULL
}

mod_kmeans_clustering_server <- function(ca_result, cache, get_site_group, get_element_details = NULL, meta_data = NULL, filtered_data = NULL, input, output, session, tr = function(x) x) {

  # Reactive values for custom cluster names
  cluster_names <- reactiveValues()

  # Store last plotly object for HTML export
  last_kmeans_plotly <- reactiveVal(NULL)
  
  # === FIX 3: Clean up cluster names when k changes ===
  observeEvent(input$num_clusters, {
    # Clean up old cluster names when k changes
    current_k <- input$num_clusters %||% 3
    old_names <- names(reactiveValuesToList(cluster_names))
    
    # Remove names for clusters > current_k
    for (name in old_names) {
      cluster_id <- as.integer(gsub("cluster_", "", name))
      if (!is.na(cluster_id) && cluster_id > current_k) {
        cluster_names[[name]] <- NULL
      }
    }
  }, ignoreInit = TRUE)
  
  # UI for cluster name inputs with Enter key confirmation
  output$cluster_name_inputs <- renderUI({
    req(cache$kmeans_result)
    ns <- session$ns

    num_clusters <- input$num_clusters %||% 3

    for (i in 1:num_clusters) {
      if (is.null(cluster_names[[paste0("cluster_", i)]])) {
        cluster_names[[paste0("cluster_", i)]] <- paste("Cluster", i)
      }
    }

    # Generate input fields with Enter key handler
    inputs <- lapply(1:num_clusters, function(i) {
      input_id <- paste0("cluster_name_", i)
      tagList(
        textInput(
          input_id,
          label = paste("Cluster", i, ":"),
          value = cluster_names[[paste0("cluster_", i)]],
          placeholder = paste("Cluster", i)
        ),
        # JavaScript to handle Enter key press
        tags$script(HTML(sprintf(
          "$(document).on('keypress', '#%s', function(e) {
            if (e.which == 13) {
              e.preventDefault();
              Shiny.setInputValue('%s', {id: %d, value: $(this).val(), nonce: Math.random()});
            }
          });",
          ns(input_id), ns("cluster_name_confirmed"), i
        )))
      )
    })

    tagList(
      tags$p(tags$small(tags$em(tr("mod_cluster.name.hint"))), class = "text-muted"),
      do.call(tagList, inputs)
    )
  })

  # Handle Enter key confirmation for cluster names
  observeEvent(input$cluster_name_confirmed, {
    req(input$cluster_name_confirmed)
    confirmed <- input$cluster_name_confirmed
    cluster_id <- confirmed$id
    new_name <- confirmed$value

    if (!is.null(new_name) && nzchar(trimws(new_name))) {
      cluster_names[[paste0("cluster_", cluster_id)]] <- new_name
      showNotification(
        sprintf(tr("notify.cluster.name.updated"), cluster_id, new_name),
        type = "message",
        duration = 2
      )
    }
  })
  
  # Reset button for cluster names
  observeEvent(input$reset_cluster_names, {
    req(cache$kmeans_result)
    num_clusters <- input$num_clusters %||% 3
    for (i in 1:num_clusters) {
      cluster_names[[paste0("cluster_", i)]] <- paste("Cluster", i)
    }
    showNotification(tr("notify.cluster.names.reset"), type = "message", duration = 2)
  })
  
  # Helper function to get current cluster name
  get_cluster_name <- function(cluster_id) {
    custom_name <- cluster_names[[paste0("cluster_", cluster_id)]]
    if (is.null(custom_name) || nchar(trimws(custom_name)) == 0) {
      return(paste("Cluster", cluster_id))
    }
    return(custom_name)
  }
  
  # Info button for confidence ellipses
  observeEvent(input$info_confidence_ellipses, {
    showModal(modalDialog(
      title = tr("plot.kmeans.ellipse.title"),
      size = "l",
      easyClose = TRUE,

      tagList(
        h4(tr("mod_cluster.ellipse.stat.title"), style="margin-top:0;"),
        tags$p(tr("mod_cluster.ellipse.stat.text1"), " ",
               tags$strong(tr("mod_cluster.ellipse.stat.spread")), " ", tr("mod_cluster.ellipse.stat.text2")),

        tags$hr(),

        h4(tr("mod_cluster.ellipse.arch.title")),

        div(class = "well", style="background-color: #e3f2fd;",
          h5(tr("mod_cluster.ellipse.large.title"), style="color: #1976D2;"),
          tags$ul(
            tags$li(tags$strong(tr("mod_cluster.ellipse.large.item1")), " ", tr("mod_cluster.ellipse.large.item1b")),
            tags$li(tr("mod_cluster.ellipse.large.item2"))
          )
        ),

        div(class = "well", style="background-color: #e8f5e9;",
          h5(tr("mod_cluster.ellipse.small.title"), style="color: #388E3C;"),
          tags$ul(
            tags$li(tags$strong(tr("mod_cluster.ellipse.small.item1")), " ", tr("mod_cluster.ellipse.small.item1b")),
            tags$li(tr("mod_cluster.ellipse.small.item2"))
          )
        ),

        div(class = "well", style="background-color: #fff3e0;",
          h5(tr("mod_cluster.ellipse.overlap.title"), style="color: #F57C00;"),
          tags$ul(
            tags$li(tags$strong(tr("mod_cluster.ellipse.overlap.no")), " ", tr("mod_cluster.ellipse.overlap.nob")),
            tags$li(tags$strong(tr("mod_cluster.ellipse.overlap.yes")), " ", tr("mod_cluster.ellipse.overlap.yesb"))
          )
        ),

        tags$hr(),

        h4(tr("mod_cluster.ellipse.context.title")),
        tags$ul(
          tags$li(tr("mod_cluster.ellipse.context.item1"), " ", tags$strong(tr("mod_cluster.ellipse.context.item1b")), " ", tr("mod_cluster.ellipse.context.item1c")),
          tags$li(tr("mod_cluster.ellipse.context.item2")),
          tags$li(tr("mod_cluster.ellipse.context.item3")),
          tags$li(tr("mod_cluster.ellipse.context.item4"), " ", tags$strong(tr("mod_cluster.ellipse.context.item4b")), " ", tr("mod_cluster.ellipse.context.item4c"))
        ),

        tags$hr(),

        div(class = "alert alert-info", style="margin-bottom:0;",
          tags$strong(tr("mod_cluster.ellipse.tip.title")),
          " ", tr("mod_cluster.ellipse.tip.text")
        )
      ),

      footer = modalButton(tr("mod_cluster.btn.close"))
    ))
  })
  
  # Info button for dimension selection
  observeEvent(input$info_clustering_dims, {
    showModal(modalDialog(
      title = tr("plot.kmeans.dims.title"),
      size = "l",
      easyClose = TRUE,

      tagList(
        h4(tr("mod_cluster.dims.arch.title"), style="margin-top:0;"),
        tags$p(tr("mod_cluster.dims.arch.intro")),

        div(class = "well", style="background-color: #e3f2fd;",
          h5(tr("mod_cluster.dims.dim1.title"), style="color: #1976D2;"),
          tags$ul(
            tags$li(tags$strong(tr("mod_cluster.dims.dim1.chrono")), " ", tr("mod_cluster.dims.dim1.chronob")),
            tags$li(tags$strong(tr("mod_cluster.dims.dim1.var")), " ", tr("mod_cluster.dims.dim1.varb")),
            tags$li(tags$strong(tr("mod_cluster.dims.dim1.time")), " ", tr("mod_cluster.dims.dim1.timeb"))
          )
        ),

        div(class = "well", style="background-color: #e8f5e9;",
          h5(tr("mod_cluster.dims.dim2.title"), style="color: #388E3C;"),
          tags$ul(
            tags$li(tags$strong(tr("mod_cluster.dims.dim2.region")), " ", tr("mod_cluster.dims.dim2.regionb")),
            tags$li(tags$strong(tr("mod_cluster.dims.dim2.comp")), " ", tr("mod_cluster.dims.dim2.compb")),
            tags$li(tags$strong(tr("mod_cluster.dims.dim2.cluster")), " ", tr("mod_cluster.dims.dim2.clusterb"))
          )
        ),

        div(class = "well", style="background-color: #fff3e0;",
          h5(tr("mod_cluster.dims.higher.title"), style="color: #F57C00;"),
          tags$ul(
            tags$li(tags$strong(tr("mod_cluster.dims.higher.fine")), " ", tr("mod_cluster.dims.higher.fineb")),
            tags$li(tags$strong(tr("mod_cluster.dims.higher.var")), " ", tr("mod_cluster.dims.higher.varb")),
            tags$li(tags$strong(tr("mod_cluster.dims.higher.expl")), " ", tr("mod_cluster.dims.higher.explb"))
          )
        ),

        tags$hr(),

        h4(tr("mod_cluster.dims.method.title"), style="margin-top:15px;"),
        tags$ul(
          tags$li(tags$strong(tr("mod_cluster.dims.1d")),
            tags$ul(
              tags$li(tr("mod_cluster.dims.1d.item1")),
              tags$li(tr("mod_cluster.dims.1d.item2")),
              tags$li(tr("mod_cluster.dims.1d.item3"))
            )
          ),
          tags$li(tags$strong(tr("mod_cluster.dims.2d")),
            tags$ul(
              tags$li(tr("mod_cluster.dims.2d.item1")),
              tags$li(tr("mod_cluster.dims.2d.item2")),
              tags$li(tr("mod_cluster.dims.2d.item3"))
            )
          ),
          tags$li(tags$strong(tr("mod_cluster.dims.multi")),
            tags$ul(
              tags$li(tr("mod_cluster.dims.multi.item1")),
              tags$li(tr("mod_cluster.dims.multi.item2")),
              tags$li(tr("mod_cluster.dims.multi.item3"))
            )
          )
        ),

        tags$hr(),

        div(class = "alert alert-success", style="margin-top:15px;",
          h5(tr("mod_cluster.dims.rec.title"), style="margin-top:0;"),
          tags$ul(style="margin-bottom:0;",
            tags$li(tags$strong(tr("mod_cluster.dims.rec.time")),
              tags$ul(
                tags$li(tr("mod_cluster.dims.rec.time1")),
                tags$li(tr("mod_cluster.dims.rec.time2"))
              )
            ),
            tags$li(tags$strong(tr("mod_cluster.dims.rec.typo")),
              tags$ul(
                tags$li(tr("mod_cluster.dims.rec.typo1"))
              )
            ),
            tags$li(tags$strong(tr("mod_cluster.dims.rec.expl")),
              tags$ul(
                tags$li(tr("mod_cluster.dims.rec.expl1"))
              )
            )
          )
        ),

        div(class = "alert alert-warning", style="margin-top:10px; margin-bottom:0;",
          tags$strong(tr("mod_cluster.dims.note")),
          " ", tr("mod_cluster.dims.note.text")
        )
      ),

      footer = modalButton(tr("mod_cluster.btn.close"))
    ))
  })
  
  # ReactiveVal for auto-cluster suggestion (storage between modal display and button click)
  auto_cluster_suggestion <- reactiveVal(NULL)
  auto_cluster_details <- reactiveValues(
    suggested_k = NULL,
    silhouette_k = NULL,
    elbow_k = NULL,
    gap_k = NULL
  )

  # AUTO-SUGGESTION FOR OPTIMAL CLUSTER COUNT (METHOD-SPECIFIC)
  observeEvent(input$auto_suggest_clusters, {
    req(ca_result(), input$x_dim, input$y_dim)
    
    # CRITICAL: Wait until input is initialized
    req(input$clustering_method)
    
    # Get method with isolate()
    method_raw <- isolate(input$clustering_method)
    
    # CRITICAL: Validate method against active mode
    expert_mode <- !is.null(input$expert_clustering_mode) && input$expert_clustering_mode
    
    if (expert_mode) {
      # Expert mode: Only fuzzy/gmm valid
      if (is.null(method_raw) || !(method_raw %in% c("fuzzy", "gmm"))) {
        method <- "fuzzy"  # Default for expert mode
      } else {
        method <- method_raw
      }
    } else {
      # Basic mode: kmeans/hierarchical/pam valid
      if (is.null(method_raw) || !(method_raw %in% c("kmeans", "hierarchical", "pam"))) {
        method <- "kmeans"  # Default for basic mode
      } else {
        method <- method_raw
      }
    }
    
    method_label <- switch(method,
                          "kmeans" = "K-Means",
                          "hierarchical" = "Hierarchical",
                          "pam" = "PAM (Medoids)",
                          "fuzzy" = "Fuzzy K-Means",
                          "gmm" = "GMM",
                          "K-Means")
    
    showNotification(
      sprintf("Calculating optimal cluster count for %s... (10-30 sec.)", method_label),
      id = "auto_cluster_calc",
      duration = NULL,
      closeButton = FALSE,
      type = "message"
    )
    
    tryCatch({
      res <- ca_result()
      x_idx <- as.numeric(gsub("Dim", "", input$x_dim))
      y_idx <- as.numeric(gsub("Dim", "", input$y_dim))
      
      row_coords <- as.data.frame(res$row$coord)
      col_coords <- as.data.frame(res$col$coord)
      
      active_data <- rbind(
        data.frame(row_coords, label = rownames(row_coords), type = 'Site',
                   element_type = 'Active', stringsAsFactors = FALSE),
        data.frame(col_coords, label = rownames(col_coords), type = 'Type',
                   element_type = 'Active', stringsAsFactors = FALSE)
      )
      
      # Isolate ALL inputs for consistent calculations
      cluster_on_val <- isolate(input$cluster_on %||% "both")
      n_dims_val <- isolate(input$kmeans_n_dims %||% 2)
      
      cluster_data <- switch(cluster_on_val,
                             "rows" = subset(active_data, type == 'Site'),
                             "cols" = subset(active_data, type == 'Type'),
                             "both" = active_data)
      
      n_dims <- min(n_dims_val, ncol(cluster_data) - 3)
      
      if (n_dims == 2) {
        coords <- as.matrix(cluster_data[, c(x_idx, y_idx), drop = FALSE])
      } else {
        dim_indices <- 1:n_dims
        coords <- as.matrix(cluster_data[, dim_indices, drop = FALSE])
      }
      
      # === FIX: max_k increased (25 instead of 10) for more flexibility ===
      max_k <- min(25, floor(nrow(coords) / 2))
      k_range <- 2:max_k
      
      # === METHOD-SPECIFIC CALCULATIONS ===
      
      # === SILHOUETTE (for all methods) - TOP-3 CANDIDATES ===
      # CRITICAL: Silhouette can fail with 1D data
      n_dims_actual <- ncol(coords)
      
      silhouette_results <- tryCatch({
        silhouette_values <- sapply(k_range, function(k) {
          set.seed(123)
          
          # Isolate ALL method-specific parameters
          if (method == "kmeans") {
            km <- kmeans(coords, centers = k, nstart = 25, iter.max = 100)
            clusters <- km$cluster
          } else if (method == "hierarchical") {
            hc_method_val <- isolate(input$hclust_method %||% "average")
            hc <- hclust(dist(coords), method = hc_method_val)
            clusters <- cutree(hc, k = k)
          } else if (method == "pam") {
            pam_result <- cluster::pam(coords, k = k)
            clusters <- pam_result$clustering
          } else if (method == "fuzzy") {
            fuzzy_m_val <- isolate(input$fuzzy_m %||% 2)
            fuzzy_result <- cluster::fanny(coords, k = k, memb.exp = fuzzy_m_val, maxit = 100, tol = 0.001)
            clusters <- apply(fuzzy_result$membership, 1, which.max)
          } else if (method == "gmm") {
            if (!requireNamespace("mclust", quietly = TRUE)) {
              return(NA)
            }
            library(mclust)
            gmm_model_type_val <- isolate(input$gmm_model_type %||% "auto")
            if (gmm_model_type_val == "auto") {
              gmm_result <- Mclust(coords, G = k, verbose = FALSE)
            } else {
              gmm_result <- Mclust(coords, G = k, modelNames = gmm_model_type_val, verbose = FALSE)
            }
            if (is.null(gmm_result)) return(NA)
            clusters <- gmm_result$classification
          }
          
          # CRITICAL: Secure dist() for 1D data
          # silhouette() needs dist object (not matrix!)
          if (ncol(coords) == 1) {
            # 1D: dist() on vector (drop=TRUE important!)
            dist_mat <- dist(coords[, 1, drop = TRUE])
          } else {
            dist_mat <- dist(coords)
          }
          
          sil <- cluster::silhouette(clusters, dist_mat)
          mean(sil[, 3])
        })
        
        # Top-3 candidates sorted by silhouette value
        sil_order <- order(silhouette_values, decreasing = TRUE, na.last = TRUE)
        top3_indices <- head(sil_order, 3)
        top3_k <- k_range[top3_indices]
        top3_values <- silhouette_values[top3_indices]
        
        list(
          best_k = k_range[which.max(silhouette_values)],
          top3_k = top3_k,
          top3_values = top3_values,
          all_values = silhouette_values
        )
      }, error = function(e) {
        message("Silhouette failed: ", e$message)
        list(best_k = NA, top3_k = rep(NA, 3), top3_values = rep(NA, 3), all_values = NA)
      })
      
      silhouette_k <- silhouette_results$best_k
      
      # METHOD 2: Depends on clustering method
      method2_k <- NA
      method2_name <- ""
      method2_desc <- ""
      method2_when <- ""
      
      # Isolate parameters ONCE at the beginning for all method-2 calculations
      hc_method_val <- isolate(input$hclust_method %||% "average")
      fuzzy_m_val <- isolate(input$fuzzy_m %||% 2)
      gmm_model_type_val <- isolate(input$gmm_model_type %||% "auto")
      
      if (method == "kmeans") {
        # ELBOW for K-Means
        method2_name <- "Elbow Method"
        method2_desc <- "Finds the 'elbow' in the within-cluster variance curve."
        method2_when <- "Good for initial orientation. Less precise for gradual transitions, but fast and intuitive."
        
        wss_values <- sapply(k_range, function(k) {
          set.seed(123)
          km <- kmeans(coords, centers = k, nstart = 25, iter.max = 100)
          km$tot.withinss
        })
        
        if (length(wss_values) >= 3) {
          wss_diff1 <- diff(wss_values)
          wss_diff2 <- diff(wss_diff1)
          method2_k <- k_range[which.max(abs(wss_diff2)) + 1]
        } else {
          method2_k <- k_range[1]
        }
        
      } else if (method == "hierarchical") {
        # COPHENETIC CORRELATION for Hierarchical
        method2_name <- "Cophenetic Correlation"
        method2_desc <- "Measures how well the hierarchy preserves original distances."
        method2_when <- "Best choice for hierarchical clustering. High values (>0.75) indicate good hierarchy quality."
        
        method2_k <- tryCatch({
          coph_values <- sapply(k_range, function(k) {
            hc <- hclust(dist(coords), method = hc_method_val)
            cor(dist(coords), cophenetic(hc))
          })
          # For Cophenetic: Highest value is not necessarily at a specific k,
          # but constant - use Gap or Silhouette instead
          # Simplified: Show k with best balance
          NA  # Disabled - for Hierarchical use only Silhouette + Gap
        }, error = function(e) NA)
        
      } else if (method == "fuzzy") {
        # PARTITION COEFFICIENT for Fuzzy
        method2_name <- "Partition Coefficient (Dunn)"
        method2_desc <- "Measures the sharpness of fuzzy assignments (higher = sharper)."
        method2_when <- "Best choice for Fuzzy K-Means. Values >0.7 = sharp clusters, <0.5 = very fuzzy."
        
        method2_k <- tryCatch({
          pc_values <- sapply(k_range, function(k) {
            fuzzy_result <- cluster::fanny(coords, k = k, memb.exp = fuzzy_m_val, maxit = 100, tol = 0.001)
            membership <- fuzzy_result$membership
            mean(apply(membership, 1, function(row) sum(row^2)))
          })
          k_range[which.max(pc_values)]
        }, error = function(e) NA)
        
      } else if (method == "pam") {
        # AVERAGE SILHOUETTE WIDTH for PAM (built-in via pam$silinfo)
        method2_name <- "Average Silhouette Width (ASW)"
        method2_desc <- "Mean silhouette value computed directly from PAM's internal distance matrix."
        method2_when <- "Ideal for PAM: uses the same distance matrix as the clustering itself. Higher = better separated clusters."

        method2_k <- tryCatch({
          asw_values <- sapply(k_range, function(k) {
            pam_result <- cluster::pam(coords, k = k)
            pam_result$silinfo$avg.width
          })
          k_range[which.max(asw_values)]
        }, error = function(e) NA)

      } else if (method == "gmm") {
        # BIC for GMM
        method2_name <- "BIC (Bayesian Information Criterion)"
        method2_desc <- "Bayesian Information Criterion (BIC) - higher = better (mclust convention). Balances model fit and complexity."
        method2_when <- "Standard for GMM. Strongly regularizing - often prefers fewer clusters. Note: In literature sometimes defined as 'lower = better', but mclust-BIC is 'higher = better' scaled."
        
        method2_k <- tryCatch({
          if (!requireNamespace("mclust", quietly = TRUE)) return(NA)
          library(mclust)
          
          bic_values <- sapply(k_range, function(k) {
            if (gmm_model_type_val == "auto") {
              gmm_result <- Mclust(coords, G = k, verbose = FALSE)
            } else {
              gmm_result <- Mclust(coords, G = k, modelNames = gmm_model_type_val, verbose = FALSE)
            }
            if (is.null(gmm_result)) return(-Inf)  # Invalid model -> lowest BIC
            gmm_result$bic
          })
          k_range[which.max(bic_values)]  # === FIX: MAX for BIC in mclust (higher = better) ===
        }, error = function(e) NA)
      }
      
      # === GAP STATISTIC: TWO VARIANTS (firstSE + max) ===
      gap_results <- if (method != "gmm") {
        tryCatch({
          # === FIX 10: Gap statistic bugfix (clusGap FUN + Off-by-one) ===

          # CRITICAL: Handle edge case
          if (max_k < 2) {
            showNotification(
              "Gap statistic requires at least K.max=2. Not enough data points!",
              type = "warning", duration = 5
            )
            return(NA)
          }
          
          # === FIX 7b: Adaptive B for gap statistic (large datasets) ===
          gap_B <- if (nrow(coords) > 100) 100 else 50
          
          cluster_fun <- if (method == "kmeans") {
            function(x, k) {
              tryCatch({
                km <- kmeans(x, k, nstart = 25, iter.max = 100)
                if (is.null(km$cluster)) stop("K-Means returned no clusters")
                # CRITICAL: clusGap expects list with $cluster
                list(cluster = km$cluster)
              }, error = function(e) {
                # Fallback: Simpler K-Means
                km <- kmeans(x, k, nstart = 5, iter.max = 50)
                list(cluster = km$cluster)
              })
            }
          } else if (method == "hierarchical") {
            function(x, k) {
              tryCatch({
                # Do not use pre-computed distance matrix here (x can be bootstrap sample)
                hc <- hclust(dist(x), method = hc_method_val)
                clusters <- cutree(hc, k = k)
                if (is.null(clusters) || length(clusters) != nrow(x)) {
                  stop("Hierarchical clustering failed")
                }
                list(cluster = clusters)
              }, error = function(e) {
                # Fallback: complete linkage
                hc <- hclust(dist(x), method = "complete")
                list(cluster = cutree(hc, k = k))
              })
            }
          } else if (method == "pam") {
            function(x, k) {
              tryCatch({
                pam_result <- cluster::pam(x, k = k)
                list(cluster = pam_result$clustering)
              }, error = function(e) {
                # Fallback: K-Means
                km <- kmeans(x, centers = k, nstart = 10, iter.max = 50)
                list(cluster = km$cluster)
              })
            }
          } else if (method == "fuzzy") {
            function(x, k) {
              tryCatch({
                fuzzy_result <- cluster::fanny(x, k = k, memb.exp = fuzzy_m_val, maxit = 100, tol = 0.001)
                # Check if membership exists
                if (is.null(fuzzy_result$membership)) {
                  stop("Fuzzy clustering returned no membership")
                }
                list(cluster = apply(fuzzy_result$membership, 1, which.max))
              }, error = function(e) {
                # Fallback: K-Means when Fuzzy fails
                km <- kmeans(x, centers = k, nstart = 10, iter.max = 50)
                list(cluster = km$cluster)
              })
            }
          }

          # Gap statistic WITH CORRECT FUN
          gap_result <- cluster::clusGap(coords, FUN = cluster_fun, K.max = max_k,
                                        B = gap_B, verbose = FALSE)
          
          # CRITICAL: Validate structure
          if (is.null(gap_result)) stop("clusGap returned NULL")
          
          # ROBUST access to Tab component
          gap_tab <- if (inherits(gap_result, "clusGap")) {
            gap_result$Tab
          } else if (is.list(gap_result) && "Tab" %in% names(gap_result)) {
            gap_result$Tab
          } else if (is.matrix(gap_result) || is.data.frame(gap_result)) {
            gap_result
          } else {
            stop("Unexpected gap_result structure")
          }
          
          if (is.null(gap_tab)) stop("Could not extract Tab")
          
          # ROBUST access to columns
          gap_values <- if (is.matrix(gap_tab)) {
            if (!("gap" %in% colnames(gap_tab))) stop("Column 'gap' missing")
            gap_tab[, "gap"]
          } else if (is.data.frame(gap_tab)) {
            if (!("gap" %in% names(gap_tab))) stop("Column 'gap' missing")
            gap_tab$gap
          } else {
            stop(sprintf("gap_tab structure unexpected: %s", class(gap_tab)))
          }
          
          se_values <- if (is.matrix(gap_tab)) {
            gap_tab[, "SE.sim"]
          } else {
            gap_tab$SE.sim
          }
          
          # === FIX 10: TWO Gap variants ===
          # Variant 1: Standard firstSE rule (conservative)
          gap_firstSE_k <- NA
          for (i in 1:(length(gap_values) - 1)) {
            if (gap_values[i] >= gap_values[i + 1] - se_values[i + 1]) {
              gap_firstSE_k <- k_range[i]
              break
            }
          }
          if (is.na(gap_firstSE_k)) {
            gap_firstSE_k <- k_range[which.max(gap_values)]
          }
          
          # Variant 2: Maximum Gap (less conservative)
          gap_max_k <- k_range[which.max(gap_values)]
          
          list(
            firstSE = gap_firstSE_k,
            max = gap_max_k,
            all_values = gap_values
          )
        }, error = function(e) {
          message("Gap statistic failed: ", e$message)
          list(firstSE = NA, max = NA, all_values = NA)
        })
      } else {
        list(firstSE = NA, max = NA, all_values = NA)  # GMM uses BIC instead of Gap
      }
      
      gap_k <- gap_results$firstSE  # For backward compatibility
      
      removeNotification("auto_cluster_calc")
      
      # === FIX 11: sqrt(n/2) RULE (Mardia et al. 1979) ===
      # Heuristic for initial orientation
      sqrt_rule_k <- round(sqrt(nrow(coords) / 2))
      sqrt_rule_k <- max(2, min(sqrt_rule_k, max_k))  # Constrain to [2, max_k]
      
      # Store values (EXTENDED)
      auto_cluster_details$silhouette_k <- silhouette_k
      auto_cluster_details$silhouette_top3_k <- silhouette_results$top3_k
      auto_cluster_details$silhouette_top3_values <- silhouette_results$top3_values
      auto_cluster_details$method2_k <- method2_k
      auto_cluster_details$method2_name <- method2_name
      auto_cluster_details$gap_firstSE_k <- gap_results$firstSE
      auto_cluster_details$gap_max_k <- gap_results$max
      auto_cluster_details$sqrt_rule_k <- sqrt_rule_k
      auto_cluster_details$n_points <- nrow(coords)  # For notification
      
      # MODAL WITH METHOD-SPECIFIC INFORMATION
      showModal(modalDialog(
        title = sprintf(tr("plot.kmeans.auto.title"), method_label),
        size = "l",
        easyClose = FALSE,

        tagList(
          # Info banner
          div(class = "alert alert-success", style="margin-bottom:15px;",
            tags$strong(tr("mod_cluster.auto.info")), sprintf(" %s", sprintf(tr("mod_cluster.auto.calc.with"), method_label))
          ),
          
          # === SILHOUETTE: TOP-3 CANDIDATES ===
          if (!is.na(silhouette_k) && length(auto_cluster_details$silhouette_top3_k) >= 1) {
            top3_k <- auto_cluster_details$silhouette_top3_k
            top3_values <- auto_cluster_details$silhouette_top3_values
            
            valid_indices <- which(!is.na(top3_k))
            
            if (length(valid_indices) > 0) {
              tagList(
                div(class = "well", style="background-color: #e3f2fd; border-left: 4px solid #2196F3;",
                  h4(tr("mod_cluster.auto.silhouette.title"), style="margin-top:0; color: #1976D2; margin-bottom:15px;"),
                  tags$p(class="text-muted", style="font-size:0.9em; margin-bottom:10px;",
                    tags$strong(tr("mod_cluster.auto.what.measures")), " ", tr("mod_cluster.auto.silhouette.desc")),
                  tags$p(class="text-muted", style="font-size:0.9em; margin-bottom:15px;",
                    tags$strong(tr("mod_cluster.auto.when.use")), " ", tr("mod_cluster.auto.silhouette.when")),

                  # Top-3 buttons side by side
                  fluidRow(
                    lapply(valid_indices, function(i) {
                      k_val <- top3_k[i]
                      sil_val <- top3_values[i]
                      rank_label <- c(tr("mod_cluster.auto.rank.best"), tr("mod_cluster.auto.rank.2nd"), tr("mod_cluster.auto.rank.3rd"))[i]
                      
                      column(4,
                        actionButton(
                          paste0("auto_accept_silhouette_", i),
                          sprintf("✅ k=%d\n(%.3f)", k_val, sil_val),
                          class = if(i == 1) "btn btn-primary" else "btn btn-default",
                          style = "width:100%; white-space:pre-wrap; margin-bottom:5px;"
                        ),
                        tags$small(rank_label, style="display:block; text-align:center; color:#666;")
                      )
                    })
                  )
                ),
                tags$br()
              )
            }
          },
          
          if (!is.na(silhouette_k)) tags$br(),
          
          # METHOD 2 (method-specific)
          if (!is.na(method2_k) && nchar(method2_name) > 0) {
            tagList(
              div(class = "well", style="background-color: #fff3e0; border-left: 4px solid #FF9800;",
                fluidRow(
                  column(8,
                    h4(paste0("📐 ", method2_name), style="margin-top:0; color: #F57C00;"),
                    tags$p(tags$strong(tr("mod_cluster.auto.recommendation")), sprintf(tr("mod_cluster.auto.clusters"), method2_k)),
                    tags$p(class="text-muted", style="font-size:0.9em;",
                      tags$strong(tr("mod_cluster.auto.what.measures")), " ", method2_desc),
                    tags$p(class="text-muted", style="font-size:0.9em;",
                      tags$strong(tr("mod_cluster.auto.when.use")), " ", method2_when)
                  ),
                  column(4, style="text-align:right; padding-top:25px;",
                    actionButton("auto_accept_method2",
                               sprintf(tr("mod_cluster.auto.clusters"), method2_k),
                               class = "btn btn-warning btn-lg",
                               style = "width:100%;")
                  )
                )
              ),
              tags$br()
            )
          },
          
          # === GAP: TWO VARIANTS (firstSE + max) ===
          if (!is.na(auto_cluster_details$gap_firstSE_k) || !is.na(auto_cluster_details$gap_max_k)) {
            gap_firstSE <- auto_cluster_details$gap_firstSE_k
            gap_max <- auto_cluster_details$gap_max_k

            tagList(
              div(class = "well", style="background-color: #e8f5e9; border-left: 4px solid #4CAF50;",
                h4(tr("mod_cluster.auto.gap.title"), style="margin-top:0; color: #388E3C; margin-bottom:15px;"),
                tags$p(class="text-muted", style="font-size:0.9em; margin-bottom:10px;",
                  tags$strong(tr("mod_cluster.auto.what.measures")), " ", tr("mod_cluster.auto.gap.desc")),
                tags$p(class="text-muted", style="font-size:0.9em; margin-bottom:15px;",
                  tags$strong(tr("mod_cluster.auto.when.use")), " ", tr("mod_cluster.auto.gap.when")),

                # Two variants side by side
                fluidRow(
                  if (!is.na(gap_firstSE)) {
                    column(6,
                      actionButton(
                        "auto_accept_gap_firstSE",
                        sprintf("\u2705 k=%d\n(firstSE)", gap_firstSE),
                        class = "btn btn-success",
                        style = "width:100%; white-space:pre-wrap; margin-bottom:5px;"
                      ),
                      tags$small(tr("mod_cluster.auto.gap.conservative"), style="display:block; text-align:center; color:#666;")
                    )
                  },
                  if (!is.na(gap_max)) {
                    column(6,
                      actionButton(
                        "auto_accept_gap_max",
                        sprintf("\u2705 k=%d\n(Maximum)", gap_max),
                        class = "btn btn-default",
                        style = "width:100%; white-space:pre-wrap; margin-bottom:5px;"
                      ),
                      tags$small(tr("mod_cluster.auto.gap.less.conservative"), style="display:block; text-align:center; color:#666;")
                    )
                  }
                )
              ),
              tags$br()
            )
          },
          
          # === sqrt(n/2) RULE (FIX 11) ===
          if (!is.na(sqrt_rule_k)) {
            tagList(
              div(class = "well", style="background-color: #fff8e1; border-left: 4px solid #FFA726;",
                fluidRow(
                  column(8,
                    h4(tr("mod_cluster.auto.sqrt.title"), style="margin-top:0; color: #F57C00;"),
                    tags$p(tags$strong(tr("mod_cluster.auto.recommendation")), sprintf(tr("mod_cluster.auto.clusters"), sqrt_rule_k)),
                    tags$p(class="text-muted", style="font-size:0.9em;",
                      tags$strong(tr("mod_cluster.auto.sqrt.what")), " ", sprintf(tr("mod_cluster.auto.sqrt.desc"), nrow(coords))),
                    tags$p(class="text-muted", style="font-size:0.9em;",
                      tags$strong(tr("mod_cluster.auto.when.use")), " ", tr("mod_cluster.auto.sqrt.when"))
                  ),
                  column(4, style="text-align:right; padding-top:25px;",
                    actionButton("auto_accept_sqrt",
                               sprintf(tr("mod_cluster.auto.clusters"), sqrt_rule_k),
                               class = "btn btn-warning btn-lg",
                               style = "width:100%;")
                  )
                )
              ),
              tags$br()
            )
          },
          
          tags$hr(),
          
          # === HELP TEXT: INTERPRETATION OF SUGGESTIONS ===
          div(class = "panel panel-info", style="margin-bottom:15px;",
            div(class = "panel-heading", style="cursor:pointer;",
              tags$strong(tr("mod_cluster.auto.help.title"))
            ),
            div(class = "panel-body", style="font-size:0.9em;",
              tags$p(
                tr("mod_cluster.auto.help.intro"), " ",
                tags$strong(tr("mod_cluster.auto.help.intro2")),
                " ", tr("mod_cluster.auto.help.intro3")
              ),
              tags$ul(
                tags$li(tags$strong("Silhouette"), " ", tr("mod_cluster.auto.help.silhouette")),
                tags$li(tags$strong("Elbow-Methode"), " ", tr("mod_cluster.auto.help.elbow")),
                tags$li(tags$strong("Gap-Statistik"), " ", tr("mod_cluster.auto.help.gap"),
                  tags$ul(
                    tags$li(tags$em("firstSE:"), " ", tr("mod_cluster.auto.help.gap.firstse")),
                    tags$li(tags$em("Maximum:"), " ", tr("mod_cluster.auto.help.gap.max"))
                  )
                ),
                tags$li(tags$strong("\u221A(n/2)-Regel"), " ", tr("mod_cluster.auto.help.sqrt"))
              ),
              div(class = "alert alert-warning", style="margin-top:10px; margin-bottom:10px;",
                tags$strong("\U0001F449 ", tr("mod_cluster.dims.note")),
                " ", tr("mod_cluster.auto.help.note"), " ",
                tags$strong(tr("mod_cluster.auto.help.note2")),
                " ", tr("mod_cluster.auto.help.note3")
              ),
              tags$p(class = "text-muted", style="margin-bottom:0;",
                tags$strong(tr("mod_cluster.auto.recommendation")),
                " ", tr("mod_cluster.auto.help.rec"), " ",
                tags$strong(tr("mod_cluster.auto.help.rec2")),
                tr("mod_cluster.auto.help.rec3")
              )
            )
          ),
          
          tags$hr(),
          
          div(class = "alert alert-info", style="margin-bottom:0;",
            tags$strong("\U0001F4A1 ", tr("mod_cluster.auto.recommendation")),
            if (method == "kmeans") {
              paste0(" ", tr("mod_cluster.auto.tip.kmeans"))
            } else if (method == "hierarchical") {
              paste0(" ", tr("mod_cluster.auto.tip.hier"))
            } else if (method == "fuzzy") {
              paste0(" ", tr("mod_cluster.auto.tip.fuzzy"))
            } else if (method == "gmm") {
              paste0(" ", tr("mod_cluster.auto.tip.gmm"))
            }
          )
        ),

        footer = modalButton(tr("mod_cluster.auto.btn.cancel"))
      ))
      
    }, error = function(e) {
      removeNotification("auto_cluster_calc")
      showNotification(
        paste(tr("mod_cluster.auto.error"), e$message),
        type = "error",
        duration = 10
      )
      message("Auto-Cluster Error: ", e$message)
    })
  })

  # === Observer for Silhouette Top-3 buttons ===
  observeEvent(input$auto_accept_silhouette_1, {
    k <- isolate(auto_cluster_details$silhouette_top3_k[1])
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.silhouette"), tr("mod_cluster.auto.rank.best"), k),
        type = "message", duration = 3
      )
    }
  })
  
  observeEvent(input$auto_accept_silhouette_2, {
    k <- isolate(auto_cluster_details$silhouette_top3_k[2])
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.silhouette"), tr("mod_cluster.auto.rank.2nd"), k),
        type = "message", duration = 3
      )
    }
  })

  observeEvent(input$auto_accept_silhouette_3, {
    k <- isolate(auto_cluster_details$silhouette_top3_k[3])
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.silhouette"), tr("mod_cluster.auto.rank.3rd"), k),
        type = "message", duration = 3
      )
    }
  })

  # Observer for Method-2 button (method-specific: Elbow/Cophenetic/PC/BIC)
  observeEvent(input$auto_accept_method2, {
    k <- isolate(auto_cluster_details$method2_k)
    method_name <- isolate(auto_cluster_details$method2_name)
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.method2"), method_name, k),
        type = "message",
        duration = 3
      )
    }
  })

  # === Observer for Gap two-variant buttons ===
  observeEvent(input$auto_accept_gap_firstSE, {
    k <- isolate(auto_cluster_details$gap_firstSE_k)
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.gap.firstse"), k),
        type = "message", duration = 3
      )
    }
  })

  observeEvent(input$auto_accept_gap_max, {
    k <- isolate(auto_cluster_details$gap_max_k)
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.gap.max"), k),
        type = "message", duration = 3
      )
    }
  })

  # Observer for sqrt(n/2) rule button (FIX 11)
  observeEvent(input$auto_accept_sqrt, {
    k <- isolate(auto_cluster_details$sqrt_rule_k)
    n <- isolate(auto_cluster_details$n_points)
    if (!is.null(k) && !is.na(k)) {
      updateSliderInput(session, "num_clusters", value = k)
      removeModal()
      showNotification(
        sprintf(tr("mod_cluster.auto.notify.sqrt"), k),
        type = "message",
        duration = 3
      )
    }
  })

  kmeans_res <- eventReactive(input$refresh3, {
    req(ca_result(), input$x_dim, input$y_dim)

    # Isolate ALL parameters (frozen at refresh click)
    method <- isolate(input$clustering_method %||% "kmeans")
    num_clusters <- isolate(input$num_clusters %||% 3)
    cluster_on_val <- isolate(input$cluster_on %||% "both")
    n_dims_val <- isolate(input$kmeans_n_dims %||% 2)
    hc_method_val <- isolate(input$hclust_method %||% "average")
    fuzzy_m_val <- isolate(input$fuzzy_m %||% 2)
    gmm_model_type_val <- isolate(input$gmm_model_type %||% "auto")
    x_dim_val <- isolate(input$x_dim)
    y_dim_val <- isolate(input$y_dim)
    method_label <- switch(method,
                          "kmeans" = "K-Means",
                          "hierarchical" = "Hierarchical",
                          "pam" = "PAM (Medoids)",
                          "fuzzy" = "Fuzzy K-Means",
                          "gmm" = "GMM")

    withProgress(message = sprintf('%s Clustering in progress...', method_label), {
      incProgress(0.2, detail = "Extracting CA coordinates...")
      
      tryCatch({
        res <- ca_result()
        x_idx <- as.numeric(gsub("Dim", "", x_dim_val))
        y_idx <- as.numeric(gsub("Dim", "", y_dim_val))
        
        # Active points for clustering
        row_coords <- as.data.frame(res$row$coord)
        col_coords <- as.data.frame(res$col$coord)
        
        # === CRITICAL: Validate that CA is current (Selected filter) ===
        # Check if CA data contains elements that have since been set to Selected=FALSE
        if (!is.null(meta_data) && !is.null(meta_data$data)) {
          # Sites validieren
          if (!is.null(meta_data$data$sites) && "Selected" %in% names(meta_data$data$sites)) {
            selected_sites <- meta_data$data$sites$Entity[meta_data$data$sites$Selected]
            deselected_sites <- setdiff(rownames(row_coords), selected_sites)
            
            if (length(deselected_sites) > 0) {
              showNotification(
                sprintf("CA data is outdated! %d sites are now deselected: %s\n\nPlease RECALCULATE CA before clustering!",
                       length(deselected_sites),
                       paste(head(deselected_sites, 3), collapse=", ")),
                type = "error", duration = 10
              )
              stop(sprintf("CA contains %d deselected sites. Please recalculate CA!", length(deselected_sites)))
            }
          }
          
          # Validate types
          if (!is.null(meta_data$data$types) && "Selected" %in% names(meta_data$data$types)) {
            selected_types <- meta_data$data$types$Entity[meta_data$data$types$Selected]
            deselected_types <- setdiff(rownames(col_coords), selected_types)
            
            if (length(deselected_types) > 0) {
              showNotification(
                sprintf("CA data is outdated! %d types are now deselected: %s\n\nPlease RECALCULATE CA before clustering!",
                       length(deselected_types),
                       paste(head(deselected_types, 3), collapse=", ")),
                type = "error", duration = 10
              )
              stop(sprintf("CA contains %d deselected types. Please recalculate CA!", length(deselected_types)))
            }
          }
        }
        
        active_data <- rbind(
          data.frame(row_coords, label = rownames(row_coords), type = 'Site', 
                     element_type = 'Active', stringsAsFactors = FALSE),
          data.frame(col_coords, label = rownames(col_coords), type = 'Type', 
                     element_type = 'Active', stringsAsFactors = FALSE)
        )
        
        # Supplementary points for projection (SHOULD be displayed in clustering!)
        supplementary_data <- data.frame()
        if (!is.null(res$row.sup) && !is.null(res$row.sup$coord)) {
          suppl_row_coords <- as.data.frame(res$row.sup$coord)
          
          # === VALIDATION: Check if supplementary sites have been deselected ===
          if (!is.null(meta_data) && !is.null(meta_data$data) && 
              !is.null(meta_data$data$sites) && "Selected" %in% names(meta_data$data$sites)) {
            selected_sites <- meta_data$data$sites$Entity[meta_data$data$sites$Selected]
            deselected_suppl_sites <- setdiff(rownames(suppl_row_coords), selected_sites)
            
            if (length(deselected_suppl_sites) > 0) {
              showNotification(
                sprintf("CA data outdated! %d supplementary sites are now deselected: %s\n\nRECALCULATE CA!",
                       length(deselected_suppl_sites),
                       paste(deselected_suppl_sites, collapse=", ")),
                type = "error", duration = 10
              )
              stop(sprintf("CA contains %d deselected supplementary sites. Please recalculate CA!", length(deselected_suppl_sites)))
            }
          }
          
          supplementary_data <- rbind(supplementary_data, data.frame(
            suppl_row_coords, label = rownames(suppl_row_coords), 
            type = 'Site', element_type = 'Supplementary', stringsAsFactors = FALSE))
        }
        if (!is.null(res$col.sup) && !is.null(res$col.sup$coord)) {
          suppl_col_coords <- as.data.frame(res$col.sup$coord)
          
          # === VALIDATION: Check if supplementary types have been deselected ===
          if (!is.null(meta_data) && !is.null(meta_data$data) && 
              !is.null(meta_data$data$types) && "Selected" %in% names(meta_data$data$types)) {
            selected_types <- meta_data$data$types$Entity[meta_data$data$types$Selected]
            deselected_suppl_types <- setdiff(rownames(suppl_col_coords), selected_types)
            
            if (length(deselected_suppl_types) > 0) {
              showNotification(
                sprintf("CA data outdated! %d supplementary types are now deselected: %s\n\nRECALCULATE CA!",
                       length(deselected_suppl_types),
                       paste(head(deselected_suppl_types, 3), collapse=", ")),
                type = "error", duration = 10
              )
              stop(sprintf("CA contains %d deselected supplementary types. Please recalculate CA!", length(deselected_suppl_types)))
            }
          }
          
          supplementary_data <- rbind(supplementary_data, data.frame(
            suppl_col_coords, label = rownames(suppl_col_coords), 
            type = 'Type', element_type = 'Supplementary', stringsAsFactors = FALSE))
        }
        
        # Data for ACTIVE clustering (only active points!)
        cluster_data <- switch(cluster_on_val,
                               "rows" = subset(active_data, type == 'Site'),
                               "cols" = subset(active_data, type == 'Type'),
                               "both" = active_data)
        
        validate(need(nrow(cluster_data) >= num_clusters,
                      sprintf("Not enough ACTIVE data points (%d) for %d clusters!",
                              nrow(cluster_data), num_clusters)))
        
        incProgress(0.5, detail = sprintf("Performing %s on active points...", method_label))

        # MULTI-DIMENSIONAL CLUSTERING (scientific improvement)
        n_dims <- n_dims_val
        n_dims <- min(n_dims, ncol(cluster_data) - 3)  # max available dimensions (minus label, type, element_type)

        if (n_dims == 2) {
          # Standard: only selected 2 dimensions
          coords <- as.matrix(cluster_data[, c(x_idx, y_idx), drop = FALSE])
          colnames(coords) <- c("x", "y")
        } else {
          # Multi-Dimensional: first n dimensions
          dim_indices <- 1:n_dims
          coords <- as.matrix(cluster_data[, dim_indices, drop = FALSE])
          colnames(coords) <- paste0("Dim", dim_indices)
        }

        # === CLUSTERING METHODS ===
        km <- NULL
        clustering_result <- list()

        if (method == "kmeans") {
          # === K-MEANS ===
          # Seed for reproducibility
          set.seed(123)

          km <- kmeans(coords, centers = num_clusters, nstart = 50, iter.max = 200)
          clustering_result$cluster <- km$cluster
          clustering_result$centers <- km$centers

        } else if (method == "hierarchical") {
          # === HIERARCHICAL CLUSTERING ===
          hc_method <- hc_method_val
          dist_matrix <- dist(coords)
          hc <- hclust(dist_matrix, method = hc_method)
          hc_clusters <- cutree(hc, k = num_clusters)

          centers <- t(sapply(1:num_clusters, function(i) {
            colMeans(coords[hc_clusters == i, , drop = FALSE])
          }))
          rownames(centers) <- 1:num_clusters

          clustering_result$cluster <- hc_clusters
          clustering_result$centers <- centers
          clustering_result$hc_tree <- hc  # Store for dendrogram

        } else if (method == "pam") {
          # === PAM (Partitioning Around Medoids) ===
          # χ²-consistent alternative: uses real data points as cluster centers,
          # works with any distance metric, more robust to outliers than k-means.
          pam_result <- cluster::pam(coords, k = num_clusters, metric = "euclidean")

          clustering_result$cluster  <- pam_result$clustering
          clustering_result$centers  <- pam_result$medoids
          clustering_result$pam_obj  <- pam_result  # store for silinfo

        } else if (method == "fuzzy") {
          # === FUZZY K-MEANS ===
          fuzzy_m <- fuzzy_m_val
          
          # === FIX 8a OPTIMIZED: Realistic maxit + tolerance for faster convergence ===
          # CRITICAL: 1000 iterations take too long!
          # New strategy: Lower maxit (100-200) + higher tolerance (0.001 instead of 0.0001)
          fuzzy_maxit <- if (nrow(coords) > 100) 200 else 100
          fuzzy_tol <- 0.001  # Higher tolerance = faster convergence (default: 1e-4)
          
          fuzzy_result <- cluster::fanny(coords, k = num_clusters, memb.exp = fuzzy_m, 
                                        maxit = fuzzy_maxit, tol = fuzzy_tol)

          if (!is.null(fuzzy_result$convergence)) {
            if (isTRUE(!fuzzy_result$convergence)) {
              showNotification(
                sprintf("Warning: Fuzzy K-Means did not converge (maxit=%d, tol=%.4f). Increase maxit or choose different parameters.",
                       fuzzy_maxit, fuzzy_tol),
                type = "warning", duration = 8
              )
            }
          }
          # Alternative check: Iterations vs. maxit
          if (!is.null(fuzzy_result$iterations)) {
            iterations_val <- as.numeric(fuzzy_result$iterations)[1]  # Safe: only first element
            if (!is.na(iterations_val) && iterations_val >= fuzzy_maxit) {
              showNotification(
                sprintf("Info: Fuzzy K-Means reached maxit=%d. Possibly no full convergence, but result often sufficiently good.",
                       fuzzy_maxit),
                type = "warning", duration = 6
              )
            }
          }

          # Hard assignment: Cluster with highest membership
          hard_clusters <- apply(fuzzy_result$membership, 1, which.max)

          # Centers as weighted means based on membership (ROBUST)
          centers_list <- lapply(1:num_clusters, function(i) {
            weights <- fuzzy_result$membership[, i]
            colSums(coords * weights) / sum(weights)
          })
          centers <- do.call(rbind, centers_list)
          rownames(centers) <- 1:num_clusters
          
          # Validate dimensions
          if (is.null(dim(centers)) || nrow(centers) != num_clusters || ncol(centers) != ncol(coords)) {
            stop(sprintf("Fuzzy centers have wrong dimensions: %s (expected: %d x %d)",
                        paste(dim(centers), collapse=" x "), num_clusters, ncol(coords)))
          }

          clustering_result$cluster <- hard_clusters
          clustering_result$centers <- centers
          clustering_result$membership <- fuzzy_result$membership  # Fuzzy memberships

        } else if (method == "gmm") {
          # === GAUSSIAN MIXTURE MODELS (GMM) ===
          if (!requireNamespace("mclust", quietly = TRUE)) {
            stop("GMM requires the 'mclust' package. Please install: install.packages('mclust')")
          }

          # Load mclust explicitly (requireNamespace does not load functions)
          library(mclust)

          gmm_model_type <- gmm_model_type_val

          # === FIX 4: GMM dimension validation ===
          # Check if n_dims matches available dimensions in coords
          if (n_dims > ncol(coords)) {
            stop(sprintf(
              "GMM error: n_dims=%d is larger than available dimensions in coords (%d). Please reduce n_dims or use more CA dimensions.",
              n_dims, ncol(coords)
            ))
          }

          # Validate coords
          if (is.null(coords) || !is.matrix(coords) || nrow(coords) == 0) {
            stop("Invalid coordinates for GMM: coords is NULL, empty or not a matrix")
          }

          # mclust model names (coords is already a matrix)
          if (gmm_model_type == "auto") {
            gmm_result <- Mclust(coords, G = num_clusters, verbose = FALSE)
          } else {
            gmm_result <- Mclust(coords, G = num_clusters, modelNames = gmm_model_type, verbose = FALSE)
          }

          # Check if GMM was successful
          if (is.null(gmm_result)) {
            stop("GMM model could not be computed (Mclust returned NULL)")
          }

          # Cluster-Zuordnungen
          gmm_clusters <- gmm_result$classification
          if (is.null(gmm_clusters)) {
            stop("GMM model did not return cluster assignments")
          }
          
          # === FIX 8b: GMM Empty Cluster Check ===
          gmm_cluster_sizes <- table(gmm_clusters)
          gmm_empty_clusters <- which(!(1:num_clusters %in% as.integer(names(gmm_cluster_sizes))))
          if (length(gmm_empty_clusters) > 0) {
            showNotification(
              sprintf("⚠️ Warning: GMM produced %d empty cluster(s) (%s). Choose fewer clusters or different seed.",
                     length(gmm_empty_clusters), paste(gmm_empty_clusters, collapse=", ")),
              type = "warning", duration = 10
            )
          }

          # Zentren (Mittelwerte der Komponenten)
          # gmm_result$parameters$mean is a matrix: columns = clusters, rows = dimensions
          if (is.null(gmm_result$parameters) || is.null(gmm_result$parameters$mean)) {
            stop("GMM model has no parameters or means")
          }

          mean_matrix <- gmm_result$parameters$mean
          if (is.null(dim(mean_matrix))) {
            # 1D-Fall: mean ist ein Vektor
            centers <- matrix(mean_matrix, nrow = num_clusters, ncol = 1)
          } else {
            # 2D+ Fall: transponieren (Zeilen = Cluster, Spalten = Dimensionen)
            centers <- t(mean_matrix)
          }
          rownames(centers) <- 1:num_clusters

          # Posterior probabilities (similar to fuzzy membership)
          gmm_prob <- gmm_result$z
          if (is.null(gmm_prob)) {
            # Fallback: hard assignment as 0/1 matrix
            gmm_prob <- matrix(0, nrow = nrow(coords), ncol = num_clusters)
            for (i in 1:nrow(coords)) {
              gmm_prob[i, gmm_clusters[i]] <- 1
            }
          }

          clustering_result$cluster <- gmm_clusters
          clustering_result$centers <- centers
          clustering_result$gmm_model <- gmm_result  # Store for BIC/Likelihood
          clustering_result$gmm_prob <- gmm_prob  # Posterior probabilities
        }

        # === FIX 9: CHECK EMPTY CLUSTERS (BEFORE REORDERING) - METHOD-SPECIFIC ===
        cluster_sizes <- table(clustering_result$cluster)
        empty_clusters <- which(!(1:num_clusters %in% as.integer(names(cluster_sizes))))
        if (length(empty_clusters) > 0) {
          # For K-Means/Hierarchical: ERROR (critical)
          # For Fuzzy/GMM: WARNING (hard assignment can be empty despite existing memberships)
          if (method %in% c("kmeans", "hierarchical", "pam")) {
            stop(sprintf(
              "❌ Clustering produced %d empty cluster(s) (%s)! Possible solutions: (1) Choose different cluster count, (2) Try different seed, (3) Try different method. Empty clusters occur when algorithm assigns no points to certain centers.",
              length(empty_clusters),
              paste(empty_clusters, collapse=", ")
            ))
          } else if (method == "fuzzy") {
            showNotification(
              sprintf("ℹ️ Info: Fuzzy K-Means produced %d cluster(s) (%s) without points with highest membership (hard assignment via which.max). Fuzzy memberships still exist for all clusters. Consider different k if needed.",
                     length(empty_clusters),
                     paste(empty_clusters, collapse=", ")),
              type = "warning", duration = 8
            )
          } else if (method == "gmm") {
            showNotification(
              sprintf("ℹ️ Info: GMM produced %d empty cluster(s) (%s) in maximum-likelihood assignment. Posterior probabilities still exist. Consider different k if needed.",
                     length(empty_clusters),
                     paste(empty_clusters, collapse=", ")),
              type = "warning", duration = 8
            )
          }
        }

        # Sort clusters by selected X-dimension (not hard-coded Dim.1!)
        sort_dim_name <- paste0("Dim.", x_idx)
        if (sort_dim_name %in% colnames(cluster_data)) {
          mean_per_cluster <- tapply(cluster_data[[sort_dim_name]], clustering_result$cluster,
                                     function(v) mean(v, na.rm = TRUE))
          # Sort cluster IDs by their X-dimension mean (ascending)
          sorted_means <- sort(mean_per_cluster)
          old_ids_in_order <- as.integer(names(sorted_means))
        } else {
          # Fallback: original order
          old_ids_in_order <- 1:num_clusters
        }

        new_ids_seq <- seq_len(num_clusters)
        old_to_new_mapping <- setNames(new_ids_seq, old_ids_in_order)

        # Apply mapping
        clustering_result$cluster <- old_to_new_mapping[as.character(clustering_result$cluster)]
        clustering_result$centers <- clustering_result$centers[old_ids_in_order, , drop = FALSE]
        
        # === FIX 9: DIMENSION VALIDATION BEFORE ROWNAMES ===
        if (length(old_ids_in_order) != num_clusters) {
          stop(sprintf(
            "Dimension error during reordering: old_ids_in_order has length %d, expected %d. This indicates a problem in the clustering algorithm.",
            length(old_ids_in_order), num_clusters
          ))
        }
        if (nrow(clustering_result$centers) != num_clusters) {
          stop(sprintf(
            "Dimension error: centers have %d rows, expected %d. Clustering result is inconsistent.",
            nrow(clustering_result$centers), num_clusters
          ))
        }
        
        rownames(clustering_result$centers) <- new_ids_seq

        # For Fuzzy: reorder membership matrix
        if (method == "fuzzy") {
          clustering_result$membership <- clustering_result$membership[, old_ids_in_order, drop = FALSE]
          colnames(clustering_result$membership) <- new_ids_seq
        }

        # For GMM: reorder posterior probabilities
        if (method == "gmm") {
          clustering_result$gmm_prob <- clustering_result$gmm_prob[, old_ids_in_order, drop = FALSE]
          colnames(clustering_result$gmm_prob) <- new_ids_seq
        }

        # Backward compatibility: km object for existing code
        km <- list(cluster = clustering_result$cluster, centers = clustering_result$centers)
        
        incProgress(0.2, detail = "Supplementary Punkte projizieren...")
        
        cluster_data$cluster <- factor(km$cluster)
        cluster_data$lab <- if (!is.null(input$show_labels_km) && input$show_labels_km) {
          substr(cluster_data$label, 1, input$label_chars_km %||% 12)
        } else ""
        
        # Assign supplementary points to nearest clusters (in n-dimensional space!)
        suppl_filtered <- data.frame()
        if (nrow(supplementary_data) > 0) {
          suppl_filtered <- switch(cluster_on_val,
                                   "rows" = subset(supplementary_data, type == 'Site'),
                                   "cols" = subset(supplementary_data, type == 'Type'),
                                   "both" = supplementary_data)
          if (nrow(suppl_filtered) > 0) {
            # Use same space as clustering (2D or nD)
            if (n_dims == 2) {
              suppl_coords <- suppl_filtered[, c(x_idx, y_idx), drop = FALSE]
            } else {
              suppl_coords <- suppl_filtered[, dim_indices, drop = FALSE]
            }

            suppl_clusters <- apply(suppl_coords, 1, function(point) {
              distances <- apply(km$centers, 1, function(center) sqrt(sum((point - center)^2)))
              which.min(distances)
            })
            suppl_filtered$cluster <- factor(suppl_clusters)
            suppl_filtered$lab <- if (!is.null(input$show_labels_km) && input$show_labels_km) {
              substr(suppl_filtered$label, 1, input$label_chars_km %||% 12)
            } else ""
          }
        }
        
        # Combine active and projected supplementary points
        all_cluster_data <- if (nrow(suppl_filtered) > 0) rbind(cluster_data, suppl_filtered) else cluster_data
        
        incProgress(0.1, detail = "Calculating quality metrics...")

        # EXTENDED QUALITY METRICS (scientific improvement)
        # 1. Silhouette Score (bekannt, jetzt im korrekten n-dim Raum)
        silhouette_score <- tryCatch({
          if (nrow(coords) > num_clusters && num_clusters > 1) {
            # 1D safety check: dist() on vector when ncol==1
            if (ncol(coords) == 1) {
              dist_mat <- dist(coords[, 1, drop = TRUE])
            } else {
              dist_mat <- dist(coords)
            }
            sil <- cluster::silhouette(as.integer(cluster_data$cluster), dist_mat)
            mean(sil[, 3])
          } else NA
        }, error = function(e) NA)

        # 2. Calinski-Harabasz Index (Variance Ratio Criterion)
        calinski_harabasz <- tryCatch({
          if (nrow(coords) > num_clusters && num_clusters > 1) {
            clusters <- as.integer(cluster_data$cluster)
            # Between-cluster variance
            grand_mean <- colMeans(coords)
            cluster_sizes <- table(clusters)
            bss_ch <- sum(sapply(unique(clusters), function(k) {
              cluster_mean <- colMeans(coords[clusters == k, , drop = FALSE])
              cluster_sizes[as.character(k)] * sum((cluster_mean - grand_mean)^2)
            }))
            # Within-cluster variance
            wss_ch <- sum(sapply(unique(clusters), function(k) {
              cluster_points <- coords[clusters == k, , drop = FALSE]
              cluster_mean <- colMeans(cluster_points)
              sum(apply(cluster_points, 1, function(pt) sum((pt - cluster_mean)^2)))
            }))
            # CH = (BSS / (k-1)) / (WSS / (n-k))
            ch <- (bss_ch / (num_clusters - 1)) / (wss_ch / (nrow(coords) - num_clusters))
            ch
          } else NA
        }, error = function(e) NA)

        # 3. Davies-Bouldin Index (average similarity)
        davies_bouldin <- tryCatch({
          if (nrow(coords) > num_clusters && num_clusters > 1) {
            clusters <- as.integer(cluster_data$cluster)
            # Intra-cluster distances (average distance to centroid)
            intra_dist <- sapply(unique(clusters), function(k) {
              cluster_points <- coords[clusters == k, , drop = FALSE]
              cluster_mean <- colMeans(cluster_points)
              mean(apply(cluster_points, 1, function(pt) sqrt(sum((pt - cluster_mean)^2))))
            })
            # Inter-cluster distances (distance between centroids)
            centroids <- km$centers
            db_vals <- sapply(1:num_clusters, function(i) {
              max_ratio <- 0
              for (j in 1:num_clusters) {
                if (i != j) {
                  inter_dist <- sqrt(sum((centroids[i, ] - centroids[j, ])^2))
                  ratio <- (intra_dist[i] + intra_dist[j]) / inter_dist
                  max_ratio <- max(max_ratio, ratio)
                }
              }
              max_ratio
            })
            mean(db_vals)  # Lower is better
          } else NA
        }, error = function(e) NA)

        # METHOD-SPECIFIC QUALITY METRICS
        cophenetic_corr <- NA
        dunn_coeff <- NA
        gmm_bic <- NA
        gmm_loglik <- NA

        if (method == "hierarchical") {
          # Cophenetic Correlation (quality of the hierarchy)
          cophenetic_corr <- tryCatch({
            cor(dist(coords), cophenetic(clustering_result$hc_tree))
          }, error = function(e) NA)
        }

        if (method == "fuzzy") {
          # Dunn's Partition Coefficient (sharpness of fuzzy assignment)
          dunn_coeff <- tryCatch({
            membership <- clustering_result$membership
            mean(apply(membership, 1, function(row) sum(row^2)))
          }, error = function(e) NA)
        }

        if (method == "gmm") {
          # BIC (Bayesian Information Criterion) - lower = better
          gmm_bic <- tryCatch({
            clustering_result$gmm_model$bic
          }, error = function(e) NA)

          # Log-Likelihood - higher = better
          gmm_loglik <- tryCatch({
            clustering_result$gmm_model$loglik
          }, error = function(e) NA)
        }

        tot_withinss <- sum(sapply(1:num_clusters, function(k) {
          cluster_points <- coords[clustering_result$cluster == k, , drop = FALSE]
          cluster_mean <- colMeans(cluster_points)
          sum(apply(cluster_points, 1, function(pt) sum((pt - cluster_mean)^2)))
        }))

        grand_mean <- colMeans(coords)
        betweenss <- sum(sapply(1:num_clusters, function(k) {
          cluster_points <- coords[clustering_result$cluster == k, , drop = FALSE]
          cluster_mean <- colMeans(cluster_points)
          nrow(cluster_points) * sum((cluster_mean - grand_mean)^2)
        }))

        # === CLUSTER-CHARAKTERISIERUNG: TYPEN-SPEKTRUM (DESKRIPTIV) ===
        incProgress(0.05, detail = "Typen-Spektrum berechnen...")
        cluster_characterization <- tryCatch({

          # Only when SITES are clustered (then we can sum type frequencies)
          if (cluster_on_val == "rows" || cluster_on_val == "both") {
            site_data <- subset(cluster_data, type == 'Site')

            if (nrow(site_data) > 0) {
              original_matrix <- NULL

              if (!is.null(filtered_data) && is.function(filtered_data)) {
                # filtered_data ist ein reactive - auswerten
                raw_data <- filtered_data()
                if (!is.null(raw_data) && (is.matrix(raw_data) || is.data.frame(raw_data))) {
                  original_matrix <- as.matrix(raw_data)
                }
              }

              # Fallback: CA result (only if no raw data available)
              if (is.null(original_matrix)) {
                res <- ca_result()
                if (!is.null(res$call$Xtot)) {
                  original_matrix <- as.matrix(res$call$Xtot)
                } else if (!is.null(res$call$X)) {
                  original_matrix <- as.matrix(res$call$X)
                }
              }

              if (!is.null(original_matrix)) {
                # Matrix: Cluster × Type (real frequency sums!)
                cluster_type_matrix <- matrix(0, nrow = num_clusters, ncol = ncol(original_matrix))
                rownames(cluster_type_matrix) <- 1:num_clusters
                colnames(cluster_type_matrix) <- colnames(original_matrix)

                # For each cluster: sum frequencies of all types over all sites
                for (k in 1:num_clusters) {
                  sites_in_cluster <- site_data$label[site_data$cluster == k]

                  if (length(sites_in_cluster) > 0) {
                    # Match sites in original matrix
                    site_indices <- match(sites_in_cluster, rownames(original_matrix))
                    site_indices <- site_indices[!is.na(site_indices)]

                    if (length(site_indices) > 0) {
                      # Sum over all sites in this cluster
                      cluster_type_matrix[k, ] <- colSums(original_matrix[site_indices, , drop = FALSE])
                    }
                  }
                }

                # Return only the cluster_type_matrix (descriptive analysis)
                list(
                  cluster_type_matrix = cluster_type_matrix,
                  num_clusters = num_clusters
                )
              } else {
                message("Original matrix not available in CA result")
                NULL
              }
            } else NULL
          } else NULL
        }, error = function(e) {
          message("Cluster-Charakterisierung fehlgeschlagen: ", e$message)
          NULL
        })

        result <- list(
          data = all_cluster_data,
          x_col = x_idx,
          y_col = y_idx,
          kmeans_obj = km,
          silhouette = silhouette_score,
          calinski_harabasz = calinski_harabasz,
          davies_bouldin = davies_bouldin,
          cophenetic_corr = cophenetic_corr,
          dunn_coeff = dunn_coeff,
          gmm_bic = gmm_bic,
          gmm_loglik = gmm_loglik,
          centers = km$centers,
          tot_withinss = tot_withinss,
          betweenss = betweenss,
          coords = coords,
          active_data = cluster_data,
          supplementary_data = suppl_filtered,
          n_dims = n_dims,  # For export info
          method = method,  # Store used method
          clustering_result = clustering_result,  # Complete result (incl. Membership, hc_tree, etc.)
          characterization = cluster_characterization  # Leittypen-Analyse
        )
        
        cache$kmeans_result <- result
        
        active_count <- sum(all_cluster_data$element_type == 'Active')
        suppl_count  <- sum(all_cluster_data$element_type == 'Supplementary')
        
        showNotification(
          sprintf(tr("kmeans.notify.success"), method_label, active_count, suppl_count),
          type = "message", duration = 4)
        result

      }, error = function(e) {
        showNotification(paste(tr("kmeans.notify.error"), e$message), type = "error", duration = 10)
        return(NULL)
      })
    })
  })
  
  # Cluster quality
  output$cluster_quality <- renderText({
    req(kmeans_res())
    km_data <- kmeans_res()
    wss <- km_data$tot_withinss
    bss <- km_data$betweenss
    total_ss <- wss + bss
    explained_var <- round(bss / total_ss * 100, 1)
    active_count <- sum(km_data$data$element_type == 'Active')
    suppl_count  <- sum(km_data$data$element_type == 'Supplementary')
    
    # Scientifically neutral assessment (no "truth" claims)
    # === FIX 2: Adjusted silhouette thresholds for archaeology ===
    # Archaeological data is more heterogeneous → lower thresholds
    silhouette_text <- if (!is.na(km_data$silhouette)) {
      sil_level <- if (km_data$silhouette > 0.6) tr("kmeans.quality.silhouette.high") else
        if (km_data$silhouette > 0.4) tr("kmeans.quality.silhouette.moderate") else
          if (km_data$silhouette > 0.2) tr("kmeans.quality.silhouette.low") else tr("kmeans.quality.silhouette.verylow")
      sprintf(tr("kmeans.quality.silhouette.label"), km_data$silhouette, sil_level)
    } else tr("kmeans.quality.silhouette.na")

    # NEUE METRIKEN (Phase 2)
    calinski_text <- if (!is.null(km_data$calinski_harabasz) && !is.na(km_data$calinski_harabasz)) {
      ch <- km_data$calinski_harabasz
      ch_level <- if (ch > 100) tr("kmeans.quality.silhouette.high") else
        if (ch > 50) tr("kmeans.quality.silhouette.moderate") else tr("kmeans.quality.silhouette.low")
      sprintf(tr("kmeans.quality.calinski.label"), ch, ch_level)
    } else tr("kmeans.quality.calinski.na")

    davies_text <- if (!is.null(km_data$davies_bouldin) && !is.na(km_data$davies_bouldin)) {
      db <- km_data$davies_bouldin
      db_level <- if (db < 0.5) tr("kmeans.quality.silhouette.high") else
        if (db < 1.0) tr("kmeans.quality.silhouette.moderate") else tr("kmeans.quality.silhouette.low")
      sprintf(tr("kmeans.quality.davies.label"), db, db_level)
    } else tr("kmeans.quality.davies.na")

    # Explained variance as descriptive measure (not a quality judgment)
    var_level <- if (explained_var > 70) tr("kmeans.quality.silhouette.high") else
      if (explained_var > 50) tr("kmeans.quality.silhouette.moderate") else
        if (explained_var > 30) tr("kmeans.quality.silhouette.low") else tr("kmeans.quality.silhouette.verylow")

    # Dimensionen + Methoden-Information
    n_dims <- km_data$n_dims %||% 2
    method <- km_data$method %||% "kmeans"
    method_label <- switch(method,
                          "kmeans" = tr("kmeans.method.kmeans"),
                          "hierarchical" = tr("kmeans.method.hierarchical"),
                          "pam" = tr("kmeans.method.pam"),
                          "fuzzy" = tr("kmeans.method.fuzzy"),
                          method)
    dims_info <- sprintf(tr("kmeans.quality.dims"), n_dims, method_label)
    seed_info <- if (method == "kmeans") paste0("\n", tr("kmeans.quality.seed")) else ""

    # Methoden-spezifische Metriken
    method_specific <- ""
    if (method == "hierarchical" && !is.na(km_data$cophenetic_corr)) {
      coph_level <- if (km_data$cophenetic_corr > 0.8) tr("kmeans.quality.silhouette.high") else
        if (km_data$cophenetic_corr > 0.6) tr("kmeans.quality.silhouette.moderate") else tr("kmeans.quality.silhouette.low")
      method_specific <- sprintf(paste0("\n", tr("kmeans.quality.cophenetic")),
                                 km_data$cophenetic_corr, coph_level)
    } else if (method == "fuzzy" && !is.na(km_data$dunn_coeff)) {
      dunn_level <- if (km_data$dunn_coeff > 0.7) tr("kmeans.quality.sharp") else
        if (km_data$dunn_coeff > 0.5) tr("kmeans.quality.silhouette.moderate") else tr("kmeans.quality.fuzzy")
      method_specific <- sprintf(paste0("\n", tr("kmeans.quality.dunn")),
                                 km_data$dunn_coeff, dunn_level)
      method_specific <- paste0(method_specific, sprintf(paste0("\n", tr("kmeans.quality.fuzziness")), input$fuzzy_m %||% 2))
    } else if (method == "gmm") {
      gmm_info <- ""
      if (!is.na(km_data$gmm_bic)) {
        gmm_info <- sprintf(paste0("\n", tr("kmeans.quality.bic")), km_data$gmm_bic)
      }
      if (!is.na(km_data$gmm_loglik)) {
        gmm_info <- paste0(gmm_info, sprintf(paste0("\n", tr("kmeans.quality.loglik")), km_data$gmm_loglik))
      }
      gmm_model <- input$gmm_model_type %||% "auto"
      gmm_info <- paste0(gmm_info, sprintf(paste0("\n", tr("kmeans.quality.covariance")), gmm_model))
      method_specific <- gmm_info
    } else if (method == "hierarchical") {
      method_specific <- sprintf(paste0("\n", tr("kmeans.quality.linkage")), input$hclust_method %||% "average")
    } else if (method == "pam") {
      method_specific <- sprintf(paste0("\n", tr("kmeans.quality.pam.medoids")), input$num_clusters %||% 3)
    }

    paste0(
      sprintf(tr("kmeans.quality.title"), method_label), "\n\n",
      silhouette_text, "\n",
      calinski_text, "\n",
      davies_text, method_specific, "\n",
      sprintf(tr("kmeans.quality.variance.label"), explained_var, var_level), "\n",
      sprintf(tr("kmeans.quality.wss"), wss), "\n",
      sprintf(tr("kmeans.quality.bss"), bss), "\n\n",
      tr("kmeans.quality.datapoints"), "\n",
      sprintf(tr("kmeans.quality.active"), active_count), " | ", sprintf(tr("kmeans.quality.suppl"), suppl_count), "\n",
      dims_info, seed_info, "\n\n",
      tr("kmeans.quality.note"), "\n\n",
      tr("kmeans.quality.complete")
    )
  })
  
  # Plotly plot with ellipses and legend
  output$kmeans_plotly <- renderPlotly({
    req(kmeans_res())
    
    km_data   <- kmeans_res()
    plot_data <- km_data$data
    plot_data$group <- NA_character_
    is_site <- plot_data$type == 'Site'
    if (any(is_site)) plot_data$group[is_site] <- get_site_group(plot_data$label[is_site])
    
    x_values <- plot_data[[paste0("Dim.", km_data$x_col)]]
    y_values <- plot_data[[paste0("Dim.", km_data$y_col)]]
    
    # === HOVER TEXT WITH HELPER FUNCTION ===
    plot_data$x <- x_values
    plot_data$y <- y_values
    hover_text <- generate_kmeans_hover_text(
      data = plot_data,
      x_dim = input$x_dim %||% "Dim1",
      y_dim = input$y_dim %||% "Dim2", 
      get_cluster_name_func = get_cluster_name,
      get_element_details = get_element_details
    )
    
    # === STANDARDISIERTE CLUSTER-FARBEN ===
    num_clusters <- input$num_clusters %||% 3
    colors <- seri_arc_colors()
    cluster_colors <- colors$clusters[1:num_clusters]
    names(cluster_colors) <- as.character(seq_len(num_clusters))
    
    active_data <- plot_data[plot_data$element_type == 'Active', ]
    suppl_data  <- plot_data[plot_data$element_type == 'Supplementary', ]
    
    # Sorted cluster order for legend & rendering (numeric 1..k suffices, since renumbered)
    ordered_clusters_active <- sort(unique(as.integer(active_data$cluster)))
    ordered_clusters_suppl  <- sort(unique(as.integer(suppl_data$cluster)))
    
    p <- plot_ly() %>%
      layout(
        xaxis = list(
          title = sprintf("%s (%.1f%%)", input$x_dim %||% "Dim1", 
                          if(!is.null(ca_result())) ca_result()$eig[km_data$x_col, 2] else 0),
          zeroline = TRUE, zerolinewidth = 2, zerolinecolor = "#95a5a6",
          gridcolor = "#ecf0f1"
        ),
        yaxis = list(
          title = sprintf("%s (%.1f%%)", input$y_dim %||% "Dim2", 
                          if(!is.null(ca_result())) ca_result()$eig[km_data$y_col, 2] else 0),
          zeroline = TRUE, zerolinewidth = 2, zerolinecolor = "#95a5a6",
          gridcolor = "#ecf0f1"
        ),
        title = if (!is.null(get_element_details)) {
          tryCatch({
            # Zugriff auf Parameter-Status aus Parent-Environment
            if (exists("filtermod", envir = parent.frame())) {
              filtermod_ref <- get("filtermod", envir = parent.frame())
              if (!is.null(filtermod_ref$parameter_status)) {
                status <- filtermod_ref$parameter_status()
                tr("kmeans.plot.title")
              } else {
                tr("kmeans.plot.title")
              }
            } else {
              tr("kmeans.plot.title")
            }
          }, error = function(e) {
            tr("kmeans.plot.title")
          })
        } else {
          tr("kmeans.plot.title")
        },
        hovermode = 'closest',
        plot_bgcolor = "white",
        paper_bgcolor = "white",
        legend = list(traceorder = "normal")
      )
    
    # --- CONFIDENCE ELLIPSES WITH HELPER FUNCTION ---
    if (!is.null(input$show_confidence_ellipses) && input$show_confidence_ellipses && nrow(active_data) > 0) {
      
      for (cluster_id in ordered_clusters_active) {
        cluster_subset <- active_data[as.integer(active_data$cluster) == cluster_id, ]
        x_coords <- cluster_subset[[paste0("Dim.", km_data$x_col)]]
        y_coords <- cluster_subset[[paste0("Dim.", km_data$y_col)]]
        cname <- as.character(cluster_id)
        
        if (nrow(cluster_subset) >= 3) {
          ellipse_coords <- calculate_confidence_ellipse(x_coords, y_coords, conf_level = 0.95)
          if (!is.null(ellipse_coords)) {
            p <- p %>% add_polygons(
              x = ellipse_coords$x, y = ellipse_coords$y,
              fillcolor = cluster_colors[cname],
              opacity = 0.3, line = list(width = 0),
              name = sprintf(tr("kmeans.ellipse.confidence"), get_cluster_name(cluster_id)),
              legendgroup = sprintf("cluster_%s", cname),
              legendrank = cluster_id,
              showlegend = TRUE, hoverinfo = "skip"
            )
          }
        } else if (nrow(cluster_subset) == 2) {
          dist <- sqrt((x_coords[2] - x_coords[1])^2 + (y_coords[2] - y_coords[1])^2)
          dx <- x_coords[2] - x_coords[1]; dy <- y_coords[2] - y_coords[1]
          perp_x <- if (dist > 0) -dy / dist else 0
          perp_y <- if (dist > 0)  dx / dist else 0
          buffer_width <- max(0.1, dist * 0.3); extension <- dist * 0.2
          rect_coords <- data.frame(
            x = c(
              x_coords[1] - dx/dist * extension - perp_x * buffer_width,
              x_coords[1] - dx/dist * extension + perp_x * buffer_width,
              x_coords[2] + dx/dist * extension + perp_x * buffer_width,
              x_coords[2] + dx/dist * extension - perp_x * buffer_width,
              x_coords[1] - dx/dist * extension - perp_x * buffer_width
            ),
            y = c(
              y_coords[1] - dy/dist * extension - perp_y * buffer_width,
              y_coords[1] - dy/dist * extension + perp_y * buffer_width,
              y_coords[2] + dy/dist * extension + perp_y * buffer_width,
              y_coords[2] + dy/dist * extension - perp_y * buffer_width,
              y_coords[1] - dy/dist * extension - perp_y * buffer_width
            )
          )
          p <- p %>% add_polygons(
            x = rect_coords$x, y = rect_coords$y,
            fillcolor = cluster_colors[cname],
            opacity = 0.2, line = list(width = 0),
            name = sprintf("Verbindung %s (2 Punkte)", get_cluster_name(cluster_id)),
            legendgroup = sprintf("cluster_%s", cname),
            legendrank = cluster_id,
            showlegend = TRUE, hoverinfo = "skip"
          )
        } else if (nrow(cluster_subset) == 1) {
          radius <- 0.2; ang <- seq(0, 2*pi, length.out = 50)
          circle_x <- x_coords[1] + radius * cos(ang)
          circle_y <- y_coords[1] + radius * sin(ang)
          p <- p %>% add_polygons(
            x = circle_x, y = circle_y,
            fillcolor = cluster_colors[cname],
            opacity = 0.15, line = list(width = 0),
            name = sprintf("Bereich %s (1 Punkt)", get_cluster_name(cluster_id)),
            legendgroup = sprintf("cluster_%s", cname),
            legendrank = cluster_id,
            showlegend = TRUE, hoverinfo = "skip"
          )
        }
      }
    }
    
    # --- Marker danach (in sortierter Reihenfolge) ---
    if (nrow(active_data) > 0) {
      for (cluster_id in ordered_clusters_active) {
        cluster_subset <- active_data[as.integer(active_data$cluster) == cluster_id, ]
        cname <- as.character(cluster_id)
        # Sites
        site_subset <- cluster_subset[cluster_subset$type == 'Site', ]
        if (nrow(site_subset) > 0) {
          p <- p %>% add_markers(
            x = site_subset[[paste0("Dim.", km_data$x_col)]],
            y = site_subset[[paste0("Dim.", km_data$y_col)]],
            marker = list(symbol = "circle",
                          size = (input$point_size_km %||% 3) * 4,
                          color = cluster_colors[cname],
                          opacity = 0.9, line = list(width = 1, color = "white")),
            text = if(!is.null(input$show_labels_km) && input$show_labels_km) site_subset$lab else "",
            textposition = "middle right", textfont = list(size = 10, color = "#2c3e50"),
            hovertext = hover_text[match(site_subset$label, plot_data$label)],
            hoverinfo = 'text',
            name = get_cluster_name(cluster_id),
            legendgroup = sprintf("cluster_%s", cname),
            legendrank = cluster_id,
            showlegend = TRUE
          )
        }
        # Types
        type_subset <- cluster_subset[cluster_subset$type == 'Type', ]
        if (nrow(type_subset) > 0) {
          p <- p %>% add_markers(
            x = type_subset[[paste0("Dim.", km_data$x_col)]],
            y = type_subset[[paste0("Dim.", km_data$y_col)]],
            marker = list(symbol = "triangle-up",
                          size = (input$point_size_km %||% 3) * 4,
                          color = cluster_colors[cname],
                          opacity = 0.9, line = list(width = 1, color = "white")),
            text = if(!is.null(input$show_labels_km) && input$show_labels_km) type_subset$lab else "",
            textposition = "middle right", textfont = list(size = 10, color = "#2c3e50"),
            hovertext = hover_text[match(type_subset$label, plot_data$label)],
            hoverinfo = 'text',
            name = get_cluster_name(cluster_id),
            legendgroup = sprintf("cluster_%s", cname),
            legendrank = cluster_id,
            showlegend = sum(active_data$type == 'Site') == 0  # Zeige Legende nur wenn keine Sites in aktiven Daten
          )
        }
      }
    }
    
    # --- Supplementary Punkte (geordnet) ---
    if (nrow(suppl_data) > 0) {
      for (cluster_id in ordered_clusters_suppl) {
        cluster_subset <- suppl_data[as.integer(suppl_data$cluster) == cluster_id, ]
        cname <- as.character(cluster_id)
        # Sites (Suppl.)
        site_subset <- cluster_subset[cluster_subset$type == 'Site', ]
        if (nrow(site_subset) > 0) {
          p <- p %>% add_markers(
            x = site_subset[[paste0("Dim.", km_data$x_col)]],
            y = site_subset[[paste0("Dim.", km_data$y_col)]],
            marker = list(symbol = "circle-open",
                          size = (input$point_size_km %||% 3) * 4,
                          color = cluster_colors[cname],
                          opacity = 0.6, line = list(width = 2, color = cluster_colors[cname])),
            text = if(!is.null(input$show_labels_km) && input$show_labels_km) site_subset$lab else "",
            textposition = "middle right", textfont = list(size = 10, color = "#2c3e50"),
            hovertext = hover_text[match(site_subset$label, plot_data$label)],
            hoverinfo = 'text',
            name = get_cluster_name(cluster_id),
            legendgroup = sprintf("cluster_%s", cname),
            legendrank = cluster_id + 0.1,
            showlegend = TRUE
          )
        }
        # Types (Suppl.)
        type_subset <- cluster_subset[cluster_subset$type == 'Type', ]
        if (nrow(type_subset) > 0) {
          p <- p %>% add_markers(
            x = type_subset[[paste0("Dim.", km_data$x_col)]],
            y = type_subset[[paste0("Dim.", km_data$y_col)]],
            marker = list(symbol = "triangle-up-open",
                          size = (input$point_size_km %||% 3) * 4,
                          color = cluster_colors[cname],
                          opacity = 0.6, line = list(width = 2, color = cluster_colors[cname])),
            text = if(!is.null(input$show_labels_km) && input$show_labels_km) type_subset$lab else "",
            textposition = "middle right", textfont = list(size = 10, color = "#2c3e50"),
            hovertext = hover_text[match(type_subset$label, plot_data$label)],
            hoverinfo = 'text',
            name = get_cluster_name(cluster_id),
            legendgroup = sprintf("cluster_%s", cname),
            legendrank = cluster_id + 0.2,
            showlegend = FALSE
          )
        }
      }
    }
    
    # Zentren
    if (!is.null(input$show_cluster_centers) && input$show_cluster_centers && 
        !is.null(km_data$centers) && nrow(km_data$centers) > 0) {
      
      # CRITICAL: Map centers to plot dimensions (not hardcoded [,1:2])
      n_dims <- km_data$n_dims %||% 2
      if (n_dims == 2) {
        # 2D: centers[,1:2] entsprechen x_dim, y_dim
        center_x <- km_data$centers[, 1]
        center_y <- km_data$centers[, 2]
      } else {
        # nD: centers must be projected onto x_col, y_col
        x_col <- km_data$x_col
        y_col <- km_data$y_col
        center_x <- km_data$centers[, x_col]
        center_y <- km_data$centers[, y_col]
      }
      
      centers_hover <- sprintf("Cluster %d Center<br>(based on active points)<br><i>SeriARC Analysis</i>", 1:nrow(km_data$centers))
      p <- p %>% add_markers(
        x = center_x, y = center_y,
        marker = list(symbol = "x", size = 20, color = "#2c3e50", line = list(width = 3)),
        name = "Centers (active)", showlegend = TRUE,
        hovertext = centers_hover, hoverinfo = 'text', inherit = FALSE,
        legendrank = 0
      )
    }

    p <- standard_plotly_config(p, "2d")
    last_kmeans_plotly(p)
    p
  })

  # Tabelle
  output$kmeans_table <- DT::renderDataTable({
    req(kmeans_res())
    km_data <- kmeans_res()
    result_table <- km_data$data[, c("label", "type", "element_type", "cluster")]
    
    # APPLY FILTER when Sites+Types are clustered together
    cluster_on <- input$cluster_on %||% "both"
    if (cluster_on == "both") {
      filter_choice <- input$cluster_table_filter %||% "both"
      if (filter_choice == "Site") {
        result_table <- result_table[result_table$type == "Site", ]
      } else if (filter_choice == "Type") {
        result_table <- result_table[result_table$type == "Type", ]
      }
      # "both" = keine Filterung
    }
    
    result_table$cluster_name <- sapply(result_table$cluster, get_cluster_name)
    colnames(result_table) <- c("Entity", "Element_Typ", "Status", "Cluster_Nr", "Cluster_Name")
    cluster_sizes <- table(result_table$Cluster_Nr)
    result_table$Cluster_Groesse <- as.numeric(cluster_sizes[result_table$Cluster_Nr])
    result_table <- result_table[order(result_table$Cluster_Nr, result_table$Status), ]
    
    unique_clusters <- unique(result_table$Cluster_Nr)
    num_clusters <- length(unique_clusters)
    base_colors <- c("#e3f2fd", "#ffebee", "#e8f5e8", "#fff3e0", "#f3e5f5", "#e0f2f1", 
                     "#fce4ec", "#e8eaf6", "#fff8e1", "#efebe9")
    cluster_colors <- rep(base_colors, length.out = num_clusters)
    names(cluster_colors) <- as.character(unique_clusters)
    
    dt <- DT::datatable(
      result_table,
      options = list(
        paging = FALSE,
        scrollX = TRUE,
        scrollY = "400px",
        scrollCollapse = TRUE,
        columnDefs = list(list(className = 'dt-center', targets = "_all"))
      ),
      rownames = FALSE, 
      class = 'cell-border stripe hover',
      caption = tr("kmeans.table.caption")
    )
    if (length(cluster_colors) == length(unique_clusters)) {
      dt <- dt %>% formatStyle(
        'Cluster_Nr',
        backgroundColor = styleEqual(unique_clusters, cluster_colors[as.character(unique_clusters)])
      )
    }
    dt <- dt %>% formatStyle(
      'Status',
      backgroundColor = styleEqual(c('Active', 'Supplementary'), c('#ffffff', '#f8f9fa')),
      fontWeight = styleEqual('Supplementary', 'italic')
    )
    dt
  })

  # Availability of characterization (for conditionalPanel)
  output$characterization_available <- reactive({
    km_data <- kmeans_res()
    # Show panel when Sites are clustered (both or rows)
    cluster_on <- input$cluster_on %||% "both"
    !is.null(km_data) && (cluster_on == "both" || cluster_on == "rows")
  })
  outputOptions(output, "characterization_available", suspendWhenHidden = FALSE)

  # Cluster-Charakterisierung UI
  output$cluster_characterization_ui <- renderUI({
    req(kmeans_res())
    km_data <- kmeans_res()
    characterization <- km_data$characterization
    cluster_on <- input$cluster_on %||% "both"
    num_clusters <- input$num_clusters %||% 3

    # === MODUS 1: Sites + Types gemeinsam geclustert ===
    if (cluster_on == "both") {
      # Typen direkt aus Cluster-Zuordnung extrahieren
      cluster_data <- km_data$data
      if (is.null(cluster_data)) {
        return(tags$p(class = "text-muted", tr("cluster.no.data")))
      }

      # Types im Cluster (direkt aus CA-Koordinaten geclustert)
      type_data <- subset(cluster_data, type == 'Type' & element_type == 'Active')

      # Get frequencies from original matrix
      original_matrix <- NULL
      if (!is.null(filtered_data) && is.function(filtered_data)) {
        raw_data <- filtered_data()
        if (!is.null(raw_data) && (is.matrix(raw_data) || is.data.frame(raw_data))) {
          original_matrix <- as.matrix(raw_data)
        }
      }
      # Fallback
      if (is.null(original_matrix)) {
        res <- ca_result()
        if (!is.null(res$call$Xtot)) {
          original_matrix <- as.matrix(res$call$Xtot)
        } else if (!is.null(res$call$X)) {
          original_matrix <- as.matrix(res$call$X)
        }
      }

      type_totals <- if (!is.null(original_matrix)) colSums(original_matrix) else NULL

      info_box <- div(class = "alert alert-success", style = "margin-bottom: 15px;",
        tags$strong(HTML(tr("kmeans.char.mode.direct"))),
        tags$span(style = "font-size: 0.85em; color: #3c763d; margin-left: 10px;", tr("kmeans.char.mode.direct.active")),
        tags$br(),
        tags$span(tr("kmeans.char.mode.direct.desc")),
        tags$br(),
        tags$span(style = "font-size: 0.9em;",
          HTML(paste0("<strong>", tr("kmeans.char.mode.direct.bold"), "</strong> ", tr("kmeans.char.mode.direct.top5")))
        ),
        tags$hr(style = "margin: 8px 0;"),
        tags$span(style = "font-size: 0.85em; color: #666; font-style: italic;",
          tr("kmeans.char.mode.direct.hint")
        )
      )

      cluster_panels <- lapply(1:num_clusters, function(k) {
        types_in_cluster <- type_data$label[type_data$cluster == k]

        if (length(types_in_cluster) == 0) {
          content <- tags$p(class = "text-muted", tr("kmeans.char.no.types"))
        } else {
          # Sort by frequency (if available)
          if (!is.null(type_totals)) {
            # Match type names with column names
            matched_totals <- type_totals[match(types_in_cluster, names(type_totals))]
            matched_totals[is.na(matched_totals)] <- 0
            sort_order <- order(matched_totals, decreasing = TRUE)
            types_sorted <- types_in_cluster[sort_order]
            counts_sorted <- matched_totals[sort_order]

            type_list <- lapply(seq_along(types_sorted), function(i) {
              type_name <- types_sorted[i]
              count <- counts_sorted[i]
              is_top5 <- i <= 5
              text <- sprintf("%s (n=%d)", type_name, as.integer(count))

              if (is_top5) {
                tags$li(tags$strong(text))
              } else {
                tags$li(text)
              }
            })

            total_count <- sum(counts_sorted)
          } else {
            # Fallback: alphabetical without frequencies
            types_sorted <- sort(types_in_cluster)
            type_list <- lapply(types_sorted, function(t) tags$li(t))
            total_count <- NA
          }

          content <- tagList(
            tags$ul(style = "max-height: 300px; overflow-y: auto; padding-left: 20px;", type_list),
            tags$small(class = "text-muted",
              if (!is.na(total_count)) sprintf(tr("kmeans.char.types.count"), length(types_sorted), as.integer(total_count))
              else sprintf(tr("kmeans.char.types.count.simple"), length(types_sorted))
            )
          )
        }

        div(class = "well well-sm",
            tags$strong(get_cluster_name(k)),
            content
        )
      })

      return(tagList(info_box, do.call(tagList, cluster_panels)))
    }

    # === MODE 2: Only sites clustered - frequency-based characterization ===
    if (is.null(characterization) || is.null(characterization$cluster_type_matrix)) {
      return(tags$p(class = "text-muted", tr("cluster.types.analysis.unavailable")))
    }

    ctm <- characterization$cluster_type_matrix

    # CRITICAL: Check if characterization matches current cluster count
    if (nrow(ctm) != num_clusters) {
      return(
        div(class = "alert alert-warning",
          tags$strong(tr("kmeans.char.warning")),
          sprintf(tr("kmeans.char.mismatch"), nrow(ctm), num_clusters)
        )
      )
    }

    # === CALCULATE EXCLUSIVE TYPES (appearing in only one cluster) ===
    # For each type: in how many clusters does it appear?
    type_cluster_occurrence <- colSums(ctm > 0)
    exclusive_types <- names(type_cluster_occurrence)[type_cluster_occurrence == 1]

    # === INFO-BOX ===
    info_box <- div(class = "alert alert-info", style = "margin-bottom: 15px;",
      tags$strong(HTML(tr("kmeans.char.mode.spectrum"))),
      tags$span(style = "font-size: 0.85em; color: #31708f; margin-left: 10px;", tr("kmeans.char.mode.spectrum.active")),
      tags$br(),
      tags$span(tr("kmeans.char.mode.spectrum.desc")),
      tags$br(),
      tags$span(style = "font-size: 0.9em;",
        HTML(paste0("<strong>", tr("kmeans.char.mode.direct.bold"), "</strong> ", tr("kmeans.char.mode.spectrum.legend")))
      ),
      tags$hr(style = "margin: 8px 0;"),
      tags$span(style = "font-size: 0.85em; color: #666; font-style: italic;",
        tr("kmeans.char.mode.spectrum.hint")
      )
    )

    cluster_panels <- lapply(1:num_clusters, function(k) {
      cluster_counts <- ctm[k, ]
      total_in_cluster <- sum(cluster_counts)

      if (total_in_cluster == 0) {
        content <- tags$p(class = "text-muted", tr("kmeans.char.no.finds"))
      } else {
        # Sort by frequency (only types with count > 0)
        types_present <- cluster_counts[cluster_counts > 0]
        sorted_idx <- order(types_present, decreasing = TRUE)
        sorted_types <- names(types_present)[sorted_idx]
        sorted_counts <- types_present[sorted_idx]

        if (length(sorted_types) == 0) {
          content <- tags$p(class = "text-muted", tr("kmeans.char.no.types.freq"))
        } else {
          type_list <- lapply(seq_along(sorted_types), function(i) {
            type_name <- sorted_types[i]
            count <- sorted_counts[i]
            percent <- 100 * count / total_in_cluster

            # Formatierung: Top 5 fett, exklusiv rot
            is_top5 <- i <= 5
            is_exclusive <- type_name %in% exclusive_types

            text <- sprintf("%s (n=%d, %.1f%%)", type_name, as.integer(count), percent)

            if (is_top5 && is_exclusive) {
              tags$li(tags$strong(style = "color: #c0392b;", text))
            } else if (is_top5) {
              tags$li(tags$strong(text))
            } else if (is_exclusive) {
              tags$li(tags$span(style = "color: #c0392b;", text))
            } else {
              tags$li(text)
            }
          })

          content <- tagList(
            tags$ul(style = "max-height: 300px; overflow-y: auto; padding-left: 20px;", type_list),
            tags$small(class = "text-muted",
              sprintf(tr("kmeans.char.total"), as.integer(total_in_cluster), length(sorted_types)))
          )
        }
      }

      div(class = "well well-sm",
          tags$strong(get_cluster_name(k)),
          content
      )
    })

    # Combine info box and cluster panels
    tagList(
      info_box,
      do.call(tagList, cluster_panels)
    )
  })

  # === EXPORT FUNCTIONS WITH HELPER TEMPLATES (PNG WITH REAL DATA) ===
  output$download_kmeans_plot_png <- create_png_download_handler(
    plot_output_id = "kmeans_plotly",
    base_filename = paste0("SeriARC_KMeans_Plot_", input$num_clusters %||% 3, "_Cluster"),
    session = session,
    tr = tr,
    plot_data = reactive({ 
      req(kmeans_res())
      km_data <- kmeans_res()
      plot_data <- km_data$data
      plot_data$x <- plot_data[[paste0("Dim.", km_data$x_col)]]
      plot_data$y <- plot_data[[paste0("Dim.", km_data$y_col)]]
      plot_data$cluster_numeric <- as.numeric(plot_data$cluster)
      plot_data
    }),
    plot_generator_func = function(plot_data) {
      num_clusters <- input$num_clusters %||% 3
      colors <- seri_arc_colors()
      cluster_colors <- colors$clusters[1:num_clusters]
      
      # Echte K-Means PNG-Generation
      par(mar = c(5, 4, 4, 2))
      point_colors <- cluster_colors[plot_data$cluster_numeric]
      
      plot(plot_data$x, plot_data$y, 
           col = point_colors,
           pch = ifelse(plot_data$type == 'Site', 16, 17),
           cex = ifelse(plot_data$element_type == "Active", 1.2, 0.8),
           xlab = sprintf("%s (K-Means)", input$x_dim %||% "Dim1"),
           ylab = sprintf("%s (K-Means)", input$y_dim %||% "Dim2"),
           main = sprintf("SeriARC K-Means Clustering (%d Cluster)", num_clusters))
      
      # Add cluster centers (CORRECTLY mapped to plot dimensions)
      if (!is.null(kmeans_res()$centers)) {
        km_result <- kmeans_res()
        n_dims <- km_result$n_dims %||% 2
        if (n_dims == 2) {
          center_x <- km_result$centers[, 1]
          center_y <- km_result$centers[, 2]
        } else {
          x_col <- km_result$x_col
          y_col <- km_result$y_col
          center_x <- km_result$centers[, x_col]
          center_y <- km_result$centers[, y_col]
        }
        points(center_x, center_y, 
               pch = 4, cex = 2.5, lwd = 3, col = "black")
      }
      
      # Aktive vs. Supplementary kennzeichnen
      if (any(plot_data$element_type == "Supplementary")) {
        # Overlay supplementary as open symbols
        suppl_data <- plot_data[plot_data$element_type == "Supplementary", ]
        if (nrow(suppl_data) > 0) {
          points(suppl_data$x, suppl_data$y,
                 col = cluster_colors[suppl_data$cluster_numeric],
                 pch = ifelse(suppl_data$type == 'Site', 1, 2), # Offene Symbole
                 cex = 1.0, lwd = 2)
        }
      }
      
      # Legende
      legend("topright", 
             legend = c(paste("Cluster", 1:num_clusters), "Centers", "Active", "Supplementary"),
             col = c(cluster_colors[1:num_clusters], "black", "black", "black"),
             pch = c(rep(16, num_clusters), 4, 16, 1), 
             cex = 0.7, ncol = if(num_clusters > 4) 2 else 1)
      
      # Grid for better orientation
      grid(col = "lightgray", lty = "dotted")

      # Info text with parameters (reproducibility)
      n_dims <- input$kmeans_n_dims %||% 2

      mtext(sprintf("PNG | %s vs %s | %dD | %s | k=%d | Seed=123 | %d Punkte (%d aktiv) | %s",
                    input$x_dim %||% "Dim1", input$y_dim %||% "Dim2", n_dims,
                    input$cluster_on %||% "both", num_clusters,
                    nrow(plot_data), sum(plot_data$element_type == "Active"),
                    format(Sys.time(), "%Y-%m-%d %H:%M")),
            side = 1, line = 4, cex = 0.55, col = "gray")
    }
  )
  
  output$download_kmeans_plot_svg <- create_svg_download_handler(
    plot_output_id = "kmeans_plotly",
    base_filename = paste0("SeriARC_KMeans_Plot_", input$num_clusters %||% 3, "_Cluster"),
    session = session,
    tr = tr,
    plot_data = reactive({ 
      req(kmeans_res())
      km_data <- kmeans_res()
      plot_data <- km_data$data
      plot_data$x <- plot_data[[paste0("Dim.", km_data$x_col)]]
      plot_data$y <- plot_data[[paste0("Dim.", km_data$y_col)]]
      plot_data$cluster_numeric <- as.numeric(plot_data$cluster)
      plot_data
    }),
    plot_generator_func = function(plot_data) {
      num_clusters <- input$num_clusters %||% 3
      colors <- seri_arc_colors()
      cluster_colors <- colors$clusters[1:num_clusters]
      
      # Wissenschaftliche K-Means SVG-Generation
      par(mar = c(5, 4, 4, 2))
      point_colors <- cluster_colors[plot_data$cluster_numeric]
      plot(plot_data$x, plot_data$y, 
           col = point_colors,
           pch = ifelse(plot_data$type == 'Site', 16, 17),
           cex = ifelse(plot_data$element_type == "Active", 1.2, 0.8),
           xlab = sprintf("%s (K-Means)", input$x_dim %||% "Dim1"),
           ylab = sprintf("%s (K-Means)", input$y_dim %||% "Dim2"),
           main = sprintf("SeriARC K-Means Clustering (%d Cluster) - SVG", num_clusters))
      
      # Cluster-Zentren (KORREKT auf Plot-Dimensionen abgebildet)
      if (!is.null(kmeans_res()$centers)) {
        km_result <- kmeans_res()
        n_dims <- km_result$n_dims %||% 2
        if (n_dims == 2) {
          center_x <- km_result$centers[, 1]
          center_y <- km_result$centers[, 2]
        } else {
          x_col <- km_result$x_col
          y_col <- km_result$y_col
          center_x <- km_result$centers[, x_col]
          center_y <- km_result$centers[, y_col]
        }
        points(center_x, center_y, 
               pch = 4, cex = 2, lwd = 3, col = "black")
      }
      
      # Legende
      legend("topright", 
             legend = c(paste("Cluster", 1:num_clusters), "Zentren"),
             col = c(cluster_colors[1:num_clusters], "black"),
             pch = c(rep(16, num_clusters), 4), cex = 0.8)
      
      # Grid
      grid(col = "lightgray", lty = "dotted")

      # Info with parameters
      n_dims <- input$kmeans_n_dims %||% 2

      mtext(sprintf("SVG | %s vs %s | %dD | %s | k=%d | Seed=123 | n=%d",
                    input$x_dim %||% "Dim1", input$y_dim %||% "Dim2", n_dims,
                    input$cluster_on %||% "both", num_clusters,
                    nrow(plot_data)),
            side = 1, line = 4, cex = 0.65, col = "gray")
    }
  )
  
  output$download_kmeans_plot_pdf <- create_pdf_download_handler(
    plot_data = reactive({
      req(kmeans_res())
      km_data <- kmeans_res()
      plot_data <- km_data$data
      plot_data$x <- plot_data[[paste0("Dim.", km_data$x_col)]]
      plot_data$y <- plot_data[[paste0("Dim.", km_data$y_col)]]
      plot_data$cluster_numeric <- as.numeric(plot_data$cluster)
      plot_data
    }),
    base_filename = paste0("SeriARC_KMeans_Plot_", input$num_clusters %||% 3, "_Cluster"),
    tr = tr,
    plot_generator_func = function(plot_data) {
      num_clusters <- input$num_clusters %||% 3
      colors <- seri_arc_colors()
      cluster_colors <- colors$clusters[1:num_clusters]
      point_colors <- cluster_colors[plot_data$cluster_numeric]
      plot(plot_data$x, plot_data$y, col = point_colors,
           pch = ifelse(plot_data$type == 'Site', 16, 17),
           cex = (input$point_size_km %||% 3) * 0.8,
           xlab = paste(input$x_dim %||% "Dim1"),
           ylab = paste(input$y_dim %||% "Dim2"),
           main = sprintf("SeriARC K-Means Clustering (%d Cluster)", num_clusters))
      legend("topright", legend = paste("Cluster", 1:num_clusters),
             col = cluster_colors, pch = 16, cex = 0.8)
      grid(col = "lightgray", lty = "dotted")

      # Parameter info for PDF (reproducibility)
      n_dims <- input$kmeans_n_dims %||% 2

      mtext(sprintf("PDF | %s vs %s | %dD | %s | k=%d | Seed=123 | n=%d | %s",
                    input$x_dim %||% "Dim1", input$y_dim %||% "Dim2", n_dims,
                    input$cluster_on %||% "both", num_clusters,
                    nrow(plot_data), format(Sys.time(), "%Y-%m-%d %H:%M")),
            side = 1, line = 4, cex = 0.55, col = "gray")
    }
  )
  
  output$download_kmeans_data <- create_excel_download_handler(
    data_sheets_func = function() {
      req(kmeans_res())
      km_data <- kmeans_res()
      method <- km_data$method %||% "kmeans"

      # Collect parameters for reproducibility
      params <- list(
        method = method,
        x_dim = input$x_dim,
        y_dim = input$y_dim,
        n_dims = input$kmeans_n_dims,
        cluster_on = input$cluster_on,
        num_clusters = input$num_clusters,
        seed = if (method == "kmeans") 123 else NA,
        hclust_method = if (method == "hierarchical") input$hclust_method else NA,
        fuzzy_m = if (method == "fuzzy") input$fuzzy_m else NA,
        gmm_model = if (method == "gmm") input$gmm_model_type else NA
      )
      create_kmeans_excel_sheets(km_data, get_cluster_name, parameters = params)
    },
    base_filename = paste0("Clustering_Daten_", input$num_clusters %||% 3, "_Cluster"),
    export_type = "Clustering",
    tr = tr
  )

  # HTML Export (interactive Plotly)
  output$download_kmeans_plot_html <- downloadHandler(
    filename = function() {
      num_clusters <- input$num_clusters %||% 3
      sprintf("SeriARC_KMeans_%s_Cluster_%s.html", num_clusters, Sys.Date())
    },
    content = function(file) {
      showNotification(tr("notify.export.html"), type = "message", duration = 3)
      tryCatch({
        p <- last_kmeans_plotly()
        req(p)
        htmlwidgets::saveWidget(p, file, selfcontained = TRUE)
        showNotification(tr("notify.export.html.success"), type = "message", duration = 2)
      }, error = function(e) {
        showNotification(paste(tr("notify.export.html.error"), e$message), type = "error", duration = 5)
      })
    }
  )

  # === DENDROGRAM FOR HIERARCHICAL CLUSTERING (PLOTLY + COLORED) ===
  output$dendrogram_plotly <- renderPlotly({
    req(kmeans_res())
    km_data <- kmeans_res()
    
    # Only for hierarchical clustering
    if (km_data$method != "hierarchical") {
      # Placeholder for non-hierarchical methods
      plot_ly() %>%
        add_annotations(
          text = tr("kmeans.dendro.not.available"),
          x = 0.5, y = 0.5,
          xref = "paper", yref = "paper",
          showarrow = FALSE,
          font = list(size = 16, color = "gray")
        ) %>%
        layout(
          xaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE),
          yaxis = list(showgrid = FALSE, showticklabels = FALSE, zeroline = FALSE)
        )
    } else {
      # Check if hc_tree is available
      hc_tree <- km_data$clustering_result$hc_tree
      if (is.null(hc_tree)) {
        plot_ly() %>%
          add_annotations(
            text = tr("kmeans.dendro.tree.unavailable"),
            x = 0.5, y = 0.5,
            xref = "paper", yref = "paper",
            showarrow = FALSE,
            font = list(size = 16, color = "red")
          )
      } else {
        num_clusters <- input$num_clusters %||% 3
        hc_method <- input$hclust_method %||% "average"
        
        # Cluster-Farben
        colors <- seri_arc_colors()
        cluster_colors <- colors$clusters[1:num_clusters]
        
        # Cluster-Zuordnungen
        cluster_groups <- cutree(hc_tree, k = num_clusters)
        
        # GGPLOT2 + GGPLOTLY ANSATZ
        tryCatch({
          if (requireNamespace("ggdendro", quietly = TRUE) && 
              requireNamespace("dendextend", quietly = TRUE)) {
            library(ggplot2)
            library(ggdendro)
            library(dendextend)
            
            dend <- as.dendrogram(hc_tree)
            dend <- color_branches(dend, k = num_clusters, col = cluster_colors)
            
            # Extract data
            dend_data <- dendro_data(dend)
            
            # Cluster info for labels
            label_data <- dend_data$labels
            label_data$cluster <- cluster_groups[as.character(label_data$label)]
            label_data$cluster_name <- sapply(label_data$cluster, get_cluster_name)
            label_data$hover <- paste0(label_data$label, "\n", label_data$cluster_name)
            
            # Segments with colors
            segment_data <- dend_data$segments
            
            # Color segments based on cluster membership
            # (Simplified: all segments black, only branches colored via dendextend)
            segment_data$col <- "black"
            
            # DIRECTLY WITH PLOTLY (textangle works here!)
            p <- plot_ly()
            
            # Prepare CLUSTER BLOCKS as colored rectangles (OPTIONAL)
            cluster_shapes <- list()
            if (!is.null(input$show_cluster_blocks) && input$show_cluster_blocks) {
              cluster_positions <- lapply(1:num_clusters, function(clust) {
                indices <- which(label_data$cluster == clust)
                if (length(indices) > 0) {
                  list(
                    x_min = min(label_data$x[indices]) - 0.5,
                    x_max = max(label_data$x[indices]) + 0.5,
                    cluster = clust
                  )
                } else NULL
              })
              cluster_positions <- Filter(Negate(is.null), cluster_positions)
              
              cluster_shapes <- lapply(cluster_positions, function(cp) {
                list(
                  type = "rect",
                  x0 = cp$x_min, x1 = cp$x_max,
                  y0 = -2, y1 = max(segment_data$yend) * 1.05,
                  fillcolor = cluster_colors[cp$cluster],
                  opacity = 0.15,
                  line = list(width = 0),
                  layer = "below"
                )
              })
            }
            
            # Segmente zeichnen
            for (i in 1:nrow(segment_data)) {
              seg <- segment_data[i, ]
              p <- p %>% add_segments(
                x = seg$x, xend = seg$xend,
                y = seg$y, yend = seg$yend,
                line = list(color = "black", width = 1),
                hoverinfo = "none",
                showlegend = FALSE
              )
            }
            
            # Cluster-Schnittlinie
            cut_height <- hc_tree$height[length(hc_tree$height) - num_clusters + 1]
            p <- p %>% add_segments(
              x = 0, xend = nrow(label_data),
              y = cut_height, yend = cut_height,
              line = list(color = "red", width = 2, dash = "dash"),
              name = "Cut",
              hovertext = sprintf("Cut at height %.2f", cut_height),
              hoverinfo = "text",
              showlegend = TRUE
            )
            
            # Legend for clusters (invisible traces)
            for (clust in 1:num_clusters) {
              p <- p %>% add_trace(
                x = NA, y = NA,
                type = "scatter",
                mode = "markers",
                marker = list(size = 10, color = cluster_colors[clust]),
                name = get_cluster_name(clust),
                showlegend = TRUE,
                hoverinfo = "skip"
              )
            }
            
            # Labels VERTICAL with add_annotations (BLACK)
            for (i in 1:nrow(label_data)) {
              lab <- label_data[i, ]
              p <- p %>% add_annotations(
                x = lab$x,
                y = -0.5,
                text = lab$label,
                textangle = 270,
                showarrow = FALSE,
                font = list(size = 9, color = "black"),
                xanchor = "center",
                yanchor = "top"
              )
            }
            
            # Layout WITH SHAPES
            p <- p %>% layout(
              title = sprintf("%s<br><sub>%s Linkage, %d %s</sub>",
                              tr("plot.kmeans.dendro.title"), hc_method, num_clusters,
                              if(num_clusters == 1) "Cluster" else "Clusters"),
              xaxis = list(title = "", showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
              yaxis = list(title = tr("term.height.distance"), showgrid = TRUE, gridcolor = "#ecf0f1",
                           range = list(-2, max(segment_data$yend) * 1.05)),
              hovermode = "closest",
              plot_bgcolor = "white",
              paper_bgcolor = "white",
              margin = list(b = 100, l = 60, r = 40, t = 80),
              showlegend = TRUE,
              shapes = cluster_shapes  # HERE rectangles are added
            )
            
            standard_plotly_config(p, "2d")
            
          } else {
            # FALLBACK: Base R Dendrogramm
            warning("ggdendro or dendextend not available - using Base-Plot")
            
            dend <- as.dendrogram(hc_tree)
            
            # Einfaches statisches Dendrogramm
            p <- plot_ly()
            
            # Manuelle Segment-Extraktion aus Dendrogramm
            # (Complicated without ggdendro)
            
            # Placeholder message
            p <- p %>% add_annotations(
              text = tr("kmeans.dendro.packages.required"),
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 12, color = "orange")
            ) %>%
              layout(
                xaxis = list(showgrid = FALSE, showticklabels = FALSE),
                yaxis = list(showgrid = FALSE, showticklabels = FALSE)
              )

            p
          }
        }, error = function(e) {
          message("Dendrogram error: ", e$message)
          plot_ly() %>%
            add_annotations(
              text = sprintf(tr("kmeans.dendro.error"), e$message),
              x = 0.5, y = 0.5,
              xref = "paper", yref = "paper",
              showarrow = FALSE,
              font = list(size = 12, color = "red")
            )
        })
      }
    }
  })
  
  # Download-Handler Dendrogramm PNG
  output$download_dendrogram_png <- downloadHandler(
    filename = function() {
      sprintf("SeriARC_Dendrogram_%dCluster_%s.png",
              input$num_clusters %||% 3,
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      km_data <- kmeans_res()
      hc_tree <- km_data$clustering_result$hc_tree
      num_clusters <- input$num_clusters %||% 3
      hc_method <- input$hclust_method %||% "average"
      
      png(file, width = 1200, height = 800, res = 150)
      
      # Try colored dendrogram with dendextend
      tryCatch({
        if (requireNamespace("dendextend", quietly = TRUE)) {
          library(dendextend)
          colors <- seri_arc_colors()
          cluster_colors <- colors$clusters[1:num_clusters]
          
          dend <- as.dendrogram(hc_tree)
          dend <- color_branches(dend, k = num_clusters, col = cluster_colors)
          
          par(mar = c(5, 4, 4, 2))
          plot(dend,
               main = sprintf("%s\n%s Linkage, %d Cluster",
                              tr("plot.kmeans.dendro.title"), hc_method, num_clusters),
               xlab = "", ylab = tr("term.height.distance"))

          abline(h = hc_tree$height[length(hc_tree$height) - num_clusters + 1],
                 col = "red", lty = 2, lwd = 2)
        } else {
          # Fallback without colors
          par(mar = c(5, 4, 4, 2))
          plot(as.dendrogram(hc_tree),
               main = sprintf("SeriARC Dendrogram (%s, %d Cluster)",
                              hc_method, num_clusters),
               xlab = "", ylab = tr("term.height"))

          abline(h = hc_tree$height[length(hc_tree$height) - num_clusters + 1],
                 col = "red", lty = 2, lwd = 2)
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste(tr("term.error"), ":", e$message), cex = 1.2, col = "red")
      })
      
      dev.off()
    }
  )
  
  # Download-Handler Dendrogramm SVG
  output$download_dendrogram_svg <- downloadHandler(
    filename = function() {
      sprintf("SeriARC_Dendrogram_%dCluster_%s.svg",
              input$num_clusters %||% 3,
              format(Sys.time(), "%Y%m%d_%H%M%S"))
    },
    content = function(file) {
      km_data <- kmeans_res()
      hc_tree <- km_data$clustering_result$hc_tree
      num_clusters <- input$num_clusters %||% 3
      hc_method <- input$hclust_method %||% "average"
      
      svg(file, width = 12, height = 8)

      tryCatch({
        if (requireNamespace("dendextend", quietly = TRUE)) {
          library(dendextend)
          colors <- seri_arc_colors()
          cluster_colors <- colors$clusters[1:num_clusters]

          dend <- as.dendrogram(hc_tree)
          dend <- color_branches(dend, k = num_clusters, col = cluster_colors)

          par(mar = c(5, 4, 4, 2))
          plot(dend,
               main = sprintf("%s (%s, %d Cluster)",
                              tr("plot.kmeans.dendro.title"), hc_method, num_clusters),
               xlab = "", ylab = tr("term.height.distance"))

          abline(h = hc_tree$height[length(hc_tree$height) - num_clusters + 1],
                 col = "red", lty = 2, lwd = 2)
        } else {
          par(mar = c(5, 4, 4, 2))
          plot(as.dendrogram(hc_tree),
               main = sprintf("SeriARC Dendrogram (%s, %d Cluster)",
                              hc_method, num_clusters),
               xlab = "", ylab = tr("term.height"))

          abline(h = hc_tree$height[length(hc_tree$height) - num_clusters + 1],
                 col = "red", lty = 2, lwd = 2)
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste(tr("term.error"), ":", e$message), cex = 1.2, col = "red")
      })

      dev.off()
    }
  )

  # Download-Handler Dendrogramm PDF
  output$download_dendrogram_pdf <- downloadHandler(
    filename = function() {
      sprintf("SeriARC_Dendrogram_%dCluster_%s.pdf",
              input$num_clusters %||% 3,
              format(Sys.time(), "%Y-%m-%d_%H%M%S"))
    },
    content = function(file) {
      km_data <- kmeans_res()
      hc_tree <- km_data$clustering_result$hc_tree
      num_clusters <- input$num_clusters %||% 3
      hc_method <- input$hclust_method %||% "average"
      
      pdf(file, width = 12, height = 8)
      
      tryCatch({
        if (requireNamespace("dendextend", quietly = TRUE)) {
          library(dendextend)
          colors <- seri_arc_colors()
          cluster_colors <- colors$clusters[1:num_clusters]
          
          dend <- as.dendrogram(hc_tree)
          dend <- color_branches(dend, k = num_clusters, col = cluster_colors)
          
          par(mar = c(5, 4, 4, 2))
          plot(dend,
               main = sprintf("SeriARC Dendrogram (%s, %d Cluster)",
                              hc_method, num_clusters),
               xlab = "", ylab = "Height (Distance)")
          
          abline(h = hc_tree$height[length(hc_tree$height) - num_clusters + 1],
                 col = "red", lty = 2, lwd = 2)
        } else {
          par(mar = c(5, 4, 4, 2))
          plot(as.dendrogram(hc_tree),
               main = sprintf("SeriARC Dendrogram (%s, %d Cluster)",
                              hc_method, num_clusters),
               xlab = "", ylab = "Height")
          
          abline(h = hc_tree$height[length(hc_tree$height) - num_clusters + 1],
                 col = "red", lty = 2, lwd = 2)
        }
      }, error = function(e) {
        plot.new()
        text(0.5, 0.5, paste("Error:", e$message), cex = 1.2, col = "red")
      })
      
      dev.off()
    }
  )
  
  # === CLUSTER HELPER FOR OTHER MODULES (SIMPLIFIED) ===
  get_cluster_colors <- function() {
    num_clusters <- input$num_clusters %||% 3
    colors <- seri_arc_colors()
    cluster_colors <- colors$clusters[1:num_clusters]
    names(cluster_colors) <- as.character(seq_len(num_clusters))
    return(cluster_colors)
  }
  
  get_cluster_names <- function() {
    if (is.null(cache$kmeans_result)) return(NULL)
    num_clusters <- input$num_clusters %||% 3
    result <- character(num_clusters)
    names(result) <- as.character(1:num_clusters)
    for (i in 1:num_clusters) {
      result[as.character(i)] <- get_cluster_name(i)
    }
    return(result)
  }
  
  # Return
  return(list(
    kmeans_result = kmeans_res,
    cluster_names = get_cluster_names,
    cluster_colors = get_cluster_colors
  ))
}

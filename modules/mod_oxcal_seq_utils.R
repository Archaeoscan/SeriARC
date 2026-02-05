# mod_oxcal_seq_utils.R
# OPTIMIZED VERSION - Parser simplification
# Removed: Old parser functions (~200 lines less)

# ===================== RANGE FILTERING HELPER FUNCTIONS =====================

.extract_phase_from_boundary <- function(boundary_name) {
  if (is.null(boundary_name) || !is.character(boundary_name) || length(boundary_name) == 0) {
    return(NULL)
  }

  # Special cases
  if (boundary_name %in% c("Start", "End")) {
    return(boundary_name)  # Will be handled specially in get_data()
  }

  # Remove "_Start" or "_End" suffixes
  if (grepl("_Start$", boundary_name)) {
    return(sub("_Start$", "", boundary_name))
  }
  if (grepl("_End$", boundary_name)) {
    return(sub("_End$", "", boundary_name))
  }

  # If not a boundary, return the name
  return(boundary_name)
}

# ===================== OPERATOREN =====================
`%||%` <- function(a, b) if (!is.null(a)) a else b
`%or%` <- function(a, b) if (!is.null(a)) a else b

# ===================== LABEL-SANITISIERUNG =====================

.sanitize_label <- function(x) {
  x <- as.character(x)
  x <- gsub('"', "", x)
  x <- gsub("[\u201c\u201d\u201e]", "", x)  # typographic quotation marks
  x <- gsub("[\u2018\u2019\u201a]", "", x)  # typogr. Apostrophe
  x <- gsub("`", "", x)
  x <- gsub("\u00b4", "", x)
  x <- chartr("äöüÄÖÜß", "aeoeueAeOeUess", x)
  x <- gsub("[^A-Za-z0-9_]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("^_+|_+$", "", x)
  ifelse(nzchar(x), x, "X")
}

.build_label_map <- function(site_ids) {
  pretty <- as.character(site_ids)
  safe   <- .sanitize_label(pretty)
  safe <- make.unique(safe, sep = "_")
  data.frame(pretty = pretty, safe = safe, stringsAsFactors = FALSE)
}

.map_ci_names <- function(df) {
  ren <- c(
    "from.68"="from_68","to.68"="to_68", "from.95"="from_95","to.95"="to_95",
    "lower_68"="from_68","upper_68"="to_68", "lower_95"="from_95","upper_95"="to_95",
    "Lower.68"="from_68","Upper.68"="to_68", "Lower.95"="from_95","Upper.95"="to_95"
  )
  hit <- intersect(names(df), names(ren))
  names(df)[match(hit, names(df))] <- unname(ren[hit])
  df
}

# ===================== FORMATIERUNG =====================

.to_cal_label <- function(x) {
  ifelse(is.finite(x) & x < 0, paste0(abs(round(x)), " cal BC"),
         ifelse(is.finite(x) & x > 0, paste0(round(x), " cal AD"), NA_character_))
}

.to_bc_num <- function(x) { 
  ifelse(is.finite(x), -x, NA_real_) 
}

# ===================== OXCAL SETUP =====================

.ensure_oxcal <- function() {
  if (!requireNamespace("oxcAAR", quietly = TRUE)) {
    showNotification("Package 'oxcAAR' is missing!", type="error")
    return(NULL)
  }
  ox_opt <- getOption("oxcAAR.oxcal_path")
  if (!is.null(ox_opt) && nzchar(ox_opt) && file.exists(ox_opt)) {
    return(normalizePath(ox_opt))
  }
  ox_env <- Sys.getenv("OXCAL_PATH", "")
  if (nzchar(ox_env) && file.exists(ox_env)) {
    oxcAAR::setOxcalExecutablePath(ox_env)
    return(normalizePath(ox_env))
  }
  win_guess <- "C:/Program Files/OxCal/bin/OxCalWin.exe"
  if (.Platform$OS.type == "windows" && file.exists(win_guess)) {
    oxcAAR::setOxcalExecutablePath(win_guess)
    return(normalizePath(win_guess))
  }
  try(oxcAAR::quickSetupOxcal(), silent = TRUE)
  if (is.null(getOption("oxcAAR.oxcal_path"))) {
    showNotification("No OxCal path found. Please set OXCAL_PATH!", type="error")
    return(NULL)
  }
  getOption("oxcAAR.oxcal_path")
}

.oxcal_smoke_test <- function() {
  if (!requireNamespace("oxcAAR", quietly = TRUE)) {
    return(list(success = FALSE, message = "oxcAAR package is missing"))
  }
  test_cql <- 'Plot(){ R_Date("test", 5000, 25); };'
  tryCatch({
    js_path <- oxcAAR::executeOxcalScript(test_cql)
    lines <- oxcAAR::readOxcalOutput(js_path)
    has_model_element <- any(grepl("model\\.element\\[", lines, perl=TRUE))
    has_posterior <- any(grepl("\\.posterior\\s*=", lines, perl=TRUE))
    list(
      success = TRUE,
      has_model_element = has_model_element,
      has_posterior = has_posterior,
      message = if (has_model_element) "✅ OxCal Pipeline OK" else "⚠️ No model.element[] found"
    )
  }, error = function(e) {
    list(success = FALSE, message = paste("Error:", e$message))
  })
}

# ===================== GLOBAL AGREEMENTS =====================

.parse_global_agreements <- function(lines) {
  txt <- paste(lines, collapse = "\n")
  
  pull_first_num <- function(regex_vec) {
    for (rx in regex_vec) {
      m <- regexpr(rx, txt, perl = TRUE)
      if (m[1] != -1) {
        s <- regmatches(txt, m)
        nums <- suppressWarnings(as.numeric(unlist(regmatches(
          s, gregexpr("[-]?[0-9]+\\.?[0-9]*", s, perl=TRUE)
        ))))
        if (length(nums)) return(nums[1])
      }
    }
    NA_real_
  }
  
  Aoverall <- pull_first_num(c(
    "overallAgreement\\s*[:\\=]\\s*[-]?[0-9.]+",
    "Aoverall\\s*[:\\=]\\s*[-]?[0-9.]+",
    "overall_agreement\\s*[:\\=]\\s*[-]?[0-9.]+"
  ))
  
  Amodel <- pull_first_num(c(
    "modelAgreement\\s*[:\\=]\\s*[-]?[0-9.]+",
    "Amodel\\s*[:\\=]\\s*[-]?[0-9.]+",
    "model_agreement\\s*[:\\=]\\s*[-]?[0-9.]+"
  ))
  
  Acomb <- pull_first_num(c(
    "combinedAgreement\\s*[:\\=]\\s*[-]?[0-9.]+",
    "Acomb\\s*[:\\=]\\s*[-]?[0-9.]+",
    "combined_agreement\\s*[:\\=]\\s*[-]?[0-9.]+"
  ))
  
  list(
    Aoverall = if (is.finite(Aoverall)) Aoverall else NA_real_,
    Amodel   = if (is.finite(Amodel))   Amodel   else NA_real_,
    Acomb    = if (is.finite(Acomb))    Acomb    else NA_real_
  )
}

# ===================== MODEL.PROPORTIONAL INTEGRATION =====================

.parse_model_proportional <- function(lines) {
  if (length(lines) == 0) return(data.frame())
  
  txt <- paste(lines, collapse = "\n")
  prop_rx <- 'model\\.proportional\\[(\\d+)\\]\\s*=\\s*\\{[^}]*n\\s*:\\s*(\\d+)'
  matches <- gregexpr(prop_rx, txt, perl = TRUE)
  
  if (matches[[1]][1] == -1) return(data.frame())
  
  hits <- regmatches(txt, matches)[[1]]
  results <- list()
  
  for (hit in hits) {
    prop_idx <- as.numeric(sub(prop_rx, "\\1", hit, perl = TRUE))
    ocd_idx  <- as.numeric(sub(prop_rx, "\\2", hit, perl = TRUE))
    
    outlier_rx <- sprintf('model\\.proportional\\[%d\\]\\.outlier_post\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', prop_idx)
    outlier_match <- regexpr(outlier_rx, txt, perl = TRUE)
    outlier_post <- NA_real_
    if (outlier_match[1] != -1) {
      outlier_str <- regmatches(txt, outlier_match)
      outlier_post <- as.numeric(sub(outlier_rx, "\\1", outlier_str, perl = TRUE))
    }
    
    agreement_rx <- sprintf('model\\.proportional\\[%d\\]\\.agreement\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', prop_idx)
    agreement_match <- regexpr(agreement_rx, txt, perl = TRUE)
    agreement <- NA_real_
    if (agreement_match[1] != -1) {
      agreement_str <- regmatches(txt, agreement_match)
      agreement <- as.numeric(sub(agreement_rx, "\\1", agreement_str, perl = TRUE))
    }
    
    results[[length(results) + 1]] <- data.frame(
      ocd_idx = ocd_idx,
      p_outlier = outlier_post,
      A = agreement,
      stringsAsFactors = FALSE
    )
  }
  
  if (length(results) == 0) return(data.frame())
  do.call(rbind, results)
}

.merge_model_proportional <- function(tidy, lines) {
  if (!is.data.frame(tidy) || nrow(tidy) == 0) return(tidy)
  
  prop_data <- .parse_model_proportional(lines)
  if (!is.data.frame(prop_data) || nrow(prop_data) == 0) return(tidy)
  
  txt <- paste(lines, collapse = "\n")
  
  for (i in seq_len(nrow(tidy))) {
    name <- tidy$name[i]
    ocd_rx <- sprintf('ocd\\[(\\d+)\\]\\.name\\s*=\\s*"%s"', name)
    ocd_match <- regexpr(ocd_rx, txt, perl = TRUE)
    
    if (ocd_match[1] != -1) {
      ocd_str <- regmatches(txt, ocd_match)
      ocd_idx <- as.numeric(sub(ocd_rx, "\\1", ocd_str, perl = TRUE))
      
      prop_row <- which(prop_data$ocd_idx == ocd_idx)
      
      if (length(prop_row) > 0) {
        if (is.na(tidy$p_outlier[i]) || (!is.na(prop_data$p_outlier[prop_row[1]]) && prop_data$p_outlier[prop_row[1]] > 0)) {
          tidy$p_outlier[i] <- prop_data$p_outlier[prop_row[1]]
        }
        if (is.na(tidy$A[i]) || !is.na(prop_data$A[prop_row[1]])) {
          tidy$A[i] <- prop_data$A[prop_row[1]]
        }
      }
    }
  }
  
  tidy
}

# ===================== POSTERIOR DENSITY EXTRACTION =====================

# NEW FUNCTION: Calculate likelihood intervals from densities
.extract_likelihood_intervals <- function(lines, node_names) {
  if (length(lines) == 0 || length(node_names) == 0) return(data.frame())
  
  txt <- paste(lines, collapse = "\n")
  results <- data.frame(
    name = node_names,
    like_median = NA_real_,
    like_from_68 = NA_real_, like_to_68 = NA_real_,
    like_from_95 = NA_real_, like_to_95 = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(node_names)) {
    name <- node_names[i]
    
    # OCD-Index finden
    ocd_rx <- sprintf('ocd\\[(\\d+)\\]\\.name\\s*=\\s*"%s"', name)
    ocd_match <- regexpr(ocd_rx, txt, perl = TRUE)
    if (ocd_match[1] == -1) next
    
    ocd_str <- regmatches(txt, ocd_match)
    ocd_idx <- as.numeric(sub(ocd_rx, "\\1", ocd_str, perl = TRUE))
    
    # Likelihood-Dichte-Parameter extrahieren
    start_rx <- sprintf('ocd\\[%d\\]\\.likelihood\\.start\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', ocd_idx)
    start_match <- regexpr(start_rx, txt, perl = TRUE)
    if (start_match[1] == -1) next
    start_val <- as.numeric(sub(start_rx, "\\1", regmatches(txt, start_match), perl = TRUE))
    
    res_rx <- sprintf('ocd\\[%d\\]\\.likelihood\\.resolution\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', ocd_idx)
    res_match <- regexpr(res_rx, txt, perl = TRUE)
    if (res_match[1] == -1) next
    res_val <- as.numeric(sub(res_rx, "\\1", regmatches(txt, res_match), perl = TRUE))
    
    prob_rx <- sprintf('ocd\\[%d\\]\\.likelihood\\.prob\\s*=\\s*\\[([^\\]]+)\\]', ocd_idx)
    prob_match <- regexpr(prob_rx, txt, perl = TRUE)
    if (prob_match[1] == -1) next
    
    prob_str <- regmatches(txt, prob_match)
    prob_vals_str <- sub(prob_rx, "\\1", prob_str, perl = TRUE)
    prob_vals <- suppressWarnings(as.numeric(unlist(strsplit(prob_vals_str, "\\s*,\\s*"))))
    
    if (length(prob_vals) == 0) next
    
    # X values and normalized probabilities
    x_vals <- start_val + (seq_along(prob_vals) - 1) * res_val
    prob_vals_norm <- prob_vals / sum(prob_vals, na.rm = TRUE)  # Normalization for probability
    
    # Calculate cumulative probabilities
    cum_prob <- cumsum(prob_vals_norm)
    
    # Calculate quantiles
    q025 <- approx(cum_prob, x_vals, xout = 0.025, rule = 2)$y
    q16  <- approx(cum_prob, x_vals, xout = 0.16,  rule = 2)$y  # 68% = 16%-84%
    q50  <- approx(cum_prob, x_vals, xout = 0.5,   rule = 2)$y  # Median
    q84  <- approx(cum_prob, x_vals, xout = 0.84,  rule = 2)$y
    q975 <- approx(cum_prob, x_vals, xout = 0.975, rule = 2)$y
    
    # Store results
    if (all(is.finite(c(q50, q16, q84, q025, q975)))) {
      results$like_median[i]  <- q50
      results$like_from_68[i] <- q16
      results$like_to_68[i]   <- q84
      results$like_from_95[i] <- q025
      results$like_to_95[i]   <- q975
    }
  }
  
  results
}

.extract_posterior_densities <- function(lines, node_names) {
  if (length(lines) == 0 || length(node_names) == 0) return(list())
  
  txt <- paste(lines, collapse = "\n")
  densities <- list()
  
  for (name in node_names) {
    ocd_rx <- sprintf('ocd\\[(\\d+)\\]\\.name\\s*=\\s*"%s"', name)
    ocd_match <- regexpr(ocd_rx, txt, perl = TRUE)
    
    if (ocd_match[1] == -1) next
    
    ocd_str <- regmatches(txt, ocd_match)
    ocd_idx <- as.numeric(sub(ocd_rx, "\\1", ocd_str, perl = TRUE))
    
    start_rx <- sprintf('ocd\\[%d\\]\\.posterior\\.start\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', ocd_idx)
    start_match <- regexpr(start_rx, txt, perl = TRUE)
    if (start_match[1] == -1) next
    start_val <- as.numeric(sub(start_rx, "\\1", regmatches(txt, start_match), perl = TRUE))
    
    res_rx <- sprintf('ocd\\[%d\\]\\.posterior\\.resolution\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', ocd_idx)
    res_match <- regexpr(res_rx, txt, perl = TRUE)
    if (res_match[1] == -1) next
    res_val <- as.numeric(sub(res_rx, "\\1", regmatches(txt, res_match), perl = TRUE))
    
    prob_rx <- sprintf('ocd\\[%d\\]\\.posterior\\.prob\\s*=\\s*\\[([^\\]]+)\\]', ocd_idx)
    prob_match <- regexpr(prob_rx, txt, perl = TRUE)
    if (prob_match[1] == -1) next
    
    prob_str <- regmatches(txt, prob_match)
    prob_vals_str <- sub(prob_rx, "\\1", prob_str, perl = TRUE)
    prob_vals <- suppressWarnings(as.numeric(unlist(strsplit(prob_vals_str, "\\s*,\\s*"))))
    
    if (length(prob_vals) == 0) next
    
    x_vals <- start_val + (seq_along(prob_vals) - 1) * res_val
    x_vals_bc <- -x_vals
    prob_vals_norm <- prob_vals / max(prob_vals, na.rm = TRUE)
    
    densities[[name]] <- data.frame(
      cal_bp = x_vals,  # Original x_vals for consistency
      density = prob_vals_norm,
      stringsAsFactors = FALSE
    )
  }
  
  densities
}

.extract_likelihood_densities <- function(lines, node_names) {
  if (length(lines) == 0 || length(node_names) == 0) return(list())
  
  txt <- paste(lines, collapse = "\n")
  densities <- list()
  
  for (name in node_names) {
    ocd_rx <- sprintf('ocd\\[(\\d+)\\]\\.name\\s*=\\s*"%s"', name)
    ocd_match <- regexpr(ocd_rx, txt, perl = TRUE)
    
    if (ocd_match[1] == -1) next
    
    ocd_str <- regmatches(txt, ocd_match)
    ocd_idx <- as.numeric(sub(ocd_rx, "\\1", ocd_str, perl = TRUE))
    
    start_rx <- sprintf('ocd\\[%d\\]\\.likelihood\\.start\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', ocd_idx)
    start_match <- regexpr(start_rx, txt, perl = TRUE)
    if (start_match[1] == -1) next
    start_val <- as.numeric(sub(start_rx, "\\1", regmatches(txt, start_match), perl = TRUE))
    
    res_rx <- sprintf('ocd\\[%d\\]\\.likelihood\\.resolution\\s*=\\s*([-]?[0-9]+\\.?[0-9]*)', ocd_idx)
    res_match <- regexpr(res_rx, txt, perl = TRUE)
    if (res_match[1] == -1) next
    res_val <- as.numeric(sub(res_rx, "\\1", regmatches(txt, res_match), perl = TRUE))
    
    prob_rx <- sprintf('ocd\\[%d\\]\\.likelihood\\.prob\\s*=\\s*\\[([^\\]]+)\\]', ocd_idx)
    prob_match <- regexpr(prob_rx, txt, perl = TRUE)
    if (prob_match[1] == -1) next
    
    prob_str <- regmatches(txt, prob_match)
    prob_vals_str <- sub(prob_rx, "\\1", prob_str, perl = TRUE)
    prob_vals <- suppressWarnings(as.numeric(unlist(strsplit(prob_vals_str, "\\s*,\\s*"))))
    
    if (length(prob_vals) == 0) next
    
    x_vals <- start_val + (seq_along(prob_vals) - 1) * res_val
    x_vals_bc <- -x_vals
    prob_vals_norm <- prob_vals / max(prob_vals, na.rm = TRUE)
    
    densities[[name]] <- data.frame(
      cal_bp = x_vals,  # Original x_vals for consistency 
      density = prob_vals_norm,
      stringsAsFactors = FALSE
    )
  }
  
  densities
}

# ===================== ROBUSTE CI-EXTRAKTION (aus OLD) =====================

# Hull-basierte Intervall-Extraktion
.hull_from_flat_ranges <- function(x, level = 1) {
  if (is.null(x) || !is.list(x)) return(c(NA_real_, NA_real_))
  nm <- names(x)
  if (is.null(nm)) return(c(NA_real_, NA_real_))
  rx <- sprintf("^range\\[%d\\]\\[\\d+\\]$", level)
  keys <- nm[grepl(rx, nm, perl = TRUE)]
  if (!length(keys)) return(c(NA_real_, NA_real_))
  segs <- lapply(keys, function(k) suppressWarnings(as.numeric(x[[k]])))
  segs <- segs[vapply(segs, function(v) is.numeric(v) && length(v) >= 2 && all(is.finite(v[1:2])), logical(1))]
  if (!length(segs)) return(c(NA_real_, NA_real_))
  froms <- vapply(segs, function(v) v[1], numeric(1))
  tos   <- vapply(segs, function(v) v[2], numeric(1))
  c(min(froms, na.rm = TRUE), max(tos, na.rm = TRUE))
}

# ===================== ROBUSTE AGREEMENT/OUTLIER-EXTRAKTION =====================

.get_num_safe <- function(x, keys) {
  for (k in keys) {
    v <- tryCatch(x[[k]], error = function(e) NULL)
    if (is.null(v)) next
    vv <- suppressWarnings(as.numeric(v))
    if (length(vv) == 1 && is.finite(vv)) return(vv)
  }
  NA_real_
}

# EXTENDED key lists for robust extraction
.OUTLIER_KEYS <- list(
  prob = c("pOutlier", "outlier", "outlierProb", "outlierProbability", "O", "Poutlier", "p", 
           "outlier_prob", "outlier_probability", "posterior_outlier", "likelihood_outlier")
)

.AGREEMENT_KEYS <- list(
  node  = c("agreement", "A", "Agmt", "posterior.agreement", "likelihood.agreement", 
            "posterior_agreement", "likelihood_agreement", "agmt", "Agreement"),
  model_overall = c("overallAgreement", "Aoverall", "overall_agreement", "OverallAgreement"),
  model_model   = c("modelAgreement",   "Amodel", "model_agreement", "ModelAgreement"),
  model_comb    = c("combinedAgreement","Acomb", "combined_agreement", "CombinedAgreement")
)

# IMPROVED extraction with more robustness
.extract_outlier_and_A <- function(node) {
  post <- node$posterior
  like <- node$likelihood
  p_out <- NA_real_
  A_val <- NA_real_

  # Primary search in posterior
  if (is.list(post)) {
    p_out <- .get_num_safe(post, .OUTLIER_KEYS$prob) %or% p_out
    A_val <- .get_num_safe(post, .AGREEMENT_KEYS$node) %or% A_val
  }
  
  # Secondary search in likelihood
  if (is.list(like)) {
    if (!is.finite(p_out)) p_out <- .get_num_safe(like, .OUTLIER_KEYS$prob)
    if (!is.finite(A_val)) A_val <- .get_num_safe(like, .AGREEMENT_KEYS$node)
  }
  
  list(
    p_outlier = if (is.finite(p_out)) p_out else NA_real_,
    A         = if (is.finite(A_val)) A_val else NA_real_
  )
}

# ===================== ROBUSTE KNOTEN-EXTRAKTION =====================

.extract_node <- function(node) {
  nm  <- as.character(node$name %||% node$label %||% NA_character_)
  typ <- as.character(node$op   %||% node$type %||% NA_character_)
  post <- node$posterior
  like <- node$likelihood
  
  # Posterior-Werte
  post_med <- suppressWarnings(as.numeric(post$median %||% post$mean))
  post_mn  <- suppressWarnings(as.numeric(post$mean))
  
  post_r68 <- .hull_from_flat_ranges(post, 1)
  post_r95 <- .hull_from_flat_ranges(post, 2)
  
  # FALLBACK: If no range objects, use mean±sigma
  if (all(is.na(post_r68)) || all(is.na(post_r95))) {
    sd <- suppressWarnings(as.numeric(post$sigma %||% post$sd))
    if (is.finite(post_mn) && is.finite(sd)) {
      if (all(is.na(post_r68))) post_r68 <- c(post_mn - sd,      post_mn + sd)
      if (all(is.na(post_r95))) post_r95 <- c(post_mn - 1.96*sd, post_mn + 1.96*sd)
    }
  }
  
  # Outlier & Agreement Extraktion (ROBUST)
  oa <- .extract_outlier_and_A(node)
  
  data.frame(
    name      = nm,
    type      = typ,
    mean      = if (is.finite(post_mn))  post_mn  else NA_real_,
    median    = if (is.finite(post_med)) post_med else if (is.finite(post_mn)) post_mn else NA_real_,
    from_68   = post_r68[1], to_68 = post_r68[2],
    from_95   = post_r95[1], to_95 = post_r95[2],
    A         = oa$A,
    p_outlier = oa$p_outlier,
    stringsAsFactors = FALSE
  )
}

# ===================== ROBUST MAIN PARSER =====================

# IMPROVED posterior extraction with fallback chain
.extract_posterior_df_hull <- function(parsed, lines = NULL) {
  if (is.null(parsed)) return(data.frame())
  
  if (!is.null(parsed$ocd)) {
    # Standard Fall: ocd ist ein Array  
    ocd_list <- parsed$ocd
  } else {
    # Alternative Fall: ocd[X] als flache Namen
    ocd_names <- names(parsed)[grepl("^ocd\\[\\d+\\]$", names(parsed))]
    ocd_list <- parsed[ocd_names] 
    names(ocd_list) <- NULL  # Entferne die "ocd[X]" Namen
  }
  
  if (!is.list(ocd_list) || length(ocd_list) == 0) return(data.frame())
  
  rows <- lapply(seq_along(ocd_list), function(i) {
    tryCatch({
      node <- ocd_list[[i]]
      if (is.null(node)) return(NULL)
      result <- .extract_node(node)
      if (!is.na(result$name) && nzchar(result$name)) return(result)
      NULL
    }, error = function(e) NULL)
  })
  
  valid_rows <- rows[!sapply(rows, is.null)]
  if (!length(valid_rows)) return(data.frame())
  
  out <- do.call(rbind, valid_rows)
  keep_types <- c("Boundary","R_Date","Phase","Span","Difference","Sequence","Interval")
  out$type[!(out$type %in% keep_types)] <- NA_character_
  out <- unique(out[!is.na(out$type) & nzchar(out$name), , drop = FALSE])
  
  # FALLBACK: Regex-based extraction for missing values
  if (!is.null(lines) && (any(is.na(out$p_outlier)) || any(is.na(out$A)))) {
    regex_results <- .extract_outlier_from_lines(lines, out$name)

    # Fill missing values with regex results
    for (i in seq_len(nrow(out))) {
      name_match <- which(regex_results$name == out$name[i])
      if (length(name_match) > 0) {
        if (is.na(out$p_outlier[i]) && !is.na(regex_results$p_outlier[name_match[1]])) {
          out$p_outlier[i] <- regex_results$p_outlier[name_match[1]]
        }
        if (is.na(out$A[i]) && !is.na(regex_results$A[name_match[1]])) {
          out$A[i] <- regex_results$A[name_match[1]]
        }
      }
    }
  }
  
  out
}

# NEUE FUNKTION: Regex-basierte Outlier-Extraktion als Fallback
.extract_outlier_from_lines <- function(lines, node_names) {
  if (length(lines) == 0 || length(node_names) == 0) return(data.frame())
  
  txt <- paste(lines, collapse = "\n")
  results <- data.frame(
    name = node_names,
    p_outlier = NA_real_,
    A = NA_real_,
    stringsAsFactors = FALSE
  )
  
  for (i in seq_along(node_names)) {
    name <- node_names[i]
    safe_name <- .sanitize_label(name)
    
    # Various patterns for outlier probabilities
    outlier_patterns <- c(
      sprintf('ocd\\[\\d+\\]\\.posterior\\.outlier\\s*=\\s*(\\d+\\.\\d+).*%s', name),
      sprintf('ocd\\[\\d+\\]\\.posterior\\.pOutlier\\s*=\\s*(\\d+\\.\\d+).*%s', name),
      sprintf('%s.*outlier.*?=\\s*(\\d+\\.\\d+)', name),
      sprintf('%s.*pOutlier.*?=\\s*(\\d+\\.\\d+)', name)
    )
    
    # Agreement-Patterns
    agreement_patterns <- c(
      sprintf('ocd\\[\\d+\\]\\.posterior\\.agreement\\s*=\\s*(\\d+\\.\\d+).*%s', name),
      sprintf('%s.*agreement.*?=\\s*(\\d+\\.\\d+)', name),
      sprintf('%s.*A\\s*=\\s*(\\d+\\.\\d+)', name)
    )
    
    # Suche nach Outlier-Werten
    for (pattern in outlier_patterns) {
      m <- regexpr(pattern, txt, perl = TRUE)
      if (m[1] != -1) {
        match_str <- regmatches(txt, m)
        num_match <- regexpr("\\d+\\.\\d+", match_str, perl = TRUE)
        if (num_match[1] != -1) {
          value <- as.numeric(regmatches(match_str, num_match))
          if (is.finite(value)) {
            results$p_outlier[i] <- value
            break
          }
        }
      }
    }
    
    # Suche nach Agreement-Werten
    for (pattern in agreement_patterns) {
      m <- regexpr(pattern, txt, perl = TRUE)
      if (m[1] != -1) {
        match_str <- regmatches(txt, m)
        num_match <- regexpr("\\d+\\.\\d+", match_str, perl = TRUE)
        if (num_match[1] != -1) {
          value <- as.numeric(regmatches(match_str, num_match))
          if (is.finite(value)) {
            results$A[i] <- value
            break
          }
        }
      }
    }
  }
  
  results
}

# ===================== FALLBACK PARSER =====================
# Only ONE minimal fallback parser for emergencies

.parse_oxcal_js_fallback <- function(lines) {
  if (length(lines) == 0) return(data.frame())
  txt <- paste(lines, collapse = "\n")
  m <- gregexpr('(Boundary|Span|Difference|R_Date)\\(\\"([^\\"]+)\\"', txt, perl = TRUE)
  if (m[[1]][1] == -1) return(data.frame())
  hits <- regmatches(txt, m)[[1]]
  
  ext <- lapply(hits, function(s){
    g <- regexec('(Boundary|Span|Difference|R_Date)\\(\\"([^\\"]+)\\"', s, perl = TRUE)
    sp <- regmatches(s, g)[[1]]
    data.frame(type=sp[2], name=sp[3], stringsAsFactors = FALSE)
  })
  
  result <- unique(do.call(rbind, ext))
  if (nrow(result) == 0) return(data.frame())
  
  result$mean <- result$median <- result$from_68 <- result$to_68 <- result$from_95 <- result$to_95 <- NA_real_
  result$A <- NA_real_
  result$p_outlier <- NA_real_
  result
}

# ===================== DEBUG-FUNKTIONEN =====================

.write_debug <- function(cql, lines, tidy, global_A, path = NULL) {
  if (is.null(path)) {
    path <- file.path(tempdir(), sprintf("oxcal_debug_%s.txt", format(Sys.time(), "%Y%m%d_%H%M%S")))
  }
  
  cat("=== CQL ===\n", cql, "\n\n", file = path, append = FALSE)
  
  cat("=== Global Agreements ===\n", file = path, append = TRUE)
  cat(sprintf("Aoverall: %.1f%%\n", ifelse(is.finite(global_A$Aoverall), global_A$Aoverall, -999)), file = path, append = TRUE)
  cat(sprintf("Amodel: %.1f%%\n", ifelse(is.finite(global_A$Amodel), global_A$Amodel, -999)), file = path, append = TRUE)
  cat(sprintf("Acomb: %.1f%%\n", ifelse(is.finite(global_A$Acomb), global_A$Acomb, -999)), file = path, append = TRUE)
  cat("\n", file = path, append = TRUE)
  
  if (is.data.frame(tidy) && "p_outlier" %in% names(tidy)) {
    outlier_count <- sum(!is.na(tidy$p_outlier) & tidy$p_outlier > 0.5, na.rm = TRUE)
    total_count <- sum(!is.na(tidy$p_outlier), na.rm = TRUE)
    cat(sprintf("=== Outlier-Statistiken ===\nGefunden: %d/%d haben p(Outlier) > 0.5\n", 
                outlier_count, total_count), file = path, append = TRUE)
    
    if (outlier_count > 0) {
      outliers <- tidy[!is.na(tidy$p_outlier) & tidy$p_outlier > 0.5, c("name", "p_outlier"), drop = FALSE]
      cat("Outlier-Liste:\n", file = path, append = TRUE)
      for (i in seq_len(nrow(outliers))) {
        cat(sprintf("  %s: %.1f%%\n", outliers$name[i], outliers$p_outlier[i] * 100), file = path, append = TRUE)
      }
    }
    cat("\n", file = path, append = TRUE)
  }
  
  if (is.data.frame(tidy) && "A" %in% names(tidy)) {
    low_agreement_count <- sum(!is.na(tidy$A) & tidy$A < 60, na.rm = TRUE)
    total_agreement_count <- sum(!is.na(tidy$A), na.rm = TRUE)
    cat(sprintf("=== Agreement-Statistiken ===\nNiedriges Agreement (<60%%): %d/%d\n", 
                low_agreement_count, total_agreement_count), file = path, append = TRUE)
    
    if (low_agreement_count > 0) {
      low_agreements <- tidy[!is.na(tidy$A) & tidy$A < 60, c("name", "A"), drop = FALSE]
      cat("Niedrige Agreement-Liste:\n", file = path, append = TRUE)
      for (i in seq_len(nrow(low_agreements))) {
        cat(sprintf("  %s: %.1f%%\n", low_agreements$name[i], low_agreements$A[i]), file = path, append = TRUE)
      }
    }
    cat("\n", file = path, append = TRUE)
  }
  
  cat("=== Erste 500 JS-Zeilen ===\n", paste(head(lines, 500), collapse = "\n"), "\n\n",
      file = path, append = TRUE)
  
  cat("=== tidy (Kopf) ===\n", capture.output(utils::head(tidy, 20)), sep = "\n",
      file = path, append = TRUE)
  
  cat(sprintf("\n=== Debug erstellt: %s ===\n", format(Sys.time())), file = path, append = TRUE)
  
  cat(sprintf("📋 Debug-Datei erstellt: %s\n", path))
  path
}

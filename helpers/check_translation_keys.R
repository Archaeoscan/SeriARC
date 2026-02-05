# check_keys.R - Automatic validation of translation keys
# Checks for:
# - Missing keys in translations.csv
# - Orphan keys (defined but not used anywhere)
# - Duplicate keys
# Usage:
#   source("translations/check_keys.R")
#   check_translation_keys()

check_translation_keys <- function(
  csv_path = "translations/translations.csv",
  search_dirs = c(".", "modules", "helpers", "helpers/ui_content", "helpers/server_logic"),
  file_patterns = c("\\.R$", "\\.r$"),
  verbose = TRUE
) {

  if (!file.exists(csv_path)) {
    stop("translations.csv not found: ", csv_path)
  }

  translations <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)
  defined_keys <- translations$key

  if (verbose) {
    message("\n=== Translation Key Check ===")
    message("CSV: ", csv_path)
    message("Defined keys: ", length(defined_keys))
  }

  # === 2. Check for duplicate keys ===
  duplicate_keys <- defined_keys[duplicated(defined_keys)]
  if (length(duplicate_keys) > 0) {
    warning("DUPLICATE KEYS found:\n  ", paste(duplicate_keys, collapse = "\n  "))
  }

  # === 3. Search all R files ===
  all_files <- character(0)
  for (dir in search_dirs) {
    if (dir.exists(dir)) {
      for (pattern in file_patterns) {
        files <- list.files(dir, pattern = pattern, full.names = TRUE, recursive = FALSE)
        all_files <- c(all_files, files)
      }
    }
  }

  # Remove duplicates
  all_files <- unique(all_files)
  # Exclude check_keys.R itself
  all_files <- all_files[!grepl("check_keys\\.R$", all_files)]

  if (verbose) {
    message("Files searched: ", length(all_files))
  }

  # === 4. Find tr() calls in files ===
  used_keys <- character(0)
  key_usage <- list()

  # Pattern for tr("key") or tr('key') or tr_reactive(lang, "key")
  tr_pattern <- '(?:tr|tr_reactive)\\s*\\([^)]*["\']([a-zA-Z0-9_\\.]+)["\']'

  for (file in all_files) {
    content <- tryCatch(
      readLines(file, warn = FALSE),
      error = function(e) character(0)
    )

    content_text <- paste(content, collapse = "\n")

    # Find all tr() matches
    matches <- gregexpr(tr_pattern, content_text, perl = TRUE)
    if (matches[[1]][1] != -1) {
      captured <- regmatches(content_text, matches)[[1]]
      # Extract keys from matches
      for (m in captured) {
        key_match <- regmatches(m, regexec('["\']([a-zA-Z0-9_\\.]+)["\']\\s*\\)', m, perl = TRUE))
        if (length(key_match[[1]]) >= 2) {
          key <- key_match[[1]][2]
          used_keys <- c(used_keys, key)

          # Track where key is used
          if (is.null(key_usage[[key]])) {
            key_usage[[key]] <- character(0)
          }
          key_usage[[key]] <- c(key_usage[[key]], basename(file))
        }
      }
    }
  }

  used_keys <- unique(used_keys)

  if (verbose) {
    message("Used keys: ", length(used_keys))
  }

  # === 5. Find missing keys ===
  missing_keys <- setdiff(used_keys, defined_keys)

  # === 6. Find orphan keys ===
  # Keys that are defined but not used
  orphan_keys <- setdiff(defined_keys, used_keys)

  # === 7. Output results ===
  if (verbose) {
    message("\n--- Results ---")

    if (length(duplicate_keys) > 0) {
      message("\n DUPLICATE KEYS (", length(duplicate_keys), "):")
      for (k in duplicate_keys) {
        message("  - ", k)
      }
    }

    if (length(missing_keys) > 0) {
      message("\n MISSING KEYS (", length(missing_keys), "):")
      for (k in sort(missing_keys)) {
        files <- unique(key_usage[[k]])
        message("  - ", k, "  [used in: ", paste(files, collapse = ", "), "]")
      }
    } else {
      message("\n No missing keys!")
    }

    if (length(orphan_keys) > 0) {
      message("\n ORPHAN KEYS (defined but not used): ", length(orphan_keys))
      # Show only first 20 if too many
      show_orphans <- head(sort(orphan_keys), 20)
      for (k in show_orphans) {
        message("  - ", k)
      }
      if (length(orphan_keys) > 20) {
        message("  ... and ", length(orphan_keys) - 20, " more")
      }
    }

    message("\n=== Check completed ===\n")
  }

  # === 8. Return result as list ===
  invisible(list(
    defined_keys = defined_keys,
    used_keys = used_keys,
    missing_keys = missing_keys,
    orphan_keys = orphan_keys,
    duplicate_keys = duplicate_keys,
    key_usage = key_usage,
    all_ok = length(missing_keys) == 0 && length(duplicate_keys) == 0
  ))
}

# === Helper function: Add missing keys to CSV ===
add_missing_keys <- function(
  csv_path = "translations/translations.csv",
  missing_keys = NULL,
  default_text = "[TODO]"
) {
  if (is.null(missing_keys)) {
    result <- check_translation_keys(csv_path = csv_path, verbose = FALSE)
    missing_keys <- result$missing_keys
  }

  if (length(missing_keys) == 0) {
    message("No missing keys to add.")
    return(invisible(NULL))
  }

  translations <- read.csv(csv_path, stringsAsFactors = FALSE, check.names = FALSE)

  new_rows <- data.frame(
    key = missing_keys,
    de = paste0(default_text, " ", missing_keys),
    en = paste0(default_text, " ", missing_keys),
    stringsAsFactors = FALSE
  )

  # Append
  translations <- rbind(translations, new_rows)

  write.csv(translations, csv_path, row.names = FALSE, fileEncoding = "UTF-8")

  message("Added keys: ", length(missing_keys))
  for (k in missing_keys) {
    message("  + ", k)
  }

  invisible(missing_keys)
}

# Auto-run when sourced directly
if (interactive() && !exists(".check_keys_sourced")) {
  .check_keys_sourced <- TRUE
  message("check_translation_keys() available. Call with: check_translation_keys()")
}

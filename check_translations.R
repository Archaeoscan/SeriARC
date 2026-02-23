lines <- readLines("E:/SeriARC/SeriARC_Main/translations/translations.csv", encoding = "UTF-8")
# Skip header
data_lines <- lines[-1]
result <- list()
for (line in data_lines) {
  parts <- strsplit(line, ";")[[1]]
  if (length(parts) >= 3) {
    key <- parts[1]
    de  <- parts[2]
    en  <- parts[3]
    if (nchar(de) > 15 && de == en) {
      result[[length(result)+1]] <- paste0(key, " | ", substr(de, 1, 100))
    }
  }
}
cat("=== DE == EN entries (>15 chars) ===\n")
cat(paste(result, collapse="\n"), "\n")
cat("\nTotal:", length(result), "\n")

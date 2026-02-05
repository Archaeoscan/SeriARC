# helpers/c14_curve_loader.R
# Robust IntCal20 loader: tries rcarbon -> Bchron[data] -> IntCal -> Bchron[extdata] -> CSV -> HTTP download
# Normalizes to CalBP, C14Age, Error and logs which source was used.

# --- Logging
curve_log <- function(...) cat("[IntCal20] ", sprintf(...), "\n")

# --- Normalisierung bleibt wie zuvor ---
.normalize_cols <- function(df) {
  stopifnot(is.data.frame(df), ncol(df) >= 3)
  nms <- tolower(names(df))
  pick <- function(cands, fallback_idx=NULL) {
    idx <- which(nms %in% cands)
    if (length(idx) > 0) return(df[[idx[1]]])
    if (!is.null(fallback_idx) && fallback_idx <= ncol(df)) return(df[[fallback_idx]])
    NULL
  }
  CalBP  <- pick(c("calbp","cal_bp","bp","yearbp"), 1)
  C14Age <- pick(c("c14age","c14_age","age","radiocarbonage","c14"), 2)
  Error  <- pick(c("error","sigma","se","sd","err"), 3)
  stopifnot(!is.null(CalBP), !is.null(C14Age), !is.null(Error))
  out <- data.frame(
    CalBP  = as.numeric(CalBP),
    C14Age = as.numeric(C14Age),
    Error  = as.numeric(Error),
    stringsAsFactors = FALSE
  )
  out <- out[is.finite(out$CalBP) & is.finite(out$C14Age) & is.finite(out$Error), , drop=FALSE]
  out <- out[order(out$CalBP), , drop=FALSE]
  stopifnot(nrow(out) > 0)
  out
}

# --- rcarbon (unchanged) ---
.read_rcarbon <- function() {
  if (!requireNamespace("rcarbon", quietly=TRUE)) return(NULL)
  ok <- try(utils::data("intcal20", package="rcarbon"), silent=TRUE)
  if (!inherits(ok,"try-error") && exists("intcal20")) {
    df <- get("intcal20", inherits=TRUE); rm(intcal20, envir=.GlobalEnv)
    curve_log("rcarbon::data('intcal20') ✓")
    return(.normalize_cols(df))
  }
  cf <- get0("ccurve", envir=asNamespace("rcarbon"), inherits=FALSE)
  if (is.function(cf)) {
    df <- try(rcarbon::ccurve("intcal20"), silent=TRUE)
    if (!inherits(df,"try-error")) {
      curve_log("rcarbon::ccurve('intcal20') ✓")
      return(.normalize_cols(df))
    }
  }
  NULL
}

# BChron functions no longer needed (using local file)
# .read_bchron_data() and .read_bchron_extdata() removed

# --- IntCal package (with bug fix) ---
.read_intcal_pkg <- function() {
  if (!requireNamespace("IntCal", quietly=TRUE)) return(NULL)
  ok <- try(utils::data("IntCal20", package="IntCal"), silent=TRUE)
  if (!inherits(ok,"try-error") && exists("IntCal20")) {  # && statt ||
    df <- get("IntCal20", inherits=TRUE)
    curve_log("IntCal::IntCal20 ✓")
    return(.normalize_cols(df))
  }
  NULL
}

.read_csv_fallback <- function() {
  fp <- file.path("inst","extdata","IntCal20.csv")
  if (file.exists(fp)) {
    df <- try(utils::read.csv(fp), silent=TRUE)
    if (!inherits(df,"try-error")) {
      curve_log("CSV fallback at %s ✓", fp)
      return(.normalize_cols(df))
    }
  }
  NULL
}

.try_download_to_csv <- function() {
  dir.create(file.path("inst","extdata"), recursive=TRUE, showWarnings=FALSE)
  dest <- file.path("inst","extdata","intcal20.14c")
  urls <- c(
    "https://www.radiocarbon.org/IntCal20/files/intcal20.14c",
    "https://www.radiocarbon.org/IntCal20/files/intcal20.14c.txt"
  )
  for (u in urls) {
    ok <- try(utils::download.file(u, destfile=dest, mode="wb", quiet=TRUE), silent=TRUE)
    if (!inherits(ok,"try-error") && file.exists(dest) && file.info(dest)$size > 0) {
      df <- try(utils::read.table(dest, header=TRUE), silent=TRUE)
      if (!inherits(df,"try-error") && nrow(df) > 0) {
        out <- .normalize_cols(df)
        utils::write.csv(out, file.path("inst","extdata","IntCal20.csv"), row.names=FALSE)
        curve_log("Downloaded & saved CSV fallback ✓")
        return(out)
      }
    }
  }
  NULL
}

# --- Local .14c file reader (ROBUST with data.table::fread) ---
.read_local_14c <- function() {
  # Look for intcal20.14c in current directory
  for (path in c("intcal20.14c", "./intcal20.14c", "data/intcal20.14c")) {
    if (file.exists(path)) {
      # Try data.table::fread only if available
      if (requireNamespace("data.table", quietly = TRUE)) {
        tryCatch({
          # ROBUST: Check fread() arguments (older versions don't support comment.char)
          # Strategy: Try with comment.char, on error -> without

          # Attempt 1: With comment.char (newer data.table versions)
          df <- tryCatch({
            data.table::fread(
              path,
              skip = 0,
              comment.char = "#",
              header = FALSE,
              data.table = FALSE,
              showProgress = FALSE,
              verbose = FALSE
            )
          }, error = function(e) {
            # Attempt 2: Without comment.char (older versions)
            if (grepl("unbenutztes Argument|unused argument", conditionMessage(e))) {
              curve_log("fread() comment.char not supported, using manual filtering")
              lines <- readLines(path, warn = FALSE)
              data_lines <- lines[!grepl("^\\s*#", lines) & nzchar(trimws(lines))]
              if (length(data_lines) > 0) {
                con <- textConnection(data_lines)
                on.exit(close(con), add = TRUE)
                data.table::fread(text = paste(data_lines, collapse = "\n"),
                                  header = FALSE, data.table = FALSE,
                                  showProgress = FALSE, verbose = FALSE)
              } else {
                NULL
              }
            } else {
              stop(e)  # Other error -> rethrow
            }
          })
          
          if (!is.null(df) && ncol(df) >= 3) {
            names(df) <- c("CalBP", "C14Age", "Error", "Delta14C", "Delta14C_Error")[seq_len(ncol(df))]
            df <- df[, 1:3, drop = FALSE]
            curve_log("Local .14c file %s ✓ (%d data points, fread)", path, nrow(df))
            return(.normalize_cols(df))
          }
        }, error = function(e) {
          curve_log("fread() failed for %s: %s - trying fallback", path, conditionMessage(e))
        })
      }
    }
  }
  
  # Fallback auf alte Methode
  .read_local_14c_fallback()
}

# Fallback-Methode (alte Implementierung)
.read_local_14c_fallback <- function() {
  # Look for intcal20.14c in current directory
  for (path in c("intcal20.14c", "./intcal20.14c", "data/intcal20.14c")) {
    if (file.exists(path)) {
      # Read the .14c file - skip comment lines starting with # or ##
      lines <- readLines(path, warn = FALSE)
      # Filter out comment lines and empty lines
      data_lines <- lines[!grepl("^\\s*#{1,2}", lines) & nzchar(trimws(lines))]
      
      if (length(data_lines) > 0) {
        # Parse the data lines - IntCal20 format: CalBP,C14Age,Sigma,Delta14C,Sigma
        tryCatch({
          con <- textConnection(data_lines)
          df <- read.table(con, header = FALSE, stringsAsFactors = FALSE)
          close(con)
          
          if (ncol(df) >= 3) {
            colnames(df) <- c("CalBP", "C14Age", "Error", "Delta14C", "Delta14C_Error")[seq_len(ncol(df))]
            curve_log("Local .14c file %s ✓ (%d data points, fallback)", path, nrow(df))
            # Only keep the first 3 columns (CalBP, C14Age, Error)
            df <- df[, 1:3, drop = FALSE]
            return(.normalize_cols(df))
          }
        }, error = function(e) {
          warning(sprintf("Failed to parse %s: %s", path, conditionMessage(e)))
        })
      }
    }
  }
  NULL
}

# --- Orchestrator (local file preferred) ---
ensure_intcal20_df <- function() {
  for (fn in list(.read_local_14c,      # PRIORITY 1: Local .14c file
                  .read_rcarbon,        # Fallback 1: rcarbon
                  .read_intcal_pkg,     # Fallback 2: IntCal package
                  .read_csv_fallback)) {# Fallback 3: CSV
    df <- fn()
    if (!is.null(df)) return(df)
  }
  df <- .try_download_to_csv()
  if (!is.null(df)) return(df)
  stop("IntCal20 curve not found. Please ensure intcal20.14c is in the project directory.")
}

get_curve <- function(bp_min, bp_max) {
  bp_min <- as.numeric(bp_min); bp_max <- as.numeric(bp_max)
  if (is.na(bp_min) || is.na(bp_max) || bp_min >= bp_max) stop("bad bp range")
  df <- ensure_intcal20_df()
  df <- df[df$CalBP >= bp_min & df$CalBP <= bp_max, , drop=FALSE]
  df <- df[order(df$CalBP), , drop=FALSE]
  stopifnot(nrow(df) > 0)
  list(years = as.numeric(df$CalBP),
       rc_age = as.numeric(df$C14Age),
       rc_sig = as.numeric(df$Error))
}

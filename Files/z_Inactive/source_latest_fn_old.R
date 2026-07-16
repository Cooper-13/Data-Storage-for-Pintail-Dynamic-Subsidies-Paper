source_latest_fn <- function(patterns, dir = ".", use_regex = FALSE,
                             stop_on_missing = TRUE, verbose = TRUE,
                             recursive = FALSE, ignore.case = FALSE, ...) {
  stopifnot(is.character(patterns), length(patterns) >= 1)
  
  get_version <- function(p) {
    stem <- tools::file_path_sans_ext(basename(p))
    m <- regexpr("(?:[._-]?[vV])?(\\d+(?:\\.\\d+)*)$", stem, perl = TRUE)
    if (m[1] == -1) return(NA_character_)
    full <- regmatches(stem, m)
    sub("^(?:[._-]?[vV])?(\\d+(?:\\.\\d+)*)$", "\\1", full, perl = TRUE)
  }
  
  choose_one <- function(pattern) {
    rx <- if (use_regex) pattern else utils::glob2rx(pattern)
    files <- list.files(dir, pattern = rx, full.names = TRUE,
                        recursive = recursive, ignore.case = ignore.case)
    if (length(files) == 0) {
      if (stop_on_missing) {
        stop(sprintf("No files match pattern: %s", pattern), call. = FALSE)
      } else {
        warning(sprintf("No files match pattern: %s", pattern), call. = FALSE)
        return(NA_character_)
      }
    }
    
    vers_str <- vapply(files, get_version, character(1))
    has_ver  <- !is.na(vers_str)
    
    if (any(has_ver)) {
      # Changed line: use numeric_version from base
      vers <- base::numeric_version(vers_str[has_ver])
      max_idx <- which(vers == max(vers))
      cand <- files[has_ver][max_idx]
      if (length(cand) > 1L) {
        mt <- file.info(cand)$mtime
        cand <- cand[which.max(mt)]
      }
      chosen <- cand
    } else {
      mt <- file.info(files)$mtime
      chosen <- files[which.max(mt)]
    }
    
    if (verbose) message("Sourcing: ", chosen, "  [pattern: ", pattern, "]")
    source(chosen, ...)
    chosen
  }
  
  out <- vapply(patterns, choose_one, character(1))
  names(out) <- patterns
  invisible(out)
}
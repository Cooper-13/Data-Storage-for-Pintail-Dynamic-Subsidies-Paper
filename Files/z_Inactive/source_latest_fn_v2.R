# Source the latest-matching file for each of multiple patterns, with optional printing
# - patterns: character vector of patterns (glob by default, or regex if use_regex = TRUE)
# - print_info: logical; if TRUE, print "filename | first line" for each sourced file
# - dir: directory to search (non-recursive by default)
# - use_regex: interpret patterns as regex when TRUE; otherwise as glob
# - recursive: search subdirectories when TRUE
# - ignore.case: case-insensitive matching when TRUE
# - ...: passed to base::source (e.g., local = TRUE)
source_latest_fn <- function(patterns, print_info = FALSE,
                             dir = ".", use_regex = FALSE,
                             recursive = FALSE, ignore.case = FALSE,
                             verbose = TRUE, stop_on_missing = TRUE, ...) {
  stopifnot(is.character(patterns), length(patterns) >= 1)
  
  # Extract trailing version from basename (before extension), e.g., _1, _1.2.3, -v2.10, V3
  get_version <- function(p) {
    stem <- tools::file_path_sans_ext(basename(p))
    m <- regexpr("(?:[._-]?[vV])?(\\d+(?:\\.\\d+)*)$", stem, perl = TRUE)
    if (m[1] == -1) return(NA_character_)
    full <- regmatches(stem, m)
    sub("^(?:[._-]?[vV])?(\\d+(?:\\.\\d+)*)$", "\\1", full, perl = TRUE)
  }
  
  # Helper to list matches for a single pattern with current settings
  list_matches <- function(pattern) {
    rx <- if (use_regex) pattern else utils::glob2rx(pattern)
    list.files(dir, pattern = rx, full.names = TRUE,
               recursive = recursive, ignore.case = ignore.case)
  }
  
  # Precheck: collect which patterns have at least one match
  matches_list <- lapply(patterns, list_matches)
  has_match <- vapply(matches_list, function(x) length(x) > 0L, logical(1))
  
  if (!all(has_match)) {
    missing <- paste(patterns[!has_match], collapse = ", ")
    if (isTRUE(stop_on_missing)) {
      stop(sprintf("No files match the following pattern(s): %s", missing), call. = FALSE)
    } else {
      warning(sprintf("No files match the following pattern(s): %s", missing), call. = FALSE)
      # Filter out missing patterns for subsequent steps
      patterns <- patterns[has_match]
      matches_list <- matches_list[has_match]
      if (length(patterns) == 0L) return(invisible(character()))
    }
  }
  
  # Choose one "latest" file per pattern
  choose_one_from <- function(files) {
    vers_str <- vapply(files, get_version, character(1))
    has_ver  <- !is.na(vers_str)
    
    if (any(has_ver)) {
      vers <- base::numeric_version(vers_str[has_ver])
      max_idx <- which(vers == max(vers))
      cand <- files[has_ver][max_idx]
      if (length(cand) > 1L) {
        mt <- file.info(cand)$mtime
        cand <- cand[which.max(mt)]
      }
      cand
    } else {
      mt <- file.info(files)$mtime
      files[which.max(mt)]
    }
  }
  
  chosen <- mapply(function(pat, files) {
    ch <- choose_one_from(files)
    if (verbose) message("Sourcing: ", ch, "  [pattern: ", pat, "]")
    source(ch, ...)
    ch
  }, patterns, matches_list, SIMPLIFY = TRUE, USE.NAMES = FALSE)
  
  names(chosen) <- patterns
  
  if (isTRUE(print_info)) {
    for (p in chosen) {
      first_line <- tryCatch(readLines(p, n = 1, warn = FALSE), error = function(e) "")
      cat(sprintf("%s | %s\n", basename(p), first_line))
    }
  }
  
  invisible(chosen)
}
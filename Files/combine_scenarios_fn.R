# v2
read_one_fn <- function(f) {
  dat <- readr::read_csv(f, show_col_types = FALSE)
  if (!"PintailRegion" %in% names(dat) || !"pintail_birders_k" %in% names(dat)) {
    stop("Required columns missing in file: ", f)
  }
  scenario <- sub("_wtp_out\\.csv$", "", basename(f))
  dplyr::transmute(
    dat,
    scenarioName = scenario,
    PintailRegion = .data[["PintailRegion"]],
    pintail_birders_k = .data[["pintail_birders_k"]]
  )
}

# v3
# v4
combine_scenarios_fn <- function(path = ".", pattern = "_wtp_out\\.csv$") {
  files <- list.files(path = path, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) stop("No files found matching pattern: ", pattern)
  
  long <- purrr::map_dfr(files, read_one_fn)
  
  region_levels <- c(
    "Alaska breeding",
    "Southern breeding",
    "Northern breeding",
    "Western wintering",
    "Central wintering"
  )
  long <- dplyr::mutate(
    long,
    PintailRegion = factor(.data$PintailRegion, levels = region_levels, ordered = TRUE)
  )
  
  scens <- sort(unique(long$scenarioName))
  
  wide <- tidyr::pivot_wider(
    long,
    id_cols = PintailRegion,
    names_from = scenarioName,
    values_from = pintail_birders_k,
    values_fill = NA_real_,
    values_fn = list(pintail_birders_k = ~ dplyr::first(.x))
  )
  
  wide <- dplyr::arrange(wide, PintailRegion)
  wide <- wide[, c("PintailRegion", scens[scens %in% names(wide)]), drop = FALSE]
  wide
}


# concise usage
combined_pintail_birders_k_df <- combine_scenarios_fn()
write.csv(combined_pintail_birders_k_df,"combined_pintail_birders_k_df.csv")
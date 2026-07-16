# ecosystem_service_flows*.r prepared by B. Mattsson and J. Windt
  # calculations of subsidies and ES flows based on computational formulas provided by Darius J. Semmens

# front matter ----
## If not Rstudio project, then set directory ----
#setwd("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows")


## Optional: load environment----
#load("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_service_flows.RData")
#save.image("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_service_flows.RData")


## Load packages----
x = c("dplyr", "tidyr", "ggplot2", "purrr", "readr",  "tibble", "scales", "XLConnect") # , "XLConnect", "openxlsx"
# install.packages(x)
invisible(lapply(x, library, character.only=TRUE))
rm(x)

# install and load previous version of XLConnect to avoid compatibility issues with Java
#require(devtools) # install.packages("devtools")
#pak::pak("XLConnect", version = "1.0.1", repos = "http://cran.us.r-project.org")

# Function to source the latest-matching file for each of multiple patterns, with optional printing ####
## front matter ####
# - patterns: character vector of patterns (glob by default, or regex if use_regex = TRUE)
# - print_info: logical; if TRUE, print "filename | first line" for each sourced file
# - dir: directory to search (non-recursive by default)
# - use_regex: interpret patterns as regex when TRUE; otherwise as glob
# - recursive: search subdirectories when TRUE
# - ignore.case: case-insensitive matching when TRUE
# - ...: passed to base::source (e.g., local = TRUE)
## define source_latest_fn ####
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

## source required functions and optionally print "filename | first line" ----
source_latest_fn(
  patterns = c("estimate_pintail_birders_fn*.R", "viewingWTP_fn*","helper_functions_viewingEcon*", 
               "population_model_functions*"), 
  print_info = TRUE
)
  # ,"PintailSimulation*"

# Define scenarios & regions for calculating Ds ####

scenarios <- c("1_Conservative", "2_Hunting", "3_Grassland", "4_Integrated") # 
regions <- c("Alaska breeding","Southern breeding","Northern breeding",
             "Western wintering","Central wintering")

# Loop through scenarios ####
 # to calculate ES flows between regions and subsidies including Di and Vi by region
  results_list <- vector("list", length(scenarios)) # Container for results
  # scenarioName <- "1_Conservative"; i <- 1
#for(i in seq_along(scenarios)){  
for(scenarioName in scenarios){ 
  #scenarioName <- scenarios[i]  # current scenario ####
    
    
  
  # Get Di & Vi by respectively sourcing latest versions of PintailSimulation*.r and ViewingEconomics*.r ####
    # optionally print_info "filename | first line" and print_objects
  source_latest_fn(
    patterns = c("PintailSimulation*", "ViewingEconomics*"), 
    print_info = FALSE
  )
  
  # Calculate ES flows between & subsidies for each region ----
  
  # total annual viewing value provided by pintails in region i under given scenarioName (millions 2016 USD) 
  # wtp_out generated by ViewingEconomics*.r
  Vi <- wtp_out$total_WTP_M 
  names(Vi) <- wtp_out$PintailRegion
  
  Di <- c(DS) # DS is from PintailSimulation*.r; str(DS)
  names(Di) <- c("Alaska breeding", "Southern breeding", "Northern breeding", "Western wintering", "Central wintering")
  
  MOi <- (sum(Vi)-Vi)*Di
  #outgoing migration support provided by location i to other locations
  
  MIi <- Vi*(1-Di)
  #incoming migration support received at location i from other locations
  
  MLi <- Vi*Di
  #locally received migration support from location i to location i
  
  Yi <- sum(Vi)*Di-Vi
  #spatial subsidy (net benefit flow to/from each location), or Yi= MOi-MIi
  
  YLi <- ifelse(Yi<0, Vi+Yi, Vi)
  #net local flow (net benefit flow from ecosystems to people within each region)
  
  ES_df <- data.frame(Di,Vi)
  
  # Assemble a data.frame for this scenario ####
  results_list[[match(scenarioName,scenarios)]] <- data.frame(
    scenarioName  = rep(scenarioName, length(regions)),
    region    = regions,
    DS        = as.numeric(DS),
    econValue = as.numeric(Vi),
    subsidy   = as.numeric(Yi),
    stringsAsFactors = FALSE
  ) # end data.frame
} # end for(scenarioName in scenarios)

# Combine & visualize results ####
results_long <- do.call(rbind, results_list)

# Plot: dot plot of subsidy by scenarioName with unique symbol per region
results_long$scenarioName <- factor(results_long$scenarioName, levels = scenarios)

p_subsidy <- ggplot2::ggplot(results_long, ggplot2::aes(x = scenarioName, y = subsidy, shape = region)) +
  ggplot2::geom_point(size = 3) +
  ggplot2::scale_shape_manual(values = c(16, 17, 15, 3, 4)) +
  ggplot2::labs(x = "Scenario", y = "Region-specific subsidy (millions 2016 USD)",
                shape = "Region") +
  ggplot2::theme_minimal(base_size = 12)

print(p_subsidy)
# Save plot
ggplot2::ggsave("subsidy_dotplot.png", p_subsidy, width = 7, height = 4, dpi = 300)
ggplot2::ggsave("subsidy_dotplot.pdf", p_subsidy, width = 7, height = 4)

# Build wide table: one row per region; per-scenarioName columns for DS, econValue, subsidy
results_wide <- results_long |>
  tidyr::pivot_longer(cols = c(DS, econValue, subsidy), names_to = "metric", values_to = "value") |>
  dplyr::mutate(colname = paste(scenarioName, metric, sep = "__")) |>
  dplyr::select(region, colname, value) |>
  tidyr::pivot_wider(names_from = colname, values_from = value) |>
  dplyr::arrange(factor(region, levels = regions))

# Number formatter to keep consistent digits in the table
fmt_num <- function(x, digits = 3) formatC(x, format = "f", digits = digits)

# Create a formatted (character) version of the main table to avoid type conflicts when binding summary rows
results_wide_fmt <- results_wide |>
  dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~fmt_num(.x, 3))) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# Range row (min–max across regions) for every numeric column
range_row <- results_wide |>
  dplyr::select(-region) |>
  lapply(function(x) {
    rng <- range(x, na.rm = TRUE)
    paste0(fmt_num(rng[1], 3), "-", fmt_num(rng[2], 3))
  }) |>
  tibble::as_tibble() |>
  dplyr::mutate(region = "Range") |>
  dplyr::relocate(region) |>
  # Ensure same column order as the formatted table and coerce to character
  dplyr::relocate(dplyr::all_of(names(results_wide_fmt))) |>
  dplyr::mutate(dplyr::across(dplyr::everything(), as.character))

# Total econValue row: sum across regions for each scenarioName's econValue columns
econ_cols <- grep("__econValue$", names(results_wide_fmt), value = TRUE)
total_vals <- colSums(dplyr::select(results_wide, dplyr::all_of(econ_cols)), na.rm = TRUE)

# Initialize a list of NA characters for all columns, then fill totals for econValue columns
total_row <- as.list(rep(NA_character_, ncol(results_wide_fmt)))
names(total_row) <- names(results_wide_fmt)
total_row[["region"]] <- "Total econValue"
for (cn in econ_cols) {
  total_row[[cn]] <- fmt_num(total_vals[[cn]], 3)
}
total_row <- tibble::as_tibble(total_row)

# Bind main table (formatted), range row, and total econValue row
results_wide_for_table <- dplyr::bind_rows(
  results_wide_fmt,
  range_row,
  total_row
)

# Write CSV suitable for journal table (Geography & Sustainability)
readr::write_csv(results_wide_for_table, "ES_table_GeogSust.csv")

# Ecosystem service flows between regions ####
regions <- c("Alaska breeding","Southern breeding","Northern breeding",
             "Western wintering","Central wintering")
flow_matrix <- outer(Di, Vi, `*`)
dimnames(flow_matrix) <- list(from = paste("fr",regions,sep="_"), to = paste("to",regions,sep="_"))

write.csv(flow_matrix, "flow_matrix.csv")
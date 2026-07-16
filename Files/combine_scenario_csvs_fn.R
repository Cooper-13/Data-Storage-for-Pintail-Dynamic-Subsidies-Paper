
# v5
combine_scenario_csvs_fn <- function(root = ".") {
  if (!requireNamespace("data.table", quietly = TRUE)) stop("Please install 'data.table'")
  files <- list.files(root, pattern = utils::glob2rx("season_vars_df_*.csv"),
                      recursive = TRUE, full.names = TRUE)
  if (!length(files)) return(data.frame())
  dts <- lapply(files, function(f) {
    dt <- data.table::fread(f)
    nm <- tools::file_path_sans_ext(basename(f))
    nm <- sub("_$", "", nm)  # drop the trailing underscore
    parts <- strsplit(nm, "_", fixed = TRUE)[[1]]
    season <- suppressWarnings(as.integer(parts[4]))
    scenario <- parts[5]
    dt[, `:=`(season = season, scenario = scenario, file = f)]
  })
  out <- data.table::rbindlist(dts, fill = TRUE)
  out[]
}
#end of code
abundance_by_scn_season_region_df <- combine_scenario_csvs_fn()
print(abundance_by_scn_season_region_df)
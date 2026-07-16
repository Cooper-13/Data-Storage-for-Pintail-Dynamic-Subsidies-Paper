# v12 — drop unmapped Canadian provinces with warning (configurable)
estimate_pintail_birders_fn <- function(
    birders_US2011_tbl,
    birdersUS2006_tbl,
    birding_CN_tbl,
    censusCanada_tbl,
    pintail_tbl,
    nd_pop_2006,
    nd_pop_2011,
    canada_province_col = NULL,   # e.g., "Province", "Geographic_name", "NAME"
    canada_prop_col     = NULL,   # e.g., "Birding_prop", "prop", "proportion"
    canada_drop_unmapped = TRUE   # if TRUE, warn and drop unmapped provinces; if FALSE, stop
) {
  normalize_province <- function(x) {
    x0 <- trimws(as.character(x))
    x0 <- gsub("\\+", "", x0)  # strip '+', e.g., "NU+"
    x0 <- gsub("^Newfoundland\\s*&\\s*Labrador$", "Newfoundland and Labrador", x0, ignore.case = TRUE)
    x0 <- gsub("^Prince\\s*Edward\\s*Island$", "Prince Edward Island", x0, ignore.case = TRUE)
    x0 <- gsub("^Northwest\\s*Territories$", "Northwest Territories", x0, ignore.case = TRUE)
    xu <- toupper(x0)
    abbr_map <- c(
      "AB"="Alberta","BC"="British Columbia","MB"="Manitoba","NB"="New Brunswick",
      "NL"="Newfoundland and Labrador","NS"="Nova Scotia","NT"="Northwest Territories",
      "NWT"="Northwest Territories","NU"="Nunavut","ON"="Ontario","PE"="Prince Edward Island",
      "PEI"="Prince Edward Island","QC"="Quebec","PQ"="Quebec","SK"="Saskatchewan",
      "YT"="Yukon","YK"="Yukon"
    )
    out <- x0
    idx <- match(xu, names(abbr_map))
    out[!is.na(idx)] <- abbr_map[idx[!is.na(idx)]]
    full_names <- c(
      "Alberta","British Columbia","Manitoba","New Brunswick","Newfoundland and Labrador",
      "Nova Scotia","Northwest Territories","Nunavut","Ontario","Prince Edward Island",
      "Quebec","Saskatchewan","Yukon"
    )
    for (nm in full_names) out[tolower(out) == tolower(nm)] <- nm
    out[toupper(out) %in% c("CANADA","ALL CANADA","TOTAL CANADA")] <- NA_character_
    out
  }
  
  # 1) U.S. ND override
  us_tbl <- birders_US2011_tbl %>%
    dplyr::mutate(PintailRegion = dplyr::case_when(
      State == "North Dakota" ~ "Southern breeding",
      TRUE ~ PintailRegion
    ))
  
  state_region_map <- us_tbl %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::select(State, PintailRegion) %>%
    dplyr::distinct()
  
  us_region_level <- us_tbl %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(pintail_birders_k = sum(Total, na.rm = TRUE), .groups = "drop")
  
  # 2) Canada: detect columns
  cn_cols <- names(birding_CN_tbl)
  cn_cols_lower <- tolower(cn_cols)
  
  if (is.null(canada_province_col)) {
    prov_aliases <- c("province","geographic_name","provinceterritory","province_territory",
                      "province.territory","name","jurisdiction")
    hit <- which(cn_cols_lower %in% prov_aliases)
    if (length(hit) == 0) stop(sprintf(
      "Provide canada_province_col. Available: %s", paste(cn_cols, collapse = ", ")
    ))
    canada_province_col <- cn_cols[hit[1]]
    message(sprintf("Auto-detected Canadian province column: '%s'", canada_province_col))
  } else if (!canada_province_col %in% cn_cols) {
    stop(sprintf("canada_province_col '%s' not found. Available: %s",
                 canada_province_col, paste(cn_cols, collapse = ", ")))
  }
  
  if (is.null(canada_prop_col)) {
    prop_aliases <- c("prop","proportion","share","pct","percent",
                      "pct_birders","prop_birders","birding_prop")
    hit <- which(cn_cols_lower %in% prop_aliases)
    if (length(hit) == 0) hit <- grep("prop", cn_cols_lower, fixed = TRUE)
    if (length(hit) == 0) stop(sprintf(
      "Provide canada_prop_col. Available: %s", paste(cn_cols, collapse = ", ")
    ))
    canada_prop_col <- cn_cols[hit[1]]
    message(sprintf("Auto-detected Canadian proportion column: '%s'", canada_prop_col))
  } else if (!canada_prop_col %in% cn_cols) {
    stop(sprintf("canada_prop_col '%s' not found. Available: %s",
                 canada_prop_col, paste(cn_cols, collapse = ", ")))
  }
  
  # 3) Canada: build census map and normalize names
  cn_map <- censusCanada_tbl %>%
    dplyr::transmute(
      Province     = normalize_province(Geographic_name),
      Population_2011 = as.numeric(Population_2011),
      PintailRegion   = Pintail_region
    ) %>%
    dplyr::filter(!is.na(Province)) %>%
    dplyr::distinct(Province, .keep_all = TRUE)
  
  # Standardize CN table and normalize names; parse proportions
  cn_tbl_std <- birding_CN_tbl %>%
    dplyr::rename(Province_raw = dplyr::all_of(canada_province_col)) %>%
    dplyr::mutate(
      Province = normalize_province(Province_raw),
      prop_raw = !!rlang::sym(canada_prop_col)
    ) %>%
    dplyr::filter(!is.na(Province))
  
  if (!is.numeric(cn_tbl_std$prop_raw)) {
    cn_tbl_std$prop_raw <- suppressWarnings(readr::parse_number(cn_tbl_std$prop_raw))
  }
  if (!is.numeric(cn_tbl_std$prop_raw)) {
    stop(sprintf("Column '%s' could not be coerced to numeric proportions.", canada_prop_col))
  }
  
  # Proportions: use 0–1 as-is; if any >1, treat as percentages
  if (any(cn_tbl_std$prop_raw > 1, na.rm = TRUE)) {
    cn_tbl_std <- cn_tbl_std %>% dplyr::mutate(prop = prop_raw / 100)
  } else {
    cn_tbl_std <- cn_tbl_std %>% dplyr::mutate(prop = prop_raw)
  }
  
  # Average duplicate province rows if present
  cn_prop_by_prov <- cn_tbl_std %>%
    dplyr::group_by(Province) %>%
    dplyr::summarise(prop = mean(prop, na.rm = TRUE), .groups = "drop")
  
  # Join to census mapping
  cn_joined <- cn_prop_by_prov %>%
    dplyr::left_join(cn_map %>% dplyr::select(Province, Population_2011, PintailRegion),
                     by = "Province")
  
  # Handle unmapped provinces
  unmapped <- cn_joined %>% dplyr::filter(is.na(PintailRegion)) %>% dplyr::pull(Province)
  if (length(unmapped) > 0) {
    msg <- sprintf(
      "Dropping Canadian provinces/territories without PintailRegion mapping: %s",
      paste(unique(unmapped), collapse = ", ")
    )
    if (isTRUE(canada_drop_unmapped)) {
      warning(msg)
      cn_joined <- cn_joined %>% dplyr::filter(!is.na(PintailRegion))
    } else {
      stop(gsub("^Dropping ", "", msg))
    }
  }
  
  # Ensure population present
  missing_pop <- cn_joined %>% dplyr::filter(is.na(Population_2011)) %>% dplyr::pull(Province)
  if (length(missing_pop) > 0) {
    stop(sprintf(
      "Missing Population_2011 in censusCanada_tbl for: %s",
      paste(unique(missing_pop), collapse = ", ")
    ))
  }
  
  # Compute provincial birders and convert to thousands
  cn_joined <- cn_joined %>%
    dplyr::mutate(
      birders_count = prop * Population_2011,
      total_k = birders_count / 1000
    )
  
  cn_region_level <- cn_joined %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(pintail_birders_k = sum(total_k, na.rm = TRUE), .groups = "drop")
  
  # 4) Combine U.S. and Canada; ensure canonical regions
  region_level <- dplyr::bind_rows(us_region_level, cn_region_level) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(pintail_birders_k = sum(pintail_birders_k, na.rm = TRUE), .groups = "drop")
  
  expected_regions <- c("Southern breeding","Northern breeding","Alaska breeding",
                        "Western wintering","Central wintering")
  
  region_level <- tibble::tibble(PintailRegion = expected_regions) %>%
    dplyr::left_join(region_level, by = "PintailRegion") %>%
    dplyr::mutate(pintail_birders_k = dplyr::coalesce(pintail_birders_k, 0))
  
  list(
    region_level = region_level,
    state_region_map = state_region_map,
    canada_detail = cn_joined %>%
      dplyr::select(Province, PintailRegion, Population_2011, prop, birders_count, total_k)
  )
}
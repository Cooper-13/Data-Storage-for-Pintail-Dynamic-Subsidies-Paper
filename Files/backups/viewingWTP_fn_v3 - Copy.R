# v3
viewingWTP_fn <- function(
    birders_US2011_tbl,
    birdersUS2006_tbl,
    birding_CN_tbl,
    censusCanada_tbl,
    pintail_tbl,
    nd_pop_2006,
    nd_pop_2011,
    pintails_abundance,  # numeric scalar or named vector keyed by PintailRegion
    WTP_tbl              # must include: state (USPS), trp_num, max_WTP_BM, trips_dbl_birds
) {
  # 1) Pintail birders by region (k)
  pb <- estimate_pintail_birders_fn(
    birders_US2011_tbl = birders_US2011_tbl,
    birdersUS2006_tbl  = birdersUS2006_tbl,
    birding_CN_tbl     = birding_CN_tbl,
    censusCanada_tbl   = censusCanada_tbl,
    pintail_tbl        = pintail_tbl,
    nd_pop_2006        = nd_pop_2006,
    nd_pop_2011        = nd_pop_2011,
    canada_province_col = "Province",
    canada_prop_col     = "Birding_prop"
  )$region_level %>%
    dplyr::select(PintailRegion, pintail_birders_k)
  
  # 2) Map states to PintailRegion with ND override and compute WTP totals
  state_region_map <- .make_state_region_map(birders_US2011_tbl)
  
  wtp_prepped <- WTP_tbl %>%
    dplyr::mutate(
      State = .abbr_to_name(state),
      WTP_tot_abund1 = max_WTP_BM * trp_num,
      WTP_tot_abund2 = max_WTP_BM * trips_dbl_birds
    ) %>%
    dplyr::left_join(state_region_map, by = "State")
  
  # 3) Compute regional medians (anchors)
  region_wtp_medians <- wtp_prepped %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(
      WTP_med_abund1 = stats::median(WTP_tot_abund1, na.rm = TRUE),
      WTP_med_abund2 = stats::median(WTP_tot_abund2, na.rm = TRUE),
      .groups = "drop"
    )
  
  # 4) Impute missing regions:
  #    - Northern breeding := median across ND and SD
  #    - Alaska breeding   := median across CA and OR
  get_state_medians <- function(states, col) {
    wtp_prepped %>%
      dplyr::filter(State %in% states) %>%
      dplyr::pull({{ col }}) %>%
      stats::median(na.rm = TRUE)
  }
  
  nd_sd_a1 <- get_state_medians(c("North Dakota", "South Dakota"), WTP_tot_abund1)
  nd_sd_a2 <- get_state_medians(c("North Dakota", "South Dakota"), WTP_tot_abund2)
  ca_or_a1 <- get_state_medians(c("California", "Oregon"), WTP_tot_abund1)
  ca_or_a2 <- get_state_medians(c("California", "Oregon"), WTP_tot_abund2)
  
  # Ensure rows exist for all regions we care about so we can fill
  all_regions <- unique(pb$PintailRegion)
  region_wtp_complete <- tibble::tibble(PintailRegion = all_regions) %>%
    dplyr::left_join(region_wtp_medians, by = "PintailRegion") %>%
    dplyr::mutate(
      WTP_med_abund1 = dplyr::case_when(
        PintailRegion == "Northern breeding" & (is.na(WTP_med_abund1) | is.infinite(WTP_med_abund1)) ~ nd_sd_a1,
        PintailRegion == "Alaska breeding"   & (is.na(WTP_med_abund1) | is.infinite(WTP_med_abund1)) ~ ca_or_a1,
        TRUE ~ WTP_med_abund1
      ),
      WTP_med_abund2 = dplyr::case_when(
        PintailRegion == "Northern breeding" & (is.na(WTP_med_abund2) | is.infinite(WTP_med_abund2)) ~ nd_sd_a2,
        PintailRegion == "Alaska breeding"   & (is.na(WTP_med_abund2) | is.infinite(WTP_med_abund2)) ~ ca_or_a2,
        TRUE ~ WTP_med_abund2
      )
    )
  
  # 5) Abundance input (scalar or named vector)
  if (length(pintails_abundance) == 1 && is.numeric(pintails_abundance)) {
    abund_tbl <- pb %>% dplyr::mutate(Abundance = pintails_abundance)
  } else if (is.numeric(pintails_abundance) && !is.null(names(pintails_abundance))) {
    abund_tbl <- tibble::tibble(PintailRegion = names(pintails_abundance),
                                Abundance = as.numeric(pintails_abundance)) %>%
      dplyr::right_join(pb, by = "PintailRegion")
  } else {
    stop("pintails_abundance must be a numeric scalar or a named numeric vector with names matching PintailRegion.")
  }
  
  # 6) Compute per-capita WTP at abundance A using linear interpolation between medians
  out <- abund_tbl %>%
    dplyr::left_join(region_wtp_complete, by = "PintailRegion") %>%
    dplyr::mutate(
      WTP_per_capita = WTP_med_abund1 + (WTP_med_abund2 - WTP_med_abund1) * (Abundance - 1),
      total_WTP_M = (pintail_birders_k * 1000 * WTP_per_capita) / 1e6
    ) %>%
    dplyr::select(PintailRegion, pintail_birders_k, Abundance,
                  WTP_med_abund1, WTP_med_abund2, WTP_per_capita, total_WTP_M)
  
  # 7) Inform about any remaining missing anchors (should be none after imputation)
  missing_wtp <- out %>% dplyr::filter(is.na(WTP_med_abund1) | is.na(WTP_med_abund2))
  if (nrow(missing_wtp) > 0) {
    warning(sprintf("Still missing WTP medians for: %s",
                    paste(unique(missing_wtp$PintailRegion), collapse = ", ")))
  }
  
  out
}

# Helpers (define once)
.make_state_region_map <- function(birders_US2011_tbl) {
  birders_US2011_tbl %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::select(State, PintailRegion) %>%
    dplyr::distinct() %>%
    dplyr::filter(State != "North Dakota") %>%
    dplyr::bind_rows(tibble::tibble(
      State = "North Dakota",
      PintailRegion = "Southern breeding"
    ))
}

.abbr_to_name <- function(x) {
  idx <- match(x, state.abb)
  out <- x
  out[!is.na(idx)] <- state.name[idx[!is.na(idx)]]
  out
}
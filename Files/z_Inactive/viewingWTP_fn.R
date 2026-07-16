# v1
viewingWTP_fn <- function(
    birders_US2011_tbl,
    birdersUS2006_tbl,
    birding_CN_tbl,
    censusCanada_tbl,
    pintail_tbl,
    flyway_tbl,
    nd_pop_2006,
    nd_pop_2011,
    pintails_abundance  # numeric scalar or named numeric vector keyed by PintailRegion
) {
  # 1) Get pintail-birder counts by region (k)
  pb <- estimate_pintail_birders_fn(
    birders_US2011_tbl = birders_US2011_tbl,
    birdersUS2006_tbl  = birdersUS2006_tbl,
    birding_CN_tbl     = birding_CN_tbl,
    censusCanada_tbl   = censusCanada_tbl,
    pintail_tbl        = pintail_tbl,
    nd_pop_2006        = nd_pop_2006,
    nd_pop_2011        = nd_pop_2011
  )$region_level %>%
    dplyr::select(PintailRegion, pintail_birders_k)
  
  # 2) Build a simple linear WTP function from flyway_tbl using the two abundance points
  wtpfly <- flyway_tbl %>%
    dplyr::mutate(WTP = Trips_n * Expenses) %>%
    dplyr::select(Flyway, Abundance, WTP)
  
  wtp1 <- wtpfly %>% dplyr::filter(Abundance == 1) %>% dplyr::summarise(WTP = mean(WTP, na.rm = TRUE)) %>% dplyr::pull(WTP)
  wtp2 <- wtpfly %>% dplyr::filter(Abundance == 2) %>% dplyr::summarise(WTP = mean(WTP, na.rm = TRUE)) %>% dplyr::pull(WTP)
  
  if (length(wtp1) == 0 || length(wtp2) == 0 || is.na(wtp1) || is.na(wtp2)) {
    stop("flyway_tbl must contain WTP for Abundance = 1 and 2.")
  }
  
  # Linear interpolation/extrapolation for any abundance A:
  # WTP(A) = wtp1 + (wtp2 - wtp1) * (A - 1)
  wtp_of_A <- function(A) wtp1 + (wtp2 - wtp1) * (A - 1)
  
  # 3) Resolve abundance input (scalar or named vector by region)
  if (length(pintails_abundance) == 1 && is.numeric(pintails_abundance)) {
    abund_tbl <- pb %>% dplyr::mutate(Abundance = pintails_abundance)
  } else if (is.numeric(pintails_abundance) && !is.null(names(pintails_abundance))) {
    abund_tbl <- tibble::tibble(PintailRegion = names(pintails_abundance),
                                Abundance = as.numeric(pintails_abundance)) %>%
      dplyr::right_join(pb, by = "PintailRegion") %>%
      dplyr::mutate(Abundance = ifelse(is.na(Abundance), NA_real_, Abundance))
  } else {
    stop("pintails_abundance must be a numeric scalar or a named numeric vector with names matching PintailRegion.")
  }

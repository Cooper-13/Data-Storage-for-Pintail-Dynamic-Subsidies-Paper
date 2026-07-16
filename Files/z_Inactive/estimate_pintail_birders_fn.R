# v1
estimate_pintail_birders_fn <- function(
    birders_US2011_tbl,
    birdersUS2006_tbl,
    birding_CN_tbl,
    censusCanada_tbl,
    pintail_tbl,
    nd_pop_2006,  # numeric
    nd_pop_2011   # numeric
) {
  us2011 <- birders_US2011_tbl %>%
    dplyr::mutate(
      Nonresidents = dplyr::if_else(is.na(Nonresidents), 1 - Residents, Nonresidents)
    )
  
  nd2006 <- birdersUS2006_tbl %>%
    dplyr::filter(State == "North Dakota") %>%
    dplyr::select(State, Total_Birders, Pct_Residents) %>%
    dplyr::distinct()
  
  if (nrow(nd2006) != 1) {
    stop("North Dakota row not found uniquely in 2006 table.")
  }
  if (is.na(nd_pop_2006) || is.na(nd_pop_2011) || nd_pop_2006 <= 0) {
    stop("Provide valid nd_pop_2006 and nd_pop_2011 values (positive numerics).")
  }
  
  nd_growth <- nd_pop_2011 / nd_pop_2006
  nd2011_est <- tibble::tibble(
    State = "North Dakota",
    Total = nd2006$Total_Birders * nd_growth,
    Residents = nd2006$Pct_Residents,
    Nonresidents = 1 - nd2006$Pct_Residents,
    SmallSample = NA_integer_,
    Flyway = "Central",
    PintailRegion = "Southern breeding"
  )
  
  us_places <- us2011 %>%
    dplyr::filter(!is.na(PintailRegion)) %>%
    dplyr::select(Place = State, PintailRegion, birders_k = Total) %>%
    dplyr::bind_rows(nd2011_est %>% dplyr::transmute(Place = State, PintailRegion, birders_k = Total))
  
  birding_CN_clean <- birding_CN_tbl %>%
    dplyr::mutate(Region = dplyr::if_else(Region == "NU+", "NU", Region))
  
  name_map <- tibble::tibble(
    Geographic_name = censusCanada_tbl$Geographic_name,
    Region = c("Yukon" = "YT",
               "Northwest Territories" = "NT",
               "Manitoba" = "MB",
               "Saskatchewan" = "SK",
               "Alberta" = "AB")[censusCanada_tbl$Geographic_name] %>% unname()
  )
  
  canada_places <- censusCanada_tbl %>%
    dplyr::left_join(name_map, by = "Geographic_name") %>%
    dplyr::left_join(birding_CN_clean, by = "Region") %>%
    dplyr::mutate(
      birders_k = (Population_2011 * Birding_pct) / 1000,
      Place = Geographic_name,
      PintailRegion = Pintail_region
    ) %>%
    dplyr::select(Place, PintailRegion, birders_k)
  
  places_2011 <- dplyr::bind_rows(us_places, canada_places)
  
  pintail_place <- pintail_tbl %>%
    dplyr::filter(!IsSubtotal, !IsTotal) %>%
    dplyr::select(Place_pintail = Place, Prop_pintails_birding)
  
  alaska_sub_prop <- pintail_tbl %>%
    dplyr::filter(Group == "Alaska breeding", IsSubtotal, !is.na(Prop_pintails_birding)) %>%
    dplyr::pull(Prop_pintails_birding) %>%
    { if (length(.) == 0) NA_real_ else .[1] }
  
  name_xwalk <- c(
    "North Dakota" = "N. Dakota",
    "South Dakota" = "S. Dakota",
    "Northwest Territories" = "NW Territories"
  )
  
  places_with_prop <- places_2011 %>%
    dplyr::mutate(Place_join = dplyr::recode(Place, !!!name_xwalk, .default = Place)) %>%
    dplyr::left_join(pintail_place, by = c("Place_join" = "Place_pintail")) %>%
    dplyr::mutate(
      Prop_pintails_birding = dplyr::case_when(
        Place == "Alaska" & is.na(Prop_pintails_birding) ~ alaska_sub_prop,
        TRUE ~ Prop_pintails_birding
      )
    )
  
  missing_prop <- places_with_prop %>% dplyr::filter(is.na(Prop_pintails_birding))
  if (nrow(missing_prop) > 0) {
    warning(sprintf("Missing pintail proportion for: %s",
                    paste(unique(missing_prop$Place), collapse = ", ")))
  }
  
  out_place <- places_with_prop %>%
    dplyr::mutate(pintail_birders_k = birders_k * Prop_pintails_birding) %>%
    dplyr::select(Place, PintailRegion, birders_k, Prop_pintails_birding, pintail_birders_k)
  
  out_region <- out_place %>%
    dplyr::group_by(PintailRegion) %>%
    dplyr::summarise(
      total_birders_k = sum(birders_k, na.rm = TRUE),
      pintail_birders_k = sum(pintail_birders_k, na.rm = TRUE),
      .groups = "drop"
    )
  
  list(
    place_level = out_place,
    region_level = out_region
  )
}
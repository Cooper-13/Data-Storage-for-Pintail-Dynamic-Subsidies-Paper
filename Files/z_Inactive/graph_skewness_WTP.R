# Example usage with ND mapped to Southern breeding

# 1) Load packages
library(dplyr)
library(ggplot2)

# 2) Assume WTP_tbl and birders_US2011_tbl are already in your environment

# 3) Helper to convert USPS abbreviations to full state names
abbr_to_name <- function(x) {
  idx <- match(x, state.abb)
  out <- x
  out[!is.na(idx)] <- state.name[idx[!is.na(idx)]]
  out
}

# 4) Build State -> PintailRegion map and force ND to "Southern breeding"
state_region_map <- birders_US2011_tbl %>%
  filter(!is.na(PintailRegion)) %>%
  distinct(State, PintailRegion) %>%
  # Remove any existing ND (if present) and then add the override
  filter(State != "North Dakota") %>%
  bind_rows(tibble::tibble(State = "North Dakota",
                           PintailRegion = "Southern breeding"))

# 5) Prepare data with PintailRegion and WTP totals
wtp_region <- WTP_tbl %>%
  mutate(
    State = abbr_to_name(state),
    WTP_tot_abund1 = max_WTP_BM * trp_num,
    WTP_tot_abund2 = max_WTP_BM * trips_dbl_birds
  ) %>%
  left_join(state_region_map, by = "State")

# 6) Check unmapped rows (optional)
unmapped_n <- sum(is.na(wtp_region$PintailRegion))
if (unmapped_n > 0) message(sprintf("Unmapped rows: %d (will be excluded from plots)", unmapped_n))

# 7) Plot histograms (faceted by region)
p_abund1 <- wtp_region %>%
  filter(!is.na(PintailRegion)) %>%
  ggplot(aes(x = WTP_tot_abund1)) +
  geom_histogram(color = "white", fill = "#2C7FB8", bins = 20) +
  facet_wrap(~ PintailRegion, scales = "free_y") +
  labs(
    title = "Per-respondent WTP totals by region (abundance = 1)",
    x = "WTP_tot_abund1 = max_WTP_BM × trp_num (dollars)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

p_abund2 <- wtp_region %>%
  filter(!is.na(PintailRegion)) %>%
  ggplot(aes(x = WTP_tot_abund2)) +
  geom_histogram(color = "white", fill = "#41AE76", bins = 20) +
  facet_wrap(~ PintailRegion, scales = "free_y") +
  labs(
    title = "Per-respondent WTP totals by region (abundance = 2)",
    x = "WTP_tot_abund2 = max_WTP_BM × trips_dbl_birds (dollars)",
    y = "Count"
  ) +
  theme_minimal(base_size = 12)

# 8) Display in the plotting window
p_abund1
p_abund2

# 9) (Optional) Save to files
ggsave("wtp_hist_abund1.png", p_abund1, width = 8, height = 5, dpi = 300)
ggsave("wtp_hist_abund2.png", p_abund2, width = 8, height = 5, dpi = 300)
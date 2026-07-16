######### Model effect of duck abundance on economic value of viewing ##########
# This script prepares inputs and analyzes them for estimating economic value associated with pintail watching as a function of pintail abundance
  # Code prepared by Brady Mattsson with support from Academic AI
# load libraries ####
x = c("tidyverse", "ggplot2", "dplyr")   
lapply(x, library, character.only=TRUE)
rm(x)

# prepare data and related inputs for "new" economic analysis ####
## Number of birders by state #### 
### from Table 6 in birding addendum to 2011 USFWS recreation survey ####
# Carver, E. 2013. Birding in the United States: A demographic and economic analysis. 
  # Addendum to the 2011 National Survey of Fishing, Hunting, and Wildlife-associated Recreation. 
  # US Fish and Wildlife Service, Division of Economics, Arlington, Virginia, USA. Online: https://digitalmedia.fws.gov/cdm/ref/collection/document/id/1874
                                        
birders_US2011_tbl <- tibble::tribble(
  ~State, ~Total, ~Residents, ~Nonresidents, ~SmallSample,       ~Flyway, ~ PintailRegion,
  "Alabama",    607,       0.94,            NA,           NA, "Mississippi", NA,
  "Alaska",    512,       0.31,          0.69,           0L,     "Pacific", "Alaska breeding",
  "Arizona",   1110,       0.82,          0.18,           1L,     "Pacific", NA,
  "Arkansas",    539,       0.98,            NA,           NA, "Mississippi",NA,
  "California",   4864,       0.94,          0.06,           0L,     "Pacific", "Western wintering",
  "Colorado",   1188,       0.85,          0.15,           0L,     "Central", NA,
  "Connecticut",    873,       0.93,          0.07,           1L,    "Atlantic",NA,
  "Delaware",    171,        0.8,            NA,           NA,    "Atlantic",NA,
  "Florida",   2966,       0.75,          0.25,           0L,    "Atlantic",NA,
  "Georgia",   1903,       0.87,          0.13,           1L,    "Atlantic",NA,
  "Hawaii",    254,       0.27,          0.73,           1L,     "Pacific",NA,
  "Idaho",    419,       0.81,          0.19,           1L,     "Pacific",NA,
  "Illinois",   1811,        0.9,           0.1,           1L, "Mississippi",NA,
  "Indiana",   1175,       0.99,            NA,           NA, "Mississippi",NA,
  "Iowa",    531,       0.89,            NA,           NA, "Mississippi",NA,
  "Kansas",    476,       0.95,            NA,           NA,     "Central",NA,
  "Kentucky",    827,        0.9,           0.1,           1L, "Mississippi",NA,
  "Louisiana",    712,       0.71,            NA,           NA, "Mississippi","Central wintering",
  "Maine",    689,       0.38,          0.63,           0L,    "Atlantic",NA,
  "Maryland",    934,       0.84,          0.16,           1L,    "Atlantic",NA,
  "Massachusetts",   1238,       0.75,          0.25,           0L,    "Atlantic",NA,
  "Michigan",   2015,       0.93,          0.07,           1L, "Mississippi",NA,
  "Minnesota",   1112,       0.93,          0.07,           1L, "Mississippi",NA,
  "Mississippi",    456,       0.87,            NA,           NA, "Mississippi",NA,
  "Missouri",   1110,       0.92,          0.08,           1L, "Mississippi",NA,
  "Montana",    291,        0.6,           0.4,           1L,     "Central", "Southern breeding",
  "Nebraska",    273,       0.89,            NA,           NA,     "Central",NA,
  "Nevada",    447,       0.72,          0.28,           1L,     "Pacific",NA,
  "New Hampshire",    527,       0.55,          0.45,           1L,    "Atlantic",NA,
  "New Jersey",   1195,       0.87,          0.13,           1L,    "Atlantic",NA,
  "New Mexico",    415,       0.78,          0.22,           1L,     "Central",NA,
  "New York",   3272,       0.93,          0.07,           0L,    "Atlantic",NA,
  "North Carolina",   1854,       0.84,          0.16,           0L,    "Atlantic",NA,
  "Ohio",   1583,       0.97,            NA,           NA, "Mississippi",NA,
  "Oklahoma",    773,       0.97,            NA,           NA,     "Central",NA,
  "Oregon",    892,       0.79,          0.21,           1L,     "Pacific","Western wintering",
  "Pennsylvania",   2699,       0.89,          0.11,           0L,    "Atlantic",NA,
  "Rhode Island",    201,        0.8,           0.2,           0L,    "Atlantic",NA,
  "South Carolina",    536,       0.72,          0.28,           1L,    "Atlantic",NA,
  "South Dakota",    235,       0.64,          0.36,           0L,     "Central", "Southern breeding",
  "Tennessee",   1382,       0.82,          0.18,           0L, "Mississippi",NA,
  "Texas",   2238,       0.95,          0.05,           1L,     "Central","Central wintering",
  "Utah",    410,       0.69,          0.31,           0L,     "Pacific",NA,
  "Vermont",    292,       0.69,          0.31,           1L,    "Atlantic",NA,
  "Virginia",   1425,       0.81,          0.19,           1L,    "Atlantic",NA,
  "Washington",   1516,       0.83,          0.17,           1L,     "Pacific",NA,
  "West Virginia",    547,       0.88,            NA,           NA,    "Atlantic",NA,
  "Wisconsin",   1678,       0.89,          0.11,           1L, "Mississippi",NA,
  "Wyoming",    417,       0.31,          0.69,           0L,     "Central", NA
)

summary(birders_US2011_tbl); head(birders_US2011_tbl)

### from Table 6 in birding addendum to 2006 USFWS recreation survey  #### 
# Carver, E. 2009. Birding in the United States: A demographic and economic analysis. 
  # Addendum to the 2006 National Survey of Fishing, Hunting, and Wildlife-associated Recreation. 
  # US Fish and Wildlife Service, Division of Economics, Arlington, Virginia, USA https://www.fws.gov/sites/default/files/documents/2024-04/258.pdf
birdersUS2006_tbl <- tibble::tribble(
  ~State,            ~Total_Birders, ~Pct_Residents, ~Pct_Nonresidents,
  "Alabama",                 828L,           0.83,              0.17,
  "Alaska",                  429L,           0.34,              0.66,
  "Arizona",                1038L,           0.74,              0.26,
  "Arkansas",                764L,           0.79,              0.21,
  "California",             4493L,           0.88,              0.12,
  "Colorado",               1229L,           0.73,              0.27,
  "Connecticut",             857L,           0.91,              0.09,
  "Delaware",                189L,           0.66,              0.34,
  "Florida",                3101L,           0.79,              0.21,
  "Georgia",                1210L,           0.88,              0.12,
  "Hawaii",                  205L,           0.49,              0.51,
  "Idaho",                   557L,           0.56,              0.44,
  "Illinois",               1784L,           0.87,              0.13,
  "Indiana",                1345L,           0.86,              0.14,
  "Iowa",                    842L,           0.93,              0.07,
  "Kansas",                  493L,           0.92,               NA_real_,
  "Kentucky",               1041L,           0.84,              0.16,
  "Louisiana",               552L,           0.94,               NA_real_,
  "Maine",                   622L,           0.68,              0.32,
  "Maryland",                980L,           0.84,              0.16,
  "Massachusetts",          1377L,           0.86,              0.14,
  "Michigan",               1997L,           0.89,              0.11,
  "Minnesota",              1448L,           0.93,              0.07,
  "Mississippi",             535L,           0.79,              0.21,
  "Missouri",               1576L,           0.87,              0.13,
  "Montana",                 571L,           0.53,              0.47,
  "Nebraska",                364L,           0.87,               NA_real_,
  "Nevada",                  518L,           0.57,              0.43,
  "New Hampshire",           548L,           0.60,              0.40,
  "New Jersey",             1132L,           0.83,              0.17,
  "New Mexico",              641L,           0.54,              0.46,
  "New York",               2517L,           0.87,              0.13,
  "North Carolina",         1586L,           0.79,              0.21,
  "North Dakota",             83L,           0.83,               NA_real_,
  "Ohio",                   2405L,           0.95,              0.05,
  "Oklahoma",                765L,           0.94,               NA_real_,
  "Oregon",                 1046L,           0.74,              0.26,
  "Pennsylvania",           2669L,           0.91,              0.09,
  "Rhode Island",            297L,           0.71,               NA_real_,
  "South Carolina",          809L,           0.78,              0.22,
  "South Dakota",            283L,           0.68,              0.32,
  "Tennessee",              1838L,           0.79,              0.21,
  "Texas",                  2476L,           0.94,              0.06,
  "Utah",                    639L,           0.57,              0.43,
  "Vermont",                 364L,           0.52,              0.47,
  "Virginia",               1572L,           0.89,              0.11,
  "Washington",             1853L,           0.83,              0.17,
  "West Virginia",           398L,           0.67,              0.33,
  "Wisconsin",              1454L,           0.79,              0.21,
  "Wyoming",                 448L,           0.27,              0.73
)


## Number of birders by province/territory ####
### Proportion of birders by province/territory from Table 9 in 2012 Canadian Nature Survey ####
# Federal, Provincial, and Territorial Governments of Canada (FPTGC). 2014. 
  # 2012 Canadian Nature Survey: Awareness, participation, and expenditures in nature-based recreation, conservation, and subsistence activities. Ottawa
  # Canadian Councils of Resource Ministers. https://www.biodivcanada.ca/reports/canadiannaturesurvey 
# Tibble with only the first column (region) and the Birding column
birding_CN_tbl <- tibble::tribble(
  ~Region,   ~Birding_pct,
  "Canada",      0.18,
  "AB",          0.14,
  "BC",          0.19,
  "MB",          0.19,
  "NB",          0.22,
  "NL",          0.23,
  "NS",          0.23,
  "NT",          0.15,
  "NU+",         0.19,
  "ON",          0.19,
  "PE",          0.23,
  "QC",          0.15,
  "SK",          0.22,
  "YT",          0.27
)



### Number of residents by province/territory from 2011 census ####
# Statistics Canada. 2012. Population and dwelling counts, for Canada, provinces and territories, 2011 and 2006 censuses (table). 
  # Population and Dwelling Count Highlight Tables. 2011 Census. Statistics Canada Catalogue no. 98-310-XWE2011002. 
  # http://www12.statcan.gc.ca/census-recensement/2011/dp-pd/hlt-fst/pd-pl/File.cfm?T=101&SR=1&RPP=25&PR=0&CMA=0&S=50&O=A&LANG=Eng&OFT=CSV 
  # From landing page:   https://www12.statcan.gc.ca/census-recensement/2011/dp-pd/hlt-fst/pd-pl/Table-Tableau.cfm?LANG=Eng&T=101&S=50&O=A

censusCanada_tbl <- tibble::tribble(
  ~Geographic_name,        ~Population_2011, ~Pintail_region,
  "Yukon",                          33897L,   "Northern breeding",
  "Northwest Territories",          41462L,   "Northern breeding",
  "Manitoba",                     1208268L,   "Southern breeding",
  "Saskatchewan",                 1033381L,   "Southern breeding",
  "Alberta",                      3645257L,   "Southern breeding"
)

## proportion of pintail obs in ebird & economics by region ####
  # from Table 3 in Mattsson et al. 2018, appended total birders from Table 6 in USFWS 2006 recreation report and Environment Canada 2016 
pintail_tbl <- tibble::tribble(
  ~Group,              ~Place,          ~Viewing, ~Sport_harvest, ~Expend_birding_M, ~Prop_pintails_birding, ~Expend_hunting_M, ~CS_hunting_M, ~Hunting_total_M, ~Prop_pintails_migbirds, ~Prop_pintails_wfowl, ~IsSubtotal, ~IsTotal, ~Total_birders,
  "Western wintering", "California",          2848,           3122,              5970,                 0.0029,             110.3,          70.2,          180.4,                    0.061,                 0.102,    FALSE,     FALSE, 1870945,
  "Western wintering", "Oregon",               938,            463,              1400,                 0.0061,              54.7,          16.7,           71.4,                    0.086,                 0.096,    FALSE,     FALSE,  435568,
  "Western wintering", "Subtotal",            3785,           3585,              7370,                 NA_real_,           165.0,          86.9,          251.9,                 NA_real_,             NA_real_,   TRUE,     FALSE, 2306513,
  
  "Central wintering", "Louisiana",            478,            443,               921,                 0.0011,              69.7,          43.1,          112.8,                    0.022,                 0.028,    FALSE,     FALSE,  229860,
  "Central wintering", "Texas",               1403,           1436,              2839,                 0.0030,             296.6,          39.8,          336.4,                    0.009,                 0.048,    FALSE,     FALSE, 1031039,
  "Central wintering", "Subtotal",            1881,           1879,              3760,                 NA_real_,           366.3,          82.9,          449.2,                 NA_real_,             NA_real_,   TRUE,     FALSE, 1260900,
  
  "Southern breeding", "Alberta",              307,             44,               351,                 0.0072,             105.3,           3.3,          108.7,                 NA_real_,               0.037,    FALSE,     FALSE,  335685,
  "Southern breeding", "Manitoba",              70,             81,               152,                 0.0081,               0.5,           1.9,            2.4,                 NA_real_,               0.027,    FALSE,     FALSE,  117161,
  "Southern breeding", "Montana",              183,             69,               252,                 0.0042,               2.8,           5.9,            8.7,                    0.014,                 0.016,    FALSE,     FALSE,  237772,
  "Southern breeding", "N. Dakota",             99,             46,               144,                 0.0089,              12.5,          12.1,           24.6,                    0.026,                 0.029,    FALSE,     FALSE,   34562,
  "Southern breeding", "S. Dakota",             85,             73,               158,                 0.0071,               6.1,           9.8,           16.0,                    0.021,                 0.028,    FALSE,     FALSE,  117845,
  "Southern breeding", "Saskatchewan",          67,             13,                79,                 0.0087,              12.5,           1.0,           13.5,                 NA_real_,               0.029,    FALSE,     FALSE,   98772,
  "Southern breeding", "Subtotal",             810,            326,              1136,                 NA_real_,           139.8,          34.1,          173.8,                 NA_real_,             NA_real_,   TRUE,     FALSE,  941798,
  
  "Alaska breeding",   "Subtotal",             990,            321,              1310,                 0.0181,               5.7,           2.9,            8.6,                    0.109,                 0.109,     TRUE,     FALSE,  178641,
  
  "Northern breeding", "NW Territories",         2,             15,                17,                 0.0130,               4.6,           1.6,            6.2,                 NA_real_,               0.026,    FALSE,     FALSE,   11055,
  "Northern breeding", "Yukon",                 95,             12,               107,                 0.0138,               1.3,           1.4,            2.7,                 NA_real_,               0.004,    FALSE,     FALSE,    8097,
  "Northern breeding", "Subtotal",              97,             27,               124,                 NA_real_,             5.9,           3.0,            8.9,                 NA_real_,             NA_real_,   TRUE,     FALSE,   19152,
  
  "Total",             "Total",               7563,           6138,             13701,                 NA_real_,           682.7,         209.7,          892.4,                 NA_real_,             NA_real_,  FALSE,      TRUE, 4707004
)
# Columns:
# Group = region header
# Place = state/province/territory or "Subtotal"/"Total"
# Viewing, Sport_harvest = counts
# Expend_birding_M = expenditure on birding (all avian taxa, $M)
# Prop_pintails_birding = proportion of pintails (where shown)
# Expend_hunting_M, CS_hunting_M, Hunting_total_M = hunting values ($M)
# Prop_pintails_migbirds, Prop_pintails_wfowl = pintail proportions (where shown)
# IsSubtotal, IsTotal = markers for subtotal/grand total rows

## per capita willingness to pay for duck-watching as a function of duck abundance ####
# from Appendix D in Thogmartin et al 2023 https://www.sciencedirect.com/science/article/pii/S000632072300352X#s0130

flyway_tbl <- tibble::tribble(
  ~Flyway, ~Respondents, ~Abundance, ~Trips_n, ~Trips_SD, ~Trips_LCL, ~Trips_UCL, ~Expenses, ~Expenses_SD, ~Expenses_LCL, ~Expenses_UCL,
  "Central",      34,          1,       1.99,     1.44,      1.32,       2.65,       27.96,      4.65,         18.56,         37.36,
  "Mississippi",  12,          1,       2.40,     1.80,      1.04,       3.76,      120.04,      2.63,         52.12,        187.95,
  "Central",      34,          2,       5.63,     2.73,      3.74,       7.53,       43.37,      4.10,         28.79,         57.94,
  "Mississippi",  12,          2,      10.31,     5.59,      4.48,      16.14,      155.22,      2.58,         67.40,        243.05
)




## maybe not needed: collapse pintail_tbl by region ####

region_summary <- pintail_tbl %>%
  filter(!IsSubtotal, !IsTotal) %>%
  group_by(region = Group) %>%
  summarise(
    Viewing                = sum(Viewing, na.rm = TRUE),
    Sport_harvest          = sum(Sport_harvest, na.rm = TRUE),
    Expend_birding_M       = sum(Expend_birding_M, na.rm = TRUE),
    Prop_pintails_birding  = mean(Prop_pintails_birding, na.rm = TRUE),
    Expend_hunting_M       = sum(Expend_hunting_M, na.rm = TRUE),
    CS_hunting_M           = sum(CS_hunting_M, na.rm = TRUE),
    Hunting_total_M        = sum(Hunting_total_M, na.rm = TRUE),
    Prop_pintails_migbirds = mean(Prop_pintails_migbirds, na.rm = TRUE),
    Prop_pintails_wfowl    = mean(Prop_pintails_wfowl, na.rm = TRUE),
    Total_birders          = sum(Total_birders, na.rm = TRUE),
    .groups = "drop"
  )
# summary(region_summary); names(region_summary)


# calculate num. pintail birders in each region ####
#  North Dakota resident population values (replace with actual census numbers)
nd_pop_2006 <- 636019  # https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2010-evaluation-estimates.html
nd_pop_2011 <- 685526  # https://www.census.gov/programs-surveys/popest/technical-documentation/research/evaluation-estimates/2020-evaluation-estimates/2010s-totals-national.html

# function in estimate_pintail_birders_fn.r
res <- estimate_pintail_birders_fn(
  birders_US2011_tbl = birders_US2011_tbl,
  birdersUS2006_tbl = birdersUS2006_tbl,
  birding_CN_tbl = birding_CN_tbl,
  censusCanada_tbl = censusCanada_tbl,
  pintail_tbl = pintail_tbl,
  nd_pop_2006 = nd_pop_2006,
  nd_pop_2011 = nd_pop_2011
)

# Region-level totals (thousands)
res$region_level

# Place-level details (thousands)
res$place_level
# calculate viewing WTP in each region ####

# 4) Compute per-capita WTP and total WTP (millions of dollars) by region
out <- abund_tbl %>%
  dplyr::mutate(
    WTP_per_capita = wtp_of_A(Abundance),                 # $/person/year
    total_WTP_M = (pintail_birders_k * 1000 * WTP_per_capita) / 1e6  # $M/year
  ) %>%
  dplyr::select(PintailRegion, pintail_birders_k, Abundance, WTP_per_capita, total_WTP_M)

out
}
# resume ####

######### Model effect of duck abundance on economic value of viewing ##########

# load libraries ####
x = c("tidyverse", "ggplot2", "readxl", "lme4", "sjPlot", "DHARMa", "MuMIn",
      "performance", "car", "stringi", "mgcv",  "plotrix", 
      "scales", "ggpubr", "dplyr", "rebird")   # "yarrr", , "beepr"
lapply(x, library, character.only=TRUE)
rm(x)

# maybe not needed ######

#run WOTUSdataUSPPRprepare.R if needing to update/correct data preparation
WOTUSdataUSPPR <- read.csv("WOTUSdataUSPPR.csv", header=TRUE)
WOTUSdata2<-WOTUSdataUSPPR[7:122,]


#from https://drive.google.com/file/d/1z4HnrCFfgVh3VOFcch7_PlL0eIimEPSO/view?usp=sharing, specifically the viewing trips & WTP 2019-09-03-ANOVA-by flyway.pdf
#Tables 2, 3, 4, 5

rownum <- seq(1:4)
abund <- c(1, 1, 2, 2)
respondents <- c(34, 12, 34, 12)
vflyway <- as.factor(c(2, 3, 2, 3))
flyname <- c("Central", "Mississippi", "Central", "Mississippi")


CFtrips <- c(2L, 2L, 2L, 1L, 3L, 2L, 2L, 1L, 2L, 2L, 4L, 1L, 2L, 2L, 2L, 2L, 2L, 5L, 3L, 2L, 2L, 2L, 2L, 1L, 2L, 2L, 2L, 1L, 3L, 3L, 2L, 2L, 2L, 2L)
MFtrips <- c(1L, 2L, 3L, 2L, 2L, 2L, 3L, 1L, 2L, 3L, 7L, 6L)

# also not needed? ####

base::mean(MFtrips); base::mean(CFtrips); stats::median(MFtrips); stats::median(CFtrips)
gmeanMFtrips <- exp(mean(log(MFtrips)))
gmeanCFtrips <- exp(mean(log(CFtrips)))
gsdMFtrips <- exp(sd(log(MFtrips)))
gsdCFtrips <- exp(sd(log(CFtrips)))

MFdoubletrips <- c(1L, 3L, 9L, 9L, 3L, 6L, 9L, 7L, 9L, 6L, 370L, 262L)
CFdoubletrips <-  c(9L,3L, 12L,1L,6L,3L,3L,1L,6L,6L,254L,1L,6L,
                    9L,9L,6L,6L,15L,12L,6L,6L,9L,3L,4L,
                    6L,9L,3L,1L,6L,6L,12L,9L,9L,3L) 
base::mean(MFdoubletrips); base::mean(CFdoubletrips); stats::median(MFdoubletrips); stats::median(CFdoubletrips)
gmeanMFdoubletrips <- exp(mean(log(MFdoubletrips)))
gmeanCFdoubletrips <- exp(mean(log(CFdoubletrips)))
gsdMFdoubletrips <- exp(sd(log(MFdoubletrips)))
gsdCFdoubletrips <- exp(sd(log(CFdoubletrips)))


tripnum <- c(gmeanCFtrips, gmeanMFtrips, gmeanCFdoubletrips, gmeanMFdoubletrips)
tripnumSD <- c(gsdCFtrips, gsdMFtrips, gsdCFdoubletrips, gsdMFdoubletrips)
triplow <- tripnum - 1.96*tripnum/sqrt(respondents)
triphigh <- tripnum + 1.96*tripnum/sqrt(respondents)


CFexp <- c(0.5, 2, 7, 5, 5, 5, 2.5, 10, 15, 25, 15, 17, 20, 25, 25, 25, 30, 33, 45, 10, 60, 65, 60, 50, 75, 58, 80, 110, 65, 200, 210, 348, 325, 380)    #0.1 was 0, but gmean cannot address 0s
MFexp <- c(20, 45, 35, 95, 68, 120, 180, 200, 290, 295, 350, 340)
base::mean(CFexp); base::mean(MFexp); stats::median(CFexp); stats::median(MFexp)
gmeanCFexp <- exp(mean(log(CFexp)))
gmeanMFexp <- exp(mean(log(MFexp)))
gsdCFexp <- exp(sd(log(CFexp)))
gsdMFexp <- exp(sd(log(MFexp)))

CFdoubleexp <- c(0.5, 2, 7, 10, 10, 15, 22.5, 25, 25, 25, 25, 27, 30, 35, 40, 45, 50, 53, 55, 60, 60, 65, 70, 80, 95, 108, 110, 115, 135, 220, 230, 358, 375, 430)  #0.1 was 0, but gmean cannot address 0s
MFdoubleexp <- c(20, 45, 85, 105, 138, 150, 200, 300, 340, 345, 380, 440)
base::mean(CFdoubleexp); base::mean(MFdoubleexp)
gmeanCFdoubleexp <- exp(mean(log(CFdoubleexp)))
gmeanMFdoubleexp <- exp(mean(log(MFdoubleexp)))
gsdCFdoubleexp <- exp(sd(log(CFdoubleexp)))
gsdMFdoubleexp <- exp(sd(log(MFdoubleexp)))



viewexp <- c(gmeanCFexp, gmeanMFexp, gmeanCFdoubleexp, gmeanMFdoubleexp)
viewexpSD <- c(gsdCFexp, gsdMFexp, gsdCFdoubleexp, gsdMFdoubleexp)
viewlow <- viewexp - 1.96*viewexp/sqrt(respondents)
viewhigh <- viewexp + 1.96*viewexp/sqrt(respondents)


view.dat <- data.table::data.table(rownum, vflyway, flyname, respondents, abund, 
                                   tripnum, tripnumSD, triplow, triphigh, 
                                   viewexp, viewexpSD, viewlow, viewhigh, 
                                   stringsAsFactors = FALSE)
sjPlot::tab_df(view.dat, file="viewingdata.doc")
plotrix::plotCI(x=view.dat$rownum, y=view.dat$tripnum, ui=view.dat$triphigh, li=view.dat$triplow, 
                ylim=c(0,20), pt.bg=par("bg"), pch=21, xaxt="none",
                ylab="Viewing-related Expenses ($)", 
                xlab=" ")
axis(1, seq(0,4,1))

view.dat$group <- with(view.dat, interaction(view.dat$vflyway,  view.dat$abund))
view.dat$group <- factor(as.numeric(view.dat$group) - 1)
view.dat$scen <- c("Central-Current", "Mississippi-Current", "Central-Double", "Mississippi-Double")

numplot <- ggplot(data=view.dat) +
  geom_line(aes(x=as.factor(abund), y=tripnum, 
                group=flyname, color=flyname)) +     
  geom_pointrange(aes(x=as.factor(abund), y=tripnum, ymin=triplow, ymax=triphigh, 
                      group=flyname, color=flyname), 
                  position = position_dodge(width = 1), size=0.75)  +
  facet_grid(~flyname)+
  scale_color_manual(values=c("firebrick", "darkblue")) +
  xlab("Abundance") + ylab("Annual Number\nof Viewing Trips") + labs(color = "Flyway") +
  theme_minimal()

expplot <- ggplot(data=view.dat) +
  geom_line(aes(x=as.factor(abund), y=viewexp, 
                group=flyname, color=flyname)) +     
  geom_pointrange(aes(x=as.factor(abund), y=viewexp, ymin=viewlow, ymax=viewhigh, 
                      group=flyname, color=flyname), 
                  position = position_dodge(width = 1), size=0.75)  +
  facet_grid(~flyname)+
  scale_color_manual(values=c("firebrick", "darkblue")) +
  xlab("Abundance") + ylab("Viewing-related Expenditures\n(2014$), per Trip") + labs(color = "Flyway") +
  theme_minimal()

ggpubr::ggarrange(numplot, expplot, 
                  labels = c("A", "B"),
                  ncol = 1, nrow = 2)

######scrape table from viewing report

library("pdftools")
#download.file("https://www.fws.gov/sites/default/files/documents/2024-11/2022-birding-in-the-us-demographic-and-economic-analysis.pdf", destfile = "birding.pdf", mode = "wb")

#table <- pdftools::pdf_text("birding.pdf") %>% strsplit(split = "\n")
#cat(table[[11]]) 

#there's only 49 states -> North Dakota is missing

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

###collapse pintail_tbl by region ####

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

## per capita willingness to pay for duck-watching as a function of duck abundance ####
# from Appendix D in Thogmartin et al 2023 https://www.sciencedirect.com/science/article/pii/S000632072300352X#s0130

flyway_tbl <- tibble::tribble(
  ~Flyway, ~Respondents, ~Abundance, ~Trips_n, ~Trips_SD, ~Trips_LCL, ~Trips_UCL, ~Expenses, ~Expenses_SD, ~Expenses_LCL, ~Expenses_UCL,
  "Central",      34,          1,       1.99,     1.44,      1.32,       2.65,       27.96,      4.65,         18.56,         37.36,
  "Mississippi",  12,          1,       2.40,     1.80,      1.04,       3.76,      120.04,      2.63,         52.12,        187.95,
  "Central",      34,          2,       5.63,     2.73,      3.74,       7.53,       43.37,      4.10,         28.79,         57.94,
  "Mississippi",  12,          2,      10.31,     5.59,      4.48,      16.14,      155.22,      2.58,         67.40,        243.05
)




# resume ####

#number of birders*DU number of trips per birder*DU expenses per trip*eBird waterfowl proportion

CFview1exp <- Centralbirders*1000*as.data.frame(view.dat[1,6])*as.data.frame(view.dat[1,10])*CFwaterfowlprop   #*0.902
MFview1exp <- Mississippibirders*1000*as.data.frame(view.dat[2,6])*as.data.frame(view.dat[2,10])*MFwaterfowlprop   #*0.906
CFview2exp <- Centralbirders*1000*as.data.frame(view.dat[3,6])*as.data.frame(view.dat[3,10])*CFwaterfowlprop   #*0.902
MFview2exp <- Mississippibirders*1000*as.data.frame(view.dat[4,6])*as.data.frame(view.dat[4,10])*MFwaterfowlprop   #*0.906

#with birder decrement, i.e., the committed waterfowler hunter-birder
CFview1expB <- Centralbirders*1000*as.data.frame(view.dat[1,6])*as.data.frame(view.dat[1,10])*CFwaterfowlprop*0.105
MFview1expB <- Mississippibirders*1000*as.data.frame(view.dat[2,6])*as.data.frame(view.dat[2,10])*MFwaterfowlprop*0.094
CFview2expB <- Centralbirders*1000*as.data.frame(view.dat[3,6])*as.data.frame(view.dat[3,10])*CFwaterfowlprop*0.105
MFview2expB <- Mississippibirders*1000*as.data.frame(view.dat[4,6])*as.data.frame(view.dat[4,10])*MFwaterfowlprop*0.094

#with birder decrement, i.e., the committed waterfowler hunter-birder
CFview1expB250 <- Centralbirders*1000*250*CFwaterfowlprop*0.105
MFview1expB250 <- Mississippibirders*1000*250*MFwaterfowlprop*0.094
CFview2expB250 <- Centralbirders*1000*250*CFwaterfowlprop*0.105
MFview2expB250 <- Mississippibirders*1000*250*MFwaterfowlprop*0.094

Mississippibirders/Centralbirders  #2.464
MFwaterfowlprop/CFwaterfowlprop #2.001

#donations distribution
CFamountdon <- c(0, 125, 575, 1750, 3750, 7500, 10000)
CFpercentdon <- c(0.667, 0.300, 0.024, 0.006, 0.002, 0, 0.001)
plot(CFamountdon~CFpercentdon)
library(fitdistrplus)
add_nls <- nls(CFamountdon ~ a*exp(r*CFpercentdon), 
               start = list(a = 0.5, r = -0.2))
coef(add_nls)
CFdon <- data.frame(CFpercentdon, CFpercenta)
fo1 <- y ~ 1 / (1 + x^c)
fm1 <- nls(fo1, data, start = list(c = 1))
Plot(data, fm1, "1 parameter")

exp.f <- fitdistr(cyber.data$event.count, "exp")

#calculate just for pintails, to see how well it coheres with previous publication, Mattsson et al.
CFpintail1exp <- Centralbirders*1000*as.data.frame(view.dat[1,6])*as.data.frame(view.dat[1,10])*CFnorpinprop
MFpintail1exp <- Mississippibirders*1000*as.data.frame(view.dat[2,6])*as.data.frame(view.dat[2,10])*MFnorpinprop
CFpintail2exp <- Centralbirders*1000*as.data.frame(view.dat[3,6])*as.data.frame(view.dat[3,10])*CFnorpinprop
MFpintail2exp <- Mississippibirders*1000*as.data.frame(view.dat[4,6])*as.data.frame(view.dat[4,10])*MFnorpinprop

view.dat$birders <- c(Centralbirders, Mississippibirders, Centralbirders, Mississippibirders)
view.dat$ebirdwfprop <- c(CFwaterfowlprop, MFwaterfowlprop, CFwaterfowlprop, MFwaterfowlprop)
view.dat$totviewexp <- c(CFview1exp, MFview1exp, CFview2exp, MFview2exp)

sjPlot::tab_df(view.dat[,c(3,5,16:18)], file="viewingdata1.doc")

#number of birders*USFWS number of trips per birder[238036000/17818000 = 13.3593]*USFWS expenses per trip[14868424740/238036000 = 62.46]*eBird waterfowl proportion
#https://www.fws.gov/southeast/pdf/report/birding-in-the-united-states-a-demographic-and-economic-analysis.pdf Table 7 (page 10, units should be in thousands!!!!)
CFview1USFWSexp <- Centralbirders*1000*13.3593*62.46*CFwaterfowlprop
MFview1USFWSexp <- Mississippibirders*1000*13.3593*62.46*MFwaterfowlprop
#what values to use for number of trips and expenses per trip when doubling?
#CFview2USFWSexp <- Centralbirders*1000*VALUE*VALUE HERE*CFwaterfowlprop
#MFview2USFWSexp <- Mississippibirders*1000*VALUE*VALUE HERE*MFwaterfowlprop




#multiplier of 0.07381776 applied, but why?
#Are Wildlife Recreationists Conservationists? The Journal of Wildlife Management 79(3):446–457; 2015; DOI: 10.1002/jwmg.855
#number of hunter-birdwatchers = 64, number of hunters = 290, number of birdwatchers = 513; 64/(64+290+513) = 0.07381776
PropHunterBirder <- 64/(64+290) #proportion of hunters who are birders too


#WOTUSdata2[,4] is number of active hunters*number of trips per waterfowler*expenses per trip*eBird waterfowl proportion*Proportion of hunters who are birders

CFview1hexp <- WOTUSdata2[107,4]*view.dat[1,6]*view.dat[1,10]*CFwaterfowlprop*PropHunterBirder
MFview1hexp <- WOTUSdata2[108,4]*view.dat[2,6]*view.dat[2,10]*MFwaterfowlprop*PropHunterBirder
CFview2hexp <- WOTUSdata2[107,4]*view.dat[3,6]*view.dat[3,10]*CFwaterfowlprop*PropHunterBirder
MFview2hexp <- WOTUSdata2[108,4]*view.dat[4,6]*view.dat[4,10]*MFwaterfowlprop*PropHunterBirder


#calculate slope
MFriseexp <- MFview2exp-MFview1exp
CFriseexp <- CFview2exp-CFview1exp
run <- (2*49152200)-(1*49152200)  #49152.2 is the number of waterfowl (i.e., the BPOP) in 2014
MFslope <- MFriseexp/run   #
CFslope <- CFriseexp/run   #

wetlossscen

CFwfowlprop <- CF.waterfowln/(CF.waterfowln+MF.waterfowln)
MFwfowlprop <- MF.waterfowln/(CF.waterfowln+MF.waterfowln)



#0.8 is 80% of waterfowl in PPR go to Central and Mississippi Flyways; what is the correct percent?
totalCFviewlosslow <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[2])*CFslope*CFwfowlprop*0.8 #low 
totalCFviewlossmedlo <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[3])*CFslope*CFwfowlprop*0.8 #medium-low
totalCFviewlossmedhi <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[4])*CFslope*CFwfowlprop*0.8#medium-high
totalCFviewlosshigh <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[5])*CFslope*CFwfowlprop*0.8#high

totalMFviewlosslow <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[2])*MFslope*MFwfowlprop*0.8 #low 
totalMFviewlossmedlo <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[3])*MFslope*MFwfowlprop*0.8 #medium-low
totalMFviewlossmedhi <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[4])*MFslope*MFwfowlprop*0.8#medium-high
totalMFviewlosshigh <- (wetlossscen$duckloss[1]-wetlossscen$duckloss[5])*MFslope*MFwfowlprop*0.8#high

# trips_dbl_birds <- (data.table::data.table(
#   trips_dbl_birds = c(1L,1L,1L,1L,1L,1L,1L,1L,
#                       1L,1L,1L,1L,1L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,
#                       3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,
#                       3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,3L,4L,4L,6L,
#                       6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,
#                       6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,6L,
#                       6L,6L,6L,6L,6L,7L,9L,9L,9L,9L,9L,9L,9L,9L,
#                       9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,9L,12L,
#                       12L,12L,12L,12L,12L,12L,12L,12L,12L,12L,12L,
#                       15L,15L,15L,17L,18L,18L,20L,20L,23L,29L,34L,35L,
#                       40L,49L,168L,171L,177L,254L,262L,354L,362L,370L)
# )
# )
# 
# MASS::truehist(t(do.call(rbind.data.frame, trips_dbl_birds)), xlab="Trips at Double Abundance")




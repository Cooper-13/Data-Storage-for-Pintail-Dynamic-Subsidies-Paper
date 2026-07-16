#Ecosystem service flows adapted from Darius J. Semmens, Kenneth J. Bagstad, and Christine Sample

# front matter ----
## directory----
#setwd("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows")


## load environment----
#load("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_service_flows.RData")
#save.image("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_service_flows.RData")


## packages----
x = c("dplyr", "tidyr", "ggplot2", "tibble", "scales", "XLConnect")
# install.packages(x)
lapply(x, library, character.only=TRUE)
rm(x)

# Function to source the latest-matching file for each of multiple patterns, with optional printing ####
## front matter ####
# - patterns: character vector of patterns (glob by default, or regex if use_regex = TRUE)
# - print_info: logical; if TRUE, print "filename | first line" for each sourced file
# - dir: directory to search (non-recursive by default)
# - use_regex: interpret patterns as regex when TRUE; otherwise as glob
# - recursive: search subdirectories when TRUE
# - ignore.case: case-insensitive matching when TRUE
# - ...: passed to base::source (e.g., local = TRUE)
# define source_latest_fn ####
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


# Define scenario for calculating Ds
scenarioName <- "1_Conservative"
scenarioName <- "4_Integrated"
## source & call required functions ----
# Source latest versions and optionally print "filename | first line"
source_latest_fn(
  patterns = c("estimate_pintail_birders_fn*.R", "viewingWTP_fn*","helper_functions_viewingEcon*", 
               "population_model_functions*","PintailSimulation*", "ViewingEconomics*"),
  print_info = TRUE
)

# not needed?   define parameters for C-metrics ----
seasons <- 3 # Number of seasons or steps in one annual cycle
# This must match number of spreadsheets in input files

num_nodes <- 5 # Number of nodes in the network
# This must match the number of initial conditions given in input files

NODENAMES <- c("AK", "PR", "NU", "CA", "GC")
# Used to name row values in CR outputs, should be ordered to match node order in the .xlsx file

NETNAME <- c("adult_female", "adult_male", "juvenile_female", "juvenile_male")  # Give a distinct name for each class as used in input files
# Input files should be in the same directory as RunSpecies.R and named: metric_inputs_NETNAME[[]].xlsx
# Order is important when looking at the code: here we would index [[1]] = class 1 and [[2]] = class 2

PRINT_RESULTS <- TRUE # If true final CR results will print to the screen

source("C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/CR.R") #accesses calculating script

if(PRINT_RESULTS == TRUE){
  ## Print data to screen
  print("C-metric for all classes, seasons, and nodes:")
  print(round(CR,4))
  print("-------------------------------------------------------------------")
  
  print("Class averaged C-metric for all nodes and seasons:")
  print(round(CRt,4))
  print("-------------------------------------------------------------------")
  
  print("Season averaged C-metric for all nodes:")
  print(round(CRs,4))
  print("-------------------------------------------------------------------")
  
  print("Network Growth Rate for all seasons:")
  print(round(LAMBDAt,4))
  print("-------------------------------------------------------------------")
  
  print("Cpath-metric for all pathways, classes, and seasons:")
  print(round(CRpath,4))
  print("-------------------------------------------------------------------")
  
  print("Class averaged Cpath-metric for all pathways and seasons")
  print(round(CRpatht,4))
  print("-------------------------------------------------------------------")
  
  print("Season averaged Cpath-metric for all pathways:")
  print(round(CRpaths,4))
  print("-------------------------------------------------------------------")
  
} #prints C-metrics


# not needed? Proportional dependencies (Di)----
#dti <- (Cti_female*pti_female+Cti_male*pti_male)/sum(Cti_female*pti_female+Cti_male*pti_male)
#Di <- sum(dti)
d1i <- CR[, "season 1"]/(sum(CR[, "season 1"]))

d2i <- CR[, "season 2"]/(sum(CR[, "season 2"]))

d3i <- CR[, "season 3"]/(sum(CR[, "season 3"]))

Di <- c(sum(d1i[1:4]+d2i[1:4]+d3i[1:4])/3, sum(d1i[5:8]+d2i[5:8]+d3i[5:8])/3, sum(d1i[9:12]+d2i[9:12]+d3i[9:12])/3,
        sum(d1i[13:16]+d2i[13:16]+d3i[13:16])/3, sum(d1i[17:20]+d2i[17:20]+d3i[17:20])/3)
#proportional dependence of a species or subpopulation on location i




  
#Ecosystem service flows ----

Region <- c("PC", "SEUS", "C", "AK", "NCan")
#PC (Pacific Coast), SEUS (Southeast US), C (Central), AK (Alaska), NCan (Northern Canada)

Vi <- c(46415839, 17215010, 8497828, 25606513, 3870155)
#the total annual value of ESs provided by the species at location i

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

ES <- data.frame(Region, Di, Vi, MOi, MIi, MLi, Yi, YLi)
print(ES)


#Ecosystem services flows (ESF)----
#Abb.: _in (gross flows into region), _out (gross flows out of region), WW (Western Wintering), CW (Central Wintering), SB (Southern Breeding), AB (Alaska Breeding), NB (Northern Breeding)
WW_in <- c(ES[ES$Region == "PC", "Di"]*ES[ES$Region == "PC", "Vi"], 
           ES[ES$Region == "SEUS", "Di"]*ES[ES$Region == "PC", "Vi"], 
           ES[ES$Region == "C", "Di"]*ES[ES$Region == "PC", "Vi"], 
           ES[ES$Region == "AK", "Di"]*ES[ES$Region == "PC", "Vi"], 
           ES[ES$Region == "NCan", "Di"]*ES[ES$Region == "PC", "Vi"])

WW_out <- c(ES[ES$Region == "PC", "Vi"]*ES[ES$Region == "PC", "Di"], 
            ES[ES$Region == "SEUS", "Vi"]*ES[ES$Region == "PC", "Di"],
            ES[ES$Region == "C", "Vi"]*ES[ES$Region == "PC", "Di"],
            ES[ES$Region == "AK", "Vi"]*ES[ES$Region == "PC", "Di"],
            ES[ES$Region == "NCan", "Vi"]*ES[ES$Region == "PC", "Di"])

CW_in <- c(ES[ES$Region == "PC", "Di"]*ES[ES$Region == "SEUS", "Vi"], 
           ES[ES$Region == "SEUS", "Di"]*ES[ES$Region == "SEUS", "Vi"], 
           ES[ES$Region == "C", "Di"]*ES[ES$Region == "SEUS", "Vi"], 
           ES[ES$Region == "AK", "Di"]*ES[ES$Region == "SEUS", "Vi"], 
           ES[ES$Region == "NCan", "Di"]*ES[ES$Region == "SEUS", "Vi"])

CW_out <- c(ES[ES$Region == "PC", "Vi"]*ES[ES$Region == "SEUS", "Di"], 
            ES[ES$Region == "SEUS", "Vi"]*ES[ES$Region == "SEUS", "Di"],
            ES[ES$Region == "C", "Vi"]*ES[ES$Region == "SEUS", "Di"],
            ES[ES$Region == "AK", "Vi"]*ES[ES$Region == "SEUS", "Di"],
            ES[ES$Region == "NCan", "Vi"]*ES[ES$Region == "SEUS", "Di"])

SB_in <- c(ES[ES$Region == "PC", "Di"]*ES[ES$Region == "C", "Vi"], 
           ES[ES$Region == "SEUS", "Di"]*ES[ES$Region == "C", "Vi"], 
           ES[ES$Region == "C", "Di"]*ES[ES$Region == "C", "Vi"], 
           ES[ES$Region == "AK", "Di"]*ES[ES$Region == "C", "Vi"], 
           ES[ES$Region == "NCan", "Di"]*ES[ES$Region == "C", "Vi"])

SB_out <- c(ES[ES$Region == "PC", "Vi"]*ES[ES$Region == "C", "Di"], 
            ES[ES$Region == "SEUS", "Vi"]*ES[ES$Region == "C", "Di"],
            ES[ES$Region == "C", "Vi"]*ES[ES$Region == "C", "Di"],
            ES[ES$Region == "AK", "Vi"]*ES[ES$Region == "C", "Di"],
            ES[ES$Region == "NCan", "Vi"]*ES[ES$Region == "C", "Di"])

AB_in <- c(ES[ES$Region == "PC", "Di"]*ES[ES$Region == "AK", "Vi"], 
           ES[ES$Region == "SEUS", "Di"]*ES[ES$Region == "AK", "Vi"], 
           ES[ES$Region == "C", "Di"]*ES[ES$Region == "AK", "Vi"], 
           ES[ES$Region == "AK", "Di"]*ES[ES$Region == "AK", "Vi"], 
           ES[ES$Region == "NCan", "Di"]*ES[ES$Region == "AK", "Vi"])

AB_out <- c(ES[ES$Region == "PC", "Vi"]*ES[ES$Region == "AK", "Di"], 
            ES[ES$Region == "SEUS", "Vi"]*ES[ES$Region == "AK", "Di"],
            ES[ES$Region == "C", "Vi"]*ES[ES$Region == "AK", "Di"],
            ES[ES$Region == "AK", "Vi"]*ES[ES$Region == "AK", "Di"],
            ES[ES$Region == "NCan", "Vi"]*ES[ES$Region == "AK", "Di"])

NB_in <- c(ES[ES$Region == "PC", "Di"]*ES[ES$Region == "NCan", "Vi"], 
           ES[ES$Region == "SEUS", "Di"]*ES[ES$Region == "NCan", "Vi"], 
           ES[ES$Region == "C", "Di"]*ES[ES$Region == "NCan", "Vi"], 
           ES[ES$Region == "AK", "Di"]*ES[ES$Region == "NCan", "Vi"], 
           ES[ES$Region == "NCan", "Di"]*ES[ES$Region == "NCan", "Vi"])

NB_out <- c(ES[ES$Region == "PC", "Vi"]*ES[ES$Region == "NCan", "Di"], 
            ES[ES$Region == "SEUS", "Vi"]*ES[ES$Region == "NCan", "Di"],
            ES[ES$Region == "C", "Vi"]*ES[ES$Region == "NCan", "Di"],
            ES[ES$Region == "AK", "Vi"]*ES[ES$Region == "NCan", "Di"],
            ES[ES$Region == "NCan", "Vi"]*ES[ES$Region == "NCan", "Di"])

ESF <- data.frame(Region, WW_in, WW_out, CW_in, CW_out, SB_in, SB_out, AB_in, AB_out, NB_in, NB_out)
print(ESF)


#Ecosystem services net flows (ESNF)----
C <- c(ES[ES$Region == "C", "Yi"]*ES[ES$Region == "PC", "Yi"]/(ES[ES$Region == "PC", "Yi"]+ES[ES$Region == "SEUS", "Yi"]),
       ES[ES$Region == "C", "Yi"]*ES[ES$Region == "SEUS", "Yi"]/(ES[ES$Region == "SEUS", "Yi"]+ES[ES$Region == "PC", "Yi"]),
       "-", "-", "-")

AK <- c(ES[ES$Region == "AK", "Yi"]*ES[ES$Region == "PC", "Yi"]/(ES[ES$Region == "PC", "Yi"]+ES[ES$Region == "SEUS", "Yi"]),
        ES[ES$Region == "AK", "Yi"]*ES[ES$Region == "SEUS", "Yi"]/(ES[ES$Region == "SEUS", "Yi"]+ES[ES$Region == "PC", "Yi"]),
        "-", "-", "-")

NCan <- c(ES[ES$Region == "NCan", "Yi"]*ES[ES$Region == "PC", "Yi"]/(ES[ES$Region == "PC", "Yi"]+ES[ES$Region == "SEUS", "Yi"]),
          ES[ES$Region == "NCan", "Yi"]*ES[ES$Region == "SEUS", "Yi"]/(ES[ES$Region == "SEUS", "Yi"]+ES[ES$Region == "PC", "Yi"]),
          "-", "-", "-")

ESNF <- data.frame(Region, YLi, C, AK, NCan)
print(ESNF)


#save as csv-file----
write.csv(ES,"C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_services.csv")
write.csv(ESF,"C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_services_flows.csv")
write.csv(ESNF,"C:/Users/jendrik/Seafile/Projekte/DISES/analysis/R/ecosystem_service_flows/ecosystem_services_net_flows.csv")

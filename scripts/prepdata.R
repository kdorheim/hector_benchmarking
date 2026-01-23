

# 0. Set Up --------------------------------------------------------------------

# Load required libraries
library(dplyr)
library(tidyr)
library(hector)

# TODO delete
library(ggplot2)

# Define the paths to the data 
RAWDATA_DIR <- here::here("data", "raw-data")
OUT_DIR <- here::here("data")


## 0B. Mapping Files ------------------------------------------------------------

rcmip_variable <- c("Effective Radiative Forcing", 
                    "Atmospheric Concentrations|CO2",  
                    "Atmospheric Concentrations|CH4", 
                    "Surface Air Temperature Change")
hector_variable <- c(RF_TOTAL(), 
                     CONCENTRATIONS_CO2(), 
                     CONCENTRATIONS_CH4(), 
                     GLOBAL_TAS())
RCMIP1_var_mapping <- data.frame(rcmip_variable = rcmip_variable, 
                                 hector_variable = hector_variable)



rcmip_scenario <- c("1pctCO2" , "abrupt-4xCO2", 
                    "esm-ssp119", "esm-ssp245", "esm-ssp585")
hector_scenario <- c("1pctCO2", "abruptx4CO2",
                     "ssp119", "ssp245", "ssp585")
RCMIP_scn_mapping <- data.frame(rcmip_scenario = rcmip_scenario, 
                                hector_scenario = hector_scenario)

# The expected column names for data sets
expected_cols <- c("scenario", "variable", "model", "year", "value", "units")


## 0C. Helper Fxns -------------------------------------------------------------


# Note that there are only selected variables/scenarios that will 
# need normalization. Mainly the multiforcing temperature results, 
# this helper function will facilitate that. 
# Args
#   d: data.frame of results to be normalized (can work on a data frame 
#      containing multiple scenarios/variables if need be)
#   ref_period: default set to 1850-1900
# Returns: data.frame of normalized results
noramalize_data <- function(d, ref_period = 1850:1900){
  
  d %>% 
    filter(year %in% ref_period) %>% 
    summarize(ref = mean(value), .by = c(scenario, variable)) -> 
    ref_val
  
  d %>% 
    left_join(ref_val, by = c("scenario", "variable")) %>% 
    mutate(value = value - ref) %>% 
    select(-ref) -> 
    out
  
  return(out)
  
}






# 1. Hector V3.5 ---------------------------------------------------------------
# The idea here is that this variable/scenario list may grow as more benchmark 
# data sets are added. 
vars <- RCMIP1_var_mapping$hector_variable
scns <- RCMIP_scn_mapping$hector_scenario

# Download the release data from zenodo. 
url("https://zenodo.org/records/17459384/files/output-V3.5.0.csv") %>% 
  read.csv %>% 
  filter(scenario %in% scns) %>% 
  filter(variable %in% vars) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>% 
  mutate(year = as.integer(gsub(replacement = "", pattern = "X", x = year))) %>% 
  mutate(model = paste0("hector ", version)) %>% 
  select(all_of(expected_cols)) -> 
  hector_data


# Normalize the non idealized temperature results. 
hector_data %>% 
  filter(variable %in% c(GLOBAL_TAS()) & grepl(x = scenario, pattern = "ssp")) %>% 
  noramalize_data -> 
  normalized_temp_data

# Add the normalized temp back to the data set. 
hector_data %>% 
  filter(!(variable %in% c(GLOBAL_TAS()) & grepl(x = scenario, pattern = "ssp"))) %>% 
  bind_rows(normalized_temp_data) -> 
  hector_data

# 2. MAGICC ---------------------------------------------------------------

# Read in the RCMIP I csv file and change from wide to long format. 
file.path(RAWDATA_DIR, "rcmip_phase-1_magicc7.1.0.beta_v1-0-0.csv") %>% 
  read.csv() %>% 
  filter(Region == "World") %>% 
  filter(Variable %in% RCMIP1_var_mapping$rcmip_variable) %>% 
  filter(Scenario %in% RCMIP_scn_mapping$rcmip_scenario) %>% 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value") %>% 
  mutate(year = as.integer(gsub(x = year, replacement = "", pattern = "X")), 
         Climatemodel = gsub(x = Climatemodel, pattern = ".beta-rcmip-phase-1", replacement = "")) -> 
  magicc_data

# Update the names in preparation of mapping to the hector variable names. 
colnames(magicc_data) <- tolower(paste("rcmip_", colnames(magicc_data), sep = ""))

# Update to hector variable & scenario names. 
magicc_data %>%  
  left_join(RCMIP1_var_mapping, by = "rcmip_variable") %>% 
  left_join(RCMIP_scn_mapping, by = "rcmip_scenario") %>% 
  # select the columns of interest
  select(scenario = hector_scenario, 
         variable = hector_variable,
         model = rcmip_climatemodel, 
         year = rcmip_year, 
         value = rcmip_value) ->
  magicc_data

# Normalize the non idealized temperature results. 
magicc_data %>% 
  filter(variable %in% c(GLOBAL_TAS()) & grepl(x = scenario, pattern = "ssp")) %>% 
  noramalize_data -> 
  normalized_temp_data

# Add the normalized temp back to the data set. 
magicc_data %>% 
  filter(!(variable %in% c(GLOBAL_TAS()) & grepl(x = scenario, pattern = "ssp"))) %>% 
  bind_rows(normalized_temp_data) -> 
  magicc_data  

# Save a copy of the data for the hector / magicc comparison.
hector_data %>%  
  filter(scenario %in% magicc_data$scenario & variable %in% magicc_data$variable) %>% 
  bind_rows(magicc_data) %>% 
  filter(grepl(x = scenario, pattern = "ssp")) -> 
  out

write.csv(x = out, file = file.path(OUT_DIR, "hector_magicc.csv"), row.names = FALSE)


# 2. fair ----------------------------------------------------------------------

scn_patterns <- "ssp119|ssp245|ssp585"

# Identify and read fair results for the the scenarios of interest.  
data.frame(files = list.files(file.path(RAWDATA_DIR, "fair"), pattern = "csv", full.names = TRUE)) %>% 
  filter(grepl(x = files, pattern = scn_patterns)) %>% 
  # search the file names for the different conditions (emission driven, default results, the lattest version, ect.)
  filter(grepl(x = files, pattern = "esm")) %>% 
  filter(grepl(x = files, pattern = "default")) %>%  
  filter(grepl(x = files, pattern = "v1-0-1")) %>%  
  filter(!grepl(x = files, pattern = "allGHG")) %>% 
  pull(files) %>% 
  lapply(read.csv) %>% 
  bind_rows %>%  
  # Select the variables of interest. 
  filter(Region == "World") %>% 
  filter(Variable %in% RCMIP1_var_mapping$rcmip_variable) %>% 
  filter(Scenario %in% RCMIP_scn_mapping$rcmip_scenario) %>% 
  # Change from wide to long data fromat. 
  pivot_longer(cols = starts_with("X"), names_to = "year", values_to = "value")  %>%  
  mutate(year = as.integer(substr(year, 2, 5)),
         Climatemodel = gsub(x = Climatemodel, pattern = "-DEFAULT", replacement = "")) -> 
  fair_data

# Update the names in preparation of mapping to the hector variable names. 
colnames(fair_data) <- tolower(paste("rcmip_", colnames(fair_data), sep = ""))


# Update to hector variable & scenario names. 
fair_data %>%  
  left_join(RCMIP1_var_mapping, by = "rcmip_variable") %>% 
  left_join(RCMIP_scn_mapping, by = "rcmip_scenario") %>% 
  # select the columns of interest
  select(scenario = hector_scenario, 
         variable = hector_variable,
         model = rcmip_climatemodel, 
         year = rcmip_year, 
         value = rcmip_value) ->
  fair_data

# Normalize the non idealized temperature results. 
fair_data %>% 
  filter(variable %in% c(GLOBAL_TAS()) & grepl(x = scenario, pattern = "ssp")) %>% 
  noramalize_data -> 
  normalized_temp_data

# Add the normalized temp back to the data set. 
fair_data %>% 
  filter(!(variable %in% c(GLOBAL_TAS()) & grepl(x = scenario, pattern = "ssp"))) %>% 
  bind_rows(normalized_temp_data) -> 
  fair_data  

# Save a copy of the data for the hector / magicc comparison.
hector_data %>%  
  filter(scenario %in% fair_data$scenario & variable %in% fair_data$variable) %>% 
  bind_rows(fair_data) %>% 
  filter(grepl(x = scenario, pattern = "ssp")) -> 
  out

write.csv(x = out, file = file.path(OUT_DIR, "hector_fair.csv"), row.names = FALSE)




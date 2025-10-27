#------------------------------------------------------------------------------#
#------------ all management ---------------------------------------------------#
#------------------------------------------------------------------------------#

# ---- Configure base output folder ----
base_out <- "./output/all"
dir.create("./output", showWarnings = FALSE)
dir.create(base_out, recursive = TRUE, showWarnings = FALSE)

# ---- Create shared subfolders ----
shared_dirs <- list(
  path_model = file.path(base_out, "model"),
  path_stat  = file.path(base_out, "stat"),
  path_graph = file.path(base_out, "graph"),
  path_data  = file.path(base_out, "data")
)

# ---- Create each directory
invisible(lapply(shared_dirs, dir.create, recursive = TRUE, showWarnings = FALSE))

# ---- Optional: access paths
shared_dirs$path_model  # "./output/all/model"
shared_dirs$path_data   # "./output/all/data"


#------------------------------------------------------------------------------#
# 1. Regression
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)


source("./functions/rfp_regression.R")

#---load the data
mod_data <- read_xlsx("./input/rfps_data.xlsx", sheet = "full_data") %>% 
  filter(!is.na(effectSize)) %>% 
  select(-c("key", "x", "y", "Crop_Group", "kg_clim", 
            "GDD_maize", "GDD_wheat", "GDD_rice", "GDD_soybean", "pet")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb), "Unknown"),
    regions  = fct_na_value_to_level(as.factor(regions), "Unknown")
  ) %>%
  mutate(across(
    where(is.numeric),
    ~ ifelse(is.na(.), median(., na.rm = TRUE), .)
  ))

colSums(is.na(mod_data))
dim(mod_data)

   
# Provide function variables 
path_model<-"./output/all/model/"
path_stat<-"./output/all/stat/"
path_graph<-"./output/all/graph/"
path_data<-"./output/all/data/"
es_map<-"all_es.tif"


# Modelling
rfp_reg(mod_data,
        path_model,
        path_stat,
        path_data,
        path_graph,
        es_map)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir<-"./output/"
unc_map<-"all_unc.tif"

rfp_unc(path_model,
    path_graph,
    dir,
    unc_map)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model,
         path_graph,
         path_stat,
         path_data)


#------------------------------------------------------------------------------#
#------------ CC management ---------------------------------------------------#
#------------------------------------------------------------------------------#

# ---- Configure crops & subfolders ----
crops    <- c("maize", "wheat", "cereal", "veg_fruits")
subdirs  <- c("model", "stat", "graph", "data")
base_out <- "./output/cc"

# ---- Create base folders ----
dir.create("./output", showWarnings = FALSE)
dir.create(base_out, recursive = TRUE, showWarnings = FALSE)

# ---- Build & create paths for each crop ----
paths_by_crop <- setNames(lapply(crops, function(crop) {
  base <- file.path(base_out, crop)
  dirs <- list(
    path_model = file.path(base, "model"),
    path_stat  = file.path(base, "stat"),
    path_graph = file.path(base, "graph"),
    path_data  = file.path(base, "data")
  )
  invisible(lapply(dirs, dir.create, recursive = TRUE, showWarnings = FALSE))
  dirs
}), crops)

#------------------------------------------------------------------------------#
# All crops

rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)


source("./functions/rfp_regression.R")

#---load the data
mod_data <- read_xlsx("./input/rfps_data.xlsx", sheet = "CC") %>% 
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
path_model<-"./output/cc/all/model/"
path_stat<-"./output/cc/all/stat/"
path_graph<-"./output/cc/all/graph/"
path_data<-"./output/cc/all/data/"
es_map<-"cc_es.tif"


rfp_reg(mod_data,
        path_model,
        path_stat,
        path_data,
        path_graph,
        es_map)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")
dir<-"./output/CC/"
unc_map<-"cc_unc.tif"

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

#------------------------------------------------------------------------------#
# MODELLING FOR EACH INDIVIDUAL CROPS UNDER CC---------------------------------#
#------------------------------------------------------------------------------#

#-----------@ 1. Maize @-------------------------------------------------------#
  
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/crops_rfp_regression.R")

#---load the data
df_m <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("CC"), Crop_Group %in% c("Maize"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_wheat","GDD_rice","GDD_soybean")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),      "Unknown"),
    kg_clim  = as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_m<-na.omit(df_m)
mod_data<-df_sel_m
names(mod_data)
dim(mod_data)

# set paths
# path_model<-"./output/cc/maize/model/"
# path_stat<-"./output/cc/maize/stat/"
# path_graph<-"./output/cc/maize/graph/"
# path_data<-"./output/cc/maize/data/"
# spacevar = "regions"
# esap<-"cc_es.tif"

# set paths
path_model_m<-"./output/cc/maize/model/"
path_stat_m<-"./output/cc/maize/stat/"
path_graph_m<-"./output/cc/maize/graph/"
path_data_m<-"./output/cc/maize/data/"
spacevar_m = "regions"
es_map_m<-"cc_es_m.tif"


# Modelling
crops_rfp_reg(df_sel_m,
        path_model_m,
        path_stat_m,
        path_data_m,
        path_graph_m,
        spacevar_m,
        es_map_m)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_m<-"./output/cc/maize"
unc_map_m<-"cc_unc_m.tif"

rfp_unc(path_model_m,
    path_graph_m,
    dir_m,
    unc_map_m)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_m,
         path_graph_m,
         path_stat_m,
         path_data_m)

#-----------@ 2. Wheat @-------------------------------------------------------#

#------------------------------------------------------------------------------#

rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/CV_crops_rfp_regression.R")

#---load the data
df_w <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("CC"), Crop_Group %in% c("Wheat"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_rice",
                                         "GDD_soybean")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),      "Unknown"),
    kg_clim  = as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_w<-na.omit(df_w)
mod_data<-df_sel_w
names(mod_data)
dim(mod_data)

# set paths
# path_model<-"./output/cc/wheat/model/"
# path_stat<-"./output/cc/wheat/stat/"
# path_graph<-"./output/cc/wheat/graph/"
# path_data<-"./output/cc/wheat/data/"

# set paths
path_model_w<-"./output/cc/wheat/model/"
path_stat_w<-"./output/cc/wheat/stat/"
path_graph_w<-"./output/cc/wheat/graph/"
path_data_w<-"./output/cc/wheat/data/"
spacevar_w = "kg_clim"
es_rfp_w<-"cc_es_w.tif"

# Modelling
cv_crops_rfp_reg(df_sel_w,
        path_model_w,
        path_stat_w,
        path_data_w,
        path_graph_w,
        spacevar_w,
        es_rfp_w)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_w<-"./output/cc/wheat"
unc_rfp_w<-"cc_unc_w.tif"

rfp_unc(path_model_w,
    path_graph_w,
    dir_w,
    unc_rfp_w)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_w,
         path_graph_w,
         path_stat_w,
         path_data_w)

#-----------@ 3. Vegetables, fruits and others @-------------------------------#

rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/crops_rfp_regression.R")

#---load the data
df_vf <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("CC"), Crop_Group %in% c("Veg&Fruit and others"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_wheat","GDD_rice",
                                         "GDD_soybean")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform),"Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),"Unknown"),
    kg_clim  = as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_vf<-na.omit(df_vf)
mod_data<-df_sel_vf
names(mod_data)
dim(mod_data)

# path_model<-"./output/cc/veg_fruits/model/"
# path_stat<-"./output/cc/veg_fruits/stat/"
# path_graph<-"./output/cc/veg_fruits/graph/"
# path_data<-"./output/cc/veg_fruits/data/"
# spacevar<-"region"
# es_rfp<-"cc_es_vf.tif"

# set paths
path_model_vf<-"./output/cc/veg_fruits/model/"
path_stat_vf<-"./output/cc/veg_fruits/stat/"
path_graph_vf<-"./output/cc/veg_fruits/graph/"
path_data_vf<-"./output/cc/veg_fruits/data/"
spacevar_vf<-"regions"
es_rfp_vf<-"cc_es_vf.tif"

# Modelling
crops_rfp_reg(df_sel_vf,
        path_model_vf,
        path_stat_vf,
        path_data_vf,
        path_graph_vf,
        spacevar_vf,
        es_rfp_vf)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_vf<-"./output/cc/veg_fruits"
unc_rfp_vf<-"cc_unc_vf.tif"

rfp_unc(path_model_vf,
    path_graph_vf,
    dir_vf,
    unc_rfp_vf)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_vf,
         path_graph_vf,
         path_stat_vf,
         path_data_vf)




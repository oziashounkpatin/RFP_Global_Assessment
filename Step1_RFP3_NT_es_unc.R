# ---- Configure crops & subfolders ----
crops    <- c("maize", "wheat", "cereal", "veg_fruits","rice","soybean","cash_crop")
subdirs  <- c("model", "stat", "graph", "data")
base_out <- "./output/nt"

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

#rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)


source("./functions/rfp_regression.R")

#---load the data
mod_data <- read_xlsx("./input/rfps_data.xlsx", sheet = "NT") %>% 
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

dim(mod_data)

# Provide function variables 
path_model<-"./output/nt/all/model/"
path_stat<-"./output/nt/all/stat/"
path_graph<-"./output/nt/all/graph/"
path_data<-"./output/nt/all/data/"
es_map<-"nt_es.tif"

rfp_reg(mod_data,
        path_model,
        path_stat,
        path_data,
        path_graph,
        es_map)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")
dir<-"./output/nt/"
unc_map<-"nt_unc.tif"

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
# MODELLING FOR EACH INDIVIDUAL CROPS UNDER AF---------------------------------#
#------------------------------------------------------------------------------#

#-----------@ 1. Maize @-------------------------------------------------------#
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)


source("./functions/crops_rfp_regression.R")

#---load the data
df_m <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Maize"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_wheat","GDD_rice","GDD_soybean","pet")) %>%
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
# path_model<-"./output/nt/maize/model/"
# path_stat<-"./output/nt/maize/stat/"
# path_graph<-"./output/nt/maize/graph/"
# path_data<-"./output/nt/maize/data/"
# spacevar = "regions"
# esap<-"nt_es.tif"

# set paths
path_model_m<-"./output/nt/maize/model/"
path_stat_m<-"./output/nt/maize/stat/"
path_graph_m<-"./output/nt/maize/graph/"
path_data_m<-"./output/nt/maize/data/"
spacevar_m = "regions"
es_map_m<-"nt_es_m.tif"


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

dir_m<-"./output/nt/maize"
unc_map_m<-"nt_unc_m.tif"

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

source("./functions/crops_rfp_regression.R")

#---load the data
df_w <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Wheat"), !is.na(Crop_Group)) %>%
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

# # set paths
# path_model<-"./output/nt/wheat/model/"
# path_stat<-"./output/nt/wheat/stat/"
# path_graph<-"./output/nt/wheat/graph/"
# path_data<-"./output/nt/wheat/data/"

# set paths
path_model_w<-"./output/nt/wheat/model/"
path_stat_w<-"./output/nt/wheat/stat/"
path_graph_w<-"./output/nt/wheat/graph/"
path_data_w<-"./output/nt/wheat/data/"
spacevar_w = "regions"
es_rfp_w<-"nt_es_w.tif"

# Modelling
crops_rfp_reg(df_sel_w,
        path_model_w,
        path_stat_w,
        path_data_w,
        path_graph_w,
        spacevar_w,
        es_rfp_w)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_w<-"./output/cc/wheat"
unc_rfp_w<-"nt_unc_w.tif"

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


#-----------@ 3. Cereal @------------------------------------------------------#
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/crops_rfp_regression.R")

#---load the data
df_c <- read_xlsx("./input/rfps_data.xlsx") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Cereal"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_wheat","GDD_rice",
                                 "GDD_soybean","pet")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),      "Unknown"),
    kg_clim  = as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_c<-na.omit(df_c)
mod_data<-df_sel_c
names(mod_data)
dim(mod_data)
# 
# # set paths
# path_model<-"./output/nt/cereal/model/"
# path_stat<-"./output/nt/cereal/stat/"
# path_graph<-"./output/nt/cereal/graph/"
# path_data<-"./output/nt/cereal/data/"
# es_map<-"nt_es_c.tif"

# set paths
path_model_c<-"./output/nt/cereal/model/"
path_stat_c<-"./output/nt/cereal/stat/"
path_graph_c<-"./output/nt/cereal/graph/"
path_data_c<-"./output/nt/cereal/data/"
spacevar_c = "regions"
es_rfp_c<-"nt_es_nt.tif"

# Modelling
crops_rfp_reg(df_sel_c,
        path_model_c,
        path_stat_c,
        path_data_c,
        path_graph_c,
        spacevar_c,
        es_rfp_c)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_c<-"./output/nt/cereal"
unc_rfp_c<-"nt_unc_c.tif"

rfp_unc(path_model_c,
    path_graph_c,
    dir_c,
    unc_rfp_c)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_c,
         path_graph_c,
         path_stat_c,
         path_data_c)

#-----------@ 4. Vegetables, fruits and others @-------------------------------#
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/crops_rfp_regression.R")

#---load the data
df_vf <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Veg&Fruit and others"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_wheat","GDD_rice",
                                 "GDD_soybean","pet")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),"Unknown"),
    kg_clim  =  as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_vf<-na.omit(df_vf)
mod_data<-df_sel_vf
names(mod_data)
dim(mod_data)

# path_model<-"./output/nt/veg_fruits/model/"
# path_stat<-"./output/nt/veg_fruits/stat/"
# path_graph<-"./output/nt/veg_fruits/graph/"
# path_data<-"./output/nt/veg_fruits/data/"
# spacevar<-"region"
# es_rfp<-"nt_es_vf.tif"

# set paths
path_model_vf<-"./output/nt/veg_fruits/model/"
path_stat_vf<-"./output/nt/veg_fruits/stat/"
path_graph_vf<-"./output/nt/veg_fruits/graph/"
path_data_vf<-"./output/nt/veg_fruits/data/"
spacevar_vf<-"regions"
es_rfp_vf<-"nt_es_vf.tif"

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

dir_vf<-"./output/nt/veg_fruits"
unc_rfp_vf<-"nt_unc_vf.tif"

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

#-----------@ 5. Rice @--------------------------------------------------------#
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/crops_rfp_regression.R")

#---load the data
df_r <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Rice"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_wheat",
                                 "GDD_soybean","pet")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),"Unknown"),
    kg_clim  =  as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_r<-na.omit(df_r)
mod_data<-df_sel_r
names(mod_data)
dim(mod_data)
# 
# path_model<-"./output/nt/rice/model/"
# path_stat<-"./output/nt/rice/stat/"
# path_graph<-"./output/nt/rice/graph/"
# path_data<-"./output/nt/rice/data/"
# spacevar<-"region"
# es_rfp<-"nt_es_r.tif"

# set paths
path_model_r<-"./output/nt/rice/model/"
path_stat_r<-"./output/nt/rice/stat/"
path_graph_r<-"./output/nt/rice/graph/"
path_data_r<-"./output/nt/rice/data/"
spacevar_r<-"regions"
es_rfp_r<-"nt_es_r.tif"

# Modelling
crops_rfp_reg(df_sel_r,
        path_model_r,
        path_stat_r,
        path_data_r,
        path_graph_r,
        spacevar_r,
        es_rfp_r)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_r<-"./output/nt/rice"
unc_rfp_r<-"nt_unc_r.tif"

rfp_unc(path_model_r,
    path_graph_r,
    dir_r,
    unc_rfp_r)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_r,
         path_graph_r,
         path_stat_r,
         path_data_r)


#-----------@ 6. Soybean @-----------------------------------------------------#
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/crops_rfp_regression.R")

#---load the data
df_s <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Soybean"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_wheat","GDD_rice",
                                 "pet")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),"Unknown"),
    kg_clim  =  as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_s<-na.omit(df_s)
mod_data<-df_sel_s
names(mod_data)
dim(mod_data)

# path_model<-"./output/nt/soybean/model/"
# path_stat<-"./output/nt/soybean/stat/"
# path_graph<-"./output/nt/soybean/graph/"
# path_data<-"./output/nt/soybean/data/"
# spacevar<-"region"
# es_sfp<-"nt_es_s.tif"

# set paths
path_model_s<-"./output/nt/soybean/model/"
path_stat_s<-"./output/nt/soybean/stat/"
path_graph_s<-"./output/nt/soybean/graph/"
path_data_s<-"./output/nt/soybean/data/"
spacevar_s<-"regions"
es_rfp_s<-"nt_es_s.tif"

# Modelling
crops_rfp_reg(df_sel_s,
        path_model_s,
        path_stat_s,
        path_data_s,
        path_graph_s,
        spacevar_s,
        es_rfp_s)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_s<-"./output/nt/soybean"
unc_rfp_s<-"nt_unc_s.tif"

rfp_unc(path_model_s,
    path_graph_s,
    dir_s,
    unc_rfp_s)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_s,
         path_graph_s,
         path_stat_s,
         path_data_s)

#-----------@ 7. cash crop @---------------------------------------------------#
rm(list = ls())

library(terra)
library(readxl)
library(tidyverse)

source("./functions/CV_crops_rfp_regression.R")

#---load the data
df_ca <- read_xlsx("./input/rfps_data.xlsx",sheet = "full_data") %>%
  filter(key %in% c("NT"), Crop_Group %in% c("Cash crop"), !is.na(Crop_Group)) %>%
  select(effectSize,sand:wrb, -c("GDD_maize","GDD_wheat","GDD_rice",
                                 "GDD_soybean","pet")) %>%
  mutate(
    landform = fct_na_value_to_level(as.factor(landform), "Unknown"),
    wrb      = fct_na_value_to_level(as.factor(wrb),"Unknown"),
    kg_clim  =  as.factor(kg_clim),
    regions  = as.factor(regions))  

# Select relevant variables
df_sel_ca<-na.omit(df_ca)
mod_data<-df_sel_ca
names(mod_data)
dim(mod_data)

# path_model<-"./output/nt/soybean/model/"
# path_catat<-"./output/nt/soybean/stat/"
# path_graph<-"./output/nt/soybean/graph/"
# path_data<-"./output/nt/soybean/data/"
# spacevar<-"region"
# es_cafp<-"nt_es_ca.tif"

# set paths
path_model_ca<-"./output/nt/cash_crop/model/"
path_stat_ca<-"./output/nt/cash_crop/stat/"
path_graph_ca<-"./output/nt/cash_crop/graph/"
path_data_ca<-"./output/nt/cash_crop/data/"
spacevar_ca<-"regions"
es_rfp_ca<-"nt_es_ca.tif"

# Modelling
cv_crops_rfp_reg(df_sel_ca,
        path_model_ca,
        path_stat_ca,
        path_data_ca,
        path_graph_ca,
        spacevar_ca,
        es_rfp_ca)

# 2. Uncertainties
source("./functions/unc_0.05_0.95.R")

dir_ca<-"./output/nt/cash_crop"
unc_rfp_ca<-"nt_unc_ca.tif"

rfp_unc(path_model_ca,
    path_graph_ca,
    dir_ca,
    unc_rfp_ca)

# 3. Shapley values
source("./functions/shapley_values.R")
shap_val(path_model_ca,
         path_graph_ca,
         path_stat_ca,
         path_data_ca)
















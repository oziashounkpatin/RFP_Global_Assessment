#------------------------------------------------------------------------------#
#------------ af management ---------------------------------------------------#
#------------------------------------------------------------------------------#

# 1. Regression

library(terra)
library(readxl)
library(tidyverse)


source("./functions/rfp_regression.R")

#---load the data
df_af <- read_xlsx("./input/rfps_data.xlsx", sheet = "AF_Agroforestry")



# Provide function variables 
path_model<-"./output/af/model/"
path_stat<-"./output/af/stat/"
path_graph<-"./output/af/graph/"
path_data<-"./output/af/data/"
es_map<-"af_es"

# Create the folders (silently if they already exist)
paths_to_make <- c("./output/", "./output/af/", path_model, path_stat, path_graph, path_data)
invisible(lapply(paths_to_make, dir.create, recursive = TRUE, showWarnings = FALSE))

rap_reg(df_af,
        area_grid,
        path_model,
        path_stat,
        path_data,
        path_graph,
        es_map)

# 2. Uncertainties

source("./functions/unc_0.05_0.95.R")

dir<-"./output/AF/"
unc_map<-"af_unc"

rap_unc(path_model,
    path_graph,
    dir,
    unc_map)

# 3. Shapley values

source("./functions/shapley_values.R")

shap_val(path_model,path_graph)
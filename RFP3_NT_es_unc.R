#------------------------------------------------------------------------------#
#------------ NT management ---------------------------------------------------#
#------------------------------------------------------------------------------#

library(terra)
library(readxl)
library(tidyverse)


source("./functions/rfp_regression.R")

#---load the data
df_NT <- read_xlsx("./input/rfps_data.xlsx", sheet = "NT_No-tillage")



# Provide function variables 
path_model<-"./output/NT/model/"
path_stat<-"./output/NT/stat/"
path_graph<-"./output/NT/graph/"
path_data<-"./output/NT/data/"
es_map<-"nt_es"

# Create the folders (silently if they already exist)
paths_to_make <- c("./output/", "./output/NT/", path_model, path_stat, path_graph, path_data)
invisible(lapply(paths_to_make, dir.create, recursive = TRUE, showWarnings = FALSE))


rap_reg(df_NT,
        area_grid,
        path_model,
        path_stat,
        path_data,
        path_graph,
        es_map)

# 2. Uncertainties

source("./script/uncertainties/unc_0.05_0.95.R")

dir<-"./output/NT/"
unc_map<-"nt_unc"

rap_unc(path_model,
    path_graph,
    dir,
    unc_map)

# 3. Shapley values

source("./functions/shapley_values.R")

shap_val(path_model,path_graph)
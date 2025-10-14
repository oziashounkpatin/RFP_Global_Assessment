#------------------------------------------------------------------------------#
#------------ CC management ---------------------------------------------------#
#------------------------------------------------------------------------------#

library(terra)
library(readxl)
library(tidyverse)


source("./functions/rfp_regression.R")

#---load the data
df_cc <- read_xlsx("./input/rfps_data.xlsx", sheet = "CC_Cover_Crop")



# Provide function variables 
path_model<-"./output/cc/model/"
path_stat<-"./output/cc/stat/"
path_graph<-"./output/cc/graph/"
path_data<-"./output/cc/data/"
es_map<-"cc_es"

# Create the folders (silently if they already exist)
paths_to_make <- c("./output/", "./output/cc/", path_model, path_stat, path_graph, path_data)
invisible(lapply(paths_to_make, dir.create, recursive = TRUE, showWarnings = FALSE))


rap_reg(df_cc,
        area_grid,
        path_model,
        path_stat,
        path_data,
        path_graph,
        es_map)

# 2. Uncertainties

source("./functions/unc_0.05_0.95.R")

dir<-"./output/CC/"
unc_map<-"cc_unc"

rap_unc(path_model,
    path_graph,
    dir,
    unc_map)

# 3. Shapley values

source("./functions/shapley_values.R")

shap_val(path_model,path_graph)
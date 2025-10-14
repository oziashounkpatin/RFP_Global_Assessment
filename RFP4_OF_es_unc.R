#------------------------------------------------------------------------------#
#------------ of management ---------------------------------------------------#
#------------------------------------------------------------------------------#

rap_reg<-function(mod_data,
                     area_grid,
                     path_model,
                     path_stat,
                     path_data,
                     path_graph){

library(tidyverse)
library(caret)
library(CAST)
library(readxl)
library(writexl)
library(ranger)
library(mltools)
library(data.table)
library(caret)
library(terra)
library(sf)
library(latticeExtra)
library(doParallel)
library(gridExtra)
library(mice)
library(yardstick)
library(corrtable)

source("./script/RF_modelling/myFun_ffsRanger.R")

#---Load grid
area_grid<-rast("./input/areagrid.tif")
  
#----------------------------------------------------------------------------#
#------------------- Data preparation----------------------------------------#
#----------------------------------------------------------------------------#

df_sel<-mod_data

## Specify split rate
split <- 0.8

#### Partition the table into training and testing 
#### Seed ensures that same split is done for each soil parameter
set.seed(300)
trainIndex <- createDataPartition(c(df_sel["effectSize"], 
                                    recursive=T), p=split, list = F)

#### Training samples
df_training <- df_sel[trainIndex,1:ncol(df_sel)]

#### Testing samples
df_testing <- df_sel[-trainIndex,1:ncol(df_sel)]

dim(df_training)
dim(df_testing)

#-----------------------------------------------------------------------------#
#------------------ Remove zero or near zero variance ------------------------#
#-----------------------------------------------------------------------------#
tr_df_red<-df_training %>% select(-landform,-regions,-wrb)
tr_nzv<- caret::nearZeroVar(tr_df_red,saveMetrics=TRUE, names = T)

  # Shortlist of NZV features
tr_nzv_to_remove <-
  tr_nzv %>% 
  filter(zeroVar==T | nzv==T) %>% 
  rownames_to_column("col_names")

# Let's remove our NZV variable
trdf_nzvfilter <-
  tr_df_red %>% select(-pull(tr_nzv_to_remove, col_names))

#-----------------------------------------------------------------------------#
#------------------ Remove highly correlated variables  ----------------------#
#-----------------------------------------------------------------------------#

# Remove highly correlated variables
tr_cor_df_sel<-trdf_nzvfilter
tr_cor_df <- cor(tr_cor_df_sel)
tr_hc <- findCorrelation(tr_cor_df, cutoff<-0.75)
tr_hc <- sort(tr_hc)
df_tr_filtered <- tr_cor_df_sel[,-c(tr_hc)]

cordf <- df_tr_filtered %>% 
  as.matrix %>%
  cor %>%
  as.data.frame %>%
  rownames_to_column(var = 'var1') %>%
  gather(var2, value, -var1)

save_correlation_matrix(df_tr_filtered, 
                        digits = 2,
                        use = "lower",
                        replace_diagonal = T, 
                        filename = str_c(path_stat,#stat_output,
                                         "corr_df_data_na_rm.csv"))

write_xlsx(cordf, 
           col_names=T,
           path = str_c(path_stat,#stat_output, 
                        "corr_pair_data_na_rm.xlsx")
)


#-----------------------------------------------------------------------------#
#------------------ Adjust testing columns  ----------------------------------#
#-----------------------------------------------------------------------------#
training<-bind_cols(df_tr_filtered,df_training[,c("landform","wrb","regions")])
testing <-df_testing[,names(training)]

# Get predictors
predictors_CH <- training %>% dplyr:: select(-effectSize,-regions)
names(predictors_CH)

# Get response variable
response_CH <-training%>% dplyr:: select(effectSize)
response_CH <-as.numeric(unlist(response_CH))

#----------------------------------------------------------------------------#
#------------------ MODELLING ------------------------------------------------#
#-----------------------------------------------------------------------------#

# Trtraining# Training and tuning controls -----------------------------------#

set.seed(7) # 50
# 10 folds, class="group_UN",all UN ID represented (when possible)
spatial_folds_hex <- CreateSpacetimeFolds(training, spacevar="regions", k=7) 
spatial_folds_hex<-as.matrix(spatial_folds_hex)

ctrl_hex <- trainControl(method="repeatedcv",
                         repeats = 3,
                         savePredictions = TRUE,
                         index=spatial_folds_hex$index,
                         indexOut=spatial_folds_hex$indexOut)

tgrid <-  expand.grid(mtry = length(predictors_CH),
                      splitrule = "extratrees",
                      min.node.size = c(1,3,5))

#----------------------------------------------------------------------------#
#------------------- Modelling RF mean --------------------------------------#
#----------------------------------------------------------------------------#

 rf_fit <- ffsRanger(predictors_CH,
                     response_CH,
                     method="ranger",
                     seed = 20, #20
                     metric="RMSE",
                     tuneGrid=tgrid,
                     trControl = ctrl_hex,
                     num.trees=500,
                     num.threads = 12,
                     importance = "permutation")

saveRDS(rf_fit, str_c(path_model, "model_effectSize.rds"))
#rf_fit<-readRDS(str_c(path_model, "model_effectSize.rds"))


# Extract model accuracy metrics
model_output<-as.data.frame(getTrainPerf(rf_fit))

# Make prediction
pred_train<-as.data.frame(predict(rf_fit,predictors_CH))
training_df<-training
training_df$pred_train<-pred_train$`predict(rf_fit, predictors_CH)`

pred<-as.data.frame(predict(rf_fit,testing[,names(predictors_CH)]))
testing$pred<-pred$`predict(rf_fit, testing[, names(predictors_CH)])`

# Get prediction statistics
#defaultSummary(data, lev = NULL, model = NULL)
obs <- as.data.frame(testing["effectSize"])
names(obs) <- NULL

# Get validation error metrics with val data
val_output<- caret::postResample(pred, obs)

val_data<-testing %>% dplyr::select(effectSize,pred)%>%
  dplyr::rename(obs=effectSize)%>%
  as.data.frame()

# Compute Lin’s concordance correlation coefficient (CCC)
ccc_train<-yardstick::ccc(training_df,effectSize,pred_train)
ccc_val<-yardstick::ccc(val_data,obs,pred)
ccc_bind<-bind_rows(ccc_train,ccc_val)

# Bind model and validation output
mod<-model_output %>% 
  dplyr:: rename(R2=2,RMSE=1) %>% 
  add_column(Type="model") %>% 
  dplyr:: select(5,1,2)

val<-  as.data.frame(val_output) %>% 
  rownames_to_column() %>% 
  gather(variable, value, -rowname) %>% 
  spread(rowname, value) %>% 
  add_column(Type="val") %>% 
  dplyr:: rename(R2=4,RMSE=3) %>% 
  dplyr:: select(5,3,4)

mod_val_metrics<-bind_cols(
  bind_rows(mod,val),
  ccc_bind$.estimate) %>%
  dplyr::rename(CCC=4)

# Write all results to folders
write.table(mod_val_metrics, 
            row.names=FALSE,
            file = str_c(path_stat, 
                         "mod_val_metrics.txt"), 
            sep = ";", dec = ".")

write.table(val_data, row.names=FALSE,
            file = str_c(path_data,
                         "val_data.txt"),
            sep = ";", dec = ".")

write.table(training, row.names=FALSE,
            file = str_c(path_data,
                         "train_data.txt"),
            sep = ";", dec = ".")

# Get correlation for selected variables only
options(max.print=100000)
cor_data<-training[,names(rf_fit$trainingData[,1:(ncol(rf_fit$trainingData)-1)])]
cor_rf_fit<-bind_cols(training[,1],cor_data)

save_correlation_matrix(cor_rf_fit, 
                        digits = 2,
                        use = "lower",
                        replace_diagonal = T, 
                        filename = str_c(path_stat,#stat_output,
                                         "corr_rf_fit.csv"))

#----------------------------------------------------------------------------#
#------------------- Mapping ------------------------------------------------#
#----------------------------------------------------------------------------#

#Create grid for mapping
r_grid<-subset(area_grid,names(rf_fit$ptype))
r_grid_na.rm <- terra::subst(r_grid, NA, 999)


# #Upload regional vector
reg<- st_read("./input/boundaries/ne_10m_admin_0_countries.shp")

# remove antartica
reg_sf<-reg %>%
  dplyr::select(REGION_WB,SOVEREIGNT)%>%
  filter(!(REGION_WB == "Antarctica"))

 # grid_sf<- terra::mask(crop(r_grid,
 #                           vect(reg_sf)),
 #                      vect(reg_sf),
 #                      touches = FALSE)

grid_sf<- terra::mask(crop(r_grid,
                           vect(reg_sf)),
                      vect(reg_sf),
                      touches = FALSE)

# Predict the map
map_effectSize<-terra::predict(grid_sf,rf_fit,na.rm=TRUE)
plot_effectSize<-plot(map_effectSize)

# Save the created maps
fpn <- file.path(path_graph, "map_effectSize.tif") 
writeRaster(map_effectSize,fpn,overwrite=TRUE)

return (list(mod_val_metrics))

doParallel::stopImplicitCluster()
}

library(readxl)
library(tidyverse)

#---load the data
df_of <- read_xlsx("./input/rfps_data.xlsx", sheet = "OF_organic_farming")

# Provide function variables 
path_model<-"./output/of/model/"
path_stat<-"./output/of/stat/"
path_graph<-"./output/of/graph/"
path_data<-"./output/of/data/"
es_map<-"of_es"

# Create the folders (silently if they already exist)
paths_to_make <- c("./output/", "./output/of/", path_model, path_stat, path_graph, path_data)
invisible(lapply(paths_to_make, dir.create, recursive = TRUE, showWarnings = FALSE))


rap_reg(df_of,
      area_grid,
      path_model,
      path_stat,
      path_data,
      path_graph,
      es_map)

# 2. Uncertainties

source("./script/uncertainties/unc_0.05_0.95.R")

dir<-"./output/OF/"
unc_map<-"of_unc"

rap_unc(path_model,
    path_graph,
    dir,
    unc_map)

# 3. Shapley values

source("./functions/shapley_values.R")

shap_val(path_model,path_graph)

rap_unc<-function(path_model,path_graph, dir,unc_map)
{

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
library(tidyterra)
library(data.table)


# Create folder to host the tiles 
create_folder<-function(dir)
  
{   library(stringr)
  output_dir1 <- file.path(str_c(dir, "local_tiles"))
  if (!dir.exists(output_dir1)){
    dir.create(output_dir1)
  } else {
    print("Dir already exists!")
  }
  
}

create_folder(dir)

# Load the model
rf_fit<-readRDS(str_c(path_model, "model_effectSize.rds"))

#---Load grid
area_grid<-rast("./input/areagrid.tif")

# Subset model variables
r_grid<-terra::subset(area_grid,names(rf_fit$ptype))

# Create path for named tiles
dir_tiles<-paste(str_c(dir,"local_tiles/"))
filename <- paste0(dir_tiles,"tile", "_.tif")

# Create the tiles
ff<-makeTiles(r_grid, 2000, 
              extend=T, buffer=5,overwrite=T)

# read each file as raster an
grid<-purrr:::map(ff,~ rast(.x))

#------------------------------------------------------------------------------#
#------------------- Modelling RF quantiles -----------------------------------#
#------------------------------------------------------------------------------#

# This should give the training data with the selected variables by 
# Random Forest
xtrain<-(rf_fit$trainingData)[1:(ncol(rf_fit$trainingData)-1)]
ytrain<-(rf_fit$pred$obs)

qrf_data<-cbind(ytrain,xtrain) |>
  dplyr::rename(effectSize=ytrain)

fm <- as.formula(paste("effectSize ~ ", paste0(names(xtrain), collapse="+")))

qrf_fit <- ranger(fm,
                  data = na.omit(qrf_data),
                  seed = 20, 
                  num.trees=500,
                  num.threads = 12,
                  importance = "permutation",
                  #write.forest = TRUE,
                  quantreg = TRUE,
                  mtry = as.data.frame(rf_fit$bestTune)[1,1], 
                  min.node.size = as.data.frame(rf_fit$bestTune)[1,4] 
                  )

# tile_grid<- grid[[1]]
#tile_grid<- terra::subst(tile_grid, NA, 999)

quantile_pred<-function(tile_grid,qrf_fit,qtile){
  
  library(ranger)
  library(data.table)
  library(terra)
  library(caret)
  library(operators)
  
  # convert the covariates into a dataframe (data.table object)
  xDt <- data.table(values(tile_grid))
  
  # identify number the cell IDs without NAs
  idx <- which(complete.cases(xDt))
  df_qrf<-as.data.frame(xDt[idx])
  
  # df_qrf<- df_qrf %>% 
  #   mutate(across(where(is.numeric), ~ coalesce(., median(., na.rm = TRUE))))
  
  # df_qrf %>%
  #   select(everything()) %>%  # replace to your needs
  #   summarise_all(funs(sum(is.na(.))))
  
  # checking if-else if ladder
  if("landform" %in% colnames(df_qrf) & "wrb"  %!in% colnames(df_qrf)){
    df_qrf$landform<-as.factor( df_qrf$landform)
    # predict
    qpred <- terra::predict(object = qrf_fit,
                            data = df_qrf, 
                            type = "quantiles", 
                            quantiles = qtile,
                            na.rm=TRUE)  
    # Datatable to image again
    r_grid <- tile_grid[[1]]
    names(r_grid) <- "pred_quant"
    r_grid[!is.na(r_grid$pred_quant)] <- NaN
    r_grid[idx] <- qpred$predictions
    plot(r_grid)
    
  }else{
    if("wrb" %in% colnames(df_qrf) & "landform"  %!in% colnames(df_qrf)){
      df_qrf$wrb<-as.factor( df_qrf$wrb)
      
      # predict
      qpred <- terra::predict(object = qrf_fit,
                              data = df_qrf, 
                              type = "quantiles", 
                              quantiles = qtile,
                              na.rm=TRUE)  
      # Datatable to image again
      r_grid <- tile_grid[[1]]
      names(r_grid) <- "pred_quant"
      r_grid[!is.na(r_grid$pred_quant)] <- NaN
      r_grid[idx] <- qpred$predictions
      plot(r_grid)
      
    }else{
      if ("wrb" %in% colnames(df_qrf) & "landform"  %in% colnames(df_qrf)){
        df_qrf$wrb<-as.factor( df_qrf$wrb)
        df_qrf$landform<-as.factor( df_qrf$landform)
        
        # predict
        qpred <- terra::predict(object = qrf_fit,
                                data = df_qrf, 
                                type = "quantiles", 
                                quantiles = qtile,
                                na.rm=TRUE)  
        # Datatable to image again
        r_grid <- tile_grid[[1]]
        names(r_grid) <- "pred_quant"
        r_grid[!is.na(r_grid$pred_quant)] <- NaN
        r_grid[idx] <- qpred$predictions
        plot(r_grid)
      }
      else{
        
        # predict
        qpred <- terra::predict(object = qrf_fit,
                                data = df_qrf, 
                                type = "quantiles", 
                                quantiles = qtile,
                                na.rm=TRUE)  
        # Datatable to image again
        r_grid <- tile_grid[[1]]
        names(r_grid) <- "pred_quant"
        r_grid[!is.na(r_grid$pred_quant)] <- NaN
        r_grid[idx] <- qpred$predictions
        plot(r_grid)
      }
    }
  }
      return(r_grid)

}


# Mapping lower bound values

pred0.05_1 <- quantile_pred(grid[[1]],qrf_fit,0.05)
pred0.05_2 <- quantile_pred(grid[[2]],qrf_fit,0.05)
pred0.05_3 <- quantile_pred(grid[[3]],qrf_fit,0.05)

# Mapping upper bound values
pred0.95_1 <- quantile_pred(grid[[1]],qrf_fit,0.95)
pred0.95_2 <- quantile_pred(grid[[2]],qrf_fit,0.95)
pred0.95_3 <- quantile_pred(grid[[3]],qrf_fit,0.95)


# Mapping 90 % confidence interval
pred_0.90_1<-pred0.95_1 - pred0.05_1
pred_0.90_2<-pred0.95_2 - pred0.05_2
pred_0.90_3<-pred0.95_3 - pred0.05_3

# Mosaic rasters
rlist_0.5 <- list(pred0.05_1, pred0.05_2,pred0.05_3)
clist1<- sprc(rlist_0.5)
mos_0.5 <- mosaic(clist1)

rlist_0.95 <- list(pred0.95_1, pred0.95_2,pred0.95_3)
clist2 <- sprc(rlist_0.95)
mos_0.95 <- mosaic(clist2)

rlist_0.90 <- list(pred_0.90_1, pred_0.90_2,pred_0.90_3)
clist3 <- sprc(rlist_0.90)
mos_0.90 <- mosaic(clist3)

unc<-c(mos_0.5,mos_0.90,mos_0.95)
names(unc)<-c("lower_bound", "range_interval", "upper_bound")

# #Upload regional vector
reg_sf<- st_read("./boundaries/regions.shp")

unc_reg_sf<- mask(crop(unc,
                       vect(reg_sf)),
                  vect(reg_sf),
                  touches = FALSE)

# Save the created maps
fpn <- file.path(path_graph, unc_map) 
writeRaster(unc_reg_sf,fpn,overwrite=TRUE)

return(unc_reg_sf)
}

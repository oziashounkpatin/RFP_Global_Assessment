
#------------------------------------------------------------------------------#
#------------ All management and crops ----------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/mega/model/"
path_graph<-"./output/mega/graph/"
dir<-"./output/mega/data/"
v_shap(path_model,path_graph)


#------------------------------------------------------------------------------#
#------------ NT --------------------------------------------------------------#
#------------------------------------------------------------------------------#


#-----------@ Maize @----------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/maize/model/"
path_graph<-"./output/NT/Regions/maize/graph/"
dir<-"./output/NT/Regions/maize/"
v_shap(path_model,path_graph)

#-----------@ wheat @----------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/wheat/model/"
path_graph<-"./output/NT/Regions/wheat/graph/"
dir<-"./output/NT/Regions/wheat/"
v_shap(path_model,path_graph)

#-----------@ rice @-----------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/rice/model/"
path_graph<-"./output/NT/Regions/rice/graph/"
dir<-"./output/NT/Regions/rice/"
v_shap(path_model,path_graph)

#-----------@ soybean @--------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/soybean/model/"
path_graph<-"./output/NT/Regions/soybean/graph/"
dir<-"./output/NT/Regions/soybean/"
v_shap(path_model,path_graph)

#-----------@ cereal @---------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/cereal/model/"
path_graph<-"./output/NT/Regions/cereal/graph/"
dir<-"./output/NT/Regions/cereal/"
v_shap(path_model,path_graph)

#-----------@ cash_crop @------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/cash_crop/model/"
path_graph<-"./output/NT/Regions/cash_crop/graph/"
dir<-"./output/NT/Regions/cash_crop/"
v_shap(path_model,path_graph)


#-----------@ veg_fruits @-----------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/NT/Regions/veg_fruits/model/"
path_graph<-"./output/NT/Regions/veg_fruits/graph/"
dir<-"./output/NT/Regions/veg_fruits/"
v_shap(path_model,path_graph)

#------------------------------------------------------------------------------#
#------------ Regions/AF --------------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/AF/Regions/maize/model/"
path_graph<-"./output/AF/Regions/maize/graph/"
dir<-"./output/AF/Regions/maize/"
v_shap(path_model,path_graph)

#-----------@ cereal @---------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/AF/Regions/cereal/model/"
path_graph<-"./output/AF/Regions/cereal/graph/"
dir<-"./output/AF/Regions/cereal/"
v_shap(path_model,path_graph)

#-----------@ veg_fruits @-----------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/AF/Regions/veg_fruits/model/"
path_graph<-"./output/AF/Regions/veg_fruits/graph/"
dir<-"./output/AF/Regions/veg_fruits/"
v_shap(path_model,path_graph)

#------------------------------------------------------------------------------#
#------------ OF --------------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/OF/Regions/maize/model/"
path_graph<-"./output/OF/Regions/maize/graph/"
dir<-"./output/OF/Regions/maize/"
v_shap(path_model,path_graph)

#-----------@ wheat @----------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/OF/Regions/wheat/model/"
path_graph<-"./output/OF/Regions/wheat/graph/"
dir<-"./output/OF/Regions/wheat/"
v_shap(path_model,path_graph)

#-----------@ cereal @---------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/OF/Regions/cereal/model/"
path_graph<-"./output/OF/Regions/cereal/graph/"
dir<-"./output/OF/Regions/cereal/"
v_shap(path_model,path_graph)

#-----------@ veg_fruits @-----------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/OF/Regions/veg_fruits/model/"
path_graph<-"./output/OF/Regions/veg_fruits/graph/"
dir<-"./output/OF/Regions/veg_fruits/"
v_shap(path_model,path_graph)

#------------------------------------------------------------------------------#
#------------ CC/Regions ------------------------------------------------------#
#------------------------------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/CC/Regions/maize/model/"
path_graph<-"./output/CC/Regions/maize/graph/"
dir<-"./output/CC/Regions/maize/"
v_shap(path_model,path_graph)

#-----------@ wheat @----------------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/CC/Regions/wheat/model/"
path_graph<-"./output/CC/Regions/wheat/graph/"
dir<-"./output/CC/Regions/wheat/"
v_shap(path_model,path_graph)

#-----------@ veg_fruits @-----------------------------------------------------#
rm(list = ls())
source("./script/Shapley_values_Var_Imp/compute_ind_shap.R")
path_model<-"./output/CC/Regions/veg_fruits/model/"
path_graph<-"./output/CC/Regions/veg_fruits/graph/"
dir<-"./output/CC/Regions/veg_fruits/"
v_shap(path_model,path_graph)


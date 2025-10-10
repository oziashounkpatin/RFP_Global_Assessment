
shap_val<-function(path_model,
            path_graph)
{

-
library(fastshap)
library(shapviz)
library(tidyverse)

# Load the model
rf_fit<-readRDS(str_c(path_model, "model_effectSize.rds"))

x_pred<- rf_fit$trainingData %>% select(names(rf_fit$ptype))

# Load the model
fshap<-readRDS(str_c(path_model, "fullshaps.rds"))

fshap.m <- as.matrix(fshap) 

# Create a shapviz object
sv.fshap <- shapviz(fshap.m, X = x_pred)

# Define theme
nolegend_theme<-theme(
             axis.title.x = element_blank(),
             axis.text = element_text(size = 14),
             panel.background = element_blank(),
             legend.position="none"
            ) 

legend_theme<-theme(
             axis.title.x = element_blank(),
             axis.text = element_text(size = 14),
             panel.grid.major = element_blank(),
             panel.background = element_blank()
            ) 

# Visualization of all individual Shapley values              
shap_plot<-sv_importance(sv.fshap,  kind = "beeswarm") +
                      legend_theme

noleg_shap_plot<-sv_importance(sv.fshap,  kind = "beeswarm") +
                     nolegend_theme

ggsave(shap_plot,
        filename = str_c(path_graph, "shap_V",'.pdf'),
        width = 150,height=40, units='mm', dpi = 450)

ggsave(noleg_shap_plot,
        filename = str_c(path_graph, "shap_V",'.png'),
        width = 5,height=5, units='cm', dpi = 450)
}

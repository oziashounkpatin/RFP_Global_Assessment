Code from manuscript: 'Where regenerative farming practices could increase yields: A global assessment'

Kpade O. L. Hounkpatin, Emanuela De Giorgi, Mika Jalava, Jeroen Poelert, Paul C. West, Matti Kummu

The ananlysis is composed of .... R scripts:

1) Regression, uncertainties and shapley values

The pipelines - RFP1_AF_es_unc, RFP2_CC_es_unc, RFP3_NT_es_unc, RFP4_OF_es_unc - train a spatially cross-validated Random Forest for each practice (AF, CC, NT, OF) to model effect sizes and generate spatial predictions across the analysis grid. It then quantifies predictive uncertainty using a quantile Random Forest to derive lower and upper bounds and their 90% interval via tiled prediction and mosaicking. Finally, it computes Shapley values to assess feature importance, providing both aggregate rankings and individual-effect visualizations.
   
3) ...
   
4) ...
   

Software requirements

Used R version: 4.2.2

Used R base packages: stats, graphics, grDevices, utils, datasets, methods, base

Used other R packages and their versions:

Core data handling:
tidyverse, dplyr, readxl, writexl, data.table, purrr, mltools, mice

Modeling and validation:
caret, CAST, ranger, yardstick, corrtable, fastshap, shapviz

Spatial data processing:
terra, sf, tidyterra

Visualization:
ggplot2, latticeExtra, gridExtra

Parallel computing:
doParallel

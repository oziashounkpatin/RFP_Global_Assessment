**Where regenerative farming practices could increase yields: A global assessment**

*Kpade O. L. Hounkpatin, Emanuela De Giorgi, Mika Jalava, Jeroen Poelert, Paul C. West, Matti Kummu*

**1. Regression, uncertainties and Shapley values**  
The pipelines — RFP1_AF_es_unc, RFP2_CC_es_unc, RFP3_NT_es_unc, RFP4_OF_es_unc — train a spatially cross-validated Random Forest for each practice (AF, CC, NT, OF) to model effect sizes and generate spatial predictions across the analysis grid. It then quantifies predictive uncertainty using a quantile Random Forest to derive lower and upper bounds and their 90% interval via tiled prediction and mosaicking. Finally, it computes Shapley values to assess feature importance, providing both aggregate rankings and individual-effect visualizations.

3) ...
   
4) ...
   

Software requirements

Used R version: 4.2.2

Used R base packages: stats, graphics, grDevices, utils, datasets, methods, base

Used other R packages and their versions:

Core data handling:
tidyverse, dplyr, readxl, writexl, data.table, purrr, mltools, mice

**Here’s a combined list of all R packages used across the _regression_, _uncertainty_, and _Shapley value_ scripts:**

- **Core data handling:**
  - `tidyverse` (2.0.0), `dplyr` (1.1.2), `readxl` (1.4.2), `writexl` (1.4.2), `data.table` (1.15.4), `purrr` (1.0.1), `mltools` (0.3.5), `mice` (3.16.0)
- **Modeling and validation:**
  - `caret` (6.0.94), `CAST` (1.0.2), `ranger` (0.16.0), `yardstick` (1.3.1), `corrtable` (0.1.1), `fastshap` (0.1.1), `shapviz` (0.9.3)
- **Spatial data processing:**
  - `terra` (1.7.71), `sf` (1.0.12), `tidyterra` (0.5.1)
- **Visualization:**
  - `ggplot2` (3.5.1), `latticeExtra` (0.6.30), `gridExtra` (2.3)
- **Parallel computing:**
  - `doParallel` (1.0.17)

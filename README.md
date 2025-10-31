**Where regenerative farming practices could increase yields: A global assessment**

*Kpade O. L. Hounkpatin, Emanuela De Giorgi, Mika Jalava, Jeroen Poelert, Paul C. West, Matti Kummu*

**1. Regression, uncertainties and Shapley values**  

The pipelines step1— RFP1_AF_es_unc, RFP2_CC_es_unc, RFP3_NT_es_unc, RFP4_OF_es_unc — train a spatially cross-validated Random Forest for each practice (AF, CC, NT, OF) to model effect sizes and generate spatial predictions across the analysis grid. It then quantifies predictive uncertainty using a quantile Random Forest to derive lower and upper bounds and their 90% interval via tiled prediction and mosaicking. Finally, it computes Shapley values to assess feature importance, providing both aggregate rankings and individual-effect visualizations.

 **2. Spatial distribution of the effect size and related uncertainty**  
 
The step2 script generates Figure 1 of the manuscript — the global map of effect sizes and uncertainties for regenerative agriculture practices — by combining ES / UNC rasters, applying cropland and biome masks, and exporting final maps. The additional Step2_Figure_1_Supplementary Table 1 generates Supplementary Table 1: % cropland by ES sign ( <0 / >0 ) and uncertainty level (low/medium/high) for AF, CC, NT, OF from the Figure 1 raster. The same Step2_Figure_1_es_unc script also produces Supplementary Figure 8 when no biome is applied. 
   
 **3. Spatial distribution of regenerative farming practices (no crop discrimination)**
 
The step 3 script produces the global map (Figure 3) of most suitable regenerative farming practices (RFP)—without crop discrimination—by combining the effect size and uncertainty for AF, CC, NT, OF, masking, class reduction (1–8), and exporting rasters/figures. The Step3_Figure_3_Table 1 script generates Table 1: coverage of regenerative farming practices by World Bank region and globally—reporting % cropland and area (million ha) for AF, CC, NT, OF and multi-RFP classes. The same Step3_Figure_3_es_unc script also produces Supplementary Figure 9 when no biome is applied. 

 **4. Spatial distribution of regenerative farming practices for different crops**
 
The step 4 script produces the global map (Figure 3) of most suitable regenerative farming practices for maize, wheat, cereal, vegetables, fruits and others. The additional Step4_Figure_4_Supplementary Table 2 script generates Supplementary Table 2: Percentage of suitability coverage at a global and regional scale across these crop groups. The same Step4_Figure_4_es_unc script also produces Supplementary Figure 10 when no biome is applied. 

 **5. Study area**
 
 The step5 Figure_5_study_area script generates Figure 4 (Study area): global point maps of field observations for AF, CC, NT, OF.

  **6. Pearson correlations**
  
 The step6 Supplementary Table 3 script produces Supplementary Table 3: Pearson correlations between each predictor and the effect size across all models (AF, CC, NT, OF by crop groups), with significance flags (* p≤0.05, ** p≤0.01, *** p≤0.001).

 **7. Modelling accuracy metrics**
 
 The step7 Supplementary Table 4 script produces Supplementary Table 4: Random Forest accuracy metrics (RMSE, R², Lin’s CCC) for Training and Validation across AF, CC, NT, OF and crop groups.

**8. Additional information**

 Other figures (e.g., Figure 2, Supplementary Figure 1-2) — Variable importance and partial dependence plots for key predictors of different regenerative farming practices (RFPs) — were created in Adobe Illustrator based on the Shapley value maps generated in Step 1. Supplementary Figure 4-7 were also assembled in Adobe Illustrator based on the effect size produced in step1 accross AF, CC, NT, OF. and crop groups. Additional input data such as raster files are deposited on Zenodo (input.zip) https://zenodo.org/records/17317054.
   

Software requirements

Used R version: 4.2.2

Used R base packages: stats, graphics, grDevices, utils, datasets, methods, base

Used other R packages and their versions:

Core data handling:
tidyverse, dplyr, readxl, writexl, data.table, purrr, mltools, mice

**Other packages:**

- **Core data handling:**
  - `tidyverse` (2.0.0), `dplyr` (1.1.2), `readxl` (1.4.2), `writexl` (1.4.2), `data.table` (1.15.4), `purrr` (1.0.1), `mltools` (0.3.5), `mice` (3.16.0)
- **Modeling and validation:**
  - `caret` (6.0.94), `CAST` (1.0.2), `ranger` (0.16.0), `yardstick` (1.3.1), `corrtable` (0.1.1), `fastshap` (0.1.1), `shapviz` (0.9.3)
- **Spatial data processing:**
  - `terra` (1.7.71), `sf` (1.0.12), `tidyterra` (0.5.1)
- **Visualization:**
  - `ggplot2` (3.5.1), `latticeExtra` (0.6.30), `gridExtra` (2.3), `tmap` (3)
- **Parallel computing:**
  - `doParallel` (1.0.17)

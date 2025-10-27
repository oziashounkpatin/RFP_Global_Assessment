library(dplyr)
library(tidyverse)
library(purrr)

# Define paths
all_path <- "./output/all/model/"
af_paths <- list(
  all = "./output/af/all/model/",
  maize = "./output/af/maize/model/",
  cereal = "./output/af/cereal/model/",
  vf = "./output/af/veg_fruits/model/"
)
cc_paths <- list(
  all = "./output/cc/all/model/",
  maize = "./output/cc/maize/model/",
  wheat = "./output/cc/wheat/model/",
  vf = "./output/cc/veg_fruits/model/"
)
nt_paths <- list(
  all = "./output/nt/all/model/",
  maize = "./output/nt/maize/model/",
  wheat = "./output/nt/wheat/model/",
  cereal = "./output/nt/cereal/model/",
  vf = "./output/nt/veg_fruits/model/"
)
of_paths <- list(
  all = "./output/of/all/model/",
  maize = "./output/of/maize/model/",
  wheat = "./output/of/wheat/model/",
  cereal = "./output/of/cereal/model/",
  vf = "./output/of/veg_fruits/model/"
)

# Load models
load_model <- function(path) readRDS(str_c(path, "model_effectSize.rds"))

model_list <- list(
  all = load_model(all_path),
  af_all = load_model(af_paths$all),
  af_m = load_model(af_paths$maize),
  af_c = load_model(af_paths$cereal),
  af_vf = load_model(af_paths$vf),
  cc_all = load_model(cc_paths$all),
  cc_m = load_model(cc_paths$maize),
  cc_w = load_model(cc_paths$wheat),
  cc_vf = load_model(cc_paths$vf),
  nt_all = load_model(nt_paths$all),
  nt_m = load_model(nt_paths$maize),
  nt_w = load_model(nt_paths$wheat),
  nt_c = load_model(nt_paths$cereal),
  nt_vf = load_model(nt_paths$vf),
  of_all = load_model(of_paths$all),
  of_m = load_model(of_paths$maize),
  of_w = load_model(of_paths$wheat),
  of_c = load_model(of_paths$cereal),
  of_vf = load_model(of_paths$vf)
)

# Extract training data
training_data_list <- map(model_list, ~ .x$trainingData %>% rename(effectSize = .outcome))

# Define labels
labels <- tibble::tibble(
  name = names(training_data_list),
  Management = c(
    rep("All management", 1),
    rep("Agroforestry", 4),
    rep("Cover crop", 4),
    rep("No-tillage", 5),
    rep("Organic farming", 5)
  ),
  Crops = c(
    "All crops",
    "All crops", "maize", "cereal", "veg_fruits",
    "All crops", "maize", "wheat", "veg_fruits",
    "All crops", "maize", "wheat", "cereal", "veg_fruits",
    "All crops", "maize", "wheat", "cereal", "veg_fruits"
  )
)

# Correlation function with significance
compute_cor_with_sig <- function(df, mgmt, crop) {
  df <- df %>% select(where(is.numeric))  # Keep only numeric columns
  vars <- setdiff(names(df), "effectSize")
  
  map_dfr(vars, function(var) {
    test <- cor.test(df[[var]], df$effectSize, method = "pearson")
    estimate <- round(test$estimate, 2)
    
    sig <- case_when(
      test$p.value <= 0.001 ~ "***",
      test$p.value <= 0.01  ~ "**",
      test$p.value <= 0.05  ~ "*",
      TRUE                  ~ ""
    )
    
    tibble(
      Management = mgmt,
      Crops = crop,
      Variables = var,
      `Effect size` = paste0(formatC(estimate, format = "f", digits = 2), sig)
    )
  })
}


# Apply to all models
cor_results <- pmap_dfr(
  list(training_data_list, labels$Management, labels$Crops),
  compute_cor_with_sig
)

cor_results_dt <- as.data.table(cor_results)

# Blank out repeated Management values within each group
cor_results_dt[, Management := {
  m <- unique(Management)
  rep(c(m, rep("", .N - 1)), length.out = .N)
}, by = .(Management, rleid(Management))]z

# Output folder
out_dir <- "./output/recap_fig/Supplementary_figure_table/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save to Excel with one sheet
out_path_xlsx <- file.path(out_dir, "Supp_Mat_table3_correlation_table.xlsx")
writexl::write_xlsx(list(Table = cor_results_dt), out_path_xlsx)
cat("Saved table to:", out_path_xlsx, "\n")


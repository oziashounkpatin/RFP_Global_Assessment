library(dplyr)
library(tidyverse)
library(purrr)
library(stringr)
library(readr)

# ----------------------------- #
# Paths (unchanged)
# ----------------------------- #
all_path <- "./output/all/stat/"
af_paths <- list(
  all   = "./output/af/all/stat/",
  maize = "./output/af/maize/stat/",
  cereal= "./output/af/cereal/stat/",
  vf    = "./output/af/veg_fruits/stat/"
)
cc_paths <- list(
  all   = "./output/cc/all/stat/",
  maize = "./output/cc/maize/stat/",
  wheat = "./output/cc/wheat/stat/",
  vf    = "./output/cc/veg_fruits/stat/"
)
nt_paths <- list(
  all   = "./output/nt/all/stat/",
  maize = "./output/nt/maize/stat/",
  wheat = "./output/nt/wheat/stat/",
  cereal= "./output/nt/cereal/stat/",
  vf    = "./output/nt/veg_fruits/stat/"
)
of_paths <- list(
  all   = "./output/of/all/stat/",
  maize = "./output/of/maize/stat/",
  wheat = "./output/of/wheat/stat/",
  cereal= "./output/of/cereal/stat/",
  vf    = "./output/of/veg_fruits/stat/"
)

# ----------------------------- #
# Loader
# ----------------------------- #
load_stat <- function(path) {
  read_delim(
    file = str_c(path, "mod_val_metrics.txt"),
    delim = ";",
    show_col_types = FALSE,
    progress = FALSE
  )
}

# ----------------------------- #
# Gather all stats (unchanged)
# ----------------------------- #
stat_list <- list(
  all    = load_stat(all_path),
  af_all = load_stat(af_paths$all),
  af_m   = load_stat(af_paths$maize),
  af_c   = load_stat(af_paths$cereal),
  af_vf  = load_stat(af_paths$vf),
  cc_all = load_stat(cc_paths$all),
  cc_m   = load_stat(cc_paths$maize),
  cc_w   = load_stat(cc_paths$wheat),
  cc_vf  = load_stat(cc_paths$vf),
  nt_all = load_stat(nt_paths$all),
  nt_m   = load_stat(nt_paths$maize),
  nt_w   = load_stat(nt_paths$wheat),
  nt_c   = load_stat(nt_paths$cereal),
  nt_vf  = load_stat(nt_paths$vf),
  of_all = load_stat(of_paths$all),
  of_m   = load_stat(of_paths$maize),
  of_w   = load_stat(of_paths$wheat),
  of_c   = load_stat(of_paths$cereal),
  of_vf  = load_stat(of_paths$vf)
)

# ----------------------------- #
# Labels derived from names(stat_list)
# ----------------------------- #
labels <- tibble(name = names(stat_list)) %>%
  mutate(
    Management = case_when(
      name == "all"            ~ "All management",
      str_starts(name, "af_")  ~ "Agroforestry",
      str_starts(name, "cc_")  ~ "Cover crop",
      str_starts(name, "nt_")  ~ "No-tillage",
      str_starts(name, "of_")  ~ "Organic farming",
      TRUE ~ NA_character_
    ),
    Crops = case_when(
      name == "all"          ~ "All crops",
      str_ends(name, "_all") ~ "All crops",
      str_ends(name, "_m")   ~ "maize",
      str_ends(name, "_w")   ~ "wheat",
      str_ends(name, "_c")   ~ "cereal",
      str_ends(name, "_vf")  ~ "Vegetable, fruits and others",
      TRUE ~ NA_character_
    )
  )

stopifnot(nrow(labels) == length(stat_list))

# ----------------------------- #
# Extract + label + set mapping
# Add (CV) to the crop label when the file has no val rows
# ----------------------------- #
extract_stat <- function(df, mgmt, crop) {
  # Has this stats file any validation rows?
  has_val <- any(df$Type == "val", na.rm = TRUE)

  # Only tag actual crops (not "All crops" or blank); avoid double-tagging
  crop_label <- if (!has_val &&
                    !is.na(crop) &&
                    crop != "" &&
                    crop != "All crops" &&
                    !str_detect(crop, "CV")) {
    paste0(crop, " (CV)")
  } else {
    crop
  }

  df %>%
    # remove out-of-fold rows
    filter(Type != "oof") %>%
    # map cv_model -> Training; keep val -> Validation; model -> Training
    mutate(Set = recode(Type,
                        model    = "Training",
                        val      = "Validation",
                        cv_model = "Training",
                        .default = Type)) %>%
    mutate(
      Management = mgmt,
      Crops      = crop_label
    ) %>%
    select(any_of(c("Set", "Management", "Crops", "N", "RMSE", "R2", "CCC")))
}

# ----------------------------- #
# Build long table
# ----------------------------- #
stat_table <- pmap_dfr(
  list(stat_list, labels$Management, labels$Crops),
  extract_stat
)

# ----------------------------- #
# Clean: drop “(CV)” rows only from Validation (if any)
# ----------------------------- #
stat_table_clean <- stat_table %>%
  filter(!(Set == "Validation" & str_detect(Crops, "CV"))) %>%
  mutate(Set = factor(Set, levels = c("Training", "Validation")))

# ----------------------------- #
# Sort: "All management" first within each Set
# ----------------------------- #
management_order <- c("All management", "Agroforestry", "Cover crop", "No-tillage", "Organic farming")

stat_table_clean <- stat_table_clean %>%
  mutate(Management = factor(Management, levels = management_order)) %>%
  arrange(Set, Management)

# ----------------------------- #
# Pretty print: blank repeated Management labels
# ----------------------------- #
stat_table_clean <- stat_table_clean %>%
  group_by(Set, Management) %>%
  mutate(Management = ifelse(row_number() == 1, as.character(Management), "")) %>%
  ungroup()

as.data.frame(stat_table_clean)

# Output folder
out_dir <- "./output/recap_fig/Supplementary_figure_table/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save to Excel with one sheet
out_path_xlsx <- file.path(out_dir, "Supp_Mat_table4_accuracy_metrics.xlsx")
writexl::write_xlsx(list(Table = stat_table_clean), out_path_xlsx)
cat("Saved table to:", out_path_xlsx, "\n")
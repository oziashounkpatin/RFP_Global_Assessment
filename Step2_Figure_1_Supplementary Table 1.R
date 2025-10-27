## --------------------- Packages ---------------------
library(dplyr)
library(ggplot2)
library(forcats)
library(terra)
library(tmap)
library(sf)
library(data.table)
library(writexl)


## --------------------- Paths & prefix ----------------
# Load figure 1 full
fig1_path<- "./output/recap_fig/fig1/r_rap_biome_agg.tif"
r_sm<-rast(fig1_path)

# Function to compute percentage table for each layer
freq_percent <- function(r, layer_name) {
  vals <- values(r)
  vals <- vals[!is.na(vals)]
  freq <- table(vals)
  perc <- 100 * freq / length(vals)
  df <- data.frame(code = as.integer(names(freq)), percent = as.numeric(perc))
  df$layer <- layer_name
  return(df)
}

# Apply to each layer
perc_list <- lapply(names(r_sm), function(nm) {
  freq_percent(r_sm[[nm]], nm)
})

# Combine all layers
perc_df <- bind_rows(perc_list)

# Map codes to categories
perc_df$category <- case_when(
  perc_df$code == 0 ~ "ES < 0: Low uncertainty",
  perc_df$code == 1 ~ "ES < 0: Medium uncertainty",
  perc_df$code == 2 ~ "ES < 0: High uncertainty",
  perc_df$code == 3 ~ "ES > 0: Low uncertainty",
  perc_df$code == 4 ~ "ES > 0: Medium uncertainty",
  perc_df$code == 5 ~ "ES > 0: High uncertainty"
)

# Pivot to wide format
library(tidyr)
table_wide <- perc_df |>
  select(layer, category, percent) |>
  pivot_wider(names_from = layer, values_from = percent, values_fill = 0)

# Add totals per category group
add_totals <- function(df, group_prefix) {
  group_df <- df[grepl(group_prefix, df$category), ]
  total_row <- summarise(group_df, across(-category, sum))
  total_row$category <- paste0(group_prefix, "Total")
  bind_rows(group_df, total_row)
}

table_final <- bind_rows(
  add_totals(table_wide, "ES < 0"),
  add_totals(table_wide, "ES > 0")
)

# Custom row order and labels
custom_order <- c(
  "ES < 0: Low uncertainty",
  "ES < 0: Medium uncertainty",
  "ES < 0: High uncertainty",
  "ES < 0Total",
  "ES > 0: Low uncertainty",
  "ES > 0: Medium uncertainty",
  "ES > 0: High uncertainty",
  "ES > 0Total"
)

custom_labels <- c(
  "Low uncertainty",
  "Medium  uncertainty",
  "High  uncertainty",
  "Total",
  "Low uncertainty",
  "Medium  uncertainty",
  "High  uncertainty",
  "Total"
)

# Apply custom order and labels
table_final <- table_final |>
  mutate(category = factor(category, levels = custom_order)) |>
  arrange(category) |>
  mutate(category = custom_labels)

# View result
print(table_final)


## --------------------- Paths & prefix ----------------
# # Output folder
# out_dir <- "./output/recap_fig/fig1/"
# dir.create("./output", showWarnings = FALSE)
# dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Output folder
out_dir <- "./output/recap_fig/Supplementary_figure_table/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save to Excel with one sheet
out_path_xlsx <- file.path(out_dir, "Supp_Mat1_fig1_effect_size_uncertainty_table.xlsx")
writexl::write_xlsx(list(Table = table_final), out_path_xlsx)
cat("Saved table to:", out_path_xlsx, "\n")



library(tidyverse)
library(terra)
library(sf)

# --------------------- Setup ---------------------
# Load rasters
r_m <- rast("./output/recap_fig/fig4/maize/maize_rap_final.tif")
r_w <- rast("./output/recap_fig/fig4/wheat/wheat_rap_final.tif")
r_c <- rast("./output/recap_fig/fig4/cereal/cereal_rap_final.tif")
r_vf <- rast("./output/recap_fig/fig4/veg_fruits/veg_fruits_rap_final.tif")
r_all <- c(r_m, r_w, r_c, r_vf)
names(r_all) <- c("r_maize", "r_wheat", "r_cereal", "r_veg_fruits")

# Management levels
m_mgmt_levels <- c("AF","CC","NT","OF","OF,CC,AF","CC,OF,AF","CC,AF,OF","other_RFA")
w_mgmt_levels <- c("CC","NT","OF","CC,NT", "NT,CC","CC,OF","< 0","other_RFA")
c_mgmt_levels <- c("AF","NT","OF","AF,NT","NT,AF")
vf_mgmt_levels <- c("AF","CC","NT","OF","CC,OF","OF,CC","NT,OF,CC","other_RFA")
mgmt_map <- list(
  r_maize = m_mgmt_levels,
  r_wheat = w_mgmt_levels,
  r_cereal = c_mgmt_levels,
  r_veg_fruits = vf_mgmt_levels
)

# Load regions
reg <- st_read("./input/boundaries/ne_10m_admin_0_countries.shp") %>%
  group_by(REGION_WB) %>%
  mutate(ID = cur_group_id()) %>%
  select(ID, REGION_WB, SOVEREIGNT) %>%
  filter(REGION_WB != "Antarctica")
reg_sf <- vect(reg)

# --------------------- Function ---------------------
generate_management_summary <- function(r, mgmt_levels, region_sf, region_field = "REGION_WB") {
  pol <- terra::project(region_sf, r)
  e <- terra::extract(r, pol, na.rm = TRUE)
  names(e)[2] <- "r_value"

  e_counts <- e %>%
    drop_na() %>%
    group_by(r_value) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(freq = 100 * n / sum(n),
           Regions = "all_regions",
           .before = r_value)

  r_reg <- rasterize(pol, r, region_field, wopt = list(names = "Regions"))
  r_reg_c <- as.data.frame(crosstab(c(r_reg, r))) %>% rename(n = Freq)
  names(r_reg_c)[2] <- "r_value"

  c_reg <- r_reg_c %>%
    group_by(Regions) %>%
    mutate(freq = 100 * n / sum(n)) %>%
    ungroup()

  counts_tbl <- bind_rows(
    e_counts %>% mutate(r_value = as.character(r_value)),
    c_reg %>% mutate(r_value = as.character(r_value))
  )

  cell_area_ha <- prod(res(r)) / 1e4
  df0 <- counts_tbl %>%
    mutate(
      area_ha     = n * cell_area_ha,
      area_ha_1e9 = area_ha / 1e9
    )

  unique_vals <- sort(unique(df0$r_value))
  value_map <- setNames(mgmt_levels, unique_vals)
  df_mgmt <- df0 %>%
    mutate(management = value_map[r_value])

  sum_by_reg <- df_mgmt %>%
    group_by(Regions, management) %>%
    summarise(area_ha_1e9 = sum(area_ha_1e9, na.rm = TRUE), .groups = "drop") %>%
    group_by(Regions) %>%
    mutate(perc = 100 * area_ha_1e9 / sum(area_ha_1e9, na.rm = TRUE)) %>%
    ungroup()

  perc_wide <- sum_by_reg %>%
    select(Regions, management, perc) %>%
    pivot_wider(names_from = Regions, values_from = perc) %>%
    mutate(row = "%")

  ha_wide <- sum_by_reg %>%
    select(Regions, management, area_ha_1e9) %>%
    pivot_wider(names_from = Regions, values_from = area_ha_1e9) %>%
    mutate(across(-management, ~ .x * 1000)) %>%
    mutate(row = "ha (x 1e6)")

  bind_rows(perc_wide, ha_wide) %>%
    mutate(row = factor(row, levels = c("%", "ha (x 1e6)"))) %>%
    arrange(management, row)
}

# --------------------- Apply to All Rasters ---------------------
mgmt_tables <- lapply(names(r_all), function(nm) {
  generate_management_summary(r_all[[nm]], mgmt_map[[nm]], reg_sf) %>%
    mutate(crop = nm, .before = row)
})

mgmt_summary_all <- bind_rows(mgmt_tables)

# --------------------- Final Reshape ---------------------
# Desired region order
region_order <- c(
  "East Asia & Pacific", "Europe & Central Asia", "Latin America & Caribbean",
  "Middle East & North Africa", "North America", "South Asia", "Sub-Saharan Africa"
)

# Named list of management levels
mgmt_map <- list(
  r_maize      = m_mgmt_levels,
  r_wheat      = w_mgmt_levels,
  r_cereal     = c_mgmt_levels,
  r_veg_fruits = vf_mgmt_levels
)

# Function to reshape one crop block with ordered management levels
reshape_crop_ordered <- function(df, crop_name, region_order, mgmt_levels) {
  df %>%
    filter(crop == crop_name, row == "%") %>%
    pivot_longer(cols = -c(crop, row, management), names_to = "region", values_to = "value") %>%
    filter(region %in% region_order) %>%
    pivot_wider(names_from = region, values_from = value) %>%
    mutate(management = factor(management, levels = mgmt_levels)) %>%
    arrange(management) %>%
    select(crop, management, all_of(region_order))
}

# Apply to all crops
final_table <- map_dfr(names(mgmt_map), function(crop) {
  reshape_crop_ordered(mgmt_summary_all, crop, region_order, mgmt_map[[crop]])
})

# View result
print(final_table)

# --------------------- save table  ---------------------
# Output folder
out_dir <- "./output/recap_fig/fig4/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Save to Excel with one sheet
out_path_xlsx <- file.path(out_dir, "Supp_Mat2_fig4_rap_per regions.xlsx")
writexl::write_xlsx(list(Table = final_table), out_path_xlsx)
cat("Saved table to:", out_path_xlsx, "\n")



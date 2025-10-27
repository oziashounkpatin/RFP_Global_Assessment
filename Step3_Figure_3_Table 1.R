# --- Libraries (load once) ---
library(tidyverse)
library(readxl)
library(writexl)
library(data.table)
library(terra)
library(sf)
library(raster)
library(tidyterra)
library(exactextractr)

#==============================#
# Function: suit_cov
#==============================#
suit_cov <- function(rap_comb) {

  # --- Read & prep admin boundaries ---
  reg <- st_read("./input/boundaries/ne_10m_admin_0_countries.shp", quiet = TRUE)

  reg <- reg %>%
    group_by(REGION_WB) %>%
    dplyr::mutate(ID = cur_group_id())

  reg_sf <- reg %>%
    dplyr::select(ID, REGION_WB, SOVEREIGNT) %>%
    filter(!(REGION_WB == "Antarctica"))

  reg_sf <- vect(reg_sf)

  # --- Load coverage raster & standardize name ---
  c_rap <- rast(rap_comb)
  if (nlyr(c_rap) != 1) {
    stop("Expected a single-layer raster in 'rap_comb'. Found: ", nlyr(c_rap))
  }
  names(c_rap) <- "class"

  # --- Reproject polygons to raster CRS ---
  pol <- terra::project(reg_sf, c_rap)

  # --- Overall counts (all regions combined) ---
  e_c_rap <- terra::extract(c_rap, pol, na.rm = TRUE)
  e_c_rap_counts <- e_c_rap %>%
    as_tibble() %>%
    drop_na(class) %>%
    group_by(class) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(
      freq    = 100 * n / sum(n),
      Regions = "all_regions",
      .before = class
    )

  # --- Counts per region via crosstab ---
  r_reg   <- rasterize(pol, c_rap, "REGION_WB", wopt = list(names = "Regions"))
  r_reg_c <- as.data.frame(crosstab(c(r_reg, c_rap)))
  names(r_reg_c)[names(r_reg_c) == "Freq"] <- "n"

  c_reg_rap <- r_reg_c %>%
    group_by(Regions) %>%
    mutate(freq = 100 * n / sum(n, na.rm = TRUE)) %>%
    ungroup()

  # --- Bind overall + per-region and harmonize types ---
  c_reg_rap_fix <- c_reg_rap %>%
    mutate(
      Regions = as.character(Regions),
      class   = as.factor(as.character(class))
    )

  e_c_rap_counts_fix <- e_c_rap_counts %>%
    mutate(
      Regions = as.character(Regions),
      class   = as.factor(as.character(class))
    )

  c_bind_rap <- bind_rows(e_c_rap_counts_fix, c_reg_rap_fix)

  # --- Cell area (assumes equal-area; for lat/long consider terra::cellSize) ---
  cell_area_ha <- prod(res(c_rap)) / 1e4

  c_bind_rap_df <- as.data.frame(c_bind_rap) %>%
    mutate(
      area_ha     = n * cell_area_ha,
      area_ha_1e9 = area_ha / 1e9
    )

  # --- Recode classes to management labels ---
  mgmt_levels <- c("AF", "CC", "NT", "OF", "AF,CC", "CC,AF", "OF,CC,AF", "other_RFP")

  df_mgmt <- c_bind_rap_df %>%
    mutate(
      management = dplyr::recode(as.character(class),
        "1"   = "AF",
        "2"   = "CC",
        "3"   = "NT",
        "4"   = "OF",
        "5"  = "AF,CC",
        "6"  = "CC,AF",
        "7" = "OF,CC,AF",
        .default = "other_RFP"
      )
    )

  # --- Summarise by region ---
  sum_by_reg <- df_mgmt %>%
    group_by(Regions, management) %>%
    summarise(area_ha_1e9 = sum(area_ha_1e9, na.rm = TRUE), .groups = "drop") %>%
    group_by(Regions) %>%
    mutate(perc = 100 * area_ha_1e9 / sum(area_ha_1e9, na.rm = TRUE)) %>%
    ungroup() %>%
    tidyr::complete(Regions, management = mgmt_levels,
                    fill = list(area_ha_1e9 = 0, perc = 0))

  # --- Two-row table per region (%, and area in x 1e6 ha) ---
  perc_wide <- sum_by_reg %>%
    dplyr::select(Regions, management, perc) %>%
    tidyr::pivot_wider(names_from = management, values_from = perc) %>%
    mutate(row = "%")

  ha_wide <- sum_by_reg %>%
    dplyr::select(Regions, management, area_ha_1e9) %>%
    tidyr::pivot_wider(names_from = management, values_from = area_ha_1e9) %>%
    mutate(across(-Regions, ~ .x * 1000)) %>%      # billions -> millions of ha
    mutate(row = "ha (x 1e6)")

  mgmt_table_2rows <- bind_rows(perc_wide, ha_wide) %>%
    mutate(row = factor(row, levels = c("%", "ha (x 1e6)")))

  # --- ensure "all_regions" first, others alphabetical ---
  regions_order <- c(
    "all_regions",
    sort(setdiff(unique(mgmt_table_2rows$Regions), "all_regions"))
  )

  mgmt_table_2rows <- mgmt_table_2rows %>%
    mutate(
      Regions = factor(Regions, levels = regions_order),
      row     = factor(row, levels = c("%", "ha (x 1e6)"))
    ) %>%
    arrange(Regions, row) %>%
    dplyr::select(Regions, row, all_of(mgmt_levels)) %>%
    mutate(across(all_of(mgmt_levels), ~ round(.x, 2)))

  # --- show region name only on the first row of each pair ---
  mgmt_table_2rows <- mgmt_table_2rows %>%
    group_by(Regions) %>%
    mutate(Regions = if_else(row == "ha (x 1e6)", "", as.character(Regions))) %>%
    ungroup()

  return(mgmt_table_2rows)
}

#==============================#
# Run
#==============================#
rap_comb   <- "./output/recap_fig/fig3/rap_final.tif"
output_path <- "./output/fig3/"

dir.create("./output", showWarnings = FALSE)
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

mgmt_table_2rows <- suit_cov(rap_comb)

mgmt_table_2rows

# (Optional) write another copy if needed
write_xlsx(mgmt_table_2rows, "./output/recap_fig/fig3/table2.xlsx")

## ------------------------------- Packages ------------------------------------
library(dplyr)
library(readxl)
library(writexl)
library(ggplot2)
library(forcats)
library(terra)
library(tmap)
library(sf)
library(data.table)

## ------------------------------- Paths ---------------------------------------
af_path_graph <- "./output/af/wheat/graph/"
cc_path_graph <- "./output/cc/wheat/graph/"
nt_path_graph <- "./output/nt/wheat/graph/"
of_path_graph <- "./output/of/wheat/graph/"

# Output folder
out_dir <- "./output/recap_fig/fig4/wheat/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Boundaries (use your actual files)
coast_path <- "./input/boundaries/coastline.shp"
land_path  <- "./input/boundaries/land_ag.shp"

# Cropland mask
mask_path <- "./input/cropland_mask/mask_crop.tif"

# Crop tag for filenames (prefix)
crop_tag  <- "wheat"
save_path <- function(name) file.path(out_dir, sprintf("%s_%s", crop_tag, name))

## --------------------- Helpers -----------------------------
load_map <- function(dir, base) {
  f <- file.path(dir, paste0(base, ".tif"))
  if (!file.exists(f)) stop("Missing file: ", f)
  rast(f)
}

plot_map <- function(r_mask, col_hex, sf_shoreLine) {
  tm_shape(r_mask, projection = "+proj=robin") +
    tm_raster(style = "cat", legend.show = FALSE, palette = c("#F0F0F0", col_hex)) +
    tm_shape(sf_shoreLine, projection = "+proj=robin") +
    tm_lines(col = "grey", lwd = 0.25) +
    tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE)
}

## --------------------------- Load ES / UNC stacks ----------------------------
es <- list(
  #af = load_map(af_path_graph, "af_es_w"),
  cc = load_map(cc_path_graph, "cc_es_w"),
  nt = load_map(nt_path_graph, "nt_es_w"),
  of = load_map(of_path_graph, "of_es_w")
)
unc <- list(
  #af = load_map(af_path_graph, "af_unc_w"),
  cc = load_map(cc_path_graph, "cc_unc_w"),
  nt = load_map(nt_path_graph, "nt_unc_w"),
  of = load_map(of_path_graph, "of_unc_w")
)

r_es  <- rast(es)  ; names(r_es)  <- c("cces","ntes","ofes")
r_unc <- rast(unc) ; r_unc <-r_unc[[grepl("_2$", names(r_unc))]]; 
names(r_unc) <- c("ccint","ntint","ofint")

## --------------------------- Cropland mask & align ---------------------------
# Load & align cropland mask
crop_mask <- rast(mask_path)

# Align to ES grid (CRS/extent/res). Use nearest-neighbor for categorical masks.
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")
}

# Apply mask to show only cropland areas (value == 1)
r_es  <- mask(r_es,  crop_mask)
r_unc <- mask(r_unc, crop_mask)

# Combine for table ops
all_mgt <- c(r_es, r_unc)


## -------- 3) Decision rules -> initial class map ----------
xDt <- data.table(values(all_mgt))
idx <- which(complete.cases(xDt))
df_all_mgt <- as.data.frame(xDt[idx])

# CC=2, NT=3, OF=4; combos used by your rules:
#   32 (CC,NT with NT more certain),
#   23 (NT,CC with CC more certain),
#   24 (CC,OF),
#   99 (<0 everywhere),
#   0 (other/none)
df_all_mgt$Class_red <- with(
  df_all_mgt,
  ifelse(ntes > 0 & ntint < ccint & ntint < ofint, 3,             
  ifelse(ntes > 0 & cces < 0 & ofes < 0,               3,

  ifelse(cces > 0 & ccint < ntint & ccint < ofint,     2,         
  ifelse(cces > 0 & ntes < 0 & ofes < 0,               2,

  ifelse(ofes > 0 & ofint < ntint & ofint < ccint,     4,         
  ifelse(ofes > 0 & ntes < 0 & cces < 0,               4,

  ifelse(ntes > 0 & cces > 0 & ofes < 0 & ntint > ccint, 23,      
  ifelse(ntes > 0 & cces > 0 & ofes < 0 & ntint < ccint, 32,      

  ifelse(ntes < 0 & cces < 0 & ofes < 0 & ntint < ccint, 99,      
  ifelse(ntes < 0 & cces > 0 & ofes > 0 & ccint > ofint, 24,      
         0
  ))))))))))
)

# Push back to raster
r_grid_red_class <- all_mgt[[1]]
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class

freq(r_red_class)

## -------- 4) Recode to plotted classes --------------------
# Final plotted classes:

r_8class_recod <- r_red_class %>%
  subst(32, 5) %>%
  subst(23, 6) %>%
  subst(24, 7) %>%
  subst(99, 8) %>%
  subst(0,  9)

# Project & aggregate for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)

## -------- 5) Biome filtering (optional but consistent) ----
# Constrain combos to the appropriate biome overlaps
biome <- rast("./input/biome_mask/RFP_biome.tif") # 4 bands: AF, CC, NT, OF
b_res <- resample(biome, red_class_pj, method = "near")

ccm <- ifel(b_res[[2]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

# Combo masks (exclude the third biome)
mask_ccnt <- ifel(!is.na(ccm) & !is.na(ntm) &  is.na(ofm), 1, NA)  # CC+NT only
mask_ntcc <- mask_ccnt                                            # same geometry; used to gate class 6
mask_ccof <- ifel(!is.na(ccm) & !is.na(ofm), 1, NA)  # CC+OF only
any_mask  <- ifel(!is.na(ccm) | !is.na(ntm) | !is.na(ofm),  1, NA)

rap <-red_class_pj

c2 <- ifel(!is.na(ccm)   & (rap == 2), 2, NA)
c3 <- ifel(!is.na(ntm)   & (rap == 3), 3, NA)
c4 <- ifel(!is.na(ofm)   & (rap == 4), 4, NA)
c5 <- ifel(!is.na(mask_ccnt) & (rap == 5), 5, NA)  # CC,NT (NT more certain)
c6 <- ifel(!is.na(mask_ntcc) & (rap == 6), 6, NA)  # NT,CC (CC more certain)
c7 <- ifel(!is.na(mask_ccof) & (rap == 7), 7, NA)  # CC,OF
c8 <- ifel(!is.na(any_mask)  & (rap == 8), 8, NA)  # <0 inside any biome
c9 <- ifel(!is.na(any_mask)  & (rap == 9), 9, NA)  # other_RFA inside any biome

rap_final <- merge(c2, c3, c4, c5, c6, c7, c8, c9)
names(rap_final) <- "RFP_class_final"

## -------- 6) Plotting ------------------------------------
red_mgtnames <- c(
  "CC", "NT", "OF",
  "CC,NT", "NT,CC",
  "CC,OF", "< 0", "other_RFA"
)
red_palette <- c(
  "#6690CB", "#CE7568", "#8CB892",
  "#FFAC1C", "#93CFC9",
  "#007FFF", "#FF0000", "#6D6BA8"
)

sf_shoreLine <- st_read(coast_path, quiet = TRUE)
pol_reg_sf   <- st_read(land_path,  quiet = TRUE)

r_agg_red<- aggregate(rap_final, fact = 4, fun = modal, na.rm = TRUE)

p_red_mgt_best <- tm_shape(pol_reg_sf, projection = "+proj=robin") +
  tm_polygons("agg_n", palette = "white", border.col = NULL, legend.show = FALSE) +
  tm_shape(r_agg_red) +
  tm_raster(style = "cat", title = "", labels = red_mgtnames, palette = red_palette) +
  tm_shape(sf_shoreLine, projection = "+proj=robin") +
  tm_lines(col = "grey", lwd = 0.25) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)

p_red_mgt_best 

png_p_red_mgt_best <- p_red_mgt_best + tm_layout(legend.show = FALSE)

## -------- 7) Save everything (prefixed wheat_) ----------
# Raster
writeRaster(rap_final, save_path("rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(p_red_mgt_best,     filename = save_path("plot_rap_best.pdf"),
          width = 80, units = "mm", dpi = 450)
tmap_save(png_p_red_mgt_best, filename = save_path("plot_rap_best.png"),
          width = 80, units = "mm", dpi = 450)

## -------- 8) Bar chart ------------------------------------
names(rap_final) <- "class"
df_r_classes <- as.data.frame(rap_final, xy = TRUE)

freq(rap_final)

perc_classes <- df_r_classes %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

# Order 2..9 to match red_mgtnames above
level_vals <- 2:9
plt_bar <- perc_classes %>%
  filter(class %in% level_vals) %>%
  mutate(class_f = factor(class, levels = level_vals, labels = red_mgtnames)) %>%
  ggplot(aes(x = forcats::fct_rev(class_f), y = as.numeric(count), fill = class_f)) +
  geom_col() +
  scale_fill_manual(values = setNames(red_palette, red_mgtnames),
                    breaks = red_mgtnames, limits = red_mgtnames) +
  geom_text(aes(label = perc), size = 7, hjust = -0.1) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_void() + theme(legend.position = "none")

ggsave(save_path("plt_bar_8class.pdf"), plt_bar,
       width = 50, height = 20, units = "cm", dpi = 450)

write_xlsx(perc_classes, save_path("perc_8class.xlsx"))

cat("Saved everything to: ", normalizePath(out_dir), "\n")

## =============================================================================
## No biome applied
## =============================================================================

# Optional: aggregate for smoother map
full_r_agg_red <- aggregate(red_class_pj, fact = 4, fun = modal, na.rm = TRUE)

full_p_red_mgt_best <- tm_shape(pol_reg_sf, projection = "+proj=robin") +
  tm_polygons("agg_n", palette = "white", border.col = NULL, legend.show = FALSE) +
  tm_shape(full_r_agg_red) +
  tm_raster(style = "cat", title = "", labels = red_mgtnames, palette = red_palette) +
  tm_shape(sf_shoreLine, projection = "+proj=robin") +
  tm_lines(col = "grey", lwd = 0.25) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)

full_p_red_mgt_best

# PNG version without legend
full_png_p_red_mgt_best <- full_p_red_mgt_best + tm_layout(legend.show = FALSE)

## ------------------------------- Save all ------------------------------------

# Save the final raster
writeRaster(red_class_pj, save_path("full_rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(full_png_p_red_mgt_best, filename = save_path("full_png_plot_red_RAP_best.png"),
          width = 100, units = "cm", dpi = 450)
tmap_save(full_p_red_mgt_best,     filename = save_path("full_png_plot_red_RAP_best.pdf"),
          width = 80, units = "cm", dpi = 450)

## =============================================================================
## Bar chart (% area per class) from rap_final
## =============================================================================
names(red_class_pj) <- "class"
full_df_r_8class_recod <- as.data.frame(red_class_pj, xy = TRUE)

full_perc_8class1 <- full_df_r_8class_recod %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count))

full_perc_8class <- full_perc_8class1 %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

# Drop class 0 if present
full_perc_8class_clean <- full_perc_8class %>% filter(class != "0")

# Order 2..9 to match red_mgtnames above
level_vals <- 2:9
full_plt_bar_8class <- full_perc_8class_clean %>%
  filter(class %in% level_vals) %>%
  mutate(class_f = factor(class, levels = level_vals, labels = red_mgtnames)) %>%
  ggplot(aes(x = forcats::fct_rev(class_f), y = as.numeric(count), fill = class_f)) +
  geom_col() +
  scale_fill_manual(values = setNames(red_palette, red_mgtnames),
                    breaks = red_mgtnames, limits = red_mgtnames) +
  geom_text(aes(label = perc), size = 7, hjust = -0.1) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_void() + theme(legend.position = "none")

ggsave(save_path("full_plt_bar_8class.pdf"), full_plt_bar_8class,
       width = 50, height = 20, units = "cm", dpi = 450)

write_xlsx(full_perc_8class1, save_path("full_perc_8class.xlsx"))


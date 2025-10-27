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
# Graph folders (AF was pointing at NT before — fixed)
af_path_graph <- "./output/af/veg_fruits/graph/"
cc_path_graph <- "./output/cc/veg_fruits/graph/"
nt_path_graph <- "./output/nt/veg_fruits/graph/"
of_path_graph <- "./output/of/veg_fruits/graph/"

# Output folder
out_dir <- "./output/recap_fig/fig4/veg_fruits/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Boundaries (keep consistent with your files)
coast_path <- "./input/boundaries/coastline.shp"
land_path  <- "./input/boundaries/land_ag.shp"

# Cropland mask
mask_path <- "./input/cropland_mask/mask_crop.tif"

# Crop tag for filenames
crop_tag  <- "veg_fruits"
save_path <- function(name) file.path(out_dir, sprintf("%s_%s", crop_tag, name))

## --------------------- Helpers ----------------------
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

## ----------- 1) Load per-management ES / UNC --------
es <- list(
  af = load_map(af_path_graph, "af_es_vf"),
  cc = load_map(cc_path_graph, "cc_es_vf"),
  nt = load_map(nt_path_graph, "nt_es_vf"),
  of = load_map(of_path_graph, "of_es_vf")
)
unc <- list(
  af = load_map(af_path_graph, "af_unc_vf"),
  cc = load_map(cc_path_graph, "cc_unc_vf"),
  nt = load_map(nt_path_graph, "nt_unc_vf"),
  of = load_map(of_path_graph, "of_unc_vf")
)

r_es  <- rast(es)  ; names(r_es)  <- c("afes","cces","ntes","ofes")
r_unc <- rast(unc) ; r_unc <-r_unc[[grepl("_2$", names(r_unc))]]; 
names(r_unc) <- c("afint","ccint","ntint","ofint")

## ----------- 2) Cropland mask & align --------------
crop_mask <- rast("./input/cropland_mask/mask_crop.tif")
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")  # categorical
}
# If mask is 1=crop, 0=non-crop:
r_es  <- mask(r_es,  crop_mask)
r_unc <- mask(r_unc, crop_mask)

all_mgt <- c(r_es, r_unc)

## -------- 3) Decision rules -> initial class map ----
xDt <- data.table(values(all_mgt))
idx <- which(complete.cases(xDt))
df_all_mgt <- as.data.frame(xDt[idx])

# AF=1, CC=2, NT=3, OF=4
# Combos: 24, 42, 342; 0 = other/none
df_all_mgt$Class_red <- with(
  df_all_mgt,
  ifelse(ntes > 0 & ntint < ccint & ntint < ofint & ntint < afint, 3,       
  ifelse(ntes > 0 & cces < 0 & ofes < 0 & afes < 0,               3,

  ifelse(cces > 0 & ccint < ntint & ccint < ofint & ccint < afint, 2,         
  ifelse(cces > 0 & ntes < 0 & ofes < 0 & afes < 0,               2,

  ifelse(ofes > 0 & ofint < ntint & ofint < ccint & ofint < afint, 4,         
  ifelse(ofes > 0 & ntes < 0 & cces < 0 & afes < 0,               4,

  ifelse(afes > 0 & afint < ntint & afint < ccint & afint < ofint, 1,          
  ifelse(afes > 0 & ntes < 0 & cces < 0 & ofes < 0,               1,

  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes < 0 & ccint > ofint, 42,        
  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes < 0 & ccint < ofint, 24,        

  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes < 0 &
         ntint < ccint & ntint < ofint & ccint > ofint,             342,      
         0
  )))))))))))
)

# Rasterize
r_grid_red_class <- all_mgt[[1]]
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class
freq(r_red_class)

## -------- 4) Recode to 8 plotted classes ----------
# Map: singles (1..4); 24->5 (CC,OF), 42->6 (OF,CC), 342->7 (NT,OF,CC); 0->8 other
r_8class_recod <- r_red_class %>%
  subst(0,   8) %>%
  subst(24,  5) %>%
  subst(42,  6) %>%
  subst(342, 7)

# Project & aggregate for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)

## -------- 5) Biome filtering (optional) -----------
# You can keep this to ensure combos only appear where those biomes co-occur
biome <- rast("./input/biome_mask/RFP_biome.tif") # 4 bands: AF, CC, NT, OF
b_res <- resample(biome, red_class_pj, method = "near")

afm <- ifel(b_res[[1]] == 1, 1, NA)
ccm <- ifel(b_res[[2]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

mask_ccof   <- ifel(!is.na(ccm) & !is.na(ofm), 1, NA) # CC+OF only
mask_ntofcc <- ifel(!is.na(ntm) & !is.na(ofm) & !is.na(ccm) &  is.na(afm), 1, NA) # NT+OF+CC only
any_mask    <- ifel(!is.na(afm) | !is.na(ccm) | !is.na(ntm) | !is.na(ofm),  1, NA)

rap <- red_class_pj

c1 <- ifel(!is.na(afm)       & (rap == 1), 1, NA)  # AF
c2 <- ifel(!is.na(ccm)       & (rap == 2), 2, NA)  # CC
c3 <- ifel(!is.na(ntm)       & (rap == 3), 3, NA)  # NT
c4 <- ifel(!is.na(ofm)       & (rap == 4), 4, NA)  # OF
c5 <- ifel(!is.na(mask_ccof) & (rap == 5), 5, NA)  # CC,OF (24)
c6 <- ifel(!is.na(mask_ccof) & (rap == 6), 6, NA)  # OF,CC (42)
c7 <- ifel(!is.na(mask_ntofcc) & (rap == 7), 7, NA)# NT,OF,CC (342)
c8 <- ifel(!is.na(any_mask)  & (rap == 8), 8, NA)  # other_RFA inside any biome

rap_final <- merge(c1, c2, c3, c4, c5, c6, c7, c8)
names(rap_final) <- "RFP_class_final"

freq(rap_final)

## -------- 6) Plotting -----------------------------
red_mgtnames <- c(
  "AF","CC","NT","OF",
  "CC,OF","OF,CC","NT,OF,CC","other_RFA"
)
red_palette <- c(
  "#EBB560", "#6690CB", "#CE7568", "#8CB892",
  "#007FFF", "#BBFFBB", "#860086", "#6D6BA8"
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

## -------- 7) Save everything (with veg_fruits_ prefix)
# Raster
writeRaster(rap_final, save_path("rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(p_red_mgt_best,     filename = save_path("plot_rap_best.pdf"),
          width = 80, units = "mm", dpi = 450)
tmap_save(png_p_red_mgt_best, filename = save_path("plot_rap_best.png"),
          width = 80, units = "mm", dpi = 450)

## -------- 8) Bar chart ----------------------------
names(rap_final) <- "class"
df_r_classes <- as.data.frame(rap_final, xy = TRUE)

perc_classes <- df_r_classes %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

plt_bar <- perc_classes %>%
  mutate(class = factor(as.integer(class), levels = 1:8, labels = red_mgtnames)) %>%
  ggplot(aes(x = forcats::fct_rev(class), y = as.numeric(count), fill = class)) +
  geom_col() +
  scale_fill_manual(values = setNames(red_palette, red_mgtnames),
                    breaks = red_mgtnames, limits = red_mgtnames) +
  geom_text(aes(label = perc), size = 7, hjust = -0.1) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_void() + theme(legend.position = "none")

ggsave(save_path("plt_bar_8class.pdf"), plt_bar, width = 50, height = 20, units = "cm", dpi = 450)
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
level_vals <- c(1,2,3,4,5,6,7,8)
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




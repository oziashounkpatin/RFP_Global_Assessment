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
af_path_graph <- "./output/af/cereal/graph/"
cc_path_graph <- "./output/cc/cereal/graph/"
nt_path_graph <- "./output/nt/cereal/graph/"
of_path_graph <- "./output/of/cereal/graph/"

# Output folder
out_dir <- "./output/recap_fig/fig4/cereal/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Boundaries (use your actual files)
coast_path <- "./input/boundaries/coastline.shp"
land_path  <- "./input/boundaries/land_ag.shp"

# Cropland mask
mask_path <- "./input/cropland_mask/mask_crop.tif"

# Crop tag for filenames
crop_tag  <- "cereal"
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

## ----------- 1) Load per-management ES / UNC --------------
es <- list(
  af = load_map(af_path_graph, "af_es_c"),
  nt = load_map(nt_path_graph, "nt_es_c"),
  of = load_map(of_path_graph, "of_es_c")
)
unc <- list(
  af = load_map(af_path_graph, "af_unc_c"),
  nt = load_map(nt_path_graph, "nt_unc_c"),
  of = load_map(of_path_graph, "of_unc_c")
)

r_es  <- rast(es)  ; names(r_es)  <- c("afes","ntes","ofes")
r_unc <- rast(unc) ; r_unc <-r_unc[[grepl("_2$", names(r_unc))]]; 
names(r_unc) <- c("afint","ntint","ofint")

## ----------- 2) Cropland mask & align ----------------------
crop_mask <- rast(mask_path)
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")  # categorical
}
# If mask is 1=crop, 0=non-crop:
r_es  <- mask(r_es,  crop_mask)
r_unc <- mask(r_unc, crop_mask)

all_mgt <- c(r_es, r_unc)

## -------- 3) Decision rules -> initial class map ----------
xDt <- data.table(values(all_mgt))
idx <- which(complete.cases(xDt))
df_all_mgt <- as.data.frame(xDt[idx])

# AF=1, NT=3, OF=4; combos: 31 (NT,AF), 41 (OF,AF); 0 = other/none
df_all_mgt$Class_red <- with(
  df_all_mgt,
  ifelse(ntes > 0 & ntint < ofint & ntint < afint, 3,                   
  ifelse(ntes > 0 & ofes < 0 & afes < 0,               3,

  ifelse(ofes > 0 & ofint < ntint & ofint < afint,     4,               
  ifelse(ofes > 0 & ntes < 0 & afes < 0,               4,

  ifelse(afes > 0 & afint < ntint & afint < ofint,     1,               
  ifelse(afes > 0 & ntes < 0 & ofes < 0,               1,

  ifelse(ntes < 0 & ofes > 0 & afes > 0 & ofint < afint, 41,            
  ifelse(ntes > 0 & ofes < 0 & afes > 0 & ntint < afint, 31,
  ifelse(ntes > 0 & ofes < 0 & afes > 0 & ntint > afint, 13,
         
  # NT,AF
  ifelse(ntes < 0 & ofes < 0 & afes < 0, 99,0
         
  
  ))))))))))
)

# Rasterize class map
r_grid_red_class <- all_mgt[[1]]
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class

df_check<-df_all_mgt %>% filter(Class_red==0)
head(df_check)
tail(df_check)

freq(r_red_class)

## -------- 4) Recode to plotted classes --------------------
# Keep: 1 (AF), 3 (NT), 4 (OF), 31 -> 5 (NT,AF), 41 -> 6 (OF,AF)
r_8class_recod <- r_red_class %>%
  subst(13, 5) %>%
  subst(31, 6) %>%
  subst(41, 7) 

# Project & aggregate for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)

## -------- 5) Biome filtering (optional but consistent) ----
biome <- rast("./input/biome_mask/RFP_biome.tif") # 4 bands: AF, CC, NT, OF
b_res <- resample(biome, red_class_pj, method = "near")

afm <- ifel(b_res[[1]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

# Only NT+AF (exclude OF) and OF+AF (exclude NT)
mask_ntaf <- ifel(!is.na(ntm) & !is.na(afm), 1, NA)
mask_ofaf  <- ifel(!is.na(ofm) & !is.na(afm), 1, NA)

rap <- red_class_pj
c1 <- ifel(!is.na(afm)     & (rap == 1), 1, NA)
c3 <- ifel(!is.na(ntm)     & (rap == 3), 3, NA)
c4 <- ifel(!is.na(ofm)     & (rap == 4), 4, NA)
c5 <- ifel(!is.na(mask_ntaf) & (rap == 5), 5, NA)
c6 <- ifel(!is.na(mask_ntaf) & (rap == 6), 6, NA)
c7 <- ifel(!is.na(mask_ofaf) & (rap == 7), 7, NA)

rap_final <- merge(c1, c3, c4, c5, c6, c7)
names(rap_final) <- "RFP_class_final"


## -------- 6) Plotting ------------------------------------
red_mgtnames <- c("AF", "NT", "OF", "AF,NT", "NT,AF", "OF,AF")
red_palette  <- c("#EBB560", 
                  "#CE7568", 
                  "#8CB892",
                  "#285799",
                  "#973526", 
                  "#008600")

sf_shoreLine <- st_read(coast_path, quiet = TRUE)
pol_reg_sf   <- st_read(land_path,  quiet = TRUE)

r_agg_red    <- aggregate(rap_final, fact = 4, fun = modal, na.rm = TRUE)

p_red_mgt_best <- tm_shape(pol_reg_sf, projection = "+proj=robin") +
  tm_polygons("agg_n", palette = "white", border.col = NULL, legend.show = FALSE) +
  tm_shape(r_agg_red) +
  tm_raster(style = "cat", title = "", labels = red_mgtnames, palette = red_palette) +
  tm_shape(sf_shoreLine, projection = "+proj=robin") +
  tm_lines(col = "grey", lwd = 0.25) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)

p_red_mgt_best  

png_p_red_mgt_best <- p_red_mgt_best + tm_layout(legend.show = FALSE)

## -------- 7) Save everything (prefixed cereal_) -----------
# Raster
writeRaster(rap_final, save_path("rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(p_red_mgt_best,     filename = save_path("plot_rap_best.pdf"),
          width = 80, units = "mm", dpi = 450)
tmap_save(png_p_red_mgt_best, filename = save_path("plot_rap_best.png"),
          width = 80, units = "mm", dpi = 450)

## --------  Bar chart ------------------------------------
names(rap_final) <- "class"
df_r_classes <- as.data.frame(rap_final, xy = TRUE)

perc_classes <- df_r_classes %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

# Order: 1,3,4,5,6,7 to match red_mgtnames
level_vals <- c(1,3,4,5,6,7)
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
level_vals <- c(1,3,4,5,6,7)
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



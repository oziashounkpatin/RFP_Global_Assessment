## --------------------- Minimal packages ---------------------
library(dplyr)
library(ggplot2)
library(forcats)
library(terra)
library(tmap)
library(sf)
library(data.table)
library(writexl)

## --------------------- Paths & prefix -----------------------
# NOTE: adjust AF path if AF rasters are not under NT
af_path_graph <- "./output/AF/cereal/graph/"
nt_path_graph <- "./output/NT/cereal/graph/"
of_path_graph <- "./output/OF/cereal/graph/"

# Output folder
out_dir <- "./output/fig_4/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Boundaries (use your actual files)
coast_path <- "./boundaries/coastline.shp"
land_path  <- "./boundaries/land_ag.shp"

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
  af = load_map(af_path_graph, "af_es"),
  nt = load_map(nt_path_graph, "nt_es"),
  of = load_map(of_path_graph, "of_es")
)
unc <- list(
  af = load_map(af_path_graph, "af_unc"),
  nt = load_map(nt_path_graph, "nt_unc"),
  of = load_map(of_path_graph, "of_unc")
)

r_es  <- rast(es)  ; names(r_es)  <- c("afes","ntes","ofes")
r_unc <- rast(unc) ; names(r_unc) <- c("afint","ntint","ofint")

## ----------- 2) Cropland mask & align ----------------------
crop_mask <- rast("./input/crop_mask/mask_crop.tif")
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")  # categorical
}
# If mask is 1=crop, 0=non-crop:
r_es  <- mask(r_es,  crop_mask, maskvalues = 0)
r_unc <- mask(r_unc, crop_mask, maskvalues = 0)

all_mgt <- c(r_es, r_unc)

## -------- 3) Decision rules -> initial class map ----------
xDt <- data.table(values(all_mgt))
idx <- which(complete.cases(xDt))
df_all_mgt <- as.data.frame(xDt[idx])

# AF=1, NT=3, OF=4; combos: 31 (NT,AF), 41 (OF,AF); 0 = other/none
df_all_mgt$Class_red <- with(
  df_all_mgt,
  ifelse(ntes > 0 & ntint < ofint & ntint < afint, 3,                   # NT wins
  ifelse(ntes > 0 & ofes < 0 & afes < 0,               3,

  ifelse(ofes > 0 & ofint < ntint & ofint < afint,     4,               # OF wins
  ifelse(ofes > 0 & ntes < 0 & afes < 0,               4,

  ifelse(afes > 0 & afint < ntint & afint < ofint,     1,               # AF wins
  ifelse(afes > 0 & ntes < 0 & ofes < 0,               1,

  ifelse(ntes < 0 & ofes > 0 & afes > 0 & ofint < afint, 41,            # OF,AF
  ifelse(ntes > 0 & ofes < 0 & afes > 0 & ntint < afint, 31,            # NT,AF
         0
  ))))))))
)

# Rasterize class map
r_grid_red_class <- all_mgt[[1]]
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class

## -------- 4) Recode to plotted classes --------------------
# Keep: 1 (AF), 3 (NT), 4 (OF), 31 -> 5 (NT,AF), 41 -> 6 (OF,AF)
r_8class_recod <- r_red_class %>%
  subst(31, 5) %>%
  subst(41, 6)

# Project & aggregate for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)
r_agg_red    <- aggregate(red_class_pj, fact = 4, fun = modal, na.rm = TRUE)

## -------- 5) Biome filtering (optional but consistent) ----
biome <- rast("./input/biome_mask/RFP_biome.tif") # 4 bands: AF, CC, NT, OF
b_res <- resample(biome, r_agg_red, method = "near")

afm <- ifel(b_res[[1]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

# Only NT+AF (exclude OF) and OF+AF (exclude NT)
mask_ntaf <- ifel(!is.na(ntm) & !is.na(afm) &  is.na(ofm), 1, NA)
mask_ofaf <- ifel(!is.na(ofm) & !is.na(afm) &  is.na(ntm), 1, NA)

rap <- r_agg_red

c1 <- ifel(!is.na(afm)     & (rap == 1), 1, NA)
c3 <- ifel(!is.na(ntm)     & (rap == 3), 3, NA)
c4 <- ifel(!is.na(ofm)     & (rap == 4), 4, NA)
c5 <- ifel(!is.na(mask_ntaf) & (rap == 5), 5, NA)
c6 <- ifel(!is.na(mask_ofaf) & (rap == 6), 6, NA)

rap_final <- merge(c1, c3, c4, c5, c6)
names(rap_final) <- "RFP_class_final"

## -------- 6) Plotting ------------------------------------
red_mgtnames <- c("AF", "NT", "OF", "NT,AF", "OF,AF")
red_palette  <- c("#EBB560", "#CE7568", "#8CB892", "#973526", "#008600")

sf_shoreLine <- st_read(coast_path, quiet = TRUE)
pol_reg_sf   <- st_read(land_path,  quiet = TRUE)

p_red_mgt_best <- tm_shape(pol_reg_sf, projection = "+proj=robin") +
  tm_polygons("agg_n", palette = "white", border.col = NULL, legend.show = FALSE) +
  tm_shape(rap_final) +
  tm_raster(style = "cat", title = "", labels = red_mgtnames, palette = red_palette) +
  tm_shape(sf_shoreLine, projection = "+proj=robin") +
  tm_lines(col = "grey", lwd = 0.25) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)

png_p_red_mgt_best <- p_red_mgt_best + tm_layout(legend.show = FALSE)

## -------- 7) Save everything (prefixed cereal_) -----------
# Raster
writeRaster(rap_final, save_path("rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(p_red_mgt_best,     filename = save_path("plot_rap_best.pdf"),
          width = 80, units = "mm", dpi = 450)
tmap_save(png_p_red_mgt_best, filename = save_path("plot_rap_best.png"),
          width = 80, units = "mm", dpi = 450)

# Single-class masks (1,3,4,5,6)
p1 <- rap_final == 1  # AF
p2 <- rap_final == 3  # NT
p3 <- rap_final == 4  # OF
p4 <- rap_final == 5  # NT,AF
p5 <- rap_final == 6  # OF,AF

p1_plt <- plot_map(p1, red_palette[1], sf_shoreLine)
p2_plt <- plot_map(p2, red_palette[2], sf_shoreLine)
p3_plt <- plot_map(p3, red_palette[3], sf_shoreLine)
p4_plt <- plot_map(p4, red_palette[4], sf_shoreLine)
p5_plt <- plot_map(p5, red_palette[5], sf_shoreLine)

tmap_save(p1_plt, filename = save_path("r1_AF.png"),     width = 180, units = "mm", dpi = 225)
tmap_save(p2_plt, filename = save_path("r3_NT.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(p3_plt, filename = save_path("r4_OF.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(p4_plt, filename = save_path("r5_NT_AF.png"),  width =  80, units = "mm", dpi = 225)
tmap_save(p5_plt, filename = save_path("r6_OF_AF.png"),  width =  80, units = "mm", dpi = 225)

## -------- 8) Bar chart ------------------------------------
names(rap_final) <- "class"
df_r_classes <- as.data.frame(rap_final, xy = TRUE)

perc_classes <- df_r_classes %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count)) %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

# Order: 1,3,4,5,6 to match red_mgtnames
level_vals <- c(1,3,4,5,6)
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

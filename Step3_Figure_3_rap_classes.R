## =============================================================================
## Packages
## =============================================================================
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, readxl, writexl, ggplot2, forcats, tibble,
  terra, tmap, sf, data.table, tidyterra
)

## =============================================================================
## Paths & helpers
## =============================================================================
# Output folder (create once)
output_path <- "./output/recap_fig/fig3"
dir.create("./output", showWarnings = FALSE)
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Per-management graph folders
af_path_graph <- "./output/af/all/graph/"
cc_path_graph <- "./output/cc/all/graph/"
nt_path_graph <- "./output/nt/all/graph/"
of_path_graph <- "./output/of/all/graph/"

# Safe loader for GeoTIFF
load_map <- function(dir, base) {
  f <- file.path(dir, paste0(base, ".tif"))
  if (!file.exists(f)) stop("Missing file: ", f)
  rast(f)
}

## =============================================================================
## Load ES/UNC per management -> stacks; apply cropland mask
## =============================================================================
# Helper
load_map <- function(dir, base) rast(file.path(dir, paste0(base, ".tif")))

# Load per-management ES/UNC
es <- list(
  af = load_map(af_path_graph, "af_es"),
  cc = load_map(cc_path_graph, "cc_es"),
  nt = load_map(nt_path_graph, "nt_es"),
  of = load_map(of_path_graph, "of_es")
)
unc <- list(
  af = load_map(af_path_graph, "af_unc"),
  cc = load_map(cc_path_graph, "cc_unc"),
  nt = load_map(nt_path_graph, "nt_unc"),
  of = load_map(of_path_graph, "of_unc")
)

# Make stacks (bands: 1=AF, 2=CC, 3=NT, 4=OF)
r_es  <- rast(es)  ; names(r_es)  <- names(es)
r_unc <- rast(unc)
r_unc<- r_unc[[grepl("_2$", names(r_unc))]]; 
names(r_unc) <- names(unc)

# Load & align cropland mask
crop_mask <- rast("./input/cropland_mask/mask_crop.tif")

# Align to ES grid (CRS/extent/res). Use nearest-neighbor for categorical masks.
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")
}

# Apply mask to show only cropland areas (value == 1)
r_es  <- mask(r_es,  crop_mask)
r_unc <- mask(r_unc, crop_mask)

names(r_es)  <- c("afes","cces","ntes","ofes")
names(r_unc) <- c("afint","ccint","ntint","ofint")

# Combine for table ops
all_mgt <- c(r_es, r_unc)

## =============================================================================
## Build the “best practice” multi-class map (your decision rules)
## =============================================================================
# To data.table
xDt <- data.table(values(all_mgt))
idx <- which(complete.cases(xDt))
df_all_mgt <- as.data.frame(xDt[idx])

# Big ifelse rule-set (as provided)
df_all_mgt$Class_red <- with(
  df_all_mgt,
  ifelse(ntes > 0 & ntint < ccint & ntint < ofint & ntint < afint, 3,
  ifelse(ntes > 0 & cces < 0 & ofes < 0 & afes < 0, 3,

  ifelse(cces > 0 & ccint < ntint & ccint < ofint & ccint < afint, 2,
  ifelse(cces > 0 & ntes < 0 & ofes < 0 & afes < 0, 2,

  ifelse(ofes > 0 & ofint < ntint & ofint < ccint & ofint < afint, 4,
  ifelse(ofes > 0 & ntes < 0 & cces < 0 & afes < 0, 4,

  ifelse(afes > 0 & afint < ntint & afint < ccint & afint < ofint, 1,
  ifelse(afes > 0 & ntes < 0 & cces < 0 & ofes < 0, 1,

  ifelse(ntes < 0 & cces > 0 & ofes < 0 & afes > 0 & ccint < afint, 21,

  ifelse(ntes > 0 & cces > 0 & ofes < 0 & afes > 0 &
         ntint < ccint & ntint < afint & ccint < afint, 321,

  ifelse(ntes < 0 & cces > 0 & ofes < 0 & afes > 0 & ccint > afint, 12,

  ifelse(ntes > 0 & cces < 0 & ofes < 0 & afes > 0 & ntint < afint, 31,

  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         ccint < ofint & ccint < afint & ofint < afint, 241,

  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         ofint < ccint & ofint < afint & ccint < afint, 421,

  ifelse(ntes > 0 & cces > 0 & ofes < 0 & afes < 0 & ntint < ccint, 32,

  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes > 0 &
         ntint < ccint & ntint < ofint & ntint < afint &
         ccint > ofint & ccint < afint & ofint < afint, 3421,

  ifelse(ntes < 0 & cces < 0 & ofes < 0 & afes < 0, 99,

  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes > 0 &
         ntint < ccint & ofint < ntint & ntint < afint &
         ofint < ccint & ccint < afint & ofint < afint, 4321, 0
  ))))))))))))))))))
)

# Back to raster
r_grid_red_class <- all_mgt[[1]]    # single-layer template
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class

## =============================================================================
## Reduce classes to 8 (based on frequency) & recode
## =============================================================================
df_red_cl <- as.data.frame(r_red_class, xy = TRUE)

perc_class <- df_red_cl %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count))

# Map infrequent classes (<3%) to 1500; keep others as-is (your logic)
cl_perc_class <- perc_class %>%
  mutate(new_Class = dplyr::case_when(
    (count > 30) ~ class,
    (count < 30 & count > 20) ~ class,
    (count < 20 & count > 10) ~ class,
    (count < 10 & count > 5)  ~ class,
    (count < 6  & count > 5)  ~ class,
    (count < 5  & count > 3)  ~ class,
    (count < 3  & count > 2)  ~ class,
    (count < 1.77 & count > 1)~ class,
    (count < 3)               ~ 1500
  )) %>%
  dplyr::select(class, new_Class)

# Remove global negative code (99) as NA
r_full_rmna <- subst(r_red_class, 99, NA)

# Bring raster values to df for join
xDt2 <- data.table(values(r_full_rmna))
idx2 <- which(complete.cases(xDt2))
df_r_full_rmna <- as.data.frame(xDt2[idx2])

# Join and push back to raster
df_red_8class <- df_r_full_rmna %>%
  left_join(cl_perc_class, by = "class")

r_grid_8class <- r_full_rmna
names(r_grid_8class) <- "class"
r_grid_8class[!is.na(r_grid_8class$class)] <- NaN
r_grid_8class[idx2] <- df_red_8class$new_Class
r_8class <- r_grid_8class
freq(r_8class)

# Recode compound codes into 5–8 buckets
r_8class_recod <- r_8class %>%
  subst(12, 5)   %>%  # "AF,CC"
  subst(21, 6)   %>%  # "CC,AF"
  subst(421, 7)  %>%  # "OF,CC,AF"
  subst(32, 8)   %>%  
  subst(241, 8)  %>%
  subst(321, 8)  %>% 
  subst(0, 8)  %>%  
  subst(1500, 8)      # "other_RFA"

# Project for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)

## =============================================================================
## Biome-based masking & final 1–8 map
## =============================================================================
biome <- rast("./input/biome_mask/RFP_biome.tif") # 4 layers: AF, CC, NT, OF
b_res <- resample(biome, red_class_pj, method = "near")

# Build per-biome binary masks (1 / NA)
afm <- ifel(b_res[[1]] == 1, 1, NA)
ccm <- ifel(b_res[[2]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

# Combination masks
mask_afcc_incl <- ifel(!is.na(afm) & !is.na(ccm), 1, NA)                      # AF ∩ CC
mask_ofccaf    <- ifel(!is.na(afm) & !is.na(ccm) & !is.na(ntm) & is.na(ofm), 1, NA)
any_mask       <- ifel(!is.na(afm) | !is.na(ccm) | !is.na(ntm) | !is.na(ofm), 1, NA)
mask_afcc      <- ifel(!is.na(afm) & !is.na(ccm) &  is.na(ntm) &  is.na(ofm), 1, NA)
mask_other     <- ifel(!is.na(any_mask) & is.na(mask_afcc) & is.na(mask_ofccaf), 1, NA)

# Apply masks to the 8-class raster (source for selection)
rap_src <- red_class_pj

# Singles (1–4)
c1 <- ifel(!is.na(afm) & (rap_src == 1), 1, NA)
c2 <- ifel(!is.na(ccm) & (rap_src == 2), 2, NA)
c3 <- ifel(!is.na(ntm) & (rap_src == 3), 3, NA)
c4 <- ifel(!is.na(ofm) & (rap_src == 4), 4, NA)

# AF,CC split into 5 and 6 inside the inclusive AF∩CC zone
c5 <- ifel(!is.na(mask_afcc_incl) & (rap_src == 5), 5, NA)
c6 <- ifel(!is.na(mask_afcc_incl) & (rap_src == 6), 6, NA)

# OF,CC,AF (7) and other_RFA (8)
c7 <- ifel(!is.na(mask_ofccaf) & (rap_src == 7), 7, NA)
c8 <- ifel(!is.na(mask_other)  & (rap_src == 8), 8, NA)

# Merge to a final categorical 1..8 map
rap_final <- merge(c1, c2, c3, c4, c5, c6, c7, c8)
names(rap_final) <- "RFP_class_final"
writeRaster(rap_final, file.path(output_path, "rap_final.tif"), overwrite = TRUE)


## =============================================================================
## Legend, palettes, coastline/boundaries, and plotting
## =============================================================================
red_mgtnames <- c(
  "AF",          # 1
  "CC",          # 2
  "NT",          # 3
  "OF",          # 4
  "AF,CC",       # 5
  "CC, AF",      # 6
  "OF,CC,AF",    # 7
  "other_RFA"    # 8
)

red_palette <- c(
  "#EBB560", # AF
  "#6690CB", # CC
  "#CE7568", # NT
  "#8CB892", # OF
  "#285799", # AF, CC
  "#93CFC9", # CC, AF
  "#691A17", # OF,CC,AF
  "#6D6BA8"  # other_RFA
)

# Boundaries
sf_shoreLine <- st_read("./input/boundaries/coastline.shp", quiet = TRUE)
pol_reg_sf   <- st_read("./input/boundaries/land_ag.shp", quiet = TRUE)

# Full map with legend
# Optional: aggregate for smoother map
r_agg_red <- aggregate(rap_final, fact = 4, fun = modal, na.rm = TRUE)
# Save the final raster
writeRaster(r_agg_red, file.path(output_path, "r_agg_red.tif"), overwrite = TRUE)


p_red_mgt_best <- tm_shape(pol_reg_sf, projection = "+proj=robin") +
  tm_polygons("agg_n", palette = "white", border.col = NULL, legend.show = FALSE) +
  tm_shape(r_agg_red) +
  tm_raster(style = "cat", title = "", labels = red_mgtnames, palette = red_palette) +
  tm_shape(sf_shoreLine, projection = "+proj=robin") +
  tm_lines(col = "grey", lwd = 0.25) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)

p_red_mgt_best 

# PNG version without legend
png_p_red_mgt_best <- p_red_mgt_best + tm_layout(legend.show = FALSE)

tmap_save(png_p_red_mgt_best,
          filename = file.path(output_path, "png_plot_red_RAP_best.png"),
          width = 100, units = "cm", dpi = 450)

tmap_save(p_red_mgt_best,
          filename = file.path(output_path, "png_plot_red_RAP_best.pdf"),
          width = 80, units = "cm", dpi = 450)

# Helper to plot a single-class mask in a given color
plot_map <- function(r_mask, col_hex) {
  tm_shape(r_mask, projection = "+proj=robin") +
    tm_raster(style = "cat", legend.show = FALSE,
              palette = c("#F0F0F0", col_hex)) +
    tm_shape(sf_shoreLine, projection = "+proj=robin") +
    tm_lines(col = "grey", lwd = 0.25) +
    tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE)
}

# Single-class masks from the final map
af        <- r_agg_red == 1
cc        <- r_agg_red == 2
nt        <- r_agg_red == 3
of        <- r_agg_red == 4
af_cc     <- r_agg_red == 5
cc_af     <- r_agg_red == 6
of_cc_af  <- r_agg_red == 7
other_rfa <- r_agg_red == 8

af_plt        <- plot_map(af,        red_palette[1])
cc_plt        <- plot_map(cc,        red_palette[2])
nt_plt        <- plot_map(nt,        red_palette[3])
of_plt        <- plot_map(of,        red_palette[4])
af_cc_plt     <- plot_map(af_cc,     red_palette[5])
cc_af_plt     <- plot_map(cc_af,     red_palette[6])
of_cc_af_plt  <- plot_map(of_cc_af,  red_palette[7])
other_rfa_plt <- plot_map(other_rfa, red_palette[8])

tmap_save(af_plt,        filename = file.path(output_path, "r1_af.png"),        width = 180, units = "mm", dpi = 225)
tmap_save(cc_plt,        filename = file.path(output_path, "r2_cc.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(nt_plt,        filename = file.path(output_path, "r3_nt.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(of_plt,        filename = file.path(output_path, "r4_of.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(af_cc_plt,     filename = file.path(output_path, "r5_af_cc.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(cc_af_plt,     filename = file.path(output_path, "r6_cc_af.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(of_cc_af_plt,  filename = file.path(output_path, "r7_of_cc_af.png"),  width =  80, units = "mm", dpi = 225)
tmap_save(other_rfa_plt, filename = file.path(output_path, "r8_other_rfa.png"), width =  80, units = "mm", dpi = 225)

## =============================================================================
## Bar chart (% area per class) from r_agg_red
## =============================================================================
names(rap_final) <- "class"
df_r_8class_recod <- as.data.frame(rap_final, xy = TRUE)

perc_8class1 <- df_r_8class_recod %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count))

perc_8class <- perc_8class1 %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

# Drop class 0 if present
perc_8class_clean <- perc_8class %>% filter(class != "0")

# Bar plot
plt_bar_8class <- perc_8class_clean %>%
  mutate(
    perc  = paste0("            ", perc, " "),
    class = factor(as.character(class), levels = as.character(1:8), labels = red_mgtnames)
  ) %>%
  ggplot(aes(x = forcats::fct_rev(class), y = count, fill = as.character(as.integer(class)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = red_palette, labels = red_mgtnames) +
  theme_void(base_size = 11) +
  theme(legend.position = "none") +
  geom_text(aes(label = perc), size = 7) +
  coord_flip()

ggsave(file.path(output_path, "plt_bar_8class.pdf"), plt_bar_8class,
       width = 50, height = 20, units = "cm", dpi = 450)

write_xlsx(perc_8class1, file.path(output_path, "perc_8class.xlsx"))

## =============================================================================
cat("All outputs saved to:", normalizePath(output_path), "\n")

## =============================================================================
## No biome applied
## =============================================================================

# Optional: aggregate for smoother map
full_r_agg_red <- aggregate(red_class_pj, fact = 4, fun = modal, na.rm = TRUE)

# Save the final raster
writeRaster(red_class_pj, file.path(output_path, "full_r_agg_red.tif"), overwrite = TRUE)

# Full map with legend
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

tmap_save(full_png_p_red_mgt_best,
          filename = file.path(output_path, "full_png_plot_red_RAP_best.png"),
          width = 100, units = "cm", dpi = 450)

tmap_save(full_p_red_mgt_best,
          filename = file.path(output_path, "full_png_plot_red_RAP_best.pdf"),
          width = 80, units = "cm", dpi = 450)

# Single-class masks from the final map
full_af        <- full_r_agg_red == 1
full_cc        <- full_r_agg_red == 2
full_nt        <- full_r_agg_red == 3
full_of        <- full_r_agg_red == 4
full_af_cc     <- full_r_agg_red == 5
full_cc_af     <- full_r_agg_red == 6
full_of_cc_af  <- full_r_agg_red == 7
full_other_rfa <- full_r_agg_red == 8

full_af_plt        <- plot_map(full_af,        red_palette[1])
full_cc_plt        <- plot_map(full_cc,        red_palette[2])
full_nt_plt        <- plot_map(full_nt,        red_palette[3])
full_of_plt        <- plot_map(full_of,        red_palette[4])
full_af_cc_plt     <- plot_map(full_af_cc,     red_palette[5])
full_cc_af_plt     <- plot_map(full_cc_af,     red_palette[6])
full_of_cc_af_plt  <- plot_map(full_of_cc_af,  red_palette[7])
full_other_rfa_plt <- plot_map(full_other_rfa, red_palette[8])

tmap_save(full_af_plt,        filename = file.path(output_path, "full_r1_af.png"),        width = 180, units = "mm", dpi = 225)
tmap_save(full_cc_plt,        filename = file.path(output_path, "full_r2_cc.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(full_nt_plt,        filename = file.path(output_path, "full_r3_nt.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(full_of_plt,        filename = file.path(output_path, "full_r4_of.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(full_af_cc_plt,     filename = file.path(output_path, "full_r5_af_cc.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(full_cc_af_plt,     filename = file.path(output_path, "full_r6_cc_af.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(full_of_cc_af_plt,  filename = file.path(output_path, "full_r7_of_cc_af.png"),  width =  80, units = "mm", dpi = 225)
tmap_save(full_other_rfa_plt, filename = file.path(output_path, "full_r8_other_rfa.png"), width =  80, units = "mm", dpi = 225)

## =============================================================================
## Bar chart (% area per class) from r_agg_red
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

# Bar plot
full_plt_bar_8class <- full_perc_8class_clean %>%
  mutate(
    perc  = paste0("            ", perc, " "),
    class = factor(as.character(class), levels = as.character(1:8), labels = red_mgtnames)
  ) %>%
  ggplot(aes(x = forcats::fct_rev(class), y = count, fill = as.character(as.integer(class)))) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = red_palette, labels = red_mgtnames) +
  theme_void(base_size = 11) +
  theme(legend.position = "none") +
  geom_text(aes(label = perc), size = 7) +
  coord_flip()

ggsave(file.path(output_path, "full_plt_bar_8class.pdf"), full_plt_bar_8class,
       width = 50, height = 20, units = "cm", dpi = 450)

write_xlsx(full_perc_8class1, file.path(output_path, "full_perc_8class.xlsx"))

## =============================================================================
cat("All outputs saved to:", normalizePath(output_path), "\n")

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
output_path <- "./output/fig_3/"
dir.create("./output", showWarnings = FALSE)
dir.create(output_path, recursive = TRUE, showWarnings = FALSE)

# Per-management graph folders
af_path_graph <- "./output/af/graph/"
cc_path_graph <- "./output/cc/graph/"
nt_path_graph <- "./output/nt/graph/"
of_path_graph <- "./output/of/graph/"

# Safe loader for GeoTIFF
load_map <- function(dir, base) {
  f <- file.path(dir, paste0(base, ".tif"))
  if (!file.exists(f)) stop("Missing file: ", f)
  rast(f)
}

## =============================================================================
## Load ES/UNC per management -> stacks; apply cropland mask
## =============================================================================
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

r_es  <- rast(es)  ; names(r_es)  <- c("afes","cces","ntes","ofes")
r_unc <- rast(unc) ; names(r_unc) <- c("afint","ccint","ntint","ofint")

# Cropland mask (prefer relative path; using nearest for categorical mask)
crop_mask <- rast("./input/cropland_mask/mask_crop.tif")
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")
}
# If mask is 1=crop, 0=non-crop:
r_es  <- mask(r_es,  crop_mask, maskvalues = 0)
r_unc <- mask(r_unc, crop_mask, maskvalues = 0)

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
  select(class, new_Class)

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

# Recode compound codes into 5–8 buckets
r_8class_recod <- r_8class %>%
  subst(12, 5)   %>%  # "AF,CC"
  subst(21, 6)   %>%  # "CC,AF"
  subst(321, 7)  %>%  # "NT,CC,AF"
  subst(1500, 8)      # "other_RFA"

# Project for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)

# Optional: aggregate for smoother map
r_agg_red <- aggregate(red_class_pj, fact = 4, fun = modal, na.rm = TRUE)

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
mask_ntccaf    <- ifel(!is.na(afm) & !is.na(ccm) & !is.na(ntm) & is.na(ofm), 1, NA)
any_mask       <- ifel(!is.na(afm) | !is.na(ccm) | !is.na(ntm) | !is.na(ofm), 1, NA)
mask_afcc      <- ifel(!is.na(afm) & !is.na(ccm) &  is.na(ntm) &  is.na(ofm), 1, NA)
mask_other     <- ifel(!is.na(any_mask) & is.na(mask_afcc) & is.na(mask_ntccaf), 1, NA)

# Apply masks to the 8-class raster (source for selection)
rap_src <- red_class_pj

# Singles (1–4)
c1 <- ifel(!is.na(afm) & (rap_src == 1), 1, NA)
c2 <- ifel(!is_na(ccm) & (rap_src == 2), 2, NA)
c3 <- ifel(!is_na(ntm) & (rap_src == 3), 3, NA)
c4 <- ifel(!is_na(ofm) & (rap_src == 4), 4, NA)

# AF,CC split into 5 and 6 inside the inclusive AF∩CC zone
c5 <- ifel(!is.na(mask_afcc_incl) & (rap_src == 5), 5, NA)
c6 <- ifel(!is.na(mask_afcc_incl) & (rap_src == 6), 6, NA)

# NT,CC,AF (7) and other_RFA (8)
c7 <- ifel(!is.na(mask_ntccaf) & (rap_src == 7), 7, NA)
c8 <- ifel(!is.na(mask_other)  & (rap_src == 8), 8, NA)

# Merge to a final categorical 1..8 map
rap_final <- merge(c1, c2, c3, c4, c5, c6, c7, c8)
names(rap_final) <- "RFP_class_final"

# Save the final raster
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
  "NT,CC,AF",    # 7
  "other_RFA"    # 8
)

red_palette <- c(
  "#EBB560", # AF
  "#6690CB", # CC
  "#CE7568", # NT
  "#8CB892", # OF
  "#285799", # AF, CC
  "#93CFC9", # CC, AF
  "#691A17", # NT,CC,AF
  "#6D6BA8"  # other_RFA
)

# Boundaries
sf_shoreLine <- st_read("./input/boundaries/coastline.shp", quiet = TRUE)
pol_reg_sf   <- st_read("./input/boundaries/land_ag.shp", quiet = TRUE)

# Full map with legend
p_red_mgt_best <- tm_shape(pol_reg_sf, projection = "+proj=robin") +
  tm_polygons("agg_n", palette = "white", border.col = NULL, legend.show = FALSE) +
  tm_shape(rap_final) +
  tm_raster(style = "cat", title = "", labels = red_mgtnames, palette = red_palette) +
  tm_shape(sf_shoreLine, projection = "+proj=robin") +
  tm_lines(col = "grey", lwd = 0.25) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)

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
af        <- rap_final == 1
cc        <- rap_final == 2
nt        <- rap_final == 3
of        <- rap_final == 4
af_cc     <- rap_final == 5
cc_af     <- rap_final == 6
nt_cc_af  <- rap_final == 7
other_rfa <- rap_final == 8

af_plt        <- plot_map(af,        red_palette[1])
cc_plt        <- plot_map(cc,        red_palette[2])
nt_plt        <- plot_map(nt,        red_palette[3])
of_plt        <- plot_map(of,        red_palette[4])
af_cc_plt     <- plot_map(af_cc,     red_palette[5])
cc_af_plt     <- plot_map(cc_af,     red_palette[6])
nt_cc_af_plt  <- plot_map(nt_cc_af,  red_palette[7])
other_rfa_plt <- plot_map(other_rfa, red_palette[8])

tmap_save(af_plt,        filename = file.path(output_path, "r1_af.png"),        width = 180, units = "mm", dpi = 225)
tmap_save(cc_plt,        filename = file.path(output_path, "r2_cc.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(nt_plt,        filename = file.path(output_path, "r3_nt.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(of_plt,        filename = file.path(output_path, "r4_of.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(af_cc_plt,     filename = file.path(output_path, "r5_af_cc.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(cc_af_plt,     filename = file.path(output_path, "r6_cc_af.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(nt_cc_af_plt,  filename = file.path(output_path, "r7_nt_cc_af.png"),  width =  80, units = "mm", dpi = 225)
tmap_save(other_rfa_plt, filename = file.path(output_path, "r8_other_rfa.png"), width =  80, units = "mm", dpi = 225)

## =============================================================================
## Bar chart (% area per class) from rap_final
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

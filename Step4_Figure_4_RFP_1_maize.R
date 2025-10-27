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
# Per-management graph folders for MAIZE
af_path_graph <- "./output/af/maize/graph/"
cc_path_graph <- "./output/cc/maize/graph/"
nt_path_graph <- "./output/nt/maize/graph/"
of_path_graph <- "./output/of/maize/graph/"

# Output folder
out_dir <- "./output/recap_fig/fig4/maize/"
dir.create("./output", showWarnings = FALSE)
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# Boundaries
coast_path <- "./input/boundaries/coastline.shp"
land_path  <- "./input/boundaries/land_ag.shp"

# ---- crop tag for filenames ----
crop_tag  <- "maize"
save_path <- function(name) file.path(out_dir, sprintf("%s_%s", crop_tag, name))

## ------------------------------- Helpers -------------------------------------
load_map <- function(dir, base) {
  f <- file.path(dir, paste0(base, ".tif"))
  if (!file.exists(f)) stop("Missing file: ", f)
  rast(f)
}

plot_map <- function(r_mask, col_hex, sf_shoreLine) {
  tm_shape(r_mask, projection = "+proj=robin") +
    tm_raster(scol.scale = tm_scale_categorical(), 
              legend.show = FALSE, 
              palette = c("#F0F0F0", col_hex)) +
    tm_shape(sf_shoreLine, projection = "+proj=robin") +
    tm_lines(col = "grey", lwd = 0.25) +
    tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE)
}

## --------------------------- Load ES / UNC stacks ----------------------------
es <- list(
  af = load_map(af_path_graph, "af_es_m"),
  cc = load_map(cc_path_graph, "cc_es_m"),
  nt = load_map(nt_path_graph, "nt_es_m"),
  of = load_map(of_path_graph, "of_es_m")
)
unc <- list(
  af = load_map(af_path_graph, "af_unc_m"),
  cc = load_map(cc_path_graph, "cc_unc_m"),
  nt = load_map(nt_path_graph, "nt_unc_m"),
  of = load_map(of_path_graph, "of_unc_m")
)

r_es  <- rast(es)  ; names(r_es)  <- c("afes","cces","ntes","ofes")
r_unc <- rast(unc) ; r_unc <-r_unc[[grepl("_2$", names(r_unc))]]; 
names(r_unc) <- c("afint","ccint","ntint","ofint")

## --------------------------- Cropland mask & align ---------------------------
# Load & align cropland mask
crop_mask <- rast("./input/cropland_mask/mask_crop.tif")

# Align to ES grid (CRS/extent/res). Use nearest-neighbor for categorical masks.
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")
}

# Apply mask to show only cropland areas (value == 1)
r_es  <- mask(r_es,  crop_mask)
r_unc <- mask(r_unc, crop_mask)

# Combine for table ops
all_mgt <- c(r_es, r_unc)

## --------------------- Decision rules -> initial class map -------------------
xDt <- data.table(values(all_mgt))
idx <- which(complete.cases(xDt))
df_all_mgt <- as.data.frame(xDt[idx])

# AF=1, CC=2, NT=3, OF=4; combos: 21, 412, 421; 0 = other/none
df_all_mgt$Class_red <- with(
  df_all_mgt,
  ifelse(ntes > 0 & ntint < ccint & ntint < ofint & ntint < afint, 3,
  ifelse(ntes > 0 & cces < 0 & ofes < 0 & afes < 0,               3,

  ifelse(cces > 0 & ccint < ntes & ccint < ofint & ccint < afint, 2,
  ifelse(cces > 0 & ntes < 0 & ofes < 0 & afes < 0,               2,

  ifelse(ofes > 0 & ofint < ntint & ofint < ccint & ofint < afint, 4,
  ifelse(ofes > 0 & ntes < 0 & cces < 0 & afes < 0,                4,

  ifelse(afes > 0 & afint < ntint & afint < ccint & afint < ofint, 1,
  ifelse(afes > 0 & ntes < 0 & cces < 0 & ofes < 0,                1,

  ifelse(ntes < 0 & cces > 0 & ofes < 0 & afes > 0 & ccint < afint, 21,

  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         afint < ccint & ofint < ccint & ofint < afint,             412,

  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         ofint < ccint & ofint < afint & ccint < afint,             421, 
  
    ifelse(ntes > 0 & cces > 0 & ofes < 0 & afes > 0 &
         ntint < ccint & ntint < afint & ccint < afint, 321,

  ifelse(ntes < 0 & cces > 0 & ofes < 0 & afes > 0 & ccint > afint, 12,

  ifelse(ntes > 0 & cces < 0 & ofes < 0 & afes > 0 & ntint < afint, 31,
         
  ifelse(ntes < 0 & cces < 0 & ofes > 0 & afes > 0 & ofint < afint, 41,

  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         ccint < ofint & ccint < afint & ofint < afint, 241,
         
  ifelse(ntes > 0 & cces > 0 & ofes < 0 & afes < 0 & ntint < ccint, 32,

  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes > 0 &
         ntint < ccint & ntint < ofint & ntint < afint &
         ccint > ofint & ccint < afint & ofint < afint, 3421,
         
  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes > 0 &
         ntint > ccint & ntint < ofint & ntint < afint &
         ccint < ofint & ccint < afint & ofint < afint, 2341,
         
  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes > 0 &
         ntint > ccint & ntint < ofint & ntint < afint &
         ccint < ofint & ccint < afint & ofint > afint, 2314,
         
  ifelse(ntes > 0 & cces > 0 & ofes < 0 & afes > 0 &
         ntint > ccint  & ntint < afint &
         ccint < afint, 231,
         
  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         ccint > afint  & ccint < ofint &
         afint < ofint, 124,
         
  ifelse(ntes < 0 & cces > 0 & ofes > 0 & afes > 0 &
         ccint < afint  & ccint < ofint &
         afint < ofint, 214,
         
         
 ifelse(
    afes > 0 & cces > 0 & ntes > 0 & ofes > 0 &
    ntint > ccint & ntint > ofint & ntint < afint &
    ccint < ofint & ccint < afint & ofint < afint,2341,
    
 ifelse(
    afes > 0 & cces > 0 & ntes > 0 & ofes > 0 &
    ntint > ccint & ntint > ofint & ntint < afint &
    ccint < ofint & ccint < afint & ofint < afint,2341,
    
 ifelse(ntes > 0 & cces < 0 & ofes > 0 & afes > 0 &
         ntint < afint  & ntint > ofint &
         afint > ofint, 431,
         
  ifelse(ntes > 0 & cces > 0 & ofes > 0 & afes < 0 &
         ntint > ccint  & ntint < ofint &
         ccint < ofint, 234,
         
  ifelse(ntes < 0 & cces < 0 & ofes > 0 & afes > 0 &
         afint < ofint, 14,
         
  ifelse(ntes > 0 & cces < 0 & ofes > 0 & afes > 0 &
         ntint < afint  & ntint < ofint &
         afint > ofint, 341,0

         )))))))))))))))))))))))))))))
)

# df_check<-df_all_mgt %>% filter(Class_red==0)
# head(df_check)
# tail(df_check)
# 
# write_xlsx(df_check,"df_check.xlsx")

# Push back to raster
r_grid_red_class <- all_mgt[[1]]
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class

## ---------------------------- Recode to 8 classes ----------------------------

# Recode compound codes into 5–8 buckets
r_8class_recod <- r_red_class %>%
  subst(421, 5)   %>%  
  subst(241, 6)   %>%  
  subst(214, 7)  %>%  
  subst(12:124, 8)   %>%  
  subst(231:234, 8)  %>%
  subst(321:412, 8)  %>%  
  subst(431:2341, 8)  %>%    
  subst(0, 8) 

# Project for cartography
red_class_pj <- terra::project(r_8class_recod, y = "+proj=robin", method = "near")
red_class_pj <- as.factor(red_class_pj)

# Optional: aggregate for smoother map
r_agg_red <- aggregate(red_class_pj, fact = 2, fun = modal, na.rm = TRUE)

## --------------------------- Biome masks (optional) --------------------------
biome <- rast("./input/biome_mask/RFP_biome.tif")   # 4 layers: AF, CC, NT, OF

if (!same.crs(biome, red_class_pj)) {
  biome <- project(biome, red_class_pj, method = "near")  # categorical
}

b_res <- resample(biome, red_class_pj, method = "near")

# Build masks *after* alignment
afm <- ifel(b_res[[1]] == 1, 1, NA)
ccm <- ifel(b_res[[2]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

# Convenience masks
any_mask <- ifel(!is.na(afm) | !is.na(ccm) | !is.na(ntm) | !is.na(ofm), 1, NA)

# (If your classes 5–7 correspond to the AF+CC+OF biome intersection)
mask_afccof <- ifel(!is.na(afm) & !is.na(ccm) & !is.na(ofm), 1, NA)

rap_src <- red_class_pj

# Class maps (no CRS warnings now)
c1 <- ifel(!is.na(afm)      & rap_src == 1, 1, NA)
c2 <- ifel(!is.na(ccm)      & rap_src == 2, 2, NA)
c3 <- ifel(!is.na(ntm)      & rap_src == 3, 3, NA)
c4 <- ifel(!is.na(ofm)      & rap_src == 4, 4, NA)
c5 <- ifel(!is.na(mask_afccof) & rap_src == 5, 5, NA)
c6 <- ifel(!is.na(mask_afccof) & rap_src == 6, 6, NA)
c7 <- ifel(!is.na(mask_afccof) & rap_src == 7, 7, NA)
c8 <- ifel(!is.na(any_mask) & is.na(mask_afccof) & rap_src == 8, 8, NA)

# Merge back into one raster if needed
rap_final <- merge(c1, c2, c3, c4, c5, c6, c7, c8)
names(rap_final) <- "RFP_class_final"

## ------------------------------- Plotting ------------------------------------
red_mgtnames <- c(
  "AF", "CC", "NT", "OF",
  "OF,CC,AF", "CC,OF,AF", 
  "CC,AF,OF", "other_RFA"
)
red_palette <- c(
  "#EBB560", "#6690CB", "#CE7568", "#8CB892",
  "#93CFC9", "#50FF50", "#009E73", "#6D6BA8"
)

# Boundaries
sf_shoreLine <- st_read(coast_path, quiet = TRUE)
pol_reg_sf   <- st_read(land_path,  quiet = TRUE)

# Full map with legend
# tmap v4
# Optional: aggregate for smoother 
r_agg_red <- aggregate(rap_final, fact = 4, fun = modal, na.rm = TRUE)
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


## ------------------------------- Save all ------------------------------------
# Raster
writeRaster(rap_final, save_path("rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(png_p_red_mgt_best, filename = save_path("png_plot_red_RAP_best.png"),
          width = 100, units = "cm", dpi = 450)
tmap_save(p_red_mgt_best,     filename = save_path("png_plot_red_RAP_best.pdf"),
          width = 80, units = "cm", dpi = 450)

## ------------------------------ Bar chart ------------------------------------
names(rap_final) <- "class"
df_r_8class_recod <- as.data.frame(rap_final, xy = TRUE)

perc_8class1 <- df_r_8class_recod %>%
  group_by(class) %>%
  summarise(count = (n() / nrow(.)) * 100, .groups = "drop") %>%
  arrange(desc(count))

perc_8class <- perc_8class1 %>%
  mutate(perc = paste0(sprintf("%4.1f", count), "%"))

plt_bar_8class <- perc_8class %>%
  mutate(class = factor(as.integer(class), levels = 1:8, labels = red_mgtnames)) %>%
  ggplot(aes(x = forcats::fct_rev(class), y = as.numeric(count), fill = class)) +
  geom_col() +
  scale_fill_manual(values = setNames(red_palette, red_mgtnames),
                    breaks = red_mgtnames, limits = red_mgtnames) +
  geom_text(aes(label = perc), size = 7, hjust = -0.1) +
  coord_flip(clip = "off") +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
  theme_void() +
  theme(legend.position = "none")

ggsave(save_path("plt_bar_8class.pdf"), plt_bar_8class,
       width = 50, height = 20, units = "cm", dpi = 450)
write_xlsx(perc_8class1, save_path("perc_8class.xlsx"))

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

ggsave(save_path("full_plt_bar_8class.pdf"), full_plt_bar_8class,
       width = 50, height = 20, units = "cm", dpi = 450)

write_xlsx(full_perc_8class1, save_path("full_perc_8class.xlsx"))



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
af_path_graph <- "./output/AF/maize/graph/"
cc_path_graph <- "./output/CC/maize/graph/"
nt_path_graph <- "./output/NT/maize/graph/"
of_path_graph <- "./output/OF/maize/graph/"

# Output folder
out_dir <- "./output/fig_4/"
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
    tm_raster(style = "cat", legend.show = FALSE, palette = c("#F0F0F0", col_hex)) +
    tm_shape(sf_shoreLine, projection = "+proj=robin") +
    tm_lines(col = "grey", lwd = 0.25) +
    tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE)
}

## --------------------------- Load ES / UNC stacks ----------------------------
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

## --------------------------- Cropland mask & align ---------------------------
crop_mask <- rast("./input/cropland_mask/mask_crop.tif")
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")  # categorical resampling
}
# If mask is 1=crop, 0=non-crop:
r_es  <- mask(r_es,  crop_mask, maskvalues = 0)
r_unc <- mask(r_unc, crop_mask, maskvalues = 0)

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
         ofint < ccint & ofint < afint & ccint < afint,             421, 0
  )))))))))))
)

# Push back to raster
r_grid_red_class <- all_mgt[[1]]
names(r_grid_red_class) <- "class"
r_grid_red_class[!is.na(r_grid_red_class$class)] <- NaN
r_grid_red_class[idx] <- df_all_mgt$Class_red
r_red_class <- r_grid_red_class

## ---------------------------- Recode to 8 classes ----------------------------
# Singles: 1..4; Combos: 21->5, 412->6, 421->7; Other: 0->8
r_8class_recod <- r_red_class %>%
  subst(0,    8) %>%
  subst(21,   5) %>%
  subst(412,  6) %>%
  subst(421,  7)

## --------------------------- Biome masks (optional) --------------------------
biome <- rast("./input/biome_mask/RFP_biome.tif")   # 4 layers: AF, CC, NT, OF
b_res <- resample(biome, r_8class_recod, method = "near")

afm <- ifel(b_res[[1]] == 1, 1, NA)
ccm <- ifel(b_res[[2]] == 1, 1, NA)
ntm <- ifel(b_res[[3]] == 1, 1, NA)
ofm <- ifel(b_res[[4]] == 1, 1, NA)

mask_ccaf    <- ifel(!is.na(afm) & !is.na(ccm) &  is.na(ntm) &  is.na(ofm), 1, NA)
mask_ofafcc  <- ifel(!is.na(afm) & !is.na(ccm) & !is.na(ofm), 1, NA)
any_mask     <- ifel(!is.na(afm) | !is.na(ccm) | !is.na(ntm) | !is.na(ofm), 1, NA)
mask_other   <- ifel(!is.na(any_mask) & is.na(mask_ccaf) & is.na(mask_ofafcc), 1, NA)

rap_src <- r_8class_recod

c1 <- ifel(!is.na(afm)         & (rap_src == 1), 1, NA)
c2 <- ifel(!is.na(ccm)         & (rap_src == 2), 2, NA)
c3 <- ifel(!is.na(ntm)         & (rap_src == 3), 3, NA)
c4 <- ifel(!is.na(ofm)         & (rap_src == 4), 4, NA)
c5 <- ifel(!is.na(mask_ccaf)   & (rap_src == 5), 5, NA)
c6 <- ifel(!is.na(mask_ofafcc) & (rap_src == 6), 6, NA)
c7 <- ifel(!is.na(mask_ofafcc) & (rap_src == 7), 7, NA)
c8 <- ifel(!is.na(mask_other)  & (rap_src == 8), 8, NA)

rap_final <- merge(c1, c2, c3, c4, c5, c6, c7, c8)
names(rap_final) <- "RFP_class_final"

## ------------------------------- Plotting ------------------------------------
red_mgtnames <- c(
  "AF", "CC", "NT", "OF",
  "CC,AF", "OF,AF,CC", "OF,CC,AF", "other_RFA"
)
red_palette <- c(
  "#EBB560", "#6690CB", "#CE7568", "#8CB892",
  "#93CFC9", "#50FF50", "#009E73", "#6D6BA8"
)

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

## ------------------------------- Save all ------------------------------------
# Raster
writeRaster(rap_final, save_path("rap_final.tif"), overwrite = TRUE)

# Full maps
tmap_save(png_p_red_mgt_best, filename = save_path("png_plot_red_RAP_best.png"),
          width = 100, units = "cm", dpi = 450)
tmap_save(p_red_mgt_best,     filename = save_path("png_plot_red_RAP_best.pdf"),
          width = 80, units = "cm", dpi = 450)

# Single-class masks
af        <- rap_final == 1
cc        <- rap_final == 2
nt        <- rap_final == 3
of        <- rap_final == 4
cc_af     <- rap_final == 5
of_af_cc  <- rap_final == 6
of_cc_af  <- rap_final == 7
other_rfa <- rap_final == 8

af_plt        <- plot_map(af,        red_palette[1], sf_shoreLine)
cc_plt        <- plot_map(cc,        red_palette[2], sf_shoreLine)
nt_plt        <- plot_map(nt,        red_palette[3], sf_shoreLine)
of_plt        <- plot_map(of,        red_palette[4], sf_shoreLine)
cc_af_plt     <- plot_map(cc_af,     red_palette[5], sf_shoreLine)
of_af_cc_plt  <- plot_map(of_af_cc,  red_palette[6], sf_shoreLine)
of_cc_af_plt  <- plot_map(of_cc_af,  red_palette[7], sf_shoreLine)
other_rfa_plt <- plot_map(other_rfa, red_palette[8], sf_shoreLine)

tmap_save(af_plt,        filename = save_path("r1_af.png"),        width = 180, units = "mm", dpi = 225)
tmap_save(cc_plt,        filename = save_path("r2_cc.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(nt_plt,        filename = save_path("r3_nt.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(of_plt,        filename = save_path("r4_of.png"),        width =  80, units = "mm", dpi = 225)
tmap_save(cc_af_plt,     filename = save_path("r5_cc_af.png"),     width =  80, units = "mm", dpi = 225)
tmap_save(of_af_cc_plt,  filename = save_path("r6_of_af_cc.png"),  width =  80, units = "mm", dpi = 225)
tmap_save(of_cc_af_plt,  filename = save_path("r7_of_cc_af.png"),  width =  80, units = "mm", dpi = 225)
tmap_save(other_rfa_plt, filename = save_path("r8_other_rfa.png"), width =  80, units = "mm", dpi = 225)

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

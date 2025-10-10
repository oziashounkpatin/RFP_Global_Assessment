## ---------------------------- Packages ---------------------------------------
# Prefer pacman to load/install & avoid duplicates; don't load both plyr & dplyr
if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")
pacman::p_load(
  dplyr, readxl, writexl, skimr, vip, fastshap, PerformanceAnalytics,
  DALEXtra, shapviz, doFuture, kernelshap, reticulate, caret, doParallel,
  iml, ggthemes, ggeffects, pdp, partykit, libcoin, mvtnorm, CAST,
  patchwork, gridExtra, terra, tmap, RColorBrewer, scico, sf, data.table,
  tibble, tidyterra, basemaps, rcartocolor
)

# Graph folders
af_path_graph <- "./output/af/graph/"
cc_path_graph <- "./output/cc/graph/"
nt_path_graph <- "./output/nt/graph/"
of_path_graph <- "./output/of/graph/"

# Helper
load_map <- function(dir, base) rast(file.path(dir, paste0(base, ".tif")))

# 1) Load per-management ES/UNC
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

# 2) Make stacks (bands: 1=AF, 2=CC, 3=NT, 4=OF)
r_es  <- rast(es)  ; names(r_es)  <- names(es)
r_unc <- rast(unc) ; names(r_unc) <- names(unc)

# 3) Load & align cropland mask
#    (use your relative path if available instead of C:/...)
crop_mask <- rast("./input/cropland_mask/mask_crop.tif")

# Align to ES grid (CRS/extent/res). Use nearest-neighbor for categorical masks.
if (!compareGeom(crop_mask, r_es, stopOnError = FALSE)) {
  crop_mask <- project(crop_mask, r_es, method = "near")
}

# If mask is 0/1, drop 0; if it’s NA/non-NA, you can just use mask() without maskvalues
r_es  <- mask(r_es,  crop_mask, maskvalues = 0)
r_unc <- mask(r_unc, crop_mask, maskvalues = 0)

## ---------------------------- Helpers ----------------------------------------
# 1) Classify ES/UNC -> classes 0..6 (one function for all)
classify_es_unc <- function(es, unc,
                            low = 0.6, mid = 1.5) {
  # returns a categorical SpatRaster with values 0..6
  s <- c(es, unc); names(s) <- c("es", "unc")
  dt <- data.table(values(s))
  idx <- which(complete.cases(dt))
  out <- es; names(out) <- "class"
  out[!is.na(out$class)] <- NaN

  if (length(idx)) {
    df  <- as.data.frame(dt[idx,])
    clv <- with(df, ifelse(es < 0 &  unc <  low, 0,
                    ifelse(es < 0 &  unc >= low & unc <= mid, 1,
                    ifelse(es < 0 &  unc >  mid, 2,
                    ifelse(es > 0 &  unc <  low, 3,
                    ifelse(es > 0 &  unc >= low & unc <= mid, 4,
                    ifelse(es > 0 &  unc >  mid, 5, 6)))))))
    out[idx] <- clv
  }
  out
}

# 2) Aggregate categorical raster by modal
agg_modal <- function(x, fact = 4) aggregate(x, fact = fact, fun = modal, na.rm = TRUE)

# 3) tmap plot factory (same look everywhere)
tm_class <- function(class_rast, coastline, polygons, labels, palette) {
  tm_shape(polygons, projection = "+proj=robin") +
    tm_polygons("agg_n", palette = "lightgrey", border.col = NULL, legend.show = FALSE) +
    tm_shape(class_rast) +
    tm_raster(style = "cat", title = "", legend.show = FALSE, labels = labels, palette = palette) +
    tm_shape(coastline, projection = "+proj=robin") +
    tm_lines(col = "grey", lwd = 0.25) +
    tm_layout(legend.show = FALSE, legend.title.size = 3, bg.color = NA, frame = FALSE)
}

# 4) Safe loader for GeoTIFFs (handy if you later need it)
load_map <- function(dir, base) {
  f <- file.path(dir, paste0(base, ".tif"))
  if (!file.exists(f)) stop("Missing file: ", f)
  rast(f)
}

## ---------------------------- Constants --------------------------------------
mgmts    <- c("af","cc","nt","of")
labels7  <- setNames(as.character(0:6), 0:6)
pal7     <- setNames(c("#704F21","#D89E50","#AA7535","#4E4E89","#6D6DB5","#9999D6","#000000"), 0:6)

## ---------------------------- Biome mask -------------------------------------
# 4 biome layers (one-hot in bands 1..4). Resample to class rasters later.
biome  <- rast("./input/biome_mask/RFP_biome.tif")

## ---------------------------- Boundaries -------------------------------------
sf_shoreLine <- st_read("./input/boundaries/coastline.shp", quiet = TRUE)
pol_reg_sf   <- st_read("./input/boundaries/land_ag.shp", quiet = TRUE)

## ---------------------------- Main pipeline ----------------------------------
# 1) Build class rasters by management in a loop
#    Assumes rap_es/rap_unc bands are ordered: 1=AF, 2=CC, 3=NT, 4=OF
idx <- setNames(seq_along(mgmts), mgmts)

class_rasters <- lapply(mgmts, function(k) {
  classify_es_unc(r_es[[ idx[[k]] ]], r_unc[[ idx[[k]] ]])
})
names(class_rasters) <- mgmts

# 2) Aggregate (modal) for cleaner cartography
class_agg <- lapply(class_rasters, agg_modal)

# 3) Biome masking (band 1..4 correspond to AF, CC, NT, OF)
#    TRUE where biome == target; keep only those areas.
biome_res <- resample(biome, class_agg[[1]], method = "near")
biome_maskers <- lapply(1:4, function(i) biome_res[[i]] == 1)
names(biome_maskers) <- mgmts

class_biome <- lapply(mgmts, function(k) {
  mask(class_agg[[k]], biome_maskers[[k]], maskvalues = FALSE)
})
names(class_biome) <- mgmts

# 4) Save combined TIFF (same as your r_bio_all)
r_bio_all <- rast(class_biome)
dir.create(out_root, recursive = TRUE, showWarnings = FALSE)
writeRaster(r_bio_all, file.path(out_root, "r_rap_biome_agg.tif"), overwrite = TRUE)

# 5) Make maps with identical styling
plots <- lapply(mgmts, function(k) {
  tm_class(class_biome[[k]], coastline = sf_shoreLine, polygons = pol_reg_sf,
           labels = labels7, palette = pal7)
})
names(plots) <- mgmts

# 6) Save maps (png)
png_names <- c(af = "fig1_af.png", cc = "fig1_cc.png", nt = "fig1_nt.png", of = "fig1_of.png")
invisible(lapply(mgmts, function(k) {
  tmap_save(plots[[k]], filename = file.path(out_root, png_names[[k]]),
            width = 20, height = 10, units = "cm", dpi = 450)
}))

# 7) Legend export once (use NT as example)
legend_plot <- tm_shape(class_agg[[ "nt" ]]) +
  tm_raster(style = "cat", title = "", labels = labels7, palette = pal7) +
  tm_layout(legend.outside = TRUE, legend.title.size = 3, bg.color = NA, frame = FALSE)
tmap_save(legend_plot, filename = file.path(out_root, "nt_cond.pdf"),
          width = 30, height = 20, units = "cm", dpi = 450)

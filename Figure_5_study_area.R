#------------------------------------------------------------------------------#
# Packages
#------------------------------------------------------------------------------#
library(dplyr)
library(readxl)
library(sf)
library(tmap)
library(ggplot2)
library(stringr)
library(rnaturalearth)
library(rnaturalearthdata)

theme_set(theme_bw())

#------------------------------------------------------------------------------#
# Paths
#------------------------------------------------------------------------------#
excel_path <- "./input/rfps_data.xlsx"      # single file, sheet = "full_dataset"
sheet_name <- "full_dataset"
path_graph <- "./output/fig_5/"
dir.create(path_graph, recursive = TRUE, showWarnings = FALSE)

#------------------------------------------------------------------------------#
# Helpers
#------------------------------------------------------------------------------#
# Read the full sheet and standardize column names
read_full_xy <- function(path, sheet_nm = "full_dataset") {
  df <- read_xlsx(path, sheet = sheet_nm, guess_max = 10000)

  # standardize names -> lower case
  names(df) <- tolower(names(df))

  needed <- c("key","x","y","effectsize")
  miss   <- setdiff(needed, names(df))
  if (length(miss)) {
    stop("Missing required columns in sheet '", sheet_nm, "': ",
         paste(miss, collapse = ", "))
  }

  df %>%
    transmute(
      key        = as.character(key),
      x          = suppressWarnings(as.numeric(x)),
      y          = suppressWarnings(as.numeric(y)),
      effectSize = as.numeric(effectsize)
    ) %>%
    tidyr::drop_na(x, y) %>%
    mutate(key = toupper(key)) %>%                 # be robust to casing
    filter(key %in% c("AF","CC","NT","OF"))
}

# Turn an XY dataframe into sf points (WGS84)
to_sf <- function(df_xy) st_as_sf(df_xy, coords = c("x", "y"), crs = 4326)

# Small plotting helper
study_area <- function(spat_data) {
  world <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf") |>
    dplyr::filter(!iso_a3 %in% "ATA")

  tm_shape(world, crs = "+proj=robin") +
    tm_polygons(col = "lightgrey", fill = "white", lwd = 0.1) +
    tm_shape(spat_data) +
    tm_symbols(size = 0.1, col = "black", border.col = NA) +
    tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE)
}

#------------------------------------------------------------------------------#
# Read once -> split by key -> sf objects
#------------------------------------------------------------------------------#
all_xy <- read_full_xy(excel_path, sheet_name)

af_sp <- all_xy |> filter(key == "AF") |> to_sf()
cc_sp <- all_xy |> filter(key == "CC") |> to_sf()
nt_sp <- all_xy |> filter(key == "NT") |> to_sf()
of_sp <- all_xy |> filter(key == "OF") |> to_sf()

message(sprintf("Rows — AF: %d | CC: %d | NT: %d | OF: %d",
                nrow(af_sp), nrow(cc_sp), nrow(nt_sp), nrow(of_sp)))

#------------------------------------------------------------------------------#
# Build maps
#------------------------------------------------------------------------------#
af_map <- study_area(af_sp)
cc_map <- study_area(cc_sp)
nt_map <- study_area(nt_sp)
of_map <- study_area(of_sp)

#------------------------------------------------------------------------------#
# Save maps
#------------------------------------------------------------------------------#
tmap_save(af_map, file.path(path_graph, "Fig_1_1_AF.png"), width = 80, units = "mm")
tmap_save(cc_map, file.path(path_graph, "Fig_1_2_CC.png"), width = 80, units = "mm")
tmap_save(nt_map, file.path(path_graph, "Fig_1_3_NT.png"), width = 80, units = "mm")
tmap_save(of_map, file.path(path_graph, "Fig_1_4_OF.png"), width = 80, units = "mm")

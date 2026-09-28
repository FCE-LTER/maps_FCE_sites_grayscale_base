library(tidyverse)
library(sf)
# NOTE: This script has been updated to work with tmap 4 and won't work with earlier versions of tmap.
# Uncomment the line below to install the latest version of tmap.
# install.packages("tmap")
# Check for tmap 4.0 or higher
if (packageVersion("tmap") < "4.0") {
  stop(paste(
    "This script has been updated to work with tmap tmap 4.0 or higher.\n",
    "The current version of tmap is", packageVersion("tmap"), ".\n",
    "Please update tmap by running: install.packages('tmap')"
  ))
}
library(tmap)
# install.packages("tmaptools")
library(tmaptools)
# install.packages("grid")
library(grid)
library(sp)

# Read shapefiles used in the map
ENP <- st_read("./shapefiles/enp_boundary_line.shp")
FLstate <- st_read("./shapefiles/Florida_State_Boundary.shp")
FLstate_inset <- st_read("./shapefiles/statebnd_poly.shp")
canals <- st_read("./shapefiles/canals_utm.shp")
FCEsites <- st_read("./shapefiles/ltersites_current_utm.shp")
SRS <- st_read("./shapefiles/srs_utm_clipped.shp")
TS <- st_read("./shapefiles/taylor_slough_utm_clipped.shp")
roads <- st_read("./shapefiles/US41_US1.shp")
Tamiami_bridges <- st_read("./shapefiles/Tamiami_trail_bridges.shp")
CERP_projects_eastern_ENP <- st_read("./shapefiles/CERP_Project_Boundaries_ENP_east.shp")
saltwater_east_2018 <- st_read("./shapefiles/InlandExtentOfSaltwater_2018.shp")

# Select a subset of current FCE LTER sites to display on the map 
FCEsites_subset <- filter(
  FCEsites, 
  SITE == "SRS-1d" |
    SITE == "SRS-2" |
    SITE == "SRS-3" |
    SITE == "SRS-4" |
    SITE == "SRS-5" |
    SITE == "SRS-6" |
    SITE == "TS/Ph-1a" |
    SITE == "TS/Ph-2b" |
    SITE == "TS/Ph-3" |
    SITE == "TS/Ph-6b" |
    SITE == "TS/Ph-7b" |
    SITE == "TS/Ph-9" |
    SITE == "TS/Ph-10" |
    SITE == "TS/Ph-11" 
)

# Bounding coordinates are UTM Zone 17N and specify the map extent
northing_max = 2854277
northing_min = 2747545
easting_max = 584555
easting_min = 440316

# Calculate extent of the bounding box for the main map and the inset map in the upper left corner
map_extent = matrix(c(easting_min,northing_min,easting_min,northing_max,easting_max,northing_max,easting_max,northing_min,easting_min,northing_min),ncol=2, byrow=TRUE)

map_extent_coords = list(map_extent)

bbox_map_extent <- st_polygon(map_extent_coords) %>%
  st_sfc(crs = 32617)

tmap_mode("plot") 

# Plotting layers in the main map
# Comment out layers to remove them from the map
main_map <- tm_shape(FLstate, bbox = bbox_map_extent) +
  tm_polygons(
    fill = "#f0f0f0",
    col = "#525252"
  ) +
  # Shark River Slough
  tm_shape(SRS) +
  tm_polygons(
    fill = "#969696",
    col = "#969696"
  ) +
  tm_add_legend(
    type = "polygons", 
    fill = "#969696",
    col = "#969696",
    labels = "Shark River Slough",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 5 # position in the legend
  ) +
  # Taylor Slough
  tm_shape(TS) +
  tm_polygons(
    fill = "#cccccc",
    col = "#cccccc"
  ) +
  tm_add_legend(
    type = "polygons",
    fill = "#cccccc",
    col = "#cccccc",
    labels = "Taylor Slough",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 4
  ) +
  # CERP projects
  # source: South Florida Water Management District
  # https://hub.arcgis.com/datasets/8b529d03ce534b27addc573c4166ebd8_0/explore
  tm_shape(CERP_projects_eastern_ENP) +
  tm_polygons(
    fill = "#ffcc00",
    col = "#cccccc"
  ) +
  tm_add_legend(
    type = "polygons", 
    fill = "#ffcc00",
    col = "#cccccc",
    size = 1,
    labels = "CERP restoration projects",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 8
  ) +
  # Everglades National Park boundary
  # source: National Park Service
  tm_shape(ENP) +
  tm_lines(
    col = "#525252",
    lwd = 1.5,
    lty = "dashed"
  ) +
  tm_add_legend(
    type = "lines", 
    lwd = 1.5,
    lty = "dashed",
    col = "#525252", 
    labels = "Everglades National Park",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 6
  ) +
  # US Highways (US1 and US 41 (AKA Tamiami Trail))
  # source: Florida Department of Transportation, Transportation Data & Analytics Office (TDA)
  tm_shape(roads) +
  tm_lines(
    col = "#cc0000",
    lwd = 1.5,
    lty = "solid"
  ) +
  tm_add_legend(
    type = "lines", 
    lwd = 1.5, 
    col = "#cc0000", 
    labels = "US Highways",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 2
  ) +
  # Tamiami Trail bridges 
  # source: Florida Department of Transportation, Transportation Data & Analytics Office (TDA)
  tm_shape(Tamiami_bridges) +
  tm_lines(
    col = "#ffff00",
    lwd = 8,
    lty = "solid"
  ) +
  tm_add_legend(
    type = "lines", 
    lwd = 6, 
    col = "#ffff00", 
    labels = "Tamiami Trail bridges",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 7
  ) +
  # Major canals
  # source: South Florida Water Management District
  tm_shape(canals) +
  tm_lines(
    col = "#0000cc",
    lwd = 0.75,
    lty = "solid"
  ) +
  tm_add_legend(
    type = "lines", 
    lwd = 0.75, 
    col = "#0000cc", 
    labels = "Canals",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 1
  ) +
  # Approximate inland extent of saltwater interface in the Biscayne aquifer in 2018, Miami-Dade County
  # source: U.S. Geological Survey (USGS), in cooperation with Miami-Dade County
  tm_shape(saltwater_east_2018) +
  tm_lines(
    col = "#00cccc",
    lwd = 3,
    lty = "solid"
  ) +
  tm_add_legend(
    type = "lines", 
    lwd = 3, 
    col = "#00cccc", 
    labels = "Saltwater intrusion 2018",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 9
  ) +
  # Subset of Florida Coastal Everglades (FCE) LTER sites
  # source: Florida Coastal Everglades LTER program
  # https://doi.org/10.6073/pasta/82c13533b7323a4a7f39934c752f0da0
  tm_shape(FCEsites_subset) +
  tm_dots(
    fill = "#000000",
    size = 0.5
  ) +
  tm_add_legend(
    type = "symbols", 
    size=.5,
    shape = 19,
    fill = "#000000", 
    labels = "FCE sites",
    item.height = 0.8,
    item.space = 0,
    item_text.margin = 0.6,
    z = 0,
    bg.alpha = 0
  ) +
  # Graticules along the left and bottom of the map
  tm_graticules(
    lines = FALSE, 
    labels.size = 0.8
  )  +
  tm_compass(
    north = 0,
    type = "arrow",
    text.size = 1.2,
    size = NA,
    position = tm_pos_in(pos.h = 0.87, pos.v = 0.3)
  ) +
  tm_scalebar(
    breaks = seq(0, 20, by = 5),
    text.size = 0.8,
    text.color = "#000000", 
    color.dark = "#000000",
    color.light = "#FFFFFF",
    lwd = 1,
    position = tm_pos_in(pos.h = 0.77, pos.v = 0.11),
    bg.color = NA,
    bg.alpha = NA
  ) +
  
  tm_layout(
    bg.color = "#ffffff",
    outer.margins = 0.001,
    inner.margins = 0.02,
    legend.show = TRUE,
    legend.text.size = 0.85,
    legend.position = tm_pos_in("left","bottom")
  ) 

# Inset map in upper left corner
inset_map <- tm_shape(FLstate_inset) +
  tm_polygons(
    fill = "#f0f0f0",
    col = "#525252",
    lwd = 0.5, 
    lty = "solid"
  ) + 
  tm_shape(
    bbox_map_extent
  ) +
  tm_polygons(
    fill = "#ffffff",
    fill_alpha = 0,
    col = "#000000", 
    lwd = 2, 
    lty = "solid"
  ) + 
  tm_title(
    "FLORIDA",
    position = tm_pos_in("center", "TOP"),
    size = 0.8
  )  +
  tm_layout(
    legend.show = FALSE, 
    bg.color = "#ffffff", 
    inner.margins = 0.15, 
    frame = TRUE
  ) 

print(main_map, vp=viewport(x = 0.5, y = 0.5, width= 1, height= 0.98, just = c("center", "center")))
print(inset_map, vp=viewport(x = 0.221, y = 0.830, width= 0.33, height= 0.33, just = c("center", "center")))
# Might need to adjust the position of the inset_map viewport, x lower = left, y higher = up

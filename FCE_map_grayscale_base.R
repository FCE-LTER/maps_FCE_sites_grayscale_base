library(tidyverse)
library(sf)
# install.packages("tmap")
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
FCEsites <- st_read("./shapefiles/ltersites_current_utm_one_pt_per_site.shp")
SRS <- st_read("./shapefiles/srs_utm_clipped.shp")
TS <- st_read("./shapefiles/taylor_slough_utm_clipped.shp")
roads <- st_read("./shapefiles/US41_US1.shp")
Tamiami_bridges <- st_read("./shapefiles/Tamiami_trail_bridges.shp")
CERP_projects_eastern_ENP <- st_read("./shapefiles/CERP_Project_Boundaries_ENP_east.shp")
saltwater_east_2018 <- st_read("./shapefiles/InlandExtentOfSaltwater_2018.shp")

# Bounding coordinates are UTM Zone 17N and specify the map extent
northing_max = 2852277
northing_min = 2748545
easting_max = 582555
easting_min = 461316

tmap_mode("plot") 

# Plotting layers in the main map
# Comment out layers to remove them from the map
main_map <- tm_shape(FLstate, projection = 32617, bbox = c(easting_min,northing_min,easting_max,northing_max)) +
  tm_polygons(col = "#f0f0f0",
              alpha = NA,
              border.col = "#525252",
              border.alpha = NA) +
  # Shark River Slough
  tm_shape(SRS) +
  tm_polygons(
    col = "#969696",
    border.col = "#969696"
  ) +
  tm_add_legend(
    type = "fill", 
    col = "#969696",
    border.col = "#969696",
    labels = "Shark River Slough",
    z = 5 # position in the legend
  ) +
  # Taylor Slough
  tm_shape(TS) +
  tm_polygons(
    col = "#cccccc",
    border.col = "#cccccc"
  ) +
  tm_add_legend(
    type = "fill", 
    col = "#cccccc",
    border.col = "#cccccc", 
    labels = "Taylor Slough",
    z = 4
  ) +
  # CERP projects
  # source: South Florida Water Management District
  # https://hub.arcgis.com/datasets/8b529d03ce534b27addc573c4166ebd8_0/explore
  tm_shape(CERP_projects_eastern_ENP) +
  tm_polygons(
    col = "#ffcc00",
    border.col = "#cccccc"
  ) +
  tm_add_legend(
    type = "fill", 
    col = "#ffcc00",
    border.col = "#cccccc",
    size = 1,
    labels = "CERP restoration projects",
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
    type = "line", 
    lwd = 1.5,
    lty = "dashed",
    col = "#525252", 
    labels = "Everglades National Park",
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
    type = "line", 
    lwd = 1.5, 
    col = "#cc0000", 
    labels = "US Highways",
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
    type = "line", 
    lwd = 6, 
    col = "#ffff00", 
    labels = "Tamiami Trail bridges",
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
    type = "line", 
    lwd = 0.75, 
    col = "#0000cc", 
    labels = "Canals",
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
    type = "line", 
    lwd = 3, 
    col = "#00cccc", 
    labels = "Saltwater intrusion 2018",
    z = 9
  ) +
  # Florida Coastal Everglades (FCE) LTER sites
  # source: Florida Coastal Everglades LTER program
  # https://doi.org/10.6073/pasta/82c13533b7323a4a7f39934c752f0da0
  tm_shape(FCEsites) +
  tm_symbols(
    size=.25,
    shape = 19,
    col = "#000000"
  ) +
  # Graticules along the left and bottom of the map
  tm_graticules(
    lines = FALSE, 
    labels.size = 0.8
  ) +
  tm_add_legend(
    type = "symbol", 
    size=.25,
    shape = 19,
    col = "#000000", 
    labels = "FCE sites",
    z = 0
  ) +
  tm_compass(
    north = 0,
    type = NA,
    text.size = 1.2,
    size = NA,
    show.labels = 1,
    cardinal.directions = c("N", "E", "S", "W"),
    text.color = NA,
    color.dark = NA,
    color.light = NA,
    lwd = 1,
    position = NA,
    bg.color = NA,
    bg.alpha = NA,
    just = NA
  ) +
  tm_scale_bar(
    width = 0.15,
    text.size = 0.8,
    text.color = NA,
    color.dark = "black",
    color.light = "white",
    lwd = 1,
    position = NA,
    bg.color = NA,
    bg.alpha = NA,
    just = NA
  ) +
  
  tm_layout(
    bg.color = "#ffffff",
    outer.margins = 0.0005,
    legend.show = TRUE,
    legend.text.size = 0.9,
    legend.position = c("left","bottom")
  ) 

# Calculate extent of the bounding box for the inset map in the upper left corner
map_extent = matrix(c(easting_min,northing_min,easting_min,northing_max,easting_max,northing_max,easting_max,northing_min,easting_min,northing_min),ncol=2, byrow=TRUE)

map_extent_coords = list(map_extent)

bbox_map_extent <- st_polygon(map_extent_coords) %>%
  st_sfc(crs = 32617)

# Inset map in upper left corner
inset_map <- tm_shape(FLstate_inset, projection = 32617) +
  tm_polygons(
    border.col = "#525252", 
    col = "#f0f0f0", 
    lwd = 0.5, 
    lty = "solid"
  ) + 
  tm_shape(bbox_map_extent, projection = 32617) +
  tm_polygons(
    alpha = 0, 
    border.alpha = 1, 
    col=NA,
    border.col = "#000000", 
    lwd = 2, 
    lty = "solid"
  )  + 
  tm_layout(
    "FLORIDA", 
    legend.show = FALSE, 
    bg.color = "#ffffff", 
    title.size = 0.9, 
    inner.margins = 0.1, 
    title.position = c("center", "TOP"), 
    frame = TRUE
  ) 

print(main_map, vp=viewport(x = 0.5, y = 0.5, width= 1, height= 1, just = c("center", "center")))
print(inset_map, vp=viewport(x = 0.271, y = 0.865, width= 0.27, height= 0.28, just = c("center", "center")))
# Might need to adject the position of the inset_map viewport, x lower = left, y higher = up
#Create map of State of New Mexico

#useful website
#https://cran.r-project.org/web/packages/osmdata/vignettes/osmdata.html
#To download large data
#https://docs.ropensci.org/osmextract/

#Libraries
library(tidyverse)
library(osmdata) # package for working with streets
library(osmextract)
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(sf)

#New mexico colour palette
NM_brown <- "#665600"
NM_red <- "#bf0a30"
NM_blue <- "#40e0d0"
NM_yellow <- "#ffd700"
NM_maroon <- "#4c0413"

#Load in data ####
#Get new Mexico area as polygon
NM_area <- osmdata::getbb("New Mexico")
NM <- osmdata::getbb("New Mexico", format="polygon")

#Lines ####
# #Get all osm_lines of New mexico
# osm_lines <- oe_get("us/new-mexico", stringsAsFactors = FALSE, quiet = TRUE)
# #Save this and load
# save(osm_lines, file = "nm_osm_lines.RData")
#Load osm data
load("nm_osm_lines.RData")
# River ####
#Fiver features to extract
waterway_feats <- c("river")
#Extract river info
rivers_nm <- osm_lines[osm_lines$waterway %in% waterway_feats, ]

#Get length of rivers to filter rivers by length
river_all_names <- unique(rivers_nm$name)
#Loop through river names
river_lengths_vector <- c()
for (i in 1:length(river_all_names)) {
  r <- river_all_names[i]
  line_string_vector <- rivers_nm[rivers_nm$name == r,"geometry"]
  length <- sum(st_length(line_string_vector))
  river_lengths_vector[r] <- length
}
#Sort vector
river_lengths_vector <- sort(river_lengths_vector, decreasing = TRUE)
#Extract rivers of interest
#Big rivers
river_names <- names(river_lengths_vector[river_lengths_vector > 100000])
big_rivers_nm <- rivers_nm[rivers_nm$name %in% river_names,]
#BIg and Small rivers (set so big rivers go on top)
river_names <- names(river_lengths_vector[river_lengths_vector > 10000])
smaller_rivers_nm <- rivers_nm[rivers_nm$name %in% river_names,]

# Highways ####
#Highway features to extract
highway_features = c("motorway", "primary", "motorway_link", "primary_link")
#Extract 
highway_nm <- osm_lines[osm_lines$highway %in% highway_features, ]
#Secondary roads
#Highway features to extract
road_features = c("secondary",  "secondary_link")
#Extract 
roads_nm <- osm_lines[osm_lines$highway %in% road_features, ]
# Remove line data ####
remove(osm_lines)


# Polygons ####
# #Get multipolygon data
# osm_polygon <- oe_get("us/new-mexico", layer="multipolygons", stringsAsFactors = FALSE, quiet = TRUE)
# #Save this and load
# save(osm_polygon, file = "nm_osm_polygons.RData")
#Load data
load("nm_osm_polygons.RData")
# Natural features ####
#Forest and woods
#Extract 
forest_nm <- osm_polygon[osm_polygon$landuse %in% "forest",]
wood_nm <- osm_polygon[osm_polygon$natural %in% "wood",]
nature_reserve_forest_nm <- osm_polygon[osm_polygon$leisure %in% "nature_reserve",]
nature_reserve_forest_nm <- nature_reserve_forest_nm[grep(pattern = "Forest", nature_reserve_forest_nm$name),]
gila_forest <- osm_polygon[grep("Gila National Forest", osm_polygon$name),]
lincoln_forest <- osm_polygon[(osm_polygon$name == "Lincoln National Forest"),]
#Lincoln forest is chopped in half by mescalero reservation so I will add it as a forest
mescalero <- osm_polygon[(osm_polygon$name == "Mescalero Apache Indian Reservation"),]
#Mountains
#Extract
mountains <- osm_polygon[osm_polygon$leisure %in% "nature_reserve",]
mountains <- mountains[grep(pattern = "Mountain", mountains$name),]
#White sands
white_sands_nm <- osm_polygon[osm_polygon$leisure %in% "nature_reserve",]
white_sands_nm <- white_sands_nm[grep(pattern = "White Sands National Park", white_sands_nm$name),]
#White sands missile range
white_sands_missile_range_nm <- osm_polygon[osm_polygon$military %in% "range",]
white_sands_missile_range_nm <- white_sands_missile_range_nm[grep(pattern = "White Sands Missile Range", white_sands_missile_range_nm$name),]
#Mescalero indian reservation
#mescalero <- osm_polygon[osm_polygon$name == "Mescalero Apache Indian Reservation",]
#Water features
water <- osm_polygon[osm_polygon$natural %in% "water",]


#Plot ####
map_plot <- ggplot() +
  #Geoms towards top go below geom towards bottom
  #Mounatins
  geom_sf(data= mountains, color="#FE7FFF",fill="#FE7FFF") +
  #Forest
  geom_sf(data = forest_nm, color = "#C0FF7F", fill = "#C0FF7F") +
  geom_sf(data = wood_nm, color = "#C0FF7F", fill = "#C0FF7F") +
  geom_sf(data = nature_reserve_forest_nm, color = "#C0FF7F", fill = "#C0FF7F") +
  geom_sf(data = gila_forest, color = "#C0FF7F", fill = "#C0FF7F") +
  geom_sf(data = lincoln_forest, color = "#C0FF7F", fill = "#C0FF7F") +
  geom_sf(data = mescalero, color = "#C0FF7F", fill = "#C0FF7F") +
  #Mescalero indian reservation
  #geom_sf(data = mescalero, color = NM_maroon, fill = NM_red) +
  #White sands
  geom_sf(data = white_sands_nm, color = "white", fill = "white") +
  geom_sf(data = white_sands_missile_range_nm, color = NM_red, fill = NM_yellow) +
  #Rivers
  geom_sf(data = smaller_rivers_nm, color = "#7fc0ff", size=0.5, alpha=0.5) +
  geom_sf(data = big_rivers_nm, color = "#7fc0ff", size=2) +
  #Water features like lakes
  geom_sf(data = water, color = "#7fc0ff", fill = "#7fc0ff") +
  #Roads
  geom_sf(data = roads_nm, color = "#ffbe7f", size=0.5, alpha=0.5) +
  geom_sf(data = highway_nm, color = "#ffbe7f", size=1) +
  #Format stuff
  coord_sf(xlim = NM_area[c(1,3)], ylim = NM_area[c(2,4)], expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "#282828")
  )

ggsave("NM_map.png", plot = map_plot, 
       device = "png", dpi = 300, units = "mm", height = 420, width = 297)



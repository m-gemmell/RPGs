#Trying a tutorial from: https://joshuamccrain.com/tutorials/maps/streets_tutorial.html
#Other tutorial is https://ggplot2tutor.com/tutorials/streetmaps

#Libraries
library(tidyverse)
library(osmdata) # package for working with streets
library(showtext) # for custom fonts
library(ggmap)
library(rvest)
library(sf)

# Grabbing data ####
#See all the features we can see
osmdata::available_features()

#Each feature has tags
#See all available tags
osmdata::available_tags("highway")

#Get geographic location of a place
osmdata::getbb("Atlanta Georgia")

#Get info on streets
big_streets <- 
  #Only grab features from our are of interest
  osmdata::getbb("Asheville United States") %>%
  osmdata::opq() %>%
  #Add feature
  #Key set tot he feature we want, highway
  osmdata::add_osm_feature(key = "highway",
                           #Values set to the tags of the feature we want
                           value = c("motorway", "primary", "motorway_link","primary_link")) %>%
  #Convert into format that works with ggmap package
  osmdata::osmdata_sf()

#Grab some more data
#This case medium and small streets
med_streets <- getbb("Asheville United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("secondary", "tertiary", "secondary_link", "tertiary_link")) %>%
  osmdata_sf()


small_streets <- getbb("Asheville United States")%>%
  opq()%>%
  add_osm_feature(key = "highway", 
                  value = c("residential", "living_street",
                            "unclassified",
                            "service", "footway"
                  )) %>%
  osmdata_sf()

#Also grab river and railways
river <- getbb("Asheville United States")%>%
  opq()%>%
  add_osm_feature(key = "waterway", value = "river") %>%
  osmdata_sf()

railway <- getbb("Asheville United States")%>%
  opq()%>%
  add_osm_feature(key = "railway", value="rail") %>%
  osmdata_sf()

#Plots ####

#Map big streets
ggplot() +
  sf::geom_sf(data = big_streets$osm_lines, inherit.aes = FALSE, color = "black")

#Map rivers
ggplot() +
  geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "black")

#Big streets and river
ggplot() +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black") +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "black")

#Street and river zoomed into our area of interest
ggplot() +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black") +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "black") +
  coord_sf(xlim = c(-82.65, -82.48), 
           ylim = c(35.5, 35.65),
           expand = FALSE)
#All our features
ggplot() +
  geom_sf(data = river$osm_lines,
          inherit.aes = FALSE,
          color = "steelblue",
          size = .8,
          alpha = .3) +
  geom_sf(data = railway$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .2,
          linetype="dotdash",
          alpha = .5) +
  geom_sf(data = med_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .3,
          alpha = .5) +
  geom_sf(data = small_streets$osm_lines,
          inherit.aes = FALSE,
          color = "#666666",
          size = .2,
          alpha = .3) +
  geom_sf(data = big_streets$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = .5,
          alpha = .6) +
  coord_sf(xlim = c(-82.65, -82.48), 
           ylim = c(35.5, 35.65),
           expand = FALSE)

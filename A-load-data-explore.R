# Author: _crushton 10/12/21
# Combining csv and json files into one table and cleaning structure into csv
# Scraped EdGap data SAT and household income 2016 (www.edgap.org)

# ================== Library Imports ===========================
# library(tidyverse)
# library(ggplot2) # For data visualization
library(readr) # For CSV file I/O
library(magrittr) # Pipe %>%
library(rjson) # To load json
library(leaflet) # To plot spatial data
library(geojsonio) # To read/write geojson
library(sf) # To convert shapefiles and find intersections
library(dplyr) # to join dataframes

# ================== Loading Data =============================
setwd("~/R-projects/edgap")
schSAT <- read.csv("./Data/csv/merged.csv", header=TRUE) # School SAT scores from 2016
nhIncome <- fromJSON(file="./Data/json/merged.json") # Neighborhood shapefiles and household income
neighborhoods <- geojsonio::geojson_read("./Data/json/merged.geojson", what = "sp")

# ================== Cleaning Data =============================
cols.num = c(1,2,3,4,6,8,11,12,13,17,19,22,23,24,25)
neighborhoods@data[cols.num] <- sapply(neighborhoods@data[cols.num], as.numeric)
neighborhoods@data <- rename(neighborhoods@data, tract = TRACTCE10)
# ================== Plotting Map ==============================
pal <- colorNumeric("viridis", NULL)
pal2 <- colorNumeric("Reds", domain = 746:1374)

# Family Income
leaflet(neighborhoods) %>% addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(med_fam_inc)),
              label = ~paste0(NAMELSAD10, ": ", med_fam_inc)) %>%
  addLegend(pal = pal, values = ~log10(med_fam_inc), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% 
  addCircleMarkers(schSAT, lng = ~schSAT$lon, lat = ~schSAT$lat,
                   radius = 3,
                   color = ~pal2(schSAT$OneYr),
                   label = ~paste0(schSAT$school_name, ": ", schSAT$OneYr)) %>%
  addLegend(pal = pal2, values = schSAT$OneYr, opacity = 1.0, title = "SAT score")

# Household Income
leaflet(neighborhoods) %>% addTiles() %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(log10(med_hh_inc)),
              label = ~paste0(NAMELSAD10, ": ", med_hh_inc)) %>%
  addLegend(pal = pal, values = ~log10(med_hh_inc), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) round(10^x))) %>% 
  addCircleMarkers(schSAT, lng = ~schSAT$lon, lat = ~schSAT$lat,
                   radius = 3,
                   color = ~pal2(schSAT$OneYr),
                   label = ~paste0(schSAT$school_name, ": ", schSAT$OneYr)) %>%
  addLegend(pal = pal2, values = schSAT$OneYr, opacity = 1.0, title = "SAT score")

# ================== Joining Data =============================
# Convert dataframes to shapefiles
SATsf <- sf::st_as_sf(schSAT, coords=c("lon","lat"), crs=4326)
hoodsf <- sf::st_as_sf(neighborhoods, crs=4326)

# Plot for visual reference, uses sf::plot_sf:
plot(hoodsf$geometry, reset=FALSE)
plot(SATsf$geometry, add=TRUE, reset=FALSE, pch=16, col="red", cex=1.5)
axis(1, line=-1) ; axis(2, line=-1, las=1)

# Find the intersection of schools with census tracts
# code from Berry at https://gis.stackexchange.com/questions/282750/identify-polygon-containing-point-with-r-sf-package/343477?newreg=8a56650b82cc4c9fb171c9e47b07ccab
int <- sf::st_intersects(SATsf$geometry, hoodsf$geometry)
int
int[119] <- 3026 # Arlington HS did not have a tract, chose the next closest
# since tract alone is not unique, using tract & county
SATsf$tractcty <- paste0(as.character(hoodsf$tract[unlist(int)]), as.character(hoodsf$COUNTYFP10[unlist(int)]))
schSAT$tractcty <- SATsf$tractcty
neighborhoods@data$tractcty <- paste0(as.character(neighborhoods@data$tract), as.character(neighborhoods@data$COUNTYFP10))
# Check intersection
# (3) Carpinteria: 001706083,  (56) Glendora: 400402037, (419) Aliso: 062640059
schSAT[c(3,56,419), ]

# Join neighborhood data with school data with tract as key
schSAT <- left_join(schSAT, neighborhoods@data, by="tractcty")

# Check the join
nrow(schSAT) # 435
sum(as.numeric(duplicated(schSAT$school_name))) # 6
schSAT$school_name[duplicated(schSAT$school_name)] # c("Canyon High", "Valencia High", "Santiago High", "Pacifica High", "Centennial High", John F. Kennedy High")

# Upon investigation, duplicated names are schools in different locations with different stats
'###############################################################################
#   World Travel                     ###########################################
#   Build Shiny Input                ###########################################
################################################################################'

  library(readxl)
  library(dplyr)
  library(jsonlite)

  # Set working directory
  setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
  
  # Load helper functions
  source('utils.R')
  
  # Complete list w/o coordinates
  raw_all <- read_excel("data/input/travel.xlsx", 
                         col_types = c("date", "date", "text", "text", "text"))
  raw_all$place_country <- paste(raw_all$place, raw_all$country, sep = ',')
  

  # Load list with existing coordinates (if available)
  if(file_test("-f", 'data/output/travel_coords.csv')) {
    
    raw_coord <- read.csv("data/output/travel_coords.csv", header = TRUE, sep = ";", 
                          stringsAsFactors = F)
    raw_coord$place_country <- paste(raw_coord$place, raw_coord$country, sep = ",")
    write.csv(raw_coord, 'data/output/travel_coords_bkp.csv', sep = ";")
    
    # Create list of places without coordinates
    places_list <- unique(merge(raw_all["place_country"], 
                                raw_coord[c("place_country", "lat", "lng")], all.x = T))
    nocoord <- places_list[rowSums(is.na(places_list)) > 0, "place_country"]
    
  } else nocoord <- raw_all$place_country

  # Get coordinates for places without
  if(length(nocoord) > 0) {
          coord <- get_geo_osm_df(nocoord)
          names(coord) <- c("place_country", "lng2", "lat2")
  } else {
          coord <- data.frame(place_country=character(),
                              lng2=character(), 
                              lat2=character(),
                              stringsAsFactors = F)
  }
  
  coord$place <- sapply(strsplit(as.character(coord$place_country), ","), `[`, 1)
  coord <- coord[,-1]
  
  # Scaffold for final frame
  world_all <- data.frame(inbound = as.Date(raw_all$inbound, "%d.%m.%Y"),
                          outbound = as.Date(raw_all$outbound, "%d.%m.%Y"),
                          place = raw_all$place,
                          country = raw_all$country,
                          Besch = raw_all$desc,
                          stringsAsFactors = F)
  
  # Merge with scaffold frame
  world_all_new <- unique(merge(world_all, raw_coord[c("place", "lat", "lng")], all.x = T))
  world_all_new <- merge(world_all_new, coord, all.x = T)
  world_all_new[is.na(world_all_new)] <- ""
  
  world_all_new$lat <- paste0(world_all_new$lat, world_all_new$lat2)
  world_all_new$lng <- paste0(world_all_new$lng, world_all_new$lng2)
  
  world_all_new <- world_all_new[1:7]
  
  world_all_new$inbound = as.Date(world_all_new$inbound, "%d.%m.%Y")
  world_all_new$outbound = as.Date(world_all_new$outbound, "%d.%m.%Y")
  world_all_new <- world_all_new[order(world_all_new$inbound),]
  
  # Calculate number of days
  world_all_new$days <- as.numeric((world_all_new$outbound - world_all_new$inbound) + 1)
  
  # Write table
  write.table(world_all_new, "data/output/travel_coords.csv", 
              sep = ';', row.names = FALSE)
  
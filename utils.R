'###############################################################################
#   World Travel                     ###########################################
#   Utils                            ###########################################
################################################################################'

### Get coordiantes of single place (Open Street Map)
### Thx to https://datascienceplus.com/osm-nominatim-with-r-getting-locations-geo-coordinates-by-its-address/
get_geo_osm <- function(place)
{
  if(suppressWarnings(is.null(place)))
    return(data.frame())
  tryCatch(
    d <- jsonlite::fromJSON( 
      gsub('\\@addr\\@', gsub('\\s+', '\\%20', place), 
           'http://nominatim.openstreetmap.org/search/@addr@?format=json&addressdetails=0&limit=1')
    ), error = function(c) return(data.frame())
  )
  if(length(d) == 0) return(data.frame())
  return(data.frame(Ort_Land = place, lon = as.numeric(d$lon), lat = as.numeric(d$lat)))
}



### Get data frame with coordiantes from list of places
get_geo_osm_df <- function(places_list) {
  
  places_coord <- suppressWarnings(lapply(places_list, get_geo_osm) %>% 
                                     bind_rows() %>% data.frame())
  return(places_coord)
}
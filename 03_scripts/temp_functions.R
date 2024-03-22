gpi_map_theme = function (base_size = 9, base_family = ""){
  theme_bw(base_size = base_size, base_family = base_family) %+replace%
    theme(axis.line = element_blank(), axis.text = element_blank(),
          axis.ticks = element_blank(), axis.title = element_blank(),
          panel.grid = element_blank(), panel.border = element_blank(),
          panel.spacing = unit(0, "lines"),  strip.background = element_blank(),
          legend.justification = c(0, 0), legend.position = c(0,0))
}

add_world_boundaries = function(map){
  require(tidyverse)
  require(sf) # Ensure sf package is loaded for spatial data handling
  
  # Use iepg_get_gadm function to get level0 (country-level) shapefile information
  world <- iepg_get_gadm("level0")
  
  # Optional: You might want to perform any necessary transformations or filtering on the world dataset here
  
  # Add the world boundaries to the map. Assuming 'world' is an sf object, we do not need to specify LON, LAT manually
  map = map +
    geom_sf(data = world,
            color = "black", fill = NA, size = 0.5)
  
  return(map)
}
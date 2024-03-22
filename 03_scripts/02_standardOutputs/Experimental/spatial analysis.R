library(spatstat)
library(tidyverse)

spatial.df <- rio::import("02_data/processed/clean_TT.rds") %>% 
  filter(!is.na(longitude) | !is.na(latitude))

long_range <- range(spatial.df$longitude, na.rm = TRUE)
lat_range <- range(spatial.df$latitude, na.rm = TRUE)

points <- ppp(spatial.df$longitude, spatial.df$latitude, 
              window=owin(xrange=long_range, yrange=lat_range))


fit <- ppm(points ~ 1)
plot(points)
plot(intensity(fit))
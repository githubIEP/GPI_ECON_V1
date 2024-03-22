##### ----- Animated Charts

f_LibraryLoader(tidyverse,
                iepg,
                lubridate,
                rio,
                sf,
                zoo,
                extrafont,
                gganimate,
                transformr,
                gifski)

attacks.df <- rio::import("02_data/processed/clean_TT.rds") %>%
  filter(year >= TT_FIRST_YEAR, year <= GTI_YEAR, deaths_total >= 0) %>% 
  select(event_id, date, year, geocode, latitude, longitude, deaths_total) %>%
  distinct(event_id, date, year, geocode, latitude, longitude, deaths_total)

incidents.map <- iepg_get_gadm("level0") %>%
  filter(!is.na(gpi_region))

# Assuming incidents.map and attacks.df are already prepared

library(gganimate)

# Assuming incidents.map and attacks.df are already prepared

p = ggplot(incidents.map) +
  geom_sf() +
  geom_point(data = attacks.df, 
             aes(x = longitude, y = latitude, size = deaths_total),
             alpha = 0.8,
             color = "red") +
  theme_void() + 
  theme(legend.position = "none") + 
  scale_size_continuous(range = c(min(attacks.df$deaths_total + 1), max(attacks.df$deaths_total) * 0.02))

# Animate the plot with fading effect and year title
animated_plot <- p + 
  transition_states(states = attacks.df$year, transition_length = 2, state_length = 1) +
  ease_aes('linear') +
  labs(title = 'Year: {floor(closest_state)}') +
  shadow_mark(alpha = 0, past = TRUE, future = TRUE)

# Save the animation
anim <- animate(animated_plot, nframes = 200, width = 800, height = 600, fps = 10, renderer = gifski_renderer())
anim_save("animated_attacks.gif", animation = anim)

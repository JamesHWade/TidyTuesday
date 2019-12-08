library(tidyverse)
library(leaflet)
library(ggthemes)
library(gganimate)
# library(cowplot)
# library(patchwork)
# library(ggtext)
# library(osmdata)
# library(sf)
# library(ggpointdensity)
# library(beepr)


if (!file.exists('data/2019-49_tickets.csv')) {
  d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv')
  write_csv(d, 'data/2019-49_tickets.csv')
}

d <- read_csv('data/2019-49_tickets.csv')

philly <- us.cities %>% filter(name == "Philadelphia PA")

glimpse(d)

d2 <- d %>% filter(lat > 39.8 & lon > -75.4) %>% 
  mutate(year = lubridate::year(issue_datetime),
         month = lubridate::month(issue_datetime),
         hour = lubridate::hour(issue_datetime),
         day = lubridate::day(issue_datetime),
         wday = lubridate::wday(issue_datetime),
         violation_desc = str_squish(str_remove(pattern = "CC|PING", violation_desc)),
         type = fct_lump(violation_desc, n = 5, other_level = "OTHER"))

ggplot() + geom_polygon(data = usa, aes(x=long, y = lat, group = group)) + 
  coord_fixed(1.3)

ggplot() + geom_polygon(data = sample_n(d2, 100), aes(x=lon, y = lat, group = type)) + 
  coord_fixed(1.3)

ggplot(sample_n(d2, 1e3)) + 
  # geom_point(aes(x = lat, y = lon, color = type), alpha = 0.2) +
  theme_void() + 
  scale_color_brewer(palette = "Spectral") +
  labs(color = "VIOLATION") +
  guides(colour = guide_legend(override.aes = list(alpha = 1))) +
  geom_polygon(aes(x = lat, y = lon)) +
  coord_fixed(1.3)

ggplot(sample_n(d2, 1e4)) + 
  ggpointdensity::geom_pointdensity(aes(x = lat, y = lon)) +
  theme_void() + 
  scale_color_viridis_c(option = 'viridis') +
  transition_states(
    type,
    transition_length = 2,
    state_length = 2
  ) +
  labs(title = 'Violation: {closest_state}', color = "")

ggplot(sample_n(d2, 1e4)) + 
  geom_bar(aes(fill = type, x = hour)) + 
  coord_polar(start = -.14) +
  scale_fill_brewer(palette = "Spectral") +
  labs(fill = "Violation") + 
  theme(legend.position = "right",
        legend.background = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_line(color = "black", size = 0.1, linetype = "dotted"),
        panel.background = element_blank(),
        plot.title = element_text(face = "bold", size = 14))

n_days <- d2 %>% select(month, day, year) %>% distinct() %>% nrow()

ggplot(d2) + 
  geom_bar(aes(x = type, fill = type), width = 0.3) +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = function(x) x / n_days, breaks = c(0, 250, 500, 750, 1000, 1250) * n_days) +
  labs(y = "Tickets per Day", x = "Violation") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")


factpal <- colorFactor(topo.colors(6), d2$type)
m <- leaflet(sample_n(d2, 1e4)) %>% 
  setView(lng = median(d2$lon), lat = median(d2$lat), zoom = 12) %>%
  addCircles(lng = ~ lon, lat = ~ lat, color = ~ factpal(type)) %>% 
  addTiles()

m %>% addProviderTiles(providers$CartoDB.Positron)

m %>% addTiles()


m <- leaflet() %>%
  addTiles() %>%  # Add default OpenStreetMap map tiles
  addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R") %>% 
  
m
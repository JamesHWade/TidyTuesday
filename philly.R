library(tidyverse)
library(leaflet)
library(ggthemes)
library(gganimate)
library(ggpubr)
library(osmdata)
library(sf)
library(ggpointdensity)

# Load in data set
if (!file.exists('data/2019-49_tickets.csv')) {
  d = read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-12-03/tickets.csv')
  write_csv(d, 'data/2019-49_tickets.csv')
}

d <- read_csv('data/2019-49_tickets.csv')

# Collect map data for Philadelphia
coord = getbb('Philadelphia, PA')

streets1 = opq(coord) %>% 
  add_osm_feature('highway', c('motorway', 'primary', 'secondary', 'tertiary')) %>% 
  osmdata_sf()

streets2 = opq(coord) %>% 
  add_osm_feature('highway', c('residential', 'living_street', 'unclassified', 'service', 'foodway')) %>% 
  osmdata_sf()

# Clean data, mostly cleaning up violation names and separating date info
d2 <- d %>% filter(between(lon, coord[1, 1], coord[1, 2]) & 
                     between(lat, coord[2, 1], coord[2, 2])) %>% 
  mutate(year = lubridate::year(issue_datetime),
         month = lubridate::month(issue_datetime),
         hour = lubridate::hour(issue_datetime),
         day = lubridate::day(issue_datetime),
         wday = lubridate::wday(issue_datetime),
         violation_desc = str_squish(str_remove(pattern = "CC|PING", violation_desc)),
         type = fct_lump(violation_desc, n = 5, other_level = "OTHER"),
         type_2 = as.numeric(type))

t1 <- d2 %>% 
  select(type_2, type) %>% 
  distinct() %>% 
  arrange(type_2) %>% 
  rename(Index = type_2, Violation = type) %>% 
  ggtexttable(rows = NULL, theme = ttheme(tbody.style = tbody_style(fill = alpha('white', 0.5)),
                                          colnames.style = colnames_style(fill = alpha('white', 0.5)))) %>% 
  ggplotGrob()

p <- ggplot() + 
  geom_sf(data = streets2$osm_lines, col = 'grey20', size = .1) +
  geom_sf(data = streets1$osm_lines, col = 'grey20', size = .1) +
  geom_pointdensity(data = sample_n(d2, 1e4), aes(y = lat, x = lon), alpha = 0.2) +
  coord_sf(xlim = coord[1,], ylim = coord[2,], expand = FALSE) +
  theme_void() +
  scale_color_viridis_c(option = 'viridis') + labs(color = "Ticket Density") +
  theme(legend.background = element_rect(fill=alpha('white', 0.5)),
        legend.position = c(0.8, 0.2)) +
  ggtitle("Philadelphia Parking Tickets - 2017 \nAll Ticket Types")

ggsave(filename = "philly_tickets.png", plot = p)

p_anim <- p + transition_states(type_2, transition_length = 1, state_length = 2) +
  ggtitle('Philadelphia Parking Tickets - 2017 \nViolation Type: {closest_state}') +
  enter_fade() + 
  exit_shrink() +
  ease_aes('sine-in-out') +
  annotation_custom(grob = t1, ymax = 39.975, xmin = -75.13) +
  theme(legend.position = c(0.88, 0.88))

anim_save(filename = "philly_tickets_animated.gif", animation = p_anim)

p_byhour <- ggplot(sample_n(d2, 1e4)) + 
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

ggsave(filename = "philly_tickets_byhour.png", plot = p_byhour)

n_days <- d2 %>% select(month, day, year) %>% distinct() %>% nrow()

p_bytype <- ggplot(d2) + 
  geom_bar(aes(x = type, fill = type), width = 0.3) +
  scale_fill_brewer(palette = "Spectral") +
  scale_y_continuous(labels = function(x) x / n_days, 
                     breaks = c(0, 250, 500, 750, 1000, 1250) * n_days) +
  labs(y = "Tickets per Day", x = "Violation") +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none")

ggsave(filename = "philly_tickets_byday.png", plot = p_bytype)


# factpal <- colorFactor(topo.colors(6), d2$type)
# m <- leaflet(sample_n(d2, 1e4)) %>% 
#   setView(lng = median(d2$lon), lat = median(d2$lat), zoom = 12) %>%
#   addCircles(lng = ~ lon, lat = ~ lat, color = ~ factpal(type)) %>% 
#   addTiles()
# 
# m %>% addProviderTiles(providers$CartoDB.Positron)
# 
# m %>% addTiles()
# 
# m <- leaflet() %>%
#   addTiles() %>%  # Add default OpenStreetMap map tiles
#   addMarkers(lng=174.768, lat=-36.852, popup="The birthplace of R")
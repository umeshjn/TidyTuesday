library(tidyverse)
library(viridis)
library(urbnmapr)
library(gridExtra)
library(scales)

## Loading data
data <- read_csv("us-airports.csv")
head(data)

## Preparing the data to plot Number of passengers served by each state in 2013
p1 <- data %>%
  filter(year == 2013) %>%
  group_by(state_abbv) %>%
  summarise(Total = sum(passengers)) %>%
  left_join(states, by = "state_abbv")  %>%
  ggplot(aes(long, lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Passengers in 2013") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis( direction = -1, labels = comma)

## Preparing the data to plot Number of passengers served by each state in 2014
p2 <- data %>%
  filter(year == 2014) %>%
  group_by(state_abbv) %>%
  summarise(Total = sum(passengers)) %>%
  left_join(states, by = "state_abbv")  %>%
  ggplot(aes(long, lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Passengers in 2014") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis( direction = -1, labels = comma)


## Preparing the data to plot Number of passengers served by each state in 2015
p3 <- data %>%
  filter(year == 2015) %>%
  group_by(state_abbv) %>%
  summarise(Total = sum(passengers)) %>%
  left_join(states, by = "state_abbv")  %>%
  ggplot(aes(long, lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Passengers in 2015") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis( direction = -1, labels = comma)



## Preparing the data to plot Number of passengers served by each state in 2016
p4 <- data %>%
  filter(year == 2016) %>%
  group_by(state_abbv) %>%
  summarise(Total = sum(passengers)) %>%
  left_join(states, by = "state_abbv")  %>%
  ggplot(aes(long, lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Passengers in 2016") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis( direction = -1, labels = comma)

## Preparing the data to plot Number of passengers served by each state in 2017
p5 <- data %>%
  filter(year == 2017) %>%
  group_by(state_abbv) %>%
  summarise(Total = sum(passengers)) %>%
  left_join(states, by = "state_abbv")  %>%
  ggplot(aes(long, lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Passengers in 2017",
       caption = "Data Source:  faa.gov - Created for #TidyTuesday") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis( direction = -1, labels = comma)


### Arranging into one view
grid.arrange(p1, p2, p3, p4, p5, 
             layout_matrix = rbind(c(1,2),c(3,4), c(5,5)), 
             top=textGrob("How many Passengers were served by all the Airports in each state?", 
                          gp=gpar(fontsize=15,font=1)))

library(tidyverse)
library(viridis)
library(urbnmapr)
library(gridExtra)

## Loading data
data <- read_csv("us-airports.csv")
head(data)

data <- left_join(data, states, by = "state_name") 

## Preparing the data to plot the most number of airports by state
p1 <- data %>%
  select(state_abbv, airport_name) %>%
  unique() %>%
  group_by(state_abbv) %>%
  summarise(Total = n()) %>%
  left_join(states, by = "state_abbv")  %>%
  ggplot(aes(long, lat, group = group, fill = Total)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Which State has the most number of Airports?",
       caption = "Data Source:  faa.gov - Created for #TidyTuesday") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis(option="magma", direction = -1) 

## Plotting Top 10 States with the most number of airports
p2 <- data %>%
  select(state_abbv, airport_name) %>%
  unique() %>%
  group_by(state_abbv) %>%
  summarise(Total = n()) %>%
  left_join(states, by = "state_abbv")  %>%
  select(state_abbv, Total) %>%
  unique() %>%
  group_by(state_abbv) %>%
  arrange(desc(Total)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(state_abbv, Total), y = Total, fill = Total)) +
  geom_bar(stat = "identity") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_fill_viridis_c(option = "magma", direction = -1) +
  coord_flip() +
  labs(x = "State Name", 
       y = "Number of Airports", 
       title = "Top 10 States with Most Airports", 
       caption = "Data Source:  faa.gov - Created for #TidyTuesday")



### Arranging into one view
grid.arrange(p1, p2, nrow = 1)


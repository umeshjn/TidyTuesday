library(tidyverse)
library(viridis)
library(urbnmapr)
library(gridExtra)
library(scales)
library(highcharter)

## Loading data
data <- read_csv("us-airports.csv")
head(data)

data_2012 <- data %>%
                filter(year == 2012) %>%
                group_by(state_abbv) %>%
                summarise(Total_2012 = sum(passengers))

data_2017 <- data %>%
              filter(year == 2017) %>%
              group_by(state_abbv) %>%
              summarise(Total_2017= sum(passengers))

passenger_change <- data_2012 %>% left_join(data_2017, by = "state_abbv")
passenger_change$Percentage <- (passenger_change$Total_2017 - passenger_change$Total_2012)/passenger_change$Total_2012 * 100                      

hcmap("countries/us/us-all", data = passenger_change, value = "Percentage",
      joinBy = c("hc-a2", "state_abbv"), name = "Fake data",
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1,
      tooltip = list(valueDecimals = 2, valuePrefix = "$", valueSuffix = " USD"))


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

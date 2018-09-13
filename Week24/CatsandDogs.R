library(tidyverse)
library(viridis)
library(urbnmapr)

data <- read_csv("cats_vs_dogs.csv")
head(data)
data$DogstoCatsRation <- data$dog_population/data$cat_population
data$CatstoDogsRation <- data$cat_population/data$dog_population
data$DogsOrCats <- ifelse(data$DogstoCatsRation>0, "LovesDogs", "LovesCats")



catsdogs <- left_join(data, states, by = "state_name") 

## Plotting Dogs to Cats Ratio
data %>%
  left_join(states, by = "state_name") %>%
  ggplot(aes(long, lat, group = group, fill = DogstoCatsRation)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Dogs to Cats Ratio", subtitle = "Southern States Love Dogs more than Cats", caption = "Data Source: Washington Post via data.world for #TidyTuesday") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis()

## Plotting Top 10 Dog loving states
data %>%
  group_by(state_name) %>%
  arrange(desc(DogstoCatsRation)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(state_name, -DogstoCatsRation), y = DogstoCatsRation, fill = DogstoCatsRation)) +
  geom_bar(stat = "identity") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_fill_viridis_c() +
  labs(x = "State Name", y = "Dogs to Cats Ratio", title = "Top 10 Dogs Loving States", caption = "Data Source: Washington Post via data.world for #TidyTuesday")

## Plotting Cats to Dogs Ratio
data %>%
  left_join(states, by = "state_name") %>%
  ggplot(aes(long, lat, group = group, fill = CatstoDogsRation)) +
  geom_polygon(color = "#ffffff", size = .25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  labs(title = "Cats to Dogs Ratio", subtitle = "Northern Eastern States Love Cats more than Dogs", caption = "Data Source: Washington Post via data.world for #TidyTuesday") +
  theme(panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_blank(),
        legend.title = element_blank()) +
  scale_fill_viridis()


## Plotting Top 10 Cat loving states
data %>%
  group_by(state_name) %>%
  arrange(desc(CatstoDogsRation)) %>%
  head(10) %>%
  ggplot(aes(x = reorder(state_name, -CatstoDogsRation), y = CatstoDogsRation, fill = CatstoDogsRation)) +
  geom_bar(stat = "identity") +
  theme(panel.grid = element_blank(),
        panel.background = element_blank(),
        legend.position = "none") +
  scale_fill_viridis_c() +
  labs(x = "State Name", y = "Cats to Dogs Ratio", title = "Top 10 Cats Loving States", caption = "Data Source: Washington Post via data.world for #TidyTuesday")

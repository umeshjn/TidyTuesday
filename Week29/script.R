library(tidyverse)
library(data.table)
library(ggthemes)

data <- fread("recent-grads.csv")

## Popular Major among women
data %>%
  group_by(Major_category) %>%
  summarise(WomenLoves = round(median(ShareWomen, na.rm = T), 2) * 100) %>%
  ggplot(aes(
    x = reorder(Major_category, WomenLoves),
    y = WomenLoves,
    fill = WomenLoves
  )) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = WomenLoves),
    hjust = 1.6,
    color = "white",
    size = 3
  ) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    y = "Median Percent of Women Graduates",
    x = "Major Category",
    title = "Psychology & Social Work is the Most Popular Major among Women",
    caption = "Source : Tidy Tuesday Week 29"
  ) +
  scale_fill_viridis_c(option = "E", name = "Median Women Share")



## Popular Major 
data %>%
  group_by(Major_category) %>%
  summarise(Total = sum(Total, na.rm = T)) %>%
  ggplot(aes(
    x = reorder(Major_category, Total),
    y = Total,
    fill = Total
  )) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(label = Total),
    hjust = 1.1,
    color = "white",
    size = 3
  ) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(
    y = "Total Number of Graduates",
    x = "Major Category",
    title = "Business Major has highest number of graduates",
    caption = "Source : Tidy Tuesday Week 29"
  ) +
  scale_fill_viridis_c(option = "E", name = "Total Number of Graduates")



## Major with most employed
data %>%
  group_by(Major_category) %>%
  summarise(Total = sum(Total, na.rm = T), Employed = sum(Employed, na.rm = T)) %>%
  mutate(Percentage = round(Employed/Total * 100, 2)) %>%
  ggplot(aes(
    x = reorder(Major_category, Percentage),
    y = Percentage,
    fill = Percentage
  )) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = Percentage),
            hjust = 1.6,
            color = "white",
            size = 3) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank()
  ) +
  labs(y = "Percentage of Employed",
       x = "Major Category",
       title = "Major Categories & Employment rates",
       caption = "Source : Tidy Tuesday Week 29") +
  scale_fill_viridis_c(option = "E")
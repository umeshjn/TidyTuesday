library(tidyverse)
library(dplyr)
library(highcharter)
library(lubridate)
library(ggthemes)
library(ggtech)
library(scales)

## Loading the data
data <- read_csv("us_births_2000-2014.csv")

## Creating the date column based on the other fields
data$BirthDate <- paste(data$year, data$month, data$date_of_month, sep = "-")

## Converting the values into factors for plotting purpose
data$year <- as.factor(data$year)
data$month <- as.character(month(data$BirthDate, label = TRUE))
data$weekday <- as.character(wday(data$BirthDate, label = TRUE))
data$date_of_month <- as.factor(data$date_of_month)


## Finding the total births by year and month
year_month <- data %>% 
              group_by(year, month) %>%
              summarise(Total = sum(births))

## Ordering the month factors
year_month$month <- factor(year_month$month, levels = month.abb)

## Finding the percentage by births in every month by year
year_month_percentage <- year_month %>% group_by(year) %>% 
  summarise(Yearly = sum(Total)) %>% 
  left_join(year_month, by = "year") %>% 
  mutate(Percentage = Total/Yearly * 100)

## Creating heatmap using ggplot
ggplot(year_month_percentage, aes(x=month, y = year, fill = round(Percentage, 2))) +
geom_tile(color = "black")+ 
  geom_text(aes(label= paste(round(Percentage, 2), "%")), color='white') +
  theme_minimal(base_size = 10, base_family = "") +
  labs(title = 'Peak Summer months are slightly more popular among babies', 
       subtitle = "Feb having the least is expected as it always has less number of days compared to other months",
       x = "Weekday",
       y = "Year of Birth") +
  scale_fill_viridis_c(direction = -1, option = "A", labels = comma,  name = "Percentage of Births") +
  theme_fivethirtyeight()


## Finding the total births by year and weekday
year_wday <- data %>% 
  group_by(year, weekday) %>%
  summarise(Total = sum(births))


## Ordering the weekday factors
year_wday$weekday <- ordered(year_wday$weekday, 
                             levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))

## Finding the percentage by births in every weekday by year
year_wday_percentage <- year_wday %>% group_by(year) %>% 
                        summarise(Yearly = sum(Total)) %>% 
                        left_join(year_wday, by = "year") %>% 
                        mutate(Percentage = Total/Yearly * 100)


## Creating the heatmap
ggplot(year_wday_percentage, aes(x = weekday, y = year, fill = round(Percentage,2))) +
  geom_tile(color="black") +
  geom_text(aes(label = paste(round(Percentage,2), "%")), color = "white") +
  theme_minimal(base_size = 10, base_family = "") +
  labs(title = 'Fewer Babies are born during the weekends', 
       subtitle = "Hospitals are understaffed during the weekends.Tuesday is the most favorite day",
       x = "Weekday",
       y = "Year of Birth") +
  scale_fill_viridis_c(direction = -1, option = "A", labels = comma,  name = "Percentage of Births") +
  theme_fivethirtyeight()


## Finding the total births by month and date of month
month_date <- data %>% 
  group_by(month, date_of_month) %>%
  summarise(Total = sum(births))


## Ordering the month factors
month_date$month <- factor(month_date$month, levels = month.abb)


## Finding the percentage by births in every day by month
month_date_percentage <- month_date %>% group_by(month) %>% 
  summarise(Daily = sum(Total)) %>% 
  left_join(month_date, by = "month") %>% 
  mutate(Percentage = Total/Daily * 100)


## Creating the heatmap
ggplot(month_date_percentage, aes(x = date_of_month, y = month, fill = round(Percentage,2))) +
  geom_tile(color="black") +
  geom_text(aes(label = paste(round(Percentage,1), "%")), color = "white", size = 3) +
  theme_minimal(base_size = 10, base_family = "") +
  labs(title = 'Babies dont like Christmas, New Year and Independence day', 
       subtitle = "Or should we say people are too busy on these days for babies") +
  scale_fill_viridis_c(direction = -1, option = "A", labels = comma,  name = "Percentage of Births") +
  theme_fivethirtyeight()


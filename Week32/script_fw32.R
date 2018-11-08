library(tidyverse)
library(highcharter)
library(viridis)

data <- read_csv("us_wind.csv")
states <- data %>% group_by(t_state) %>% summarise(Total = n()) 

## States with most number of Wind mills
hcmap("countries/us/us-all", data = states, 
      value = "Total", 
      joinBy = c("hc-a2", "t_state"),
      dataLabels = list(enabled = TRUE, format = '{point.name}'),
      borderColor = "#FAFAFA", borderWidth = 0.1) %>%
      hc_colorAxis(stops = color_stops(10, rev(cividis(10)))) %>%
      hc_title(text = "Texas has the Largest Number of Wind Turbines")  %>% 
      hc_credits(
         enabled = TRUE, text = "TidyTuesday - Week32",
         style = list(fontSize = "12px")
      ) %>%
      hc_subtitle(text = "Texas produces more wind power alone than 25 U.S. states produce from all power sources combined!") %>% 
  hc_add_theme(hc_theme_flatdark())


## Getting the County codes from unemployment dataset
countycodes <- separate(unemployment, col = "name", into = c("t_county", "State"), sep = ", ") %>% 
                select(code, t_county)

## Data to Map the number of wind turbines by Counties of Texas
texas <- texas %>% 
          left_join(countycodes, by = "t_county")

texas_counties <- texas %>%
                    group_by(code) %>%
                    summarise(Total = n())


## Creating Map of number of wind turbines by Counties of Texas
hcmap("countries/us/us-tx-all", data = texas_counties,
      name = "County", value = "Total", joinBy = c("hc-key", "code"),
      borderColor = "black", borderWidth = 0.1) %>%
  hc_colorAxis(stops = color_stops(10, rev(cividis(10)))) %>%
  hc_title(text = "Nolan County in Texas has Largest Number of Wind Turbines")  %>% 
  hc_credits(
    enabled = TRUE, text = "TidyTuesday - Week32",
    style = list(fontSize = "12px")
  ) %>%
  hc_subtitle(text = "Texas produces more wind power alone than 25 U.S. states produce from all power sources combined!") %>% 
  hc_add_theme(hc_theme_flatdark())

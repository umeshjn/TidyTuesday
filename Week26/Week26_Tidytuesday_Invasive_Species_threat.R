library(tidyverse)
library(highcharter)
library(countrycode)
library(viridis)


## Loading data table 1 of Overall invasion threats
data <- read_csv("table_1.csv")

## Getting the iso3c value for the countries
data$iso3 <- countrycode(data$country, 'country.name', 'iso3c')

## Changing the INvasion_threat to percentage
data$invasion_threat <- data$invasion_threat*100

## Creating the map for Overall Invasion Threat by %
data(worldgeojson, package = "highcharter")
highchart(type = "map") %>% 
  hc_add_series_map(map = worldgeojson, df = data, value = "invasion_threat", joinBy = "iso3")  %>%
  hc_colorAxis(stops = color_stops(n = 4, colors = c("#FDE725", "#21908C", "#440154"))) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Overall Invasion Threat by %") %>%
  hc_subtitle(text = "World's Agriculture Looks Very Threatened") %>%
  hc_credits(enabled = TRUE, text = "Sources: TidyTuesday Week 26", style = list(fontSize = "10px")) 



## Loading data table 2 of Total invasion costs
data2 <- read_csv("table_2.csv")

## Getting the iso3c value for the countries
data2$iso3 <- countrycode(data2$country, 'country.name', 'iso3c')

## Creating the map for Overall Invasion Threat by %
highchart(type = "map") %>% 
  hc_add_series_map(map = worldgeojson, df = data2, value = "invasion_cost", joinBy = "iso3")  %>%
  hc_colorAxis(stops = color_stops(10, rev(inferno(10)))) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Total Invasion Cost(US$)") %>%
  hc_subtitle(text = "China's cost is 27% of the Total cost paid by all other countries combined") %>%
  hc_credits(enabled = TRUE, text = "Sources: TidyTuesday Week 26", style = list(fontSize = "10px")) 


## Loading data table 3 of Total invasion costs as proportion of mean gdp
data3 <- read_csv("table_3.csv")

## Getting the iso3c value for the countries
data3$iso3 <- countrycode(data3$country, 'country.name', 'iso3c')

## Creating the map 
highchart(type = "map") %>% 
  hc_add_series_map(map = worldgeojson, df = data3, value = "gdp_proportion", joinBy = "iso3")  %>%
  hc_colorAxis(stops = color_stops(10, rev(cividis(10)))) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Total Invasion Cost as proportion of mean GDP") %>%
  hc_subtitle(text = "African Countries are Most Impacted") %>%
  hc_credits(enabled = TRUE, text = "Sources: TidyTuesday Week 26", style = list(fontSize = "10px")) 



## Loading data table 4 of Source Countries Total invasion costs
data4 <- read_csv("table_4.csv")

## Getting the iso3c value for the countries
data4$iso3 <- countrycode(data4$country, 'country.name', 'iso3c')

## Creating the map
highchart(type = "map") %>% 
  hc_add_series_map(map = worldgeojson, df = data4, value = "invasion_cost", joinBy = "iso3")  %>%
  hc_colorAxis(stops = color_stops(10, rev(magma(10)))) %>% 
  hc_legend(enabled = TRUE) %>% 
  hc_add_theme(hc_theme_google()) %>% 
  hc_mapNavigation(enabled = TRUE) %>%
  hc_title(text = "Source Countries Total Invasion Cost (US$)") %>%
  hc_subtitle(text = "China and USA are the Top 2 Countries") %>%
  hc_credits(enabled = TRUE, text = "Sources: TidyTuesday Week 26", style = list(fontSize = "10px")) 



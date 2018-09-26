library(tidyverse)
library(highcharter)
library(countrycode)
library(viridis)
library(geojsonio)
library(geojsonR)
library(jsonlite)

africageojson <- fromJSON("africa.geo.json", simplifyVector = FALSE)
africageojson <- as.json(africageojson)
africageojson <- FROM_GeoJson(africageojson)

## Loading data table
data <- read_csv("africa_species.csv")

## Creating a table of species count by African countries
africa <- data %>% group_by(country) %>% summarise(Total = n())


## Getting the iso3c value for the countries
africa$iso3 <- countrycode(africa$country, 'country.name', 'iso3c')


## Creating the map for Overall Invasion Threat by %
highchart(type = "map") %>% 
  hc_add_series_map(map = africageojson, df = africa, value = "Total", joinBy = c("iso-a3", "iso3"))  

p <- hcmap("custom/africa", data = africa, value = "Total",
      joinBy = c("iso-a3", "iso3"), name = "Number of Invasive Species",
      borderColor = "#FAFAFA", borderWidth = 1) %>%
  hc_add_theme(hc_theme_google()) %>% 
  hc_legend(layout = "vertical", align = "right",
            floating = TRUE, valueDecimals = 0, valueSuffix = "%") %>%
  hc_title(text = " Invasive Species in Africa") %>%
  hc_subtitle(text = "South Africa is the most impacted country") %>% 
  hc_credits(enabled = TRUE, text = "Sources: TidyTuesday Week 26", style = list(fontSize = "10px"))  

## Cividis color scheme
p %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = rev(cividis(10))))


## Plasma color scheme
p %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = rev(plasma(10))))


## Magma color scheme
p %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = rev(magma(10))))


## Inferno color scheme
p %>%
  hc_colorAxis(stops = color_stops(n = 10, colors = rev(inferno(10))))



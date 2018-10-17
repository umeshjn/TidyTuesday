library(tidyverse)
library(tidyverse)
library(viridis)
library(urbnmapr)
library(gridExtra)
library(scales)
library(magick)

data <- read_csv("voter_turnout.csv")
data[is.na(data)] <- ""
data$votes <- as.numeric(data$votes)
data$percentageofvoterturnout <-
  round(data$votes / data$eligible_voters * 100)
names(data)[5] <- "state_name"
data[is.na(data)] <- ""
data$percentageofvoterturnout <-
  as.numeric(data$percentageofvoterturnout)
data$ElectionType <-
  ifelse(data$year %in% seq(1980, 2012, 4), "Mid-Term", "President")
data$Turnout <- cut(
  data$percentageofvoterturnout,
  5,
  labels = c("Below 35%", "35-46%", "47-57%", "58-68%", "68-79%")
)



## Geneating the maps for different elections and saving the image as png
for (y in seq(1980, 2012, 4)) {
  print(y)
  p <- data %>%
    filter(year == y) %>%
    group_by(state_name) %>%
    summarise(Total = sum(percentageofvoterturnout)) %>%
    left_join(states, by = "state_name")  %>%
    ggplot(aes(long, lat, group = group, fill = Total)) +
    geom_polygon(color = "black", size = .25) +
    coord_map(projection = "albers",
              lat0 = 39,
              lat1 = 45) +
    labs(
      subtitle = paste("MidTerm Election", y , sep = " "),
      title = "US Voter TurnOut",
      caption = "Data Source:  data.world - Created for #TidyTuesday"
    ) +
    theme(
      panel.grid = element_blank(),
      axis.title = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.background = element_blank(),
      legend.title = element_blank()
    ) +
    scale_fill_viridis_c(breaks = seq(0, 100, 10), limit = c(0, 100))
  #    scale_fill_viridis_c(direction = -1, comma = TRUE, )
  
  ggsave(
    paste("MidTermElections", y, ".png", sep = ""),
    plot = p,
    width = 6,
    height = 5
  )
}


## Creating the gif using the pngs created

# Create a list of the png file names in the us_maps folder
the_list <- paste0("midterm/", list.files("midterm/"))

# apply the image_read function to each of the files and store it as a list in frames
frames <- lapply(the_list, image_read)

# use the image_animate function, which creates a GIF out of the list of images
animation <- image_animate(image_join(frames), fps = 1)

# Save the image as us_map.gif
image_write(animation, "midterm.gif")

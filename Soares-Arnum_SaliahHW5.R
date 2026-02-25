setwd("/Users/saliahsoares/Desktop")

library(readr)
library(tidyverse)
install.packages("maps")
library(maps)
library(tools)

Obesity <- read.csv("obesity_prevalence_2022.csv")
Obesity_cont <- map_data("state") %>%
  filter(!region %in% c("alaska", "hawaii", "Guam", "District of Columbia", "Virgin Islands", "Puerto Rico"))
install.packages("mapproj")
library(mapproj)

m1 <- ggplot(data=Obesity_cont, aes(x=long, y=lat, group=region)) +
  geom_polygon(fill="white" , color="black" , linewidth=0.1) +
  theme_void()
m2 <- m1 +
  coord_map(projection = "albers", lat0=39, lat1=45)
m2
Obesity$region <- tolower(Obesity$State) 
map_full <- left_join(Obesity_cont, Obesity, by = "region")

map_full$category <- cut(map_full$Prevalence,
                         breaks = c(0,20,25,30,35,40,45,100),
                         labels = c("<20%",
                                    "20%-<25%",
                                    "25%-<30%",
                                    "30%-<35%",
                                    "35%-<40%",
                                    "40%-<45%",
                                    "45%+"),
                         right = FALSE)
m3 <- ggplot(map_full, aes(x = long, y = lat, group = group, fill = category)) +
  geom_polygon(color = "black", linewidth = 0.1) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45) +
  scale_fill_manual(values = c(
    "<20%" = "#4CAF50",
    "20%-<25%" = "#8BC34A",
    "25%-<30%" = "#FFEB3B",
    "30%-<35%" = "#FF9800",
    "35%-<40%" = "#F44336",
    "40%-<45%" = "#B71C1C",
    "45%+" = "#7F0000"
  ), name = "Prevalence") +
  theme_void()

m3

ggsave("Obesity_Thematic_Map_2022.png",
       plot = m3,
       width = 10,
       height = 6,
       dpi = 300)


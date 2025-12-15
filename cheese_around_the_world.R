library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(sf)
library(knitr)
library(rnaturalearth)
library(tigris)
library(ggthemes)
library(cartogram)
library(scales)
library(viridis)
library(htmltools)
#format the data. rename countries so cheese and rnaturalearth names align
cheeses <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2024/2024-06-04/cheeses.csv')
cheese_data <- cheeses %>%
  separate_rows(country, sep = ", ") %>%
  separate_rows(country, sep = " and ")
rename_countries <- cheese_data %>%
  group_by(country) %>%
  summarise(cheese_types = n_distinct(cheese)) %>%
  mutate(country = ifelse(country == "United States", "United States of America", country)) %>%
  mutate(country = ifelse(country == "England", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Great Britain", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Wales", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Scotland", "United Kingdom", country)) %>%
  mutate(country = ifelse(country == "Czech Republic", "Czechia", country)) %>%
  mutate(country = ifelse(country == "Holland", "Netherlands", country)) %>%
  mutate(country = ifelse(country == "Macedonia", "North Macedonia", country)) %>%
  mutate(country = ifelse(country == "Serbia", "Republic of Serbia", country)) %>%
  mutate(country = ifelse(country == "Tibet", "China", country)) %>%
  rename(sovereignt = country) %>%
  filter(!is.na(sovereignt))

#load the country borders
borders <- ne_countries(scale = "small",
                        returnclass = "sf")
#clean country borders
sf_use_s2(FALSE)  #chat told me to turn this off

borders_clean <- borders %>%  #make valid borders
  st_make_valid() %>%
  st_buffer(dist = 0)

cat("Cleaned borders valid:", all(st_is_valid(borders_clean)), "\n") #check to see worked, TRUE

sf_use_s2(TRUE) #turn the thing back on


#now that countries are aligned, join borders with cheese data to create cheese borders
cheese_borders <- left_join(borders_clean, rename_countries, by = "sovereignt")
cheese_borders <- cheese_borders %>%
  mutate(cheese_types = replace_na(cheese_types, 0)) %>%
  st_transform(crs = 4326)



#adding in leaflet

pal <- colorBin(
  palette = c("white", plasma(13)), 
  domain = cheese_borders$cheese_types,
  bins = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 320)
)

cheese_map <- leaflet(data = cheese_borders) %>%
  addPolygons(
    fillColor = ~pal(cheese_types),
    fillOpacity = 0.7,
    color = "black",
    weight = 0.5,
    label = ~lapply(paste0(sovereignt, "<br/>Cheese Types: ", cheese_types), HTML)) %>%
  addLegend(
    colors = c("white", plasma(13)),
    position = "bottomright",
    values = cheese_borders$cheese_types,
    title = "World Cheese Production<br/> # Varieties",
    labels = c(
      "No Data Collected", "1-3", "4-6", "7-10", "11-15", "16-25", "26-40", "41-60", "61-90", "91-150",
      "151-200", "201-250", "251-300", "<301"
    ),
    opacity = 0.7
  ) %>%
  print()

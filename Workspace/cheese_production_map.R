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

#format the data. rename countries so cheese and rnaturalearth country names match
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
  rename(sovereignt = country)

#load the country borders
borders <- ne_countries(scale = "small",
                        returnclass = "sf")


#now that countries are aligned, join borders with cheese data to create cheese borders
cheese_borders <- left_join(borders, rename_countries, by = "sovereignt")
cheese_borders <- cheese_borders %>%
  mutate(cheese_types = replace_na(cheese_types, 0))

#adding in leaflet
cheese_map <- leaflet(data = cheese_borders) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorBin("viridis", cheese_types, bins = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 320))(cheese_types),
    fillOpacity = 0.7,
    color = "gray70",
    weight = 1,
    label = ~paste0(sovereignt, " ", "Cheese Types: ", cheese_types, "Most Common Milk Origin")
  ) %>%
  addLegend(
    pal = colorBin("viridis", domain = cheese_borders$cheese_types, bins = c(0, 1, 4, 7, 11, 16, 26, 41, 61, 91, 151, 201, 251, 301)),
    values = ~cheese_types,
    title = "Different Types of Cheese Reported",
    labels = c(
      "No Data Collected", "1-3", "4-6", "7-10", "11-15", "16-25", "26-40", "41-60", "61-90", "91-150",
      "151-200", "201-250", "251-300", "<301"
    ),
    opacity = 0.7
  )

# Print the map
cheese_map

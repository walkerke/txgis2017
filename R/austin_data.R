library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
options(tigris_class = "sf")

x <- load_variables(2015, "acs5/profile", cache = TRUE)

# View(x)


tx <- get_acs(geography = "tract", variables = "DP05_0009P", state = "TX", geometry = TRUE)

austin <- core_based_statistical_areas(cb = TRUE) %>%
  filter(GEOID == "12420")

austin_tracts <- tx[austin, op = st_within]

st_write(austin_tracts, "data/austin.shp")
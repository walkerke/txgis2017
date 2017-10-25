library(tidycensus)
library(tidyverse)
library(tigris)
library(sf)
library(stringr)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")
# We need: 2015 metro feature geometry; 2000 and 2011-2015 
# metro county data on # of residents with a bachelor's degree or higher

cb <- core_based_statistical_areas(cb = TRUE) %>%
  filter(LSAD == "M1") %>%
  select(metro_name = NAME) %>%
  st_transform(102008)

# 2000 data
x00 <- load_variables(2000, "sf3", cache = TRUE)

# We need 15-18, 32-35
educ00 <- get_decennial(geography = "county", table = "P037", year = 2000, 
                        sumfile = "sf3", summary_var = "P037001", geometry = TRUE)

ba00 <- educ00 %>%
  mutate(variable = str_replace(variable, "P0370", "")) %>%
  filter(variable %in% as.character(c(15:18, 32:35))) %>%
  group_by(GEOID) %>%
  mutate(ba = sum(value)) %>%
  ungroup() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  st_transform(102008) %>%
  select(GEOID, ba, pop25 = summary_value)


x15 <- load_variables(2015, "acs5", cache = TRUE)

# To keep: 22-25

vars <- c("B15003_022", "B15003_023", "B15003_024", "B15003_025")

ba15 <- get_acs(geography = "county", variables = vars, 
                summary_var = "B15003_001", geometry = TRUE) %>%
  group_by(GEOID) %>%
  mutate(ba = sum(estimate)) %>%
  ungroup() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  st_transform(102008) %>%
  select(GEOID, ba, pop25 = summary_est)

# Do the st_join thing
metro00 <- ba00 %>%
  st_point_on_surface(.) %>%
  st_join(cb, join = st_covered_by, left = FALSE) %>%
  as.data.frame() %>%
  group_by(metro_name) %>%
  summarize(ba = sum(ba), pop25 = sum(pop25)) %>%
  ungroup() %>%
  mutate(pctba00 = 100 * (ba / pop25)) %>%
  select(metro_name, pctba00)


metro15 <- ba15 %>%
  st_point_on_surface(.) %>%
  st_join(cb, join = st_covered_by, left = FALSE) %>%
  as.data.frame() %>%
  group_by(metro_name) %>%
  summarize(ba = sum(ba), pop25 = sum(pop25)) %>%
  ungroup() %>%
  mutate(pctba15 = 100 * (ba / pop25)) %>%
  select(metro_name, pctba15)

metrocomp <- left_join(metro00, metro15, by = "metro_name") %>%
  mutate(change = pctba15 - pctba00)

write_rds(metrocomp, "data/metrocomp.rds")
  


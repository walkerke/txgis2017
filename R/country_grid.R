library(jsonlite)
library(tidyverse)
library(countrycode)
library(idbr)

df <- fromJSON("https://raw.githubusercontent.com/mustafasaifee42/Tile-Grid-Map/master/Tile-Grid-Map-Cleaned.json") 

g <- rep(c("col", "row"), 192)

df2 <- df %>%
  unnest() %>%
  select(iso2 = `alpha-2`, coordinates)

df2$grid <- g

df2 <- df2 %>%
  spread(key = grid, value = coordinates) %>%
  mutate(code = countrycode(iso2, "iso2c", "fips104")) %>%
  rename(name = iso2)

df2 <- na.omit(df2)
# Slow - has to make lots of calls to the API and stitch them together.  
# Grab a cup of coffee while you wait.  
lex <- idb5(country = "all", year = seq(1990, 2020, 5), 
            variables = c("E0_F", "E0_M"), country_name = TRUE)

write_rds(lex, "data/lexall.rds")

lex2 <- lex %>%
  select(-NAME) %>%
  gather(key = sex, value = lex, -FIPS, -time) %>%
  mutate(sex = ifelse(sex == "E0_M", "Male", "Female")) %>%
  rename(code = FIPS)

library(geofacet)

ggplot(lex2, aes(x = time, y = lex, color = sex)) + 
  geom_line(size = 1) + 
  theme_minimal(base_size = 14) + 
  scale_color_manual(values = c('darkred', 'navy')) + 
  labs(title = "Life expectancy at birth", 
       caption = "Data source: US Census Bureau IDB via the idbr R package", 
       x = "Year", 
       y = "Life expectancy at birth", 
       color = "") + 
  facet_geo(~ code, grid = df2, move_axes = TRUE)



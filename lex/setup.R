library(idbr)
library(tidyverse)
library(plotly)

ssr_df <- idb5(c('Russia', 'Ukraine', 'Belarus', 'Moldova', 'Georgia', 'Kazakhstan', 
                 'Uzbekistan', 'Lithuania', 'Latvia', 'Estonia', 'Kyrgyzstan', 
                 'Tajikistan', 'Turkmenistan', 'Armenia', 'Azerbaijan'), 1989:2017, 
               variables = c("E0_M", "E0_F"), country_name = TRUE)

ssr2 <- ssr_df %>%
  select(-FIPS) %>%
  gather(key = sex, value = lex, -NAME, -time) %>%
  mutate(sex = ifelse(sex == "E0_M", "Male", "Female"))

write_rds(ssr2, "lex/lex.rds")



library(tigris)
library(tidyverse)
library(sf)
options(tigris_use_cache = TRUE)
options(tigris_class = "sf")

cty <- counties(cb = TRUE) %>%
  select(cty_id = GEOID, cty_name = NAME)

zc <- zctas(cb = TRUE)

zipcty <- st_join(zc, cty)
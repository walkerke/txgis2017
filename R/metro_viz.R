library(tigris)
library(tidyverse)
library(tmap)
library(sf)
library(viridis)
library(grid)
library(extrafont)
options(tigris_class = "sf")
options(tigris_use_cache = TRUE)

metrocomp <- read_rds("data/metrocomp.rds")

# Map it

cb <- core_based_statistical_areas(cb = TRUE, resolution = "20m") %>%
  select(metro_name = NAME)

mgeo <- inner_join(cb, metrocomp, by = "metro_name") %>%
  separate(metro_name, into = c("metro", "state"), sep = ",") %>%
  filter(!grepl("AK|HI|PR", state)) %>%
  st_transform(102008)

st1 <- states(cb = TRUE, resolution = "20m") %>%
  filter(!grepl("AK|HI|PR", STUSPS)) %>%
  st_transform(102008)

m1 <- tm_shape(st1) + 
  tm_fill("white") + 
  tm_shape(mgeo) + 
  tm_fill("change", n = 7, style = "quantile", 
          palette = viridis(7), title = "Change in % of pop.\nage 25+ with a BA or \nhigher, 2000-2015") + 
  tm_layout(legend.outside = TRUE, bg.color = "grey90", 
            inner.margins = c(0.1, 0.1, 0.05, 0.05), fontfamily = "Tahoma") + 
  tm_credits("Data source: 2000 Census, 2011-2015 ACS via the tidycensus R package", 
             position = c("RIGHT", "BOTTOM")) + 
  tm_shape(st1) + 
  tm_borders()


ak <- inner_join(cb, metrocomp, by = "metro_name") %>%
  separate(metro_name, into = c("metro", "state"), sep = ",") %>%
  filter(grepl("AK", state)) %>%
  st_transform(3338)

akst <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS == "AK") %>%
  st_transform(3338)

akmap <- tm_shape(akst) + 
  tm_fill("white") + 
  tm_shape(ak) + 
  tm_fill("change", breaks = c(0.44, 2.71, 3.47, 4.14, 4.66, 5.46, 6.23, 15.77), 
          palette = viridis(7)) + 
  tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE) + 
  tm_shape(akst) + 
  tm_borders()


hi <- inner_join(cb, metrocomp, by = "metro_name") %>%
  separate(metro_name, into = c("metro", "state"), sep = ",") %>%
  filter(grepl("HI", state)) %>%
  st_transform(3759)

hist <- states(cb = TRUE, resolution = "20m") %>%
  filter(STUSPS == "HI") %>%
  st_transform(3759)

himap <- tm_shape(hist) + 
  tm_fill("white") + 
  tm_shape(hi) + 
  tm_fill("change", breaks = c(0.44, 2.71, 3.47, 4.14, 4.66, 5.46, 6.23, 15.77), 
          palette = viridis(7)) + 
  tm_layout(legend.show = FALSE, bg.color = NA, frame = FALSE) + 
  tm_shape(hist) + 
  tm_borders()

vp_AK <- viewport(x = 0.12, y = 0.25, width = 0.2, height = 0.2)
vp_HI <- viewport(x = 0.25, y = 0.25, width = 0.2, height = 0.1)

m1
print(akmap, vp = vp_AK)
print(himap, vp = vp_HI)

# save_tmap(m1, "data/change.png", scale = 0.7, width = 6.125, 
#           insets_tm = list(m_AK, m_HI), 
#           insets_vp = list(vp_AK, vp_HI))


# Bar chart for Texas

tx <- metrocomp %>%
  separate(metro_name, into = c("metro", "state"), sep = ",") %>%
  mutate(state = str_trim(state)) %>%
  filter(state == "TX")

ggplot(tx, aes(y = change, x = reorder(metro, change), 
               fill = change)) +
  geom_bar(stat = "identity") + 
  coord_flip() + 
  theme_minimal(base_family = "Tahoma") + 
  scale_fill_viridis(guide = FALSE) + 
  labs(title = "Change in percentage of population with a 4-year degree",
       subtitle = "Metropolitan areas in Texas. Population age 25+, 2000-2015", 
       x = "", 
       y = "Percent change", 
       caption = "Data sources: 2000 Census, 2011-2015 ACS, tidycensus R package") 

ggsave("img/tx_bar.png", dpi = 300)


# Slopegraph
# metros <- "Dallas|Houston|San Antonio|Austin|El Paso|McAllen|Corpus Christi|Killeen|Brownsville|Beaumont|Lubbock|Laredo|Amarillo|Waco|College Station"

metros <- "Dallas|Houston|San Antonio|Austin|El Paso|McAllen|Corpus Christi"

tx2 <- tx %>%
  filter(grepl(metros, metro)) %>%
  select(metro, pctba00, pctba15) %>%
  gather(key = year, value = percent, -metro) %>%
  mutate(year = str_replace(year, "pctba", "20"), 
         metro = str_replace(metro, "-.*", "")) %>%
  mutate(label00 = paste0(metro, " ", round(percent, 1), "%  "), 
         label15 = paste0("  ", round(percent, 1), "%"))

ggplot(tx2) + 
  geom_line(aes(x = year, y = percent, group = metro, color = metro), size = 1) + 
  geom_point(aes(x = year, y = percent, group = metro, color = metro), size = 3) + 
  geom_text(data = filter(tx2, year == 2000), 
            aes(x = year, y = percent, label = label00, hjust = 1, 
                family = "Tahoma")) + 
  geom_text(data = filter(tx2, year == 2015), 
            aes(x = year, y = percent, label = label15, hjust = 0, 
                family = "Tahoma")) + 
  theme_minimal(base_family = "Tahoma") + 
  scale_color_brewer(palette = "Paired", guide = FALSE) + 
  labs(x = "",
       title = "Percent of population age 25+ with a 4-year degree",
       subtitle = "Largest metro areas in Texas",
       caption = "Data sources: US Census Bureau, tidycensus R package") +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank())

ggsave("img/slope1.png", dpi = 300, height = 7.36, width = 7.26)


# Focus on particular metros

focus <- "Abilene|Midland"

tx3 <- tx %>%
  select(metro, pctba00, pctba15) %>%
  gather(key = year, value = percent, -metro) %>%
  mutate(year = str_replace(year, "pctba", "20"), 
         metro = str_replace(metro, "-.*", "")) 

txfocus <- tx3 %>%
  filter(grepl(focus, metro)) %>%
  mutate(label00 = paste0(metro, " ", round(percent, 1), "%  "), 
         label15 = paste0("  ", round(percent, 1), "%"))

txother <- tx3 %>%
  filter(!grepl(focus, metro))

ggplot() + 
  geom_line(data = txother, aes(x = year, y = percent, group = metro), color = "grey70") + 
  geom_point(data = txother, aes(x = year, y = percent, group = metro), color = "grey70") + 
  geom_line(data = txfocus, aes(x = year, y = percent, group = metro), color = "darkred", size = 1.5) + 
  geom_point(data = txfocus, aes(x = year, y = percent, group = metro), color = "darkred", size = 3) + 
  geom_text(data = filter(txfocus, year == 2000), 
            aes(x = year, y = percent, label = label00, hjust = 1, 
                family = "Tahoma"), color = "darkred") + 
  geom_text(data = filter(txfocus, year == 2015), 
            aes(x = year, y = percent, label = label15, hjust = 0, 
                family = "Tahoma"), color = "darkred") + 
  theme_minimal(base_family = "Tahoma") + 
  scale_color_brewer(palette = "Paired", guide = FALSE) + 
  labs(x = "",
       title = "Educational attainment in Abilene and Midland",
       subtitle = "Comparative context: large metropolitan areas in Texas",
       caption = "Data sources: US Census Bureau, tidycensus R package") +
  theme(legend.position = "none", 
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.ticks.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank())

ggsave("img/slope2.png", dpi = 300)
  
  
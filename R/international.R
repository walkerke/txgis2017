library(idbr)
library(tidyverse)
library(plotly)

# idb_api_key('your api key here')

ssr_df <- idb5(c("Russia", "Belarus", "Ukraine"), 1989:2017, 
               variables = "E0", country_name = TRUE)

ssr_df <- rename(ssr_df, LEX = E0, Year = time, Country = NAME)

p1 <- ggplot(ssr_df, aes(x = Year, y = LEX, color = Country)) + 
  geom_line(size = 1) + 
  theme_minimal(base_size = 14) + 
  scale_color_brewer(palette = "Set1") + 
  ylab('Life expectancy at birth') + 
  xlab('Year') + 
  theme(legend.title = element_blank(), 
        legend.position = "bottom") + 
  annotate('text', x = 2010, y = 64.5, 
           label = 'Data source: US Census Bureau IDB \nvia the idbr R package', 
           size = 3.5)

ggsave("img/ussr.png", p1, dpi = 300, width = 8, height = 6.5)

g1 <- ggplotly(p1)

htmlwidgets::saveWidget(g1, file = "ussr_py.html")
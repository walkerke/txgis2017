library(tidyverse)

df <- read_rds("lex/lex.rds")

ggplot(df, aes(x = time, y = lex, color = sex)) + 
  geom_line(size = 1) + 
  theme_minimal(base_size = 14) + 
  scale_color_manual(values = c('darkred', 'navy')) + 
  labs(title = paste0("Life expectancy at birth since the fall of the USSR"), 
       caption = "Data source: US Census Bureau IDB via the idbr R package", 
       x = "Year", 
       y = "Life expectancy at birth", 
       color = "") + 
  facet_wrap(~ NAME)

ggsave("img/sm.png", dpi = 300, height = 7.26, width = 9.04)
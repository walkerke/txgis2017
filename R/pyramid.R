library(tidyverse)
library(geofacet)
library(countrycode)
library(idbr)

grid1 <- read_csv("https://gist.githubusercontent.com/maartenzam/787498bbc07ae06b637447dbd430ea0a/raw/9a9dafafb44d8990f85243a9c7ca349acd3a0d07/worldtilegrid.csv") %>%
  select(name, code = alpha.2, col = x, row = y) %>%
  mutate(code = countrycode(code, "iso2c", "fips104")) %>%
  na.omit()

grid_preview(grid1)

valid_countries <- c("AA", "AC", "AE", "AF", "AG", "AJ", "AL", "AM", "AN", "AO",
                     "AQ", "AR", "AS", "AU", "AV", "BA", "BB", "BC", "BD", "BE", "BF",
                     "BG", "BH", "BK", "BL", "BM", "BN", "BO", "BP", "BR", "BT", "BU",
                     "BX", "BY", "CA", "CB", "CD", "CE", "CF", "CG", "CH", "CI", "CJ",
                     "CM", "CN", "CO", "CQ", "CS", "CT", "CU", "CV", "CW", "CY", "DA",
                     "DJ", "DO", "DR", "EC", "EG", "EI", "EK", "EN", "ER", "ES", "ET",
                     "EZ", "FI", "FJ", "FM", "FO", "FP", "FR", "GA", "GB", "GG", "GH",
                     "GI", "GJ", "GK", "GL", "GM", "GQ", "GR", "GT", "GV", "GY", "GZ",
                     "HA", "HK", "HO", "HR", "HU", "IC", "ID", "IM", "IN", "IR", "IS",
                     "IT", "IV", "IZ", "JA", "JE", "JM", "JO", "KE", "KG", "KN", "KR",
                     "KS", "KU", "KV", "KZ", "LA", "LE", "LG", "LH", "LI", "LO", "LS",
                     "LT", "LU", "LY", "MA", "MC", "MD", "MG", "MH", "MI", "MJ", "MK",
                     "ML", "MN", "MO", "MP", "MR", "MT", "MU", "MV", "MX", "MY", "MZ",
                     "NC", "NG", "NH", "NI", "NL", "NN", "NO", "NP", "NR", "NS", "NU",
                     "NZ", "OD", "PA", "PE", "PK", "PL", "PM", "PO", "PP", "PS", "PU",
                     "QA", "RI", "RM", "RN", "RO", "RP", "RQ", "RS", "RW", "SA", "SB",
                     "SC", "SE", "SF", "SG", "SH", "SI", "SL", "SM", "SN", "SO", "SP",
                     "ST", "SU", "SW", "SY", "SZ", "TB", "TD", "TH", "TI", "TK", "TN",
                     "TO", "TP", "TS", "TT", "TU", "TV", "TW", "TX", "TZ", "UC", "UG",
                     "UK", "UP", "US", "UV", "UY", "UZ", "VC", "VE", "VI", "VM", "VQ",
                     "WA", "WE", "WF", "WI", "WS", "WZ", "YM", "ZA", "ZI")

df <- map_df(valid_countries, function(x) {
  
  male <- idb1(x, 2017, sex = "male") %>%
    mutate(POP = POP * -1, 
           SEX = "Male")
  
  female = idb1(x, 2017, sex = "female") %>%
    mutate(SEX = "Female")
  
  return(rbind(male, female))
  
})

df2 <- df %>%
  group_by(FIPS, SEX) %>%
  mutate(totalpop = abs(sum(POP))) %>%
  ungroup() %>%
  mutate(percent = 100 * (POP / totalpop)) %>%
  filter(NAME != "Montserrat") %>%
  rename(code = FIPS)
  

ggplot(df2, aes(x = AGE, y = percent, fill = SEX)) + 
  geom_bar(stat = "identity", width = 1) + 
  coord_flip() + 
  theme_minimal() + 
  facet_geo(~ code, grid = grid1)
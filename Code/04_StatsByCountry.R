#Author: Dabalà Alvise

#Analysis of the results at country scale and continental scale
#Plot of circular barplots and print of excel sheets with the results

#Open all the packages needed
library(tidyverse)
library(sf)
library(prioritizr)
library(patchwork)
library(viridis)
library(ggthemes)
library(openxlsx)
library(xlsx)

source("Functions/fStat_CountryContinent.r")
source("Functions/fStat_CountryContinent_PAs.r")
source("Functions/fPlot_Circular.r")

result_BioServ <- readRDS("RDS/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/result_BioServ_WDPA.rds")
result_Bio <- readRDS("RDS/result_Bio.rds")
result_Bio_WDPA <- readRDS("RDS/result_Bio_WDPA.rds")
PUs <- readRDS("RDS/PUs_Splitted.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
ConsFeatures <- readRDS("RDS/ConsFeatures.rds")
ConsFeatures_NotSplitted <- readRDS("RDS/ConsFeatures_NotSplitted.rds")
species <- readRDS("RDS/species.rds")
result_BioServ_WDPA_rmPA <- readRDS("RDS/result_BioServ_WDPA_rmPA.rds")

################################################################################

# Percentages of already protected priority areas
# Top ranked-10%
for (i in c(10, 30, 50)) {
  priority_protected <- result_BioServ %>%
  as_tibble() %>% 
  filter(rank <= i) %>% 
  filter(Protected == TRUE) %>% 
  summarise(priority_protected = sum(AreaGMWKm))

priority_notprotected <- result_BioServ %>%
  as_tibble() %>% 
  filter(rank <= i) %>% 
  filter(Protected == FALSE) %>%  
  summarise(priority_notprotected = sum(AreaGMWKm))

print(priority_protected/(priority_protected + priority_notprotected)*100)
}

# Statistics when optimising for biodiversity and ecosystem services
Stat_CountryContinent_10 <- fStat_CountryContinent(result_BioServ, 10)
Stat_CountryContinent_30 <- fStat_CountryContinent(result_BioServ, 30)
Stat_CountryContinent_50 <- fStat_CountryContinent(result_BioServ, 50)

Stat_CountryContinent <- c(Stat_CountryContinent_10, Stat_CountryContinent_30, Stat_CountryContinent_50
                           )

# Save in an excel file
list_sheets <- c("10_Country", "10_Continent", "30_Country", "30_Continent", "50_Country", "50_Continent")

file <- paste("Figures/", sep = "")

lapply(seq_along(Stat_CountryContinent), function(z) {
  write.xlsx(Stat_CountryContinent[[z]], paste0(file, "Stat_CountryContinent.xlsx"), sheetName = list_sheets[z], append = TRUE) 
}
)

# Statistics when building on already protected areas
Stat_CountryContinent_PAs <- fStat_CountryContinent_PAs(PUs_NotSplitted)
Stat_CountryContinent_30_WDPA <- fStat_CountryContinent(result_BioServ_WDPA_rmPA, 30)
Stat_CountryContinent_50_WDPA <- fStat_CountryContinent(result_BioServ_WDPA_rmPA, 50)

Stat_CountryContinent_WDPA <- c(Stat_CountryContinent_PAs, Stat_CountryContinent_30_WDPA, Stat_CountryContinent_50_WDPA)

# Save in an excel file
list_sheets <- c("WDPA_country", "WDPA_continent", "+16.8%_Country_WDPA", "+16.8%_Continent_WDPA", "+36.8%_Country_WDPA", "+36.8%_Continent_WDPA")

file <- paste("Figures/", sep = "")

lapply(seq_along(Stat_CountryContinent_WDPA), function(z) {
  write.xlsx(Stat_CountryContinent_WDPA[[z]], paste0(file, "Stat_CountryContinent_WDPA.xlsx"), sheetName = list_sheets[z], append = TRUE) 
}
)

# Number of countries with all the mangroves in priority areas protected
Stat_CountryContinent_30[[1]] %>% 
  filter(perc_priority_area_country == 100) %>% 
  nrow()

Stat_CountryContinent_50[[1]] %>% 
  filter(perc_priority_area_country == 100) %>% 
  nrow()

# Maximum services
PUs %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            Population = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm)) %>% 
  dplyr::select(c("Fishing", "Population", "Properties", "Carbon", "AreaGMWKm")) %>% 
  st_drop_geometry() %>% 
  as_tibble()

### Circular barplot
# Country → example at 50% target

result_BioServ <- result_BioServ %>% 
  mutate(country = recode(country,  
                          `Micronesia (Federated States of)` = "Micronesia",
                          `Papua New Guinea` = "PNG",
                          `Congo-Kinshasa` = "COD",
                          `Myanmar (Burma)` = "Myanmar",
                          `United Arab Emirates` = "UEA",
                          `Turks & Caicos Islands` = "TCI",
                          `United States` = "USA"))
  
# PUs selected at 50%
country_protection_10 <- result_BioServ %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 10) %>% 
  group_by(country) %>% 
  summarise(Fishing10 = sum(Fishing_Intensity*AreaGMWKm), 
            Population10 = sum(POP*AreaGMWKm),
            Properties10 = sum(TOT_STOCK*AreaGMWKm),
            Carbon10 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm10 = sum(AreaGMWKm))
#dplyr::select(country, AreaGMWKm10)

country_protection_30 <- result_BioServ %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 30) %>% 
  mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
  group_by(country) %>% 
  summarise(Fishing30 = sum(Fishing_Intensity*AreaGMWKm), 
            Population30 = sum(POP*AreaGMWKm),
            Properties30 = sum(TOT_STOCK*AreaGMWKm),
            Carbon30 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm30 = sum(AreaGMWKm))
#dplyr::select(country, AreaGMWKm30)

country_protection_50 <- result_BioServ %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 50) %>%  
  mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
  group_by(country) %>% 
  summarise(Fishing50 = sum(Fishing_Intensity*AreaGMWKm), 
            Population50 = sum(POP*AreaGMWKm),
            Properties50 = sum(TOT_STOCK*AreaGMWKm),
            Carbon50 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm50 = sum(AreaGMWKm))

country_protection_100 <- result_BioServ %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 100) %>%
  mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
  group_by(country) %>% 
  summarise(Fishing100 = sum(Fishing_Intensity*AreaGMWKm), 
            Population100 = sum(POP*AreaGMWKm),
            Properties100 = sum(TOT_STOCK*AreaGMWKm),
            Carbon100 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm100 = sum(AreaGMWKm),
            continent = first(continent))
#dplyr::select(country, AreaGMWKm100)

country_protection_100$AreaGMWKm100 <- 15000

colors <- c('30%' = 'blue', '50%' = 'orange')
legends <- c('30%', '50%', "70%")

df_circbp <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(AreaGMWKm30 = AreaGMWKm30 - AreaGMWKm10) %>% 
  mutate(AreaGMWKm50 = AreaGMWKm50 - AreaGMWKm30 - AreaGMWKm10) %>% 
  mutate(AreaGMWKm100 = AreaGMWKm100 - AreaGMWKm50 - AreaGMWKm30 - AreaGMWKm10) %>%
  dplyr::select(country, AreaGMWKm100, AreaGMWKm50, AreaGMWKm30, AreaGMWKm10, continent) %>% 
  group_by(continent) %>%
  top_n(10, AreaGMWKm10 + AreaGMWKm30 + AreaGMWKm50) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('AreaGMWKm100', 'AreaGMWKm50', 'AreaGMWKm30','AreaGMWKm10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "AreaGMWKm100" ~ "100%",
                            name == "AreaGMWKm50" ~ "Top-ranked 50%",
                            name == "AreaGMWKm30" ~ "Top-ranked 30%",
                            name == "AreaGMWKm10" ~ "Top-ranked 10%"))

df_circbp$name <- NULL

circ_area <- fPlot_Circular(df_circbp, colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"), 
               ext_val = 3750, lab = c("0", "3.75", "7.5", "11.25", "15"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Top-ranked 10%"))

#ggsave("Figures/CircularBarplot.svg", dpi = 1000, width = 15, height = 18, units = "cm", limitsize = FALSE)

################################################################################
#Circular barplot ecosystem services 
################################################################################

max(country_protection_50$Fishing50)

country_protection_100$Fishing100 <- 25000000

df_circbp_Fishing <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Fishing30 = Fishing30 - Fishing10) %>% 
  mutate(Fishing50 = Fishing50 - Fishing30 - Fishing10) %>% 
  mutate(Fishing100 = Fishing100 - Fishing50 - Fishing30 - Fishing10) %>%
  dplyr::select(country, Fishing100, Fishing50, Fishing30, Fishing10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Fishing10 + Fishing30 + Fishing50) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Fishing100', 'Fishing50', 'Fishing30','Fishing10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Fishing100" ~ "100%",
                            name == "Fishing50" ~ "Top-ranked 50%",
                            name == "Fishing30" ~ "Top-ranked 30%",
                            name == "Fishing10" ~ "Top-ranked 10%"))

df_circbp_Fishing$name <- NULL

circ_fish <- fPlot_Circular(df_circbp_Fishing, colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"),
               ext_val = 25000000/4, lab = c("0", "6.25", "12.5", "18.75", "25"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Top-ranked 10%"))

ggsave("Figures/CircularBarplot_Fishing.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

## Properties
max(country_protection_50$Properties50)

country_protection_100$Properties100 <- 8000000000

df_circbp_Properties <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Properties30 = Properties30 - Properties10) %>% 
  mutate(Properties50 = Properties50 - Properties30 - Properties10) %>% 
  mutate(Properties100 = Properties100 - Properties50 - Properties30 - Properties10) %>%
  dplyr::select(country, Properties100, Properties50, Properties30, Properties10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Properties10 + Properties30 + Properties50) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Properties100', 'Properties50', 'Properties30','Properties10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Properties100" ~ "100%",
                            name == "Properties50" ~ "Top-ranked 50%",
                            name == "Properties30" ~ "Top-ranked 30%",
                            name == "Properties10" ~ "Top-ranked 10%"))
df_circbp_Properties$name <- NULL

circ_prop <- fPlot_Circular(df_circbp_Properties, colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"),
               ext_val = 8000000000/4, lab = c("0", "2", "4", "6", "8"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Top-ranked 10%"))

ggsave("Figures/CircularBarplot_Properties.svg", dpi = 300, width = 13, height = 14, units = "cm", limitsize = FALSE)

## Population
max(country_protection_50$Population50)

country_protection_100$Population100 <- 4000000

df_circbp_Population <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Population30 = Population30 - Population10) %>% 
  mutate(Population50 = Population50 - Population30 - Population10) %>% 
  mutate(Population100 = Population100 - Population50 - Population30 - Population10) %>%
  dplyr::select(country, Population100, Population50, Population30, Population10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Population10 + Population30 + Population50) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Population100', 'Population50', 'Population30','Population10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Population100" ~ "100%",
                            name == "Population50" ~ "Top-ranked 50%",
                            name == "Population30" ~ "Top-ranked 30%",
                            name == "Population10" ~ "Top-ranked 10%"))

df_circbp_Population$name <- NULL

circ_pop <- fPlot_Circular(df_circbp_Population, colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"), 
               ext_val = 4000000/4, lab = c("0", "1", "2", "3", "4"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Top-ranked 10%"))

ggsave("Figures/CircularBarplot_Population.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

## Carbon
max(country_protection_50$Carbon50)

country_protection_100$Carbon100 <- 120000

df_circbp_Carbon <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Carbon30 = Carbon30 - Carbon10) %>% 
  mutate(Carbon50 = Carbon50 - Carbon30 - Carbon10) %>% 
  mutate(Carbon100 = Carbon100 - Carbon50 - Carbon30 - Carbon10) %>%
  dplyr::select(country, Carbon100, Carbon50, Carbon30, Carbon10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Carbon10 + Carbon30 + Carbon50) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Carbon100', 'Carbon50', 'Carbon30','Carbon10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Carbon100" ~ "100%",
                            name == "Carbon50" ~ "Top-ranked 50%",
                            name == "Carbon30" ~ "Top-ranked 30%",
                            name == "Carbon10" ~ "Top-ranked 10%"))

df_circbp_Carbon$name <- NULL

circ_carb <- fPlot_Circular(df_circbp_Carbon, colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"), 
               ext_val = 120000/4, lab = c("0", "3", "6", "9", "12"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Top-ranked 10%"))

ggsave("Figures/CircularBarplot_Carbon.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

plot <- circ_area + plot_spacer() + circ_prop + circ_pop + circ_carb + circ_fish +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag = element_text(face = 'bold'))

ggsave("Figures/CircularBarplot.svg", width = 17.0, height = 25.6, units = "cm")

################################################################################
# BUilding on already protected areas
################################################################################

result_BioServ_WDPA <- result_BioServ_WDPA %>% 
  mutate(country = recode(country,  
                          `Micronesia (Federated States of)` = "Micronesia",
                          `Papua New Guinea` = "PNG",
                          `Congo - Kinshasa` = "COD",
                          `Myanmar (Burma)` = "Myanmar",
                          `United Arab Emirates` = "UEA",
                          `Turks & Caicos Islands` = "TCI",
                          `United States` = "USA",
                          `Equatorial Guinea` = "GNQ"))

PUs <- PUs %>% 
  mutate(country = recode(country,  
                          `Micronesia (Federated States of)` = "Micronesia",
                          `Papua New Guinea` = "PNG",
                          `Congo - Kinshasa` = "COD",
                          `Myanmar (Burma)` = "Myanmar",
                          `United Arab Emirates` = "UEA",
                          `Turks & Caicos Islands` = "TCI",
                          `United States` = "USA",
                          `Equatorial Guinea` = "GNQ"))

# PUs selected at 50%
country_protection_PA <- result_BioServ_WDPA %>% 
  as_tibble %>% 
  filter(Protected == 1) %>%  
  group_by(country) %>% 
  summarise(Fishing10 = sum(Fishing_Intensity*AreaGMWKm), 
            Population10 = sum(POP*AreaGMWKm),
            Properties10 = sum(TOT_STOCK*AreaGMWKm),
            Carbon10 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm10 = sum(AreaGMWKm))

country_protection_30 <- result_BioServ_WDPA %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 30) %>% 
  #mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
  group_by(country) %>% 
  summarise(Fishing30 = sum(Fishing_Intensity*AreaGMWKm), 
            Population30 = sum(POP*AreaGMWKm),
            Properties30 = sum(TOT_STOCK*AreaGMWKm),
            Carbon30 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm30 = sum(AreaGMWKm))

country_protection_50 <- result_BioServ_WDPA %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 50) %>% 
  #mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
  group_by(country) %>% 
  summarise(Fishing50 = sum(Fishing_Intensity*AreaGMWKm), 
            Population50 = sum(POP*AreaGMWKm),
            Properties50 = sum(TOT_STOCK*AreaGMWKm),
            Carbon50 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm50 = sum(AreaGMWKm))

country_protection_100 <- result_BioServ_WDPA %>% 
  as_tibble %>% 
  dplyr::filter(rank <= 100) %>% 
  #mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
  group_by(country) %>% 
  summarise(Fishing100 = sum(Fishing_Intensity*AreaGMWKm), 
            Population100 = sum(POP*AreaGMWKm),
            Properties100 = sum(TOT_STOCK*AreaGMWKm),
            Carbon100 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm100 = sum(AreaGMWKm),
            continent = first(continent))

country_protection_100$AreaGMWKm100 <- 15000

df_circbp <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(AreaGMWKm30 = AreaGMWKm30 - AreaGMWKm10) %>% 
  mutate(AreaGMWKm50 = AreaGMWKm50 - AreaGMWKm30 - AreaGMWKm10) %>% 
  mutate(AreaGMWKm100 = AreaGMWKm100 - AreaGMWKm50 - AreaGMWKm30 - AreaGMWKm10) %>%
  dplyr::select(country, AreaGMWKm100, AreaGMWKm50, AreaGMWKm30, AreaGMWKm10, continent) %>% 
  group_by(continent) %>%
  top_n(10, AreaGMWKm10 + AreaGMWKm30 + AreaGMWKm50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('AreaGMWKm100', 'AreaGMWKm50', 'AreaGMWKm30','AreaGMWKm10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "AreaGMWKm100" ~ "100%",
                            name == "AreaGMWKm50" ~ "Top-ranked 50%",
                            name == "AreaGMWKm30" ~ "Top-ranked 30%",
                            name == "AreaGMWKm10" ~ "Already protected areas"))

df_circbp$name <- NULL

circ_area <- fPlot_Circular(df_circbp, colr = c("#e5e5e5", "#47B5FF", "#1363DF", "#06283D"), #colr = c("#e5e5e5", "#FEB078", "#B4367A", "#000000"), 
               ext_val = 3750, 
               lab = c("0", "3.75", "7.5", "11.25", "15"), 
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Already protected areas"))

#ggsave("Figures/CircularBarplot_WDPA.svg", dpi = 1000, width = 15, height = 18, units = "cm", limitsize = FALSE)

################################################################################
#Circular barplot ecosystem services
max(country_protection_50$Fishing50)

country_protection_100$Fishing100 <- 20000000

df_circbp_Fishing <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Fishing30 = Fishing30 - Fishing10) %>% 
  mutate(Fishing50 = Fishing50 - Fishing30 - Fishing10) %>% 
  mutate(Fishing100 = Fishing100 - Fishing50 - Fishing30 - Fishing10) %>%
  dplyr::select(country, Fishing100, Fishing50, Fishing30, Fishing10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Fishing10 + Fishing30 + Fishing50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Fishing100', 'Fishing50', 'Fishing30','Fishing10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Fishing100" ~ "100%",
                            name == "Fishing50" ~ "Top-ranked 50%",
                            name == "Fishing30" ~ "Top-ranked 30%",
                            name == "Fishing10" ~ "Already protected areas"))

df_circbp_Fishing$name <- NULL

circ_fish <- fPlot_Circular(df_circbp_Fishing, colr = c("#e5e5e5", "#47B5FF", "#1363DF", "#06283D"), 
               ext_val = 20000000/4, lab = c("0", "5", "10", "15", "20"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Already protected areas"))

#ggsave("Figures/CircularBarplot_Fishing_WDPA.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

## Properties
max(country_protection_50$Properties50)

country_protection_100$Properties100 <- 8000000000

df_circbp_Properties <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>%
  replace(is.na(.), 0) %>% 
  mutate(Properties30 = Properties30 - Properties10) %>% 
  mutate(Properties50 = Properties50 - Properties30 - Properties10) %>% 
  mutate(Properties100 = Properties100 - Properties50 - Properties30 - Properties10) %>%
  dplyr::select(country, Properties100, Properties50, Properties30, Properties10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Properties10 + Properties30 + Properties50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Properties100', 'Properties50', 'Properties30','Properties10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Properties100" ~ "100%",
                            name == "Properties50" ~ "Top-ranked 50%",
                            name == "Properties30" ~ "Top-ranked 30%",
                            name == "Properties10" ~ "Already protected areas"))

df_circbp_Properties$name <- NULL

circ_prop <- fPlot_Circular(df_circbp_Properties, colr = c("#e5e5e5", "#47B5FF", "#1363DF", "#06283D"), #colr = c("#e5e5e5", "#F4A4C1", "#E94984", "#6D0D30"),
               ext_val = 8000000000/4, lab = c("0", "2", "4", "6", "8"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Already protected areas"))

#ggsave("Figures/CircularBarplot_Properties_WDPA.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

## Population
max(country_protection_50$Population50)

country_protection_100$Population100 <- 4000000

df_circbp_Population <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Population30 = Population30 - Population10) %>% 
  mutate(Population50 = Population50 - Population30 - Population10) %>% 
  mutate(Population100 = Population100 - Population50 - Population30 - Population10) %>%
  dplyr::select(country, Population100, Population50, Population30, Population10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Population10 + Population30 + Population50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Population100', 'Population50', 'Population30','Population10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Population100" ~ "100%",
                            name == "Population50" ~ "Top-ranked 50%",
                            name == "Population30" ~ "Top-ranked 30%",
                            name == "Population10" ~ "Already protected areas"))

df_circbp_Population$name <- NULL

circ_pop <- fPlot_Circular(df_circbp_Population, colr = c("#e5e5e5", "#47B5FF", "#1363DF", "#06283D"), #colr = c("#e5e5e5", "#FFE085", "#B88A00", "#523D00"), 
               ext_val = 4000000/4, lab = c("0", "1", "2", "3", "4"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Already protected areas"))

#ggsave("Figures/CircularBarplot_Population_WDPA.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

## Carbon
max(country_protection_50$Carbon50)

country_protection_100$Carbon100 <- 100000

df_circbp_Carbon <- country_protection_50 %>%
  left_join(country_protection_30, by = "country") %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Carbon30 = Carbon30 - Carbon10) %>% 
  mutate(Carbon50 = Carbon50 - Carbon30 - Carbon10) %>% 
  mutate(Carbon100 = Carbon100 - Carbon50 - Carbon30 - Carbon10) %>%
  dplyr::select(country, Carbon100, Carbon50, Carbon30, Carbon10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Carbon10 + Carbon30 + Carbon50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Carbon100', 'Carbon50', 'Carbon30','Carbon10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Carbon100" ~ "100%",
                            name == "Carbon50" ~ "Top-ranked 50%",
                            name == "Carbon30" ~ "Top-ranked 30%",
                            name == "Carbon10" ~ "Already protected areas"))

df_circbp_Carbon$name <- NULL

circ_carb <- fPlot_Circular(df_circbp_Carbon, colr = c("#e5e5e5", "#47B5FF", "#1363DF", "#06283D"), #colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"),
               ext_val = 100000/4, lab = c("0", "2.5", "5", "7.5", "10"),
               lvl = c("100%", "Top-ranked 50%","Top-ranked 30%", "Already protected areas"))

#ggsave("Figures/CircularBarplot_Carbon_WDPA.svg", dpi = 1000, width = 13, height = 14, units = "cm", limitsize = FALSE)

plot <- circ_area + plot_spacer() + circ_prop + circ_pop + circ_carb + circ_fish +
  plot_layout(ncol = 2) +
  plot_annotation(tag_levels = 'a') +
  theme(plot.tag = element_text(face = 'bold'))

ggsave("Figures/CircularBarplot_WDPA.svg", width = 17.0, height = 25.6, units = "cm")


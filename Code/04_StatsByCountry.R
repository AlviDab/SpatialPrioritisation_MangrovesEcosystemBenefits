#Author: Dabalà Alvise

#Analysis of the results at country scale and continental scale
#Plot of circular barplots and print of excel sheets with the results

#Open all the packages needed
pacman::p_load(tidyverse, sf, prioritizr, patchwork, viridis, ggthemes, openxlsx, xlsx)

source("Functions/fStat_CountryContinent.r")
source("Functions/fStat_CountryContinent_PAs.r")
source("Functions/fStat_CountryContinent_AllPAs.r")
source("Functions/fPlot_Circular.r")

result_BioServ <- readRDS("RDS/1e-4/gurobi/result_BioServ.rds")
result_BioServ_WDPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_WDPA.rds")
result_BioServ_AllWDPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_AllWDPA.rds")
PUs <- readRDS("RDS/PUs_Splitted_I_IV_and_All_9111.rds")
PUs_NotSplitted <- readRDS("RDS/PUs_NotSplitted.rds")
ConsFeatures <- readRDS("RDS/ConsFeatures.rds")
ConsFeatures_NotSplitted <- readRDS("RDS/ConsFeatures_NotSplitted.rds")
species <- readRDS("RDS/species.rds")
result_BioServ_WDPA_rmPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_WDPA_rmPA.rds")
result_BioServ_AllWDPA_rmPA <- readRDS("RDS/1e-4/gurobi/result_BioServ_AllWDPA_rmPA.rds")

################################################################################

# Percentages of already protected priority areas
# Top ranked-10%
for (i in c(10, 30, 50)) {
  priority_protected <- result_BioServ %>%
    as_tibble() %>% 
    filter(rank <= i) %>% #Filter all the features with rank > i
    filter(Protected == TRUE) %>% #Select only the PUs already protected
    summarise(priority_protected = sum(AreaGMWKm)) #Sum the area of mangroves
  
  priority_notprotected <- result_BioServ %>% 
    as_tibble() %>% 
    filter(rank <= i) %>% #Filter all the features with rank > i
    filter(Protected == FALSE) %>%  #Select only PUs that are not already protected
    summarise(priority_notprotected = sum(AreaGMWKm)) #Sum the area of mangroves

  priority_protected_AllWDPA <- result_BioServ %>%
    as_tibble() %>% 
    filter(rank <= i) %>% #Filter all the features with rank > i
    filter(Protected_I_VI == TRUE) %>% #Select only the PUs already protected
    summarise(priority_protected = sum(AreaGMWKm)) #Sum the area of mangroves
  
  priority_notprotected_AllWDPA <- result_BioServ %>% 
    as_tibble() %>% 
    filter(rank <= i) %>% #Filter all the features with rank > i
    filter(Protected == FALSE) %>%  #Select only PUs that are not already protected
    summarise(priority_notprotected = sum(AreaGMWKm))
  
print(priority_protected/(priority_protected + priority_notprotected)*100) #Calculate the percentage of priority areas already protected
print(priority_protected_AllWDPA/(priority_protected_AllWDPA + priority_notprotected_AllWDPA)*100)
}

# Statistics when optimising for biodiversity and ecosystem services

Stat_CountryContinent_10 <- fStat_CountryContinent(result_BioServ, 10)
Stat_CountryContinent_30 <- fStat_CountryContinent(result_BioServ, 30)
Stat_CountryContinent_50 <- fStat_CountryContinent(result_BioServ, 50)

Stat_CountryContinent <- c(Stat_CountryContinent_10, Stat_CountryContinent_30, Stat_CountryContinent_50
                           )

# Save in an excel file
list_sheets <- c("10_Country", "10_Continent", "30_Country", "30_Continent", "50_Country", "50_Continent")

file <- paste("Figures/gurobi/", sep = "")

lapply(seq_along(Stat_CountryContinent), function(z) {
  write.xlsx(Stat_CountryContinent[[z]], paste0(file, "Stat_CountryContinent.xlsx"), sheetName = list_sheets[z], append = TRUE) 
}
)

# Statistics when building on already protected areas
Stat_CountryContinent_PAs <- fStat_CountryContinent_PAs(PUs, PUs_NotSplitted)
Stat_CountryContinent_30_WDPA <- fStat_CountryContinent(result_BioServ_WDPA_rmPA, 30)
Stat_CountryContinent_50_WDPA <- fStat_CountryContinent(result_BioServ_WDPA_rmPA, 50)

Stat_CountryContinent_WDPA <- c(Stat_CountryContinent_PAs, Stat_CountryContinent_30_WDPA, Stat_CountryContinent_50_WDPA)

# Save in an excel file
list_sheets <- c("WDPA_country", "WDPA_continent", "+16.8%_Country_WDPA", "+16.8%_Continent_WDPA", "+36.8%_Country_WDPA", "+36.8%_Continent_WDPA")

file <- paste("Figures/gurobi/", sep = "")

lapply(seq_along(Stat_CountryContinent_WDPA), function(z) {
  write.xlsx(Stat_CountryContinent_WDPA[[z]], paste0(file, "Stat_CountryContinent_WDPA.xlsx"), sheetName = list_sheets[z], append = TRUE) 
}
)

# Statistics when building on all already protected areas
PUs_1 <- PUs
PUs_1$Protected <- PUs$Protected_I_VI
PUs_1$AreaWDPA <- PUs$AreaWDPA_I_VI

Stat_CountryContinent_AllPAs <- fStat_CountryContinent_AllPAs(PUs)
Stat_CountryContinent_50_AllWDPA <- fStat_CountryContinent(result_BioServ_AllWDPA_rmPA, 50, IUCN = "All")

Stat_CountryContinent_AllWDPA <- c(Stat_CountryContinent_AllPAs, Stat_CountryContinent_50_AllWDPA)

# Save in an excel file
list_sheets <- c("AllWDPA_country", "AllWDPA_continent", "+7%_Country_AllWDPA", "+7%_Continent_AllWDPA")

file <- paste("Figures/gurobi/", sep = "")

lapply(seq_along(Stat_CountryContinent_AllWDPA), function(z) {
  write.xlsx(Stat_CountryContinent_AllWDPA[[z]], paste0(file, "Stat_CountryContinent_AllWDPA.xlsx"), sheetName = list_sheets[z], append = TRUE) 
}
)

# Number of countries with all the mangroves in priority areas protected
Stat_CountryContinent_30[[1]] %>% 
  filter(perc_priority_area_country == 100) %>%
  nrow()

Stat_CountryContinent_50[[1]] %>% 
  filter(perc_priority_area_country == 100) %>% 
  nrow()

################################################################################
# Amount of services protected

# Maximum services
Serv <- PUs %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            Population = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm)) %>% 
  dplyr::select(c("Fishing", "Population", "Properties", "Carbon", "AreaGMWKm")) %>% 
  st_drop_geometry() %>% 
  as_tibble()

ServPAs <- PUs %>% 
  filter(Protected == "TRUE") %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            Population = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm)) %>% 
  dplyr::select(c("Fishing", "Population", "Properties", "Carbon", "AreaGMWKm")) %>% 
  st_drop_geometry() %>% 
  as_tibble()

ServPAs/Serv*100

#Additional services +30%
Serv30 <- result_BioServ_WDPA %>% 
  filter(Protected == "FALSE") %>% 
  filter(rank <= 30) %>% 
  summarise(Fishing = sum(Fishing_Intensity*AreaGMWKm), 
            Population = sum(POP*AreaGMWKm),
            Properties = sum(TOT_STOCK*AreaGMWKm),
            Carbon = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm = sum(AreaGMWKm)) %>% 
  dplyr::select(c("Fishing", "Population", "Properties", "Carbon", "AreaGMWKm")) %>% 
  st_drop_geometry() %>% 
  as_tibble()

Serv30
Serv30/Serv*100

################################################################################
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

# country_protection_50 <- result_BioServ %>% 
#   as_tibble %>% 
#   dplyr::filter(rank <= 50) %>%  
#   mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
#   group_by(country) %>% 
#   summarise(Fishing50 = sum(Fishing_Intensity*AreaGMWKm), 
#             Population50 = sum(POP*AreaGMWKm),
#             Properties50 = sum(TOT_STOCK*AreaGMWKm),
#             Carbon50 = sum(Tot_Carbon*AreaGMWKm),
#             AreaGMWKm50 = sum(AreaGMWKm))

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

max(country_protection_30$AreaGMWKm30)
country_protection_100$AreaGMWKm100 <- 12000

colors <- c('30%' = 'blue', '50%' = 'orange')
legends <- c('30%', '50%', "70%")

df_circbp <- #country_protection_50 %>%
  country_protection_30 %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(AreaGMWKm30 = AreaGMWKm30 - AreaGMWKm10) %>% 
  #mutate(AreaGMWKm50 = AreaGMWKm50 - AreaGMWKm30 - AreaGMWKm10) %>% 
  mutate(AreaGMWKm100 = AreaGMWKm100 - 
           #AreaGMWKm50 - 
           AreaGMWKm30 - AreaGMWKm10) %>%
  dplyr::select(country, AreaGMWKm100, 
                #AreaGMWKm50, 
                AreaGMWKm30, AreaGMWKm10, continent) %>% 
  group_by(continent) %>%
  top_n(10, AreaGMWKm10 + AreaGMWKm30 #+ AreaGMWKm50
        ) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('AreaGMWKm100', 
                 #'AreaGMWKm50', 
                 'AreaGMWKm30','AreaGMWKm10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "AreaGMWKm100" ~ "100%",
                            #name == "AreaGMWKm50" ~ "Top-ranked 50%",
                            name == "AreaGMWKm30" ~ "Top-ranked 30%",
                            name == "AreaGMWKm10" ~ "Top-ranked 10%"))

df_circbp$name <- NULL

circ_area <- fPlot_Circular(df_circbp, colr = c("#e5e5e5", "#0AFFD6", 
                                                #"#00A388", 
                                                "#004D40"), 
               ext_val = 3000, lab = c("0", "3", "6", "9", "12"),
               lvl = c("100%", 
                       #"Top-ranked 50%",
                       "Top-ranked 30%", "Top-ranked 10%"))

################################################################################
#Circular barplot ecosystem services 
################################################################################

max(country_protection_30$Fishing30)

country_protection_100$Fishing100 <- 15000000

df_circbp_Fishing <- country_protection_30 %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Fishing30 = Fishing30 - Fishing10) %>% 
  mutate(Fishing100 = Fishing100 - Fishing30 - Fishing10) %>%
  dplyr::select(country, Fishing100, Fishing30, Fishing10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Fishing10 + Fishing30) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Fishing100', 'Fishing30','Fishing10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Fishing100" ~ "100%",
                            name == "Fishing30" ~ "Top-ranked 30%",
                            name == "Fishing10" ~ "Top-ranked 10%"))

df_circbp_Fishing$name <- NULL

circ_fish <- fPlot_Circular(df_circbp_Fishing, colr = c("#e5e5e5", "#0AFFD6", "#004D40"),
               ext_val = 15000000/4, lab = c("0", "3.75", "7.5", "11.25", "15"),
               lvl = c("100%", "Top-ranked 30%", "Top-ranked 10%"))

## Properties
max(country_protection_30$Properties30)

country_protection_100$Properties100 <- 8000000000

df_circbp_Properties <- country_protection_30 %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Properties30 = Properties30 - Properties10) %>%
  mutate(Properties100 = Properties100 - Properties30 - Properties10) %>%
  dplyr::select(country, Properties100, Properties30, Properties10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Properties10 + Properties30) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Properties100', 'Properties30','Properties10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Properties100" ~ "100%",
                            name == "Properties30" ~ "Top-ranked 30%",
                            name == "Properties10" ~ "Top-ranked 10%"))
df_circbp_Properties$name <- NULL

circ_prop <- fPlot_Circular(df_circbp_Properties, colr = c("#e5e5e5", "#0AFFD6", "#004D40"),
               ext_val = 8000000000/4, lab = c("0", "2", "4", "6", "8"),
               lvl = c("100%", "Top-ranked 30%", "Top-ranked 10%"))

## Population
max(country_protection_30$Population30)

country_protection_100$Population100 <- 3600000

df_circbp_Population <- country_protection_30 %>%
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Population30 = Population30 - Population10) %>% 
  mutate(Population100 = Population100 - Population30 - Population10) %>%
  dplyr::select(country, Population100, Population30, Population10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Population10 + Population30) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Population100', 'Population30','Population10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Population100" ~ "100%",
                            name == "Population30" ~ "Top-ranked 30%",
                            name == "Population10" ~ "Top-ranked 10%"))

df_circbp_Population$name <- NULL

circ_pop <- fPlot_Circular(df_circbp_Population, colr = c("#e5e5e5", "#0AFFD6", "#004D40"), 
               ext_val = 3600000/4, lab = c("0", "0.9", "1.8", "2.7", "3.6"),
               lvl = c("100%", "Top-ranked 30%", "Top-ranked 10%"))

#Carbon
max(country_protection_30$Carbon30)

country_protection_100$Carbon100 <- 800

df_circbp_Carbon <- country_protection_30 %>% 
  left_join(country_protection_10, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  mutate(Carbon30 = Carbon30 - Carbon10) %>%
  mutate(Carbon100 = Carbon100 - Carbon30 - Carbon10) %>%
  dplyr::select(country, Carbon100, Carbon30, Carbon10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Carbon10 + Carbon30) %>% 
  #filter(continent != "Europe") %>%
  pivot_longer(c('Carbon100', 'Carbon30','Carbon10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Carbon100" ~ "100%",
                            name == "Carbon30" ~ "Top-ranked 30%",
                            name == "Carbon10" ~ "Top-ranked 10%"))

df_circbp_Carbon$name <- NULL

circ_carb <- fPlot_Circular(df_circbp_Carbon, colr = c("#e5e5e5", "#0AFFD6", "#004D40"), 
               ext_val = 800/4, lab = c("0", "200", "400", "600", "800"),
               lvl = c("100%", "Top-ranked 30%", "Top-ranked 10%"))

p1 <- circ_area + 
  plot_annotation(title = (expression(bold('a) Mangrove area (thousands km'^2*')'))),
                  theme = theme(plot.title = element_text(face = 'bold', size = 10))) 
p2 <- circ_prop +
  plot_annotation(title = 'b) Properties (billions USD)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p3 <- circ_pop +
  plot_annotation(title = 'c) Population (million people)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p4 <- circ_carb + 
  plot_annotation(title = 'd) Carbon (Mt)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p5 <- circ_fish +
  plot_annotation(title = expression(bold('e) Fishing intensity (million fisher days yr'^-1*')')),
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))

ptot <- wrap_elements(p1) + plot_spacer() + wrap_elements(p2) + wrap_elements(p3) + wrap_elements(p4) + wrap_elements(p5) +
  plot_layout(ncol = 2)

ggsave("Figures/gurobi/CircularBarplot.svg", width = 17.0, height = 25.6, units = "cm")

df_circbp <- df_circbp %>% 
  rename(MangroveArea = value)

df_circbp_Properties <- df_circbp_Properties %>% 
  rename(Properties = value)

df_circbp_Population <- df_circbp_Population %>% 
  rename(Population = value)

df_circbp_Carbon <- df_circbp_Carbon %>% 
  rename(Carbon = value)

df_circbp_Fishing <- df_circbp_Fishing %>% 
  rename(FishingIntensity = value)

#Data source Supplementary Fig. 4

SuppFig4 <- df_circbp %>% 
  full_join(df_circbp_Properties, by = c("feature", "group", "target")) %>% 
  full_join(df_circbp_Population, by = c("feature", "group", "target")) %>% 
  full_join(df_circbp_Carbon, by = c("feature", "group", "target"))  %>% 
  full_join(df_circbp_Fishing, by = c("feature", "group", "target")) 

saveRDS(SuppFig4, "RDS/SuppFig4.rds")

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

# country_protection_50 <- result_BioServ_WDPA %>% 
#   as_tibble %>% 
#   dplyr::filter(rank <= 50) %>% 
#   #mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>% 
#   group_by(country) %>% 
#   summarise(Fishing50 = sum(Fishing_Intensity*AreaGMWKm), 
#             Population50 = sum(POP*AreaGMWKm),
#             Properties50 = sum(TOT_STOCK*AreaGMWKm),
#             Carbon50 = sum(Tot_Carbon*AreaGMWKm),
#             AreaGMWKm50 = sum(AreaGMWKm))

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

max(country_protection_30$AreaGMWKm30)

country_protection_100$AreaGMWKm100 <- 12000

df_circbp <- country_protection_30 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(AreaGMWKm30 = AreaGMWKm30 - AreaGMWKm10) %>%
  mutate(AreaGMWKm100 = AreaGMWKm100 - AreaGMWKm30 - AreaGMWKm10) %>%
  dplyr::select(country, AreaGMWKm100, AreaGMWKm30, AreaGMWKm10, continent) %>% 
  group_by(continent) %>%
  top_n(10, AreaGMWKm10 + AreaGMWKm30) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('AreaGMWKm100', 'AreaGMWKm30','AreaGMWKm10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "AreaGMWKm100" ~ "100%",
                            name == "AreaGMWKm30" ~ "Top-ranked 30%",
                            name == "AreaGMWKm10" ~ "Already protected areas"))

df_circbp$name <- NULL

circ_area <- fPlot_Circular(df_circbp, colr = c("#e5e5e5", "#47B5FF", "#06283D"), #colr = c("#e5e5e5", "#FEB078", "#B4367A", "#000000"), 
               ext_val = 12000/4, 
               lab = c("0", "3", "6", "9", "12"), 
               lvl = c("100%", "Top-ranked 30%", "Already protected areas"))

################################################################################
#Circular barplot ecosystem services
max(country_protection_30$Fishing30)

country_protection_100$Fishing100 <- 15000000

df_circbp_Fishing <- country_protection_30 %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Fishing30 = Fishing30 - Fishing10) %>% 
  mutate(Fishing100 = Fishing100 - Fishing30 - Fishing10) %>%
  dplyr::select(country, Fishing100, Fishing30, Fishing10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Fishing10 + Fishing30) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Fishing100', 'Fishing30','Fishing10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Fishing100" ~ "100%",
                            name == "Fishing30" ~ "Top-ranked 30%",
                            name == "Fishing10" ~ "Already protected areas"))

df_circbp_Fishing$name <- NULL

circ_fish <- fPlot_Circular(df_circbp_Fishing, colr = c("#e5e5e5", "#47B5FF", "#06283D"), 
               ext_val = 15000000/4, lab = c("0", "3.75", "7.5", "11.25", "15"),
               lvl = c("100%", "Top-ranked 30%", "Already protected areas"))

## Properties
max(country_protection_30$Properties30)

country_protection_100$Properties100 <- 8000000000

df_circbp_Properties <- country_protection_30 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>%
  replace(is.na(.), 0) %>% 
  mutate(Properties30 = Properties30 - Properties10) %>% 
  mutate(Properties100 = Properties100 - Properties30 - Properties10) %>%
  dplyr::select(country, Properties100, Properties30, Properties10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Properties10 + Properties30) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Properties100', 'Properties30','Properties10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Properties100" ~ "100%",
                            name == "Properties30" ~ "Top-ranked 30%",
                            name == "Properties10" ~ "Already protected areas"))

df_circbp_Properties$name <- NULL

circ_prop <- fPlot_Circular(df_circbp_Properties, colr = c("#e5e5e5", "#47B5FF", "#06283D"), #colr = c("#e5e5e5", "#F4A4C1", "#E94984", "#6D0D30"),
               ext_val = 8000000000/4, lab = c("0", "2", "4", "6", "8"),
               lvl = c("100%", "Top-ranked 30%", "Already protected areas"))

## Population
max(country_protection_30$Population30)

country_protection_100$Population100 <- 3200000

df_circbp_Population <- country_protection_30 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Population30 = Population30 - Population10) %>% 
  mutate(Population100 = Population100 - Population30 - Population10) %>%
  dplyr::select(country, Population100, Population30, Population10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Population10 + Population30) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Population100', 'Population30','Population10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Population100" ~ "100%",
                            name == "Population30" ~ "Top-ranked 30%",
                            name == "Population10" ~ "Already protected areas"))

df_circbp_Population$name <- NULL

circ_pop <- fPlot_Circular(df_circbp_Population, colr = c("#e5e5e5", "#47B5FF", "#06283D"), #colr = c("#e5e5e5", "#FFE085", "#B88A00", "#523D00"), 
               ext_val = 3200000/4, lab = c("0", "0.8", "1.6", "2.4", "3.2"),
               lvl = c("100%", "Top-ranked 30%", "Already protected areas"))

## Carbon
max(country_protection_30$Carbon30)

country_protection_100$Carbon100 <- 800

df_circbp_Carbon <- country_protection_30 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Carbon30 = Carbon30 - Carbon10) %>% 
  mutate(Carbon100 = Carbon100 - Carbon30 - Carbon10) %>%
  dplyr::select(country, Carbon100, Carbon30, Carbon10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Carbon10 + Carbon30) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Carbon100', 'Carbon30','Carbon10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Carbon100" ~ "100%",
                            name == "Carbon30" ~ "Top-ranked 30%",
                            name == "Carbon10" ~ "Already protected areas"))

df_circbp_Carbon$name <- NULL

circ_carb <- fPlot_Circular(df_circbp_Carbon, colr = c("#e5e5e5", "#47B5FF", "#06283D"), #colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"),
               ext_val = 800/4, lab = c("0", "200", "400", "600", "800"),
               lvl = c("100%", "Top-ranked 30%", "Already protected areas"))

p1 <- circ_area + 
  plot_annotation(title = (expression(bold('a) Mangrove area (thousands km'^2*')'))),
                  theme = theme(plot.title = element_text(face = 'bold', size = 10))) 
p2 <- circ_prop +
  plot_annotation(title = 'b) Properties (billions USD)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p3 <- circ_pop +
  plot_annotation(title = 'c) Population (million people)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p4 <- circ_carb + 
  plot_annotation(title = 'd) Carbon (Mt)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p5 <- circ_fish +
  plot_annotation(title = expression(bold('e) Fishing intensity (million fisher days yr'^-1*')')),
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))

ptot <- wrap_elements(p1) + plot_spacer() + wrap_elements(p2) + wrap_elements(p3) + wrap_elements(p4) + wrap_elements(p5) +
  plot_layout(ncol = 2)

ggsave("Figures/gurobi/CircularBarplot_WDPA.svg", width = 17.0, height = 25.6, units = "cm")

df_circbp <- df_circbp %>% 
  rename(MangroveArea = value)

df_circbp_Properties <- df_circbp_Properties %>% 
  rename(Properties = value)

df_circbp_Population <- df_circbp_Population %>% 
  rename(Population = value)

df_circbp_Carbon <- df_circbp_Carbon %>% 
  rename(Carbon = value)

df_circbp_Fishing <- df_circbp_Fishing %>% 
  rename(FishingIntensity = value)

#Data source Fig. 1

Fig1 <- df_circbp %>% 
  full_join(df_circbp_Properties, by = c("feature", "group", "target")) %>% 
  full_join(df_circbp_Population, by = c("feature", "group", "target")) %>% 
  full_join(df_circbp_Carbon, by = c("feature", "group", "target"))  %>% 
  full_join(df_circbp_Fishing, by = c("feature", "group", "target")) 

saveRDS(Fig1, "RDS/Fig1.rds")

################################################################################
# BUilding on all already protected areas
################################################################################

result_BioServ_AllWDPA <- result_BioServ_AllWDPA %>% 
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
country_protection_PA <- result_BioServ_AllWDPA %>% 
  as_tibble %>% 
  filter(Protected_I_VI == "TRUE") %>%  
  group_by(country) %>% 
  summarise(Fishing10 = sum(Fishing_Intensity*AreaGMWKm), 
            Population10 = sum(POP*AreaGMWKm),
            Properties10 = sum(TOT_STOCK*AreaGMWKm),
            Carbon10 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm10 = sum(AreaGMWKm))

country_protection_50 <- result_BioServ_AllWDPA %>%
  as_tibble %>%
  dplyr::filter(rank <= 50) %>%
  #mutate(country = recode(country,  `Micronesia (Federated States of)` = "Micronesia")) %>%
  group_by(country) %>%
  summarise(Fishing50 = sum(Fishing_Intensity*AreaGMWKm),
            Population50 = sum(POP*AreaGMWKm),
            Properties50 = sum(TOT_STOCK*AreaGMWKm),
            Carbon50 = sum(Tot_Carbon*AreaGMWKm),
            AreaGMWKm50 = sum(AreaGMWKm))

country_protection_100 <- result_BioServ_AllWDPA %>% 
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

max(country_protection_50$AreaGMWKm50)

country_protection_100$AreaGMWKm100 <- 10000

df_circbp <- country_protection_50 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(AreaGMWKm50 = AreaGMWKm50 - AreaGMWKm10) %>%
  mutate(AreaGMWKm100 = AreaGMWKm100 - AreaGMWKm50 - AreaGMWKm10) %>%
  dplyr::select(country, AreaGMWKm100, AreaGMWKm50, AreaGMWKm10, continent) %>% 
  group_by(continent) %>%
  top_n(10, AreaGMWKm10 + AreaGMWKm50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('AreaGMWKm100', 'AreaGMWKm50','AreaGMWKm10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "AreaGMWKm100" ~ "100%",
                            name == "AreaGMWKm50" ~ "Top-ranked 50%",
                            name == "AreaGMWKm10" ~ "Already protected areas"))

df_circbp$name <- NULL

circ_area <- fPlot_Circular(df_circbp, colr = c("#e5e5e5", "#B6C97E", "#2C3A34"), #colr = c("#e5e5e5", "#FEB078", "#B4367A", "#000000"), 
                            ext_val = 10000/4, 
                            lab = c("0", "2.5", "5", "7.5", "10"), 
                            lvl = c("100%", "Top-ranked 50%", "Already protected areas"))

################################################################################
#Circular barplot ecosystem services
max(country_protection_50$Fishing50)

country_protection_100$Fishing100 <- 14000000

df_circbp_Fishing <- country_protection_50 %>% 
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Fishing50 = Fishing50 - Fishing10) %>% 
  mutate(Fishing100 = Fishing100 - Fishing50 - Fishing10) %>%
  dplyr::select(country, Fishing100, Fishing50, Fishing10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Fishing10 + Fishing50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Fishing100', 'Fishing50','Fishing10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Fishing100" ~ "100%",
                            name == "Fishing50" ~ "Top-ranked 50%",
                            name == "Fishing10" ~ "Already protected areas"))

df_circbp_Fishing$name <- NULL

circ_fish <- fPlot_Circular(df_circbp_Fishing, colr = c("#e5e5e5", "#B6C97E", "#2C3A34"), 
                            ext_val = 14000000/4, lab = c("0", "3.5", "7", "10.5", "14"),
                            lvl = c("100%", "Top-ranked 50%", "Already protected areas"))


## Properties
max(country_protection_50$Properties50)

country_protection_100$Properties100 <- 8000000000

df_circbp_Properties <- country_protection_50 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>%
  replace(is.na(.), 0) %>% 
  mutate(Properties50 = Properties50 - Properties10) %>% 
  mutate(Properties100 = Properties100 - Properties50 - Properties10) %>%
  dplyr::select(country, Properties100, Properties50, Properties10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Properties10 + Properties50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Properties100', 'Properties50','Properties10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Properties100" ~ "100%",
                            name == "Properties50" ~ "Top-ranked 50%",
                            name == "Properties10" ~ "Already protected areas"))

df_circbp_Properties$name <- NULL

circ_prop <- fPlot_Circular(df_circbp_Properties, colr = c("#e5e5e5", "#B6C97E", "#2C3A34"), #colr = c("#e5e5e5", "#F4A4C1", "#E94984", "#6D0D30"),
                            ext_val = 8000000000/4, lab = c("0", "2", "4", "6", "8"),
                            lvl = c("100%", "Top-ranked 50%", "Already protected areas"))

## Population
max(country_protection_50$Population50)

country_protection_100$Population100 <- 3000000

df_circbp_Population <- country_protection_50 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Population50 = Population50 - Population10) %>% 
  mutate(Population100 = Population100 - Population50 - Population10) %>%
  dplyr::select(country, Population100, Population50, Population10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Population10 + Population50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Population100', 'Population50','Population10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Population100" ~ "100%",
                            name == "Population50" ~ "Top-ranked 50%",
                            name == "Population10" ~ "Already protected areas"))

df_circbp_Population$name <- NULL

circ_pop <- fPlot_Circular(df_circbp_Population, colr = c("#e5e5e5", "#B6C97E", "#2C3A34"), #colr = c("#e5e5e5", "#FFE085", "#B88A00", "#523D00"), 
                           ext_val = 3000000/4, lab = c("0", "0.75", "1.5", "2.25", "3"),
                           lvl = c("100%", "Top-ranked 50%", "Already protected areas"))

## Carbon
max(country_protection_50$Carbon50)

country_protection_100$Carbon100 <- 800

df_circbp_Carbon <- country_protection_50 %>%
  left_join(country_protection_PA, by = "country") %>% 
  left_join(country_protection_100, by = "country") %>% 
  replace(is.na(.), 0) %>% 
  mutate(Carbon50 = Carbon50 - Carbon10) %>% 
  mutate(Carbon100 = Carbon100 - Carbon50 - Carbon10) %>%
  dplyr::select(country, Carbon100, Carbon50, Carbon10, continent) %>% 
  group_by(continent) %>%
  top_n(10, Carbon10 + Carbon50) %>% 
  filter(continent != "Europe") %>%
  pivot_longer(c('Carbon100', 'Carbon50','Carbon10'), values_to = "value") %>% 
  rename(feature = country,
         group = continent) %>% 
  mutate(target = case_when(name == "Carbon100" ~ "100%",
                            name == "Carbon50" ~ "Top-ranked 50%",
                            name == "Carbon10" ~ "Already protected areas"))

df_circbp_Carbon$name <- NULL

circ_carb <- fPlot_Circular(df_circbp_Carbon, colr = c("#e5e5e5", "#B6C97E", "#2C3A34"), #colr = c("#e5e5e5", "#0AFFD6", "#00A388", "#004D40"),
                            ext_val = 800/4, lab = c("0", "200", "400", "600", "800"),
                            lvl = c("100%", "Top-ranked 50%", "Already protected areas"))

p1 <- circ_area + 
  plot_annotation(title = (expression(bold('a) Mangrove area (thousands km'^2*')'))),
                  theme = theme(plot.title = element_text(face = 'bold', size = 10))) 
p2 <- circ_prop +
  plot_annotation(title = 'b) Properties (billions USD)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p3 <- circ_pop +
  plot_annotation(title = 'c) Population (million people)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p4 <- circ_carb + 
  plot_annotation(title = 'd) Carbon (Mt)',
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))
p5 <- circ_fish +
  plot_annotation(title = expression(bold('e) Fishing intensity (million fisher days yr'^-1*')')),
                  theme = theme(plot.title = element_text(face = 'bold', size = 10)))

ptot <- wrap_elements(p1) + plot_spacer() + wrap_elements(p2) + wrap_elements(p3) + wrap_elements(p4) + wrap_elements(p5) +
  plot_layout(ncol = 2)

ggsave("Figures/gurobi/CircularBarplot_AllWDPA.svg", width = 17.0, height = 25.6, units = "cm")

df_circbp <- df_circbp %>% 
  rename(MangroveArea = value)

df_circbp_Properties <- df_circbp_Properties %>% 
  rename(Properties = value)

df_circbp_Population <- df_circbp_Population %>% 
  rename(Population = value)

df_circbp_Carbon <- df_circbp_Carbon %>% 
  rename(Carbon = value)

df_circbp_Fishing <- df_circbp_Fishing %>% 
  rename(FishingIntensity = value)

#Data source Supplementary Fig.3

SuppFig3 <- df_circbp %>% 
  full_join(df_circbp_Properties, by = c("feature", "group", "target")) %>% 
  full_join(df_circbp_Population, by = c("feature", "group", "target")) %>% 
  full_join(df_circbp_Carbon, by = c("feature", "group", "target"))  %>% 
  full_join(df_circbp_Fishing, by = c("feature", "group", "target")) 

saveRDS(SuppFig3, "RDS/SuppFig3.rds")

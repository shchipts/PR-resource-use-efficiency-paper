library(dplyr)
library(phosphateRock)
library(spatstat.geom)
library(reshape2)

# GHG emissions (????2-eq), kg per tonne of ore
# Representative points: PhosAgro (Russia) & OCP (Morocco)
# details on data in emissions.xlsx
co2_ore_ign <- 17.146 # source: PhosAgro annual reports
co2_ore_sed <- 46.643 # source: OCP annual reports and factsheets


data <- mining_complexes %>%
  filter(Status == "operational") %>%
  select(Name, Region, Country, Rock_type, Capacity) %>%
  left_join(
    ore %>% mutate(G_Ore = Value / 100) %>% select(Name, G_Ore), 
    by = c("Name")) %>%
  left_join(
    PR %>% mutate(G_PR = Value / 100) %>% select(Name, G_PR), 
    by = c("Name")) %>%
  left_join(
    recovery_mass %>% select(Name, Value) %>% rename(R_mass = Value), 
    by = c("Name")) %>%
  left_join(
    recovery_mineral %>% select(Name, Value) %>% rename(R_mineral = Value), 
    by = c("Name")) %>%
  mutate(
    PR = 1 / G_PR,
    Ore = 1 / (G_PR * R_mass),
    Waste = (1 - R_mass) / (G_PR * R_mass),
    P2O5_Loss = 1 / R_mineral - 1) %>%
  mutate(
    GHG_kg = Ore * case_when(
      Rock_type == "sedimentary" ~ co2_ore_sed,
      TRUE ~ co2_ore_ign)) %>%
  filter(!is.na(Capacity)) %>%
  mutate(Capacity_Content = Capacity * G_PR) %>%
  mutate(weight = Capacity_Content / sum(Capacity_Content)) %>%
  arrange(desc(weight)) %>%
  select(-c(G_Ore, G_PR, R_mass, R_mineral)) %>%
  melt(id.vars = c(
    "Name",
    "Region",
    "Country",
    "Rock_type",
    "Capacity",
    "Capacity_Content",
    "weight"))


stats <- function(d) {
  
  ws <- d %>% pull(weight)
  
  if (all(is.na(ws))) {
    ws <- rep(1, length(ws))
  }
  
  d %>%
    summarise(
      Capacity = sum(Capacity, na.rm = TRUE),
      Capacity_Content = sum(Capacity_Content, na.rm = TRUE),
      weight = sum(weight, na.rm = TRUE),
      median = weighted.median(value, ws),
      q17 = weighted.quantile(value, ws, probs = c(0.17)),
      q83 = weighted.quantile(value, ws, probs = c(0.83)),
      mean = weighted.mean(value, ws))
}

data_world <- data %>%
  group_by(variable) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = "World")

data_regions <- data %>%
  group_by(variable, Region) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  rename(Id = Region)

data_regions_rock <- data %>%
  group_by(variable, Region, Rock_type) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = paste(Region, " - ", Rock_type, sep = "")) %>%
  select(-c(Region, Rock_type))

data_rock <- data %>%
  group_by(variable, Rock_type) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = paste("World", " - ", Rock_type, sep = "")) %>%
  select(-Rock_type)

data_complex <- data %>%
  mutate(
    Company = case_when(
      Name %in% c("Khouribga", "Gantour", "Boucraa") ~ "OCP (Morocco)",
      Name %in% c("Kola") ~ "PhosAgro (Russia)",
      Name %in% c("Bayovar", "Florida") ~ "Mosaic (US, Peru)",
      Name %in% c("Tapira", "Araxa/Patrocinio", "Catalao", "Cajati") ~ "Mosaic (Brazil)",
      Country == "Saudi Arabia" ~ "Ma'aden",
      Name %in% c("Haikou") ~ "YPH-Haikou (China)",
      TRUE ~ NA)) %>%
  filter(!is.na(Company)) %>%
  group_by(variable, Company) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  rename(Id = Company)

output <- data_world %>%
  rbind(data_regions) %>%
  rbind(data_regions_rock) %>%
  rbind(data_rock) %>%
  arrange(Id) %>%
  rbind(data_complex) %>%
  select(Id, everything())

write.csv(
  output,
  file = "bin/material consumption.csv",
  row.names = FALSE)
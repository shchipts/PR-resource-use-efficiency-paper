library(dplyr)
library(phosphateRock)
library(reshape2)

data <- mining_complexes %>%
  select(Name, Region, Country, Rock_type, Capacity, Status) %>%
  left_join(
    PR %>% rename(G_PR = Value) %>% select(Name, G_PR), 
    by = c("Name")) %>%
  rbind(
    data.frame(
      Name = "China rest",
      Region = "East Asia",
      Country = "China",
      Rock_type = "sedimentary",
      Status = "operational",
      Capacity = 130 - (mining_complexes %>%
                          filter(Region == "East Asia") %>% 
                          pull(Capacity) %>% 
                          sum()), # IFA 2023: East Asia capacity 2022
      G_PR = PR %>% filter(Name == "Haikou") %>% pull(Value))) %>% # Assumed same P2O5 grade as for Haikou
  mutate(
    Company = case_when(
      Name %in% c("Khouribga", "Gantour", "Boucraa") ~ "OCP (Morocco)",
      Name %in% c("Kola") ~ "PhosAgro (Russia)",
      Name %in% c("Bayovar", "Florida") ~ "Mosaic (US, Peru)",
      Name %in% c("Tapira", "Araxa/Patrocinio", "Catalao", "Cajati") ~ "Mosaic (Brazil)",
      Country == "Saudi Arabia" ~ "Ma'aden",
      Name %in% c("Haikou") ~ "YPH-Haikou (China)",
      TRUE ~ NA)) %>%
  mutate(Capacity_Content = Capacity * G_PR / 100)
  

regions <- data %>%
  filter(Status != "in development") %>%
  group_by(Region) %>%
  summarise(
    Capacity = sum(Capacity, na.rm = TRUE),
    Capacity_Content = sum(Capacity_Content, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    frac_Capacity = Capacity / sum(Capacity),
    frac_Capacity_Content = Capacity_Content / sum(Capacity_Content)) %>%
  as.data.frame() %>%
  mutate(Category = "Region (fractions excl. non-operational)") %>%
  mutate(Id = Region) %>%
  arrange(desc(frac_Capacity_Content))

rock_types <- data %>%
  filter(Status != "in development") %>%
  group_by(Rock_type) %>%
  summarise(
    Capacity = sum(Capacity, na.rm = TRUE),
    Capacity_Content = sum(Capacity_Content, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    frac_Capacity = Capacity / sum(Capacity),
    frac_Capacity_Content = Capacity_Content / sum(Capacity_Content)) %>%
  as.data.frame() %>%
  mutate(Category = "Rock type (fractions excl. non-operational)") %>%
  rename(Id = Rock_type) %>%
  mutate(Region = NA) %>%
  arrange(desc(frac_Capacity_Content))

countries <-  data %>%
  filter(Status != "in development") %>%
  group_by(Country) %>%
  summarise(
    Region = head(Region, 1),
    Capacity = sum(Capacity, na.rm = TRUE),
    Capacity_Content = sum(Capacity_Content, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    frac_Capacity = Capacity / sum(Capacity),
    frac_Capacity_Content = Capacity_Content / sum(Capacity_Content)) %>%
  as.data.frame() %>%
  mutate(Category = "Country (fractions excl. non-operational)") %>%
  rename(Id = Country) %>%
  arrange(desc(frac_Capacity_Content))

companies <- data %>%
  filter(Status != "in development") %>%
  filter(Region != "East Asia") %>%
  group_by(Company) %>%
  summarise(
    Region = head(Region, 1),
    Capacity = sum(Capacity, na.rm = TRUE),
    Capacity_Content = sum(Capacity_Content, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    Region = case_when(
      Company == "Mosaic (US, Peru)" ~ NA,
      Company == "Mosaic (Brazil)" ~ NA,
      TRUE ~ Region)) %>%
  mutate(
    frac_Capacity = Capacity / sum(Capacity),
    frac_Capacity_Content = Capacity_Content / sum(Capacity_Content)) %>%
  as.data.frame() %>%
  filter(!is.na(Company)) %>%
  mutate(Category = "Company (fractions excl. non-operational and East Asia)") %>%
  rename(Id = Company) %>%
  arrange(desc(frac_Capacity_Content))

complexes <- data %>%
  select(Name, Region, Capacity, Capacity_Content) %>%
  mutate(
    frac_Capacity = Capacity / sum(Capacity, na.rm = TRUE),
    frac_Capacity_Content = 
      Capacity_Content / sum(Capacity_Content, na.rm = TRUE)) %>%
  as.data.frame() %>%
  mutate(Category = "Complex") %>%
  rename(Id = Name) %>%
  arrange(desc(Capacity_Content))

percentiles <- data %>%
  select(Name, Capacity, Capacity_Content) %>%
  melt(id.vars = c("Name")) %>%
  group_by(variable) %>%
  summarise(
    q17 = quantile(value, probs = c(0.17), na.rm = TRUE),
    q50 = quantile(value, probs = c(0.5), na.rm = TRUE),
    q83 = quantile(value, probs = c(0.83), na.rm = TRUE)) %>%
  ungroup() %>%
  as.data.frame() %>%
  rename(Id = variable) %>%
  melt(id.vars = c("Id")) %>%
  mutate(
    Capacity = case_when(
      Id == "Capacity" ~ value,
      TRUE ~ NA),
    Capacity_Content = case_when(
      Id == "Capacity_Content" ~ value,
      TRUE ~ NA)) %>%
  mutate(
    Id = paste(Id, variable),
    frac_Capacity = NA,
    frac_Capacity_Content = NA,
    Region = "World",
    Category = "Percentile") %>%
  select(-c(variable, value))

output <- regions %>%
  rbind(rock_types) %>%
  rbind(countries) %>%
  rbind(companies) %>%
  rbind(complexes) %>%
  rbind(percentiles) %>%
  left_join(
    regions %>%
      select(Region, Capacity_Content) %>%
      rename(Region_Capacity_Content = Capacity_Content),
    by = c("Region")) %>%
  mutate(
    frac_Region_Capacity_Content = 
      Capacity_Content / Region_Capacity_Content) %>%
  select(-c(Region_Capacity_Content)) %>%
  select(Id, Region, everything())

write.csv(output, file = "bin/capacity summary.csv", row.names = FALSE)
library(dplyr)
library(phosphateRock)

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
  filter(!is.na(Capacity)) %>%
  mutate(Capacity_Content = Capacity * G_PR) %>%
  mutate(weight = Capacity_Content / sum(Capacity_Content)) %>%
  arrange(desc(weight))

material_flows <- function(d) {
  
  ws <- d %>% pull(weight) %>% sum()
  flows <- d %>%
    select(PR, Ore, Waste, P2O5_Loss, weight) %>%
    mutate_each(~ . * weight / ws) %>%
    select(-weight) %>%
    summarise(across(everything(), list(sum = sum)))
  
  return (d %>%
            summarise(
              Capacity = sum(Capacity),
              Capacity_Content = sum(Capacity_Content),
              weight = sum(weight)) %>%
            cbind(flows))
}

data_world <- data %>%
  material_flows() %>%
  mutate(Id = "World")

data_regions <- data %>%
  group_by(Region) %>%
  group_modify(~ material_flows(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  rename(Id = Region)

data_regions_rock <- data %>%
  group_by(Region, Rock_type) %>%
  group_modify(~ material_flows(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = paste(Region, " - ", Rock_type, sep = "")) %>%
  select(-c(Region, Rock_type))

data_rock <- data %>%
  group_by(Rock_type) %>%
  group_modify(~ material_flows(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = paste("World", " - ", Rock_type, sep = "")) %>%
  select(-Rock_type)

data_complex <- data %>%
  mutate(
    Company = case_when(
      Name == "Kola" ~ "PhosAgro",
      Country == "Saudi Arabia" ~ "Ma'aden",
      Name %in% c("Khouribga", "Gantour", "Boucraa") ~ "OCP",
      Name %in% c(
        "Florida", 
        "Bayovar", 
        "Tapira", 
        "Araxa/Patrocinio", 
        "Catalao", 
        "Cajati") ~ "Mosaic",
      Name == "Haikou" ~ Name,
      TRUE ~ NA)) %>%
  filter(!is.na(Company)) %>%
  group_by(Company) %>%
  group_modify(~ material_flows(.x)) %>%
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
library(dplyr)
library(phosphateRock)
library(reshape2)

data <- mining_complexes %>%
  select(Name, Country, Company, Rock_type, Capacity) %>%
  left_join(
    ore %>% rename(G_Ore = Value) %>% select(Name, G_Ore), 
    by = c("Name")) %>%
  left_join(
    PR %>% rename(G_PR = Value) %>% select(Name, G_PR), 
    by = c("Name")) %>%
  left_join(
    mineral %>% rename(G_Mineral = Value) %>% select(Name, G_Mineral), 
    by = c("Name")) %>%
  left_join(
    recovery_mass %>% select(Name, Value) %>% rename(R_mass = Value), 
    by = c("Name")) %>%
  mutate(G_Waste = case_when(
    R_mass < 1 ~ (G_Ore - G_PR * R_mass) / (1 - R_mass),
    TRUE ~ NA)) %>%
  mutate(
    Beneficiation_Upgrade = case_when(
      R_mass < 1 ~ G_PR - G_Ore,
      TRUE ~ NA)) %>%
  mutate(Potential =  G_Mineral - G_PR) %>%
  mutate(Capacity_Content = Capacity * G_PR / 100) %>%
  mutate(weight = Capacity_Content / sum(Capacity_Content, na.rm = TRUE))

grades <- function(d) {
  
  ws <- d %>% pull(weight) %>% sum(na.rm = TRUE)
  
  if (ws == 0) {
    
    d <- d %>% mutate(weight = 1)
    ws <- d %>% pull(weight) %>% sum()
  }
  
  gs <- d %>%
    select(
      G_PR, 
      G_Ore, 
      G_Mineral, 
      G_Waste, 
      Beneficiation_Upgrade, 
      Potential, 
      weight) %>%
    mutate_each(~ . * weight / ws) %>%
    select(-weight) %>%
    summarise(across(everything(), list(~ sum(., na.rm = TRUE))))
  
  return (d %>%
            summarise(
              Capacity = sum(Capacity, na.rm = TRUE),
              Capacity_Content = sum(Capacity_Content, na.rm = TRUE),
              weight = sum(weight, na.rm = TRUE)) %>%
            cbind(gs))
}

grade_limit <- function(d, func) {
  
  d %>%
    filter(value == func(value)) %>%
    rbind(
      d %>%
        filter(G_PR == func(G_PR)) %>%
        select(Name, Country, G_PR) %>%
        rename(G = G_PR) %>%
        mutate(Material = "PR")) %>%
    rbind(
      d %>%
        filter(G_Mineral == func(G_Mineral)) %>%
        select(Name, Country, G_Mineral) %>%
        rename(G = G_Mineral) %>%
        mutate(Material = "Mineral")) %>%
    rbind(
      d %>%
        filter(Beneficiation_Upgrade == func(Beneficiation_Upgrade)) %>%
        select(Name, Country, Beneficiation_Upgrade) %>%
        rename(G = Beneficiation_Upgrade) %>%
        mutate(Material = "Beneficiation_Upgrade")) %>%
    rbind(
      d %>%
        filter(Potential == func(Potential)) %>%
        select(Name, Country, Potential) %>%
        rename(G = Potential) %>%
        mutate(Material = "Potential"))
}


grades_world <- data %>%
  grades() %>%
  mutate(Id = "World")

grades_rock <- data %>%
  group_by(Rock_type) %>%
  group_modify(~ grades(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = paste("World", " - ", Rock_type, sep = "")) %>%
  select(-Rock_type)

grades_companies <- data %>%
  group_by(Company) %>%
  group_modify(~ grades(.x)) %>%
  ungroup() %>%
  arrange(desc(Capacity_Content)) %>%
  as.data.frame() %>%
  rename(Id = Company)

output <- grades_world %>%
  rbind(grades_rock) %>%
  arrange(Id) %>%
  rbind(grades_companies %>% filter(Capacity_Content > 2)) %>%
  rbind(
    data %>%
      group_by(Name) %>%
      group_modify(~ grades(.x)) %>%
      ungroup() %>%
      as.data.frame() %>%
      rename(Id = Name)) %>%
  select(Id, everything())

output_limit <- data %>%
  select(
    Name, 
    Country,
    Rock_type,
    G_Ore, 
    G_PR, 
    G_Mineral, 
    G_Waste, 
    Beneficiation_Upgrade,
    Potential) %>%
  melt(id.vars = c("Name", "Country", "Rock_type")) %>%
  filter(!is.na(value)) %>%
  group_by(Rock_type, variable) %>%
  group_modify(~ {
    
    .x %>%
      filter(value == min(value)) %>%
      mutate(limit = "min") %>%
      rbind(
        .x %>%
          filter(value == max(value)) %>%
          mutate(limit = "max"))
  }) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(variable, Rock_type, limit)

write.csv(
  output,
  file = "bin/grades summary.csv",
  row.names = FALSE)

write.csv(
  output_limit,
  file = "bin/grades limits.csv",
  row.names = FALSE)

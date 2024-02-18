library(dplyr)
library(phosphateRock)
library(spatstat.geom)
library(reshape2)

data <- mining_complexes %>%
  select(Name, Rock_type, Capacity) %>%
  left_join(
    ore %>% rename(G_Ore = Value) %>% select(Name, G_Ore), 
    by = c("Name")) %>%
  left_join(
    PR %>% rename(G_PR = Value) %>% select(Name, G_PR), 
    by = c("Name")) %>%
  left_join(
    mineral %>% rename(G_Mineral = Value) %>% select(Name, G_Mineral), 
    by = c("Name")) %>%
  mutate(G_Gain = G_PR - G_Ore) %>%
  mutate(G_Potential =  G_Mineral - G_PR) %>%
  left_join(
    recovery_mass %>% select(Name, Value) %>% rename(R_mass = Value), 
    by = c("Name")) %>%
  left_join(
    recovery_mineral %>% select(Name, Value) %>% rename(R_mineral = Value), 
    by = c("Name")) %>%
  mutate(G_Waste = case_when(
    R_mass < 1 ~ (G_Ore - G_PR * R_mass) / (1 - R_mass),
    TRUE ~ 0)) %>%
  mutate(Capacity_Content = Capacity * G_PR / 100) %>%
  mutate(weight = Capacity_Content / sum(Capacity_Content, na.rm = TRUE)) %>%
  melt(id.vars = c(
    "Name", 
    "Rock_type", 
    "Capacity", 
    "Capacity_Content",
    "weight"))

stats <- function(d, y= NA) {
  
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
      q05 = weighted.quantile(value, ws, probs = c(0.05)),
      q95 = weighted.quantile(value, ws, probs = c(0.95)))
}

world <- data %>%
  group_by(variable) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = "World")

rock_types <- data %>%
  group_by(variable, Rock_type) %>%
  group_modify(~ stats(.x)) %>%
  ungroup() %>%
  as.data.frame() %>%
  mutate(Id = paste("World", " - ", Rock_type, sep = "")) %>%
  select(-Rock_type)

output <- world %>%
  rbind(rock_types) %>%
  select(Id, everything()) %>%
  arrange(desc(Capacity_Content))

write.csv(
  output,
  file = "bin/grades & recoveries summary.csv",
  row.names = FALSE)

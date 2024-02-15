library(cmocean)
library(dplyr)
library(ggnewscale)
library(ggpattern)
library(ggplot2)
library(ggstance)
library(gridExtra)
library(hrbrthemes)
library(patchwork)
library(phosphateRock)
library(scales)
library(stringr)
library(reshape2)
library(tidyr)

plot_grades <- function(data) {
  
  ybreaks <- data %>% pull(y) - 0.5
  ylabels <- data %>% pull(Label)
  
  plot <- ggplot() +
    labs(x = bquote('%'~P[2]*O[5])) +
    theme_ipsum(base_family = "Segoe UI") +
    theme(
      text = element_text(size = 18,  family = "Segoe UI"),
      panel.grid.minor = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title.x = element_text(
        size = 24, 
        hjust = 0.5, 
        margin = margin(t = 10)),
      axis.title.y = element_blank(),
      axis.text.x = element_text(
        size = 18, 
        color = "black", 
        margin = margin(t = 2, b = 0)),
      axis.text.y = element_text(
        size = 18, 
        color = "black", 
        margin = margin(r = 5)),
      axis.line = element_line(color = "black"),
      axis.ticks.x = element_line(size = 0.5),
      axis.ticks.y = element_line(size = 0.5),
      legend.position = "none",
      strip.text.x = element_text(
        size = 24, 
        hjust = 0.5, 
        face = "bold",
        margin = margin(b = 20)),
      strip.text.y = element_text(
        size = 24, 
        hjust = 0.5, 
        face = "bold",
        margin = margin(l = 20)),
      strip.placement = "outside",
      panel.spacing.x = unit(7, "lines"),
      panel.spacing.y = unit(2, "lines"),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    geom_rect(
      data = data,
      aes(
        xmin = xmin, 
        xmax = xmax, 
        ymin = y - 1, 
        ymax = y,
        fill = group),
      color = "black") +
    scale_x_continuous(
      expand = expansion(mult = c(0, 0), add = c(0.05, 0))) +
    scale_y_continuous(
      breaks = ybreaks,
      labels = ylabels,
      expand = c(0, 0)) +
    scale_fill_manual(
      breaks = c("mining", "beneficiation", "potential", "waste"),
      values = c("#88CCEE", "#CC6677", "#DDCC77", "#6699CC")) +
    facet_grid(
      Rock_type ~ cat,
      scales = "free_y",
      space = "free_y")
  
  png(
    filename = "bin/grades.jpg",
    units = "in",
    width = 12,
    height = 16,
    res = 1000)
  
  print(plot)
  dev.off()
}

data_raw <- mining_complexes %>%
  select(Name, Country, Region, Rock_type, Status) %>%
  mutate(
    Name = case_when(
      Name == "central Jordan (Al Hassa and Al Abiad)" ~ "Al Hassa and Al Abiad",
      TRUE ~ Name)) %>%
  left_join(
    ore %>% rename(Ore = Value) %>% select(Name, Ore), 
    by = c("Name")) %>%
  left_join(
    PR %>% rename(PR = Value) %>% select(Name, PR), 
    by = c("Name")) %>%
  left_join(
    mineral %>% rename(Mineral = Value) %>% select(Name, Mineral), 
    by = c("Name")) %>%
  left_join(
    recovery_mass %>% select(Name, Value) %>% rename(R_mass = Value), 
    by = c("Name")) %>%
  mutate(Waste = case_when(
    R_mass < 1 ~ (Ore - PR * R_mass) / (1 - R_mass),
    TRUE ~ NA)) %>%
  mutate(across(everything(), .fns = ~ replace_na(., 0))) %>%
  mutate(
    Label = case_when(
      Status == "operational" ~  paste(Name, " (", Country, ")", sep = ""),
      TRUE ~  paste(Name, "* (", Country, ")", sep = ""))) %>%
  mutate(Rock_type = str_to_title(Rock_type)) %>%
  arrange(Rock_type, PR) %>%
  mutate(y = row_number()) %>%
  select(-c(R_mass, Region, Country, Name, Status))

data <- data_raw %>%
  select(y, Label, Rock_type, Ore) %>%
  mutate(
    xmin = 0,
    group = "mining") %>%
  rename(xmax = Ore) %>%
  rbind(
    data_raw %>%
      select(y, Label, Rock_type, Ore, PR) %>%
      mutate(group = "beneficiation") %>%
      rename(
        xmin = Ore,
        xmax = PR)) %>%
  rbind(
    data_raw %>%
      select(y, Label, Rock_type, PR, Mineral) %>%
      mutate(group = "potential") %>%
      rename(
        xmin = PR,
        xmax = Mineral)) %>%
  mutate(cat = "Materials") %>%
  rbind(
    data_raw %>%
      select(y, Label, Rock_type, Waste) %>%
      mutate(
        xmin = 0,
        group = "waste",
        cat = "Waste") %>%
      rename(xmax = Waste))

plot_grades(data)
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
library(spatstat.geom)
library(stringr)
library(reshape2)
library(tidyr)


plot_materials <- function(data, companies, scales, boxes, bars) {
  
  plot <- ggplot() +
    theme_ipsum(base_family = "Segoe UI") +
    theme(
      aspect.ratio = 1,
      text = element_text(size = 18,  family = "Segoe UI"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
      plot.title = element_blank(),
      plot.subtitle = element_blank(),
      axis.title.x = element_blank(),
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
      legend.position = "bottom",
      legend.text = element_text(size = 18),
      legend.title = element_blank(),
      legend.box.spacing = unit(1.5, "lines"),
      strip.text.x = element_text(
        size = 24, 
        hjust = 0.5, 
        face = "bold",
        margin = margin(b = 20)),
      strip.text.y = element_text(size = 24, hjust = 0.5, face = "bold"),
      strip.placement = "outside",
      panel.spacing.x = unit(5, "lines"),
      panel.spacing.y = unit(2, "lines"),
      plot.margin = margin(t = 0, r = 0, b = 0, l = 0)) +
    geom_segment(
      data = boxes,
      aes(x = x, y = q83, xend = x, yend = 1),
      color = "gray20") +
    geom_segment(
      data = boxes,
      aes(x = x, y = 0, xend = x, yend = q17),
      color = "gray20") +
    geom_segment(
      data = boxes,
      aes(x = x - 0.1, y = 1, xend = x + 0.1, yend = 1),
      color = "gray20") +
    geom_segment(
      data = boxes,
      aes(x = x - 0.1, y = 0, xend = x + 0.1, yend = 0),
      color = "gray20") +
    geom_rect(
      data = boxes,
      aes(
        xmin = x - 0.1,
        xmax = x + 0.1,
        ymin = q17,
        ymax = q83),
      fill = "gray20",
      alpha = 0.5) +
    geom_segment(
      data = bars,
      aes(x = x, y = q17, xend = x, yend = q83),
      color = "#8c510a",
      size = 4) +
    geom_line(
      data = data,
      aes(x = x, y = y, group = Name),
      color = "#0072B2",
      alpha = 1) + 
    geom_line(
      data = companies,
      aes(x = x, y = y, group = Label, color = as.factor(z)),
      size = 3) +
    geom_segment(
      data = boxes,
      aes(
        x = x - 0.1,
        xend = x + 0.1,
        y = q50,
        yend = q50),
      color = "black", 
      size = 3) +
    geom_point(
      data = bars,
      aes(x = x, y = q50),
      shape = 21,
      color = "black",
      fill = "grey80",
      size = 5) +
    geom_text(
      data = scales %>% filter(y == 0),
      aes(x = x, y = 0, label = ylabel),
      size = 6,
      fontface = "italic",
      hjust = 0.5, 
      vjust = 1.5) +
    geom_text(
      data = scales %>% filter(y == 1),
      aes(x = x, y = 1, label = ylabel),
      size = 6,
      fontface = "italic",
      hjust = 0.5, 
      vjust = -0.5) +
    scale_x_continuous(
      breaks = data %>% arrange(x) %>% pull(x) %>% unique(),
      labels = data %>% arrange(x) %>% pull(variable) %>% unique(),
      expand = expansion(mult = c(0, 0), add = c(0.3, 0.5))) +
    scale_y_continuous(
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0), add = c(0.1, 0.1))) +
    scale_color_manual(
      breaks = companies %>% pull(z) %>% unique(),
      values = c(
        "#D55E00",
        "#56B4E9",
        "#E69F00", 
        "#CC79A7", 
        "#009E73", 
        "#F0E442"),
      labels = companies %>% pull(Label) %>% unique()) +
    facet_grid(. ~ Rock_type)
  
  png(
    filename = "bin/materials.jpg",
    units = "in",
    width = 12,
    height = 16,
    res = 300)
  
  print(plot)
  dev.off()
}

# GHG emissions (CO2-eq), kg per tonne of ore
# Representative points: PhosAgro (Russia) and OCP (Morocco)
# details on data in emissions.xlsx
co2_ore_ign <- 17.146 # source: PhosAgro annual reports
co2_ore_sed <- 46.643 # source: OCP annual reports and factsheets


data <- mining_complexes %>%
  select(Name, Capacity, Country, Rock_type) %>%
  mutate(
    Label = case_when(
      Name %in% c("Khouribga", "Gantour", "Boucraa") ~ "OCP (Morocco)",
      Name %in% c("Kola") ~ "PhosAgro (Russia)",
      Name %in% c("Bayovar", "Florida") ~ "Mosaic (US, Peru)",
      Name %in% c("Tapira", "Araxa/Patrocinio", "Catalao", "Cajati") ~ "Mosaic (Brazil)",
      Country == "Saudi Arabia" ~ "Ma'aden",
      Name %in% c("Haikou") ~ "YPH-Haikou (China)",
      TRUE ~ NA)) %>%
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
    Ore = 1 / (G_PR * R_mass),
    Waste = (1 - R_mass) / (G_PR * R_mass),
    P2O5_Loss_Mass = 1 / R_mineral - 1) %>%
  mutate(P2O5_Loss_Frac = P2O5_Loss_Mass / (1 + P2O5_Loss_Mass)) %>%
  mutate(
    GHG_kg = Ore * case_when(
      Rock_type == "sedimentary" ~ co2_ore_sed,
      TRUE ~ co2_ore_ign)) %>%
  mutate(Capacity_Content = Capacity * G_PR / 100) %>%
  mutate(weight = Capacity_Content / sum(Capacity_Content, na.rm = TRUE)) %>%
  mutate(
    Rock_type = case_when(
      Rock_type == "igneous" ~ "Igneous",
      TRUE ~ "Sedimentary")) %>%
  select(-c(G_Ore, G_PR, R_mass, R_mineral, Country, Capacity, P2O5_Loss_Mass)) %>%
  melt(id.vars = c("Name", "Label", "Rock_type", "Capacity_Content", "weight")) %>%
  group_by(variable) %>%
  mutate(
    ymin = min(value),
    ymax = max(value),
    y = (value - ymin) / (ymax - ymin)) %>%
  ungroup() %>%
  mutate(
    x = case_when(
      variable == "P2O5_Loss_Frac" ~ 3,
      variable == "Ore" ~ 1,
      variable == "Waste" ~ 2,
      TRUE ~ 4)) %>%
  as.data.frame() %>%
  mutate(
    variable = case_when(
      variable == "P2O5_Loss_Frac" ~ "P loss",
      variable == "GHG_kg" ~ "GHG",
      TRUE ~ variable))

companies <- data %>%
  filter(!is.na(Label)) %>%
  group_by(Label, variable) %>%
  group_modify(~ {
    
    ws <- .x %>% pull(weight)
    
    ymin <- .x %>% slice(1) %>% pull(ymin)
    ymax <- .x %>% slice(1) %>% pull(ymax)
    
    return (.x %>%
              summarise(
                x = head(x, 1),
                Rock_type = head(Rock_type, 1),
                Capacity_Content = sum(Capacity_Content),
                weight = sum(weight),
                mid = weighted.mean(value, ws)) %>%
              mutate(y = (mid - ymin) / (ymax - ymin)))
  }) %>%
  ungroup() %>%
  arrange(desc(Capacity_Content)) %>%
  group_by(variable) %>%
  mutate(z = row_number()) %>%
  ungroup() %>%
  as.data.frame()

scales <- data %>%
  group_by(variable) %>%
  group_modify(~ {
    data.frame(
      x = .x %>% slice(1:2) %>% pull(x),
      y = c(0, 1),
      ylabel = c(
        format(round(min(.x %>% pull(value)), 1), nsmall = 1),
        format(round(max(.x %>% pull(value)), 1), nsmall = 1)))
  }) %>%
  ungroup() %>%
  as.data.frame()

bars <- data %>%
  group_by(Rock_type, variable, x) %>%
  group_modify(~ {
    
    ymin <- .x %>% slice(1) %>% pull(ymin)
    ymax <- .x %>% slice(1) %>% pull(ymax)
    
    ws <- .x %>% pull(weight)
    
    if (all(is.na(ws))) {
      ws <- rep(1, length(ws))
    }
    
    q50 <- (weighted.median(.x %>% pull(value), ws) - ymin) / (ymax - ymin)
    q17 <- (weighted.quantile(.x %>% pull(value), ws, probs = c(0.17)) - ymin) / (ymax - ymin)
    q83 <- (weighted.quantile(.x %>% pull(value), ws, probs = c(0.83)) - ymin) / (ymax - ymin)
    
    return (data.frame(
      q50 = q50,
      q17 = q17,
      q83 = q83))
  }) %>%
  ungroup() %>%
  as.data.frame()

boxes <- data %>%
  group_by(variable, x) %>%
  group_modify(~ {
    
    ymin <- .x %>% slice(1) %>% pull(ymin)
    ymax <- .x %>% slice(1) %>% pull(ymax)
    
    ws <- .x %>% pull(weight)
    
    if (all(is.na(ws))) {
      ws <- rep(1, length(ws))
    }
    
    q50 <- (weighted.median(.x %>% pull(value), ws) - ymin) / (ymax - ymin)
    q17 <- (weighted.quantile(.x %>% pull(value), ws, probs = c(0.17)) - ymin) / (ymax - ymin)
    q83 <- (weighted.quantile(.x %>% pull(value), ws, probs = c(0.83)) - ymin) / (ymax - ymin)
    
    return (data.frame(
      q50 = q50,
      q17 = q17,
      q83 = q83))
  }) %>%
  ungroup() %>%
  as.data.frame()

plot_materials(data, companies, scales, boxes, bars)
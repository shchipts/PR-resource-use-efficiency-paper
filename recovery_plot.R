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


plot_recovery <- function(data_company, data_world) {
  
  ymax <- data_company %>% pull(y) %>% max() + 0.2
  
  area <- data_world %>%
    group_by(variable) %>%
    summarise(xmax = min(xmin))
  
  plot <- ggplot() +
    labs(x = "Recovery") +
    theme_ipsum(base_family = "Segoe UI") +
    theme(
      aspect.ratio = 1,
      text = element_text(size = 18,  family = "Segoe UI"),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank(),
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
      legend.position = "right",
      legend.text = element_text(size = 18),
      legend.spacing.y = unit(0.1, 'cm'),
      legend.title = element_blank(),
      legend.box.spacing = unit(3, "lines"),
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
    geom_rect_pattern(
      data = area,
      aes(xmin = 0, xmax = xmax, ymin = 0, ymax = ymax),
      fill = "white",
      pattern_fill = "grey70",
      pattern_color = "grey70",
      pattern_density = 0.002,
      pattern_spacing = 0.015) +
    geom_rect(
      data = data_world,
      aes(
        xmin = xmin, 
        xmax = xmax, 
        ymin = 0, 
        ymax = ymax,
        fill = as.factor(id)),
      color = "white") +
    geom_point(
      data = data_company,
      aes(x = mid, y = y - 0.5),
      shape = 21,
      color = "black",
      fill = "grey80",
      size = 5,
      stroke = 1.5) +
    geom_text(
      data = data_company,
      aes(x = mid, y = y - 0.5, label = ylabel),
      size = 6,
      fontface = "bold",
      hjust = 0.35, 
      vjust = -0.8) +
    scale_x_continuous(
      limits = c(0, 1),
      labels = scales::percent_format(accuracy = 1),
      expand = expansion(mult = c(0, 0), add = c(0, 0.03))) +
    scale_y_continuous(
      breaks = data_company %>% pull(y) - 0.5,
      labels = data_company %>% pull(Label),
      expand = expansion(mult = c(0, 0), add = c(0, 0))) +
    scale_fill_manual(
      breaks = 1:10,
      values = c(
        "#8e0152", 
        "#c51b7d", 
        "#de77ae", 
        "#f1b6da", 
        "#fde0ef", 
        "#e6f5d0", 
        "#b8e186", 
        "#7fbc41", 
        "#4d9221", 
        "#276419"),
      labels = data_world %>% pull(range) %>% unique()) +
    guides(fill = guide_legend(byrow = TRUE)) +
    facet_grid(. ~ variable)
  
  png(
    filename = "bin/recoveries.jpg",
    units = "in",
    width = 12,
    height = 16,
    res = 1000)
  
  print(plot)
  dev.off()
}


data <- mining_complexes %>%
  select(Name, Capacity, Country) %>%
  mutate(
    Label = case_when(
      Name %in% c("Khouribga", "Gantour", "Boucraa", "Meskala") ~ "OCP (Morocco)",
      Name %in% c("Kola") ~ "PhosAgro (Russia)*",
      Name %in% c("Bayovar", "Florida") ~ "Mosaic (US, Peru)",
      Name %in% c("Tapira", "Araxa/Patrocinio", "Catalao", "Cajati") ~ "Mosaic (Brazil)*",
      Country == "Saudi Arabia" ~ "Ma'aden",
      Name %in% c("Haikou") ~ "YPH-Haikou (China)",
      TRUE ~ NA)) %>%
  left_join(
    recovery_mass %>% rename(Mass = Value) %>% select(Name, Mass), 
    by = c("Name")) %>%
  left_join(
    recovery_mineral %>% rename(Mineral = Value) %>% select(Name, Mineral), 
    by = c("Name")) %>%
  left_join(
    PR %>% rename(G_PR = Value) %>% select(Name, G_PR), 
    by = c("Name")) %>%
  mutate(Capacity_Content = Capacity * G_PR / 100) %>%
  mutate(weight = Capacity_Content / sum(Capacity_Content, na.rm = TRUE)) %>%
  select(-c(G_PR, Country)) %>%
  melt(id.vars = c("Name", "Label", "Capacity", "Capacity_Content", "weight"))


companies <- data %>%
  filter(!is.na(Label)) %>%
  group_by(Label, variable) %>%
  group_modify(~ {
    
    ws <- .x %>% pull(weight)
    
    return (.x %>%
              summarise(
                Capacity = sum(Capacity),
                Capacity_Content = sum(Capacity_Content),
                weight = sum(weight),
                mid = weighted.mean(value, ws)))
  }) %>%
  ungroup() %>%
  as.data.frame() %>%
  arrange(Capacity_Content) %>%
  group_by(variable) %>%
  mutate(
    y = row_number(),
    ylabel = scales::percent_format(accuracy = 1)(mid)) %>%
  ungroup() %>%
  as.data.frame()

world <- data %>%
  group_by(variable) %>%
  group_modify(~ {
    
    ws <- .x %>% pull(weight)
    
    xs <- .x %>%
      summarise(
        q0 = weighted.quantile(value, ws, probs = c(0)),
        q10 = weighted.quantile(value, ws, probs = c(0.1)),
        q20 = weighted.quantile(value, ws, probs = c(0.2)),
        q30 = weighted.quantile(value, ws, probs = c(0.3)),
        q40 = weighted.quantile(value, ws, probs = c(0.4)),
        q50 = weighted.quantile(value, ws, probs = c(0.5)),
        q60 = weighted.quantile(value, ws, probs = c(0.6)),
        q70 = weighted.quantile(value, ws, probs = c(0.7)),
        q80 = weighted.quantile(value, ws, probs = c(0.8)),
        q90 = weighted.quantile(value, ws, probs = c(0.9)),
        q100 = weighted.quantile(value, ws, probs = c(1))) %>%
      melt() %>%
      pull(value)
    
    return (
      data.frame(
        xmin = head(xs, -1), 
        xmax = tail(xs, -1)) %>%
        mutate(id = row_number()) %>%
        cbind(
          range = c(
            "0-10",
            "10-20",
            "20-30",
            "30-40",
            "40-50",
            "50-60",
            "60-70",
            "70-80",
            "80-90",
            "90-100")))
  }) %>%
  ungroup() %>%
  as.data.frame()

plot_recovery(companies, world)
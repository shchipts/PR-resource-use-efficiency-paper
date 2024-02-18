library(dplyr)
library(phosphateRock)

data <- ore %>%
  mutate(Category = "Ore") %>%
  rbind(PR %>% mutate(Category = "PR")) %>%
  rbind(mineral %>% mutate(Category = "Mineral")) %>%
  rbind(recovery_mass %>% mutate(Category = "Mass recovery")) %>%
  rbind(recovery_mineral %>% mutate(Category = "Mineral recovery")) %>%
  group_by(Category, Confidence) %>%
  summarise(Count = n()) %>%
  ungroup() %>%
  as.data.frame() %>%
  group_by(Category) %>%
  mutate(Frequency = Count / sum(Count)) %>%
  ungroup() %>%
  as.data.frame()

write.csv(data, file = "bin/confidence.csv", row.names = FALSE)
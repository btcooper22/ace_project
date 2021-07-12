# Packages
require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)
require(forcats)

# Load data
results <- read_rds("analysis/boostrap_aggregate_additional.RDS") %>% 
  select(1:12) %>% 
  pivot_longer(1:12) %>% 
  na.omit() %>% 
  mutate(model = "additional") %>% 
  rbind(
    read_rds("analysis/boostrap_aggregate_original.RDS") %>% 
      select(1:12) %>% 
      pivot_longer(1:12) %>% 
      na.omit() %>% 
      mutate(model = "original")
  ) %>% 
  mutate(model = fct_rev(model))

# Plot
results %>% 
  ggplot(aes(value)) +
  geom_density(aes(fill = model),
               alpha = 0.8)+
  scale_fill_brewer(palette = "Set1",
                    name = "Model",
                    direction = -1)+
  theme_classic(20)+
  theme(legend.position = "top")+
  labs(x = "", y = "Density")+
  facet_wrap(~name, scales = "free")

results %>% 
  group_by(name, model) %>% 
  summarise(median = round(median(value),2),
            Q1 = round(quantile(value, 0.25),2),
            Q3 = round(quantile(value, 0.75),2)) %>% 
  mutate(value = paste(median, " [", Q1, ", ",
                       Q3, "]", sep = "")) %>% 
  select(metric = name, model, value) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  relocate(metric, original, additional)

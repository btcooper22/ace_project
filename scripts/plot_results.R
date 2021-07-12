# Packages
require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)

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
  )

# Plot
results %>% 
  ggplot(aes(value)) +
  geom_density(aes(fill = model),
               alpha = 0.8)+
  scale_fill_brewer(palette = "Set1",
                    name = "Model")+
  theme_classic(20)+
  theme(legend.position = "top")+
  labs(x = "", y = "Density")+
  facet_wrap(~name, scales = "free")

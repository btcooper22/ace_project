# Packages
require(readr)
require(dplyr)
require(ggplot2)
require(magrittr)
require(tidyr)
require(ggbump)
require(readxl)

# Cross-tabulation function
crosstab <- function(varname, .df = results)
{
  options(dplyr.summarise.inform=F)
  
  # Find
  .df  %>% 
    group_by_at(c("hosp_reqd", varname)) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = "hosp_reqd",
                values_from = "n") %>% 
    mutate(rate = `TRUE` / (`FALSE` + `TRUE`)) %>% 
    mutate(`TRUE (%)` = paste(`TRUE`, " (", round(rate * 100,1),
                           "%)", sep = "")) %>% 
    select(any_of(c(varname, "FALSE", "TRUE (%)"))) %>% 
    as.data.frame()
}

# Load data----
results <- read_xlsx("data/ace_data_LSOA.xlsx") %>% 
  select(pkid, LSOA, Outcome_for_care_episode) %>% 
  filter(Outcome_for_care_episode != "NA") %>% 
  mutate(hosp_reqd = grepl("Admitted", Outcome_for_care_episode)) %>% 
  select(-Outcome_for_care_episode)

# Load spatial stats
air_data <- read_csv("spatial/area_stats/bradford_air_LSOA.csv") %>%
  rename("LSOA" = lsoa11cd)
imd_data <- read_csv("spatial/area_stats/bradford_imd_LSOA.csv") %>%
  rename("LSOA" = lsoa11cd)

# Attach spatial variables
results %<>% 
  left_join(air_data) %>% 
  left_join(imd_data) %>% 
  na.omit()

# Air variables----

# Quick-cut air variables
level_labels <- c("VeryLow", "Low", "Medium",
                   "High", "VeryHigh")
results %<>% 
  mutate(NO2_cut = cut(NO2, 5, labels = level_labels),
         NOx_cut = cut(NOx, 5, labels = level_labels),
         PM10_cut = cut(PM10, 5, labels = level_labels),
         PM25_cut = cut(PM25, 5, labels = level_labels))

# Cross-tabulate
crosstab("NO2_cut")
crosstab("NOx_cut")
crosstab("PM10_cut")
crosstab("PM25_cut")

# Plot NO2
results %>% 
  select(hosp_reqd, NO2_cut, NOx_cut,
         PM10_cut, PM25_cut) %>% 
  pivot_longer(2:5) %>% 
  group_by(name, value) %>% 
  summarise(hosp_reqd = mean(hosp_reqd)) %>% 
  ungroup() %>% 
  mutate(NO2_cut = as.numeric(value)) %>% 
  ggplot(aes(NO2_cut, hosp_reqd * 100,
             colour = name))+
  geom_bump(size = 2)+
  scale_colour_brewer(palette = "Set1",
                      name = "Variable")+
  labs(x = "Variable category",
    y = "Hospitalisation rate")+
  theme_classic(20)+
  theme(legend.position = "top")+
  scale_x_continuous(labels = c("1" = "VeryLow", "2" = "Low", "3" = "Medium",
                              "4" = "High", "5" = "VeryHigh"))

# Construct new variables
results %<>% 
  mutate(NO2_high = NO2_cut %in% c("High", "VeryHigh"),
         NOx_high = NOx_cut %in% c("High", "VeryHigh"),
         PM10_high = PM10_cut %in% c("VeryLow", "Low") == FALSE,
         PM25_high = PM25_cut %in% c("VeryLow", "Low") == FALSE)


# Model
summary(glm(hosp_reqd ~ PM10_high,
            data = results, family = "binomial"))

summary(glm(hosp_reqd ~ NOx_high,
            data = results, family = "binomial"))

summary(glm(hosp_reqd ~ NO2_high,
            data = results, family = "binomial"))

# Previous model - mentions asthma, mentions salbutamol, referral from GP, moderate severity, abnormal resp. rate

# IMD variables-----


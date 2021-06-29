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

# Quick cut-and-summarise function
cut_bump <- function(varname, .df = results,
                     Q = FALSE)
{
  options(dplyr.summarise.inform=F)
  
  # Cut variable
  if(Q)
  {
    cut_df <- .df  %>% 
      mutate(cut_var = cut(get(varname), quantile(get(varname), qlist),
                           labels = c("VeryLow", "Low", "Medium",
                                      "High", "VeryHigh"), TRUE)) 
  }else
  {
    qlist <- seq(0, 1, 0.2)
    cut_df <- .df  %>% 
      mutate(cut_var = cut(get(varname), 5, labels = c("VeryLow", "Low", "Medium",
                                                       "High", "VeryHigh"))) 
  }

  # Calculate and print stats
  cut_df %>% 
    group_by_at(c("hosp_reqd", "cut_var")) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = "hosp_reqd",
                values_from = "n") %>% 
    mutate(rate = `TRUE` / (`FALSE` + `TRUE`)) %>% 
    mutate(`TRUE (%)` = paste(`TRUE`, " (", round(rate * 100,1),
                              "%)", sep = "")) %>% 
    select(any_of(c("cut_var", "FALSE", "TRUE (%)"))) %>% 
    as.data.frame() %>% 
    print()
  
  # Plot
  cut_df %>% 
    select(hosp_reqd, cut_var) %>% 
    group_by(cut_var) %>% 
    summarise(hosp_reqd = mean(hosp_reqd)) %>% 
    ungroup() %>% 
    mutate(cut_var = as.numeric(cut_var)) %>% 
    ggplot(aes(cut_var, hosp_reqd * 100),
           colour = "red")+
    geom_bump(size = 2)+
    labs(x = varname,
         y = "Hospitalisation rate")+
    theme_classic(20)+
    theme(legend.position = "top")+
    scale_x_continuous(labels = c("1" = "VeryLow", "2" = "Low", "3" = "Medium",
                                  "4" = "High", "5" = "VeryHigh"))
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

# Quantiles

cut_bump("NO2", Q = TRUE) # >=H
cut_bump("NOx", Q = TRUE) # >=H
cut_bump("PM10", Q = TRUE) # >=M
cut_bump("PM25", Q = TRUE) # ==VL

# IMD variables-----

# Adult Skills Sub-domain Score
cut_bump("ASScore") # >=M
cut_bump("ASScore", Q = TRUE) # >=M

# Barriers to Housing and Services Score
cut_bump("BHSScore") # None
cut_bump("BHSScore", Q = TRUE) # None

# Crime Score
cut_bump("CriScore")
cut_bump("CriScore", Q = TRUE) # ==VH

# Children and Young People Sub-domain Score
cut_bump("CYPScore") # >=H
cut_bump("CYPScore", Q = TRUE) # >=H

# Education, Skills and Training Score
cut_bump("EduScore") # Continuous/==VL
cut_bump("EduScore", Q = TRUE) # >=M

# Employment Score (rate)
cut_bump("EmpScore") # None
cut_bump("EmpScore", Q = TRUE) # >=H

# Living Environment Score
cut_bump("EnvScore") # None
cut_bump("EnvScore", Q = TRUE) # None

# Geographical Barriers Sub-domain Score
cut_bump("GBScore") # Continuous/>=M
cut_bump("GBScore", Q = TRUE) # None

# Health Deprivation and Disability Score
cut_bump("HDDScore") # ==VL
cut_bump("HDDScore", Q = TRUE) # >= VH

# Income Deprivation Affecting Children Index (IDACI) Score (rate)
cut_bump("IDCScore") # >=H
cut_bump("IDCScore", Q = TRUE) # >=H

# Income Deprivation Affecting Older People (IDAOPI) Score (rate)
cut_bump("IDOScore") # None
cut_bump("IDOScore", Q = TRUE) # None

# Index of Multiple Deprivation (IMD) Score
cut_bump("IMDScore") # >=H
cut_bump("IMDScore", Q = TRUE) # >=H

# Income Score (rate)
cut_bump("IncScore") # None
cut_bump("IncScore", Q = TRUE) # None

# Indoors Sub-domain Score
cut_bump("IndScore") # None
cut_bump("IndScore", Q = TRUE) # None

# Outdoors Sub-domain Score
cut_bump("OutScore") # >=H
cut_bump("OutScore", Q = TRUE) # None

# Wider Barriers Sub-domain Score
cut_bump("WBScore") # ==VL
cut_bump("WBScore", Q = TRUE) # None


# Construct new variables----

# List variables worth investigation

results %<>% 
  mutate(NO2_high = NO2_cut %in% c("High", "VeryHigh"),
         NOx_high = NOx_cut %in% c("High", "VeryHigh"),
         PM10_high = PM10_cut %in% c("VeryLow", "Low") == FALSE,
         PM25_high = PM25_cut %in% c("VeryLow", "Low") == FALSE)


# Model
# Previous model - mentions asthma, mentions salbutamol, referral from GP, moderate severity, abnormal resp. rate

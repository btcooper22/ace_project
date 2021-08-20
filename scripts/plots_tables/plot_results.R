# Packages
require(readr)
require(ggplot2)
require(dplyr)
require(tidyr)
require(forcats)
require(tibble)
require(janitor)
require(bayestestR)
require(stringr)
source("functions/inverse_logit.R")
require(xtable)
require(RColorBrewer)
require(snakecase)

boolean_clean <- function(x)
{
  return(
    x == "y"
  )
}

# Load data
results <- read_csv("data/ace_data_cooper_final.csv")

# Check for duplicates
duplicated_rowid <- results %>% 
  get_dupes(person_id, date_referred) %>% 
  group_by(person_id, date_referred) %>% 
  slice_max(rowid) %>% 
  ungroup() %>% 
  select(rowid) %>% 
  deframe()

# Cleaning results
results %<>%
  # Remove duplicaes
  filter(!rowid %in% duplicated_rowid) %>% 
  # Clean boolean values
  mutate(hospital_reqd = hospital_reqd == 1,
         safeguarding = boolean_clean(safeguarding),
         food_allergy = boolean_clean(food_allergy),
         drug_allergy = boolean_clean(drug_allergy),
         other_allergy = boolean_clean(other_allergy),
         ox_sat_low = boolean_clean(ox_sat_low),
         meets_ace_criteria = boolean_clean(meets_ace_criteria),
         mentions_asthma = boolean_clean(mentions_asthma),
         mentions_salbutamol = boolean_clean(mentions_salbutamol)) %>% 
  # Scale numeric variables
  mutate(ox_sat = scale(ox_sat),
         temp = scale(temp),
         resp_rate = scale(resp_rate),
         heart_rate = scale(heart_rate)) %>% 
  # Add new variables
  mutate(referral_from_gp = referral_from == "gp",
         abnormal_resp_rate = apls_resp_rate_cat != "normal",
         gut_feeling_abnormal = gut_feeling != "well",
         abnormal_heart_rate = ace_heart_rate_cat != "normal")

# Model results----
model_results <- read_rds("analysis/boostrap_aggregate_additional.RDS") %>% 
  select(13:20) %>% 
  pivot_longer(1:8) %>% 
  na.omit() %>% 
  mutate(model = "additional") %>% 
  rbind(
    read_rds("analysis/boostrap_aggregate_original.RDS") %>% 
      select(13:21) %>% 
      pivot_longer(1:9) %>% 
      na.omit() %>% 
      mutate(model = "original")
  )

# Table of model results
coef_table <- model_results %>% 
  group_by(name, model) %>% 
  summarise(coeficient = paste(median(round(value,2)), " [",
                               hdi(round(value,2))$CI_low, ", ",
                               hdi(round(value,2))$CI_high, "]", sep = "")) %>% 
  arrange(desc(model)) %>% 
  # Sort names
  mutate(name = str_split(name, "TRUE", 2, TRUE)[,1],
         name = str_split(name, "moderate", 2, TRUE)[,1]) %>% 
  ungroup()

# N and hosp for variables included
n_hosp_table <- results %>%
  select(any_of(c("hospital_reqd", coef_table$name))) %>% 
  select(-heart_rate, -ox_sat) %>% 
  mutate(illness_severity = illness_severity == "moderate") %>% 
  pivot_longer(2:11) %>% 
  group_by(name, hospital_reqd) %>% 
  na.omit() %>% 
  summarise(ocurrance = paste(sum(value), " (",
                              round(mean(value) * 100, 1),
                              "%)", sep = "")) %>% 
  pivot_wider(names_from = "hospital_reqd",
              values_from = "ocurrance") %>% 
  rename(`Discharged from ACE` = "FALSE",
         `Admitted to Hospital` = "TRUE") %>% 
  rbind(
    results %>%
      select(hospital_reqd, heart_rate, ox_sat) %>% 
      mutate(ox_sat = ox_sat * attr(ox_sat, "scaled:scale") + attr(ox_sat, "scaled:center"),
             heart_rate = heart_rate * attr(heart_rate, "scaled:scale") + attr(heart_rate, "scaled:center")) %>% 
      pivot_longer(2:3) %>% 
      group_by(name, hospital_reqd) %>% 
      summarise(ocurrance = paste(median(round(value,2)), " [",
                                  quantile(round(value,2), 0.25), ", ",
                                  quantile(round(value,2), 0.75), "]", sep = "")) %>% 
      pivot_wider(names_from = "hospital_reqd",
                  values_from = "ocurrance") %>% 
      rename(`Discharged from ACE` = "FALSE",
             `Admitted to Hospital` = "TRUE")
  ) %>% 
  rbind(tibble(name = "(Intercept)", `Discharged from ACE` = "---",
                   `Admitted to Hospital` = "---"))

# Built names translation
names_translation <- tibble(name = unique(n_hosp_table$name),
                            newname = c("Abnormal respiratory rate","Any eczema diagnosis",
                                        "Food allergy","High local NO2", "Moderate illness severity",
                                        "Mentions asthma","Mentions salbutamol", "Any pneumonia diagnosis",
                                        "Referral from GP", "Slow bronchodilators in last year",
                                        "Heart rate", "Oxygen saturation", "Intercept"))

# Combine and print
coef_table %>% 
  left_join(n_hosp_table) %>% 
  left_join(names_translation) %>% 
  select(-name, -model) %>% 
  relocate(newname, `Discharged from ACE`, 
           `Admitted to Hospital`, coeficient) %>% 
  rename(Variable = newname,
         Coefficient = coeficient) %>% 
  xtable() %>% 
  print(include.rownames = FALSE)

# Plot
pal <- brewer.pal(12, "Set3")
pal[2] <- "#8f7068"

model_results %>% 
  left_join(tibble(name = unique(model_results$name),
                   newname = c("Intercept", "Any eczema diagnosis", "High local NO2",
                               "Mentions asthma",  "Oxygen saturation", "Any pneumonia diagnosis",
                               "Referral from GP", "Slow bronchodilators in last year", "Abnormal respiratory rate",
                               "Food allergy", "Heart rate", "Moderate illness severity",
                               "Mentions salbutamol"))) %>% 
  select(name = newname, value, model) %>% 
  filter(name != "Intercept") %>% 
  mutate(model = fct_rev(model),
         model = fct_recode(model, Original = "original",
                            Additional = "additional")) %>% 
  ggplot(aes(x = value, fill = name,
             colour = name))+
  geom_density(alpha = 0.5, size = 2)+
  facet_wrap(~model, nrow = 2)+
  theme_classic(20)+
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())+
  scale_fill_manual(values = pal, name = "")+
  scale_colour_manual(values = pal, name = "")+
  labs(x = "Coefficient value",
       y = "")+
  scale_x_continuous(breaks = seq(-1.5,2.5,0.5))

ggsave("written_work/report/images/additional_coefs.png",
       height = 10, width = 13)

# Model validation----

# Load data
model_out <- read_rds("analysis/boostrap_aggregate_additional.RDS") %>% 
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

# Table
model_out %>% 
  group_by(name, model) %>% 
  summarise(median = round(median(value),2),
            Q1 = round(quantile(value, 0.25),2),
            Q3 = round(quantile(value, 0.75),2)) %>% 
  mutate(value = paste(median, " [", Q1, ", ",
                       Q3, "]", sep = "")) %>% 
  select(metric = name, model, value) %>% 
  pivot_wider(names_from = model,
              values_from = value) %>% 
  relocate(metric, original, additional) %>% 
  mutate(metric = case_when(
    metric %in% c("AUC", "F1") ~ metric,
    metric == "sensitivity_recall" ~ "Sensitivity",
    is.character(metric) ~ to_any_case(metric, "title")
  )) %>% 
  mutate(ratio = as.numeric(str_split(additional, " ", 2, TRUE)[,1])/
         as.numeric(str_split(original, " ", 2, TRUE)[,1])) %>% 
  xtable() %>% 
  print(include.rownames = FALSE)
  
           
# Plot
model_out %>% 
  mutate(name = case_when(
    name %in% c("AUC", "F1") ~ name,
    name == "sensitivity_recall" ~ "Recall",
    name == "detection_prevalence" ~ "Prevalence",
    is.character(name) ~ to_any_case(name, "title")
  ),
  model = to_any_case(as.character(model), "title"),
  model = fct_rev(model)) %>% 
  ggplot(aes(value)) +
  geom_density(aes(fill = model,
                   colour = model),
               alpha = 0.5, size = 2)+
  scale_fill_manual(values = c("#ff7f00", "#984ea3"),
                    name = "Model")+
  scale_colour_manual(values = c("#ff7f00", "#984ea3"),
                    name = "Model")+
  theme_classic(20)+
  theme(legend.position = "top",
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.line.y = element_blank())+
  labs(x = "Performance metric value", y = "")+
  facet_wrap(~name, scales = "free")

ggsave("written_work/report/images/additional_performance.png",
       height = 10, width = 12)

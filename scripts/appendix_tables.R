# Packages
require(readr)
require(caret)
require(glmnet)
require(foreach)
require(dplyr)
require(doParallel)
require(tools)
require(pROC)
require(ResourceSelection)
require(ggplot2)
require(tidyr)
require(tibble)
require(magrittr)
require(ggpol)
require(matrixStats)
require(bayestestR)
require(stringr)
require(janitor)
require(purrr)
require(progressr)
require(xtable)
handlers(global = TRUE)

source("functions/inverse_logit.R")

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

# Comorbidities
results %>%
  select(4, 41:47) %>% 
  pivot_longer(2:8) %>% 
  group_by(name) %>% 
  na.omit() %>% 
  summarise(ocurrance = sum(value),
            hosp = sum(hospital_reqd & value)) %>% 
  arrange(desc(ocurrance)) %>% 
  mutate(ocurrance = paste(ocurrance, " (",
                              (round((ocurrance / 446) * 100,1)),
                              "%)", sep = ""),
         hosp = paste(hosp, " (",
                           (round((hosp / 78) * 100,1)),
                           "%)", sep = ""),
         timeframe = str_split(name, "_", 2, TRUE)[,2],
         condition = str_split(name, "_", 2, TRUE)[,1]) %>%
  relocate(condition, timeframe, ocurrance, hosp) %>% 
  select(-name) %>% 
  xtable() %>% 
  print(include.rownames = FALSE)

        
# Demographics    
demographics <- read_csv("data/cBradford/demographics.csv") %>% 
  right_join(results) %>% 
  na.omit
table(demographics$gender_source_value)

tabyl(demographics, gender_source_value)
summary(glm(hospital_reqd ~ (gender_source_value == "M"),
            binomial, demographics))

demographics %<>% 
  mutate(ethnic_group = case_when(
    ethnicity_concept_id %in% c(46286810) ~ "White",
    ethnicity_concept_id %in% c(46285832) ~ "Pakistani",
    ethnicity_concept_id %in% c(46285827, 46285828, 46285829,
                                46285830, 46285836, 46285837,
                                46285839, 46286811,  0,
                                46285831, 46285833, 46285835) ~ "Other"
  ))

tabyl(demographics, ethnic_group)
summary(glm(hospital_reqd ~ (ethnic_group == "Pakistani"), binomial, demographics))
summary(glm(hospital_reqd ~ (ethnic_group == "White"), binomial, demographics))
summary(glm(hospital_reqd ~ (ethnic_group == "Other"), binomial, demographics))

demographics %>%
  select(10, 83, 5) %>% 
  mutate(white = ethnic_group == "White",
         pakistani = ethnic_group == "Pakistani",
         other = ethnic_group == "Other",
         male = gender_source_value == "M") %>% 
  select(-ethnic_group, -gender_source_value) %>% 
  pivot_longer(2:5) %>% 
  group_by(name) %>% 
  na.omit() %>% 
  summarise(ocurrance = sum(value),
            hosp = sum(hospital_reqd & value)) %>% 
  arrange(desc(ocurrance)) %>% 
  mutate(ocurrance = paste(ocurrance, " (",
                           (round((ocurrance / 436) * 100,1)),
                           "%)", sep = ""),
         hosp = paste(hosp, " (",
                      (round((hosp / 78) * 100,1)),
                      "%)", sep = "")) %>%
  xtable() %>% 
  print(include.rownames = FALSE)

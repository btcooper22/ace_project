# Packages
require(readr)
require(dplyr)
require(magrittr)
require(readxl)

# Load raw data
raw_df <- read_csv("data/ace_data_linked.csv")

# Select columns to take forward
results <- raw_df %>% 
  select(person_id, `Hospital Required?`,
         `Referral from`, Age, Allergy,
         `Date of referral`, `Time of referral`,
         `Severity of Illness`, `Activity Level of Child`,
         `\"Gut Feeling\" of Referrer`, `Oxygen saturations`,
         `Respiratory rate`, `Heart rate`,
         `Safeguarding issues`) %>% 
  filter(!is.na(person_id))

free_text_fields <- raw_df %>% 
  select(`Background (Medical Histroy)`, `Referral's examination summary`,
         `Recommendation`, `Planned_Huddle_Comments`, `Unplanned_Huddle_Comments`,
         Comments)

# Rename columns
names(results) <- c("id","hosp_reqd","referral_source","age",
                    "allergy","season","time","severity",
                    "activity","gut_feeling","ox_sat","resp_rate",
                    "hr","safeguarding")
names(free_text_fields) <- c("medical_history","examination","recommendation",
                             "planned_huddle_comments", "unplanned_huddle_comments",
                             "general_comments")

# Assess data quality----

# Hospital required
results$hosp_reqd <- results$hosp_reqd == "Y"
mean(results$hosp_reqd)

# Referral source
table(results$referral_source)
results %<>% 
  mutate(referral_from_gp = referral_source == "GP") %>% 
  select(-referral_source)

# Allergies
table(results$allergy)
results %<>% 
  mutate(food_allergy = grepl("Food", allergy),
         drug_allergy = grepl("Drug", allergy),
         other_allergy = grepl("Other", allergy)) %>% 
  select(-allergy)

# Severity
results %<>% 
  mutate(severity = case_when(severity == "None" ~ "Mild",
                              is.character(severity) ~ severity))

# Activity
results %<>% 
  mutate(activity = case_when(activity == "None" ~ "usual",
                              is.character(activity) ~ tolower(activity)))

# Severity
results %<>% 
  mutate(gut_feeling = case_when(gut_feeling == "None" ~ "well",
                              is.character(gut_feeling) ~ gut_feeling))

# Physiology
results %<>% 
  filter(ox_sat != "None" & hr != "None" &
           resp_rate != "None") %>% 
  filter(hr != "125-130" & ox_sat != "122") %>%
  mutate(ox_sat = as.numeric(ox_sat),
         hr = as.numeric(hr),
         resp_rate = as.numeric(resp_rate))

# Safeguarding
results$safeguarding <- results$safeguarding == "Y"


# Attaching spatial data----

# Load spatial stats
air_data <- read_csv("spatial/area_stats/bradford_air_LSOA.csv") %>%
  rename("LSOA" = lsoa11cd)
imd_data <- read_csv("spatial/area_stats/bradford_imd_LSOA.csv") %>%
  rename("LSOA" = lsoa11cd)

# Load LSOA conversion and add spatial data
ace_data_LSOA <- read_excel("data/ace_data_LSOA.xlsx")[,c(2,4)] %>% 
  left_join(air_data) %>% 
  left_join(imd_data) %>% 
  filter(!duplicated(pkid))

# Add to main results
results %<>% 
  mutate(id = as.character(id)) %>% 
  left_join(ace_data_LSOA,
            by = c("id" = "pkid"))

# Write
write_csv(results, "data/full_results.csv")
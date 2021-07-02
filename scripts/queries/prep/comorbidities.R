# Packages
require(dplyr)
require(magrittr)
require(readr)
require(tibble)
require(doParallel)

# Load person_id list from ACE data
person_id_list <- read_csv("data/ace_data_080621.csv",
         col_types = cols()) %>% 
  select(pkid) %>% 
  deframe() %>% 
  unique() %>% 
  na.omit() %>% 
  as.numeric()

# Load connected yorkshire
source("scripts/functions/load_connected_yorkshire.R")

# Pull out conditions
condition_occurances <- CY$condition_occurrence %>% 
  filter(person_id %in% person_id_list)

condition_concepts <- CY$concept %>% 
  filter(domain_id == "Condition")

conditions <- condition_occurances %>% 
  left_join(condition_concepts, 
            by = c("condition_concept_id" = "concept_id")) %>% 
  select(person_id, condition_concept_id,
         concept_name, condition_start_date) %>% 
  collect()

# Load conditions dictionary
D_conditions <- condition_concepts %>%
  collect() %>% 
  select(concept_id, concept_name)

# Write conditions to file for easier searching
# D_conditions %>% 
#   select(concept_id, concept_name) %>% 
#   write_csv("data/cBradford/conditions.csv")

# List comorbidities (from DOI = 10.1183/09031936.00121308)
comorbs <- c("rhinitis" = "rhinitis", "chronic_sinusitis" = "chronic sinusitis",
             "hyperventilation" = "hyperventilation",
             "COPD" = "COPD", "COPD" = "chronic obstructive pulmonary",
             "COPD" = "chronic obstructive airway", "respiratory_infection" = "influenza",
             "respiratory_infection" = "common cold", "respiratory_infection" = "asthma",
             "respiratory_infection" = "hay fever", "respiratory_infection" = "sinusitis",
             "respiratory_infection" = "bronchitis", "respiratory_infection" = "croup",
             "respiratory_infection" = "strep throat", "respiratory_infection" = "Streptococcal pharyngitis",
             "respiratory_infection" = "pneumonia", "eczema" = "Atopic dermatitis", "eczema" = "eczema",
             "ABPA" = "Allergic bronchopulmonary aspergillosis", "bronchiectasis" = "bronchiectasis",
             "OSA" = "obstructive sleep apnea", "OSA" = "obstructive sleep apnoea", "GERD" = "GERD",
             "GERD" = "reflux", "obesity" = "obes", "nicotine" = "nicotine dep")

# Extract relevant codes
cl <- makeCluster(8)
registerDoParallel(cl)

comorbidity_codes <- foreach(i = 1:length(comorbs),
        .combine = "rbind", .packages = c("dplyr", "tibble")) %dopar%
  {
    print(i)
    
    # Filter to match search term
    concept_id_list <- D_conditions %>% 
      filter(grepl(comorbs[i], concept_name,
                   ignore.case = TRUE))
    
    # Build output
    data.frame(comorbidity = names(comorbs)[i],
               term = comorbs[i],
               name = concept_id_list$concept_name,
               concept_id = concept_id_list$concept_id)
  }
stopCluster(cl)

# Filter duplicates
comorbidity_codes %<>% 
  filter(!duplicated(concept_id))
comorb_codes <- comorbidity_codes$concept_id

# Build full query
comorbidity_query <- CY$condition_occurrence %>% 
  filter(person_id %in% person_id_list) %>% 
  left_join(condition_concepts <- CY$concept %>% 
              filter(domain_id == "Condition"), 
            by = c("condition_concept_id" = "concept_id")) %>% 
  select(person_id, condition_concept_id,
         concept_name, condition_start_date) %>% 
  filter(condition_concept_id %in% comorb_codes)

# Test query (R)
test_df <- comorbidity_query %>% 
  collect()

# (COPY + PASTE QUERY BELOW INTO SQL SCRIPT)
show_query(comorbidity_query)

# Load
loaded_q <- read_file("scripts/queries/cooper_comorbidity_query.sql")

# Test query (BQ)
test_sql <- bq_dataset_query(bq_dataset("yhcr-prd-phm-bia-core", "CY_CDM_V1_50k_Random"),
                             query = loaded_q, billing = "yhcr-prd-phm-bia-core")

# Load in query results
con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

query_results <- tbl(con, "tbl_cooper_comorbity_query") %>% 
  collect()

# Add metadata
comorb_results <- query_results %>% 
  left_join(comorbidity_codes,
            by = c("concept_name" = "name",
                   "condition_concept_id" = "concept_id"))

# Write results
comorb_results %>% 
  write_csv("data/cBradford/comorbidities.csv")

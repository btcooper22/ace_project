# Packages
require(dplyr)
require(magrittr)
require(readr)
require(tibble)
require(doParallel)
require(tidyr)
require(xtable)

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

# Pull out drug codes and exposures
drug_occurances <- CY$drug_exposure %>%
  filter(person_id %in% person_id_list)

drug_concepts <- CY$concept %>% 
  filter(domain_id == "Drug")

drugs <- drug_occurances %>% 
  left_join(drug_concepts, 
            by = c("drug_concept_id" = "concept_id")) %>% 
  select(person_id, drug_concept_id,
         concept_name, drug_exposure_start_date) %>% 
  collect()

# List prescriptions
prescrip <- c("fast_bronchodilator" = "albuterol", "fast_bronchodilator" = "salbutamol",
              "fast_bronchodilator" = "ipratropium", "fast_bronchodilator" = "aminophylline",
              "fast_bronchodilator" = "theophylline", 
              "slow_bronchodilator" = "montelukast", "slow_bronchodilator" = "salmeterol",
              "slow_bronchodilator" = "formoterol", "slow_bronchodilator" = "vilanterol",
              "slow_bronchodilator" = "tiotropium", "slow_bronchodilator" = "aclidinium", 
              "slow_bronchodilator" = "glycopyrronium", "slow_bronchodilator" = "olodaterol",
              "slow_bronchodilator" = "Bambuterol", "slow_bronchodilator" = "Indacaterol",
              "antihistamine" = "cetirizine", "antihistamine" = "cromolyn sodium",
              "antihistamine" = "chlorphenamine", "antihistamine" = "hydroxyzine", 
              "antihistamine" = "fexofenadine", "antihistamine" = "loratadine",
              "antihistamine" = "promethazine", "prednisolone" = "prednisolone",
              "other_steroid" = "Beclometasone", "slow_bronchodilator" =  "budesonide", 
              "other_steroid" = "fluticasone" 
)

# Extract relevant codes
prescription_codes <- foreach(i = 1:length(prescrip),
                             .combine = "rbind") %do%
  {
    print(i)
    
    # Filter to match search term
    concept_id_list <- bq_dataset_query(
          bq_dataset("yhcr-prd-phm-bia-core", "CY_CDM_V1_50k_Random"),
          paste(
            "SELECT *
             FROM (SELECT *
             FROM `concept`
             WHERE (`domain_id` = 'Drug')) `q01`
             WHERE (REGEXP_CONTAINS(`concept_name`, '(?i)",
            prescrip[i],
            "'))",
            sep = ""
          )
        ) %>% 
        bq_table_download()
      
    # Build output
    suppressWarnings(
    data.frame(class = names(prescrip)[i],
               drug = prescrip[i],
               name = concept_id_list$concept_name,
               concept_id = concept_id_list$concept_id))
  }

# Isolate flu vaccine codes
flu_vaccine <- drug_concepts %>% 
  filter(grepl(concept_name, "(?i)vaccine")) %>% 
  filter(grepl(concept_name, "(?i)influenza")) %>% 
  collect() %>% 
  select(concept_name,
         concept_id) %>% 
  rename(name = "concept_name") %>% 
  mutate(class = "flu_vaccine",
         drug = "flu_vaccine")

# Build full database
codes_df <- prescription_codes %>% 
  rbind(flu_vaccine) %>% 
  filter(!duplicated(concept_id))
prescription_codes <- codes_df$concept_id

# Build full query
prescription_query <- CY$drug_exposure %>% 
  filter(person_id %in% person_id_list) %>% 
  left_join(drug_concepts, 
            by = c("drug_concept_id" = "concept_id")) %>% 
  select(person_id, drug_concept_id,
         concept_name, drug_exposure_start_date,
         refills, quantity, days_supply,
         dose_unit_source_value) 

# Test query (R)
test_df <- prescription_query %>% 
  collect() %>% 
  filter(drug_concept_id %in% prescription_codes) %>% 
  left_join(codes_df, by = c("drug_concept_id" = 
                               "concept_id",
                             "concept_name" = "name"))

# (COPY + PASTE QUERY BELOW INTO SQL SCRIPT)
show_query(prescription_query)

# Load
loaded_q <- read_file("scripts/queries/cooper_prescription_query.sql")

# Test query (BQ)
test_sql <- bq_dataset_query(bq_dataset("yhcr-prd-phm-bia-core", "CY_CDM_V1_50k_Random"),
                             query = loaded_q, billing = "yhcr-prd-phm-bia-core")

# Load in query results
con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

query_results <- tbl(con, "tbl_cooper_prescription_qry") %>% 
  collect() %>% 
  filter(drug_concept_id %in% prescription_codes) %>% 
  left_join(codes_df, by = c("drug_concept_id" = 
                               "concept_id",
                             "concept_name" = "name"))

# Write results
query_results %>% 
  write_csv("data/cBradford/prescriptions.csv")

# Clean for tabulation
prescriptions_filtered <- query_results %>% 
  filter(class %in% c("antihistamine", "fast_bronchodilator",
                      "prednisolone", "slow_bronchodilator")) %>% 
  rownames_to_column("row_id")

final_patients <- read_csv("data/ace_data_cooper_final.csv")[,1:4]

# Filter to right patients and before ACE acceptance
table_df <- foreach(i = 1:nrow(final_patients), .combine = "rbind") %do%
  {
    # Isolate
    incidence_prescription <-  prescriptions_filtered %>% 
      filter(person_id == final_patients$person_id[i] &
               drug_exposure_start_date < final_patients$date_referred[i]) %>% 
      select(class) %>% deframe()

    # Flag
    data.frame(
      antihistamine = "antihistamine" %in% incidence_prescription,
      fast_bronchodilator = "fast_bronchodilator" %in% incidence_prescription,
      slow_bronchodilator = "slow_bronchodilator" %in% incidence_prescription,
      prednisolone = "prednisolone" %in% incidence_prescription
    )
  }


table_df %>% 
  pivot_longer(1:4) %>% 
  group_by(name) %>% 
  summarise(occurence = sum(value)) %>% 
  arrange(desc(occurence)) %>% 
  mutate(occurence = paste(occurence, " (",
                           round((occurence/447)*100,1),
                           "%)", sep= ""))%>% 
  xtable() %>% 
  print(include.rownames = FALSE)

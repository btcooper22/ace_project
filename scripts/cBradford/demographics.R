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

# Pull out patients
patients <- CY$person %>% 
  filter(person_id %in% person_id_list) %>% 
  select(person_id, gender_concept_id,
         race_concept_id, ethnicity_concept_id,
         gender_source_value, race_source_value,
         ethnicity_source_value) %>%
  collect()

# Construct query
demographics_query <- CY$person %>% 
  filter(person_id %in% person_id_list) %>% 
  select(person_id, gender_concept_id,
         race_concept_id, ethnicity_concept_id,
         gender_source_value, race_source_value,
         ethnicity_source_value)

# Test query (R)
test_df <- demographics_query %>% 
  collect()

# (COPY + PASTE QUERY BELOW INTO SQL SCRIPT)
show_query(demographics_query)

# Load
loaded_q <- read_file("scripts/queries/cooper_demographics_query.sql")

# Test query (BQ)
test_sql <- bq_dataset_query(bq_dataset("yhcr-prd-phm-bia-core", "CY_CDM_V1_50k_Random"),
                             query = loaded_q, billing = "yhcr-prd-phm-bia-core")


# Load in query results
con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

query_results <- tbl(con, "cooper_demographics_query_20210707") %>% 
  collect()

# Write results
query_results %>% 
  write_csv("data/cBradford/demographics.csv")

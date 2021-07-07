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

# Pull out visits
visit_occurances <- CY$visit_occurrence %>%
  filter(person_id %in% person_id_list)

visit_concepts <- CY$concept %>% 
  filter(domain_id == "Visit")

visits <- visit_occurances %>% 
  left_join(visit_concepts, 
            by = c("visit_concept_id" = "concept_id")) %>% 
  select(person_id, visit_concept_id,
         concept_name, visit_start_date,
         visit_end_date) %>% 
  collect()

# Construct query
visit_query <- visit_occurances %>% 
  left_join(visit_concepts, 
            by = c("visit_concept_id" = "concept_id")) %>% 
  select(person_id, visit_concept_id,
         concept_name, visit_start_date,
         visit_end_date) 

# Test query (R)
test_df <- visit_query %>% 
  collect()

# (COPY + PASTE QUERY BELOW INTO SQL SCRIPT)
show_query(visit_query)

# Load
loaded_q <- read_file("scripts/queries/cooper_visit_query.sql")

# Test query (BQ)
test_sql <- bq_dataset_query(bq_dataset("yhcr-prd-phm-bia-core", "CY_CDM_V1_50k_Random"),
                             query = loaded_q, billing = "yhcr-prd-phm-bia-core")

# Load in query results
con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

query_results <- tbl(con, "cooper_VISIT_query_20210707") %>% 
  collect()

# Write results
query_results %>% 
  write_csv("data/cBradford/demographics.csv")
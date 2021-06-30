require(bigrquery)
require(DBI)
require(dplyr)
require(dbplyr)
require(magrittr)
require(stringr)

bq_auth(email= "b.cooper@yhcr.nhs.uk")

con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_CDM_V1_50k_Random"
)

table_list <- bq_dataset_tables(con) %>% unlist()
table_list <- table_list[seq(3, length(table_list), by = 3)] %>% unname()

# Create blank list
CY <- list()

# Loading loop
print(noquote("Loading Connected Yorkshire..."))
for(i in 1:length(table_list))
{
  # Pull table
  loaded_table <- tbl(con, table_list[i])
  
  # Insert into list
  CY[[i]] <- loaded_table
  
  # Rename
  names(CY)[i] <- table_list[i]
}

# Load vocabulary
con2 <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_CDM_VOCAB"
)

table_list2 <- bq_dataset_tables(con2) %>% unlist()
table_list2 <- table_list2[seq(3, length(table_list2), by = 3)] %>% 
  unname()

# Create blank list
CY_V <- list()

print(noquote("Loading vocabulary..."))
for(i in 1:length(table_list2))
{
  # Pull table
  loaded_table <- tbl(con2, table_list2[i])
  
  # Insert into list
  CY_V[[i]] <- loaded_table
  
  # Rename
  names(CY_V)[i] <- paste("V", table_list2[i],
                          sep = "_")
}

# Combine and cleanup
CY <- c(CY, CY_V)

rm(i, table_list, table_list2,
   loaded_table, con, con2, CY_V)

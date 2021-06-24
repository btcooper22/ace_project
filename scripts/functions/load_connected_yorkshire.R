require(bigrquery)
require(DBI)
require(dplyr)
require(dbplyr)
require(magrittr)
require(stringr)

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
for(i in 1:length(table_list))
{
  # Generate name
  obj_name <- paste("CY", table_list[i], sep = "_")
  
  # Pull table
  loaded_table <- tbl(con, table_list[i])
  
  # Insert into list
  CY[[i]] <- loaded_table
  
  # Rename
  names(CY)[i] <- table_list[i]
}
rm(i, obj_name, table_list, 
   loaded_table, con)
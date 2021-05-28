require(bigrquery)
require(DBI)
require(dplyr)
require(dbplyr)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

df <- tbl(con, "tbl_ACE") %>% 
  collect()

# Filter to wheezy cases
ace_df <- df %>% 
  filter(Reason_for_Referral == "Wheezy child")

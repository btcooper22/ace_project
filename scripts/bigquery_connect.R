require(bigrquery)
require(DBI)
require(dplyr)
require(dbplyr)
require(readr)
require(magrittr)
require(lubridate)
require(readxl)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

bq_project_datasets("yhcr-prd-phm-bia-core")
bq_dataset_tables("yhcr-prd-phm-bia-core.CY_MYSPACE_BC")

df <- tbl(con, "tmp_ACE_v2") %>% 
  collect()

# Filter to wheezy cases
ace_df <- df %>% 
  filter(Reason_for_Referral == "Wheezy child" &
           Referral_accepted == "Yes")

ace_df

# ace_df %>% 
#   filter(substr(Postcode, 1, 2) == "BD") %>% 
#   write_csv("data/ace_data_080621.csv")
ace_df$season <- c("Spring", "Summer", "Autumn", "Winter")[quarter(ace_df$Date_of_referral_accepted)]

# Clear of NAs in identifying info
ace_df %<>% 
  filter(!is.na(Age) & !is.na(Postcode) &
           !is.na(GPSurgery) & !is.na(Outcome_for_care_episode) &
           !is.na(Bed_Days_saved) & !is.na(Ethnicity) & !is.na(Referral_Source) &
           !is.na(season))

length(unique(paste(ace_df$Age, ace_df$Postcode, ace_df$GPSurgery,
                    ace_df$Outcome_for_care_episode, ace_df$Bed_Days_saved,
                    ace_df$Ethnicity, ace_df$Referral_Source, ace_df$season)))

# Load original datasets----

ace_data_orig <- read_excel("data/ace_data_orig.xlsx", 
                            sheet = "Analysis", skip = 1)
ace_data_orig <- ace_data_orig[2:nrow(ace_data_orig),1:24]
names(ace_data_orig)[22] <- "Temperature"

ace_data_extra <- read_excel("data/ace_data_extra.xslx.xlsx", 
                                  sheet = "Referral Accepted", skip = 2)
ace_data_extra <- ace_data_extra[2:nrow(ace_data_extra),1:35]
names(ace_data_extra)[c(20, 25,27,28)] <- c("Activity Level of Child",
                                            "Temperature", "CRT", "AVPU")

brand_new_data <- read_excel("data/brand_new_data.xlsx")
brand_new_data <- brand_new_data[2:nrow(brand_new_data),c(1:3, 5:36)]
names(brand_new_data)[c(20,25,27,28)] <- c("Activity Level of Child", "Temperature",
                                           "CRT", "AVPU")

# Combine original sources
ace_data <- left_join(ace_data_orig, ace_data_extra) %>% 
  rbind(brand_new_data)

# Match formatting
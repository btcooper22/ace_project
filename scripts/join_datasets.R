require(bigrquery)
require(DBI)
require(dplyr)
require(dbplyr)
require(readr)
require(magrittr)
require(lubridate)
require(readxl)
require(stringr)

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
ace_spreadsheet <- left_join(ace_data_orig, ace_data_extra) %>% 
  rbind(brand_new_data)

# Match formatting----
# Age
ace_df$Age %<>% as.integer()
ace_spreadsheet$Age %<>% as.integer()

# Postcode
names(ace_df)[3] <- "Address"
ace_df$Address <- paste(substr(trimws(ace_df$Address),1,2),
                        str_pad(substr(trimws(ace_df$Address),3,4),
                                2, "left", "0"), sep = "")

# Surgery name
names(ace_spreadsheet)[6] <- "GPSurgery"

ace_df %<>% 
  mutate(GPSurgery = case_when(
    GPSurgery == "Dr Gilkar little horton lane" ~ "Little Horton Lane",
    GPSurgery == "Little horton Lane" ~ "Little Horton Lane",
    GPSurgery == "Saltaire and Windhill Medical Practice" ~ "Saltaire & Windhill",
    GPSurgery == "The Bradford Moor Practice" ~ "Bradford Moor",
    GPSurgery == "Frizinghall Medical" ~ "Frizinghall",
    GPSurgery == "Dr I Gilkar" ~ "Dr I Gilkhar",
    GPSurgery == "Kensington Partnership" ~ "Kensington",
    grepl("Picton", GPSurgery) ~ "Picton Medical Centre",
    is.character(GPSurgery) ~ GPSurgery
  ))

ace_spreadsheet %<>% 
  mutate(GPSurgery = case_when(
    GPSurgery == "Kensington Partnership" ~ "Kensington",
    GPSurgery == "Leylands" ~ "Leylands Medical Practice",
    GPSurgery == "Low Moor" ~ "Low Moor Medical Practice",
    GPSurgery == "Thornton" ~ "Thornton Medical Centre",
    GPSurgery == "Rockwell & Wrose" ~ "Rockwell & Wrose Practice",
    GPSurgery == "Parkside" ~ "Parkside Medical Centre",
    GPSurgery == "Farrow" ~ "Farrow Medical Practice",
    GPSurgery == "Moorside" ~ "Moorside Surgery",
    grepl("Picton", GPSurgery) ~ "Picton Medical Centre",
    GPSurgery == "The City Medical" ~ "The City Medical Practice",
    GPSurgery == "Sunny Bank" ~ "Sunnybank Medical Centre",
    GPSurgery == "Horton Park" ~ "Horton Park Surgery",
    GPSurgery == "Parklands" ~ "Parklands Medical Practice",
    GPSurgery == "Hollys Health & Wellbeing" ~ "Hollyn's Health & Wellbeing",
    GPSurgery == "Grange" ~ "Grange Medical Centre",
    GPSurgery == "Manor" ~ "Manor Medical Practice",
    GPSurgery == "Ashwell" ~ "Ashwell Medical Centre",
    GPSurgery == "Ridge" ~ "Ashwell Medical Centre",
    GPSurgery == "The Ridge" ~ "The Ridge Medical Practice - Great Horton Surgery",
    GPSurgery == "Bowling Highfield" ~ "Bowling Highfield Medical Practice",
    is.character(GPSurgery) ~ GPSurgery
  ))
setdiff(unique(ace_spreadsheet$GPSurgery), unique(ace_df$GPSurgery))

ace_spreadsheet %<>% 
  filter(GPSurgery %in% c("Other", "None") == FALSE)

# Outcome
table(ace_df$Outcome_for_care_episode)
table(ace_spreadsheet$`Hospital Required?`)

ace_df %<>% 
  mutate(`Hospital Required?` = ifelse(Outcome_for_care_episode == "Remained at home",
                                      "N", "Y"))
table(ace_df$`Hospital Required?`)

# Bed days saved
table(ace_df$Bed_Days_saved)
table(ace_spreadsheet$`No of days bed saved`)

ace_spreadsheet %<>% 
  mutate(`No of days bed saved` = as.integer(`No of days bed saved`))

ace_df %<>% 
  mutate(`No of days bed saved` = as.integer(Bed_Days_saved))
table(ace_df$`No of days bed saved`)

# Ethnicity
ace_df$Ethnicity %<>% tolower()
ace_spreadsheet$Ethnicity %<>% tolower()

ace_spreadsheet %<>% 
  filter(!is.na(Ethnicity) & Ethnicity != "not stated" &
           Ethnicity != "none") %>% 
  mutate(Ethnicity = ifelse(Ethnicity == "bristish", "british",
                            Ethnicity))

setdiff(unique(ace_spreadsheet$Ethnicity), unique(ace_df$Ethnicity))

ace_df %<>% 
  filter(!is.na(Ethnicity) & Ethnicity != "unspecified" &
           Ethnicity != "filipino") %>% 
  mutate(Ethnicity = trimws(Ethnicity)) %>% 
  mutate(Ethnicity = ifelse(Ethnicity == "asain", "asian",
                            Ethnicity))

setdiff(unique(ace_df$Ethnicity), unique(ace_spreadsheet$Ethnicity))

# Referral source
table(ace_spreadsheet$`Referral from`)

ace_spreadsheet %<>% 
  mutate(`Referral from` = case_when(
    `Referral from` == "CCDA ANP" ~ "CCDA",
    `Referral from` == "ED Pead" | `Referral from` == "ED Pead ANP" ~ "ED",
    `Referral from` == "GP ANP" ~ "GP",
    is.character(`Referral from`) ~ `Referral from`
  )) %>% 
  filter(`Referral from` != "None")

table(ace_df$Referral_Source)

ace_df %<>% 
  filter(Referral_Source != "APP'S") %>% 
  mutate(`Referral from` = case_when(
    Referral_Source == "CCDA ANP" ~ "CCDA",
    Referral_Source == "ED Pead" | Referral_Source == "ED Paed ANP" ~ "ED",
    Referral_Source == "GP ANP" | Referral_Source == "GP - Advanced Pharmacist" ~ "GP",
    is.character(Referral_Source) ~ Referral_Source)
  )

setdiff(unique(ace_df$`Referral from`), unique(ace_spreadsheet$`Referral from`))
setdiff(unique(ace_spreadsheet$`Referral from`), unique(ace_df$`Referral from`))

# Combination---------

# Check unique code numbers
identifier <- paste(ace_df$Age, ace_df$Address, ace_df$GPSurgery,
                    ace_df$`Hospital Required?`, ace_df$`No of days bed saved`,
                    ace_df$Ethnicity, ace_df$`Referral from`, sep = "_")
length(unique(identifier))
non_unique <- identifier[duplicated(identifier)]

# Filter out those without unique identifiers
ace_df <- ace_df[which(identifier %in% non_unique == FALSE),]

# Repeat for spreadsheet
identifier_s <- paste(ace_spreadsheet$Age, ace_spreadsheet$Address, ace_spreadsheet$GPSurgery,
                    ace_spreadsheet$`Hospital Required?`, ace_spreadsheet$`No of days bed saved`,
                    ace_spreadsheet$Ethnicity, ace_spreadsheet$`Referral from`, sep = "_")
length(unique(identifier))
non_unique_s <- identifier_s[duplicated(identifier_s)]
ace_spreadsheet <- ace_spreadsheet[which(identifier %in% non_unique_s == FALSE),]

# Join
combined_dataset <- ace_spreadsheet %>% 
  inner_join(ace_df, by = c("Age", "Address", "GPSurgery",
                            "Hospital Required?", "No of days bed saved",
                            "Ethnicity", "Referral from"))
write_csv(combined_dataset, "data/ace_data_linked.csv")

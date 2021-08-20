# Packages
require(readr)
require(dplyr)
require(ggplot2)
require(foreach)
require(doParallel)
require(tools)
require(tidyr)
require(magrittr)
require(readxl)
require(tibble)
require(geosphere)
require(bigrquery)
require(DBI)
require(dbplyr)
require(xtable)

bq_auth(email= "b.cooper@yhcr.nhs.uk")

# Load data
con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

patients <- tbl(con, "tmp_ACE_v2") %>% 
  filter(Outcome_for_care_episode != "NA" &
           Referral_accepted == "Yes",
         Reason_for_Referral == "Wheezy child") %>% 
  select(person_id, date = Date_of_referral_accepted,
         Outcome_for_care_episode, GPSurgery) %>% 
  collect() %>% 
  mutate(hosp_reqd = grepl("Admitted", Outcome_for_care_episode)) %>% 
  select(-Outcome_for_care_episode) %>% 
  na.omit() %>% 
  mutate(date = as.Date(date, format = c("%d/%m/%Y")),
         person_id = as.character(person_id))

# Load LSOA and attach
LSOA_df <- read_xlsx("data/ace_data_LSOA.xlsx") %>% 
  select(pkid, LSOA) %>% 
  filter(!duplicated(pkid))

patients %<>% 
  left_join(LSOA_df, c("person_id" = "pkid"))

# Read LSOA translation data
postcode_gridref <- read_csv("spatial/raw/postcode_gridref.csv") %>% 
  na.omit()
postcode_ons <- read_csv("spatial/area_stats/postcode_ONS_translation.csv")

# Join and aggregate
LSOA_coords <- postcode_ons %>% 
  left_join(postcode_gridref %>% 
              select(Postcode, Latitude,
                     Longitude),
            by = c("pcds" = "Postcode")) %>% 
  na.omit() %>% 
  group_by(lsoa11cd) %>% 
  summarise(lat = mean(Latitude),
            long = mean(Longitude))

# Filter patients by available LSOA data
patients %<>% 
  filter(LSOA %in% LSOA_coords$lsoa11cd)

# Write raw list of GP surgeries to add lat/long
# patients %>% 
#   select(GPSurgery) %>% 
#   filter(!duplicated(GPSurgery)) %>% 
#   arrange(GPSurgery) %>% 
#   write_csv("spatial/data/surgery_coords.csv")

# Read list of GP surgeries and coordinates
surgery_coords <- read_csv("spatial/raw/surgery_coords.csv")

# Loop through patients
results <- foreach(i = 1:nrow(patients),.combine = "rbind") %do%
  {
    # Find LSOA coordinates
     LSOA_latlong <- LSOA_coords %>% 
      filter(lsoa11cd == patients$LSOA[i]) %>% 
      select(long, lat) %>% 
       unlist()
     
     # Find surgery coordinates
     surgery_latlong <- surgery_coords %>% 
       filter(GPSurgery == patients$GPSurgery[i]) %>% 
       select(Longitude, Latitude) %>% 
       unlist() %>% 
       as.numeric()
     
     # Measure distance to surgery
     surgery_distance <- distGeo(LSOA_latlong, surgery_latlong)
     
     # Measure distances to hospitalts
     royal_infirmary_distance <- distGeo(LSOA_latlong, c(-1.796910261657741,
                                                         53.806674145377066))
     st_lukes_distance <- distGeo(LSOA_latlong, c(-1.7612790664998457,
                                                   53.78386494622222))
     
     # Output
     data.frame(patients[i,], surgery_distance,
                hospital_distance = min(c(royal_infirmary_distance,
                                          st_lukes_distance)))
  } %>% 
  mutate(log_surgery_distance = log(surgery_distance),
         log_hospital_distance = log(hospital_distance))

# Tabulate
results %>% 
  filter(person_id %in% read_csv("data/ace_data_cooper_final.csv")$person_id) %>% 
  select(4, 6:9) %>% 
  pivot_longer(2:5) %>% 
  group_by(name, hosp_reqd) %>% 
  summarise(median = round(median(value),2),
            Q1 = round(quantile(value, 0.25),2),
            Q3 = round(quantile(value, 0.75),2)) %>% 
  mutate(value = paste(median, " [", Q1, ", ",
                       Q3, "]", sep = "")) %>% 
  select(-median, -Q1, -Q3) %>% 
  pivot_wider(names_from = "hosp_reqd",
              values_from = "value") %>% 
  rename(`Discharged from ACE` = "FALSE",
         `Admitted to Hospital` = "TRUE")  %>%
  xtable() %>% 
  print(include.rownames = FALSE)

# Assess results
summary(glm(hosp_reqd ~ log_surgery_distance, data = results,
            family = "binomial")) # Yes

summary(glm(hosp_reqd ~ log_hospital_distance, data = results,
            family = "binomial")) # No

summary(glm(hosp_reqd ~ surgery_distance, data = results,
            family = "binomial")) # No

summary(glm(hosp_reqd ~ hospital_distance, data = results,
            family = "binomial")) # No

# Binarisation
variables_of_interest <- c("surgery_distance", "log_surgery_distance",
                           "hospital_distance", "log_hospital_distance")

division_results <- foreach(i = 1:length(variables_of_interest),
                            .combine = "rbind") %do%
  {
    # Extract to data frame
    print(variables_of_interest[i])
    var_df <- results %>% 
      select(any_of(c("hosp_reqd", variables_of_interest[i]))) %>% 
      rename("value" = variables_of_interest[i])
    
    # Find limits and build sequence
    limits <- quantile(var_df$value, c(0.025, 0.975))
    divisions_seq <- seq(limits[1], limits[2], length.out = 50)
    
    # Inner loop for divisions
    profile <- foreach(d = 1:length(divisions_seq),
                       .combine = "rbind") %do%
      {
        # Binarise variable
        var_df$value_bin <- var_df$value > divisions_seq[d]
        
        # Build model
        mod <- glm(hosp_reqd ~ value_bin, data = var_df,
                   family = "binomial")
        
        # Extract confidence interval
        conf_inteval <- suppressMessages(confint(mod)) 
        
        # Output
        data.frame(div = divisions_seq[d], coef = coef(mod)[2], 
                   L95 = conf_inteval[2,1], U95 = conf_inteval[2,2])
      }
    
    # Add variable name
    profile$varname <- variables_of_interest[i]
    
    # Calculate CI width 
    profile$CI_width <- abs(profile$U95 - profile$L95)
    
    # Calculate periods where effect direction is consistant
    profile$CI_dir_const <- sign(profile$L95) == sign(profile$U95)
    
    # Check for any periods that match the above
    if(any(profile$CI_dir_const))
    {
      output <- profile %>% 
        filter(CI_dir_const == TRUE) %>% 
        slice_min(CI_width)
    }else
    {
      output <- profile %>% 
        slice_min(CI_width)
    }
    
    # Prepare output
    output %>% 
      slice_head(n = 1)
  }

# Explore binarised results
summary(glm(hosp_reqd ~ (surgery_distance > division_results$div[1]),
            "binomial", results)) # Yes

summary(glm(hosp_reqd ~ (log_surgery_distance > division_results$div[2]),
            "binomial", results)) # Yes

summary(glm(hosp_reqd ~ (hospital_distance > division_results$div[3]),
            "binomial", results)) # Yes

summary(glm(hosp_reqd ~ (log_hospital_distance > division_results$div[4]),
            "binomial", results)) # Yes

# Write to file
results %>% 
  mutate(high_surgery_distance = surgery_distance > division_results$div[1],
         high_log_surgery_distance = log_surgery_distance > division_results$div[2],
         high_hospital_distance = hospital_distance > division_results$div[3],
         high_log_hospital_distance = log_hospital_distance > division_results$div[4]) %>% 
  select(person_id, date, hosp_reqd,
         log_surgery_distance,
         high_surgery_distance, high_log_surgery_distance,
         high_hospital_distance, high_log_hospital_distance) %>% 
  write_csv("data/new_features/distance.csv")

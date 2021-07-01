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

# Load data
patients <- read_xlsx("data/ace_data_LSOA.xlsx") %>% 
  select(pkid, LSOA, Outcome_for_care_episode,
         GPSurgery) %>% 
  filter(Outcome_for_care_episode != "NA") %>% 
  mutate(hosp_reqd = grepl("Admitted", Outcome_for_care_episode)) %>% 
  select(-Outcome_for_care_episode)

# Read LSOA translation data
postcode_gridref <- read_csv("spatial/data/grid/postcode_gridref.csv") %>% 
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
surgery_coords <- read_csv("spatial/data/surgery_coords.csv")

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
  }

summary(glm(hosp_reqd ~ log(surgery_distance), data = results,
            family = "binomial"))

summary(glm(hosp_reqd ~ log(hospital_distance), data = results,
            family = "binomial"))

plot(log(results$surgery_distance),
     results$hosp_reqd)

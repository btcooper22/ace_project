# Packages
require(bigrquery)
require(DBI)
require(dplyr)
require(magrittr)
require(readr)
require(tibble)
require(readxl)
require(stringr)
require(dbplyr)
require(lubridate)
require(foreach)
require(tidyr)
bq_auth(email= "b.cooper@yhcr.nhs.uk")

crosstab <- function(varname, .df = results)
{
  options(dplyr.summarise.inform=F)
  
  # Find
  .df  %>% 
    group_by_at(c("hosp_reqd", varname)) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = "hosp_reqd",
                values_from = "n") %>% 
    mutate(rate = `TRUE` / (`FALSE` + `TRUE`)) %>% 
    mutate(`TRUE (%)` = paste(`TRUE`, " (", round(rate * 100,1),
                              "%)", sep = "")) %>% 
    select(any_of(c(varname, "FALSE", "TRUE (%)"))) %>% 
    as.data.frame()
}

quick_glm <- function(varname, .df = results)
{
  formu <- paste("hosp_reqd ~", varname)
  summary(glm(formu, "binomial", .df))
}

# Load dataset
con <- dbConnect(
  bigrquery::bigquery(),
  project = "yhcr-prd-phm-bia-core",
  dataset = "CY_MYSPACE_BC"
)

results <- tbl(con, "tmp_ACE_v2") %>% 
  filter(Outcome_for_care_episode != "NA" &
           Referral_accepted == "Yes",
         Reason_for_Referral == "Wheezy child") %>% 
  select(person_id, date = Date_of_referral_accepted,
         Outcome_for_care_episode) %>% 
  collect() %>% 
  mutate(hosp_reqd = grepl("Admitted", Outcome_for_care_episode)) %>% 
  select(-Outcome_for_care_episode) %>% 
  na.omit() %>% 
  mutate(date = as.Date(date, format = c("%d/%m/%Y")))

# Comorbidities
# Load and examine
comorbidities <- read_csv("data/cBradford/comorbidities.csv")
table(comorbidities$comorbidity)

# Only eczema and and respiratory infections look promising
# Are there enough individuals for GERD and rhinitis?
comorbidities %>% 
  filter(comorbidity %in% c("GERD", "rhinitis")) %>% 
  group_by(comorbidity) %>% 
  summarise(unique_patients = length(unique(person_id)))
# Can explore with both

# GERD----
patients_gerd <- comorbidities %>% 
  filter(comorbidity == "GERD")

results_gerd <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in GERD database
    gerd_subset <- patients_gerd %>% 
      filter(person_id == pid)
    
    if(nrow(gerd_subset) == 0)
    {
      gerd_any <- FALSE
      gerd_year <- FALSE
      gerd_6m <- FALSE
      gerd_1m <- FALSE
    }else
    {
      # Any gerd
      gerd_any <- TRUE
      
      # Find time difference
      recent_gerd <- max(gerd_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_gerd) %>% 
        select(time_diff) %>% 
        deframe()
      
      # GERD diagnosis within year
      gerd_year <- time_diff < 365
      
      # GERD diagnosis within 6 months
      gerd_6m <- time_diff < 182
      
      # GERD diagnosis within 1 month
      gerd_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], gerd_any, gerd_year,
               gerd_6m, gerd_1m)
  }

crosstab("gerd_any", results_gerd) # No
quick_glm("gerd_any", results_gerd)

# Rhinitis----
patients_rhinitis <- comorbidities %>% 
  filter(comorbidity == "rhinitis")

results_rhinitis <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in rhinitis database
    rhinitis_subset <- patients_rhinitis %>% 
      filter(person_id == pid)
    
    if(nrow(rhinitis_subset) == 0)
    {
      rhinitis_any <- FALSE
      rhinitis_year <- FALSE
      rhinitis_6m <- FALSE
      rhinitis_1m <- FALSE
    }else
    {
      # Any rhinitis
      rhinitis_any <- TRUE
      
      # Find time difference
      recent_rhinitis <- max(rhinitis_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_rhinitis) %>% 
        select(time_diff) %>% 
        deframe()
      
      # rhinitis diagnosis within year
      rhinitis_year <- time_diff < 365
      
      # rhinitis diagnosis within 6 months
      rhinitis_6m <- time_diff < 182
      
      # rhinitis diagnosis within 1 month
      rhinitis_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], rhinitis_any, rhinitis_year,
               rhinitis_6m, rhinitis_1m)
  }

crosstab("rhinitis_any", results_rhinitis) 
crosstab("rhinitis_year", results_rhinitis) 
crosstab("rhinitis_6m", results_rhinitis)
crosstab("rhinitis_1m", results_rhinitis)
# Maybe something there?

quick_glm("rhinitis_any", results_rhinitis)
quick_glm("rhinitis_year", results_rhinitis)
quick_glm("rhinitis_6m", results_rhinitis)
quick_glm("rhinitis_1m", results_rhinitis)

# Eczema----
patients_eczema <- comorbidities %>% 
  filter(comorbidity == "eczema")

results_eczema <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in eczema database
    eczema_subset <- patients_eczema %>% 
      filter(person_id == pid)
    
    if(nrow(eczema_subset) == 0)
    {
      eczema_any <- FALSE
      eczema_year <- FALSE
      eczema_6m <- FALSE
      eczema_1m <- FALSE
    }else
    {
      # Any eczema
      eczema_any <- TRUE
      
      # Find time difference
      recent_eczema <- max(eczema_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_eczema) %>% 
        select(time_diff) %>% 
        deframe()
      
      # eczema diagnosis within year
      eczema_year <- time_diff < 365
      
      # eczema diagnosis within 6 months
      eczema_6m <- time_diff < 182
      
      # eczema diagnosis within 1 month
      eczema_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], eczema_any, eczema_year,
               eczema_6m, eczema_1m)
  }

crosstab("eczema_any", results_eczema) 
crosstab("eczema_year", results_eczema) 
crosstab("eczema_6m", results_eczema)
crosstab("eczema_1m", results_eczema)

quick_glm("eczema_any", results_eczema) # Yes 
quick_glm("eczema_year", results_eczema) # Maybe
quick_glm("eczema_6m", results_eczema) # No
quick_glm("eczema_1m", results_eczema) # No

# Asthma----
patients_asthma <- comorbidities %>% 
  filter(comorbidity == "respiratory_infection" &
           term == "asthma")

results_asthma <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in asthma database
    asthma_subset <- patients_asthma %>% 
      filter(person_id == pid)
    
    if(nrow(asthma_subset) == 0)
    {
      asthma_any <- FALSE
      asthma_year <- FALSE
      asthma_6m <- FALSE
      asthma_1m <- FALSE
    }else
    {
      # Any asthma
      asthma_any <- TRUE
      
      # Find time difference
      recent_asthma <- max(asthma_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_asthma) %>% 
        select(time_diff) %>% 
        deframe()
      
      # asthma diagnosis within year
      asthma_year <- time_diff < 365
      
      # asthma diagnosis within 6 months
      asthma_6m <- time_diff < 182
      
      # asthma diagnosis within 1 month
      asthma_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], asthma_any, asthma_year,
               asthma_6m, asthma_1m)
  }

crosstab("asthma_any", results_asthma) 
crosstab("asthma_year", results_asthma) 
crosstab("asthma_6m", results_asthma)
crosstab("asthma_1m", results_asthma)

quick_glm("asthma_any", results_asthma) # Yes 
quick_glm("asthma_year", results_asthma) # Yes
quick_glm("asthma_6m", results_asthma) # Yes
quick_glm("asthma_1m", results_asthma) # Yes

# Other respiratory infections----
# Pooled
patients_respiratory <- comorbidities %>% 
  filter(comorbidity == "respiratory_infection" &
           term != "asthma")

results_respiratory <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in respiratory database
    respiratory_subset <- patients_respiratory %>% 
      filter(person_id == pid)
    
    if(nrow(respiratory_subset) == 0)
    {
      respiratory_any <- FALSE
      respiratory_year <- FALSE
      respiratory_6m <- FALSE
      respiratory_1m <- FALSE
    }else
    {
      # Any respiratory
      respiratory_any <- TRUE
      
      # Find time difference
      recent_respiratory <- max(respiratory_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_respiratory) %>% 
        select(time_diff) %>% 
        deframe()
      
      # respiratory diagnosis within year
      respiratory_year <- time_diff < 365
      
      # respiratory diagnosis within 6 months
      respiratory_6m <- time_diff < 182
      
      # respiratory diagnosis within 1 month
      respiratory_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], respiratory_any, respiratory_year,
               respiratory_6m, respiratory_1m)
  }

crosstab("respiratory_any", results_respiratory) 
crosstab("respiratory_year", results_respiratory) 
crosstab("respiratory_6m", results_respiratory)
crosstab("respiratory_1m", results_respiratory)

quick_glm("respiratory_any", results_respiratory) # Yes 
quick_glm("respiratory_year", results_respiratory) # Yes
quick_glm("respiratory_6m", results_respiratory) # Yes
quick_glm("respiratory_1m", results_respiratory) # Yes

# Bronchitis
patients_bronchitis <- comorbidities %>% 
  filter(comorbidity == "respiratory_infection" &
           term == "bronchitis")

results_bronchitis <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in bronchitis database
    bronchitis_subset <- patients_bronchitis %>% 
      filter(person_id == pid)
    
    if(nrow(bronchitis_subset) == 0)
    {
      bronchitis_any <- FALSE
      bronchitis_year <- FALSE
      bronchitis_6m <- FALSE
      bronchitis_1m <- FALSE
    }else
    {
      # Any bronchitis
      bronchitis_any <- TRUE
      
      # Find time difference
      recent_bronchitis <- max(bronchitis_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_bronchitis) %>% 
        select(time_diff) %>% 
        deframe()
      
      # bronchitis diagnosis within year
      bronchitis_year <- time_diff < 365
      
      # bronchitis diagnosis within 6 months
      bronchitis_6m <- time_diff < 182
      
      # bronchitis diagnosis within 1 month
      bronchitis_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], bronchitis_any, bronchitis_year,
               bronchitis_6m, bronchitis_1m)
  }

crosstab("bronchitis_any", results_bronchitis) 
crosstab("bronchitis_year", results_bronchitis) 
crosstab("bronchitis_6m", results_bronchitis)
crosstab("bronchitis_1m", results_bronchitis)

quick_glm("bronchitis_any", results_bronchitis) # Yes 
quick_glm("bronchitis_year", results_bronchitis) # Yes
quick_glm("bronchitis_6m", results_bronchitis) # Yes
quick_glm("bronchitis_1m", results_bronchitis) # Yes

# Pneumonia
patients_pneumonia <- comorbidities %>% 
  filter(comorbidity == "respiratory_infection" &
           term == "pneumonia")

results_pneumonia <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in pneumonia database
    pneumonia_subset <- patients_pneumonia %>% 
      filter(person_id == pid)
    
    if(nrow(pneumonia_subset) == 0)
    {
      pneumonia_any <- FALSE
      pneumonia_year <- FALSE
      pneumonia_6m <- FALSE
      pneumonia_1m <- FALSE
    }else
    {
      # Any pneumonia
      pneumonia_any <- TRUE
      
      # Find time difference
      recent_pneumonia <- max(pneumonia_subset$condition_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_pneumonia) %>% 
        select(time_diff) %>% 
        deframe()
      
      # pneumonia diagnosis within year
      pneumonia_year <- time_diff < 365
      
      # pneumonia diagnosis within 6 months
      pneumonia_6m <- time_diff < 182
      
      # pneumonia diagnosis within 1 month
      pneumonia_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], pneumonia_any, pneumonia_year,
               pneumonia_6m, pneumonia_1m)
  }

crosstab("pneumonia_any", results_pneumonia) 
crosstab("pneumonia_year", results_pneumonia) 
crosstab("pneumonia_6m", results_pneumonia)
crosstab("pneumonia_1m", results_pneumonia)

quick_glm("pneumonia_any", results_pneumonia) # Yes 
quick_glm("pneumonia_year", results_pneumonia) # Yes
quick_glm("pneumonia_6m", results_pneumonia) # Yes
quick_glm("pneumonia_1m", results_pneumonia) # Yes

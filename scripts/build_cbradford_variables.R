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

# Comorbidities: Load and examine
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

# Bronchitis----
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

# Pneumonia----
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

# Prescriptions: Load and examine----
prescriptions <- read_csv("data/cBradford/prescriptions.csv")
table(prescriptions$class)

# Number of inhalers
prescriptions %<>% 
  mutate(inhaler = grepl("inhaler", dose_unit_source_value)) %>% 
  mutate(n_inhaler = ifelse(inhaler, substr(dose_unit_source_value, 1, 1), 0) %>% 
           as.numeric())

# Count number of inhalers in last X
patients_inhalers <- prescriptions %>% 
  filter(inhaler == TRUE)

# Quick count function
count_or_zero <- function(df)
{
  if(nrow(df) == 0)
  {
    return(0)
  }else
  {
    return(sum(df$n_inhaler))
  }
}

results_inhalers <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in inhalers database
    inhalers_subset <- patients_inhalers %>% 
      filter(person_id == pid)
    
    # Extract inhaler types
    relievers <-  inhalers_subset %>% 
      filter(class == "fast_bronchodilator")
    
    preventers <- inhalers_subset %>% 
      filter(class == "slow_bronchodilator")
    
    # Total
    inhalers_total <- count_or_zero(inhalers_subset)
    relievers_total <- count_or_zero(relievers)
    preventers_total <- count_or_zero(preventers)
    
    # Last three years
    inhalers_3y <- count_or_zero(inhalers_subset %>% 
                                   filter(drug_exposure_start_date > (date_instance - 1095))
                                 )
    relievers_3y <- count_or_zero(relievers %>% 
                                   filter(drug_exposure_start_date > (date_instance - 1095))
    )
    preventers_3y <- count_or_zero(preventers %>% 
                                   filter(drug_exposure_start_date > (date_instance - 1095))
    )
    
    # Last year
    inhalers_1y <- count_or_zero(inhalers_subset %>% 
                                   filter(drug_exposure_start_date > (date_instance - 365))
    )
    relievers_1y <- count_or_zero(relievers %>% 
                                   filter(drug_exposure_start_date > (date_instance - 365))
    )
    preventers_1y <- count_or_zero(preventers %>% 
                                   filter(drug_exposure_start_date > (date_instance - 365))
    )
    
    # Last six months
    inhalers_6m <- count_or_zero(inhalers_subset %>% 
                                   filter(drug_exposure_start_date > (date_instance - 182))
    )
    relievers_6m <- count_or_zero(relievers %>% 
                                   filter(drug_exposure_start_date > (date_instance - 182))
    )
    preventers_6m <- count_or_zero(preventers %>% 
                                   filter(drug_exposure_start_date > (date_instance - 182))
    )
    
    # Output
    data.frame(results[i,], inhalers_total, inhalers_3y, inhalers_1y, inhalers_6m,
               relievers_total, relievers_3y, relievers_1y, relievers_6m, 
               preventers_total, preventers_3y, preventers_1y, preventers_6m)
  }

quick_glm("inhalers_total", results_inhalers)

# Find limits and build sequence
divisions_seq <- 1:67

# Inner loop for divisions
profile <- foreach(d = 1:length(divisions_seq),
                   .combine = "rbind") %do%
  {
    # Binarise variable
    results_inhalers$inhalers_total_bin <- results_inhalers$inhalers_total > divisions_seq[d]
    
    # Build model
    mod <- glm(hosp_reqd ~ inhalers_total_bin, data = results_inhalers,
               family = "binomial")
    
    # Extract confidence interval
    conf_inteval <- suppressMessages(confint(mod)) 
    
    # Output
    data.frame(div = divisions_seq[d], coef = coef(mod)[2], 
               L95 = conf_inteval[2,1], U95 = conf_inteval[2,2])
  }
summary(glm(hosp_reqd ~ (inhalers_total > 12), "binomial", results_inhalers)) # Sure, why not

# Antihistamines----
patients_antihistamine <- prescriptions %>% 
  filter(class == "antihistamine")

results_antihistamine <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in antihistamine database
    antihistamine_subset <- patients_antihistamine %>% 
      filter(person_id == pid)
    
    if(nrow(antihistamine_subset) == 0)
    {
      antihistamine_any <- FALSE
      antihistamine_year <- FALSE
      antihistamine_6m <- FALSE
      antihistamine_1m <- FALSE
    }else
    {
      # Any antihistamine presciption
      antihistamine_any <- TRUE
      
      # Find time difference
      recent_antihistamine <- max(antihistamine_subset$drug_exposure_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_antihistamine) %>% 
        select(time_diff) %>% 
        deframe()
      
      # antihistamine presciption within year
      antihistamine_year <- time_diff < 365
      
      # antihistamine presciption within 6 months
      antihistamine_6m <- time_diff < 182
      
      # antihistamine presciption within 1 month
      antihistamine_1m <- time_diff <= 31
    }
    
    # Output
    data.frame(results[i,], antihistamine_any, antihistamine_year,
               antihistamine_6m, antihistamine_1m)
  }

crosstab("antihistamine_any", results_antihistamine) 
crosstab("antihistamine_year", results_antihistamine) 
crosstab("antihistamine_6m", results_antihistamine)
crosstab("antihistamine_1m", results_antihistamine)

quick_glm("antihistamine_any", results_antihistamine) # No 
quick_glm("antihistamine_year", results_antihistamine) # No
quick_glm("antihistamine_6m", results_antihistamine) # No
quick_glm("antihistamine_1m", results_antihistamine) # No

# Prednisolone----
patients_prednisolone <- prescriptions %>% 
  filter(class == "prednisolone")

results_prednisolone <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in prednisolone database
    prednisolone_subset <- patients_prednisolone %>% 
      filter(person_id == pid)
    
    if(nrow(prednisolone_subset) == 0)
    {
      prednisolone_any <- FALSE
      prednisolone_courses_total <- 0
      prednisolone_year <- FALSE
      prednisolone_courses_year <- 0
      prednisolone_6m <- FALSE
      prednisolone_courses_6m <- 0
      prednisolone_1m <- FALSE
      prednisolone_courses_1m <- 0
    }else
    {
      # Any prednisolone presciption
      prednisolone_any <- TRUE
      prednisolone_courses_total <- nrow(prednisolone_subset)
      
      # Find time difference
      recent_prednisolone <- max(prednisolone_subset$drug_exposure_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_prednisolone) %>% 
        select(time_diff) %>% 
        deframe()
      
      # prednisolone presciption within year
      prednisolone_year <- time_diff < 365
      prednisolone_courses_year <- nrow(prednisolone_subset %>% filter(time_diff < 365))
      
      # prednisolone presciption within 6 months
      prednisolone_6m <- time_diff < 182
      prednisolone_courses_6m <- nrow(prednisolone_subset %>% filter(time_diff < 182))
      
      # prednisolone presciption within 1 month
      prednisolone_1m <- time_diff <= 31
      prednisolone_courses_1m <- nrow(prednisolone_subset %>% filter(time_diff <= 31))
    }
    
    # Output
    data.frame(results[i,], prednisolone_any, prednisolone_courses_total, 
               prednisolone_year, prednisolone_courses_year,
               prednisolone_6m, prednisolone_courses_6m,
               prednisolone_1m, prednisolone_courses_1m)
  }

crosstab("prednisolone_any", results_prednisolone) 
crosstab("prednisolone_year", results_prednisolone) 
crosstab("prednisolone_6m", results_prednisolone)
crosstab("prednisolone_1m", results_prednisolone)

quick_glm("prednisolone_any", results_prednisolone) # Kind of 
quick_glm("prednisolone_year", results_prednisolone) # Yes
quick_glm("prednisolone_6m", results_prednisolone) # Yes
quick_glm("prednisolone_1m", results_prednisolone) # Yes

# List variables worth investigation
variables_of_interest <- c("prednisolone_courses_total",
                           "prednisolone_courses_year",
                           "prednisolone_courses_6m",
                           "prednisolone_courses_1m")

division_results <- foreach(i = 1:length(variables_of_interest),
                            .combine = "rbind") %do%
  {
    # Extract to data frame
    print(variables_of_interest[i])
    var_df <- results_prednisolone %>% 
      select(any_of(c("hosp_reqd", variables_of_interest[i]))) %>% 
      rename("value" = variables_of_interest[i])
    
    # Find limits and build sequence
    limits <- quantile(var_df$value, c(0.01, 0.99))
    divisions_seq <- unique(round(seq(limits[1], limits[2], length.out = 50)))
    
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

with(results_prednisolone, table(hosp_reqd, prednisolone_courses_total > 3))
summary(glm(hosp_reqd ~ (prednisolone_courses_total > 3),
            "binomial",results_prednisolone)) # Yes

# Prepare output of useful variables----
results_eczema %>% 
  select(person_id, date, hosp_reqd,
         eczema_any, eczema_year) %>% 
  cbind(results_asthma %>% 
          select(asthma_any, asthma_year,
                 asthma_6m, asthma_1m),
        results_respiratory %>% 
          select(respiratory_any, respiratory_year,
                 respiratory_6m, respiratory_1m),
        results_bronchitis %>% 
          select(bronchitis_any, bronchitis_year,
                 bronchitis_6m, bronchitis_1m),
        results_pneumonia %>% 
          select(pneumonia_any, pneumonia_year,
                 pneumonia_6m, pneumonia_1m),
        results_inhalers %>% 
          mutate(many_inhalers = inhalers_total > 12) %>% 
          select(many_inhalers),
        results_prednisolone %>% 
          mutate(many_prednisolone_courses = prednisolone_courses_total > 3,
                 multiple_prednisolone_courses_year = prednisolone_courses_year > 1,
                 multiple_prednisolone_courses_6m = prednisolone_courses_6m > 1) %>% 
          select(many_prednisolone_courses, multiple_prednisolone_courses_year,
                 multiple_prednisolone_courses_6m, prednisolone_year,
                 prednisolone_6m, prednisolone_1m)) %>% 
  write_csv("data/new_features/comorbidities_prescriptions.csv")

# Demographics - Gender----
demographics <- read_csv("data/cBradford/demographics.csv") %>% 
  left_join(results) %>% 
  na.omit
table(demographics$gender_source_value)

crosstab("gender_source_value", demographics)
summary(glm(hosp_reqd ~ (gender_source_value == "M"),
            binomial, demographics))

# Ethnicity----
asian_ethnicity <-  c("indian", "mixed Asian", "pakistani", "white asain", "british asian",
         "asian", "sri Lankan", "other asian background", "bangladeshi")
european_ethnicity <-  c("slovak", "british", "other white background", "czech republic",
            "white europeon", "white british", "commonwealth russian",
            "other european", "mixed white")

demographics %<>% 
  mutate(ethnic_group = case_when(
    ethnicity_concept_id %in% c(46286810, 0) ~ "European",
    ethnicity_concept_id %in% c(46285832) ~ "Pakistani",
    ethnicity_concept_id %in% c(46285831, 46285833, 46285835) ~ "Other Asian",
    ethnicity_concept_id %in% c(46285827, 46285828, 46285829,
                           46285830, 46285836, 46285837,
                           46285839, 46286811) ~ "Other"
  ))

crosstab("ethnic_group", demographics)
quick_glm("ethnic_group", demographics)

# Visits----
visits <- read_csv("data/cBradford/visits.csv") %>% 
  left_join(results) %>% 
  na.omit
table(visits$concept_name)

results_prednisolone <- foreach(i = 1:nrow(results), .combine = "rbind") %do%
  {
    # Isolate person
    pid <- results$person_id[i]
    date_instance <- results$date[i]
    
    # Check in prednisolone database
    visit_subset <- visits %>% 
      filter(person_id == pid)
    
    if(nrow(visit_subset) == 0)
    {
      visit_any <- FALSE
      visit_courses_total <- 0
      visit_year <- FALSE
      visit_courses_year <- 0
      visit_6m <- FALSE
      visit_courses_6m <- 0
      visit_1m <- FALSE
      visit_courses_1m <- 0
    }else
    {
      # Any visits
      visit_any <- TRUE
      visits_total <- nrow(visit_subset)
      
      # Extract visit types
      table(visit_subset$concept_name)
      
      # Find time difference
      recent_visit <- max(visit_subset$drug_exposure_start_date)
      time_diff <- results %>% 
        filter(person_id == pid &
                 date == date_instance) %>% 
        mutate(time_diff = date - recent_visit) %>% 
        select(time_diff) %>% 
        deframe()
      
      # visit presciption within year
      visit_year <- time_diff < 365
      visit_courses_year <- nrow(visit_subset %>% filter(time_diff < 365))
      
      # visit presciption within 6 months
      visit_6m <- time_diff < 182
      visit_courses_6m <- nrow(visit_subset %>% filter(time_diff < 182))
      
      # visit presciption within 1 month
      visit_1m <- time_diff <= 31
      visit_courses_1m <- nrow(visit_subset %>% filter(time_diff <= 31))
    }
    
    # Output
    data.frame(results[i,], visit_any, visit_courses_total, 
               visit_year, visit_courses_year,
               visit_6m, visit_courses_6m,
               visit_1m, visit_courses_1m)
  }
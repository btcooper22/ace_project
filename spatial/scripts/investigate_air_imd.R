# Packages
require(readr)
require(dplyr)
require(ggplot2)
require(magrittr)
require(tidyr)
require(ggbump)
require(readxl)
require(foreach)
require(bigrquery)
require(DBI)
require(dbplyr)

bq_auth(email= "b.cooper@yhcr.nhs.uk")

# Cross-tabulation function
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

# Quick cut-and-summarise function
cut_bump <- function(varname, .df = results,
                     Q = FALSE)
{
  options(dplyr.summarise.inform=F)
  
  # Cut variable
  if(Q)
  {
    qlist <- seq(0, 1, 0.2)
    cut_df <- .df  %>% 
      mutate(cut_var = cut(get(varname), quantile(get(varname), qlist),
                           labels = c("VeryLow", "Low", "Medium",
                                      "High", "VeryHigh"), TRUE)) 
  }else
  {
    cut_df <- .df  %>% 
      mutate(cut_var = cut(get(varname), 5, labels = c("VeryLow", "Low", "Medium",
                                                       "High", "VeryHigh"))) 
  }

  # Calculate and print stats
  cut_df %>% 
    group_by_at(c("hosp_reqd", "cut_var")) %>% 
    summarise(n = n()) %>% 
    pivot_wider(names_from = "hosp_reqd",
                values_from = "n") %>% 
    mutate(rate = `TRUE` / (`FALSE` + `TRUE`)) %>% 
    mutate(`TRUE (%)` = paste(`TRUE`, " (", round(rate * 100,1),
                              "%)", sep = "")) %>% 
    select(any_of(c("cut_var", "FALSE", "TRUE (%)"))) %>% 
    as.data.frame() %>% 
    print()
  
  # Plot
  cut_df %>% 
    select(hosp_reqd, cut_var) %>% 
    group_by(cut_var) %>% 
    summarise(hosp_reqd = mean(hosp_reqd)) %>% 
    ungroup() %>% 
    mutate(cut_var = as.numeric(cut_var)) %>% 
    ggplot(aes(cut_var, hosp_reqd * 100),
           colour = "red")+
    geom_bump(size = 2)+
    labs(x = varname,
         y = "Hospitalisation rate")+
    theme_classic(20)+
    theme(legend.position = "top")+
    scale_x_continuous(labels = c("1" = "VeryLow", "2" = "Low", "3" = "Medium",
                                  "4" = "High", "5" = "VeryHigh"))
}

# Load data----
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
  mutate(date = as.Date(date, format = c("%d/%m/%Y")),
         person_id = as.character(person_id))

# Load LSOA and attach
LSOA_df <- read_xlsx("data/ace_data_LSOA.xlsx") %>% 
  select(pkid, LSOA) %>% 
  filter(!duplicated(pkid))

results %<>% 
  left_join(LSOA_df, c("person_id" = "pkid"))

# Load spatial stats
air_data <- read_csv("spatial/area_stats/bradford_air_LSOA.csv") %>%
  rename("LSOA" = lsoa11cd)
imd_data <- read_csv("spatial/area_stats/bradford_imd_LSOA.csv") %>%
  rename("LSOA" = lsoa11cd)

# Attach spatial variables
results %<>% 
  left_join(air_data) %>% 
  left_join(imd_data) %>% 
  na.omit()

# Air variables----

# Quick-cut air variables
level_labels <- c("VeryLow", "Low", "Medium",
                   "High", "VeryHigh")
results %<>% 
  mutate(NO2_cut = cut(NO2, 5, labels = level_labels),
         NOx_cut = cut(NOx, 5, labels = level_labels),
         PM10_cut = cut(PM10, 5, labels = level_labels),
         PM25_cut = cut(PM25, 5, labels = level_labels))

# Cross-tabulate
crosstab("NO2_cut")
crosstab("NOx_cut")
crosstab("PM10_cut")
crosstab("PM25_cut")

# Plot NO2
results %>% 
  select(hosp_reqd, NO2_cut, NOx_cut,
         PM10_cut, PM25_cut) %>% 
  pivot_longer(2:5) %>% 
  group_by(name, value) %>% 
  summarise(hosp_reqd = mean(hosp_reqd)) %>% 
  ungroup() %>% 
  mutate(NO2_cut = as.numeric(value)) %>% 
  ggplot(aes(NO2_cut, hosp_reqd * 100,
             colour = name))+
  geom_bump(size = 2)+
  scale_colour_brewer(palette = "Set1",
                      name = "Variable")+
  labs(x = "Variable category",
    y = "Hospitalisation rate")+
  theme_classic(20)+
  theme(legend.position = "top")+
  scale_x_continuous(labels = c("1" = "VeryLow", "2" = "Low", "3" = "Medium",
                              "4" = "High", "5" = "VeryHigh"))

# Quantiles

cut_bump("NO2", Q = TRUE) # >=H
cut_bump("NOx", Q = TRUE) # >=H
cut_bump("PM10", Q = TRUE) # >=M
cut_bump("PM25", Q = TRUE) # ==VL

# IMD variables-----

# Adult Skills Sub-domain Score
cut_bump("ASScore") # >=M
cut_bump("ASScore", Q = TRUE) # >=M

# Barriers to Housing and Services Score
cut_bump("BHSScore") # None
cut_bump("BHSScore", Q = TRUE) # None

# Crime Score
cut_bump("CriScore")
cut_bump("CriScore", Q = TRUE) # ==VH

# Children and Young People Sub-domain Score
cut_bump("CYPScore") # >=H
cut_bump("CYPScore", Q = TRUE) # >=H

# Education, Skills and Training Score
cut_bump("EduScore") # Continuous/==VL
cut_bump("EduScore", Q = TRUE) # >=M

# Employment Score (rate)
cut_bump("EmpScore") # None
cut_bump("EmpScore", Q = TRUE) # >=H

# Living Environment Score
cut_bump("EnvScore") # None
cut_bump("EnvScore", Q = TRUE) # None

# Geographical Barriers Sub-domain Score
cut_bump("GBScore") # Continuous/>=M
cut_bump("GBScore", Q = TRUE) # None

# Health Deprivation and Disability Score
cut_bump("HDDScore") # ==VL
cut_bump("HDDScore", Q = TRUE) # >= VH

# Income Deprivation Affecting Children Index (IDACI) Score (rate)
cut_bump("IDCScore") # >=H
cut_bump("IDCScore", Q = TRUE) # >=H

# Income Deprivation Affecting Older People (IDAOPI) Score (rate)
cut_bump("IDOScore") # None
cut_bump("IDOScore", Q = TRUE) # None

# Index of Multiple Deprivation (IMD) Score
cut_bump("IMDScore") # >=H
cut_bump("IMDScore", Q = TRUE) # >=H

# Income Score (rate)
cut_bump("IncScore") # None
cut_bump("IncScore", Q = TRUE) # None

# Indoors Sub-domain Score
cut_bump("IndScore") # None
cut_bump("IndScore", Q = TRUE) # None

# Outdoors Sub-domain Score
cut_bump("OutScore") # >=H
cut_bump("OutScore", Q = TRUE) # None

# Wider Barriers Sub-domain Score
cut_bump("WBScore") # ==VL
cut_bump("WBScore", Q = TRUE) # None


# Construct new variables----

# List variables worth investigation
variables_of_interest <- c("NO2", "NOx", "PM10", "PM25", "ASScore",
                           "CriScore", "CYPScore", "EduScore",
                           "HDDScore", "IDCScore", "IMDScore")

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

# Construct new variables from divisions
results %<>% 
  mutate(high_NO2 = NO2 > division_results$div[1],
         high_NOx = NOx > division_results$div[2],
         high_PM10 = PM10 > division_results$div[3],
         high_PM25 = PM25 > division_results$div[4],
         high_ASScore = ASScore > division_results$div[5],
         high_CriScore = CriScore > division_results$div[6],
         high_CYPScore = CYPScore > division_results$div[7],
         high_EduScore = EduScore > division_results$div[8],
         high_HDDScore = HDDScore > division_results$div[9],
         high_IDCScore = IDCScore > division_results$div[10],
         high_IMDScore = IMDScore > division_results$div[11],
  )

# Model
# Previous model - mentions asthma, mentions salbutamol, referral from GP, moderate severity, abnormal resp. rate

# NO2 - 0.650, p = 0.0129
summary(glm(hosp_reqd ~ high_NO2, data = results,
            family = "binomial"))

# NOx - 0.650, p = 0.0129
summary(glm(hosp_reqd ~ high_NOx, data = results,
            family = "binomial"))

# PM10 - 0.527, p = 0.0236
summary(glm(hosp_reqd ~ high_PM10, data = results,
            family = "binomial"))

# PM25 - NS
summary(glm(hosp_reqd ~ high_PM25, data = results,
            family = "binomial"))

# Adult Skills Sub-domain Score - NS
summary(glm(hosp_reqd ~ high_ASScore, data = results,
            family = "binomial"))

# Crime Score - 0.627, p - 0.0365
summary(glm(hosp_reqd ~ high_CriScore, data = results,
            family = "binomial"))

# Children and Young People Sub-domain Score - 0.471, p = 0.0386
summary(glm(hosp_reqd ~ high_CYPScore, data = results,
            family = "binomial"))

# Education, Skills and Training Score - 0.471, p = 0.0441
summary(glm(hosp_reqd ~ high_EduScore, data = results,
            family = "binomial"))

# Health Deprivation and Disability Score - NS
summary(glm(hosp_reqd ~ high_HDDScore, data = results,
            family = "binomial"))

# Income Deprivation Affecting Children Index (IDACI) Score - 0.471, p = 0.0386
summary(glm(hosp_reqd ~ high_IDCScore, data = results,
            family = "binomial"))

# Index of Multiple Deprivation (IMD) Score - NS
summary(glm(hosp_reqd ~ high_IMDScore, data = results,
            family = "binomial"))

# Prepare output of useful variables
results %>% 
  select(person_id, date, LSOA, hosp_reqd,
         high_NO2, high_NOx, high_PM10,
         high_CYPScore, high_EduScore,
         high_IDCScore) %>% 
  write_csv("data/new_features/air_imd.csv")

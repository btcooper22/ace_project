# Packages and functions----
require(readr)
require(caret)
require(glmnet)
require(foreach)
require(dplyr)
require(doParallel)
require(tools)
require(ROCR)
require(ResourceSelection)
require(ggplot2)
require(tidyr)
require(tibble)
require(magrittr)
require(ggpol)
require(matrixStats)
require(bayestestR)
require(stringr)

source("functions/inverse_logit.R")

# Load and prepare data-----

# Load data
results <- read.csv("data/ace_data_april21.csv")
results$id <- 1:nrow(results)

# Clean
results$hospital_reqd <- results$hospital_reqd == 1

# Scale
results[,13:16] <- scale(results[,13:16])

# Add new variables
results %<>% 
  mutate(referral_from_gp = referral_from == "gp",
         abnormal_resp_rate = apls_resp_rate_cat != "normal")

# Load air and IMD data at postcode distict level
air_database <- read_csv("spatial/area_stats/bradford_air_district.csv")
IMD_database <- read_csv("spatial/area_stats/bradford_imd_district.csv")

# Convert postcodes to lowercase with spacing
air_database$pcdhl <-  substr(air_database$pcdhl,3,4) %>% 
  str_pad(2,"left", 0) %>% 
  paste(casefold(air_database$pcdhl) %>% 
          substr(1, 2), ., sep = "")

IMD_database$pcdhl <-  substr(IMD_database$pcdhl,3,4) %>% 
  str_pad(2,"left", 0) %>% 
  paste(casefold(IMD_database$pcdhl) %>% 
          substr(1, 2), ., sep = "")

# Join air and IMD data to results
results %<>% 
  left_join(air_database, by = c("address" = "pcdhl")) %>% 
  left_join(IMD_database, by = c("address" = "pcdhl")) %>% 
  na.omit()

results[,36:60] <- scale(results[,36:60])

# Rebuild rare cases
results$gut_feeling <- ifelse(results$gut_feeling == "well", "good", "some concern")
results$referral_time <- ifelse(results$referral_time == "morning",
                                "morning", "afternoon_evening")
results$age_range <- ifelse(results$age_range == "pre_school",
                                "pre_school", "primary_secondary")
results$ace_resp_rate_cat <- ifelse(results$ace_resp_rate_cat == "normal", "normal", "abnormal")
results$ace_heart_rate_cat <- ifelse(results$ace_heart_rate_cat == "normal", "normal", "abnormal")
results$apls_heart_rate_cat <- ifelse(results$apls_heart_rate_cat == "normal", "normal", "abnormal")
results$apls_resp_rate_cat <- ifelse(results$ace_resp_rate_cat == "normal", "normal", "abnormal")

# Build vector of acceptable features
feature_id <- c(2, 5, 7, 10:21, 23:31, 34:60)
names(results)[feature_id]

# Bootstrap variable selection----

# Set parameters
nboot <- 1000

# Prepare parallel options
ptm <- proc.time()
psnice(value = 19)
n_cores <- 15
cl <- makeCluster(ifelse(detectCores() <= n_cores,
                         detectCores() - 1,
                         n_cores))
registerDoParallel(cl)

# Loop for bootstraps
boot_results <- foreach(i = 1:nboot, .combine = "rbind",
                        .packages = c("dplyr", "caret",
                                      "glmnet", "foreach",
                                      "ROCR", "magrittr")) %dopar%
  {
    set.seed(i)
    
    # Split data
    train_df <- results %>% 
      group_by(hospital_reqd) %>% 
      slice_sample(prop = 0.66)
    
    valid_df <- results %>% 
      filter(id %in% train_df$id == FALSE)
    
    # Trim to acceptable features
    train_df %<>% 
      select(all_of(feature_id))
    
    valid_df %<>% 
      select(all_of(feature_id))
    
    # Remove variables with incompatible factor levels
    train_levels <- apply(train_df, 2, function(x){length(unique(x))})
    valid_levels <- apply(valid_df, 2, function(x){length(unique(x))})
    true_levels <- apply(results, 2, function(x){length(unique(x))})
    var_class <- lapply(results, class)
    
    compatible_list <- (train_levels == true_levels[feature_id] & 
        valid_levels == true_levels[feature_id]) | 
      var_class[feature_id] %in% c("numeric", "integer")
    
    train_df <- train_df[,compatible_list]
    valid_df <- valid_df[,compatible_list]
    
    # Build model components
    x <- model.matrix(hospital_reqd~., train_df)[,-1]
    y <- train_df$hospital_reqd
    
    # Build model range
    model_profile <-  glmnet(x, y, alpha = 1, nlambda = 100,
                              family = "binomial")
    
    # Predict from models
    predictions <- predict(model_profile, newx = model.matrix(hospital_reqd~., valid_df)[,-1])
    
    pred_matrix <- foreach(j = 1:ncol(predictions), .combine = "rbind") %do%
      {
        # Measure AUC
        pred <- prediction(predictions[,j], valid_df$hospital_reqd)
        AUC <- performance(pred, measure = "auc")@y.values[[1]]
        
        # Return AUC and lambda
        data.frame(j, AUC, lambda = model_profile$lambda[j])
      }
    
    # Rebuild best model
    best_model <-  glmnet(x, y, alpha = 1, family = "binomial",
                             lambda = pred_matrix$lambda[which.max(pred_matrix$AUC)])
    
    # Output
    output <- as.matrix(best_model$beta) %>% 
      as.data.frame()
    output$var <- rownames(output)
    output$iteration <- i
    output$AUC <- max(pred_matrix$AUC)
    output
  }
stopCluster(cl)
proc.time() - ptm

write_rds(boot_results, "analysis/bootstrap_building.RDS",
          compress = "gz")

# Identify best variables----

# Identify iterations with best performance
best_results <- boot_results %>% 
  select(iteration, AUC) %>% 
  group_by(iteration) %>% 
  summarise(AUC = median(AUC)) %>% 
  filter(AUC > quantile(AUC, 0.75)) %>% 
  left_join(boot_results)

# Identify retained variables
best_results %>%
  group_by(var) %>% 
  summarise(prop = mean(s0 != 0)) %>% 
  #filter(prop > 0.8) %>% 
  mutate(prop = round(prop, 2)) %>% 
  arrange(desc(prop)) %>% 
  slice_head(n = 10) %>% 
  as.data.frame()

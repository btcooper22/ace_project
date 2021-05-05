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
require(stringr)

source("functions/inverse_logit.R")

# Load and bind data----

# Load data
results <- read.csv("data/ace_data_april21.csv")
results$id <- 1:nrow(results)

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

# Prepare data for modelling----

# Build vector of acceptable features
feature_id <- c(2, 34:58)
names(results)[feature_id]

# Clean
results$hospital_reqd <- results$hospital_reqd == 1

# Scale
results$ox_sat <- scale(results$ox_sat)
results$temp <- scale(results$temp)
results$resp_rate <- scale(results$resp_rate)
results$heart_rate <- scale(results$heart_rate)
results[,34:58] <- scale(results[,34:58])

# Add new variables
results %<>% 
  mutate(referral_from_gp = referral_from == "gp",
         abnormal_resp_rate = apls_resp_rate_cat != "normal")

# Build model and predict-----

# Select predictor and outcome vectors
x <- model.matrix(hospital_reqd~., results[,feature_id])[,-1]
y <- results$hospital_reqd

# Determine lambda
cv <- cv.glmnet(x, y, alpha = 1,
                family = "binomial", type.measure = "auc")

# Fit model
model_fit <- glmnet(x, y, alpha = 1, lambda = seq(0.005, 1e-05,
                                                  length.out = 100),
                     family = "binomial")

predictions <- predict(model_fit, newx = x)

# Assess predictions----

pred_matrix <- foreach(i = 1:ncol(predictions), .combine = "rbind") %do%
  {
    # Measure AUC
    pred <- prediction(predictions[,i],y)
    AUC <- performance(pred, measure = "auc")@y.values[[1]]
    
    # Return AUC and lambda
    data.frame(i, AUC, lambda = model_fit$lambda[i])
  }
plot(pred_matrix$i, pred_matrix$AUC)

model_fit <- glmnet(x, y, alpha = 1, lambda = 0.0023790,
                    family = "binomial")
model_fit$beta

# Full modelling ----

# Build full model
model_full <- glm(hospital_reqd ~ ox_sat + mentions_asthma +
                        referral_from_gp + mentions_salbutamol +
                        illness_severity + abnormal_resp_rate,
                      data = results, family = "binomial")

# Measure AUC
pred <- prediction(predict(model_full, newdata = results),y)
performance(pred, measure = "auc")@y.values[[1]]

# Build additional variables model
model_additional <- glm(hospital_reqd ~ ox_sat + mentions_asthma +
                    referral_from_gp + mentions_salbutamol +
                    illness_severity + abnormal_resp_rate +
                    NO2 + DepChi + IMDScore,
                  data = results, family = "binomial")
summary(model_additional)

# Measure AUC
pred <- prediction(predict(model_additional, newdata = results),y)
performance(pred, measure = "auc")@y.values[[1]]

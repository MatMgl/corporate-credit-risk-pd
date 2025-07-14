library(readxl)
library(ggplot2)
library(caret)
library(scorecard)
library(dplyr)
library(pROC)
library(forcats)
library(pheatmap)



#===== Dataset =====

# Initialize results data frame
final_results <- data.frame(
  Model = character(),
  AUC = numeric(),
  KS = numeric(),
  Accuracy = numeric(),
  F1 = numeric(),
  stringsAsFactors = FALSE
)

# import data
data = read_excel("credit-risk_data.xls")
summary(data)

# customer_id does not affect the probability of default
id <- which(colnames(data)=="CUSTOMER_ID")
data <- data[-id] 

# we can see that last 3 row contain NA value, so we delete them from our data
data <- data[-5802:-5804,] 
sum(is.na(data)) # we have clear data without NA values

attach(data)

# categorical variables
table(ASSESSMENT_YEAR)
table(GROUP_FLAG)
table(INDUSTRY)
table(DEFAULT_FLAG)

data$ASSESSMENT_YEAR <- as.factor(ASSESSMENT_YEAR)
data$GROUP_FLAG <- as.factor(GROUP_FLAG)
data$INDUSTRY<- as.factor(INDUSTRY)
data$DEFAULT_FLAG <- as.factor(DEFAULT_FLAG)

# to better plots
data$INDUSTRY <- fct_recode(data$INDUSTRY, 
                            "A,L&F" = "Agriculture, Livestock and Fisheries",
                            "EI" = "Extractive Industries",
                            "M" = "Manufacturing",
                            "O" = "Other",
                            "T" = "Trade",
                            "EG&W" = "Electricity, Gas and Water",
                            "H&L" = "Hotels and Leisure",
                            "OM&CI" = "Office Machinery and Computer Industries",
                            "P&CS" = "Property and Construction Sectors",
                            "TS&CI" = "Transport, Storage and Communications Infrastructure")



#===== Data visualization =====
# correlation matrix between all variables
corr_matrix <- cor(data[,c("PRODUCT_DEMAND","OWNERS_MANAGEMENT", 
                           "ACCESS_CREDIT", "PROFITABILITY", "SHORT_TERM_LIQUIDITY", 
                           "MEDIUM_TERM_LIQUIDITY", "TURNOVER")], 
                   use = "complete.obs", method = "pearson")

pheatmap(corr_matrix, cluster_rows = FALSE, cluster_cols = FALSE, display_numbers = TRUE)


# DEFAULT_FLAG
ggplot(data, aes(x = factor(DEFAULT_FLAG), fill = factor(DEFAULT_FLAG))) +
  geom_bar(color = "black") +
  scale_fill_manual(
    values = c("steelblue", "tomato"),
    labels = c("0 = No Default", "1 = Default")
  ) +
  theme_minimal() +
  labs(
    title = "Default Status Distribution",
    x = "Default Flag",
    y = "Count"
  ) +
  theme(
    text = element_text(size = 11),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )


# ASSESSMENT_YEAR
ggplot(data, aes(x = factor(ASSESSMENT_YEAR), fill = factor(DEFAULT_FLAG))) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Assessment Year by Default Status", x = "Assessment Year", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# PRODUCT_DEMAND
ggplot(data, aes(x = PRODUCT_DEMAND, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Product Demand by Default Status", x = "Product Demand Score", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# OWNERS_MANAGEMENT
ggplot(data, aes(x = OWNERS_MANAGEMENT, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Owners Management by Default Status", x = "Owner Management Score", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# ACCESS_CREDIT
ggplot(data, aes(x = ACCESS_CREDIT, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Access to Credit by Default Status", x = "Access to Credit Score", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# PROFITABILITY
ggplot(data, aes(x = PROFITABILITY, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Profitability by Default Status", x = "Profitability Score", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# SHORT_TERM_LIQUIDITY
ggplot(data, aes(x = SHORT_TERM_LIQUIDITY, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Short-Term Liquidity by Default Status", x = "Short-Term Liquidity", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# MEDIUM_TERM_LIQUIDITY
ggplot(data, aes(x = MEDIUM_TERM_LIQUIDITY, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Medium-Term Liquidity by Default Status", x = "Medium-Term Liquidity", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# GROUP_FLAG
ggplot(data, aes(x = factor(GROUP_FLAG), fill = factor(DEFAULT_FLAG))) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Group Flag by Default Status", x = "Group Flag (0/1)", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# TURNOVER
ggplot(data, aes(x = TURNOVER, fill = factor(DEFAULT_FLAG))) +
  geom_histogram(position = "dodge", bins = 30, color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Turnover by Default Status", x = "Turnover", y = "Count") +
  theme(text = element_text(size = 11), plot.title = element_text(face = "bold"), legend.position = "none")


# INDUSTRY
ggplot(data, aes(x = INDUSTRY, fill = factor(DEFAULT_FLAG))) +
  geom_bar(position = "dodge", color = "black") +
  scale_fill_manual(values = c("steelblue", "tomato")) +
  theme_minimal() +
  labs(title = "Industry by Default Status", x = "Industry", y = "Count") +
  theme(
    text = element_text(size = 11),
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold"),
    legend.position = "none"
  )



#===== Split to train and test =====
dt_split <- split_df(data, y=NULL, ratio=c(0.8, 0.2), seed=1906) 
dt_train <- dt_split$train
dt_test <- dt_split$test



#===== Variables selection =====
# Information value for all train variables
iv(dt_train, y="DEFAULT_FLAG")

data_train_sel <- var_filter(dt_train, "DEFAULT_FLAG", return_rm_reason = TRUE)

# What is removed and why?
data_train_sel$rm

# Final train dataset
data_train_sel <- var_filter(dt_train, "DEFAULT_FLAG", return_rm_reason = FALSE)
summary(data_train_sel)

# Problem: default flag as character
data_train_sel$DEFAULT_FLAG <- as.factor(data_train_sel$DEFAULT_FLAG)
summary(data_train_sel)

# Test dataset with same variables
data_test_sel <- subset(dt_test, select=colnames(data_train_sel))
summary(data_test_sel)



#===== Binning for the variables =====
bins <- woebin(data_train_sel, "DEFAULT_FLAG") 

# Plots of results
woebin_plot(bins)

# Train dataset with bins
data_train_woe <- woebin_ply(data_train_sel, bins)
data_test_woe <- woebin_ply(data_test_sel, bins)

summary(data_train_woe)
summary(data_test_woe)



#===== Logistic regression =====
##===== Models lg =====

# null_model 
null_lg<- glm(DEFAULT_FLAG ~ 1, data = data_train_sel, family = binomial)
summary(null_lg)

# full_model 
model_1_lg <- glm(DEFAULT_FLAG ~ ., data= data_train_sel, family = binomial)
summary(model_1_lg)

# full_model without turnover
model_2_lg <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR + PRODUCT_DEMAND + OWNERS_MANAGEMENT + 
                    ACCESS_CREDIT + PROFITABILITY + SHORT_TERM_LIQUIDITY + MEDIUM_TERM_LIQUIDITY + 
                    INDUSTRY, data= data_train_sel, family = binomial)
summary(model_2_lg)

# forward
model_3_lg <- step(null_lg, direction = "forward", scope = formula(model_1_lg))
summary(model_3_lg)

# backward
model_4_lg <- step(model_1_lg, direction = "backward")
summary(model_4_lg)


# same models?
formula(model_3_lg)
formula(model_4_lg)
# yes -> skip model 4


##===== Evaluation LG - 10-fold Cross Validation =====

# List of logistic regression models to evaluate
model_list <- list(model_1_lg, model_2_lg, model_3_lg)
model_names <- paste0("Model_", 1:length(model_list), "_lg")

# Initialize results data frame
results <- data.frame(Model = model_names, AUC = NA, KS = NA, Accuracy = NA, F1 = NA)

# Create stratified 10-folds
set.seed(1906)
folds <- createFolds(data_train_sel$DEFAULT_FLAG, k = 10, list = TRUE)

# Loop through each model
for (m in 1:length(model_list)) {
  auc_vals <- c()
  ks_vals <- c()
  acc_vals <- c()
  f1_vals  <- c()
  
  for (k in 1:10) {
    test_indices <- folds[[k]]
    data_test <- data_train_sel[test_indices, ]
    data_train <- data_train_sel[-test_indices, ]
    
    # Refit model on training fold
    fitted_model <- glm(formula(model_list[[m]]), data = data_train, family = binomial())
    
    # Predict probabilities
    predict_prob <- predict(fitted_model, newdata = data_test, type = "response")
    
    # Convert label to numeric 0/1
    label <- as.numeric(as.character(data_test$DEFAULT_FLAG))
    
    # Evaluate performance using perf_eva (suppress messages)
    eva <- suppressMessages(suppressWarnings(
      perf_eva(pred = predict_prob, label = label, show_plot = FALSE)
    ))
    
    # Extract AUC and KS from perf_eva
    auc_val <- eva$binomial_metric$dat$AUC
    ks_val  <- eva$binomial_metric$dat$KS
    
    # Get optimal threshold using Youden index
    roc_obj <- suppressMessages(suppressWarnings(
      roc(label, predict_prob)
    ))
    thresh_df <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = TRUE)
    thresh <- as.numeric(thresh_df[1])
    
    # Classify using threshold
    predict_class <- factor(ifelse(predict_prob >= thresh, 1, 0), levels = c(0, 1))
    truth <- factor(label, levels = c(0, 1))
    
    # Confusion matrix
    cm <- confusionMatrix(predict_class, truth)
    
    # Store metrics
    auc_vals <- c(auc_vals, auc_val)
    ks_vals  <- c(ks_vals, ks_val)
    acc_vals <- c(acc_vals, cm$overall["Accuracy"])
    f1_vals  <- c(f1_vals, cm$byClass["F1"])
  }
  
  # Save average metrics across 10 folds
  results$AUC[m] <- mean(auc_vals)
  results$KS[m] <- mean(ks_vals)
  results$Accuracy[m] <- mean(acc_vals)
  results$F1[m] <- mean(f1_vals)
}

# Final results
models_lg <- results
models_lg


##===== Models WOE =====

# null_model 
null_lg_woe<- glm(DEFAULT_FLAG ~ 1, data = data_train_woe, family = binomial)
summary(null_lg_woe)

# full_model 
model_1_lg_woe <- glm(DEFAULT_FLAG ~ ., data= data_train_woe, family = binomial())
summary(model_1_lg_woe)

# full_model without turnover
model_2_lg_woe <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR_woe + PRODUCT_DEMAND_woe + OWNERS_MANAGEMENT_woe + 
                    ACCESS_CREDIT_woe + PROFITABILITY_woe + SHORT_TERM_LIQUIDITY_woe + MEDIUM_TERM_LIQUIDITY_woe + 
                    INDUSTRY_woe, data= data_train_woe, family = binomial())
summary(model_2_lg_woe)

# forward
model_3_lg_woe <- step(null_lg_woe, direction = "forward", scope = formula(model_1_lg_woe))
summary(model_3_lg_woe)

# backward
model_4_lg_woe <- step(model_1_lg_woe, direction = "backward")
summary(model_4_lg_woe)


# same models?
formula(model_3_lg_woe)
formula(model_4_lg_woe)
# yes -> again skip 4


##===== Evaluation WOE - 10-fold Cross Validation =====
# List of WOE-based logistic models
model_list_woe <- list(model_1_lg_woe,model_2_lg_woe,model_3_lg_woe)
model_names_woe <- paste0("Model_", 1:length(model_list_woe),"_lg_woe")

# Data frame for results
results_woe <- data.frame(Model = model_names_woe, AUC = NA, KS = NA, Accuracy = NA, F1 = NA)

# Create stratified 10-folds
set.seed(1906)
folds <- createFolds(data_train_woe$DEFAULT_FLAG, k = 10, list = TRUE)

# Loop through each model
for (m in 1:length(model_list_woe)) {
  auc_vals <- c()
  ks_vals <- c()
  acc_vals <- c()
  f1_vals  <- c()
  
  for (k in 1:10) {
    test_indices <- folds[[k]]
    data_test <- data_train_woe[test_indices, ]
    data_train <- data_train_woe[-test_indices, ]
    
    # Fit model on training fold
    fitted_model <- glm(formula(model_list_woe[[m]]), data = data_train, family = binomial())
    
    # Predict probabilities
    predict_prob <- predict(fitted_model, newdata = data_test, type = "response")
    
    # Convert label to numeric
    label <- as.numeric(as.character(data_test$DEFAULT_FLAG))
    
    # Evaluate AUC and KS with scorecard
    eva <- suppressMessages(suppressWarnings(
      perf_eva(pred = predict_prob, label = label, show_plot = FALSE)
    ))
    auc_val <- eva$binomial_metric$dat$AUC
    ks_val <- eva$binomial_metric$dat$KS
    
    # Optimal threshold using Youden index
    roc_obj <- suppressMessages(suppressWarnings(
      roc(label, predict_prob)
    ))
    thresh_df <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = TRUE)
    thresh <- as.numeric(thresh_df[1])
    
    # Classify with threshold
    predict_class <- factor(ifelse(predict_prob >= thresh, 1, 0), levels = c(0, 1))
    truth <- factor(label, levels = c(0, 1))
    
    # Confusion matrix
    cm <- confusionMatrix(predict_class, truth)
    
    # Store fold results
    auc_vals <- c(auc_vals, auc_val)
    ks_vals  <- c(ks_vals, ks_val)
    acc_vals <- c(acc_vals, cm$overall["Accuracy"])
    f1_vals  <- c(f1_vals, cm$byClass["F1"])
  }

  # Store average metrics for model
  results_woe$AUC[m] <- mean(auc_vals)
  results_woe$KS[m] <- mean(ks_vals)
  results_woe$Accuracy[m] <- mean(acc_vals)
  results_woe$F1[m] <- mean(f1_vals)
}

# Final results
models_lg_woe <- results_woe
models_lg_woe


##===== Comparision =====
models_lg
# f1: best model_2_lg

models_lg_woe
# f1: best model_2_lg_woe
# better than model without binning


# this two models with test data

###===== Model LG =====
# Predict probabilities
pred_lg <- predict(model_2_lg, newdata = data_test_sel, type = "response")

# Convert label to numeric (0/1)
label_lg <- as.numeric(as.character(data_test_sel$DEFAULT_FLAG))

# Evaluate AUC and KS using scorecard
eva_lg <- perf_eva(
  pred = pred_lg,
  label = label_lg,
  show_plot = c("roc", "ks")
)

# Extract AUC and KS
auc_lg <- eva_lg$binomial_metric$dat$AUC
ks_lg  <- eva_lg$binomial_metric$dat$KS

# Get optimal threshold using Youden index
roc_obj <- roc(label_lg, pred_lg)
thresh <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Predict classes using threshold
predict_class <- factor(ifelse(pred_lg >= thresh[[1]], 1, 0))

# Confusion matrix
cm <- confusionMatrix(predict_class, data_test_sel$DEFAULT_FLAG)

# Extract Accuracy and F1
acc_lg <- cm$overall["Accuracy"]
f1_lg  <- cm$byClass["F1"]
 
# Add results to final_results
final_results <- rbind(
  final_results,
  data.frame(
    Model = "model_lg",
    AUC = auc_lg,
    KS = ks_lg,
    Accuracy = as.numeric(acc_lg),
    F1 = as.numeric(f1_lg)
  )
)

cm_lg <- cm

###===== Model LG WOE =====

# Predict probabilities
pred_lg_woe <- predict(model_2_lg_woe, newdata = data_test_woe, type = "response")

# Convert label to numeric (0/1)
label_lg_woe <- as.numeric(as.character(data_test_woe$DEFAULT_FLAG))

# Evaluate AUC and KS using scorecard
eva_lg_woe <- perf_eva(
  pred = pred_lg_woe,
  label = label_lg_woe,
  show_plot = c("roc", "ks")
)

# Extract AUC and KS
auc_lg_woe <- eva_lg_woe$binomial_metric$dat$AUC
ks_lg_woe  <- eva_lg_woe$binomial_metric$dat$KS

# Get optimal threshold using Youden index
roc_obj_woe <- roc(label_lg_woe, pred_lg_woe)
thresh_woe <- coords(roc_obj_woe, "best", ret = "threshold", best.method = "youden")

# Predict classes using threshold
predict_class_woe <- factor(ifelse(pred_lg_woe >= thresh_woe[[1]], 1, 0), levels = c(0, 1))
truth_woe <- factor(data_test_woe$DEFAULT_FLAG, levels = c(0, 1))

# Confusion matrix
cm_woe <- confusionMatrix(predict_class_woe, truth_woe)

# Extract Accuracy and F1
acc_lg_woe <- cm_woe$overall["Accuracy"]
f1_lg_woe  <- cm_woe$byClass["F1"]

# Add results to final_results
final_results <- rbind(
  final_results,
  data.frame(
    Model = "model_lg_woe",
    AUC = auc_lg_woe,
    KS = ks_lg_woe,
    Accuracy = as.numeric(acc_lg_woe),
    F1 = as.numeric(f1_lg_woe)
  )
)

cm_lg_woe <- cm_woe


###===== Results =====
final_results
#cm_lg
#cm_lg_woe



#===== Probit =====
##===== Models Probit =====

# null_model 
null_pb<- glm(DEFAULT_FLAG ~ 1, data = data_train_sel, family = binomial(link = "probit"))
summary(null_pb)

# full_model 
model_1_pb <- glm(DEFAULT_FLAG ~ ., data= data_train_sel, family = binomial(link = "probit"))
summary(model_1_pb)

# full_model without turnover
model_2_pb <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR + PRODUCT_DEMAND + OWNERS_MANAGEMENT + 
                    ACCESS_CREDIT + PROFITABILITY + SHORT_TERM_LIQUIDITY + MEDIUM_TERM_LIQUIDITY + 
                    INDUSTRY, data= data_train_sel, family = binomial(link = "probit"))
summary(model_2_pb)

# forward
model_3_pb <- step(null_pb, direction = "forward", scope = formula(model_1_pb))
summary(model_3_pb)

# backward
model_4_pb <- step(model_1_pb, direction = "backward")
summary(model_4_pb)


# same models?
formula(model_3_pb)
formula(model_4_pb)
# no -> we do not skip model 4


##===== Evaluation PB - 10-fold Cross Validation =====

# List of logistic regression models to evaluate
model_list <- list(model_1_pb, model_2_pb, model_3_pb,model_4_pb)
model_names <- paste0("Model_", 1:length(model_list), "_pb")

# Initialize results data frame
results <- data.frame(Model = model_names, AUC = NA, KS = NA, Accuracy = NA, F1 = NA)

# Create stratified 10-folds
set.seed(1906)
folds <- createFolds(data_train_sel$DEFAULT_FLAG, k = 10, list = TRUE)

# Loop through each model
for (m in 1:length(model_list)) {
  auc_vals <- c()
  ks_vals <- c()
  acc_vals <- c()
  f1_vals  <- c()
  
  for (k in 1:10) {
    test_indices <- folds[[k]]
    data_test <- data_train_sel[test_indices, ]
    data_train <- data_train_sel[-test_indices, ]
    
    # Refit model on training fold
    fitted_model <- glm(formula(model_list[[m]]), data = data_train, family = binomial(link = "probit"))
    
    # Predict probabilities
    predict_prob <- predict(fitted_model, newdata = data_test, type = "response")
    
    # Convert label to numeric 0/1
    label <- as.numeric(as.character(data_test$DEFAULT_FLAG))
    
    # Evaluate performance using perf_eva (suppress messages)
    eva <- suppressMessages(suppressWarnings(
      perf_eva(pred = predict_prob, label = label, show_plot = FALSE)
    ))
    
    # Extract AUC and KS from perf_eva
    auc_val <- eva$binomial_metric$dat$AUC
    ks_val  <- eva$binomial_metric$dat$KS
    
    # Get optimal threshold using Youden index
    roc_obj <- suppressMessages(suppressWarnings(
      roc(label, predict_prob)
    ))
    thresh_df <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = TRUE)
    thresh <- as.numeric(thresh_df[1])
    
    # Classify using threshold
    predict_class <- factor(ifelse(predict_prob >= thresh, 1, 0), levels = c(0, 1))
    truth <- factor(label, levels = c(0, 1))
    
    # Confusion matrix
    cm <- confusionMatrix(predict_class, truth)
    
    # Store metrics
    auc_vals <- c(auc_vals, auc_val)
    ks_vals  <- c(ks_vals, ks_val)
    acc_vals <- c(acc_vals, cm$overall["Accuracy"])
    f1_vals  <- c(f1_vals, cm$byClass["F1"])
  }
  
  # Save average metrics across 10 folds
  results$AUC[m] <- mean(auc_vals)
  results$KS[m] <- mean(ks_vals)
  results$Accuracy[m] <- mean(acc_vals)
  results$F1[m] <- mean(f1_vals)
}

# Final results
models_pb <- results
models_pb


##===== Models Probit WOE =====

# null_model 
null_pb_woe<- glm(DEFAULT_FLAG ~ 1, data = data_train_woe, family = binomial(link="probit"))
summary(null_pb_woe)

# full_model 
model_1_pb_woe <- glm(DEFAULT_FLAG ~ ., data= data_train_woe, family = binomial(link="probit"))
summary(model_1_pb_woe)

# full_model without turnover
model_2_pb_woe <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR_woe + PRODUCT_DEMAND_woe + OWNERS_MANAGEMENT_woe + 
                        ACCESS_CREDIT_woe + PROFITABILITY_woe + SHORT_TERM_LIQUIDITY_woe + MEDIUM_TERM_LIQUIDITY_woe + 
                        INDUSTRY_woe, data= data_train_woe, family = binomial(link="probit"))
summary(model_2_pb_woe)

# forward
model_3_pb_woe <- step(null_pb_woe, direction = "forward", scope = formula(model_1_pb_woe))
summary(model_3_pb_woe)

# backward
model_4_pb_woe <- step(model_1_pb_woe, direction = "backward")
summary(model_4_pb_woe)


# same models?
formula(model_3_pb_woe)
formula(model_4_pb_woe)
# yes -> again skip 4


##===== Evaluation PB WOE - 10-fold Cross Validation =====
# List of WOE-based logistic models
model_list_woe <- list(model_1_pb_woe,model_2_pb_woe,model_3_pb_woe)
model_names_woe <- paste0("Model_", 1:length(model_list_woe),"_pb_woe")

# Data frame for results
results_woe <- data.frame(Model = model_names_woe, AUC = NA, KS = NA, Accuracy = NA, F1 = NA)

# Create stratified 10-folds
set.seed(1906)
folds <- createFolds(data_train_woe$DEFAULT_FLAG, k = 10, list = TRUE)

# Loop through each model
for (m in 1:length(model_list_woe)) {
  auc_vals <- c()
  ks_vals <- c()
  acc_vals <- c()
  f1_vals  <- c()
  
  for (k in 1:10) {
    test_indices <- folds[[k]]
    data_test <- data_train_woe[test_indices, ]
    data_train <- data_train_woe[-test_indices, ]
    
    # Fit model on training fold
    fitted_model <- glm(formula(model_list_woe[[m]]), data = data_train, family = binomial(link = "probit"))
    
    # Predict probabilities
    predict_prob <- predict(fitted_model, newdata = data_test, type = "response")
    
    # Convert label to numeric
    label <- as.numeric(as.character(data_test$DEFAULT_FLAG))
    
    # Evaluate AUC and KS with scorecard
    eva <- suppressMessages(suppressWarnings(
      perf_eva(pred = predict_prob, label = label, show_plot = FALSE)
    ))
    auc_val <- eva$binomial_metric$dat$AUC
    ks_val <- eva$binomial_metric$dat$KS
    
    # Optimal threshold using Youden index
    roc_obj <- suppressMessages(suppressWarnings(
      roc(label, predict_prob)
    ))
    thresh_df <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = TRUE)
    thresh <- as.numeric(thresh_df[1])
    
    # Classify with threshold
    predict_class <- factor(ifelse(predict_prob >= thresh, 1, 0), levels = c(0, 1))
    truth <- factor(label, levels = c(0, 1))
    
    # Confusion matrix
    cm <- confusionMatrix(predict_class, truth)
    
    # Store fold results
    auc_vals <- c(auc_vals, auc_val)
    ks_vals  <- c(ks_vals, ks_val)
    acc_vals <- c(acc_vals, cm$overall["Accuracy"])
    f1_vals  <- c(f1_vals, cm$byClass["F1"])
  }
  
  # Store average metrics for model
  results_woe$AUC[m] <- mean(auc_vals)
  results_woe$KS[m] <- mean(ks_vals)
  results_woe$Accuracy[m] <- mean(acc_vals)
  results_woe$F1[m] <- mean(f1_vals)
}

# Final results
models_pb_woe <- results_woe
models_pb_woe


##===== Comparision =====
models_pb
# f1: best model_2_lg

models_pb_woe
# f1: best model_2_lg_woe
# better than model without binning


# this two models with test data

###===== Model PB =====
# Predict probabilities
pred_pb <- predict(model_2_pb, newdata = data_test_sel, type = "response")

# Convert label to numeric (0/1)
label_pb <- as.numeric(as.character(data_test_sel$DEFAULT_FLAG))

# Evaluate AUC and KS using scorecard
eva_pb <- perf_eva(
  pred = pred_pb,
  label = label_pb,
  show_plot = c("roc", "ks")
)

# Extract AUC and KS
auc_pb <- eva_pb$binomial_metric$dat$AUC
ks_pb  <- eva_pb$binomial_metric$dat$KS

# Get optimal threshold using Youden index
roc_obj <- roc(label_pb, pred_pb)
thresh <- coords(roc_obj, "best", ret = "threshold", best.method = "youden")

# Predict classes using threshold
predict_class <- factor(ifelse(pred_pb >= thresh[[1]], 1, 0))

# Confusion matrix
cm <- confusionMatrix(predict_class, data_test_sel$DEFAULT_FLAG)

# Extract Accuracy and F1
acc_pb <- cm$overall["Accuracy"]
f1_pb  <- cm$byClass["F1"]

# Add results to final_results
final_results <- rbind(
  final_results,
  data.frame(
    Model = "model_pb",
    AUC = auc_pb,
    KS = ks_pb,
    Accuracy = as.numeric(acc_pb),
    F1 = as.numeric(f1_pb)
  )
)

cm_pb <- cm

###===== Model PB WOE =====

# Predict probabilities
pred_pb_woe <- predict(model_2_pb_woe, newdata = data_test_woe, type = "response")

# Convert label to numeric (0/1)
label_pb_woe <- as.numeric(as.character(data_test_woe$DEFAULT_FLAG))

# Evaluate AUC and KS using scorecard
eva_pb_woe <- perf_eva(
  pred = pred_pb_woe,
  label = label_pb_woe,
  show_plot = c("roc", "ks")
)

# Extract AUC and KS
auc_pb_woe <- eva_pb_woe$binomial_metric$dat$AUC
ks_pb_woe  <- eva_pb_woe$binomial_metric$dat$KS

# Get optimal threshold using Youden index
roc_obj_woe <- roc(label_pb_woe, pred_pb_woe)
thresh_woe <- coords(roc_obj_woe, "best", ret = "threshold", best.method = "youden")

# Predict classes using threshold
predict_class_woe <- factor(ifelse(pred_pb_woe >= thresh_woe[[1]], 1, 0), levels = c(0, 1))
truth_woe <- factor(data_test_woe$DEFAULT_FLAG, levels = c(0, 1))

# Confusion matrix
cm_woe <- confusionMatrix(predict_class_woe, truth_woe)

# Extract Accuracy and F1
acc_pb_woe <- cm_woe$overall["Accuracy"]
f1_pb_woe  <- cm_woe$byClass["F1"]

# Add results to final_results
final_results <- rbind(
  final_results,
  data.frame(
    Model = "model_pb_woe",
    AUC = auc_pb_woe,
    KS = ks_pb_woe,
    Accuracy = as.numeric(acc_pb_woe),
    F1 = as.numeric(f1_pb_woe)
  )
)

cm_pb_woe <- cm_woe


###===== Results =====
final_results
# cm_pb
# cm_pb_woe



#===== Linear Regression =====
##===== Linear Regression Models =====
# Convert DEFAULT_FLAG to numeric (required for linear regression)
data_train_sel$DEFAULT_FLAG <- as.numeric(as.character(data_train_sel$DEFAULT_FLAG))

# null_model
null_lm <- glm(DEFAULT_FLAG ~ 1, data = data_train_sel, family = gaussian())
summary(null_lm)

# full_model
model_1_lm <- glm(DEFAULT_FLAG ~ ., data = data_train_sel, family = gaussian())
summary(model_1_lm)

# full_model without turnover
model_2_lm <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR + PRODUCT_DEMAND + OWNERS_MANAGEMENT + 
                    ACCESS_CREDIT + PROFITABILITY + SHORT_TERM_LIQUIDITY + 
                    MEDIUM_TERM_LIQUIDITY + INDUSTRY,
                  data = data_train_sel, family = gaussian())
summary(model_2_lm)

# forward
model_3_lm <- step(null_lm, direction = "forward", scope = formula(model_1_lm))
summary(model_3_lm)

# backward
model_4_lm <- step(model_1_lm, direction = "backward")
summary(model_4_lm)

# same models?
formula(model_3_lm)
formula(model_4_lm)
# yes -> skip model 4


##===== Cross-validation for Linear Regression Models =====
# Models to evaluate
lm_model_list <- list(model_1_lm, model_2_lm, model_3_lm)
lm_model_names <- paste0("Model_", 1:length(lm_model_list), "_lm")

# Create stratified folds
set.seed(1906)
folds_lm <- createFolds(data_train_sel$DEFAULT_FLAG, k = 10, list = TRUE)

# Initialize results table
results_lm_classif <- data.frame(Model = lm_model_names, AUC = NA, KS = NA, Accuracy = NA, F1 = NA)

# Cross-validation loop
for (m in 1:length(lm_model_list)) {
  auc_vals <- c()
  ks_vals  <- c()
  acc_vals <- c()
  f1_vals  <- c()
  
  for (k in 1:10) {
    test_indices <- folds_lm[[k]]
    data_test <- data_train_sel[test_indices, ]
    data_train <- data_train_sel[-test_indices, ]
    
    # Refit model on current fold
    fitted_model <- glm(formula(lm_model_list[[m]]), data = data_train, family = gaussian())
    
    # Predict probabilities (in [0,1] range ideally)
    pred_prob <- predict(fitted_model, newdata = data_test)
    
    # Actual labels
    label <- data_test$DEFAULT_FLAG
    
    # AUC and KS using perf_eva
    eva <- suppressMessages(suppressWarnings(
      perf_eva(pred = pred_prob, label = label, show_plot = FALSE)
    ))
    auc_val <- eva$binomial_metric$dat$AUC
    ks_val  <- eva$binomial_metric$dat$KS
    
    # Optimal threshold using Youden index
    roc_obj <- suppressMessages(suppressWarnings(roc(label, pred_prob)))
    thresh_df <- coords(roc_obj, "best", ret = "threshold", best.method = "youden", transpose = TRUE)
    threshold <- as.numeric(thresh_df[1])
    
    # Class prediction and confusion matrix
    pred_class <- factor(ifelse(pred_prob >= threshold, 1, 0), levels = c(0, 1))
    truth <- factor(label, levels = c(0, 1))
    
    cm <- confusionMatrix(pred_class, truth)
    
    # Store fold metrics
    auc_vals <- c(auc_vals, auc_val)
    ks_vals  <- c(ks_vals, ks_val)
    acc_vals <- c(acc_vals, cm$overall["Accuracy"])
    f1_vals  <- c(f1_vals, cm$byClass["F1"])
  }
  
  # Store average results
  results_lm_classif$AUC[m] <- mean(auc_vals)
  results_lm_classif$KS[m]  <- mean(ks_vals)
  results_lm_classif$Accuracy[m] <- mean(acc_vals)
  results_lm_classif$F1[m] <- mean(f1_vals)
}

# Final cross-validated classification metrics for linear models
models_lm <- results_lm_classif
models_lm
# f1: best model 3


##===== Evaluation of model_3_lm on test set =====
# Ensure test labels are numeric
data_test_sel$DEFAULT_FLAG <- as.numeric(as.character(data_test_sel$DEFAULT_FLAG))

# Predict probabilities using model_3_lm
pred_lm <- predict(model_3_lm, newdata = data_test_sel)

# Actual labels
label_lm <- data_test_sel$DEFAULT_FLAG

# Evaluate AUC and KS
eva_lm <- perf_eva(
  pred = pred_lm,
  label = label_lm,
  show_plot = c("roc", "ks")
)

# Extract AUC and KS
auc_lm <- eva_lm$binomial_metric$dat$AUC
ks_lm  <- eva_lm$binomial_metric$dat$KS

# Optimal threshold (Youden index)
roc_obj_lm <- roc(label_lm, pred_lm)
thresh_lm <- coords(roc_obj_lm, "best", ret = "threshold", best.method = "youden")

# Classify using threshold
predict_class_lm <- factor(ifelse(pred_lm >= thresh_lm[[1]], 1, 0), levels = c(0, 1))
truth_lm <- factor(label_lm, levels = c(0, 1))

# Confusion matrix
cm_lm <- confusionMatrix(predict_class_lm, truth_lm)

# Accuracy and F1
acc_lm <- cm_lm$overall["Accuracy"]
f1_lm  <- cm_lm$byClass["F1"]

# Add results to final_results
final_results <- rbind(
  final_results,
  data.frame(
    Model = "model_lm",
    AUC = auc_lm,
    KS = ks_lm,
    Accuracy = as.numeric(acc_lm),
    F1 = as.numeric(f1_lm)
  )
)

# View updated final_results
final_results
#cm_lm



#===== Experts model =====
##===== Definition of new link function to glm =====
custom_logit_01 <- function () {   
  linkfun <- function(mu) log(1 / mu - 1)
  linkinv <- function(eta) 1 / (1 + exp(-0.1 * eta))
  mu.eta  <- function(eta) 0.1 * exp(-0.1 * eta) / (1 + exp(-0.1 * eta))^2
  valideta <- function(eta) TRUE
  
  structure(list(linkfun = linkfun, linkinv = linkinv,
                 mu.eta = mu.eta, valideta = valideta,
                 name = "scaled_logit_0.1"), class = "link-glm")
}


##===== Computing scores =====
data_train <- dt_train
data_train$Score <- with(data_train,
                         0.20 * PRODUCT_DEMAND +
                           0.10 * OWNERS_MANAGEMENT +
                           0.10 * ACCESS_CREDIT +
                           0.15 * PROFITABILITY +
                           0.25 * SHORT_TERM_LIQUIDITY +
                           0.20 * MEDIUM_TERM_LIQUIDITY
)

data_test <- dt_test
data_test$Score <- with(data_test,
                        0.20 * PRODUCT_DEMAND +
                          0.10 * OWNERS_MANAGEMENT +
                          0.10 * ACCESS_CREDIT +
                          0.15 * PROFITABILITY +
                          0.25 * SHORT_TERM_LIQUIDITY +
                          0.20 * MEDIUM_TERM_LIQUIDITY
)

model_expert <- glm(DEFAULT_FLAG ~ Score, family = binomial(link = custom_logit_01()), data = data_train)
summary(model_expert)


##===== Expert model evaluation on test set =====
# Ensure DEFAULT_FLAG is numeric (for evaluation)
data_test$DEFAULT_FLAG <- as.numeric(as.character(data_test$DEFAULT_FLAG))

# Predict PD using the custom link function model
pred_expert <- predict(model_expert, newdata = data_test, type = "response")

# Actual labels
label_expert <- data_test$DEFAULT_FLAG

# Evaluate AUC and KS using perf_eva
eva_expert <- perf_eva(
  pred = pred_expert,
  label = label_expert,
  show_plot = c("roc", "ks")
)

# Extract AUC and KS
auc_expert <- eva_expert$binomial_metric$dat$AUC
ks_expert  <- eva_expert$binomial_metric$dat$KS

# Compute optimal threshold using Youden index
roc_obj_expert <- roc(label_expert, pred_expert)
thresh_expert <- coords(roc_obj_expert, "best", ret = "threshold", best.method = "youden")

# Classify based on threshold
predict_class_expert <- factor(ifelse(pred_expert >= thresh_expert[[1]], 1, 0), levels = c(0, 1))
truth_expert <- factor(label_expert, levels = c(0, 1))

# Confusion matrix
cm_expert <- confusionMatrix(predict_class_expert, truth_expert)

# Extract Accuracy and F1
acc_expert <- cm_expert$overall["Accuracy"]
f1_expert  <- cm_expert$byClass["F1"]

# Add results to final_results
final_results <- rbind(
  final_results,
  data.frame(
    Model = "expert_glm",
    AUC = auc_expert,
    KS = ks_expert,
    Accuracy = as.numeric(acc_expert),
    F1 = as.numeric(f1_expert)
  )
)

# View updated results
final_results
#cm_expert



#===== Head of the Credit request: segmentation by GROUP_FLAG =====
##===== Split data by GROUP_FLAG =====
# 0 = Independent Entity (not part of Financial Holding)
# 1 = Subsidiary of Financial Holding

# Training sets
train_independent <- subset(dt_train, GROUP_FLAG == 0)
train_subsidiary  <- subset(dt_train, GROUP_FLAG == 1)

# Test sets
test_independent <- subset(dt_test, GROUP_FLAG == 0)
test_subsidiary  <- subset(dt_test, GROUP_FLAG == 1)

##===== Build logistic regression models =====

# Use the same formula as model_2_lg (without TURNOVER)
# Formula: DEFAULT_FLAG ~ ASSESSMENT_YEAR + PRODUCT_DEMAND + ... + INDUSTRY

# Independent entity model
model_independent_lg <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR + PRODUCT_DEMAND + OWNERS_MANAGEMENT +
                              ACCESS_CREDIT + PROFITABILITY + SHORT_TERM_LIQUIDITY +
                              MEDIUM_TERM_LIQUIDITY + INDUSTRY,
                            data = train_independent, family = binomial())

summary(model_independent_lg)

# Subsidiary model (i.e. part of Financial Holding)
model_subsidiary_lg <- glm(DEFAULT_FLAG ~ ASSESSMENT_YEAR + PRODUCT_DEMAND + OWNERS_MANAGEMENT +
                             ACCESS_CREDIT + PROFITABILITY + SHORT_TERM_LIQUIDITY +
                             MEDIUM_TERM_LIQUIDITY + INDUSTRY,
                           data = train_subsidiary, family = binomial())

summary(model_subsidiary_lg)


##===== Evaluation for model_independent_lg =====

# Predict probabilities
pred_independent <- predict(model_independent_lg, newdata = test_independent, type = "response")
label_independent <- as.numeric(as.character(test_independent$DEFAULT_FLAG))

# AUC & KS
eva_independent <- perf_eva(
  pred = pred_independent,
  label = label_independent,
  show_plot = c("roc", "ks")
)
auc_indep <- eva_independent$binomial_metric$dat$AUC
ks_indep  <- eva_independent$binomial_metric$dat$KS

# Optimal threshold (Youden index)
roc_indep <- roc(label_independent, pred_independent)
thresh_indep <- coords(roc_indep, "best", ret = "threshold", best.method = "youden")[[1]]

# Classify and evaluate
predict_class_indep <- factor(ifelse(pred_independent >= thresh_indep, 1, 0), levels = c(0, 1))
truth_indep <- factor(label_independent, levels = c(0, 1))
cm_indep <- confusionMatrix(predict_class_indep, truth_indep)

acc_indep <- cm_indep$overall["Accuracy"]
f1_indep  <- cm_indep$byClass["F1"]

##===== Evaluation for model_subsidiary_lg =====

# Predict probabilities
pred_subsidiary <- predict(model_subsidiary_lg, newdata = test_subsidiary, type = "response")
label_subsidiary <- as.numeric(as.character(test_subsidiary$DEFAULT_FLAG))

# AUC & KS
eva_subsidiary <- perf_eva(
  pred = pred_subsidiary,
  label = label_subsidiary,
  show_plot = c("roc", "ks")
)
auc_sub <- eva_subsidiary$binomial_metric$dat$AUC
ks_sub  <- eva_subsidiary$binomial_metric$dat$KS

# Optimal threshold
roc_sub <- roc(label_subsidiary, pred_subsidiary)
thresh_sub <- coords(roc_sub, "best", ret = "threshold", best.method = "youden")[[1]]

# Classify and evaluate
predict_class_sub <- factor(ifelse(pred_subsidiary >= thresh_sub, 1, 0), levels = c(0, 1))
truth_sub <- factor(label_subsidiary, levels = c(0, 1))
cm_sub <- confusionMatrix(predict_class_sub, truth_sub)

acc_sub <- cm_sub$overall["Accuracy"]
f1_sub  <- cm_sub$byClass["F1"]

##===== Add both results to final_results =====
final_results <- rbind(
  final_results,
  data.frame(
    Model = "model_independent_lg",
    AUC = auc_indep,
    KS = ks_indep,
    Accuracy = as.numeric(acc_indep),
    F1 = as.numeric(f1_indep)
  ),
  data.frame(
    Model = "model_subsidiary_lg",
    AUC = auc_sub,
    KS = ks_sub,
    Accuracy = as.numeric(acc_sub),
    F1 = as.numeric(f1_sub)
  )
)

#===== Final results =====
final_results

# cm_lg
# cm_lg$table
# cm_lg_woe$table
# cm_pb$table
# cm_pb_woe$table
# cm_lm$table
# cm_expert$table
# cm_indep$table
# cm_sub$table
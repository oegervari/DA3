

# ------------------------------------------------------------------------------------------------------
#### SET UP
rm(list=ls())

# Import libraries
library(haven)
library(glmnet)
library(purrr)
library(margins)
library(skimr)
library(kableExtra)
library(Hmisc)
library(cowplot)
library(gmodels) 
library(lspline)
library(sandwich)
library(modelsummary)

library(rattle)
library(caret)
library(pROC)
library(ranger)
library(rpart)
library(partykit)
library(rpart.plot)
library(viridis)


#   ---------- Loading and preparing data ----------                                                        #

# Load the data
data <- readRDS("bisnode_firms_clean.rds")


# Define variable sets -----------------------------------------------------------------------

rawvars <-  c("curr_assets", "curr_liab", "extra_exp", "extra_inc", "extra_profit_loss", "fixed_assets",
              "inc_bef_tax", "intang_assets", "inventories", "liq_assets", "material_exp", "personnel_exp",
              "profit_loss_year", "sales", "share_eq", "subscribed_cap")
engvar <- c("total_assets_bs", "fixed_assets_bs", "liq_assets_bs", "curr_assets_bs",
            "share_eq_bs", "subscribed_cap_bs", "intang_assets_bs", "extra_exp_pl",
            "extra_inc_pl", "extra_profit_loss_pl", "inc_bef_tax_pl", "inventories_pl",
            "material_exp_pl", "profit_loss_year_pl", "personnel_exp_pl")
engvar2 <- c("extra_profit_loss_pl_quad", "inc_bef_tax_pl_quad",
             "profit_loss_year_pl_quad", "share_eq_bs_quad")
engvar3 <- c(grep("*flag_low$", names(data), value = TRUE),
             grep("*flag_high$", names(data), value = TRUE),
             grep("*flag_error$", names(data), value = TRUE),
             grep("*flag_zero$", names(data), value = TRUE))
d1 <-  c("d1_sales_mil_log_mod", "d1_sales_mil_log_mod_sq",
         "flag_low_d1_sales_mil_log", "flag_high_d1_sales_mil_log")
hr <- c("female", "ceo_age", "flag_high_ceo_age", "flag_low_ceo_age",
        "flag_miss_ceo_age", "ceo_count", "labor_avg_mod",
        "flag_miss_labor_avg", "foreign_management")
firm <- c("age", "age2", "new", "ind2_cat", "m_region_loc", "urban_m")

# interactions for logit, LASSO
interactions1 <- c("ind2_cat*age", "ind2_cat*age2",
                   "ind2_cat*d1_sales_mil_log_mod", "ind2_cat*sales_mil_log",
                   "ind2_cat*ceo_age", "ind2_cat*foreign_management",
                   "ind2_cat*female",   "ind2_cat*urban_m", "ind2_cat*labor_avg_mod")
interactions2 <- c("sales_mil_log*age", "sales_mil_log*female",
                   "sales_mil_log*profit_loss_year_pl", "sales_mil_log*foreign_management")


X1 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "ind2_cat")
X2 <- c("sales_mil_log", "sales_mil_log_sq", "d1_sales_mil_log_mod", "profit_loss_year_pl", "fixed_assets_bs","share_eq_bs","curr_liab_bs ",   "curr_liab_bs_flag_high ", "curr_liab_bs_flag_error",  "age","foreign_management" , "ind2_cat")
X3 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, d1)
X4 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr)
X5 <- c("sales_mil_log", "sales_mil_log_sq", firm, engvar, engvar2, engvar3, d1, hr, interactions1, interactions2)

# for LASSO
logitvars <- c("sales_mil_log", "sales_mil_log_sq", engvar, engvar2, engvar3, d1, hr, firm, interactions1, interactions2)

# for RF (no interactions, no modified features)
rfvars  <-  c("sales_mil", "d1_sales_mil_log", rawvars, hr, firm)

# Check missing values
to_filter <- sapply(data, function(x) sum(is.na(x)))
sort(to_filter[to_filter > 0])
#### we only have missing values in birth_year, exit_year and exit_date which we won't use in the prediction



# CHECK DISTRIBUTION FOR SOME VARIABLE ------------------------------------------------------------------ 
ggplot(data=data, aes(x=cagr_sales)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
                 color = "white", fill = "green") +
  coord_cartesian(xlim = c(0, 200)) +
  labs(x = "CAGR growth",y = "Percent")+
  theme_minimal() 

ggplot(data=data, aes(x=sales_mil)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.5,
                 color = "white", fill = "green") +
  labs(x = "sales in million",y = "Percent")+
  theme_minimal() 

ggplot(data=data, aes(x=sales_mil_log)) +
  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.25,
                 color = "white", fill = "green") +
  labs(x = "log sales in million",y = "Percent")+
  theme_minimal()



#                       PART II ------------
#         ---------- Model building ----------         #
#                                                      #
# separate datasets  (train and holdout) -------------------------------------------------------

set.seed(2022)

train_indices <- as.integer(createDataPartition(data$fast_growth, p = 0.8, list = FALSE))
data_train <- data[train_indices, ]
data_holdout <- data[-train_indices, ]

dim(data_train)
dim(data_holdout)

Hmisc::describe(data$fast_growth_f)
Hmisc::describe(data_train$fast_growth_f)
Hmisc::describe(data_holdout
                $fast_growth_f)

# The proportion of fast growth firms are really similar in all the sets


# 5 fold cross-validation ----------------------------------------------------------------------
train_control <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)

# MODELS ------------------------------------------------------------------------------------

#################################
#      Prob. LOGIT models       #
#################################

logit_model_vars <- list("X1" = X1, "X2" = X2, "X3" = X3, "X4" = X4, "X5" = X5)

CV_RMSE_folds <- list()
logit_models <- list()

for (model_name in names(logit_model_vars)) {

  features <- logit_model_vars[[model_name]]

  set.seed(2022)
  glm_model <- train(
    formula(paste0("fast_growth_f ~", paste0(features, collapse = " + "))),
    method = "glm",
    data = data_train,
    family = binomial,
    trControl = train_control
  )

  logit_models[[model_name]] <- glm_model
  # Calculate RMSE on test for each fold
  CV_RMSE_folds[[model_name]] <- glm_model$resample[,c("Resample", "RMSE")]

}

#################################
#        LASSO  models          #
#################################

lambda <- 10^seq(-1, -4, length = 10)
grid <- expand.grid("alpha" = 1, lambda = lambda)

set.seed(2022)
system.time({
  logit_lasso_model <- train(
    formula(paste0("fast_growth_f ~", paste0(logitvars, collapse = " + "))),
    data = data_train,
    method = "glmnet",
    preProcess = c("center", "scale"),
    family = "binomial",
    trControl = train_control,
    tuneGrid = grid,
    na.action=na.exclude
  )
})

tuned_logit_lasso_model <- logit_lasso_model$finalModel
best_lambda <- logit_lasso_model$bestTune$lambda
logit_models[["LASSO"]] <- logit_lasso_model
lasso_coeffs <- as.matrix(coef(tuned_logit_lasso_model, best_lambda))
write.csv(lasso_coeffs, paste0(output, "lasso_logit_coeffs.csv"))

CV_RMSE_folds[["LASSO"]] <- logit_lasso_model$resample[,c("Resample", "RMSE")]



#################################
#         Random forest         #
#################################

# 5 fold cross-validation

train_control <- trainControl(
  method = "cv",
  n = 5,
  classProbs = TRUE, # same as probability = TRUE in ranger
  summaryFunction = twoClassSummaryExtended,
  savePredictions = TRUE
)
train_control$verboseIter <- TRUE

tune_grid <- expand.grid(
  .mtry = c(5, 6, 7),
  .splitrule = "gini",
  .min.node.size = c(10, 15)
)

# build rf model
set.seed(2022)
rf_model_p <- train(
  formula(paste0("fast_growth_f ~ ", paste0(rfvars , collapse = " + "))),
  method = "ranger",
  data = data_train,
  tuneGrid = tune_grid,
  trControl = train_control,
  importance = "impurity"
)

rf_model_p$results

#saveRDS(rf_model_p, paste0(data_out, "rf_model_p.rds"))

best_mtry <- rf_model_p$bestTune$mtry
best_min_node_size <- rf_model_p$bestTune$min.node.size

CV_RMSE_folds[["rf_p"]] <- rf_model_p$resample[,c("Resample", "RMSE")]


# PART III Probability prediction without loss function ------

# Logit and LASSO ------

# Calculate AUC for each folds --------------------------------
CV_AUC_folds <- list()

for (model_name in names(logit_models)) {

  auc <- list()
  model <- logit_models[[model_name]]
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)

    roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
    auc[[fold]] <- as.numeric(roc_obj$auc)
  }

  CV_AUC_folds[[model_name]] <- data.frame("Resample" = names(auc),
                                              "AUC" = unlist(auc))
}

# For each model: average RMSE and average AUC for models ----------------------------------
CV_RMSE <- list()
CV_AUC <- list()

for (model_name in names(logit_models)) {
  CV_RMSE[[model_name]] <- mean(CV_RMSE_folds[[model_name]]$RMSE)
  CV_AUC[[model_name]] <- mean(CV_AUC_folds[[model_name]]$AUC)
}

# We pick our preferred model based on that. -----------------------------------------------
nvars <- lapply(logit_models, FUN = function(x) length(x$coefnames))
nvars[["LASSO"]] <- sum(lasso_coeffs != 0)

logit_summary1 <- data.frame("Number of predictors" = unlist(nvars),
                             "CV RMSE" = unlist(CV_RMSE),
                             "CV AUC" = unlist(CV_AUC))


#  Random forest -----

# Get average RMSE and AUC ------------------------------------
auc <- list()
for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
  auc[[fold]] <- as.numeric(roc_obj$auc)
}
CV_AUC_folds[["rf_p"]] <- data.frame("Resample" = names(auc),
                                     "AUC" = unlist(auc))

CV_RMSE[["rf_p"]] <- mean(CV_RMSE_folds[["rf_p"]]$RMSE)
CV_AUC[["rf_p"]] <- mean(CV_AUC_folds[["rf_p"]]$AUC)


rf_summary <- data.frame("CV RMSE" = unlist(CV_RMSE),
                         "CV AUC" = unlist(CV_AUC))



# FOR BEST MODEL -> RANDOM FOREST
# discrete ROC (with thresholds in steps) on holdout -------------------------------------------------

best_no_loss <- rf_model_p

predicted_probabilities_holdout <- predict(best_no_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_no_loss_pred"] <- predicted_probabilities_holdout[,"fast_growth"]

# discrete ROC (with thresholds in steps) on holdout -------------------------------------------------
thresholds <- seq(0.05, 0.75, by = 0.025)

cm <- list()
true_positive_rates <- c()
false_positive_rates <- c()
for (thr in thresholds) {
  holdout_prediction <- ifelse(data_holdout[,"best_no_loss_pred"] < thr, "no_fast_growth", "fast_growth") %>%
    factor(levels = c("no_fast_growth", "fast_growth"))
  cm_thr <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)$table
  cm[[as.character(thr)]] <- cm_thr
  true_positive_rates <- c(true_positive_rates, cm_thr["fast_growth", "fast_growth"] /
                             (cm_thr["fast_growth", "fast_growth"] + cm_thr["no_fast_growth", "fast_growth"]))
  false_positive_rates <- c(false_positive_rates, cm_thr["fast_growth", "no_fast_growth"] /
                              (cm_thr["fast_growth", "no_fast_growth"] + cm_thr["no_fast_growth", "no_fast_growth"]))
}

tpr_fpr_for_thresholds <- tibble(
  "threshold" = thresholds,
  "true_positive_rate" = true_positive_rates,
  "false_positive_rate" = false_positive_rates
)

ggplot(
  data = tpr_fpr_for_thresholds,
  aes(x = false_positive_rate, y = true_positive_rate, color = threshold)) +
  labs(x = "False positive rate (1 - Specificity)", y = "True positive rate (Sensitivity)") +
  geom_point(size=2, alpha=0.8) +
  scale_color_viridis(option = "D", direction = -1) +
  scale_x_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  scale_y_continuous(expand = c(0.01,0.01), limit=c(0,1), breaks = seq(0,1,0.1)) +
  theme_bw() +
  theme(legend.position ="right") +
  theme(legend.title = element_text(size = 4), 
        legend.text = element_text(size = 4),
        legend.key.size = unit(.4, "cm")) 



# continuous ROC on holdout with best model (Logit 4) -------------------------------------------
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout$best_no_loss_pred)

createRocPlot(roc_obj_holdout, "best_no_loss_roc_plot_holdout")



# Confusion table with different tHresholds ----------------------------------------------------------

# fast_growth: the threshold 0.5 is used to convert probabilities to binary classes
class_prediction <- predict(best_no_loss, newdata = data_holdout)
summary(class_prediction)

# confusion matrix: summarize different type of errors and successfully predicted cases
# positive = "yes": explicitly specify the positive case
cm_object1 <- confusionMatrix(class_prediction, data_holdout$fast_growth_f, positive = "fast_growth")
cm1 <- cm_object1$table
cm1


# PART IV  Probability prediction with a loss function ------  

# Introduce loss function
# relative cost of of a false negative classification (as compared with a false positive classification)
FP=1
FN=2
cost = FN/FP
# the prevalence, or the proportion of cases in the population (n.cases/(n.controls+n.cases))
prevelance = sum(data_train$fast_growth)/length(data_train$fast_growth)


# Logit and LASSO ---------

# Draw ROC Curve and find optimal threshold with loss function --------------------------

best_tresholds <- list()
expected_loss <- list()
logit_cv_rocs <- list()
logit_cv_threshold <- list()
logit_cv_expected_loss <- list()

for (model_name in names(logit_models)) {
  
  model <- logit_models[[model_name]]
  colname <- paste0(model_name,"_prediction")
  
  best_tresholds_cv <- list()
  expected_loss_cv <- list()
  
  for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
    cv_fold <-
      model$pred %>%
      filter(Resample == fold)
    
    roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
    best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                            best.method="youden", best.weights=c(cost, prevelance))
    best_tresholds_cv[[fold]] <- best_treshold$threshold
    expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$fast_growth)
  }
  
  # average
  best_tresholds[[model_name]] <- mean(unlist(best_tresholds_cv))
  expected_loss[[model_name]] <- mean(unlist(expected_loss_cv))
  
  # for fold #5
  logit_cv_rocs[[model_name]] <- roc_obj
  logit_cv_threshold[[model_name]] <- best_treshold
  logit_cv_expected_loss[[model_name]] <- expected_loss_cv[[fold]]
  
}

logit_summary2 <- data.frame("Avg of optimal thresholds" = unlist(best_tresholds),
                             "Threshold for Fold5" = sapply(logit_cv_threshold, function(x) {x$threshold}),
                             "Avg expected loss" = unlist(expected_loss),
                             "Expected loss for Fold5" = unlist(logit_cv_expected_loss))



# Create plots based on Fold5 in CV ----------------------------------------------

for (model_name in names(logit_cv_rocs)) {
  
  r <- logit_cv_rocs[[model_name]]
  best_coords <- logit_cv_threshold[[model_name]]
  createLossPlot(r, best_coords,
                 paste0(model_name, "_loss_plot"))
  createRocPlotWithOptimal(r, best_coords,
                           paste0(model_name, "_roc_plot"))
}

# Pick best model based on average expected loss ----------------------------------

best_logit_with_loss <- logit_models[["X3"]]
best_logit_optimal_treshold <- best_tresholds[["X3"]]

logit_predicted_probabilities_holdout <- predict(best_logit_with_loss, newdata = data_holdout, type = "prob")
data_holdout[,"best_logit_with_loss_pred"] <- logit_predicted_probabilities_holdout[,"fast_growth"]

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "best_logit_with_loss_pred", drop=TRUE])

# Get expected loss on holdout
holdout_treshold <- coords(roc_obj_holdout, x = best_logit_optimal_treshold, input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout

# Confusion table on holdout with optimal threshold
holdout_prediction <-
  ifelse(data_holdout$best_logit_with_loss_pred < best_logit_optimal_treshold, "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object3 <- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm3 <- cm_object3$table
cm3


#  Random forest ------------
# Now use loss function and search for best thresholds and expected loss over folds -----
best_tresholds_cv <- list()
expected_loss_cv <- list()

for (fold in c("Fold1", "Fold2", "Fold3", "Fold4", "Fold5")) {
  cv_fold <-
    rf_model_p$pred %>%
    filter(mtry == best_mtry,
           min.node.size == best_min_node_size,
           Resample == fold)
  
  roc_obj <- roc(cv_fold$obs, cv_fold$fast_growth)
  best_treshold <- coords(roc_obj, "best", ret="all", transpose = FALSE,
                          best.method="youden", best.weights=c(cost, prevelance))
  best_tresholds_cv[[fold]] <- best_treshold$threshold
  expected_loss_cv[[fold]] <- (best_treshold$fp*FP + best_treshold$fn*FN)/length(cv_fold$fast_growth)
}

# average
best_tresholds[["rf_p"]] <- mean(unlist(best_tresholds_cv))
expected_loss[["rf_p"]] <- mean(unlist(expected_loss_cv))


rf_summary <- data.frame("CV RMSE" = CV_RMSE[["rf_p"]],
                         "CV AUC" = CV_AUC[["rf_p"]],
                         "Avg of optimal thresholds" = best_tresholds[["rf_p"]],
                         "Threshold for Fold5" = best_treshold$threshold,
                         "Avg expected loss" = expected_loss[["rf_p"]],
                         "Expected loss for Fold5" = expected_loss_cv[[fold]])



# Create plots - this is for Fold5

createLossPlot(roc_obj, best_treshold, "rf_p_loss_plot")
createRocPlotWithOptimal(roc_obj, best_treshold, "rf_p_roc_plot")


# Take model to holdout and estimate RMSE, AUC and expected loss ------------------------------------

rf_predicted_probabilities_holdout <- predict(logit_models$X3, newdata = data_holdout, type = "prob")
data_holdout$rf_p_prediction <- rf_predicted_probabilities_holdout[,"fast_growth"]
RMSE(data_holdout$rf_p_prediction, data_holdout$fast_growth)

# ROC curve on holdout
roc_obj_holdout <- roc(data_holdout$fast_growth, data_holdout[, "rf_p_prediction", drop=TRUE])

# AUC
as.numeric(roc_obj_holdout$auc)

# Get expected loss on holdout with optimal threshold
holdout_treshold <- coords(roc_obj_holdout, x = best_tresholds[["rf_p"]] , input= "threshold",
                           ret="all", transpose = FALSE)
expected_loss_holdout <- (holdout_treshold$fp*FP + holdout_treshold$fn*FN)/length(data_holdout$fast_growth)
expected_loss_holdout

# Confusion table on holdout set 
holdout_prediction <-
  ifelse(data_holdout$rf_p_prediction < best_tresholds[["rf_p"]] , "no_fast_growth", "fast_growth") %>%
  factor(levels = c("no_fast_growth", "fast_growth"))
cm_object_rf<- confusionMatrix(holdout_prediction,data_holdout$fast_growth_f)
cm_rf <- cm_object_rf$table
cm_rf

# Model selection is carried out on this CV RMSE ----------

nvars[["rf_p"]] <- length(rfvars)

summary_results <- data.frame("Number of predictors" = unlist(nvars),
                              "CV RMSE" = unlist(CV_RMSE),
                              "CV AUC" = unlist(CV_AUC),
                              "CV threshold" = unlist(best_tresholds),
                              "CV expected Loss" = unlist(expected_loss))

model_names <- c("Logit X3", "Logit X4",
                 "Logit LASSO","RF probability")
summary_results <- summary_results %>%
  filter(rownames(.) %in% c("X3", "X4", "LASSO", "rf_p"))
rownames(summary_results) <- model_names





# Calibration curve -----------------------------------------------------------
# how well do estimated vs actual event probabilities relate to each other?

create_calibration_plot(data_holdout, 
                        file_name = "logit-X3-calibration", 
                        prob_var = "best_logit_with_loss_pred", 
                        actual_var = "fast_growth",
                        n_bins = 20)




library(readr)
library(skimr)
library(ggplot2)
library(dplyr)
library(tidyverse)
library(caret)

data <- read_csv('insurance_data.csv')

ratio_of_neg_to_pos = sum(data$response == 0) / sum(data$response == 1)

remove_cols <- nearZeroVar(data[,-86], names = TRUE)

data <-data %>% select(-one_of(remove_cols))

data<-data %>% mutate_at(c(1,5), as.factor)

dummies_model <- dummyVars(response~ ., data = data)


predictors_dummy<- data.frame(predict(dummies_model, newdata = data)) 


data <- cbind(response=data$response, predictors_dummy) 


data$response <- as.factor(data$response)

data$response <- fct_recode(data$response,
                            buy="1",
                            notbuy= "0")
data$response <- relevel(data$response,
                         ref="buy")

levels(data$response )

## partition the data into Training and testing

library(e1071)
library(glmnet)
library(Matrix)

#set the seed again since within the train method the validation set is randomly selected
set.seed(8)
data_model_lasso <- train(response ~ .,
                          data = data,
                          method = "glmnet", # this is LASSO method
                          standardize =T, # to have similar units
                          tuneGrid = expand.grid(alpha =1, #lasso
                                                 lambda = seq(0.0001, 1, length = 20)),
                          trControl =trainControl(method = "cv",
                                                  number = 5,
                                                  # those just for classification
                                                  classProbs = TRUE, # predictive probability
                                                  summaryFunction = twoClassSummary),
                          metric="ROC")
data_model_lasso
coef(data_model_lasso$finalModel, data_model_lasso$bestTune$lambda)

# keep the lasso selected features 

best_lambda <- data_model_lasso$bestTune$lambda
coef_values <- as.matrix(coef(data_model_lasso$finalModel, s = best_lambda))
selected_feature_indices <- which(coef_values != 0, arr.ind = TRUE)
selected_feature_names <- rownames(coef_values)[selected_feature_indices[, "row"]][-1]

print(selected_feature_names)


df_new <- data[, c("response", selected_feature_names)]




########### now apply xgboost


set.seed(99) #set random seed
index <- createDataPartition(df_new$response, p = .8,list = FALSE)
df_new_train <-df_new[index,]
df_new_test<- df_new[-index,]


library(doParallel)

num_cores <- detectCores(logical = FALSE)
num_cores

#start parallel processing

cl <- makePSOCKcluster(num_cores-2)
registerDoParallel(cl)


set.seed(12)
start_time <- Sys.time()

model_gbm_new <- train(response ~.,
                       data = df_new_train,
                       method="xgbTree",
                       tuneGrid = expand.grid(
                         nrounds = c(50,300),
                         eta = c(0.025, 0.05),
                         max_depth = c(2, 3, 6, 9),
                         gamma = 0.02,
                         colsample_bytree = 0.8,
                         min_child_weight = 1,
                         subsample = 0.7), 
                       scale_pos_weight = ratio_of_neg_to_pos,
                       trControl = trainControl(method = "cv",
                                                number=5,
                                                classProbs=TRUE,
                                                summaryFunction = twoClassSummary),
                       metric = "ROC")

end_time <- Sys.time()
print(end_time - start_time)

#stop parallel processing
stopCluster(cl)
registerDoSEQ()

model_gbm_new

plot(model_gbm_new)
plot(varImp(model_gbm_new))
varImp(model_gbm_new)$importance

#First, get the predicted probabilities of the test data.
predicted_probabilities <- predict(model_gbm_new, df_new_test,type="prob")

predicted_probabilities_train <- predict(model_gbm_new, df_new_train,type="prob")

# training auc

pred_tree_train<- prediction(predicted_probabilities_train$buy,
                             df_new_train$response,
                             label.ordering = c("notbuy","buy"))
perf_tree_train<-performance(pred_tree_train,"tpr","fpr")
plot(perf_tree_train,colorize = T)


auc_tree_train<-unlist(slot(performance(pred_tree_train, "auc"), "y.values"))
auc_tree_train


# testing auc 

library(ROCR)
pred_tree<- prediction(predicted_probabilities$buy,
                       df_new_test$response,
                       label.ordering = c("notbuy","buy"))
perf_tree<-performance(pred_tree,"tpr","fpr")
plot(perf_tree,colorize = T)


auc_tree<-unlist(slot(performance(pred_tree, "auc"), "y.values"))
auc_tree


# Make predictions on the test set
predictions <- predict(model_gbm_new, newdata = df_new_test)

# Create a confusion matrix
conf_matrix <- confusionMatrix(predictions, df_new_test$response)

# Print the confusion matrix
conf_matrix

##########################################################################################################
# shap 

library(SHAPforxgboost)

Xdata<-as.matrix(select(df_new_train,-response)) # change data to matrix for plots

# Calculate SHAP values
shap <- shap.prep(model_gbm_new$finalModel, X_train = Xdata)

# SHAP importance plot for top 20 variables
shap.plot.summary.wrap1(model_gbm_new$finalModel, X = Xdata, top_n = 20)


#example partial dependence plot between total_car and perc_married
p <- shap.plot.dependence(
  shap, 
  x = "total_car", 
  color_feature = "perc_onecar", 
  smooth = FALSE, 
  jitter_width = 0.01, 
  alpha = 0.4
) +
  ggtitle("total_car")
print(p)



# Use 4 most important predictor variables. For the top 4 predictor variables (on the x-axis),
# the color is the strongest interacting variable.

top5<-shap.importance(shap, names_only = TRUE)[1:6]

 for (x in top5) {
   p <- shap.plot.dependence(
     shap, 
     x = x, 
     color_feature = "auto", 
     smooth = FALSE, 
     jitter_width = 0.01, 
     alpha = 0.4
     ) +
   ggtitle(x)
   print(p)
 }


##########################################################################################################


# Plotting training AUC curve
plot(perf_tree_train, col = "blue", main = "ROC Curve - Training vs Testing",
     col.main = "black", col.sub = "darkgrey", lwd = 2)

# Adding testing AUC curve to the plot
plot(perf_tree, col = "red", add = TRUE,
     sub = paste("Testing AUC =", round(auc_tree, 4)),
     col.main = "black", col.sub = "darkgrey", lwd = 2)


bias_variance_tradeoff <- round(abs(auc_tree_train - auc_tree), 4)


# adding a legend

legend_text <- c(paste("Training AUC =", round(auc_tree_train, 4)),
                 paste("Testing AUC =", round(auc_tree, 4)),
                 paste("Bias-Variance Tradeoff =", bias_variance_tradeoff))

legend("bottomright", legend = legend_text,
       col = c("blue", "red", "black"), lty = 1, lwd = 2, bty = "n")


# Adding cutoff value on the right side
text(1, 0.5, "Cutoff Value", pos = 2, col = "black", cex = 0.8)


#####################################################################################################

## Get Predicted Probabilities for Holdout set (Score the Holdout set)

holdout_data<- read.csv("insurance_holdout_noresponse.csv", header=T)


remove_cols <- nearZeroVar(holdout_data, names = TRUE)

holdout_data <-holdout_data %>% select(-one_of(remove_cols))

holdout_data<-holdout_data %>% mutate_at(c(1,5), as.factor)

dummies_model <- dummyVars(~ ., data = holdout_data)


predictors_dummy<- data.frame(predict(dummies_model, newdata = holdout_data)) 

holdout_data <- predictors_dummy

# filter the data to the selected features from LASSO

holdout_data_selected <- holdout_data[selected_feature_names]

# Get Predicted Probabilities

case_holdoutprob <- predict(model_gbm_new, holdout_data_selected,type="prob")

case_holdout_scored<- cbind(holdout_data, case_holdoutprob$buy)

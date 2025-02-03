
# Environment -------------------------------------------------------------

library("readxl")
library("tidyr")
library("rstatix")
library("lavaan")
library("leaps")
library("glmnet")
library("caret")
library("tidyverse") 
library("car")
library("dplyr")
library("ggplot2")


# Data wrangling ----------------------------------------------------------

# loading data
df<- read_xlsx("ace_q_data.xlsx")

# factorizing variables
df$center<- as.factor(df$center)
df$gender <- factor(ifelse(df$gender == "Male", 1, 0)) # Females = 0
df$education<- as.factor(df$education)


# Computing composite scores for cognitive performance --------------------

df_gamified<- data.frame(df$Z_BRT, df$Z_FGemChaser, df$Z_BGemChaser, df$Z_TNT) 
mean_Z_gamified<- rowMeans(df_gamified, na.rm = TRUE) # mean Z-score for gamified tests

df_standard<- data.frame(df$Z_TAP, df$Z_forwardspan, df$Z_backwardspan, df$Z_Bad_total) # mean Z-score for standard tests 
mean_Z_standard<- rowMeans(df_standard, na.rm = TRUE) #mean Z-score for standard tests

df$mean_Z_gamified<- mean_Z_gamified
df$mean_Z_standard<- mean_Z_standard


# Inspecting extreme values and distributions --------------------

# inspecting for extreme values
which(is_extreme(df$Z_BRT)==TRUE) 
which(is_extreme(df$Z_FGemChaser)==TRUE)
which(is_extreme(df$Z_BGemChaser)==TRUE)
which(is_extreme(df$Z_TNT)==TRUE) # participant in row 8 is slightly off bounds
df$Z_TNT[8]<- NA

which(is_extreme(df$Z_TAP)==TRUE)
which(is_extreme(df$Z_forwardspan)==TRUE)
which(is_extreme(df$Z_backwardspan)==TRUE)
which(is_extreme(df$Z_Bad_total)==TRUE)

which(is_extreme(df$HAD_total)==TRUE) # participant in row 14 is an extreme value (because of high depression)
df$HAD_depression[14]<- NA
which(is_extreme(df$HAD_Anxiety)==TRUE)
which(is_extreme(df$HAD_depression)==TRUE) # participant in row 14 is an extreme value
df$HAD_total[14]<- NA
which(is_extreme(df$cfq_total)==TRUE)

qqp(df$mean_Z_gamified)
qqp(df$mean_Z_standard)
qqp(df$HAD_total)
qqp(df$cfq_total)
qqp(df$ami_total)


# Predicting cognitive complaints: oldschool stepwise regression  ------------------------------

# data frame for stepwise regression
df_pred<- data.frame(df$gender, df$age, df$education, df$center,
                     df$HAD_total, df$ami_total, 
                     df$cfq_total,
                     df$mean_Z_gamified, df$mean_Z_standard)

df_pred<- df_pred[c(-5,-14, -79 , -82, -84, -90),]

# stepwise regression with cfq total score
model<- lm(df.cfq_total ~ ., data = df_pred)
summary(model)
both_model <- step(model, direction = "both")
summary(both_model)



# Predicting cognitive complaints: Elastic net -------------------------------------------------------------

# data frame
data<- df_pred[,c(-7)]
y<- df_pred$df.cfq_total
X <- data

# Train-test split of data  (70% for training the model / 30% for testing) for cross-validation
set.seed(123) 
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]


# Setting up (10 fold) cross-validation and elastic net model
train_control <- trainControl(method = "cv", 
                              number = 10, 
                              search = "grid",
                              savePredictions = "final", 
                              returnResamp = "all")

# Defining the grid of tuning parameters for alpha and lambda. 
# Alpha is the parameter determining the mixing of lasso and ridge penalty
# Lambda is the parameter determining the strength of regularization
tune_grid <- expand.grid(alpha = seq(0, 1, by = 0.1),
                         lambda = 10^seq(-2, 1, by = 0.1))


# Training elastic net model with cross-validation
elastic_model <- train(x = X_train, 
                       y = y_train, 
                       method = "glmnet", 
                       trControl = train_control, 
                       tuneGrid = tune_grid)

# Extracting the parameters of the best tune (alpha and lambda)
best_alpha <- elastic_model$bestTune$alpha
best_lambda <- elastic_model$bestTune$lambda

# Extracting model coefficients using the best alpha and lambda
final_model <- glmnet(as.matrix(X_train), y_train, alpha = best_alpha, lambda = best_lambda)
coefs <- as.data.frame(as.matrix(final_model$beta))

# Testing the model on unseen data 
y_pred <- predict(elastic_model, newdata = X_test)

# Evaluating model performance
rmse_test <- sqrt(mean((y_test - y_pred)^2))  # RMSE test data
r2_test <- cor(y_test, y_pred)^2  # R-squared data
rmse_test
r2_test


# Visualization -----------------------------------------------------------

library(viridis)

cv_results <- elastic_model$results 

ggplot(cv_results, aes(x = lambda, y = RMSE, color = as.factor(alpha), group = as.factor(alpha))) +  
  geom_line() + 
  geom_point(size = 2) +
  scale_color_viridis_d(option = "plasma", begin = 0, end = 0.90) +
  guides(colour = guide_legend(reverse=T, title = "Mixing percentage (α)")) +
  geom_vline(xintercept = 2.51, linetype = "dashed", color = "gray60") +
  geom_hline(yintercept = 10.25, linetype = "dashed", color = "gray60") +
  annotate("text", x = 2.1, y = 9.95, label = "Optimum", color = "gray60", size = 4, hjust = 0) +  
  geom_point(aes(x = 2.51, y = 10.25), shape = 1, size = 5, color = "red") +
  scale_x_continuous(breaks = seq(0, 10, by = 1)) +   
  labs(title = "Elastic Net Performance",
       x = "Regularization strength (λ)", 
       y = "RMSE",
       color = "Mixing percentage (α)") +  
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5, size = 16))



# Table of beta values across each fold -----------------------------------

# Creating custom folds
folds <- createFolds(y_train, k = 10)

# Manually extract coefficients for each fold
fold_coefs <- list()
for (i in 1:length(folds)) {
  # Get the training and test data for this fold
  fold_train_indices <- folds[[i]]
  fold_test_indices <- setdiff(1:nrow(X_train), fold_train_indices)
  
  X_fold_train <- X_train[fold_train_indices, ]
  y_fold_train <- y_train[fold_train_indices]
  X_fold_test <- X_train[fold_test_indices, ]
  y_fold_test <- y_train[fold_test_indices]
  
  # Train the model on this fold
  fold_model <- glmnet(as.matrix(X_fold_train), y_fold_train, alpha = best_alpha, lambda = best_lambda)
  
  # Extract coefficients and store them
  fold_coefs[[i]] <- as.data.frame(as.matrix(fold_model$beta))
  fold_coefs[[i]]$Fold <- i  # Add fold number for tracking
}

# Combine all coefficients into a single table
coefs_table <- do.call(rbind, fold_coefs)
transpose_s0 <- t(coefs_table$s0)
matrix_coefs <- matrix(transpose_s0, nrow = length(transpose_s0) / 8, byrow = TRUE)
CV <- c(1:10)
df_coefs<- data.frame(CV, matrix_coefs)
colnames(df_coefs)<- c("CV", "Gender", "Age", "Education", "Center", "HAD total", "AMI total", "mean Z gamified", "mean Z standard")

# Reshape the dataframe into long format
df_long <- df_coefs %>%
  pivot_longer(cols = c(-1,-2,-4,-5), 
               names_to = "Variable", 
               values_to = "Beta")
df_long$CV<- as.factor(df_long$CV)

# Create a new column to assign numeric values for colors based on the Beta value
df_long$Beta_Color <- factor(ifelse(df_long$Beta > 0, "positive", 
                                    ifelse(df_long$Beta < 0, "negative", "zero")))

# Plot the betas across folds for each variable
ggplot(df_long, aes(x = CV, y = Beta, group = 1)) +
  geom_line() + 
  geom_point(aes(color = Beta_Color), size = 2) +  # Map colors to Beta_Color
  theme_minimal() +
  labs(title = "Betas Across Folds for Each Variable", 
       x = "Fold Number", 
       y = "Beta Value") +
  theme(legend.position = "none") +
  theme(plot.title = element_text(hjust = 0.5, size = 16))+
  facet_wrap(~Variable, nrow = 2, ncol = 3) +
  scale_color_manual(values = c("positive" = "red", "negative" = "blue", "zero" = "grey"))
  

# Random forest -----------------------------------------------------------
library(randomForest)

set.seed(123)

train_control <- trainControl(method = "repeatedcv",  # Cross-validation
                              number = 10,           # 10 folds
                              repeats = 3,           # 3 repeats
                              verboseIter = TRUE)    # Show progress
rf_model_cv <- train(x = X_train, 
                     y = y_train, 
                     method = "rf",          # Use Random Forest
                     trControl = train_control, 
                     tuneLength = 5)         # Tune over 5 values of mtry
rf_model_cv
varImpPlot(rf_model_cv$finalModel, main = "Variable Importance (Cross-Validated RF)")


# If you used caret to train the random forest model:
importance_caret <- varImp(rf_model_cv, scale = FALSE)


# BORUTA ------------------------------------------------------------------
library(Boruta)

set.seed(123) 
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

boruta_result <- Boruta(X_train, y_train, doTrace = 2)

# Get selected features
selected_features <- getSelectedAttributes(boruta_result, withTentative = FALSE)
X_train_selected <- X_train[, selected_features]
X_test_selected <- X_test[, selected_features]

# Print important variables
print(selected_features)

# Extract feature importance scores
importance_df <- attStats(boruta_result) %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(meanImp))  # Sort features by importance

# Plot using ggplot2
ggplot(importance_df, aes(x = reorder(Feature, meanImp), y = meanImp, fill = decision)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +  # Flip to horizontal bars
  scale_fill_manual(values = c("Confirmed" = "forestgreen", "Tentative" = "gold", "Rejected" = "red")) +
  labs(title = "Boruta Feature Importance",
       x = "Feature",
       y = "Mean Importance Score") +
  theme_minimal()


# Final model: linear mixed model with retained factors ------------------
library(lmerTest)
library(MuMIn)

final_model<-  lmer(cfq_total ~ age + HAD_total + (1 | center), data = df)
summary(final_model)
r2_value <- r.squaredGLMM(final_model)
print(r2_value)


df_test<- data.frame(X_test, y_test)
final_model_test<-  lmer(df_test$y_test ~ df_test$df.age + df_test$df.HAD_total + (1 | df_test$df.center), data = df_test)
summary(final_model_test)
r2_value_test <- r.squaredGLMM(final_model_test)
print(r2_value_test)

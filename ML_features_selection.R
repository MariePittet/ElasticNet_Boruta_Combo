
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
df <- read_xlsx("data.xlsx")

# factorizing variables
df$center <- as.factor(df$center)
df$gender <- factor(ifelse(df$gender == "Male", 1, 0)) # Females = 0
df$education <- as.factor(df$education)


# Computing composite scores for cognitive performance --------------------

df_gamified <- data.frame(df$var1, df$var2, df$var3, df$var4) 
mean_Z_gamified <- rowMeans(df_gamified, na.rm = TRUE) # mean Z-score for gamified tests

df_standard <- data.frame(df$var1b, df$var2b, df$var3b, df$var4b) # mean Z-score for standard tests 
mean_Z_standard <- rowMeans(df_standard, na.rm = TRUE) #mean Z-score for standard tests

df$mean_Z_gamified <- mean_Z_gamified
df$mean_Z_standard <- mean_Z_standard


# Inspecting extreme values and distributions --------------------

# inspecting for extreme values
which(is_extreme(df$var1)==TRUE) 
which(is_extreme(df$var2)==TRUE)
which(is_extreme(df$var3)==TRUE)
which(is_extreme(df$var4)==TRUE) # participant in row 8 is slightly off bounds
df$var1[8]<- NA

which(is_extreme(df$var1b)==TRUE)
which(is_extreme(df$var2b)==TRUE)
which(is_extreme(df$var3b)==TRUE)
which(is_extreme(df$var4b)==TRUE)

which(is_extreme(df$HAD_total)==TRUE) # participant in row 14 is an extreme value (because of high depression)
df$HAD_depression[14]<- NA
which(is_extreme(df$HAD_Anxiety)==TRUE)
which(is_extreme(df$HAD_depression)==TRUE) # participant in row 14 is an extreme value
df$Mood[14]<- NA
which(is_extreme(df$clin)==TRUE)

qqp(df$mean_Z_gamified)
qqp(df$mean_Z_standard)
qqp(df$mood_total)
qqp(df$clin_total)
qqp(df$apathy_total)


# Predicting cognitive complaints: oldschool stepwise regression  ------------------------------

# data frame for stepwise regression
df_pred <- data.frame(df$gender, df$age, df$education, df$center,
                     df$mood, df$apathy, 
                     df$clin,
                     df$mean_Z_gamified, df$mean_Z_standard)

df_pred <- df_pred[c(-5,-14, -79 , -82, -84, -90),]

# stepwise regression with cfq total score
model <- lm(df.clin ~ ., data = df_pred)
summary(model)
both_model <- step(model, direction = "both")
summary(both_model)


# Predicting cognitive complaints: Elastic net -------------------------------------------------------------

# data frame
data <- df_pred[,c(-7)]
y <- df_pred$df.clin
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
# Alpha is the parameter determining the mixing of lasso and ridge penalty. Lambda is the parameter determining the strength of regularization
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

# Evaluating model performance on unseen data
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

# Manually extracting coefficients for each fold. There must be a better way but I'm in a rush ^^'
fold_coefs <- list()
for (i in 1:length(folds)) {
  fold_train_indices <- folds[[i]]
  fold_test_indices <- setdiff(1:nrow(X_train), fold_train_indices)
  
  X_fold_train <- X_train[fold_train_indices, ]
  y_fold_train <- y_train[fold_train_indices]
  X_fold_test <- X_train[fold_test_indices, ]
  y_fold_test <- y_train[fold_test_indices]
  
  # Training the model on the fold
  fold_model <- glmnet(as.matrix(X_fold_train), y_fold_train, alpha = best_alpha, lambda = best_lambda)
  
  # Extracting coefficients and storing them
  fold_coefs[[i]] <- as.data.frame(as.matrix(fold_model$beta))
  fold_coefs[[i]]$Fold <- i  # Add fold number for tracking
}

# Combining all coefficients into a table
coefs_table <- do.call(rbind, fold_coefs)
transpose_s0 <- t(coefs_table$s0)
matrix_coefs <- matrix(transpose_s0, nrow = length(transpose_s0) / 8, byrow = TRUE)
CV <- c(1:10)
df_coefs<- data.frame(CV, matrix_coefs)
colnames(df_coefs) <- c("CV", "Gender", "Age", "Education", "Center", "Mood", "Apathy", "mean Z gamified", "mean Z standard")

# Reshaping the dataframe for visualization
df_long <- df_coefs %>%
  pivot_longer(cols = c(-1,-2,-4,-5), 
               names_to = "Variable", 
               values_to = "Beta")
df_long$CV <- as.factor(df_long$CV)

# Assigning numeric values for colors for Beta value. I want them a different color if they are positive or negative coefficients for easy detection of variability.
df_long$Beta_Color <- factor(ifelse(df_long$Beta > 0, "positive", 
                                    ifelse(df_long$Beta < 0, "negative", "zero")))

# Visualization of variability 
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
  

# Random forest (just for fun) -----------------------------------------------------------

library(randomForest)

set.seed(123)

train_control <- trainControl(method = "repeatedcv",  
                              number = 10,           
                              repeats = 3,          
                              verboseIter = TRUE)   
rf_model_cv <- train(x = X_train, 
                     y = y_train, 
                     method = "rf",          # Use Random Forest
                     trControl = train_control, 
                     tuneLength = 5)         # Tune over 5 values of mtry
rf_model_cv
varImpPlot(rf_model_cv$finalModel, main = "Variable Importance (Cross-Validated RF)")


# importance of variables for prediciton
importance_caret <- varImp(rf_model_cv, scale = FALSE)


# BORUTA ------------------------------------------------------------------

library(Boruta)

# Resetting the train-test split
set.seed(123) 
trainIndex <- createDataPartition(y, p = 0.7, list = FALSE, times = 1)
X_train <- X[trainIndex, ]
y_train <- y[trainIndex]
X_test <- X[-trainIndex, ]
y_test <- y[-trainIndex]

# BORUTA
boruta_result <- Boruta(X_train, y_train, doTrace = 2)

# Selected variables
selected_features <- getSelectedAttributes(boruta_result, withTentative = FALSE)
X_train_selected <- X_train[, selected_features]
X_test_selected <- X_test[, selected_features]
selected_features # these are the variables to keep

# Extract variable importance scores
importance_df <- attStats(boruta_result) %>%
  rownames_to_column(var = "Feature") %>%
  arrange(desc(meanImp))  # Sorts variables by importance

# Visualization
ggplot(importance_df, aes(x = reorder(Feature, meanImp), y = meanImp, fill = decision)) +
  geom_bar(stat = "identity", show.legend = TRUE) +
  coord_flip() +  # Flip to horizontal bars
  scale_fill_manual(values = c("Confirmed" = "forestgreen", "Rejected" = "red")) +
  labs(title = "Boruta Feature Importance",
       x = "Feature",
       y = "Mean Importance Score") +
  theme_minimal()


# Final model: linear mixed model with retained factors ------------------

library(lmerTest)
library(MuMIn)

# Hierarchical model with one intercept modelled per center of data acquisition since it seems to influence clinical complaints. Slopes are fixed because it doesn't make sense for them to vary much.
final_model <-  lmer(clin ~ age + Mood + (1 | center), data = df)
summary(final_model)
r2_value <- r.squaredGLMM(final_model)
r2_value # Within-sample proportion of variance of the clinical variable explained by the model (careful, this is surely a bit overfitted). Marginal R2 for fixed effects, conditional R2 for fixed and random effects

# Hierarchical model on testing data only for out-of-sample predictions
df_test <- data.frame(X_test, y_test) # this data was not used for training the model
final_model_test <-  lmer(df_test$y_test ~ df_test$df.age + df_test$df.HAD_total + (1 | df_test$df.center), data = df_test)
summary(final_model_test)
r2_value_test <- r.squaredGLMM(final_model_test)
r2_value_test # out-of-sample  proportion of variance of the clinical variable explained by the model (this is closer to what you could expect if you ran this model on data acquired somewhere else)

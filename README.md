# Feature_selection_clin_var

Contributor: Marie Pittet, marie.pittet93@gmail.com

## Description ##
Feature selection pipeline for the predictions in clinical contexts. 
Includes: Cross-validation, Elastic net regression, BORUTA
Spicy things: multicollinearity, hierarchical data structure


## Context ##
This pipeline was developed for a study examining patients reporting varying levels of clinical complaints, assessed through a questionnaire. The data includes multiple variables, some of which are correlated. Additionally, data were collected across three different medical centers, potentially introducing center-specific differences.


## Aim ##
Predicting the level of clinical complaints of patients with the fewest variables necessary. 
The final model should be *reasonnably sparse, extremely robust, and simple* enough to allow actionable insights for reducing clinical complaints.


## Methods ##
- Variable Distribution: Extreme values are removed, and qq-plots are used to assess distributions.

- Prediction Robustness: A train-test split approach is used to evaluate model robustness.

- Elastic Net Regression: A 10-fold cross-validation is performed to optimize the hyperparameters λ (penalization strength) and α (L1/L2 mix). The model's performance is assessed using RMSE on both training and test sets. Variable coefficients are extracted for the final model, and their stability across folds is explored.
Advantages: Elastic net handles multicollinearity among predictors.
Disadvantages: It doesn't provide a direct metric for variable importance and doesn't capture non-linear relationships.

- BORUTA: BORUTA is used for confirmatory analysis, specifically to assess the importance of variables that showed instability in the elastic net model.
Advantages: BORUTA provides variable importance and handles non-linear relationships well.
Disadvantages: It is less effective with multicollinearity, making it suitable only as a supplementary test in this context.

- Linear Mixed Model: Elastic net and BORUTA both identified three variables with stable importance in predicting cognitive complaints, one of which was the medical center of data collection. This indicated a hierarchical structure in the data. Therefore, a final regression model was built with a random intercept per center (but shared slopes across centers) along with the two remaining predictors.
Marginal R² values (accounting for fixed effects) and conditional R² values (accounting for both fixed and random effects) are computed. The model is evaluated both on the entire dataset (within-sample prediction, with a potential overfitting risk) and on unseen test data (out-of-sample prediction).



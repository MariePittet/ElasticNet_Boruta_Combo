# Elastic-net-Boruta combo for sensible variable selection in clinical data

Contributor: Marie Pittet, marie.pittet93@gmail.com

## Description ##
This pipeline performs feature selection and predictive modeling for clinical symptom data with special attention to actionable, pragmatic modelling.
Includes: Cross-validation, Elastic net regression, BORUTA   
Spicy things: multicollinearity, hierarchical data structure  


## Context ##
This pipeline was developed for a study examining the relationship between patients' cognitive complaints, assessed through a questionnaire, and multiple clinical and demographic variables. 
The data includes multiple variables, some of which are correlated. Additionally, data were collected across three different medical centers, potentially introducing center-specific differences.


## Methods ##
### Preprocessing ###
- Detection and handling of extreme values
- Distribution assessment via QQ-plots

### Feature selection ### 
- Elastic Net Regression: 10-fold cross-validation to optimize the hyperparameters λ (penalization strength) and α (L1/L2 mix). RMSE to asses model performance on both training and test sets. Extraction of final model variable coefficients, and assessment of their stability across folds.
Advantages: Elastic net handles multicollinearity among predictors.
Disadvantages: It doesn't provide a direct metric for variable importance and doesn't capture non-linear relationships.

- BORUTA: BORUTA used for confirmatory analysis, specifically to assess the importance of variables that showed instability in the elastic net model.
Advantages: BORUTA provides variable importance and handles non-linear relationships well.
Disadvantages: less effective in handling multicollinearity, making it suitable only as a confirmatory approach in our context.

### Linear Mixed-Effects Modeling ###
- Linear Mixed Model: After identifying the most important variables for predictions with Elastic net and BORUTA, integration of them in a linear model to predict complaints. You may ask "Why resorting to a simple linear model after playing around with more complex modelling in variable selection?". This is because model accuracy is not the only goal we have, the model should also be actionable for clinicians. And relationships such as "the more (or the less) of variable A, the more complaints" are easier to grasp and to act on. 
Since one of the most important variable indicated a hierarchical structure in the data (the medical center of data collection), a linear mixed model was built with a random intercept per center (but shared slopes across centers) along with the other remaining predictors.
- Extraction of marginal R² values (accounting for fixed effects) and conditional R² values (accounting for both fixed and random effects). The model was evaluated both on the entire dataset (within-sample prediction, with a potential overfitting risk) and on unseen test data (out-of-sample prediction for generalization).



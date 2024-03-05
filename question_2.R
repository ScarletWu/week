# Load necessary libraries
library(dplyr)
library(stats)

# Set seed for reproducibility
set.seed(123)

# Generate data with added 'PhD' level for education
n <- 200 # Number of observations
age_groups <- c('18-24', '25-34', '35-44', '45-54', '55-64', '65+')
income_groups <- c('Low', 'Medium', 'High')
education_levels <- c('High School', 'Bachelor', 'Graduate', 'PhD') # Added 'PhD'

# Simulate dataset
data <- data.frame(
  Support = sample(c('Y', 'N'), n, replace = TRUE), # Binary outcome
  AgeGroup = sample(age_groups, n, replace = TRUE),
  IncomeGroup = sample(income_groups, n, replace = TRUE),
  EducationLevel = sample(education_levels, n, replace = TRUE)
)

# Test 1-4: Descriptive statistics
cat("Descriptive Statistics for Age Groups:\n")
print(table(data$AgeGroup))

cat("\nDescriptive Statistics for Income Groups:\n")
print(table(data$IncomeGroup))

cat("\nDescriptive Statistics for Education Levels:\n")
print(table(data$EducationLevel))

# Test 5-7: Chi-square tests for independence
cat("\nChi-square Test for Independence (AgeGroup and Support):\n")
print(chisq.test(table(data$AgeGroup, data$Support)))

cat("\nChi-square Test for Independence (IncomeGroup and Support):\n")
print(chisq.test(table(data$IncomeGroup, data$Support)))

cat("\nChi-square Test for Independence (EducationLevel and Support):\n")
print(chisq.test(table(data$EducationLevel, data$Support)))

# Convert factors to numeric for logistic regression (Test 8)
data$SupportNumeric <- as.numeric(data$Support) - 1  # Convert Y/N to 1/0

# Test 9: Logistic regression analysis
model <- glm(SupportNumeric ~ AgeGroup + IncomeGroup + EducationLevel, family = binomial, data = data)
cat("\nLogistic Regression Analysis Summary:\n")
print(summary(model))

# Test 10: Check model diagnostics - AIC for model comparison
cat("\nAIC for Logistic Regression Model:\n")
print(AIC(model))

# Additional analyses can include model diagnostics, prediction accuracy, or extending to multinomial logistic regression for more outcomes.

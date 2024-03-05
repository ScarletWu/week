library(dplyr)
library(stats)

# Set seed for reproducibility
set.seed(123)

# Simulate dataset
n <- 200

age_groups <- c("18-25", "26-35", "36-45", "46-55", "56-65", "65+")
income_groups <- c("Low", "Medium", "High")
education_levels <- c("High School", "Bachelor's", "Master's", "PhD")

data <- data.frame(
  Support = sample(c('Y', 'N'), n, replace = TRUE), 
  AgeGroup = factor(sample(age_groups, n, replace = TRUE)),
  IncomeGroup = factor(sample(income_groups, n, replace = TRUE)),
  EducationLevel = factor(sample(education_levels, n, replace = TRUE))
)



# Check for NA values
sum(is.na(data$SupportNumeric))
sum(is.na(data))

# Descriptive statistics
cat("Descriptive Statistics for Age Groups:\n")
print(table(data$AgeGroup))

cat("\nDescriptive Statistics for Income Groups:\n")
print(table(data$IncomeGroup))

cat("\nDescriptive Statistics for Education Levels:\n")
print(table(data$EducationLevel))

# Chi-square tests for independence
cat("\nChi-square Test for Independence (AgeGroup and Support):\n")
print(chisq.test(table(data$AgeGroup, data$Support)))

cat("\nChi-square Test for Independence (IncomeGroup and Support):\n")
print(chisq.test(table(data$IncomeGroup, data$Support)))

cat("\nChi-square Test for Independence (EducationLevel and Support):\n")
print(chisq.test(table(data$EducationLevel, data$Support)))

# Logistic regression analysis
data$SupportNumeric <- ifelse(data$Support == 'Y', 1, 0)

model <- glm(SupportNumeric ~ AgeGroup + IncomeGroup + EducationLevel, family = binomial, data = data)

cat("\nLogistic Regression Analysis Summary:\n")
print(summary(model))

# Model diagnostics tests
cat("\nAIC for Logistic Regression Model:\n")
print(AIC(model))


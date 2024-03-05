library(dplyr)
library(stats)
library(ggplot2)
library(bayesplot)

library(rstanarm) 

set.seed(123)

# Simulate dataset
n <- 200
data <- data.frame(
  Support = sample(c('Y', 'N'), n, replace = TRUE), 
  AgeGroup = factor(sample(age_groups, n, replace = TRUE)),
  IncomeGroup = factor(sample(income_groups, n, replace = TRUE)),
  EducationLevel = factor(sample(education_levels, n, replace = TRUE))
)


ggplot(data, aes(x = AgeGroup, fill = Support)) + 
  geom_bar(position = "dodge") +
  labs(title = "Support for Political Party by Age Group",
       x = "Education Level",
       y = "Count") +
  scale_fill_manual(values = c("Y" = "blue", "N" = "red"), name = "Support") +
  theme_minimal()

# ggplot figures
ggplot(data, aes(x = IncomeGroup, fill = Support)) + 
  geom_bar(position = "dodge") +
  labs(title = "Support for Political Party by Income Group",
       x = "Education Level",
       y = "Count") +
  scale_fill_manual(values = c("Y" = "blue", "N" = "red"), name = "Support") +
  theme_minimal()

ggplot(data, aes(x = EducationLevel, fill = Support)) + 
  geom_bar(position = "dodge") +
  labs(title = "Support for Political Party by Education Level",
       x = "Education Level",
       y = "Count") +
  scale_fill_manual(values = c("Y" = "blue", "N" = "red"), name = "Support") +
  theme_minimal()


# Bayesian logistic regression model 
# Convert 'Support' into a binary numeric variable for modeling
data$SupportNumeric <- as.numeric(data$Support == "Y")

model_stan <- stan_glm(SupportNumeric ~ AgeGroup + IncomeGroup + EducationLevel, 
                       family = binomial(link = "logit"), 
                       data = data,
                       prior = normal(0, 2.5), 
                       prior_intercept = normal(0, 5),
                       iter = 2000)


print(summary(model_stan))


# MCMC diagnostics
mcmc_trace(model_stan)
mcmc_dens_overlay(model_stan)


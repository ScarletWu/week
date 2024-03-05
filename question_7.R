# Load necessary library
library(ggplot2)

# 1. Simulate independent variables
set.seed(123) 
n <- 100 
earth <- rnorm(n, mean=50, sd=10)
fire <- rnorm(n, mean=30, sd=5)
wind <- rnorm(n, mean=20, sd=4)
water <- rnorm(n, mean=40, sd=8)

# 2. Generate dependent variable + random noise
heart <- 5 + 1.5*earth + 2*fire + 0.5*wind + 3*water + rnorm(n, mean=0, sd=5)

data <- data.frame(earth, fire, wind, water, heart)

# 3. Fit linear regression model
model <- lm(heart ~ earth + fire + wind + water, data=data)

# Summary of the model
summary(model)

# Optional: Plotting
# Basic scatter plot of heart vs. one of the variables (e.g., earth) to visualize relationship
# Adjust variable names as needed for different plots
ggplot(data, aes(x=earth, y=heart)) +
  geom_point() +
  geom_smooth(method="lm", col="blue") +
  theme_minimal() +
  ggtitle("Relationship between Earth and Heart")

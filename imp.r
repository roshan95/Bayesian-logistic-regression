library(rstan)
library(dplyr)
library(tidyr)
library(bayesplot)
library(ggplot2)
library(gridExtra)



# Load data
diabetes_data <- read.csv("diabetes.csv")

#Create histograms for each variable by outcome
Glucose_ggplot <- ggplot(diabetes_data, aes(x = Glucose, fill = factor(Outcome))) + geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + labs(x = "Glucose", y = "Count", title = "Histogram of Glucose by Outcome")
BloodPressure_ggplot <- ggplot(diabetes_data, aes(x = BloodPressure, fill = factor(Outcome))) + geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + labs(x = "Blood Pressure", y = "Count", title = "Histogram of Blood Pressure by Outcome")
SkinThickness_ggplot <- ggplot(diabetes_data, aes(x = SkinThickness, fill = factor(Outcome))) + geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + labs(x = "Skin Thickness", y = "Count", title = "Histogram of Skin Thickness by Outcome")
Insulin_ggplot<- ggplot(diabetes_data, aes(x = Insulin, fill = factor(Outcome))) + geom_histogram(binwidth = 5, alpha = 0.5, position = "identity") + labs(x = "Insulin", y = "Count", title = "Histogram of Insulin by Outcome")
BMI_ggplot<- ggplot(diabetes_data, aes(x = BMI, fill = factor(Outcome))) + geom_histogram(binwidth = 1, alpha = 0.5, position = "identity") + labs(x = "BMI", y = "Count", title = "Histogram of BMI by Outcome")
DiabetesPedigreeFunction_ggplot <-ggplot(diabetes_data, aes(x = DiabetesPedigreeFunction, fill = factor(Outcome))) + geom_histogram(binwidth = 0.05, alpha = 0.5, position = "identity") + labs(x = "Diabetes Pedigree Function", y = "Count", title = "Histogram of Diabetes Pedigree Function by Outcome")
Age_ggplot<- ggplot(diabetes_data, aes(x = Age, fill = factor(Outcome))) + geom_histogram(binwidth = 2, alpha = 0.5, position = "identity") + labs(x = "Age", y = "Count", title = "Histogram of Age by Outcome")

grid.arrange(Glucose_ggplot, BloodPressure_ggplot, SkinThickness_ggplot, Insulin_ggplot, BMI_ggplot, DiabetesPedigreeFunction_ggplot,Age_ggplot,nrow=4,ncol=2)

pairs(diabetes_data)



# Split data into training and testing sets
set.seed(123)
train_idx <- sample(seq_len(nrow(diabetes_data)), size = 0.7 * nrow(diabetes_data))
train_data <- diabetes_data[train_idx, ]
test_data <- diabetes_data[-train_idx, ]


# Compile and fit the model
try({
  stan_fit <- stan(
    file = "bayesian.stan",
    data = list(
      N = nrow(train_data),
      K = ncol(train_data) - 1,
      X = as.matrix(train_data[, -1]),
      y = as.integer(train_data$Outcome)
    ),
    chains = 4,
    iter = 1000,
    warmup = 500,
    seed = 123
  )
}, silent = TRUE)

print(summary(stan_fit))


# Check for errors in the model fit
if (is(stan_fit, "try-error")) {
  cat("Stan model failed with error message:\n")
  cat(attr(stan_fit, "condition"), "\n")
} else if (is(stan_fit, "stanfit")) {
  # Extract posterior samples
  posterior_samples <- rstan::extract(stan_fit)
  # Generate posterior predictive distribution
  y_ppc <- posterior_samples$y_rep
  # Calculate posterior predictive checks
  ppc_data <- test_data %>% 
    mutate(y = as.integer(test_data$Outcome)) %>% 
    select(-Outcome) %>% 
    as.matrix()
  ppc_stats <- sapply(seq_len(nrow(ppc_data)), function(i) {
    mean(y_ppc[, i] == ppc_data[i, ])
  })
##Plot posterior predictive checks
plot(ppc_stats, type = "l", ylim = c(0, 1), 
       xlab = "Observation", ylab = "Posterior predictive p-value", 
       main = "Posterior predictive checks for logistic regression model") %>% 
       abline(h = 0.05, lty = 2, col = "red")

print(traceplot(stan_fit))
print(stan_dens(stan_fit))
print(stan_hist(stan_fit))


}
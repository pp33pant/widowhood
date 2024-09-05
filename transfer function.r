# transfer function
# transfer the Rdata files
# read the data
rm(list = ls())
library(tidyverse)

load("../data_for_graph.RData")
datasets <- mget(ls())

process_dataset <- function(data) {
  model_ci_lower <- lm(ci_lower_linear ~ ps.logit, data = data)
  model_ci_upper <- lm(ci_upper_linear ~ ps.logit, data = data)
  model_linear_fit <- lm(linear.fit.ci.mean ~ ps.logit, data = data)
  
  new_data <- data.frame(ps.logit = seq(0.02, 1, 0.02))
  
  new_data$ci_lower_linear <- predict(model_ci_lower, new_data)
  new_data$ci_upper_linear <- predict(model_ci_upper, new_data)
  new_data$linear_fit_ci_mean <- predict(model_linear_fit, new_data)
  
  new_data$ate <- unique(data$ate)
  
  return(new_data)
}

processed_datasets <- lapply(datasets, process_dataset)
save(processed_datasets, file = "../processed_data_for_graph.RData")

rm(list = ls())
library(tidyverse)

load("../data_for_graph_white.RData")
datasets <- mget(ls())

process_dataset <- function(data) {
  model_ci_lower <- lm(ci_lower_linear ~ ps.logit, data = data)
  model_ci_upper <- lm(ci_upper_linear ~ ps.logit, data = data)
  model_linear_fit <- lm(linear.fit.ci.mean ~ ps.logit, data = data)
  
  new_data <- data.frame(ps.logit = seq(0.02, 1, 0.02))
  
  new_data$ci_lower_linear <- predict(model_ci_lower, new_data)
  new_data$ci_upper_linear <- predict(model_ci_upper, new_data)
  new_data$linear_fit_ci_mean <- predict(model_linear_fit, new_data)
  
  new_data$ate <- unique(data$ate)
  
  return(new_data)
}

processed_datasets <- lapply(datasets, process_dataset)
save(processed_datasets, file = "../processed_data_for_graph_white.RData")

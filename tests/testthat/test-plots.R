# Load necessary libraries
library(data.table)
library(ggplot2)

# Set seed for reproducibility
set.seed(123)

# Generate a simple dataset with 1000 observations
obs_values <- rnorm(1000, mean = 50, sd = 10)
dt_test <- data.table(
    obs = obs_values,
    pred = obs_values + rnorm(1000, mean = 0, sd = 5)
)

# Test the function with the full dataset
p_full <- cali_plot.linear(
    data = dt_test,
    outcome_col = "obs",
    pred_col = "pred"
)

# Display the plot
print(p_full)

# Test the function with a 50% sample proportion to mitigate overplotting
p_sampled <- cali_plot.linear(
    data = dt_test,
    outcome_col = "obs",
    pred_col = "pred",
    sample_proportion = 0.5
)

# Display the downsampled plot
print(p_sampled)

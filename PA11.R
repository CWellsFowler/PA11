############################################################
# Chatoya Wells-Fowler
# COP2073C 
# 8/2/2026
# Analyzing linear relationship with a categorical predictor
############################################################

# Install packages (only run the first time)
install.packages("tidyverse")
install.packages("modelr")

# Load required libraries
library(tidyverse)
library(modelr)

# ---------------------------------------------------------
# Create a tibble using the cyl and wt columns from mtcars.
# Convert cyl to a categorical variable (factor).
# ---------------------------------------------------------

cars_tbl <- mtcars %>%
  select(cyl, wt) %>%
  mutate(cyl = factor(cyl))

# View the tibble
cars_tbl

# ---------------------------------------------------------
# Use lm function.
# ---------------------------------------------------------

model <- lm(wt ~ cyl, data = cars_tbl)

# Display model summary (optional but useful)
summary(model)

# ---------------------------------------------------------
# Create a prediction grid using data_grid() and pipes.
# ---------------------------------------------------------

predictions <- cars_tbl %>%
  data_grid(cyl) %>%
  add_predictions(model)

# View predicted values
predictions

# ---------------------------------------------------------
# Plot the original data with predicted values.
# ---------------------------------------------------------

ggplot(cars_tbl, aes(x = cyl, y = wt)) +
  geom_point() +
  geom_point(
    data = predictions,
    aes(y = pred),
    color = "red",
    size = 4
  ) +
  labs(
    title = "Vehicle Weight by Number of Cylinders",
    x = "Number of Cylinders",
    y = "Weight (1000 lbs)"
  )

# ---------------------------------------------------------
# Analysis
# ---------------------------------------------------------

# Analysis:
# Based on the results, the number of cylinders has an
# impact on the weight of a vehicle. As the number of
# cylinders increases, the average vehicle weight also
# increases. The red prediction points represent the
# average predicted weight for each cylinder category
# and are close to the center of the actual data points.
# Overall, the model does a good job of showing the
# relationship between the number of cylinders and
# vehicle weight in the mtcars dataset.
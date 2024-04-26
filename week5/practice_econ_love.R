--# Cost of loving index --

library(tidyverse)

cost_data <- tribble(
  ~"City", ~"Two-course meal for two people*", ~"Bottle of fine wine*", ~"Two drinks at a hotel bar", ~"Two cinema tickets", ~"Initial taxi meter charge",
  "Shanghai", 142.76, 85.76, 34.97, 17.33, 5.54,
  "New York", 130.00, 60.00, 36.00, 30.00, 3.30,
  "Bahrain*", 120.00, 30.00, 26.00, 12.00, 6.60,
  "Los Angeles", 100.00, 40.00, 28.00, 28.00, 3.63,
  "Paris", 100.00, 60.00, 28.00, 24.00, 7.70,
  "Amman", 90.00, 25.00, 16.00, 8.00, 0.70,
  "Caracas", 90.00, 15.00, 20.00, 6.00, 0.60,
  "Moscow", 80.00, 40.00, 25.00, 12.00, 0.55,
  "Milan", 80.00, 30.00, 28.00, 18.00, 6.05,
  "Beijing", 78.41, 54.74, 23.49, 11.11, 3.32,
  "Barcelona", 75.00, 20.00, 24.00, 16.00, 6.30,
  "St Petersburg", 70.00, 30.00, 15.00, 10.00, 2.00,
  "Abu Dhabi", 68.00, 45.00, 20.00, 16.50, 5.47,
  "Düsseldorf", 65.00, 25.00, 16.00, 20.00, 3.90,
  "Zurich", 60.00, 30.00, 24.00, 28.00, 6.50
)

cost_data <- cost_data %>%
  mutate(Total = rowSums(.[2:6]))

cost_data <- cost_data %>%
  arrange(desc(Total))

p <- ggplot(cost_data, aes(x = reorder("City", -Total), y = Total)) +
  geom_col(fill = "#E64646") +
  geom_text(aes(label = Total), vjust = -0.3, size = 3) +
  labs(title = "Cost-of-loving index",
       x = NULL, y = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

print(p)

results$cost_bar <- p
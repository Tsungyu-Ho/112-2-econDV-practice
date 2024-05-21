# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(lubridate)

# Create sample data frame
set.seed(123) # for reproducibility
dates <- seq(as.Date("2000-01-01"), as.Date("2001-12-01"), by = "month")
n <- length(dates)
data <- data.frame(
  date = dates,
  Microsoft = cumsum(runif(n, min = -0.05, max = 0.05)),
  Apple = cumsum(runif(n, min = -0.05, max = 0.05)),
  Alphabet = cumsum(runif(n, min = -0.05, max = 0.05)),
  Amazon = cumsum(runif(n, min = -0.05, max = 0.05)),
  Facebook = cumsum(runif(n, min = -0.05, max = 0.05))
)

# Transform data for ggplot using pivot_longer
data_long <- data %>%
  pivot_longer(cols = -date, names_to = "Company", values_to = "MarketCap")

# Plot using ggplot2
ggplot(data_long, aes(x = date, y = MarketCap, color = Company)) +
  geom_line(size = 1) +
  scale_color_manual(values = c("Microsoft" = "skyblue", "Apple" = "blue",
                                "Alphabet" = "red", "Amazon" = "orange", 
                                "Facebook" = "gray")) +
  labs(title = "Trillion-dollar tech",
       subtitle = "Market capitalisation",
       x = NULL,
       y = "$trn",
       color = NULL) +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(size = 16, face = "bold"),
        plot.subtitle = element_text(size = 12),
        axis.text = element_text(size = 10),
        axis.title.y = element_text(size = 12))

# Note: The y-axis data is random and does not represent actual market capitalization.


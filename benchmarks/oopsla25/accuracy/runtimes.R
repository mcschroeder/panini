# Load necessary libraries
library(ggplot2)
library(dplyr)
library(scales)

results_by_subject <- read.csv("results/results_by_subject.csv")
categories <- read.csv("../subjects/categories.csv")

merged_data <- merge(results_by_subject, categories, by = "subject")

merged_data$method <- factor(merged_data$method, 
                             levels = c("panini", "stalagmite", "mimid", "ttt"),
                             labels = c("Panini", "Stalagmite", "Mimid", "TTT"))

merged_data$category <- factor(merged_data$category, 
                               levels = c( "loop", "cond", "straight"),
                               labels = c( "+ Loops", "+ Cond.", "Straight"))


ggplot(merged_data, aes(y = category, x = time_ms, fill = category)) +
  geom_boxplot(color = "black", fill = "white") + 
  facet_wrap(~ method,  nrow = 4) +
    scale_x_continuous(
    trans = 'log10',
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    labels = function(x) (x / 1000)
  ) +
  labs(x = "Time in seconds (log scale)", y = "")
  theme_minimal()

#ggsave("plot.pdf")

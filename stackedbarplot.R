# Load library
library(ggplot2)

# Load the data
data <- read.csv("adult income1.csv")

# Filter relevant columns
filtered_data <- data[, c("education", "income")]

# Summarize data to calculate proportions
proportions <- as.data.frame(prop.table(table(filtered_data$education, filtered_data$income), margin = 1))
colnames(proportions) <- c("Education", "Income", "Proportion")

# Create a stacked bar plot
ggplot(proportions, aes(x = Education, y = Proportion, fill = Income)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = scales::percent(Proportion)), 
            position = position_stack(vjust = 0.5), size = 2) +
  theme_minimal() +
  labs(title = "Proportion of Income by Education Level",
       x = "Education Level",
       y = "Proportion",
       fill = "Income") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Perform a chi-squared test
chi_test <- chisq.test(table(filtered_data$education, filtered_data$income))
print(chi_test)

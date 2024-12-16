# Comprehensive Analysis of Categorical Variables and Likert Scale Data

# Load necessary libraries
library(dplyr)     # Data manipulation
library(tidyr)     # Data reshaping
library(ggplot2)   # Advanced visualization
library(car)       # Advanced statistical tests
library(likert)    # Likert scale analysis
library(corrplot)  # Correlation visualization

# Set seed for reproducibility
set.seed(123)

# Generate example dataset
# Simulating data with gender and occupation as categorical variables
# and 10 Likert scale questions (5-point scale)
create_dataset <- function(n_rows = 200) {
  # Define categorical variables
  genders <- c("Male", "Female")
  occupations <- c("Student", "Professional", "Entrepreneur", "Artist", "Educator")
  
  # Create dataset
  data <- data.frame(
    gender = sample(genders, n_rows, replace = TRUE),
    occupation = sample(occupations, n_rows, replace = TRUE),
    # 10 Likert scale questions (1-5 scale)
    # Simulating different response patterns
    q1 = sample(1:5, n_rows, replace = TRUE),
    q2 = sample(1:5, n_rows, replace = TRUE),
    q3 = sample(1:5, n_rows, replace = TRUE),
    q4 = sample(1:5, n_rows, replace = TRUE),
    q5 = sample(1:5, n_rows, replace = TRUE),
    q6 = sample(1:5, n_rows, replace = TRUE),
    q7 = sample(1:5, n_rows, replace = TRUE),
    q8 = sample(1:5, n_rows, replace = TRUE),
    q9 = sample(1:5, n_rows, replace = TRUE),
    q10 = sample(1:5, n_rows, replace = TRUE)
  )
  return(data)
}

# Generate the dataset
likert_data <- create_dataset()

# 1. Descriptive Statistics
# Summary of categorical variables
cat_summary <- likert_data %>%
  group_by(gender, occupation) %>%
  summarise(count = n(), .groups = 'drop')
print("Categorical Variable Distribution:")
print(cat_summary)

# 2. Chi-Square Test of Independence
# Test relationship between gender and occupation
chi_test <- chisq.test(table(likert_data$gender, likert_data$occupation))
print("Chi-Square Test Results:")
print(chi_test)

# 3. Likert Scale Analysis
# Prepare Likert scale data
likert_cols <- c("q1", "q2", "q3", "q4", "q5", "q6", "q7", "q8", "q9", "q10")

# Convert Likert scale columns to factors with specified levels
likert_data_factors <- likert_data %>%
  mutate(across(all_of(likert_cols), 
                ~ factor(., 
                         levels = 1:5, 
                         labels = c("Strongly Disagree", 
                                    "Disagree", 
                                    "Neutral", 
                                    "Agree", 
                                    "Strongly Agree"))))

# Create Likert object with formatted data
likert_obj <- likert(likert_data_factors[, likert_cols])





# Visualization of Likert data
# 1. Stacked Bar Plot
likert_plot <- plot(likert_obj) +
  theme_minimal() +
  labs(title = "Likert Scale Responses Across Questions",
       x = "Questions", 
       y = "Percentage of Responses")

print(likert_plot)
# Save the plot
ggsave("likert_stacked_plot.png", likert_plot, width = 10, height = 6)

# 4. Grouped Analysis
# Analyze Likert responses by gender
gender_likert <- likert(likert_data_factors[, likert_cols], 
                        grouping = likert_data_factors$gender)
gender_plot <- plot(gender_likert) +
  theme_minimal() +
  labs(title = "Likert Responses by Gender",
       x = "Questions", 
       y = "Percentage of Responses")

print(gender_plot)
ggsave("likert_by_gender.png", gender_plot, width = 10, height = 6)

# Analyze Likert responses by occupation
occupation_likert <- likert(likert_data_factors[, likert_cols], 
                            grouping = likert_data_factors$occupation)
occupation_plot <- plot(occupation_likert) +
  theme_minimal() +
  labs(title = "Likert Responses by Occupation",
       x = "Questions", 
       y = "Percentage of Responses")
print(occupation_plot)
ggsave("likert_by_occupation.png", occupation_plot, width = 12, height = 8)

# 5. Advanced Statistical Analysis
# One-way ANOVA to test differences across groups
# Convert Likert scores to numeric for parametric testing
likert_data_numeric <- likert_data %>%
  mutate(avg_score = rowMeans(select(., all_of(likert_cols))))

# ANOVA by gender
gender_anova <- aov(avg_score ~ gender, data = likert_data_numeric)
print("ANOVA Results by Gender:")
summary(gender_anova)

# ANOVA by occupation
occupation_anova <- aov(avg_score ~ occupation, data = likert_data_numeric)
print("ANOVA Results by Occupation:")
summary(occupation_anova)

# 6. Correlation Analysis
# Correlation between Likert scale questions
corr_matrix <- cor(likert_data[, likert_cols])

# Visualize correlation
png("correlation_plot.png", width = 800, height = 800)
corrplot(corr_matrix, method = "color", 
         type = "full", 
         addCoef.col = "black",
         tl.col = "black", 
         tl.srt = 45,
         title = "Correlation Between Likert Scale Questions")
dev.off()

# Print out key findings
print("Analysis Complete. Check generated visualization files for detailed insights.")

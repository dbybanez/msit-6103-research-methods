# MSIT 6103 Research Methods
# Activity 4 (Practice)

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: September 30, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1. 2018 Nigeria Demographic and Health Surveys (NDHS)

# ------ I. Initial setup for working directory and load packages -------------

# Install packages
if (!requireNamespace("this.path", quietly = TRUE)) { install.packages("this.path") }
if (!requireNamespace("readr", quietly = TRUE)) { install.packages("readr") }
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }
if (!requireNamespace("pheatmap", quietly = TRUE)) { install.packages("pheatmap") }
if (!requireNamespace("confintr", quietly = TRUE)) { install.packages("confintr") }

# Load packages
library(this.path)
library(readr)
library(dplyr)
library(pheatmap)
library(confintr)

# Set the root directory to the location of the current script
# This helps in managing file paths for data import/export
script_root <- tryCatch(this.path::this.dir(), error = function(e) normalizePath(getwd()))

# Function to create file paths relative to the script root
p <- function(...) file.path(script_root, ...)

# Set working directory to script root
setwd(script_root)

# Set output directory and create it
output_dir <- "output"
plot_dir <- file.path(output_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)


# ------ 1. 2018 Nigeria Demographic and Health Surveys (NDHS) ----------------

# Load the dataset
dataset <- read_csv(p("data", "children anemia.csv"))

# Rename columns for easier reference
# Example: Rename "Anemia level...8" to "Anemia level"
colnames(dataset)[colnames(dataset) == "Anemia level...8"] <- "Anemia level"

# Display the column names to verify renaming
colnames(dataset)

# Select specific columns: 'Highest educational level' and 'Anemia level'
# Using select() to choose specific columns from the data frame
# @param {data} The data frame to select columns from
# @param {...} The names of the columns to select
# @return {data.frame} - data frame with only the selected columns
selected_data <- dataset %>%
  select('Highest educational level', 'Anemia level')

# Summarize the data to create a contingency table
# Using table() to create a contingency table of the selected columns
# @param {...} The factors to cross-tabulate
# @return {table} - contingency table of the selected factors
contingency_table <- table(selected_data$'Highest educational level', selected_data$'Anemia level')

# Print the contingency table
print(contingency_table)

# Perform Chi-square test of independence
# Using chisq.test() to perform the Chi-square test on the contingency table
# @param {x} The contingency table to test
# @return {htest} - result of the Chi-square test
chi_square_test <- chisq.test(contingency_table)
print(chi_square_test)

# Extract and print the observed counts from the Chi-square test result
# These represent the actual counts of children with different anemia levels 
# across each mother’s education level.
observed_counts <- chi_square_test$observed
print(observed_counts)

# Extract and print the expected counts from the Chi-square test result.
# These counts are calculated under the assumption that there is no association
# between the mother’s education level and the child’s anemia status.
expected_counts <- chi_square_test$expected
print(round(expected_counts, 2))

# Extract and print the Pearson residuals from the Chi-square test result
# These residuals indicate the difference between observed and expected counts,
# standardized by the expected counts. They help identify which cells contribute
# most to the overall Chi-square statistic.
pearson_residuals <- chi_square_test$residuals
print(round(pearson_residuals, 2))


# Residuals help us understand how much the observed data deviate from what we 
# would expect under the null hypothesis. Here's how to interpret them:

# Positive Residuals
# A positive residual means the observed count is higher than the expected 
# count. For example, a residual of +5.96 for "Not anemic" in the Higher
# Education group suggests that signifi cantly more children than expected
# are not anemic when their mothers have higher education.

# Negative Residuals
# A negative residual indicates the observed count is lower than expected. For 
# instance, a residual of –5.74 for "Moderate" anemia in the Higher Education 
# group means that signifi cantly fewer children than expected have moderate 
# anemia among mothers with higher education.

# Large Residuals (Positive or Negative)
# Large residuals (in either direction) signal a strong deviation from the 
# expected values. These values contribute most to the overall chi-square 
# statistic and help identify where the biggest differences lie. In the example
# above, the large positive residual for “Not anemic” and large negative 
# residual for “Moderate anemia” in the Higher Education group indicate a 
# substantial effect of maternal education on child anemia levels.

# Small Residuals
# Residuals close to zero suggest that the observed and expected counts are 
# very similar, indicating little or no deviation. For example, residuals for 
# the Primary Education group across different anemia levels are relatively 
# small, implying that the data closely follow the expected distribution for 
# this group.

# Calculate and print the contribution of each cell to the overall Chi-square statistic
# This is calculated as (observed - expected)^2 / expected for each cell in the table.
# It helps identify which specific combinations of education level and anemia status
# contribute most to the Chi-square statistic.
contributions <- (observed_counts - expected_counts)^2 / expected_counts

# Calculate percentage contributions
# This shows the relative contribution of each cell to the total Chi-square statistic.
total_chi_square <- chi_square_test$statistic
percentage_contributions <- 100 * contributions / total_chi_square
print("Percentage Contributions:")
print(round(percentage_contributions, 2))

# Visualize the percentage contributions using a heatmap
# Using pheatmap() to create a heatmap of the percentage contributions
# @param {mat} The matrix of values to visualize
# @param {display_numbers} Whether to display the values on the heatmap
# @param {cluster_rows} Whether to cluster the rows
# @param {cluster_cols} Whether to cluster the columns
# @param {main} The title of the heatmap
# @return {pheatmap} - heatmap visualization of the percentage contributions
pheatmap(percentage_contributions, display_numbers = TRUE, cluster_rows = FALSE, cluster_cols = FALSE, main = "Percentage Contribution to Chi-Square Statistic")

# Cramér’s V
# Cramer’s v was developed to measure the association between two categorical 
# variables. Since Cramer’s v is based on Chi-squared test, they share similar 
# assumptions: 
# 1. The variables should be measured at an ordinal or nominal level. 
# 2. Each variable should contain two or more independent groups.

# Note that although Cramer’s v can be applied to ordinal variables, there will
# be loss of information since the coeffi cient does not care about the order 
# of the categories. Cramer’s v ranges from 0 to 1, with 0 indicating no 
# association at all and 1 indicating perfect association.

# The effect size is calculated in the following manner: 
# 1. Determine which fi eld has the fewest number of categories. 
# 2. Subtract 1 from the number of categories in this fi eld. 
# 3. Multiply the result by the total number of records. 
# 4. Divide the chi-square value. The chi-square value is obtained from the 
#    chi-square test of independence 
# 5. Take the square root.

# Effect size (ES)             Interpretation
# ES ≤ 0.2            The result is weak. Although the result is statistically 
#                     significant, the fields are only weakly associated.
# 0.2 < ES ≤ 0.6      The result is moderate. The fields are moderately associated.
# ES > 0.6            The result is strong. The fields are strongly associated.

# Assume we have 500 students' survey responses, who divided their top three 
# topics into three categories: math, science, and literature. We also gather 
# information about their gender, which is divided into Male and Female 
# categories. We aim to find out if there is a correlation between a person's 
# favorite subjects and gender.

# Create a contingency table
subject_gender <- matrix(c(50, 60, 40, 70, 90, 80), nrow = 3, byrow = TRUE, dimnames = list(c("Math", "Science", "Literature"), c("Male", "Female")))

# Calculate Cramér's V
cramers_v <- cramersv(subject_gender)
print(cramers_v)

# A business wants to examine the correlation between product categories and 
# customer satisfaction levels. They gather survey information from three 
# hundred clients, classifying product categories as home appliances, apparel, 
# and electronics and assigning satisfaction ratings of Low, Medium, or High.

# Create a contingency table
satisfaction_product <- matrix(c(30, 40, 20, 50, 30, 10, 20, 40, 20), nrow = 3, byrow = TRUE, dimnames = list(c("Low", "Medium", "High"), c("Electronics", "Clothing", "Home Appliances")))

# Calculate Cramér's V
cramers_v <- cramersv(satisfaction_product)
print(cramers_v)



# End of Activity 4 (Practice)


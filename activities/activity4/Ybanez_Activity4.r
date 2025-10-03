# MSIT 6103 Research Methods
# Activity 4

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: October 1, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1. Titanic: class (rows) x rescue status (cols)
# -- 2. Insurance satisfaction (3x2), then collapse to 2x2

# =============================================================================
# I. Initial setup for working directory and load packages
# =============================================================================

# Install packages
if (!requireNamespace("this.path", quietly = TRUE)) { install.packages("this.path") }
if (!requireNamespace("confintr", quietly = TRUE)) { install.packages("confintr") }

# Load packages
library(this.path)
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

# =============================================================================
# 1) Titanic: class (rows) x rescue status (cols)
# =============================================================================

# The famous passenger liner Titanic hit an iceberg in 1912 and sank. A total 
# of 337 passengers travelled in first class, 285 in second class, and 721 in 
# third class. In addition, there were 885 staff members on board. Not all 
# passengers could be rescued. Only the following were rescued: 135 from the 
# first class, 160 from the second class, 541 from the third class and 674 staff.

# The data are sumarized in the following contingency table:
#                | Rescued | Not Rescued | Total
# ---------------|---------|-------------|------------
# First Class    |   135   |     202     |  337
# Second Class   |   160   |     125     |  285
# Third Class    |   541   |     180     |  721
# Staff          |   674   |     211     |  885
# ---------------|---------|-------------|------------
# Total          |  1510   |     718     | 2228

# Build the contingency tabl of observed counts
titanic_data <- matrix(
  c(135, 337 - 135,
    160, 285 - 160,
    541, 721 - 541,
    674, 885 - 674),
  nrow = 4, byrow = TRUE,
  dimnames = list(
    Class = c("First_Class", "Second_Class", "Third_Class", "Staff"),
    Rescue = c("Rescued", "Not_Rescued"))
)

# (a) Determine and interpret the contingency table for the variables "travel
# class" and "rescue status".

# Looking at the raw counts provided in the problem, we have two categorical 
# variables: "Travel Class" and "Rescue Status". The contingency table simply 
# shows the joint frequency distribution. Each cell tells us how many people in
# a specific travel class were rescued or not.

# View the contingency table of observed counts
print(titanic_data)

# The marginal totals (the sums of rows and columns) provide additional context
addmargins(titanic_data)

# Interpretation (counts-level):
# - The table shows how rescue outcomes are distributed within each travel class.
# - First Class had 135 rescued and 202 not rescued (total 337).
# - Second Class had 160 rescued and 125 not rescued (total 285).
# - Third Class had 541 rescued and 180 not rescued (total 721).
# - Staff had 674 rescued and 211 not rescued (total 885).
# - Overall: 1510 rescued and 718 not rescued out of 2228.

# (b) Use a contingency table to summarize the conditional relative frequency
# distributions of rescue status given travel class. Could there be an
# association of the two variables?

# We want the conditional distribution of Rescue Status given Travel Class.
# This means we want to look at the proportions of rescued vs not rescued
# within each travel class using prop.table() with margin = 1.
titanic_conditional <- prop.table(titanic_data, margin = 1)
print(titanic_conditional)

# Convert to percentages for easier interpretation
titanic_conditional_percent <- round(titanic_conditional * 100, 1)
print(titanic_conditional_percent)

# Make a neater table. Sum to 100% across rows
titanic_conditional_percent <- as.data.frame.matrix(titanic_conditional_percent)
titanic_conditional_percent$Total <- rowSums(titanic_conditional_percent)
print(titanic_conditional_percent)

# Interpretation (percentage-level):
# - First Class: 40.1% rescued, 59.9% not rescued
# - Second Class: 56.1% rescued, 43.9% not rescued
# - Third Class: 75.0% rescued, 25.0% not rescued
# - Staff: 76.1% rescued, 23.9% not rescued
# - There appears to be an association: higher travel classes (First and Second)
#   had lower rescue rates compared to lower classes (Third and Staff).
# - This suggests that travel class may have influenced rescue likelihood.

# (c) What would the contingency table from (a) look like under the 
# independence assumption? Calculate Cramer's V statistic. Is there any 
# association between travel class and rescue status?

# Independence assumption means the distribution of Rescue Status is the same
# across all Travel Classes. We can calculate the expected counts under this
# assumption using chisq.test().
chi_square_test <- chisq.test(titanic_data)
expected_counts <- chi_square_test$expected
print(round(expected_counts, 2))

# Interpretation (independence-level):
# - The expected counts show what we would expect if there were no association
#   between travel class and rescue status.
# - For example, for First Class, we would expect 228.03 rescued and 108.97
#   not rescued if rescue status were independent of travel class.
# - Comparing observed vs expected counts, we see large deviations, suggesting
#   that the variables are not independent.

# Calculate Cramer's V using confintr package
# Using cramersv() function from confintr package
# @param {mat} The contingency table (matrix)
# @return {numeric} - Cramer's V statistic
cramers_v <- cramersv(titanic_data)
print(cramers_v)

# Calculate Cramer's V manually for verification
n <- sum(titanic_data)  # total sample size
min_dim <- min(nrow(titanic_data) - 1, ncol(titanic_data) - 1)
cramers_v_manual <- sqrt(chi_square_test$statistic / (n * min_dim))
print(round(cramers_v_manual, 4))

# Interpretation (Cramer's V):
# - Cramer's V is approximately 0.28586, indicating a moderate association
#   between travel class and rescue status.
# - Values closer to 0 indicate weak association, while values closer to 1
#   indicate strong association.
# - Therefore, we conclude that there is a moderate association between travel
#   class and rescue status.

# (d) Combine the categories "first class" and "second class" as well as 
# "third class" and "staff". Create a contingency table based on these new 
# categories. Determine and interpret Cramer's V, the odds ratio, and relative 
# risks of your choice.

# Thought: Combining categories can help simplify the analysis and may reveal
# stronger associations if the combined groups have similar characteristics.
# We will combine First and Second Class into "First_Second_Class" and Third
# Class and Staff into "Third_Staff".

# Combine categories and create new contingency table
titanic_combined <- matrix(
  c(135 + 160, (337 - 135) + (285 - 160),
    541 + 674, (721 - 541) + (885 - 674)),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    Class = c("First_Second_Class", "Third_Staff"),
    Rescue = c("Rescued", "Not_Rescued"))
)
print(titanic_combined)

# Calculate Cramer's V for the combined table using confintr package
cramers_v_combined <- cramersv(titanic_combined) # 0.271
print(cramers_v_combined)

# Interpretation (Cramer's V for combined):
# - Cramer's V for the combined table is approximately 0.2709373, indicating a moderate
#   association between the new travel class categories and rescue status.
# - This suggests that even after combining categories, there remains a moderate
#   association between travel class and rescue status.
# - The association is slightly weaker than before combining, but still notable.

# Calculate Odds Ratio
# Odds ratio compares the odds of an event occurring in one group to the odds
# of it occurring in another group. Here, we compare the odds of being rescued
# in First_Second_Class vs Third_Staff.

# Odds of being rescued in First_Second_Class
odds_first_second <- (135 + 160) / ((337 - 135) + (285 - 160)) # 0.902
# Odds of being rescued in Third_Staff
odds_third_staff <- (541 + 674) / ((721 - 541) + (885 - 674))  # 3.11
# Odds Ratio
odds_ratio <- odds_first_second / odds_third_staff
print(odds_ratio)

# Interpretation (Odds Ratio):
# - The odds ratio is approximately 0.2903185, indicating that the odds of being
#   rescued are about 29% as high for First_Second_Class compared to Third_Staff.
# - This suggests that individuals in the First_Second_Class had significantly
#   lower odds of being rescued compared to those in Third_Staff.
# - An odds ratio less than 1 indicates rescue odds are lower in the first group
#   compared to the second group.

# Calculate Relative Risk
# Relative risk compares the probability of an event occurring in one group to
# the probability of it occurring in another group. Here, we compare the risk of
# being rescued in First_Second_Class vs Third_Staff.

# Risk of being rescued in First_Second_Class
risk_first_second <- (135 + 160) / (337 + 285) # 0.474
# Risk of being rescued in Third_Staff
risk_third_staff <- (541 + 674) / (721 + 885) # 0.757
# Relative Risk
relative_risk <- risk_first_second / risk_third_staff
print(relative_risk)

# Interpretation (Relative Risk):
# - The relative risk is approximately 0.6269038, indicating that the risk of being
#   rescued is about 63% as high for First_Second_Class compared to Third_Staff.
# - This suggests that individuals in the First_Second_Class had a lower risk of
#   being rescued compared to those in Third_Staff, but the difference is less
#   pronounced than indicated by the odds ratio.

# Interpretation summary for (d):
# - After combining, the table shows First_Second_Class had 295 rescued (47%) 
#   vs Third_Staff with 1215 rescued (76%).
# - Cramer's V ≈ 0.271 → still a moderate association, only slightly weaker than the original (~0.286).
#   This is expected since collapsing categories reduces detail.
# - Odds Ratio ≈ 0.29 → the odds of being rescued in First_Second_Class were about 71% lower
#   than in Third_Staff.
# - Relative Risk ≈ 0.63 → the probability of being rescued was only about 63% as high
#   in First_Second_Class compared to Third_Staff.
# - Both OR and RR confirm that rescue likelihood was consistently lower
#   for First_Second_Class passengers.

# (e) Given the results from (a) to (d), what are your conclusions?

# - (a) Raw counts showed clear differences in rescue outcomes by travel class.
# - (b) Conditional relative frequencies highlighted that First Class had the lowest
#   rescue rate (~40%) while Third Class and Staff had the highest (~75%).
# - (c) Under independence, observed counts differed greatly from expected counts.
#   Cramer's V (~0.286) indicated a moderate association.
# - (d) Even after collapsing into two groups, moderate association remained
#   (Cramer's V ~0.271), and both Odds Ratio (<1) and Relative Risk (<1)
#   confirmed rescue chances were lower for First_Second_Class than for Third_Staff.
# - Overall conclusion: Travel class and rescue status are not independent.
#   There is a clear, moderate association, individuals in Third Class or Staff
#   were more likely to be rescued than those in First or Second Class.

# =============================================================================
# 2) Insurance satisfaction (3x2), then collapse to 2x2
# =============================================================================

# A total of 150 customers of a petrol station are asked about their 
# satisfaction with their car and motorbike insurance. The results are 
# summarized below:

#                     | Satisfied | Unsatisfied | Total
# --------------------|-----------|-------------|------------
# Car                 |     33    |     25      |  58
# Car (diesel engine) |     29    |     31      |  60
# Motorbike           |     12    |     20      |  32
# --------------------|-----------|-------------|------------
# Total               |     74    |     76      |  150

# Build the contingency table of observed counts
insurance_data <- matrix(
  c(33, 25,
    29, 31,
    12, 20),
  nrow = 3, byrow = TRUE,
  dimnames = list(
    Vehicle = c("Car", "Car_Diesel", "Motorbike"),
    Satisfaction = c("Satisfied", "Unsatisfied"))
)
print(insurance_data)

# (a) Determine and interpret Pearson's x2 statistic, and Cramer's V.

# Calculate Chi-square test
chi_square_test_insurance <- chisq.test(insurance_data)
print(chi_square_test_insurance)
# x2 = 3.144, df = 2, p-value = 0.2076

# Cramer's V using confintr package
cramers_v_insurance <- cramersv(insurance_data)
print(cramers_v_insurance)
# 0.1447759

# Interpretation (Chi-square and Cramer's V):
# - The Chi-square statistic is approximately 3.144 with 2 degrees of freedom
#   and a p-value of 0.2076.
# - Since the p-value is greater than 0.05, we fail to reject the null hypothesis
#   of independence. This suggests that there is no statistically significant
#   association between vehicle type and satisfaction level.
# - Cramer's V is approximately 0.145, indicating a weak association between
#   vehicle type and satisfaction level.
# - Overall, there is no strong evidence to suggest that satisfaction levels
#   differ significantly by vehicle type. And both statistics point to a weak
#   or negligible association.

# (b) Combine the categories "car" and "car (diesel engine)" and produce the 
# corresponding 2 × 2 table. Calculate x2 as efficiently as possible and give a
# meaningful interpretation of the odds ratio.

# Combine categories and create new contingency table
insurance_combined <- matrix(
  c(33 + 29, 25 + 31,
    12, 20),
  nrow = 2, byrow = TRUE,
  dimnames = list(
    Vehicle = c("Car_Total", "Motorbike"),
    Satisfaction = c("Satisfied", "Unsatisfied"))
)
print(insurance_combined)

# Calculate Chi-square manually wihtout continuity correction
# Using the formula: χ² = N(ad - bc)² / ((a+b)(c+d)(a+c)(b+d))
# where a, b, c, d are the cell counts in the 2x2 table
# a = 33 + 29 = 62 (Car_Total, Satisfied)
# b = 25 + 31 = 56 (Car_Total, Unsatisfied)
# c = 12 (Motorbike, Satisfied)
# d = 20 (Motorbike, Unsatisfied)
a <- 62; b <- 56; c <- 12; d <- 20
N <- a+b+c+d
chi_sq_uncorr <- N * ( (a*d - b*c)^2 ) / ((a+b)*(c+d)*(a+c)*(b+d))
print(chi_sq_uncorr)
# 2.278823

# Calculate Chi-square test manually with Yates' continuity correction
# Yates' correction subtracts 0.5 from the absolute difference between
# observed and expected frequencies to reduce bias in small samples.
chi_sq_yates <- N * ((abs(a*d - b*c) - N/2)^2) / ((a+b)*(c+d)*(a+c)*(b+d))
print(chi_sq_yates)
# 1.716753

# Calculate Chi-square test using chisq.test() without continuity correction
chi_square_test_yates <- chisq.test(insurance_combined, correct = FALSE)
print(chi_square_test_yates)
# x2 = 2.2788, df = 1, p-value = 0.1312

# Calculate Chi-square test for combined table with continuity correction
chi_square_test_combined <- chisq.test(insurance_combined)
print(chi_square_test_combined)
# x2 = 1.7168, df = 1, p-value = 0.1901

# Efficient formula: x2 (uncorrected) ≈ 2.279, p ≈ 0.131
# R's default with Yates correction: x2 ≈ 1.717, p ≈ 0.190
# Both > 0.05 → no significant association.

# Calculate Odds Ratio
# Odds of being satisfied with Car_Total
odds_car_total <- (33 + 29) / (25 + 31) # 1.0323
# Odds of being satisfied with Motorbike
odds_motorbike <- 12 / 20  # 0.6
# Odds Ratio
odds_ratio_insurance <- odds_car_total / odds_motorbike
print(odds_ratio_insurance)
# 1.845238

# Interpretation (Chi-square and Odds Ratio for combined):
# - The Chi-square statistic for the combined table is approximately 1.7168 with
#   1 degree of freedom and a p-value of 0.1901 (with continuity correction).
# - Since the p-value is greater than 0.05, we again fail to reject the null hypothesis
#   of independence. This suggests that there is still no statistically significant
#   association between vehicle type and satisfaction level after combining categories.
# - The Odds Ratio is approximately 1.845, indicating that the odds of being satisfied
#   are about 1.85 times higher for Car_Total compared to Motorbike.
# - However, since the Chi-square test did not show significance, we should be cautious
#   in interpreting the Odds Ratio as indicating a meaningful difference in satisfaction
#   between the two vehicle types.
# - Overall, even after combining categories, there is no strong evidence to suggest
#   that satisfaction levels differ significantly by vehicle type.

# (c) Compare the results from (a) and (b).

# Interpretation:
# - In (a), using the original 3x2 table, the Chi-square statistic was ≈ 3.144
#   with df = 2 and p ≈ 0.208. Cramer's V was ≈ 0.145, showing a weak association.
# - In (b), after collapsing categories into a 2x2 table (Car_Total vs Motorbike),
#   the Chi-square statistic was ≈ 2.279 (uncorrected) or ≈ 1.717 (with Yates correction),
#   with p-values of ≈ 0.131 and ≈ 0.190 respectively. Both tests again suggest
#   no statistically significant association.
# - The Odds Ratio for the collapsed table was ≈ 1.85, suggesting car owners may
#   have higher odds of being satisfied than motorbike owners. However, since the
#   Chi-square test was not significant, this difference should be interpreted
#   with caution.
# - Overall, both analyses point to the same conclusion: there is no strong evidence
#   of a meaningful association between vehicle type and satisfaction level.
#   Collapsing categories slightly changed the numbers but did not change the
#   overall conclusion.

# =============================================================================
# 3) Chi-Square Goodness-of-Fit Test
# =============================================================================

# A shop owner claims that an equal number of customers come into his shop each
# weekday. To test this hypothesis, a researcher records the number of
# customers that come into the shop in a given week and finds the following:
# - Monday:    50 customers
# - Tuesday:   60 customers
# - Wednesday: 40 customers
# - Thursday:  47 customers
# - Friday:    53 customers

# Perform a Chi-Square goodness of fit test in R to determine if the data is 
# consistent with the shop owner's claim.

# Create a vector of observed counts
observed_counts <- c(50, 60, 40, 47, 53)
names(observed_counts) <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
print(observed_counts)

# Create a vector of expected counts (equal distribution)
expected_counts <- rep(sum(observed_counts) / length(observed_counts), length(observed_counts))
print(expected_counts) # 50 for each day

# Perform the Chi-Square goodness-of-fit test
chi_square_test <- chisq.test(observed_counts, p = expected_counts / sum(expected_counts))

# Print the results
print(chi_square_test)
# X-squared = 4.36, df = 4, p-value = 0.3595

# Interpretation:
# - The Chi-square statistic is approximately 4.36 with 4 degrees of freedom
#   and a p-value of 0.3595.
# - Since the p-value is greater than 0.05, we fail to reject the null hypothesis.
# - This suggests that there is no statistically significant difference between
#   the observed and expected customer counts across the weekdays.
# - Therefore, the data is consistent with the shop owner's claim that an equal
#   number of customers come into the shop each weekday.



# End of Activity 4



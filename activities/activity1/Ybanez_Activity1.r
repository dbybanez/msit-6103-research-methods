# MSIT 6103 Research Methods
# Activity 1

# By: David Ybanez (MSIT 2)

# Contents include:
# -- 1. Mathematical Operations 
# -- 2. Genetics Class Scores
# -- 3. Temperature Readings

# ------ 1. Mathematical Operations -------------------------------------------

# 1.A. Calculate the square root of 25 multiplied by 0.64.
# 1.B. Determine the natural logarithm of 20 multiplied by 0.5.
# 1.C. Calculate the exponential of 4 multiplied by 6.
# 1.D. Calculate the absolute value of -15 plus the square root of 9.

# 1.A. Square root of 25 multiplied by 0.64
sqrt_val <- sqrt(25) * 0.64
sqrt_val

# 1.B. Natural logarithm of 20 multiplied by 0.5
ln_val <- log(20) * 0.5
ln_val

# 1.C. Exponential of 4 multiplied by 6
exp_val <- exp(4) * 6
exp_val

# 1.D. Absolute value of -15 plus the square root of 9
abs_val <- abs(-15 + sqrt(9))
abs_val

# ------ 2. Genetics Class Scores ---------------------------------------------

# 2. In Genetics class, the students got the following scores out of a 100.
#    Determine the mean, variance and standard deviation. Repeat the first five
#    scores of the series three times. Arrange the following scores in a
#    descending order. Extract the scores of the top three performers and store
#    these scores in a new variable called top_three. Now, extract all the
#    scores less than or equal to 89 and assign them to a variable called
#    low_scores. Determine who got the highest score and the lowest score?

# Scores of Genetics class

# Students - Vector of student names
students <- c("Olivia", "Ethan", "Cruz", "Liam", "Callie", "Meredith", "Derek")

# Scores - Vector of student scores
scores <- c(86, 72, 90, 50, 98, 96, 99)

# Data frame of students and their scores
genetics_df <- data.frame(Student = students, Score = scores)
genetics_df

# Mean
# Using mean() function
# @param {numeric} (x) - numeric vector
# @return {numeric} - mean of the numeric vector
mean_scores <- mean(scores)
mean_scores

# Variance
# Using var() function
# @param {numeric} (x) - numeric vector
# @return {numeric} - variance of the numeric vector
var_scores <- var(scores)
var_scores

# Standard Deviation
# Using sd() function
# @param {numeric} (x) - numeric vector
# @return {numeric} - standard deviation of the numeric vector
sd_scores <- sd(scores)
sd_scores

# Repeat the first five scores three times
# Using rep() function
# @param {vector} (x) - vector of values to be repeated
# @param {integer} (times) - number of times to repeat the values
# @return {vector} - vector with repeated values
repeated_scores <- rep(scores[1:5], times = 3)
repeated_scores

# Arrange scores in descending order
# Using sort() function with decreasing argument set to TRUE
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @return {vector} - sorted vector
sorted_scores <- sort(scores, decreasing = TRUE)
sorted_scores

# Arrange students according to sorted scores
# Using order() function
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @return {vector} - indices that would sort the vector
sorted_students <- students[order(scores, decreasing = TRUE)]
sorted_students

# View sorted students with their scores
# Creating a data frame of sorted students and their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
sorted_genetics_df <- data.frame(Student = sorted_students, Score = sorted_scores)
sorted_genetics_df

# Extract the scores of the top three performers
top_three <- sorted_scores[1:3]
top_three

# Extract the names of the top three performers
top_three_students <- sorted_students[1:3]
top_three_students

# View top three performers with their scores
# Creating a data frame of top three students and their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
top_three_df <- data.frame(Student = top_three_students, Score = top_three)
top_three_df

# Extract all scores less than or equal to 89
low_scores <- scores[scores <= 89]
low_scores

# Extract the names of students with scores less than or equal to 89
low_score_students <- students[scores <= 89]
low_score_students

# View low scoring students with their scores
# Creating a data frame of low scoring students and their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
low_scores_df <- data.frame(Student = low_score_students, Score = low_scores)
low_scores_df

# Determine the highest and lowest scores
# Using max() and min() functions
# @param {vector} (x) - vector
# @return {numeric} - maximum or minimum value in the vector
highest_score <- max(scores)
lowest_score <- min(scores)
highest_score
lowest_score

# Determine the names of students with the highest and lowest scores
# Using which.max() and which.min() functions
# - Determines the location, i.e., index of the (first) minimum or
# - maximum of a numeric (or logical) vector.
# @param {vector} (x) - vector
# @return {integer} - index of the maximum or minimum value in the vector
highest_scorer <- students[which.max(scores)]
lowest_scorer <- students[which.min(scores)]
highest_scorer
lowest_scorer

# View highest and lowest scorers with their scores
# Creating data frames of highest and lowest scorers with their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
highest_scorer_df <- data.frame(Student = highest_scorer, Score = highest_score)
lowest_scorer_df <- data.frame(Student = lowest_scorer, Score = lowest_score)
highest_scorer_df
lowest_scorer_df

# ------ 3. Temperature Readings ----------------------------------------------

# 3. Create a vector called temp containing 10 temperature readings (°C) of a
#    reaction mixture by different students during an experiment: 22.4, 24.8,
#    27.3, NA, 33.5, 36.9, 39.6, 42.0, 43.8, 45.1. Summarize these data using
#    the summary() function. Then, create a sequence of the temperature
#    readings from lowest to highest and highest to lowest temperatures and
#    assign the new vectors as temp1 and temp2, respectively. Finally,
#    calculate the mean, sd, variance, and length without the missing value.

# Temperature readings (°C) with a missing value (NA)
temp <- c(22.4, 24.8, 27.3, NA, 33.5, 36.9, 39.6, 42.0, 43.8, 45.1)

# Summarize the temperature readings
# Using summary() function
# @param {vector} (x) - vector
# @return {summary} - summary of the vector
# Note: summary() function will show NAs in the summary
temp_summary <- summary(temp)
temp_summary

# Create a sequence of temperature readings from lowest to highest
# Using sort() function
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @param {logical} (na.last) - if TRUE, puts NA values at the end
# @return {vector} - sorted vector
temp1 <- sort(temp, na.last = TRUE)
temp1

# Without na.last argument (default is TRUE)
temp1_alt <- sort(temp)
temp1_alt

# Create a sequence of temperature readings from highest to lowest
# Using sort() function with decreasing argument set to TRUE
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @param {logical} (na.last) - if TRUE, puts NA values at the end
# @return {vector} - sorted vector
temp2 <- sort(temp, decreasing = TRUE, na.last = TRUE)
temp2

# Without na.last argument (default is TRUE)
temp2_alt <- sort(temp, decreasing = TRUE)
temp2_alt

# Calculate the mean without the missing value
# Using mean() function with na.rm argument set to TRUE
# @param {numeric} (x) - numeric vector
# @param {logical} (na.rm) - if TRUE, removes NA values before calculation
# @return {numeric} - mean of the numeric vector
mean_temp <- mean(temp, na.rm = TRUE)
mean_temp

# Calculate mean with missing value
mean_temp_na <- mean(temp)
mean_temp_na # This will return NA

# Calculate the standard deviation without the missing value
# Using sd() function with na.rm argument set to TRUE
# @param {numeric} (x) - numeric vector
# @param {logical} (na.rm) - if TRUE, removes NA values before calculation
# @return {numeric} - standard deviation of the numeric vector
sd_temp <- sd(temp, na.rm = TRUE)
sd_temp

# Calculate standard deviation with missing value
sd_temp_na <- sd(temp)
sd_temp_na # This will return NA

# Calculate the variance without the missing value
var_temp <- var(temp, na.rm = TRUE)
var_temp

# Calculate variance with missing value
var_temp_na <- var(temp)
var_temp_na # This will return NA

# Calculate the length without the missing value
length_temp <- length(temp[!is.na(temp)])
length_temp
length_temp_alt <- sum(!is.na(temp)) # Alternative method
length_temp_alt

# Calculate length with missing value
length_temp_na <- length(temp)
length_temp_na # This will return 10, including NA

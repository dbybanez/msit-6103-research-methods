# MSIT 6103 Research Methods
# Activity 1 - Introduction to R

# By: David Ybanez (MSIT 2)

# Contents include:
# -- A. R as a Calculator: Performing Basic Mathematical Operations
# -- B. Creating OBJECTS in R
# -- C. Functions in R
# -- D. Elements in R
# -- E. Activity 1

# ------ A. R as a Calculator: Performing Basic Mathematical Operations -------

# Addition
14 + 94

# Subtraction
2025 - 1995

# Multiplication
8 * 27

# Division
99 / 8

# Exponentiation
6^2

# Integer division
# Divides and returns the integer part of the quotient
5 %/% 2

# Absolute value
# Returns the absolute value of a number
abs(-8)

# Square root
# Returns the square root of a number
sqrt(18)

# Natural logarithm
# Returns the natural logarithm (base e) of a number
log(16)

# ------ B. Creating OBJECTS in R ---------------------------------------------

# Assigning values to objects
my.class <- 15
my.class2 <- 12
my.class3 <- my.class + my.class2

# Invalid object addition (trying to add numeric and character)
my.class <- 15
my.class2 <- "group1"
# my.class3 <- my.class + my.class2 # This will return an error

# ------ C. Functions in R ----------------------------------------------------

### Concatinate function c() ###

# Using basic functions
# Create a vector of numeric values
# @param {numeric} (x1, x2, x3, ...) - numeric values
# @return {numeric} - numeric vector
my_class <- c(15, 18, 16, 16, 10, 12, 20)

# Mean
# @param {numeric} (x) - numeric vector
# @return {numeric} - mean of the numeric vector
mean(my_class)

# Variance
# @param {numeric} (x) - numeric vector
# @return {numeric} - variance of the numeric vector
var(my_class)

# Standard Deviation
# @param {numeric} (x) - numeric vector
# @return {numeric} - standard deviation of the numeric vector
sd(my_class)

# Length
# @param {vector} (x) - vector
# @return {integer} - length of the vector
length(my_class)

# Summary
# @param {vector} (x) - vector
# @return {summary} - summary of the vector
summary(my_class)

### Creating sequence using : symbol and seq and rep functions ###

# Regular sequence using : symbol
class_seq <- 1:10

# Sequence in descending order
class_seq2 <- 10:1

# Using req function
# @param {numeric} (from) - starting value of the sequence
# @param {numeric} (to)   - end value of the sequence
# @param {numeric} (by)   - increment value of the sequence
class_seq3 <- seq(1, 20, by = 2)

# Using rep function
# @param {numeric} (x) - value to be repeated
# @param {integer} (times) - number of times to repeat the value
class_rep <- rep(2, times = 10)

# Rep function with non-numeric values
class_rep2 <- rep("usc", times = 5)

# ------ D. Elements in R -----------------------------------------------------

### Positional Index ###

# F1 speeds of 10 drivers in km/h
f1_speed <- c(320, 305, 290, 315, 298, 310, 325, 285, 330, 295)

# Extract the 5th element
# Using indexing with square brackets []
# @param {vector} (x) - vector
# @param {integer} (i) - index of the element to extract
# @return {numeric} - extracted element
f1_speed[5]

# Assign to another object
f1_5th <- f1_speed[5]

# Extract the 1st, 3rd, 5th, 7th, and 10th elements
f1_speed[c(1, 3, 5, 7, 10)]

# Extract the speed of 5th to 8th drivers
f1_speed[5:8]

### Logical Index ###

# Extract all elements with a value greater than 300
f1_speed[f1_speed > 300]

# Evaluate vector with logical condition
f1_speed > 300

# Extract elements that are less than 300
f1_speed[f1_speed < 300]

# Extract elements that are greater or equal to 290
f1_speed[f1_speed >= 290]

# Extract elements thet are less than or equal to 320
f1_speed[f1_speed <= 320]

# Extract elements that are not equal to 290, 310, 330
f1_speed[!(f1_speed %in% c(290, 310, 330))]

# Extract all elements with a value greater than 290 AND less than 330
f1_speed[f1_speed > 290 & f1_speed < 330]

# Extract all elements with a value greater than 290 OR less than 330
f1_speed[f1_speed > 290 | f1_speed < 330]

### Replacing Elements in a Vector ###

# Replace the 2nd element with 295
f1_speed[2] <- 295

# Replace the 6th and 10th elements with 250
f1_speed[c(2, 6)] <- 250

### Ordering Elements in a Vector ###

# Sort all elements from lowest to highest
# Using sort() function
# @param {vector} (x) - vector
# @return {vector} - sorted vector
f1_speed_sort <- sort(f1_speed)

# Sort from highest to lowest
# Using sort() function with decreasing argument set to TRUE
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @return {vector} - sorted vector
f1_speed_sort2 <- sort(f1_speed, decreasing = TRUE)

# Sort from highest to lowest using reverse function
# Using rev() function to reverse the sorted vector
# @param {vector} (x) - vector
# @return {vector} - reversed vector
f1_speed_sort3 <- rev(sort(f1_speed))

# Sort 1 vector according to the values of another vector
speed <- c(330, 325, 320, 315, 310)
team <- c("ferrari", "mercedes", "mclaren", "alpine", "haas")

# Order the speed vector in ascending order
# Using order() function
# @param {vector} (x) - vector
# @return {vector} - indices that would sort the vector
speed_ord <- order(speed)
team_speed <- team[speed_ord]

# Simplified version
team_speed_alt <- team[order(speed)]

### Dealing with Missing Data ###

# Create a vector with missing values (NA)
# Relative humidity (%) data with some missing values
rh <- c(85, 80, NA, 78, 90, 88, NA, 92, 87, 84)

# Check for missing values
# Using is.na() function
# @param {vector} (x) - vector
# @return {logical} - logical vector indicating missing values
is.na(rh)

# Count the number of missing values
# Using sum() function to count TRUE values in the logical vector
# @param {vector} (x) - vector
# @return {integer} - count of missing values
sum(is.na(rh))

# Extract and remove non-missing values
rh_nomiss <- rh[!is.na(rh)]

# Calculate the mean of the vector with missing values removed
mean_rh <- mean(rh, na.rm = TRUE)

# ------ E. Activity 1 --------------------------------------------------------

# 1.A. Calculate the square root of 25 multiplied by 0.64.
# 1.B. Determine the natural logarithm of 20 multiplied by 0.5.
# 1.C. Calculate the exponential of 4 multiplied by 6.
# 1.D. Calculate the absolute value of -15 plus the square root of 9.

# 1.A. Square root of 25 multiplied by 0.64
sqrt_val <- sqrt(25) * 0.64

# 1.B. Natural logarithm of 20 multiplied by 0.5
ln_val <- log(20) * 0.5

# 1.C. Exponential of 4 multiplied by 6
exp_val <- exp(4) * 6

# 1.D. Absolute value of -15 plus the square root of 9
abs_val <- abs(-15 + sqrt(9))

# Print results
sqrt_val
ln_val
exp_val
abs_val

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

# Mean
# Using mean() function
# @param {numeric} (x) - numeric vector
# @return {numeric} - mean of the numeric vector
mean_scores <- mean(scores)

# Variance
# Using var() function
# @param {numeric} (x) - numeric vector
# @return {numeric} - variance of the numeric vector
var_scores <- var(scores)

# Standard Deviation
# Using sd() function
# @param {numeric} (x) - numeric vector
# @return {numeric} - standard deviation of the numeric vector
sd_scores <- sd(scores)

# Repeat the first five scores three times
# Using rep() function
# @param {vector} (x) - vector of values to be repeated
# @param {integer} (times) - number of times to repeat the values
# @return {vector} - vector with repeated values
repeated_scores <- rep(scores[1:5], times = 3)

# Arrange scores in descending order
# Using sort() function with decreasing argument set to TRUE
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @return {vector} - sorted vector
sorted_scores <- sort(scores, decreasing = TRUE)

# Arrange students according to sorted scores
# Using order() function
# @param {vector} (x) - vector
# @param {logical} (decreasing) - if TRUE, sorts in decreasing order
# @return {vector} - indices that would sort the vector
sorted_students <- students[order(scores, decreasing = TRUE)]

# View sorted students with their scores
# Creating a data frame of sorted students and their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
sorted_genetics_df <- data.frame(Student = sorted_students, Score = sorted_scores)

# Extract the scores of the top three performers
top_three <- sorted_scores[1:3]

# Extract the names of the top three performers
top_three_students <- sorted_students[1:3]

# View top three performers with their scores
# Creating a data frame of top three students and their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
top_three_df <- data.frame(Student = top_three_students, Score = top_three)

# Extract all scores less than or equal to 89
low_scores <- scores[scores <= 89]

# Extract the names of students with scores less than or equal to 89
low_score_students <- students[scores <= 89]

# View low scoring students with their scores
# Creating a data frame of low scoring students and their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
low_scores_df <- data.frame(Student = low_score_students, Score = low_scores)

# Determine the highest and lowest scores
# Using max() and min() functions
# @param {vector} (x) - vector
# @return {numeric} - maximum or minimum value in the vector
highest_score <- max(scores)
lowest_score <- min(scores)

# Determine the names of students with the highest and lowest scores
# Using which.max() and which.min() functions
# - Determines the location, i.e., index of the (first) minimum or
# - maximum of a numeric (or logical) vector.
# @param {vector} (x) - vector
# @return {integer} - index of the maximum or minimum value in the vector
highest_scorer <- students[which.max(scores)]
lowest_scorer <- students[which.min(scores)]

# View highest and lowest scorers with their scores
# Creating data frames of highest and lowest scorers with their scores
# Using data.frame() function
# @param {vector} (x1, x2, ...) - vectors to be combined
# @return {data.frame} - data frame with combined vectors
# Note: This step is optional and just for better visualization
highest_scorer_df <- data.frame(Student = highest_scorer, Score = highest_score)
lowest_scorer_df <- data.frame(Student = lowest_scorer, Score = lowest_score)

# Print results
mean_scores
var_scores
sd_scores
repeated_scores
sorted_genetics_df
top_three_df
low_scores_df
highest_scorer_df
lowest_scorer_df

# 3. Create a vector called temp containing 10 temperature readings (°C) of a
#    reaction mixture by different students during an experiment: 22.4, 24.8,
#    27.3, NA, 33.5, 36.9, 39.6, 42.0, 43.8, 45.1. Summarize these data using
#    the summary() function. Then, create a sequence of the temperature
#    readings from lowest to highest and highest to lowest temperatures and
#    assign the new vectors as temp1 and temp2, respectively. Finally,
#    calculate the mean, sd, variance, and length without the missing value.

# Temperature readings (°C) with a missing value (NA)
temp <- c(22.4, 24.8, 27.3, NA, 33.5, 36.9, 39.6, 42.0, 43.8, 45.1)

# MSIT 6103 Research Methods
# Introduction to R

# By: David Ybanez (MSIT 2)

# Contents include:
# -- A. R as a Calculator: Performing Basic Mathematical Operations
# -- B. Creating OBJECTS in R
# -- C. Functions in R
# -- D. Elements in R

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
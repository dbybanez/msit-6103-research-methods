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
5 %/% 2

# Absolute value
abs(-8)

# Square root
sqrt(18)

# Natural logarithm
log(16)

# ------ B. Creating OBJECTS in R ---------------------------------------------

# Assigning values to objects
my.class <- 15
my.class2 <- 12
my.class3 <- my.class + my.class2

# Invalid object addition (trying to add numeric and character)
my.class <- 15
my.class2 <- "group1"
my.class3 <- my.class + my.class2

# ------ C. Functions in R ----------------------------------------------------

# Using basic functions
my_class <- c(15, 18, 16, 16, 10, 12, 20)

# Mean
mean(my_class)

# Variance
var(my_class)

# Standard Deviation
sd(my_class)

# Length
length(my_class)

# Summary
summary(my_class)

# ------ D. Elements in R -----------------------------------------------------

# F1 speeds of 10 drivers in km/h
f1_speed <- c(320, 305, 290, 315, 298, 310, 325, 285, 330, 295)

# Extract the 5th element
f1_speed[5] # 298

# Assign to another object
f1_5th <- f1_speed[5]

# Extract the 1st, 3rd, 5th, 7th, and 10th elements
f1_speed[c(1, 3, 5, 7, 10)]

# Extract the speed of 5th to 8th drivers
f1_speed[5:8]

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

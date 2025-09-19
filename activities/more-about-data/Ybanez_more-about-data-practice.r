# Quantitative Research Activty 3 Practice Time
# Learning how to manipulate data
# Written by David Ybanez September 13, 2025 University of San Carlos

# MSIT 6103 Research Methods
# More About Data (Manipulation)

# By: David Ybanez (MSIT 2)

# Contents include:
# -- I. Initial setup for working directory
# -- A. Practice: Data manipulation

# ------ I. Initial setup for working directory -------------------------------

# Install and load the this.path package to manage file paths

# this.path package
# The this.path package provides functions to get the path of the currently
# executing script. This is useful for setting working directories and managing
# file paths in a way that is relative to the script's location, making scripts
# more portable and easier to share.

# Install the this.path package if not already installed
if (!requireNamespace("this.path", quietly = TRUE)) {
  install.packages("this.path", repos = "https://cloud.r-project.org")
}

# Load the this.path package
library(this.path)

# Set the root directory to the location of the current script
# This helps in managing file paths for data import/export
script_root <- tryCatch(this.path::this.dir(), error = function(e) normalizePath(getwd()))

# Function to create file paths relative to the script root
p <- function(...) file.path(script_root, ...)

# Set working directory to script root
setwd(script_root)

# Set output directory and create it
out_dir <- "output"
plot_dir <- file.path(out_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# Validate the paths
# Check if the data directory and specific data file exist
# Uncomment the lines below to see the path validations
# cat("Script root:   ", script_root, "\n")
# cat("Data dir:      ", p("data"), "  exists:", dir.exists(p("data")), "\n")
# cat("Data file:     ", p("data","edidiv.csv"), "  exists:", file.exists(p("data","edidiv.csv")), "\n")
# cat("Output dir:    ", p("output"), "\n")

# ------ A. Practice: Data manipulation ---------------------------------------

# Install and load dplyr for data manipulation
install.packages("dplyr")
library(dplyr)

# Load the animal datasets
# animal_p1 contains octupuses and fish data
animal_p1 <- read.csv(p("data", "animal_p1.csv"))

# animal_p2 contains turtles only
animal_p2 <- read.csv(p("data", "animal_p2.csv"))

# animal_rp contains data from research partner
animal_rp <- read.csv(p("data", "animal_rp.csv"))

# animal_meal contains data on meal types for each animal
animal_meal <- read.csv(p("data", "animal_meal.csv"))

# Objective: To analyze data collected from a coral reef at Palmyra Atoll, 
# where 10 animals representing four different groups were identified. The 
# study aims to integrate multiple data tables to calculate and present the 
# average weight and meal type for each animal group.

# View the datasets
animal_p1
animal_p2

# Combine the two parts of the animal dataset using bind_rows()
# Using bind_rows() to combine two data frames by rows
# @param {data1} The first data frame to combine
# @param {data2} The second data frame to combine
# @return {data.frame} - combined data frame with rows from both data frames
# Wrapping the output in parentheses to display it immediately
(animal <- bind_rows(animal_p1, animal_p2))

# Check if the two data frames have the same columns
setequal(animal_p1, animal_p2) # FALSE means they have different columns

# Check for common columns between animal and animal_rp
intersect(animal, animal_rp) # 6 observations in common

# Check for columns in animal that are not in animal_rp
setdiff(animal, animal_rp) # ids no 2 and 5 are not in animal_rp

# Check for columns in animal_rp that are not in animal
setdiff(animal_rp, animal) # ids no 6 and 10 are not in animal

# Combine animal and animal_rp using union() to get unique rows from both
# Using union() to combine two data frames and retain unique rows
# @param {data1} The first data frame to combine
# @param {data2} The second data frame to combine
# @return {data.frame} - combined data frame with unique rows from both data frames
# Wrapping the output in parentheses to display it immediately
# %>% arrange(id) to sort the combined data frame by id
(animal_weight <- union(animal, animal_rp) %>% arrange(id))

# Find common rows between animal and animal_rp
# Using intersect() to find common rows between two data frames
# @param {data1} The first data frame to compare
# @param {data2} The second data frame to compare
# @return {data.frame} - data frame with rows common to both data frames
# Wrapping the output in parentheses to display it immediately
(animal_common <- intersect(animal, animal_rp))

# Note: If a data does not have a key column to join on and the rows are in the same order,
# you can use bind_cols() to combine them side by side. This is less common but useful in some cases.
# Combine animal and animal_rp using bind_cols() to combine side by side
# Using bind_cols() to combine two data frames by columns
# @param {data1} The first data frame to combine
# @param {data2} The second data frame to combine
# @return {data.frame} - combined data frame with columns from both data frames
# Wrapping the output in parentheses to display it immediately
(animal_side_by_side <- bind_cols(animal, animal_rp))
# Note: This creates duplicate id columns since both data frames have an id column
# This method should be used with caution and only when you are sure the rows align correctly

# Mutating joins using left_join(), right_join(), inner_join(), and full_join()
# These functions are used to merge two data frames based on a common key column.
# left_join() keeps all rows from the left data frame and adds matching rows from the right
# right_join() keeps all rows from the right data frame and adds matching rows from the left
# inner_join() keeps only rows with keys that are present in both data frames
# full_join() keeps all rows from both data frames, filling in NA for missing matches

# Combine animal_weight with animal_meal which contains meal type information
# Since animal_weight contains an id and animal_meal contains IDs,
# we can use left_join() to add meal type information to animal_weight
# Using left_join() to merge two data frames based on a common column
# @param {x} The main data frame (animal_weight)
# @param {y} The data frame to join (animal_meal)
# @param {by} The column(s) to join by (common column in both data frames)
# @return {data.frame} - merged data frame with meal type information added
(animal_joined <- left_join(animal_weight, animal_meal, by = c("id" = "IDs")))

# Alternatively, using the pipe operator %>%
(animal_joined <- animal_weight %>% left_join(animal_meal, by = c("id" = "IDs")))

# Only keeps rows with matching ids
inner_join(animal_weight, animal_meal, by = c("id" = "IDs")) 

# Keeps all rows from animal_meal, adding matching rows from animal_weight
right_join(animal_weight, animal_meal, by = c("id" = "IDs"))

# Keeps all rows from both data frames, filling in NA for missing matches
full_join(animal_weight, animal_meal, by = c("id" = "IDs"))

# Filtering joins using semi_join() and anti_join()
# semi_join() keeps rows in the first data frame that have matching keys in the second
# anti_join() keeps rows in the first data frame that do not have matching keys in the second

# Find animals in animal_weight that have meal type information in animal_meal
# Using semi_join() to filter rows in the first data frame based on matches in the second
# @param {x} The main data frame (animal_weight)
# @param {y} The data frame to check for matches (animal_meal)
# @param {by} The column(s) to join by (common column in both data frames)
# @return {data.frame} - filtered data frame with rows that have matches in the second data frame
semi_join(animal_weight, animal_meal, by = c("id" = "IDs"))

# Find animals in animal_weight that do not have meal type information in animal_meal
# Using anti_join() to filter rows in the first data frame based on non-matches in the second
# @param {x} The main data frame (animal_weight)
# @param {y} The data frame to check for non-matches (animal_meal)
# @param {by} The column(s) to join by (common column in both data frames)
# @return {data.frame} - filtered data frame with rows that do not have matches in the second data frame
anti_join(animal_weight, animal_meal, by = c("id" = "IDs"))


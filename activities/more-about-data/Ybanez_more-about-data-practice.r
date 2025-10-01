# Quantitative Research Activity 3 Practice Time 1 and 2
# Learning how to manipulate data
# Written by David Ybanez September 13, 2025 University of San Carlos

# MSIT 6103 Research Methods
# More About Data (Manipulation)

# By: David Ybanez (MSIT 2)

# Contents include:
# -- I. Initial setup for working directory
# -- A. Practice 1: Data manipulation


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

# More work!

animal_new <- read.csv(p("data", "animal_new.csv"))
str(animal_new)

# Combine animal_joined with animal_new using full_join() to keep all rows from both
# Using full_join() to merge two data frames based on a common column
# @param {x} The main data frame (animal_joined)
# @param {y} The data frame to join (animal_new)
# @param {by} The column(s) to join by (common column in both data frames)
# @return {data.frame} - merged data frame with all rows from both data frames
animal_final <- animal_joined %>% full_join(animal_new, by = c("id" = "ID", "animal" = "Animals", "weight", "meal" = "Meal"))

library(ggplot2)
library(gridExtra)

# Create a bar plot to visualize the diversity of meals for each animal
# Using ggplot2 to create a bar plot
# @param {data} The data frame to plot (animal_final)
# @param {aes} The aesthetic mappings (animal on x-axis, fill by meal type)
# @return {ggplot} - bar plot visualizing meal diversity
barplot_diet <- ggplot(animal_final, aes(animal, fill = meal)) +
  geom_bar(alpha = 0.8) +
  labs(title = "Diversity of meals", x = NULL) +
  scale_fill_brewer(palette = "Set3", type = "seq", na.value = "grey") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"))

barplot_diet

# Create a box plot to visualize the weight distribution for each animal
boxplot_weight <- ggplot(animal_final) +
  geom_boxplot(aes(animal, weight, fill = animal), alpha = 0.5, position = "dodge2") +
  scale_y_continuous(limits = c(0, 30)) +
  labs(title = "Mean weights of animals", x = NULL, y = "Weight (kg)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm"))

boxplot_weight

# Arrange the two plots side by side using grid.arrange() from gridExtra
# Using grid.arrange() to arrange multiple ggplot objects in a grid layout
# @param {...} The ggplot objects to arrange
# @param {ncol} The number of columns in the layout
# @return {gtable} - arranged grid of plots
animal_panel <- grid.arrange(barplot_diet, boxplot_weight, ncol = 2)

# Save the arranged plot to a file using ggsave()
# Using ggsave() to save a ggplot object to a file
# @param {filename} The file path to save the plot
# @param {plot} The ggplot object to save
ggsave(filename = p(plot_dir, "animal_panel.png"), plot = animal_panel, width = 10, height = 5, units = "in", dpi = 300)

# Practice Time 2

# import tidyr for data reshaping
install.packages("tidyr")
library(tidyr)

# import readr for reading csv files
install.packages("readr")
library(readr)

# import LPI_marine.csv dataset
marine <- read_csv(p("data", "LPI_marine.csv"))

# tidy the data using one line (vs code works weird)
# marine2 <- marine %>% gather(key = year, value = pop, c(25:69)) %>% mutate(year = parse_number(as.character(year)), pop = as.numeric(pop)) %>% drop_na(pop)

# Proper formatting to make the block work
# Using gather() to reshape data from wide to long format
# Using mutate() to transform columns (extract year number and convert pop to numeric)
# Using drop_na() to remove rows with NA in the pop column
marine2 <-
  marine %>%
  gather(key = year, value = pop, c(25:69)) %>%
  mutate(year = parse_number(as.character(year)), pop = as.numeric(pop)) %>%
  drop_na(pop)

# Using glimpse() to get a quick overview of the data structure
# @param {data} The data frame to inspect (marine2)
# @return {NULL} - prints the structure of the data frame to the console
glimpse(marine2)

# View the rows of the tidy dataset
View(marine2)

# Access specific columns using pull() and glimpse()
marine2 %>% pull(Species) %>% glimpse()

# Another way to access specific columns using select() and glimpse()
marine2 %>% select(Species) %>% glimpse()

# Access multiple columns using select() and glimpse()
# Using select() to choose specific columns from the data frame
# @param {data} The data frame to select from (marine2)
# @param {...} The columns to select (id, pop, year, Country.list)
# @return {data.frame} - data frame with only the selected columns
marine2 %>% select(id, pop, year, Country.list) %>% glimpse()

# Rename columns while selecting them using select() and glimpse()
# @return {data.frame} - data frame with only the selected and renamed columns
marine2 %>% select("Country list" = Country.list, method = Sampling.method) %>% glimpse()

# Reorder columns using select() and glimpse()
# everything() is a helper function that selects all remaining columns
# id, year, and pop are moved to the front
marine2 %>% select(id, year, pop, everything()) %>% glimpse()

# Select a range of columns using select() and glimpse()
# Using the colon operator (:) to specify a range of columns from Family to Species
marine2 %>% select(Family:Species, 24:26) %>% glimpse()

# Exclude specific columns using select() and glimpse()
# Using the minus sign (-) to exclude columns 2 to 22 and column 24
marine2 %>% select(-c(2:22, 24)) %>% glimpse()

# Create a vector of important columns for marine data analysis
marine_cols <- c("Genus", "Species", "year", "pop", "id")

# Use the vector to select columns from marine2 and glimpse the result
# Using the !! operator to unquote the vector of column names for selection
marine2 %>% select(!!marine_cols) %>% glimpse()

# More select() examples
# starts_with("x") matches names starting with "x"
# ends_with("x") matches names ending with "x"
# contains("x") matches names containing "x"

# Select columns starting with "Decimal"
marine2 %>% select(starts_with("Decimal")) %>% glimpse()

# select columns based on their data type using select_if().
# The common data types to be called are: is.character, is.double, is.factor, is.integer, is.logical, is.numeric.
marine2 %>% select_if(is.numeric) %>% glimpse()

# Mix various ways to call columns using select()
# id column, columns from Class to Family, rename Genus to genus,
# columns starting with "Decimal", all remaining columns,
# and exclude columns 6 to 9 and system to Data.transformed
# Colon (:) operator to select a range of columns
marine2 %>% select(id, Class:Family, genus = Genus, starts_with("Decimal"), everything(), -c(6:9, system:Data.transformed)) %>% glimpse()

# New object marine3 with selected and renamed columns
marine3 <- marine2 %>% select(id, Class, Genus, Species, year, pop, location = Location.of.population, lat = Decimal.Latitude, lon = Decimal.Longitude) %>% glimpse()

# Rename multiple columns using rename() and glimpse()
# Using rename() to change column names for clarity
# @param {data} The data frame with columns to be renamed (marine3)
# @param {new_name = old_name} Pairs of new and old column names
# @return {data.frame} - data frame with renamed columns
marine3 %>% rename(class = Class, genus = Genus, species = Species) %>% glimpse()

# Rename all columns to lowercase using rename_with() and glimpse()
# Using rename_with() to apply a function to all column names
# @param {data} The data frame with columns to be renamed (marine3)
# @param {.fn} The function to apply to column names (tolower)
# @return {data.frame} - data frame with all column names converted to lowercase
marine3 %>% rename_with(tolower) %>% glimpse()

# Alternatively, using select_all() to apply a function to all column names
# Using select_all() to apply a function to all column names
# @param {data} The data frame with columns to be renamed (marine3)
# @param {.funs} The function to apply to column names (tolower)
# @return {data.frame} - data frame with all column names converted to lowercase
marine4 <- marine3 %>% select_all(tolower) %>% glimpse()

# Alternatively, using select_at() to apply a function to specific columns
# Using select_at() to apply a function to specific column names
# @param {data} The data frame with columns to be renamed (marine3)
# @param {vars} The columns to apply the function to (Genus and Species)
# @param {.funs} The function to apply to the selected column names (tolower)
# @return {data.frame} - data frame with selected column names converted to lowercase
marine3 %>% select_at(vars(Genus, Species), tolower) %>% glimpse()

# Create a new column by combining genus and species using mutate() and glimpse()
# Using mutate() to create a new column based on calculations from existing columns
# @param {data} The data frame to add a new column to (marine4)
# @param {new_column = expression} The name of the new column and the expression to calculate its values
# @return {data.frame} - data frame with the new column added
marine5 <- marine4 %>% mutate(genus_species = paste(genus, species, sep = "_")) %>% glimpse()

# Create a new column for region based on latitude and longitude using case_when() and glimpse()
# Using mutate() to create a new column based on conditions using case_when()
# @param {data} The data frame to add a new column to (marine5)
# @param {new_column = case_when(...)} The name of the new column and the conditions to determine its values
# @return {data.frame} - data frame with the new column added
# Conditions:
# - NE if lat > 0 and lon >= 0
# - SE if lat <= 0 and lon >= 0
# - NW if lat > 0 and lon < 0
# - SW if lat <= 0 and lon < 0
marine6 <- marine5 %>% mutate(region = case_when(lat > 0 & lon >= 0 ~ "NE", lat <= 0 & lon >= 0 ~ "SE", lat > 0 & lon < 0 ~ "NW", lat <= 0 & lon < 0 ~ "SW")) %>% glimpse()
unique(marine6$region)

# Combine multiple operations using the pipe operator %>%
# Create genus_species and region columns in one pipeline and glimpse the result
# Using mutate() to create new columns based on calculations and conditions
# @param {data} The data frame to add new columns to (marine4)
# @param {new_column1 = expression1} The name of the first new column and the expression to calculate its values
# @param {new_column2 = case_when(...)} The name of the second new column and the conditions to determine its values
# @return {data.frame} - data frame with the new columns added
# The combined operations create genus_species and region columns in one step
# transmute() can be used instead of mutate() to keep only the new columns
marine4 %>% transmute(genus_species = paste(genus, species, sep = "_"), region = case_when(lat > 0 & lon >= 0 ~ "NE", lat <= 0 & lon >= 0 ~ "SE", lat > 0 & lon < 0 ~ "NW", lat <= 0 & lon < 0 ~ "SW")) %>% glimpse()

# Convert specific columns to lowercase using mutate_at() and glimpse()
# Using mutate_at() to apply a function to specific columns
# @param {data} The data frame with columns to be modified (marine6)
# @param {vars} The columns to apply the function to (class, genus, location)
# @param {.funs} The function to apply to the selected columns (tolower)
# @return {data.frame} - data frame with the selected columns converted to lowercase
marine6 %>% mutate_at(vars(class, genus, location), tolower) %>% glimpse()

# Add column using add_column() from tibble package
# Using add_column() to add a new column to the data frame
# @param {data} The data frame to add a new column to (marine6)
# @param {new_column = values} The name of the new column and the values to fill it with
# @return {data.frame} - data frame with the new column added
library(tibble)
marine6 %>% add_column(observation_num = 1:4456) %>% glimpse()

# Count number of observations for each genus_species using add_tally() and glimpse()
# Using add_tally() to add a column with the count of observations for each group
# @param {data} The data frame to add the tally column to (marine6)
# @param {name} The name of the new tally column (observations_count)
# @return {data.frame} - data frame with the new tally column added
# Grouping by genus_species to get counts for each unique species
marine6 %>% select(genus_species, year) %>% group_by(genus_species) %>% add_tally(name = "observations_count") %>% glimpse()

# Alternatively, using add_count() to achieve the same result more concisely
# Using add_count() to add a column with the count of observations for each group
# @param {data} The data frame to add the count column to (marine6)
# @param {group_by_columns} The columns to group by (genus_species)
# @param {name} The name of the new count column (observations_count)
# @return {data.frame} - data frame with the new count column added
# This method combines grouping and counting in one step
marine6 %>% select(genus_species, year) %>% add_count(genus_species, name = "observations_count") %>% glimpse()

# Filter rows for class Mammalia using filter() and glimpse()
# Using filter() to subset the data based on conditions
# @param {data} The data frame to be filtered (marine6)
# @param {condition} Logical condition to filter the rows (class == "Mammalia")
# @return {data.frame} - filtered data frame with only rows where class is "Mammalia"
marine6 %>% filter(class == "Mammalia") %>% glimpse()

# Operators for filtering
# >         greater than
# >=        greater than or equal to
# <         less than
# <=        less than or equal to
# ==        equal to
# !=        not equal to
# |         OR (a | b)
# xor()     exclusive OR (only a or b, not both) xor(a, b)
# &         AND (a & b)
# is.na()   is NA (missing value)
# !         NOT (!a)
# !is.na()  is not NA (!is.na(a))
# %in%      in a vector of values

# Using %in% operator to filter for multiple classes
marine6 %>% filter(class %in% c("Mammalia", "Aves")) %>% glimpse()

# Using | operator to filter for multiple classes (same result as above)
marine6 %>% filter(class == "Mammalia" | class == "Aves") %>% glimpse()

# Omit a specific class using != operator
marine6 %>% filter(class != "Actinopteri") %>% glimpse()

# Filter for a range of population values using & operator
# Example: Filter for populations between 10 and 100 (inclusive)
marine6 %>% filter(pop >= 10 & pop <= 100) %>% glimpse()

# Omit rows with NA in the pop column using !is.na()
marine6 %>% filter(!is.na(pop)) %>% glimpse()

# Combine multiple conditions using & and | operators
# Example: Filter for class Mammalia or populations greater than 100, and region not equal
# 38 rows returned
marine6 %>% filter((class == "Mammalia" | pop > 100) & region != "SE") %>% glimpse()

# Example: Filter for class Mammalia or (populations greater than 100 and region not equal to SE)
# 96 rows returned
marine6 %>% filter(class == "Mammalia" | (pop > 100 & region != "SE")) %>% glimpse()

# Find distinct values in specific columns using distinct() and glimpse()
# Using distinct() to get unique rows based on specific columns
# @param {data} The data frame to find distinct values in (marine6)
# @param {...} The columns to consider for uniqueness (class, genus, species, etc)
# @return {data.frame} - data frame with distinct rows based on the specified columns
marine6 %>% distinct() %>% glimpse()

# Count number of distinct values in specific columns using n_distinct()
# Using n_distinct() to count unique values in a specific column
# @return {integer} - number of unique values in the specified column
marine6 %>% n_distinct()

# Select which rows to keep using slice()
# Using slice() to select rows by their position
# @param {data} The data frame to slice (marine6)
# @param {...} The row indices to keep (2 to 4)
# @return {data.frame} - data frame with only the selected rows
marine6 %>% select(id:species) %>% slice(2:4)

# Find top n values in a column using top_n() and glimpse()
# Using top_n() to select the top n rows based on a specific column
# @param {data} The data frame to select from (marine6)
# @param {n} The number of top rows to select (5)
# @param {wt} The column to use for ranking (pop)
# @return {data.frame} - data frame with the top n rows based on the specified column
marine6 %>% top_n(5, pop) %>% glimpse() #5 highest population values glimpse()

# Lastly, if you would like to manually add an observation, you can use add_row().
# Although it is actually a function from tibble package, it's also important
# to learn! But first, quick exercise to keep you fresh! Can you take the
# challenge to create marine7 from marine6 which you will need to complete this
# section? Create a new table for Chrysophrys auratus population with the id
# number of 2077. Then, keep only the columns of id, full species name, year
# of measurement and size of the population.
marine7 <- marine6 %>% filter(id == "2077") %>% select(id, genus_species, year, pop)

# Chain multiple operations using the pipe operator %>%
# Example: Filter for id 2077, then select specific columns and glimpse the result
# Using filter() to subset the data, select() to choose specific columns, and glimpse() to inspect the result
# @param {data} The data frame to operate on (marine6)
# @param {condition} Logical condition to filter the rows (id == "2077")
# @param {...} The columns to select (id, genus_species, year, pop)
# @return {data.frame} - filtered and selected data frame
(marine7 <- marine6 %>% filter(id == "2077") %>% select(id, genus_species, year, pop))

# Add a new row to marine7 using add_row()
# Using add_row() to append a new row to the data frame
# @param {data} The data frame to add a new row to (marine7)
# @param {new_row} The values for the new row (id, genus_species, year, pop)
# @return {data.frame} - data frame with the new row added
# Adding a new observation for Chrysophrys auratus with id 2077, year 1997, and population 39000
(marine7 %>% add_row(id = 2077, genus_species = "Chrysophrys_auratus", year = 1997, pop = 39000))

# Add a new row at the beginning using .before parameter in add_row()
# Using add_row() with .before parameter to insert a new row at a specific position
# @param {data} The data frame to add a new row to (marine7)
# @param {new_row} The values for the new row (id, genus_species, year, pop)
# @param {.before} The position to insert the new row before (1 for the first row)
# @return {data.frame} - data frame with the new row added
# Adding a new observation for Chrysophrys auratus with id 2077, year 1969, and population 39000 at the beginning
(marine7 %>% add_row(id = 2077, genus_species = "Chrysophrys_auratus", year = 1969, pop = 39000, .before = 1))

# Green sea turtle population trend

# change `id` to factor (otherwise it would display as a continuous variable on the plot)
marine_final <- marine6 %>% filter(genus_species == "Chelonia_mydas") %>% mutate(id = as.factor(id))

library(ggplot2)

# ggplot parts
# ggplot(data = <DATA>, mapping = aes(<MAPPINGS>)) + <GEOM_FUNCTION>() + <OTHER_FUNCTIONS>()
# data = the data frame containing the variables to plot
# mapping = aesthetic mappings, defining how data variables are mapped to visual properties (aesthetics) of the plot
# aes() = function to specify aesthetic mappings, such as x and y axes, colors, shapes, sizes, etc.
# geom_point() = a geometric object (geom) that represents data points as points on the plot
# geom_smooth() = a geom that adds a smoothed line to the plot, often used to show trends
# scale_x_continuous() = function to customize the x-axis scale, including limits and breaks
# labs() = function to add labels and titles to the plot
# theme_minimal() = a predefined theme that provides a clean and minimalistic look to the plot
# theme() = function to customize various aspects of the plot's appearance, such as text size, margins, etc.
(chelonia_trends <- ggplot(marine_final, aes(x = year, y = pop, colour = location)) + geom_point(size = 2, alpha = 0.7) + geom_smooth(method = "lm", colour = "black", fill = "lightgrey") + scale_x_continuous(limits = c(1970, 2005), breaks = c(1970, 1980, 1990, 2000)) + labs(x = NULL, y = "Population count\n", title = "Positive trend of Green Sea Turtle population in Australia\n", colour = "Location") + theme_minimal() + theme(plot.title = element_text(size = 14, hjust = 0.5, face = "bold"), plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), units = "cm")))

ggsave(filename = p(plot_dir, "chelonia_trends.png"), plot = chelonia_trends, width = 8, height = 6, units = "in", dpi = 300)


# Quantitative Research Activty 3
# Learning how to manipulate data
# Written by David Ybanez September 13, 2025 University of San Carlos

# MSIT 6103 Research Methods
# More About Data (Manipulation)

# By: David Ybanez (MSIT 2)

# Contents include:
# -- I. Initial setup for working directory
# -- A. Data manipulation: Tidy data

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

# ------ A. Data manipulation: Tidy data --------------------------------------

elongation <- read.csv(p("data", "EmpetrumElongation.csv"), header = TRUE)

# View the first few rows of the dataset
head(elongation)

# Check the structure of the dataset
str(elongation)

# Accessing specific columns
elongation$Indiv

# Count the number of unique individuals (shrubs) in the dataset
length(unique(elongation$Indiv))

# Install and load the tidyr package for data manipulation
install.packages("tidyr")
library(tidyr)

# Reshape the data from wide to long format using gather()
# Using gather() to convert year columns into key-value pairs
# @param {data} The data frame to be reshaped
# @param {key} The name of the new key column (Year)
# @param {value} The name of the new value column (Length)
# @param {cols} The columns to be gathered into key-value pairs
# @return {data.frame} - reshaped data frame in long format
elongation_long <- gather(elongation, key = "Year", value = "Length", c("X2007", "X2008", "X2009", "X2010", "X2011", "X2012"))

# Reshape the data back from long to wide format using spread()
# Using spread() to convert key-value pairs back into wide format
# @param {data} The data frame to be reshaped
# @param {key} The name of the key column (Year)
# @param {value} The name of the value column (Length)
# @return {data.frame} - reshaped data frame in wide format
elongation_wide <- spread(elongation_long, key = "Year", value = "Length")

# Alternatively, using column indices instead of names
# Reshape the data from wide to long format using gather() with column indices
# Here, columns 3 to 8 correspond to the year columns
# @param {data} The data frame to be reshaped
# @param {key} The name of the new key column (Year)
# @param {value} The name of the new value column (Length)
# @param {cols} The columns to be gathered into key-value pairs (using indices)
# @return {data.frame} - reshaped data frame in long format
elongation_long2 <- gather(elongation, key = "Year", value = "Length", c(3:8))

# Easier to analyze data in long format (Inter-annual variation in the growth of hermaphroditum)
boxplot(Length ~ Year, data = elongation_long, xlab = "Year", ylab = "Elongation (cm)", main = "Annual growth of hermaphroditum")

# Install and load the dplyr package for data manipulation
install.packages("dplyr")
library(dplyr)

# Rename columns for easier reference using rename() from dplyr
# Using rename() to change column names for clarity
# @param {data} The data frame with columns to be renamed
# @param {new_name = old_name} Pairs of new and old column names
# @return {data.frame} - data frame with renamed columns
elongation_long <- rename(elongation_long, zone = Zone, indiv = Indiv, year = Year, length = Length)

# Filter data for specific zones and years using filter() from dplyr
# Using filter() to subset the data based on conditions
# @param {data} The data frame to be filtered
# @param {condition} Logical conditions to filter the rows
# @return {data.frame} - filtered data frame
# Example: Filter for zones 2 and 3, and years 2009 to 2011
# %in% operator checks for membership in a vector as an alternative to multiple OR conditions
elong_subset <- filter(elongation_long, zone %in% c(2, 3), year %in% c("X2009", "X2010", "X2011"))

# between() function can also be used for filtering ranges
# Example: Filter for zones between 2 and 3, and years between 2009 and 2011
elong_subset2 <- filter(elongation_long, between(zone, 2, 3), between(as.numeric(sub("X", "", year)), 2009, 2011))

# Remove the zone column using select() from dplyr
# Using select() to choose specific columns from the data frame
# @param {data} The data frame to select columns from
# @param {columns} The columns to be selected (can use column names or indices)
# @return {data.frame} - data frame with selected columns
elong_no.zone <- dplyr::select(elongation_long, indiv, year, length)

# Alternatively, remove the zone column using negative indexing
elong_no.zone2 <- dplyr::select(elongation_long, -zone)

# Rename columns while removing the zone column using select() from dplyr
# Using select() to choose and rename specific columns
# @param {data} The data frame to select and rename columns from
# @param {new_name = old_name} Pairs of new and old column names
# @return {data.frame} - data frame with selected and renamed columns
elong_no.zone3 <- dplyr::select(elongation_long, Year = year, Shrub.ID = indiv, Growth = length)

# Add a new column for total growth across all years using mutate() from dplyr
# Using mutate() to create a new column based on calculations from existing columns
# @param {data} The data frame to add a new column to
# @param {new_column = expression} The name of the new column and the expression to calculate its values
# @return {data.frame} - data frame with the new column added
elong_total <- mutate(elongation, total.growth = X2007 + X2008 + X2009 + X2010 + X2011 + X2012)

# Group data by individual shrubs using group_by() from dplyr
# Using group_by() to create groups based on a specific column
# @param {data} The data frame to be grouped
# @param {columns} The columns to group by
# @return {grouped_data} - grouped data frame
elong_grouped <- group_by(elongation_long, indiv)

# Helpful when combined with summarise() to calculate summary statistics for each group
# Using summarise() to calculate summary statistics for each group
# @param {data} The grouped data frame
# @param {new_column = expression} The name of the new summary column and the expression to calculate its values
# @return {data.frame} - data frame with summary statistics for each group

# The first summary is not grouped, so it gives a single total growth value
summary1 <- summarise(elongation_long, total.growth = sum(length)) # not grouped

# The second summary is grouped by individual shrubs, giving total growth per shrub
summary2 <- summarise(elong_grouped, total.growth = sum(length)) # grouped by indiv

# The third summary provides multiple statistics (total, mean, sd) per shrub
summary3 <- summarise(elong_grouped, total.growth = sum(length), mean.growth = mean(length), sd.growth = sd(length))

# Sometimes, a project has several data files—for example, one with
# measurements, another with climate data, and one with information about the
# experiment. It can be helpful to combine all this data into one table. This
# is called merging or joining datasets. Let’s say our growth data comes from
# an experiment where some plants were warmed (W), some fertilized (F), some
# got both treatments (WF), and some were controls (C). We have a file called
# EmpetrumTreatments.csv that shows which plants got which treatment. We want
# to join this with our main data, elongation_long, using the plant ID that
# appears in both files. There are different ways to join data depending on
# what you want to keep. If you want to keep all the rows from your main data
# and add treatment info, you use left_join(). This is what we’ll do here.

# Import another dataset for practice
# Empetrum treatments data
treatments <- read.csv(p("data", "EmpetrumTreatments.csv"), header = TRUE, sep = ";")

# View the first few rows of the treatments dataset
head(treatments)

# Join the treatments data with the elongation_long data using left_join() from dplyr
# Using left_join() to merge two data frames based on a common column
# @param {x} The main data frame (elongation_long)
# @param {y} The data frame to join (treatments)
# @param {by} The column(s) to join by (common column in both data frames)
# @return {data.frame} - merged data frame with treatment information added
experiment <- left_join(elongation_long, treatments, by = c("indiv" = "Indiv", "zone" = "Zone"))

# Boxplot to visualize growth by treatment
boxplot(length ~ Treatment, data = experiment, xlab = "Treatment", ylab = "Elongation (cm)", main = "Growth by Treatment")

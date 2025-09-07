# MSIT 6103 Research Methods
# Activity 2

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: September 07, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1.  Wingspan of Different Bird Species
# -- 2.  Temperature Data Analysis

# ------ I. Initial setup for working directory and load packages -------------

# Install and load the this.path package to manage file paths

# this.path package
# The this.path package provides functions to get the path of the currently
# executing script. This is useful for setting working directories and managing
# file paths in a way that is relative to the script's location, making scripts
# more portable and easier to share and execute.

# Install the this.path package if not already installed
if (!requireNamespace("this.path", quietly = TRUE)) { install.packages("this.path") }

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
output_dir <- "output"
plot_dir <- file.path(output_dir, "plots")
dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)

# Install and load the dplyr package for data manipulation
if (!requireNamespace("dplyr", quietly = TRUE)) { install.packages("dplyr") }


# Load the dplyr package
library(dplyr)

# ------ 1. Wingspan of Different Bird Species --------------------------------

# 1. Here are values of the wingspan (cm) measured on four different species of 
#    birds. Create a bar plot of the mean wingspan for each species. Add the 
#    standard error bar for each. To calculate for standard error,
#    se=sd/sqrt(n).

bird_sp <- c("sparrow", "kingfisher", "eagle", "hummingbird", "sparrow", "kingfisher", "eagle", "hummingbird", "sparrow", "kingfisher", "eagle", "hummingbird")

wingspan <- c(22, 26, 199, 8, 24, 23, 201, 9, 20, 25, 188, 8)

# Create a data frame
bird_data <- data.frame(Species = bird_sp, Wingspan = wingspan)
bird_data

summary(bird_data)
head(bird_data$Species)
class(bird_data$Species)
bird_data$Species <- as.factor(bird_data$Species)
class(bird_data$Species)

# Using aggregate() function to calculate mean, sd, n, and se
# @param {data.frame} (data) - data frame
# @param {formula} (Wingspan ~ Species) - formula to specify the response and
# predictor variables
# @param {function} (FUN) - function to apply (mean, sd, length)
# @return {data.frame} - data frame with the results

# Calculate mean wingspan for each species
mean_wingspan_agg <- aggregate(Wingspan ~ Species, data = bird_data, FUN = mean)
mean_wingspan_agg

# Calculate standard deviation for each species
sd_wingspan_agg <- aggregate(Wingspan ~ Species, data = bird_data, FUN = sd)
sd_wingspan_agg

# Calculate sample size for each species
n_wingspan_agg <- aggregate(Wingspan ~ Species, data = bird_data, FUN = length)
n_wingspan_agg

# Calculate standard error for each species
se_wingspan_agg <- sd_wingspan_agg$Wingspan / sqrt(n_wingspan_agg$Wingspan)
se_wingspan_agg

## (optional) quick table to see the numbers
round(cbind(mean = mean_wingspan_agg$Wingspan, sd = sd_wingspan_agg$Wingspan, n = n_wingspan_agg$Wingspan, se = se_wingspan_agg), 3)

# Create a bar plot with error bars
bar_centers <- barplot(mean_wingspan_agg$Wingspan, names.arg = mean_wingspan_agg$Species, ylim = c(0, 220), ylab = "Mean Wingspan (cm)", xlab = "Bird Species", main = "Mean Wingspan of Different Bird Species")
arrows(bar_centers, mean_wingspan_agg$Wingspan - se_wingspan_agg, bar_centers, mean_wingspan_agg$Wingspan + se_wingspan_agg, angle = 90, code = 3, length = 0.1, col = "red")
text(bar_centers, mean_wingspan_agg$Wingspan, labels = round(mean_wingspan_agg$Wingspan, 1), pos = 3)

# Using tapply() function to calculate mean, sd, n, and se
# @param {vector} (bird_data$Wingspan) - numeric vector
# @param {factor} (bird_data$Species) - factor vector
# @param {function} (mean, sd, length) - function to apply
# @return {vector} - vector with the results

mean_wingspan_ta <- tapply(bird_data$Wingspan, bird_data$Species, mean)
sd_wingspan_ta   <- tapply(bird_data$Wingspan, bird_data$Species, sd)
n_wingspan_ta    <- tapply(bird_data$Wingspan, bird_data$Species, length)
se_wingspan_ta   <- sd_wingspan_ta / sqrt(n_wingspan_ta)

## (optional) quick table to see the numbers
round(cbind(mean = mean_wingspan_ta, sd = sd_wingspan_ta, n = n_wingspan_ta, se = se_wingspan_ta), 3)

## Plot: bars + standard error bars
op <- par(no.readonly = TRUE)
par(mar = c(6,6,5,3), xaxs = "r", yaxs = "r")  # a little whitespace

ylim <- c(0, max(mean_wingspan_ta + se_wingspan_ta) * 1.1)  # leave headroom for error bars/labels
bp_ta <- barplot(mean_wingspan_ta,
              ylim = ylim,
              ylab = "Mean Wingspan (cm)",
              xlab = "Bird Species",
              main = "Mean Wingspan of Different Bird Species")

# error bars (mean ± SE)
arrows(x0 = bp_ta, y0 = mean_wingspan_ta - se_wingspan_ta, x1 = bp_ta, y1 = mean_wingspan_ta + se_wingspan_ta, angle = 90, code = 3, length = 0.05)

# (optional) show mean values on top of bars
text(bp_ta, mean_wingspan_ta, labels = round(mean_wingspan_ta, 1), pos = 3)
par(op)


# Using filter() function to create subsets to calculate mean, sd, n, and se
# for each species.
# @param {data.frame} (data) - data frame
# @param {expression} (Species == "species_name") - condition to filter rows
# @return {data.frame} - filtered data frame
sparrow_filter <- filter(bird_data, Species == "sparrow")
kingfisher_filter <- filter(bird_data, Species == "kingfisher")
eagle_filter <- filter(bird_data, Species == "eagle")
hummingbird_filter <- filter(bird_data, Species == "hummingbird")

sparrow_filter_mean <- mean(sparrow_filter$Wingspan)
sparrow_filter_mean
sparrow_filter_sd <- sd(sparrow_filter$Wingspan)
sparrow_filter_sd
sparrow_filter_n <- length(sparrow_filter$Wingspan)
sparrow_filter_n
sparrow_filter_se <- sparrow_filter_sd / sqrt(sparrow_filter_n)
sparrow_filter_se

kingfisher_filter_mean <- mean(kingfisher_filter$Wingspan)
kingfisher_filter_mean
kingfisher_filter_sd <- sd(kingfisher_filter$Wingspan)
kingfisher_filter_sd
kingfisher_filter_n <- length(kingfisher_filter$Wingspan)
kingfisher_filter_n
kingfisher_filter_se <- kingfisher_filter_sd / sqrt(kingfisher_filter_n)
kingfisher_filter_se

eagle_filter_mean <- mean(eagle_filter$Wingspan)
eagle_filter_mean
eagle_filter_sd <- sd(eagle_filter$Wingspan)
eagle_filter_sd
eagle_filter_n <- length(eagle_filter$Wingspan)
eagle_filter_n
eagle_filter_se <- eagle_filter_sd / sqrt(eagle_filter_n)
eagle_filter_se

hummingbird_filter_mean <- mean(hummingbird_filter$Wingspan)
hummingbird_filter_mean
hummingbird_filter_sd <- sd(hummingbird_filter$Wingspan)
hummingbird_filter_sd
hummingbird_filter_n <- length(hummingbird_filter$Wingspan)
hummingbird_filter_n
hummingbird_filter_se <- hummingbird_filter_sd / sqrt(hummingbird_filter_n)
hummingbird_filter_se

means_filter <- c(eagle_filter_mean, hummingbird_filter_mean, kingfisher_filter_mean, sparrow_filter_mean)
means_filter
sds_filter <- c(eagle_filter_sd, hummingbird_filter_sd, kingfisher_filter_sd, sparrow_filter_sd)
sds_filter
ns_filter <- c(eagle_filter_n, hummingbird_filter_n, kingfisher_filter_n, sparrow_filter_n)
ns_filter
ses_filter <- c(eagle_filter_se, hummingbird_filter_se, kingfisher_filter_se, sparrow_filter_se)
ses_filter

# Create a bar plot with error bars
bar_centers <- barplot(means_filter, names.arg = c("eagle", "hummingbird", "kingfisher", "sparrow"), ylim = c(0, 220), ylab = "Mean Wingspan (cm)", xlab = "Bird Species", main = "Mean Wingspan of Different Bird Species")
arrows(bar_centers, means_filter - ses_filter, bar_centers, means_filter + ses_filter, angle = 90, code = 3, length = 0.1, col = "red")
text(bar_centers, means_filter, labels = round(means_filter, 1), pos = 3)


# ------ 2. Temperature Data Analysis -----------------------------------------

# 2. Calculate for the mean and standard deviation of the temperature. Get the 
#    maximum and minimum temperatures. Using a bar graph, plot the temperature 
#    for August 27, 2020 (9-11AM).

### 2.1 Data inspection

# Read the temperature data from CSV file (no assumptions)
temp_raw_data <- read.csv(p("data", "20200827_light_barren_deep.csv"), header = FALSE)

# Check the structure of the data
str(temp_raw_data)

# Check the dimensions of the data
dim(temp_raw_data)

# Check the column names
colnames(temp_raw_data)

# Display first few rows of the data
head(temp_raw_data) 

# Display last few rows of the data
tail(temp_raw_data) 

# Display structure of the data
str(temp_raw_data)

# Initial observations:
# - The data has 8 columns and 2631 rows.
# - Column names are V1 to V8 (default names during read).
# - The first row, first column appears to be a metadata (serial number). This
#   should be removed for analysis.
# - The first column appears to be date-time information.
# - The second column appears to be temperature readings.
# - The third column appears to be light intensity (in lux).
# - The fourth to eighth columns appear to be button presses and other flags.
# - The seventh column appears to be an end-of-file (eof) marker.
# - The eight column appears to be empty (based on head and tail output).

# Further inspection of the data (columns four to eight)

# V4 - Button down events
table(temp_raw_data$V4)

# V5 - Button up events
table(temp_raw_data$V5)

# V6 - Host connect events
table(temp_raw_data$V6)

# V7 - End of file marker
table(temp_raw_data$V7)

# V8 - Empty column
table(temp_raw_data$V8)

# Further observations:
# - Columns V4 to V7 contain mostly zeros with a few ones, indicating button
#   presses and events.
# - Column V8 is empty and can be removed.
# - The main columns of interest for temperature analysis are V1 (datetime) and
#   V2 (temperature).

### 2.2 Data cleaning and preparation

# Create a copy of the raw data for cleaning
temp_data <- temp_raw_data

# Remove the empty column (V8)
temp_data <- temp_data[, -8]

# Store metadata (serial number) before removing
serial_number <- temp_data[1, 1]

# Remove the first row containing a metadata (serial number)
temp_data <- temp_data[-1, ]

# Use first row as headers
colnames(temp_data) <- c("datetime", "temperature", "light", "button_down", "button_up", "host_connect", "eof")

# Remove first row again after setting headers
temp_data <- temp_data[-1, ]

summary(temp_data)
str(temp_data)
head(temp_data)
tail(temp_data)

table(temp_data$button_down)
table(temp_data$button_up)
table(temp_data$host_connect)
table(temp_data$eof)

# Observations after cleaning:
# - The data now has 2629 rows and 7 columns.
# - The datetime column is of character type and needs to be converted to POSIXct.
# - The temperature column is of character type and needs to be converted to numeric.
# - The light column is of character type and needs to be converted to numeric.
# - The button_down, button_up, host_connect, and eof columns are mostly empty.
# - The button_down, button_up, host_connect, and eof columns contain "Logged" 
#   events at the end of the data.
# - The button_down, button_up, host_connect, and eof columns can be removed
#   for temperature analysis.
# - Last three rows does not contain temperature and light data.

# Further cleaning

# Remove columns not needed for temperature analysis
temp_data <- temp_data[, c("datetime", "temperature", "light")]

# Remove last three rows containing empty and "Logged" values
temp_data <- temp_data[1:(nrow(temp_data) - 3), ]

# Remove leading/trailing whitespace from datetime
temp_data$datetime <- trimws(temp_data$datetime) 

# Convert to proper POSIXct
temp_data$datetime <- as.POSIXct(temp_data$datetime, format = "%Y-%m-%d %H:%M:%S", tz = "Asia/Tokyo")

# Convert temperature to numeric
temp_data$temperature <- as.numeric(temp_data$temperature)

# Convert light to numeric
temp_data$light <- as.numeric(temp_data$light)

# Final check of cleaned data
summary(temp_data)
str(temp_data)
head(temp_data)
tail(temp_data)

# Observations after final cleaning:
# - The datetime column is now in POSIXct format.
# - The temperature and light columns are now numeric.
# - The data is ready for analysis.

### 2.3 Data analysis

# Calculate mean temperature
mean_temp <- mean(temp_data$temperature, na.rm = TRUE)
mean_temp

# Calculate standard deviation of temperature
sd_temp <- sd(temp_data$temperature, na.rm = TRUE)
sd_temp

# Get maximum temperature
max_temp <- max(temp_data$temperature, na.rm = TRUE)
max_temp

# Get minimum temperature
min_temp <- min(temp_data$temperature, na.rm = TRUE)
min_temp

# Filter data for August 27, 2020 (9-11 AM)
start_time <- as.POSIXct("2020-08-27 09:00:00", tz = "Asia/Tokyo")
end_time <- as.POSIXct("2020-08-27 11:00:00", tz = "Asia/Tokyo")
temp_subset <- subset(temp_data, datetime >= start_time & datetime <= end_time)

# View the bar plot of temperature from 9-11 AM (Looks identical)
barplot(temp_subset$temperature, names.arg = format(temp_subset$datetime, "%H:%M"), las = 2, col = "lightblue", ylab = "Temperature (°C)", xlab = "Time (9 - 11 AM)", main = "Temperature on August 27, 2020 (9 - 11 AM)", cex.names = 0.7)

# Save the plot to the output directory
png(filename = file.path(plot_dir, "temperature_barplot_V1_ugly.png"), width = 1600, height = 600)
barplot(temp_subset$temperature, names.arg = format(temp_subset$datetime, "%H:%M"), las = 2, col = "lightblue", ylab = "Temperature (°C)", xlab = "Time (9 - 11 AM)", main = "Temperature on August 27, 2020 (9 - 11 AM)", cex.names = 0.7)
dev.off()

# View a much more good looking bar plot 
# y = numeric temps, tm = "HH:MM" labels
y  <- as.numeric(temp_subset$temperature)
tm <- format(temp_subset$datetime, "%H:%M")
# compute a tight y-range around the data
rng  <- range(y, na.rm = TRUE)
# small breathing room
pad  <- max(0.01, diff(rng) * 0.25)  
ylim <- c(rng[1] - pad, rng[2] + pad)
# increase margins to avoid clipping
par(mar = c(10, 10, 10, 6)) 
# draw bars with zoomed y-axis (no zero)
bp <- barplot(y, names.arg = tm, las = 2, col = "lightblue", ylim = ylim, ylab = "Temperature (°C)", xlab = "Time (9 - 11 AM)", main = "Temperature on August 27, 2020 (9 - 11 AM)", yaxt = "n", cex.names = 0.9)
# tidy y ticks
axis(2, at = pretty(ylim, n = 6))      
# optional: mean line to show zoom effect
abline(h = mean(y, na.rm = TRUE), lty = 3)  
# value labels
text(bp, y, labels = sprintf("%.2f", y), pos = 3, cex = 0.8)

# Save the improved plot to the output directory
png(filename = file.path(plot_dir, "temperature_barplot_V2_better.png"), width = 1600, height = 600)
# increase margins to avoid clipping
par(mar = c(10, 10, 10, 6)) 
# draw bars with zoomed y-axis (no zero)
bp <- barplot(y, names.arg = tm, las = 2, col = "lightblue", ylim = ylim, ylab = "Temperature (°C)", xlab = "Time (9 - 11 AM)", main = "Temperature on August 27, 2020 (9 - 11 AM)", yaxt = "n", cex.names = 0.9)
# tidy y ticks
axis(2, at = pretty(ylim, n = 6))      
# optional: mean line to show zoom effect
abline(h = mean(y, na.rm = TRUE), lty = 3)  
# value labels
text(bp, y, labels = sprintf("%.2f", y), pos = 3, cex = 0.8)
dev.off()

# Alternatively, create a line plot of temperature from 9-11 AM for better visibility
plot(temp_subset$datetime, temp_subset$temperature, type = "b", xlab = "Time (9 - 11 AM)", ylab = "Temperature (°C)", main = "Temperature on August 27, 2020 (9 - 11 AM)")

# Save the line plot to the output directory
png(filename = file.path(plot_dir, "temperature_lineplot_V1.png"), width = 1600, height = 600)
plot(temp_subset$datetime, temp_subset$temperature, type = "b", xlab = "Time (9 - 11 AM)", ylab = "Temperature (°C)", main = "Temperature on August 27, 2020 (9 - 11 AM)")
dev.off()

# End of Activity 2

# MSIT 6103 Research Methods
# Activity 1

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: August 30, 2025

# Contents include:
# -- 1. Wingspan of Different Bird Species

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

# Calculate mean wingspan for each species
mean_wingspan <- aggregate(Wingspan ~ Species, data = bird_data, FUN = mean)
mean_wingspan

# Calculate standard deviation for each species
sd_wingspan <- aggregate(Wingspan ~ Species, data = bird_data, FUN = sd)
sd_wingspan

# Calculate sample size for each species
n_wingspan <- aggregate(Wingspan ~ Species, data = bird_data, FUN = length)
n_wingspan

# Calculate standard error for each species
se_wingspan <- sd_wingspan$Wingspan / sqrt(n_wingspan$Wingspan)
se_wingspan

# Create a bar plot with error bars
bar_centers <- barplot(mean_wingspan$Wingspan, names.arg = mean_wingspan$Species, ylim = c(0, 220), ylab = "Mean Wingspan (cm)", xlab = "Bird Species", main = "Mean Wingspan of Different Bird Species")
arrows(bar_centers, mean_wingspan$Wingspan - se_wingspan, bar_centers, mean_wingspan$Wingspan + se_wingspan, angle = 90, code = 3, length = 0.1, col = "red")

# Column names start at row 3
light_barren <- read.csv("20200827_light_barren_deep.csv", skip = 2) # Import CSV file
head(light_barren) # Display first few rows of the data
tail(light_barren) # Display last few rows of the data
str(light_barren) # Display structure of the data


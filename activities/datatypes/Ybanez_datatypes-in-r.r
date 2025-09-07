# Quantitative Research Activity 2
# Learning how to import and export data, and make graphs
# Written by David Ybanez September 6, 2025 University of San Carlos

# MSIT 6103 Research Methods
# All About Data in R

# By: David Ybanez (MSIT 2)

# Contents include:
# -- I. Initial setup for working directory
# -- A. Importing and Exporting Data
# -- B. Exploring Data
# -- C. Filtering Data
# -- D. Creating a Bar Plot of Species Richness in Edinburgh

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

# ------ A. Importing and Exporting Data --------------------------------------

# Install and load the dplyr package

# dplyr package
# The dplyr package is a powerful and popular R package for data manipulation
# and transformation. It provides a set of functions that allow users to easily
# manipulate data frames and perform common data wrangling tasks such as
# filtering, selecting, mutating, summarizing, and arranging data.
install.packages("dplyr")

# Load the dplyr package
library(dplyr)

# Load the edidiv dataset from the data directory
edidiv <- read.csv(p("data","edidiv.csv"))

# Display first few rows of the data
head(edidiv) 

# Display last few rows of the data
tail(edidiv)

# Display structure of the data
str(edidiv)

# Display first few entries of taxonGroup column
head(edidiv$taxonGroup)

# Display class of taxonGroup column
class(edidiv$taxonGroup) 

# Convert taxonGroup column to factor
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup)

# Display class of taxonGroup column after conversion
class(edidiv$taxonGroup) 

# Using square brackets to filter data in edidiv
# The square brackets [ ] are used for indexing and subsetting data frames in R.
# The general syntax is data_frame[rows, columns], where you can specify the
# rows and columns you want to select. Leaving a space before or after the comma
# indicates that you want to select all rows or all columns, respectively.

# Example:

# Display first four rows of all columns
# [1:4, ] only row 1-4
edidiv[1:4, ] 

# Display all rows except the third row
# [-3, ] all rows except third row
# edidiv[-3, ] # dont run this line, it will display a lot of rows 

# Display all rows of columns 1 to 4
# [,1:4] only columns 1-4
# edidiv[,1:4] # dont run this line, it will display a lot of rows

# Display all rows of all columns except the fifth column
# [,-5] all rows except fifth column
# edidiv[,-5] # dont run this line, it will display a lot of rows

# Display specific rows
# [c(1,3,5), ] only rows 1, 3, and 5
edidiv[c(1,3,5), ] 

# The edidiv object contains species occurrence records collected in Edinburgh 
# between 2000 and 2016. To explore the city’s biodiversity, we’ll create a
# graph showing the number of species recorded in each taxonomic group. Species
# richness refers to the total number of distinct species within a given area 
# or group. To find out how many species of birds, plants, mammals, and so on 
# are present in Edinburgh, we’ll first need to split the edidiv dataset into 
# separate objects—each containing data for just one taxonomic group. For 
# this, we’ll use the powerful filter() function from the dplyr package.

# Filter data for birds
Bird <- filter(edidiv, taxonGroup == "Bird")

# Filter data for flowering plants
Flowering.Plants <- filter(edidiv, taxonGroup == "Flowering.Plants")

# Filter data for fungi
Fungus <- filter(edidiv, taxonGroup == "Fungus") 

# Filter data for beetles
Beetle <- filter(edidiv, taxonGroup == "Beetle")

# Filter data for butterflies
Butterfly <- filter(edidiv, taxonGroup == "Butterfly")

# Filter data for dragonflies
Dragonfly <- filter(edidiv, taxonGroup == "Dragonfly")

# Filter data for hymenopterans
Hymenopteran <- filter(edidiv, taxonGroup == "Hymenopteran")

# Filter data for lichens
Lichen <- filter(edidiv, taxonGroup == "Lichen")

# Filter data for liverworts
Liverwort <- filter(edidiv, taxonGroup == "Liverwort")

# Filter data for molluscs
Mollusc <- filter(edidiv, taxonGroup == "Mollusc")

# Filter data for mammals
Mammal <- filter(edidiv, taxonGroup == "Mammal")

# Display summary of edidiv data
summary(edidiv)

# Show the different taxonomic groups in the dataset
unique(edidiv$taxonGroup) # Display unique taxonomic groups in taxonGroup

# ⏱️Practice Time
# Do these for steps for all taxa in the dataset. Once you have created objects
# for each taxon, we can calculate species richness, i.e. the number of 
# different species in each group. For this, we will nest two functions
# together: unique(), which identifies different species, and length(), which 
# counts them.

# Count unique species names in Bird group
unique_bird <- length(unique(Bird$taxonName)) 

# Count unique species names in Flowering.Plants group
unique_flowering_plants <- length(unique(Flowering.Plants$taxonName))

# Count unique species names in Fungus group
unique_fungus <- length(unique(Fungus$taxonName)) 

# Count unique species names in Beetle group
unique_beetle <- length(unique(Beetle$taxonName)) 

# Count unique species names in Butterfly group
unique_butterfly <- length(unique(Butterfly$taxonName)) 

# Count unique species names in Dragonfly grouph
unique_dragonfly <- length(unique(Dragonfly$taxonName)) 

# Count unique species names in Hymenoptera group
unique_hymenopteran <- length(unique(Hymenopteran$taxonName)) 

# Count unique species names in Lichen group
unique_lichen <- length(unique(Lichen$taxonName)) 

# Count unique species names in Liverwort group
unique_liverwort <- length(unique(Liverwort$taxonName)) 

# Count unique species names in Mollusc group
unique_mollusc <- length(unique(Mollusc$taxonName)) 

# Count unique species names in Mammal group
unique_mammal <- length(unique(Mammal$taxonName)) 

# Combine the results into a vector
biodiv <- c(unique_beetle, unique_bird, unique_butterfly, unique_dragonfly, unique_flowering_plants, unique_fungus, unique_hymenopteran, unique_lichen, unique_liverwort, unique_mammal, unique_mollusc)

# Display the biodiversity vector in alphabetical order
biodiv 

# Assign names to the vector elements
names(biodiv) <- c("Beetle", "Bird", "Butterfly", "Dragonfly", "Flowering.Plants", "Fungus", "Hymenopteran", "Lichen", "Liverwort", "Mammal", "Mollusc")

# Create a basic bar plot of species richness
barplot(biodiv) 

# Create a bar plot of species richness with labels and title
barplot(biodiv, main="Species Richness in Edinburgh by Taxonomic Group", xlab="Taxonomic Group", ylab="Number of Species", col="lightblue", las=2) 

# Save the bar plot as a PNG file relative to the script location
# Open a PNG device
png("output/barplot.png", width=1600, height=600)
png("barplot.png", width=1600, height=600)

# Create a more customized bar plot
barplot(biodiv, xlab="Taxa", ylab="Number of Species", ylim=c(0,600), cex.names=1.5, cex.axis=1.5, cex.lab=1.5)

# Turn off the PNG device 
dev.off()

# Create a dataframe and plot it. 
# Data frames are tables of values: they have a two-dimensional structure with rows and columns, where each column can have a different data type. For instance, a column called “Wingspan” would have numeric values measured on different birds (21.3, 182.1, 25.1, 8.9), and a column “Species” would have character values of with the names of the species (“House sparrow”, “Golden eagle”, “Eurasian kingfisher”, “Ruby-throated hummingbird”) Another possible data format is a matrix - a matrix can have several rows of data as well (e.g. you can combine vectors into a matrix), but the variables must be all of the same type. For instance they are all numerical and are the same length in terms of the number of rows.We will use the data.frame() function, but first we will create an object that contains the names of all the taxa (one column) and another object with all the values for the species richness of each taxon (another column). 

# Creating an object called "taxa" that contains all the taxa names 

# Turning this object into a factor, i.e. a categorical variable taxa_f <- factor(taxa) 
# Combining all the values for the number of species in an object called richness 
richness <- c(beetleUnique, birdUnique, butterFlyUnique, dragonFlyUnique, floweringPlantsUnique, fungusUnique, hymenopteranUnique, lichenUnique, liverwortUnique, mammalUnique, molluscUnique) 

# Creating the data frame from the two vectors 
biodata <- data.frame(taxa_f, richness) 

# Saving the file 
write.csv(biodata, file="biodata.csv")

png("barplot2.png", width=1600, height=600)
barplot(biodata$richness, names.arg=c("Beetle", "Bird", "Butterfly", "Dragonfly", "Flowering.Plants", "Fungus", "Hymenopteran", "Lichen", "Liverwort", "Mammal", "Mollusc"), xlab="Taxa", ylab="Number of Species", ylim=c(0,600))
dev.off()

library(here)
getwd()
here()

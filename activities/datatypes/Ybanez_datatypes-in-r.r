# Quantitative Research Activity 2
# Learning how to import and export data, and make graphs
# Written by David Ybanez September 6, 2025 University of San Carlos

# MSIT 6103 Research Methods
# All About Data in R

# By: David Ybanez (MSIT 2)

# Contents include:
# -- A. Importing and Exporting Data
# -- B. Exploring Data
# -- C. Filtering Data
# -- D. Creating a Bar Plot of Species Richness in Edinburgh

# ------ A. Importing and Exporting Data --------------------------------------

# Install and load the here and dplyr packages

# here package
# The here package is used to construct file paths relative to the top-level
# directory of a project. It helps in managing file paths in a way that is
# independent of the current working directory, making it easier to share code
# and data across different systems and environments.
install.packages("here")

# dplyr package
# The dplyr package is a powerful and popular R package for data manipulation
# and transformation. It provides a set of functions that allow users to easily
# manipulate data frames and perform common data wrangling tasks such as
# filtering, selecting, mutating, summarizing, and arranging data.
install.packages("dplyr")

# Load the here and dplyr packages
library(here)
library(dplyr)

# Display current working directory
here::dr_here()

# Import CSV file using read.csv() and here() functions
# The read.csv() function is used to read data from a CSV (Comma-Separated
# Values) file and create a data frame in R. The here() function from the here
# package is used to construct the file path to the CSV file in a way that is
# independent of the current working directory.
edidiv <- read.csv(here("data", "edidiv.csv"))


head(edidiv) # Display first few rows of the data
tail(edidiv) # Display last few rows of the data
str(edidiv) # Display structure of the data

head(edidiv$taxonGroup) # Display first few entries of taxonGroup column
class(edidiv$taxonGroup) # Display class of taxonGroup column
edidiv$taxonGroup <- as.factor(edidiv$taxonGroup) # Convert taxonGroup column to factor
class(edidiv$taxonGroup) # Display class of taxonGroup column after conversion

# Using square brackets to filter data in edidiv
# Note: In R, the square bracket [ ] notation is used to extract specific rows or columns from a data frame. This notation follows the format dataframe[rows, columns], where rows specify the desired rows and columns specify the desired columns.
# Example:
# [1:4, ] only row 1-4
edidiv[1:4, ] # Display first four rows of all columns
# [-3, ] all rows except third row
edidiv[-3, ] # Display all rows except the third row
# [,1:4] only columns 1-4
edidiv[,1:4] # Display all rows of columns 1 to 4
# [,-5] all rows except fifth column
edidiv[,-5] # Display all rows of all columns except the fifth column
# [c(1,3,5), ] only rows 1, 3, and 5
edidiv[c(1,3,5), ] # Display rows

# The edidiv object contains species occurrence records collected in Edinburgh between 2000 and 2016. To explore the city’s biodiversity, we’ll create a graph showing the number of species recorded in each taxonomic group. Species richness refers to the total number of distinct species within a given area or group. To find out how many species of birds, plants, mammals, and so on are present in Edinburgh, we’ll first need to split the edidiv dataset into separate objects—each containing data for just one taxonomic group. For this, we’ll use the powerful filter() function from the dplyr package.

# Filter data for birds
Bird <- filter(edidiv, taxonGroup == "Bird") # Create a new object for birds

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


# Filter 

summary(edidiv) # Display summary of edidiv data

# Show the different taxonomic groups in the dataset
unique(edidiv$taxonGroup) # Display unique taxonomic groups in taxonGroup

# ⏱️Practice Time
# Do these for steps for all taxa in the dataset. Once you have created objects for each taxon, we can calculate species richness, i.e. the number of different species in each group. For this, we will nest two functions together: unique(), which identifies different species, and length(), which counts them.

birdUnique <- length(unique(Bird$taxonName)) # Count unique species names in Bird group
floweringPlantsUnique <- length(unique(Flowering.Plants$taxonName)) # Count unique species names in Flowering.Plants group
fungusUnique <- length(unique(Fungus$taxonName)) # Count unique species names in Fungus group
beetleUnique <- length(unique(Beetle$taxonName)) # Count unique species names in Beetle group
butterFlyUnique <- length(unique(Butterfly$taxonName)) # Count unique species names in Butterfly group
dragonFlyUnique <- length(unique(Dragon.Fly$taxonName)) # Count unique species names in Dragon.Fly grouph
hymenopteranUnique <- length(unique(Hymenopteran$taxonName)) # Count unique species names in Hymenoptera group
lichenUnique <- length(unique(Lichen$taxonName)) # Count unique species names in Lichen group
liverwortUnique <- length(unique(Liverwort$taxonName)) # Count unique species names in Liverwort group
molluscUnique <- length(unique(Mollusc$taxonName)) # Count unique species names in Mollusc group
mammalUnique <- length(unique(Mammal$taxonName)) # Count unique species names in Mammal group

# Combine the results into a vector

biodiv <- c(beetleUnique, birdUnique, butterFlyUnique, dragonFlyUnique, floweringPlantsUnique, fungusUnique, hymenopteranUnique, lichenUnique, liverwortUnique, mammalUnique, molluscUnique) # Combine species counts into a vector
biodiv # Display the biodiversity vector in alphabetical order
names(biodiv) <- c("Beetle", "Bird", "Butterfly", "Dragonfly", "Flowering.Plants", "Fungus", "Hymenopteran", "Lichen", "Liverwort", "Mammal", "Mollusc") # Assign names to the vector elements

barplot(biodiv) # Create a basic bar plot of species richness
barplot(biodiv, main="Species Richness in Edinburgh by Taxonomic Group", xlab="Taxonomic Group", ylab="Number of Species", col="lightblue", las=2) # Create a bar plot of species richness

png("barplot.png", width=1600, height=600)

barplot(biodiv, xlab="Taxa", ylab="Number of Species", ylim=c(0,600), cex.names=1.5, cex.axis=1.5, cex.lab=1.5)
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

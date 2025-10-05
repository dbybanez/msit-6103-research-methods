# MSIT 6103 Research Methods
# Activity 5

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: October 5, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1) Speed Limits and Traffic Deaths
# -- 2) Trees dataset (correlation between Height and Volume)

# =============================================================================
# I. Initial setup for working directory and load packages
# =============================================================================

# Install packages
if (!requireNamespace("this.path", quietly = TRUE)) {
  install.packages("this.path")
}
if (!requireNamespace("confintr", quietly = TRUE)) {
  install.packages("confintr")
}

# Load packages
library(this.path)
library(confintr)

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

# =============================================================================
# 1) Speed Limits and Traffic Deaths
# =============================================================================

# There has been a big debate about the usefulness of speed limits on public
# roads. Consider the following table which lists the speed limits for country
# roads (in miles/h) and traffic deaths (per 100 million km) for different
# countries in 1986 when the debate was particularly serious:

# Country    Speed limit (miles/h)    Traffic deaths (per 100 million km)
# Denmark            55                        4.1
# Japan              55                        4.7
# Canada             60                        4.3
# Netherlands        60                        5.1
# Italy              75                        6.1

# (1a) Draw the scatter plot for the two variables.

speed_limits <- c(55, 55, 60, 60, 75)
traffic_deaths <- c(4.1, 4.7, 4.3, 5.1, 6.1)
countries <- c("Denmark", "Japan", "Canada", "Netherlands", "Italy")

data <- data.frame(Country = countries, Speed_Limit = speed_limits, Traffic_Deaths = traffic_deaths)
plot(data$Speed_Limit, data$Traffic_Deaths,
  xlab = "Speed Limit (miles/h)",
  ylab = "Traffic Deaths (per 100 million km)",
  main = "Scatter Plot of Speed Limits vs Traffic Deaths",
  pch = 19, col = "blue"
)

# Add the country labels to the points
text(data$Speed_Limit, data$Traffic_Deaths, labels = data$Country, pos = 4, cex = 0.8)

# (1b) Calculate the Pearson and Spearman correlation coefficients.

pearson_corr <- cor(data$Speed_Limit, data$Traffic_Deaths, method = "pearson")
print(pearson_corr) # 0.890833

spearman_corr <- cor(data$Speed_Limit, data$Traffic_Deaths, method = "spearman")
print(spearman_corr) # 0.7378648

# Interpretation:
# The Pearson correlation coefficient of approximately 0.891 indicates a strong
# positive linear relationship between speed limits and traffic deaths. This suggests
# that as speed limits increase, the number of traffic deaths tends to increase as well.
# The Spearman correlation coefficient of approximately 0.738 indicates a moderate
# positive monotonic relationship between the two variables. This suggests that while
# there is a general trend of increasing traffic deaths with higher speed limits,
# the relationship may not be strictly linear.

# (1c) What are the effects on the correlation coefficients if the speed limit
# is given in km/h rather than miles/h (1 mile/h ≈ 1.61 km/h)?

speed_limits_kmh <- speed_limits * 1.61 # Convert to km/h
data_kmh <- data.frame(Speed_Limit_kmh = speed_limits_kmh, Traffic_Deaths = traffic_deaths)

# Calculate the Pearson and Spearman correlation coefficients for km/h
pearson_corr_kmh <- cor(data_kmh$Speed_Limit_kmh, data_kmh$Traffic_Deaths, method = "pearson")
print(pearson_corr_kmh) # 0.890833

spearman_corr_kmh <- cor(data_kmh$Speed_Limit_kmh, data_kmh$Traffic_Deaths, method = "spearman")
print(spearman_corr_kmh) # 0.7378648

# Interpretation:
# The correlation coefficients remain unchanged when converting speed limits
# from miles/h to km/h. This is because correlation coefficients are scale-invariant,
# meaning that they do not depend on the units of measurement. Therefore, the strength
# and direction of the relationship between speed limits and traffic deaths remain the same
# regardless of the units used for speed limits.

# (1d) Consider one more observation: the speed limit for England was
# 70 miles/h and the death rate was 3.1.

# (1d.i) Add this observation to the scatter plot.

# Add England if not already present
if (!any(data$Country == "England")) {
  data <- rbind(data, data.frame(Country = "England", Speed_Limit = 70, Traffic_Deaths = 3.1))
}

# Plot points with England highlighted
plot(data$Speed_Limit, data$Traffic_Deaths,
  xlab = "Speed Limit (miles/h)",
  ylab = "Traffic Deaths (per 100 million km)",
  main = "Scatter Plot of Speed Limits vs Traffic Deaths (with England)",
  pch = 19, col = ifelse(data$Country == "England", "red", "blue")
)

# Non-England labels
text(data$Speed_Limit[data$Country != "England"],
  data$Traffic_Deaths[data$Country != "England"],
  labels = data$Country[data$Country != "England"], pos = 4, cex = 0.8
)

# England label in red
text(70, 3.1, labels = "England", pos = 4, cex = 0.8, col = "red")

# (1d.ii) Calculate the Pearson correlation coefficient given this additional observation.

# Calculate the Pearson correlation coefficient with England included
pearson_corr_england <- cor(data$Speed_Limit, data$Traffic_Deaths, method = "pearson")
print(pearson_corr_england) # 0.2411915

# Optional: Calculate the Spearman correlation coefficient with England included
spearman_corr_england <- cor(data$Speed_Limit, data$Traffic_Deaths, method = "spearman")
print(spearman_corr_england) # 0.2648204

# Interpretation:
# The Pearson correlation coefficient drops significantly to approximately 0.241
# after including the England data point, indicating a weak positive linear relationship.
# This suggests that the inclusion of England, which has a high speed limit but a low death
# rate, disrupts the previously strong linear relationship observed.

# Optional: The Spearman correlation coefficient also decreases to approximately 0.265,
# indicating a weak positive monotonic relationship. This further supports the
# idea that the additional data point has introduced variability that weakens
# the overall correlation between speed limits and traffic deaths.

# =============================================================================
# 2) Trees dataset (correlation between Height and Volume)
# =============================================================================

# Using the trees data set, determine the correlation coefficients of the
# association between tree Height and Volume. Create a scatter plot. Interpret
# your results. State the null and alternative hypotheses.

# Load the trees dataset
data(trees)

# Display the first few rows of the dataset
head(trees)

# Calculate the Pearson correlation coefficient
pearson_corr_trees <- cor(trees$Height, trees$Volume, method = "pearson")
print(pearson_corr_trees) # 0.5982497

# Calculate the Spearman correlation coefficient
spearman_corr_trees <- cor(trees$Height, trees$Volume, method = "spearman")
print(spearman_corr_trees) # 0.5787101

# Create a scatter plot
plot(trees$Height, trees$Volume,
  xlab = "Height (ft)",
  ylab = "Volume (cubic ft)",
  main = "Scatter Plot of Tree Height vs Volume",
  pch = 19, col = "blue"
)

# Add a regression line for better visualization
abline(lm(Volume ~ Height, data = trees), col = "red") # upward trend

# Interpretation:
# The Pearson correlation coefficient of approximately 0.598 indicates a moderate
# positive linear relationship between tree height and volume. This suggests that as
# tree height increases, the volume of the tree tends to increase as well.
# The Spearman correlation coefficient of approximately 0.579 indicates a moderate
# positive monotonic relationship between the two variables. This suggests that while
# there is a general trend of increasing volume with greater height, the relationship
# may not be strictly linear.

# Hypotheses:
# Null Hypothesis (H0): There is no correlation between tree height and volume (ρ = 0).
# Alternative Hypothesis (H1): There is a correlation between tree height and volume (ρ ≠ 0).

# The correlation coefficients suggest rejecting the null hypothesis in favor of the
# alternative hypothesis, indicating a significant correlation between tree height and volume.

# =============================================================================
# 3) Telomere Inheritance
# =============================================================================

# The ends of chromosomes are called telomeres. These telomeres are shortened
# a bit during each cell cycle as DNA is replicated. One of their purposes is
# to protect more valuable DNA in the chromosome from degradation during
# replication. As people get older and their cells have replicated more often,
# their telomeres shorten. There is evidence that these shortened telomeres may
# play a role in aging. Telomeres can be lengthened in germ cells and stem cells
# by an enzyme called telomerase, but this enzyme is not active in most healthy
# somatic cells. (Cancer cells, on the other hand, usually express telomerase.)

# Given that the length of telomeres is biologically important, it becomes
# interesting to know whether telomere length varies between individuals and
# whether this variation is inherited. A set of data was collected
# by Nordfjäll et al. (2005) on the telomere length of fathers and their
# children; these data are in the file "telomere inheritance.csv".

# Load the telomere inheritance data
telomere_data <- read.csv(p("data", "telomere inheritance.csv"))

summary(telomere_data)

# father_telomere_length offspring_telomere_length
# Min.   :0.2810         Min.   :0.3110
# 1st Qu.:0.5040         1st Qu.:0.5820
# Median :0.5900         Median :0.7820
# Mean   :0.6073         Mean   :0.8198
# 3rd Qu.:0.6990         3rd Qu.:1.0280
# Max.   :1.0620         Max.   :1.5390

# (3a) Create a scatter plot showing the relationship between father and
# offspring telomere length.

plot(telomere_data$father_telomere_length, telomere_data$offspring_telomere_length,
  xlab = "Father's Telomere Length",
  ylab = "Offspring's Telomere Length",
  main = "Scatter Plot of Father's vs Offspring's Telomere Length",
  pch = 19, col = "blue"
)

# (3b) Do the data require any transformation before analysis using linear
# regression?

# Check for normality of both variables using histograms
par(mfrow = c(1, 2)) # Set up the plotting area for two plots
hist(telomere_data$father_telomere_length, main = "Histogram of Father's Telomere Length", xlab = "Father's Telomere Length", col = "lightblue")
hist(telomere_data$offspring_telomere_length, main = "Histogram of Offspring's Telomere Length", xlab = "Offspring's Telomere Length", col = "lightgreen")
par(mfrow = c(1, 1)) # Reset plotting area

# Perform Shapiro-Wilk test for normality
shapiro.test(telomere_data$father_telomere_length) # W = 0.9609, p-value = 0.1691
shapiro.test(telomere_data$offspring_telomere_length) # W = 0.96542, p-value = 0.2424

# Interpretation:
# The histograms suggest that both father's and offspring's telomere lengths
# are approximately normally distributed. The Shapiro-Wilk test results (p-values
# greater than 0.05) indicate that we fail to reject the null hypothesis of normality
# for both variables. Therefore, no transformation is necessary before performing
# linear regression.

# Conclusion:
# The data do not require any transformation before analysis using linear regression,
# as both variables appear to be normally distributed.

# (3c) Is there evidence that the father’s telomere length predicts his
# offspring’s value?

# Add regression line to the scatter plot
abline(telomere_model, col = "red")

# Fit a linear regression model
telomere_model <- lm(offspring_telomere_length ~ father_telomere_length, data = telomere_data)

# Summary of the model
summary(telomere_model)

# Call:
# lm(formula = offspring_telomere_length ~ father_telomere_length,
# data = telomere_data)
#
# Residuals:
# Min       1Q   Median       3Q      Max
# -0.36768 -0.17438 -0.04824  0.12324  0.69947
#
# Coefficients:
# Term                     Estimate   Std. Error t value  Pr(>|t|)
# (Intercept)              0.2252     0.1396     1.613    0.115
# father_telomere_length   0.9792     0.2208     4.436    7.29e-05 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.2498 on 39 degrees of freedom
# Multiple R-squared:  0.3353,    Adjusted R-squared:  0.3183
# F-statistic: 19.68 on 1 and 39 DF,  p-value: 7.287e-05

# Model results (simplified)
# offspring_telomere_length = 0.2252 + 0.9792 × father_telomere_length
# Term	                    Estimate	Std. Error	t-value	p-value
# Intercept	                0.2252	  0.1396	    1.613	  0.115
# Father's telomere length	0.9792	  0.2208	    4.436	  7.29×10⁻⁵*

# R² = 0.3353, Adjusted R² = 0.3183
# Residual SE = 0.2498, F(1, 39) = 19.68, p = 7.29×10⁻⁵

# Interpretation:
# The linear regression analysis indicates that there is a statistically significant
# relationship between father's telomere length and offspring's telomere length (p-value < 0.001).
# The coefficient for father's telomere length is approximately 0.979, suggesting that for each unit increase
# in father's telomere length, the offspring's telomere length increases by about 0.979 units on average.
# The R-squared value of approximately 0.335 indicates that about 33.5% of the variability in offspring's
# telomere length can be explained by the father's telomere length.

# Conclusion:
# There is strong evidence to suggest that a father's telomere length is a significant predictor
# of his offspring's telomere length.

# End of Activity 5

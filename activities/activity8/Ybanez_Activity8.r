# MSIT 6103 Research Methods
# Activity 8

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: December 5, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1)

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
if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
if (!requireNamespace("car", quietly = TRUE)) {
  install.packages("car")
}
if (!requireNamespace("ggpubr", quietly = TRUE)) {
  install.packages("ggpubr")
}
if (!requireNamespace("rstatix", quietly = TRUE)) {
  install.packages("rstatix")
}
if (!requireNamespace("multcomp", quietly = TRUE)) {
  install.packages("multcomp")
}
if (!requireNamespace("gridExtra", quietly = TRUE)) {
  install.packages("gridExtra")
}
if (!requireNamespace("effectsize", quietly = TRUE)) {
  install.packages("effectsize")
}
if (!requireNamespace("MASS", quietly = TRUE)) {
  install.packages("MASS")
}
if (!requireNamespace("mvnormalTest", quietly = TRUE)) {
  install.packages("mvnormalTest")
}
if (!requireNamespace("heplots", quietly = TRUE)) {
  install.packages("heplots")
}

# Load packages
library(this.path)
library(confintr)
library(tidyverse)
library(car)
library(ggpubr)
library(rstatix)
library(multcomp)
library(gridExtra)
library(effectsize)
library(MASS)
library(mvnormalTest)
library(heplots)

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
# 1) Diet data – ANOVA + residual plot + Levene test
# =============================================================================

# Analyze the diet.csv file. It contains information on 78 people who undertook
# one of three diets. Determine which diet was best for losing weight
# specifically identify which diet is best for males and females. Interpret
# your results. Additionally, produce a residual plot and use Levene’s test for
# equality of homogeneity.

# Read the diet data
# Header starts at row 3
diet_data <- read.csv(p("data", "diet.csv"), skip = 2)

str(diet_data)
# 'data.frame':   78 obs. of  7 variables:
# $ gender        : chr  "Male" "Male" "Female" "Female" ...
# $ age           : int  33 41 37 45 30 39 32 44 35 38 ...
# $ height        : int  178 165 192 154 181 173 169 188 157 194 ...
# $ initial.weight: int  74 58 83 46 69 72 55 88 61 79 ...
# $ diet          : chr  "A" "B" "C" "A" ...
# $ final.weight  : int  72 55 83 46 69 78 57 88 61 79 ...
# $ X             : logi  NA NA NA NA NA NA ...

summary(diet_data)
#-    gender        age         height      initial.weight  diet    final.weight    weight_loss
#- Female:42   Min.   :19   Min.   :152.0   Min.   :41.00   A:26   Min.   :41.00   Min.   :-17.0000
#- Male  :36   1st Qu.:33   1st Qu.:163.2   1st Qu.:54.25   B:26   1st Qu.:54.25   1st Qu.:  0.0000
#-             Median :37   Median :174.5   Median :67.50   C:26   Median :67.50   Median :  0.0000
#-             Mean   :37   Mean   :174.6   Mean   :66.86          Mean   :66.97   Mean   : -0.1154
#-             3rd Qu.:41   3rd Qu.:185.8   3rd Qu.:79.75          3rd Qu.:79.75   3rd Qu.:  0.0000
#-             Max.   :45   Max.   :200.0   Max.   :92.00          Max.   :90.00   Max.   :  5.0000

# Remove X column
diet_data <- diet_data[, -7]

# Convert to factors
diet_data <- diet_data %>%
  mutate(
    diet = factor(diet),
    gender = factor(gender),
    weight_loss = initial.weight - final.weight
  )

# Inspect after transformations
str(diet_data)
#- 'data.frame':   78 obs. of  7 variables:
#-  $ gender        : Factor w/ 2 levels "Female","Male": 2 2 1 1 2 1 2 1 2 1 ...
#-  $ age           : int  33 41 37 45 30 39 32 44 35 38 ...
#-  $ height        : int  178 165 192 154 181 173 169 188 157 194 ...
#-  $ initial.weight: int  74 58 83 46 69 72 55 88 61 79 ...
#-  $ diet          : Factor w/ 3 levels "A","B","C": 1 2 3 1 2 3 1 3 2 1 ...
#-  $ final.weight  : int  72 55 83 46 69 78 57 88 61 79 ...
#-  $ weight_loss   : int  2 3 0 0 0 -6 -2 0 0 0 ...

summary(diet_data$weight_loss)
#-     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#- -17.0000   0.0000   0.0000  -0.1154   0.0000   5.0000

# -----------------------------------------------------------------------------
# One-way ANOVA: Does diet affect weight loss?
# -----------------------------------------------------------------------------

anova_diet <- aov(weight_loss ~ diet, data = diet_data)
summary(anova_diet)
# -           Df Sum Sq Mean Sq F value Pr(>F)
# diet         2    8.4   4.192   0.667  0.516
# Residuals   75  471.6   6.288

# Interpretation:
#
# There is no statistically significant difference in weight loss among Diet A,
# Diet B, and Diet C.

# Tukey HSD post-hoc test
TukeyHSD(anova_diet)
# Tukey multiple comparisons of means
# 95% family-wise confidence level
#
# Fit: aov(formula = weight_loss ~ diet, data = diet_data)
#
# $diet
# -          diff       lwr       upr     p adj
# B-A -0.65384615 -2.316777 1.0090848 0.6167570
# C-A -0.73076923 -2.393700 0.9321617 0.5474696
# C-B -0.07692308 -1.739854 1.5860078 0.9932786

# Interpretation:
#
# The Tukey HSD test confirms that there are no significant differences in weight
# loss between any pairs of diets.

# -----------------------------------------------------------------------------
# Two-way ANOVA: Does diet and gender affect weight loss?
# -----------------------------------------------------------------------------

anova_diet_gender <- aov(weight_loss ~ diet * gender, data = diet_data)
summary(anova_diet_gender)
# -           Df Sum Sq Mean Sq F value Pr(>F)
# diet         2    8.4   4.192   0.673  0.513
# gender       1    0.0   0.008   0.001  0.972
# diet:gender  2   23.1  11.558   1.856  0.164
# Residuals   72  448.5   6.229

# Interpretation:
#
# There are no statistically significant effects of diet, gender, or their
# interaction on weight loss.

# Conclusion so far:
#
# 1. No significant difference in weight loss among diets.
# 2. No significant effect of gender on weight loss.
# 3. No significant interaction between diet and gender on weight loss.

# -----------------------------------------------------------------------------
# Compute group means for descriptive comparison
# -----------------------------------------------------------------------------

diet_data %>%
  group_by(gender, diet) %>%
  summarise(
    mean_weight_loss = mean(weight_loss),
    sd = sd(weight_loss),
    n = n()
  )
#- A tibble: 6 x 5
#- Groups:   gender [2]
#-   gender diet  mean_weight_loss    sd     n
#-   <fct>  <fct>            <dbl> <dbl> <int>
#- 1 Female A                0     0        12
#- 2 Female B                0.462 1.39     13
#- 3 Female C               -0.706 2.44     17
#- 4 Male   A                0.643 1.82     14
#- 5 Male   B               -1.08  4.86     13
#- 6 Male   C                0.222 0.667     9

# A one-way ANOVA showed that weight loss did not differ significantly among
# the three diets (F(2,75) = 0.67, p = 0.516).
#
# A two-way ANOVA found no significant main effect of gender (F(1,72) = 0.001,
# p = 0.972) and no significant diet × gender interaction (F(2,72) = 1.86, p = 0.164).
#
# This indicates that the three diets produced similar outcomes, males and
# females lost similar amounts of weight, and diet effectiveness did not depend
# on gender.
#
# Descriptively, females lost the most weight under Diet B (mean = 0.46 kg),
# while males lost the most under Diet A (mean = 0.64 kg). However, these
# differences were not statistically significant and should not be interpreted
# as meaningful effects.

# -----------------------------------------------------------------------------
# Assumption checks
# -----------------------------------------------------------------------------
leveneTest(weight_loss ~ diet, data = diet_data)
#- Levene's Test for Homogeneity of Variance (center = median)
#-       Df F value Pr(>F)
#- group  2  0.3896 0.6787
#-       75

leveneTest(weight_loss ~ interaction(diet, gender), data = diet_data)
#- Levene's Test for Homogeneity of Variance (center = median)
#-       Df F value Pr(>F)
#- group  5  0.6425  0.668
#-       72

# Interpretation:
#
# Levene's test indicates that the assumption of homogeneity of variances is met
# for both one-way and two-way ANOVA (p > 0.05).

# Residual plots
par(mfrow = c(2, 2))
plot(anova_diet_gender)
par(mfrow = 1)

# The residual diagnostics indicate that the assumptions of ANOVA were
# adequately met. The Residuals vs Fitted and Scale–Location plots show no
# major pattern or heteroscedasticity. The Normal Q–Q plot shows minor
# deviations at the lower tail, but the overall distribution remains
# approximately normal, which is acceptable given the sample size. The
# Residuals vs Leverage plot does not identify any influential observations.
# Combined with non-significant Levene's tests (p > 0.05), the ANOVA
# assumptions are considered satisfied.

# MSIT 6103 Research Methods
# Activity 5 (Practice - Correlation Coefficient and Tests)

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: October 4, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1. Correlation Coefficient and Tests

# ------ I. Initial setup for working directory and load packages -------------

# Install packages
if (!requireNamespace("this.path", quietly = TRUE)) {
  install.packages("this.path")
}

# Load packages
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


# ------ 1. Correlation Coefficient and Tests ---------------------------------

# Load the “data.csv” file. Download it from Canvas.
# Determine the relationship between pH and Shannon index. Interpret the results.

dataset <- read.csv(p("data", "data.csv"))
head(dataset)

# Sample.ID Subplot Coreplot Location   Fertilizer    Weeding      Fertilizer.weeding
# 1  d_OM1cha  OM1cha    OM1ch      OM1 conventional  herbicide  conventional_herbicide
# 2  d_OM1chb  OM1chb    OM1ch      OM1 conventional  herbicide  conventional_herbicide
# 3  d_OM1chc  OM1chc    OM1ch      OM1 conventional  herbicide  conventional_herbicide
# 4  d_OM1chd  OM1chd    OM1ch      OM1 conventional  herbicide  conventional_herbicide
# 5  d_OM1che  OM1che    OM1ch      OM1 conventional  herbicide  conventional_herbicide
# 6  d_OM1cwa  OM1cwa    OM1cw      OM1 conventional mechanical conventional_mechanical
# Sampling_date Template Shannon.index    N    C   pH     Na     K    Ca    Mg    Mn    Fe
# 1    13.05.2017      DNA          7.64 0.08 1.10 4.92 -0.004 0.013 0.168 0.104 0.012 3.427
# 2    13.05.2017      DNA          7.61 0.08 1.04 4.76  0.017 0.031 0.354 0.136 0.014 3.874
# 3    13.05.2017      DNA          7.78 0.08 0.96 5.17 -0.005 0.013 0.555 0.087 0.018 2.158
# 4    13.05.2017      DNA          7.71 0.07 0.84 4.91  0.005 0.017 0.204 0.131 0.015 3.981
# 5    13.05.2017      DNA          7.36 0.06 0.96 4.86  0.003 0.012 0.244 0.116 0.013 3.839
# 6    13.05.2017      DNA          7.69 0.07 0.96 5.25  0.104 0.030 0.121 0.120 0.017 4.536
# Al     S     P
# 1 22.514 0.063 0.059
# 2 28.845 0.068 0.169
# 3 15.190 0.070 0.224
# 4 28.822 0.078 0.095
# 5 26.870 0.064 0.077
# 6 27.381 0.073 0.051

summary(dataset)
# Sample.ID           Subplot            Coreplot           Location          Fertilizer
# Length:160         Length:160         Length:160         Length:160         Length:160
# Class :character   Class :character   Class :character   Class :character   Class :character
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Mode  :character

# Weeding          Fertilizer.weeding Sampling_date        Template         Shannon.index
# Length:160         Length:160         Length:160         Length:160         Min.   :7.020
# Class :character   Class :character   Class :character   Class :character   1st Qu.:7.550
# Mode  :character   Mode  :character   Mode  :character   Mode  :character   Median :7.720
# Mean   :7.711
# 3rd Qu.:7.930
# Max.   :8.220

# N                C               pH              Na                 K
# Min.   :0.0400   Min.   :0.540   Min.   :4.460   Min.   :-0.02000   Min.   :-0.0160
# 1st Qu.:0.0700   1st Qu.:0.915   1st Qu.:4.897   1st Qu.: 0.00375   1st Qu.: 0.0100
# Median :0.0800   Median :1.140   Median :5.190   Median : 0.01700   Median : 0.0230
# Mean   :0.1005   Mean   :1.388   Mean   :5.333   Mean   : 0.07470   Mean   : 1.8582
# 3rd Qu.:0.1200   3rd Qu.:1.630   3rd Qu.:5.680   3rd Qu.: 0.05900   3rd Qu.: 0.0515
# Max.   :0.3200   Max.   :4.810   Max.   :8.830   Max.   : 0.73500   Max.   :92.0000

# Ca               Mg               Mn                Fe              Al
# Min.   :0.0160   Min.   :0.0870   Min.   :0.01000   Min.   :2.158   Min.   :12.54
# 1st Qu.:0.1948   1st Qu.:0.1360   1st Qu.:0.01575   1st Qu.:3.557   1st Qu.:19.80
# Median :0.3310   Median :0.1915   Median :0.01700   Median :4.630   Median :25.28
# Mean   :0.5261   Mean   :0.2194   Mean   :0.01922   Mean   :4.717   Mean   :24.48
# 3rd Qu.:0.6165   3rd Qu.:0.2580   3rd Qu.:0.02200   3rd Qu.:5.546   3rd Qu.:28.78
# Max.   :5.0180   Max.   :0.7890   Max.   :0.04800   Max.   :9.164   Max.   :39.54

# S                P
# Min.   :0.0410   Min.   :0.04200
# 1st Qu.:0.0695   1st Qu.:0.09275
# Median :0.0870   Median :0.12100
# Mean   :0.1038   Mean   :0.20736
# 3rd Qu.:0.1230   3rd Qu.:0.25450
# Max.   :0.3110   Max.   :1.25800

# Practice Time 1:

# Pearson’s Correlation
# Pearson’s correlation analyzes the relationship between two variables. For
# example, is there a relationship between a child’s height and age? Or is
# there a correlation between a person’s salary and educational attainment?
# Pearson’s coeffi cient ranges from -1 to 1. A value of 0 implies no linear
# association between the two variables. A positive value implies a positive
# association between the two variables (i.e., as the value of one variable
# increases, the value of the other variable also increases). A negative value
# implies a negative (i.e., as the value of one variable increases, the value
# of the other variable decreases).

# Assumptions:
# 1. The variables are of either interval or ratio scale.
# 2. The variables are approximately normally distributed.
# 3. There is a linear relationship between the two variables.
# 4. Outliers are either removed entirely or are kept to a minimum.
# 5. There is homoscedasticity of the data.

# Formula:
# r = (n(Σxy) - (Σx)(Σy)) / sqrt([nΣx^2 - (Σx)^2][nΣy^2 - (Σy)^2])
# where:
# r = Pearson’s correlation coefficient
# n = number of pairs of scores
# Σxy = sum of the product of paired scores
# Σx = sum of x scores
# Σy = sum of y scores
# Σx^2 = sum of squared x scores
# Σy^2 = sum of squared y scores

# Using R to compute Pearson's correlation coefficient
# Example: Find the corralation in the dataset between 'pH' and 'Shannon.index'

# pearson_result_cor_test <- cor.test(dataset$pH, dataset$Shannon.index, method = "pearson")
# Output:
# Pearson's product-moment correlation
#
# data:  dataset$pH and dataset$Shannon.index
# t = 6.8822, df = 158, p-value = 1.307e-10
# alternative hypothesis: true correlation is not equal to 0
# 95 percent confidence interval:
# 0.3512640 0.5913433
# sample estimates:
# cor
# 0.4802484

pearson_result_cor <- cor(dataset$pH, dataset$Shannon.index, method = "pearson")
print(pearson_result_cor) # 0.4802484


# Interpretation:
# The Pearson correlation coefficient between pH and Shannon index is 0.4802,
# indicating a moderate positive linear relationship. The p-value (1.307e-10)
# is less than 0.05, suggesting that the correlation is statistically significant.

# Practice Time 2:

# Spearman’s Correlation
# Spearman rank correlation is a non-parametric test that measures the degree
# of association between two variables. Moreover, the Spearman rank correlation
# test does not carry any assumptions about the distribution of the data. It is
# the appropriate correlation analysis when one measures the variables on a
# scale that is at least ordinal.

# Spearman’s correlation coefficient also ranges from -1 to 1. Its
# interpretation is very much like that of Pearson’s except that Spearman’s
# measures how well the relationship between two variables can be described
# using a monotonic function, which includes but is not limited to linearity.

# Assumptions:
# 1. The variables are of either interval or ratio scale.
# 2. There is a monotonic relationship between the two variables.
# 3. Outliers are kept to a minimum.

# Formula:
# ρ = 1 - (6Σd^2) / (n(n^2 - 1))
# where:
# ρ = Spearman’s correlation coefficient
# d = difference between the ranks of corresponding variables
# n = number of pairs of scores

# Using R to compute Spearman's correlation coefficient
# Example: Find the corralation in the dataset between 'pH' and 'Shannon.index'
spearman_result_cor_test <- cor.test(dataset$pH, dataset$Shannon.index, method = "spearman")
print(spearman_result_cor_test)

# Output:
# Spearman's rank correlation rho
#
# data:  dataset$pH and dataset$Shannon.index
# S = 251955, p-value < 2.2e-16
# alternative hypothesis: true rho is not equal to 0
# sample estimates:
# rho
# 0.6309112

spearman_result_cor <- cor(dataset$pH, dataset$Shannon.index, method = "spearman")
print(spearman_result_cor) # 0.6309112

# Interpretation:
# The Spearman correlation coefficient between pH and Shannon index is 0.6309,
# indicating a strong positive monotonic relationship. The p-value (< 2.2e-16)
# is less than 0.05, suggesting that the correlation is statistically significant.

# Practice Time 3:

# Kendall’s Tau
# It is a non-parametric test that measures the strength of dependence between
# two variables. If we consider two samples, a and b, where each sample size is
# n, we know that the total number of pairings with a b is n(n-1)/2.

# Assumptions:
# 1. The variables are of either interval, ratio or ordinal scale.
# 2. There is a monotonic relationship between the two variables.

# Formula:
# τ = (nc - nd) / (1/2 n(n-1))
# where:
# τ = Kendall’s Tau coefficient
# nc = number of concordant pairs
# nd = number of discordant pairs
# n = number of pairs of scores

# Using R to compute Kendall's Tau
# Example: Find the corralation in the dataset between 'pH' and 'Shannon.index'
kendall_result_cor_test <- cor.test(dataset$pH, dataset$Shannon.index, method = "kendall")
print(kendall_result_cor_test)

# Output:
# Kendall's rank correlation tau
#
# data:  dataset$pH and dataset$Shannon.index
# z = 8.2938, p-value < 2.2e-16
# alternative hypothesis: true tau is not equal to 0
# sample estimates:
# tau
# 0.4461648

kendall_result_cor <- cor(dataset$pH, dataset$Shannon.index, method = "kendall")
print(kendall_result_cor) # 0.4461648

# Interpretation:
# The Kendall's Tau coefficient between pH and Shannon index is 0.4462,
# indicating a moderate positive association. The p-value (< 2.2e-16)
# is less than 0.05, suggesting that the correlation is statistically significant.

# Correlation Test
# Unlike a correlation matrix which indicates the correlation coefficients
# between some pairs of variables in the sample, a correlation test is used to
# test whether the correlation (denoted ρ) between 2 variables is significantly
# different from 0 or not in the population.

# Actually, a correlation coefficient different from 0 in the sample does not
# mean that the correlation is significantly different from 0 in the
# population. This needs to be tested with a hypothesis test—and known as the
# correlation test.

# The null and alternative hypothesis for the correlation test are as follows:
# H0: ρ = 0 (There is no correlation between the two variables in the population)
# H1: ρ ≠ 0 (There is a correlation between the two variables

# Via this correlation test, what we are actually testing is whether: the
# sample contains sufficient evidence to reject the null hypothesis and
# conclude that the correlation coefficient does not equal 0, so the
# relationship exists in the population. or on the contrary, the sample does
# not contain enough evidence that the correlation coefficient does not equal
# 0, so in this case we do not reject the null hypothesis of no relationship
# between the variables in the population.

# Note that there are 2 assumptions for this test to be valid:
# 1. Independence of the data
# 2. For small sample sizes (usually n<30), the two variables should follow a
# normal distribution

# Practice Time 4:

# Use the mtcars dataset. Suppose that we want to test whether the rear axle
# ratio (drat) is correlated with the time to drive a quarter of a mile (qsec):
mtcars_data <- mtcars

# Pearson correlation test
pearson_mtcars_cor_test <- cor.test(mtcars_data$drat, mtcars_data$qsec)
print(pearson_mtcars_cor_test)

# Correlation tests for whole dataset
# if (!requireNamespace("Hmisc", quietly = TRUE)) { install.packages("Hmisc") }
# library(Hmisc)
remove.packages("Hmisc")
remove.packages("ggplot2")
# Calculate the correlation matrix and p-values for the mtcars dataset
# Requires the Hmisc package
res <- rcorr(as.matrix(mtcars_data))
print(res)

# Display p-values (rounded to 3 decimals)
print(round(res$P, 3))

# Visualize data using ggscatterstats
library(ggstatsplot)
ggscatterstats(data = mtcars_data, x = wt, y = mpg, bf.message = FALSE, marginal = FALSE)

library(correlation)
correlation::correlation(mtcars_data, include_factors = TRUE, method = "auto")

# Correlation Matrix (auto-method)

# Parameter1 | Parameter2 |     r |         95% CI | t(30) |         p
# --------------------------------------------------------------------
# mpg        |        cyl | -0.85 | [-0.93, -0.72] | -8.92 | < .001***
# mpg        |       disp | -0.85 | [-0.92, -0.71] | -8.75 | < .001***
# mpg        |         hp | -0.78 | [-0.89, -0.59] | -6.74 | < .001***
# mpg        |       drat |  0.68 | [ 0.44,  0.83] |  5.10 | < .001***
# mpg        |         wt | -0.87 | [-0.93, -0.74] | -9.56 | < .001***
# mpg        |       qsec |  0.42 | [ 0.08,  0.67] |  2.53 | 0.222
# mpg        |         vs |  0.66 | [ 0.41,  0.82] |  4.86 | 0.001**
# mpg        |         am |  0.60 | [ 0.32,  0.78] |  4.11 | 0.008**
# mpg        |       gear |  0.48 | [ 0.16,  0.71] |  3.00 | 0.097
# mpg        |       carb | -0.55 | [-0.75, -0.25] | -3.62 | 0.024*
# cyl        |       disp |  0.90 | [ 0.81,  0.95] | 11.45 | < .001***
# cyl        |         hp |  0.83 | [ 0.68,  0.92] |  8.23 | < .001***
# cyl        |       drat | -0.70 | [-0.84, -0.46] | -5.37 | < .001***
# cyl        |         wt |  0.78 | [ 0.60,  0.89] |  6.88 | < .001***
# cyl        |       qsec | -0.59 | [-0.78, -0.31] | -4.02 | 0.010*
# cyl        |         vs | -0.81 | [-0.90, -0.64] | -7.59 | < .001***
# cyl        |         am | -0.52 | [-0.74, -0.21] | -3.36 | 0.043*
# cyl        |       gear | -0.49 | [-0.72, -0.17] | -3.10 | 0.079
# cyl        |       carb |  0.53 | [ 0.22,  0.74] |  3.40 | 0.041*
# disp       |         hp |  0.79 | [ 0.61,  0.89] |  7.08 | < .001***
# disp       |       drat | -0.71 | [-0.85, -0.48] | -5.53 | < .001***
# disp       |         wt |  0.89 | [ 0.78,  0.94] | 10.58 | < .001***
# disp       |       qsec | -0.43 | [-0.68, -0.10] | -2.64 | 0.197
# disp       |         vs | -0.71 | [-0.85, -0.48] | -5.53 | < .001***
# disp       |         am | -0.59 | [-0.78, -0.31] | -4.02 | 0.010*
# disp       |       gear | -0.56 | [-0.76, -0.26] | -3.66 | 0.023*
# disp       |       carb |  0.39 | [ 0.05,  0.65] |  2.35 | 0.303
# hp         |       drat | -0.45 | [-0.69, -0.12] | -2.75 | 0.170
# hp         |         wt |  0.66 | [ 0.40,  0.82] |  4.80 | 0.001**
# hp         |       qsec | -0.71 | [-0.85, -0.48] | -5.49 | < .001***
# hp         |         vs | -0.72 | [-0.86, -0.50] | -5.73 | < .001***
# hp         |         am | -0.24 | [-0.55,  0.12] | -1.37 | > .999
# hp         |       gear | -0.13 | [-0.45,  0.23] | -0.69 | > .999
# hp         |       carb |  0.75 | [ 0.54,  0.87] |  6.21 | < .001***
# drat       |         wt | -0.71 | [-0.85, -0.48] | -5.56 | < .001***
# drat       |       qsec |  0.09 | [-0.27,  0.43] |  0.50 | > .999
# drat       |         vs |  0.44 | [ 0.11,  0.68] |  2.69 | 0.187
# drat       |         am |  0.71 | [ 0.48,  0.85] |  5.57 | < .001***
# drat       |       gear |  0.70 | [ 0.46,  0.84] |  5.36 | < .001***
# drat       |       carb | -0.09 | [-0.43,  0.27] | -0.50 | > .999
# wt         |       qsec | -0.17 | [-0.49,  0.19] | -0.97 | > .999
# wt         |         vs | -0.55 | [-0.76, -0.26] | -3.65 | 0.023*
# wt         |         am | -0.69 | [-0.84, -0.45] | -5.26 | < .001***
# wt         |       gear | -0.58 | [-0.77, -0.29] | -3.93 | 0.012*
# wt         |       carb |  0.43 | [ 0.09,  0.68] |  2.59 | 0.205
# qsec       |         vs |  0.74 | [ 0.53,  0.87] |  6.11 | < .001***
# qsec       |         am | -0.23 | [-0.54,  0.13] | -1.29 | > .999
# qsec       |       gear | -0.21 | [-0.52,  0.15] | -1.19 | > .999
# qsec       |       carb | -0.66 | [-0.82, -0.40] | -4.76 | 0.001**
# vs         |         am |  0.26 | [-0.09,  0.56] |  1.50 | > .999
# vs         |       gear |  0.21 | [-0.15,  0.52] |  1.15 | > .999
# vs         |       carb | -0.57 | [-0.77, -0.28] | -3.80 | 0.017*
# am         |       gear |  0.79 | [ 0.62,  0.89] |  7.16 | < .001***
# am         |       carb |  0.06 | [-0.30,  0.40] |  0.32 | > .999
# gear       |       carb |  0.27 | [-0.08,  0.57] |  1.56 | > .999
#
# p-value adjustment method: Holm (1979)
# Observations: 32

# Correlograms
# The table above is very useful and informative, but let's see if it is
# possible to combine the concepts of correlation coefficients and correlations
# test in one single visualization. A visualization that would be easy to read
# and interpret.

# Ideally, we would like to have a concise overview of correlations between all
# possible pairs of variables present in a dataset, with a clear distinction
# for correlations that are significantly different from 0.

# The figure below, known as a correlogram and adapted from the corrplot()
# function, does precisely this:
if (!requireNamespace("corrplot", quietly = TRUE)) {
  install.packages("corrplot")
}
library(corrplot)
corrplot2 <- function(
    data,
    method = "pearson",
    sig.level = 0.05,
    order = "original",
    diag = FALSE,
    type = "upper",
    tl.srt = 90,
    number.font = 1,
    number.cex = 1,
    mar = c(0, 0, 0, 0)) {
  data_incomplete <- data
  data <- data[complete.cases(data), ]
  mat <- cor(data, method = method)
  cor.mtest <- function(mat, method) {
    mat <- as.matrix(mat)
    n <- ncol(mat)
    p.mat <- matrix(NA, n, n)
    diag(p.mat) <- 0
    for (i in 1:(n - 1)) {
      for (j in (i + 1):n) {
        tmp <- cor.test(mat[, i], mat[, j], method = method)
        p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
      }
    }
    colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
    p.mat
  }
  p.mat <- cor.mtest(data, method = method)
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  corrplot(
    mat,
    method = "color",
    col = col(200),
    number.font = number.font,
    mar = mar,
    number.cex = number.cex,
    type = type,
    order = order,
    addCoef.col = "black",
    tl.col = "black",
    tl.srt = tl.srt,
    p.mat = p.mat,
    sig.level = sig.level,
    insig = "blank",
    diag = diag
  )
}

corrplot2(data = mtcars_data, method = "pearson", sig.level = 0.05, order = "original", diag = FALSE, type = "upper", tl.srt = 75)

# Using library(GGally)
if (!requireNamespace("GGally", quietly = TRUE)) {
  install.packages("GGally")
}
library(GGally)
ggpairs(mtcars_data[, c("mpg", "hp", "wt")])

# Using library(ggstatsplot)
if (!requireNamespace("ggstatsplot", quietly = TRUE)) {
  install.packages("ggstatsplot")
}
library(ggstatsplot)
ggcorrmat(
  data = mtcars_data[, c("mpg", "hp", "wt")], type = "parametric", # parametric for Pearson, nonparametric for Spearman's correlation
  colors = c("darkred", "white", "steelblue") # change default colors
)

# End of Activity 5 (Practice)

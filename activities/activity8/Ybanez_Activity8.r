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
if (!requireNamespace("vegan", quietly = TRUE)) {
  install.packages("vegan")
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
library(vegan)

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

# =============================================================================
# 2) Iris MANOVA (Sepal.Length & Petal.Length)
# =============================================================================

# Using the Iris built-in dataset, determine the effects of Iris species on
# Sepal.Length and Petal.Length using MANOVA using a multivariate statistic
# (Pillai’s Trace). Visualize your data. Check various assumptions about the
# data and perform preliminary tests. Don’t forget to perform post-hoc tests.
# Interpret your results.

library(tidyverse)
library(ggpubr)
library(rstatix)
library(mvnormalTest)
library(heplots)
library(MASS)
library(effectsize)
library(gridExtra)

# Load data
data(iris)
str(iris)
#- 'data.frame':   150 obs. of  5 variables:
#-  $ Sepal.Length: num  5.1 4.9 4.7 4.6 5 5.4 4.6 5 4.4 4.9 ...
#-  $ Sepal.Width : num  3.5 3 3.2 3.1 3.6 3.9 3.4 3.4 2.9 3.1 ...
#-  $ Petal.Length: num  1.4 1.4 1.3 1.5 1.4 1.7 1.4 1.5 1.4 1.5 ...
#-  $ Petal.Width : num  0.2 0.2 0.2 0.2 0.2 0.4 0.3 0.2 0.2 0.1 ...
#-  $ Species     : Factor w/ 3 levels "setosa","versicolor",..: 1 1 1 1 1 1 1 1 1 1 ...

iris$Species <- factor(iris$Species)
summary(iris)
str(iris)

# Combine the dependent variables
dep_vars <- cbind(
  Sepal.Length = iris$Sepal.Length,
  Petal.Length = iris$Petal.Length
)

# Fit MANOVA model
fit_manova <- manova(dep_vars ~ Species, data = iris)

# Multivariate test using Pillai's Trace
summary(fit_manova, test = "Pillai")
#-            Df Pillai approx F num Df den Df    Pr(>F)
#- Species     2 0.9885   71.829      4    294 < 2.2e-16 ***
#- Residuals 147
#- ---
#- Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretation:
#
# The MANOVA results indicate a significant effect of Species on the combined
# dependent variables (Sepal.Length and Petal.Length), Pillai's Trace = 0.9885,
# F(4, 294) = 71.83, p < 0.001.
#
# Species has a significant multivariate effect on Sepal.Length and
# Petal.Length taken together.
#
# The combination of Sepal Length + Petal Length differs strongly across
# setosa, versicolor, and virginica.

# -----------------------------------------------------------------------------
# Visualize the data
# -----------------------------------------------------------------------------

# Boxplots
p1 <- ggboxplot(
  iris,
  x = "Species", y = "Sepal.Length",
  color = "Species", add = "jitter"
) + labs(title = "Sepal Length by Species")

p2 <- ggboxplot(
  iris,
  x = "Species", y = "Petal.Length",
  color = "Species", add = "jitter"
) + labs(title = "Petal Length by Species")

grid.arrange(p1, p2, ncol = 2)

# Interpretation:
#
# All three species show clearly separated distributions for Petal.Length and
# partially overlapping but still distinct distributions for Sepal.Length. This
# visually supports the MANOVA finding that species differ significantly on the
# combination of both traits.

# Scatterplot with ellipses
ggscatter(
  iris,
  x = "Sepal.Length", y = "Petal.Length",
  color = "Species", ellipse = TRUE, mean.point = TRUE
) + labs(title = "Sepal vs Petal Length by Species")

# Interpretation:
#
# This confirms strong multivariate separation, consistent with the highly
# significant Pillai's Trace result.

# Observations so far:
#
# The visualizations strongly indicate that Iris species differ substantially
# in Sepal.Length and Petal.Length, both individually and jointly. Setosa is
# clearly distinct, while versicolor and virginica show graded increases in
# both features.

# -----------------------------------------------------------------------------
# Assumption Checks for MANOVA
# -----------------------------------------------------------------------------

# 1) Univariate normality within each Species
iris %>%
  group_by(Species) %>%
  shapiro_test(Sepal.Length, Petal.Length)
#- A tibble: 6 x 4
#-   Species    variable     statistic      p
#-   <fct>      <chr>            <dbl>  <dbl>
#- 1 setosa     Petal.Length     0.955 0.0548
#- 2 setosa     Sepal.Length     0.978 0.460
#- 3 versicolor Petal.Length     0.966 0.158
#- 4 versicolor Sepal.Length     0.978 0.465
#- 5 virginica  Petal.Length     0.962 0.110
#- 6 virginica  Sepal.Length     0.971 0.258

# Interpretation:
#
# The univariate normality assumption is satisfied for both dependent variables
# across all three species.

# 2) Multivariate normality
mardia(iris[, c("Sepal.Length", "Petal.Length")])$mv.test
#-           Test Statistic p-value Result
#- 1     Skewness   12.6691   0.013     NO
#- 2     Kurtosis   -2.1315  0.0331     NO
#- 3 MV Normality      <NA>    <NA>     NO

# Interpretation:
#
# Mardia's test indicates that the multivariate normality assumption is violated
# (p < 0.05). However, MANOVA is robust to moderate violations of this assumption,
# especially with balanced group sizes.

# 3) Homogeneity of covariance matrices (Box's M test)
boxM(
  Y = iris[, c("Sepal.Length", "Petal.Length")],
  group = iris$Species
)
#-         Box's M-test for Homogeneity of Covariance Matrices
#-
#- data:  iris[, c("Sepal.Length", "Petal.Length")]
#- Chi-Sq (approx.) = 58.376, df = 6, p-value = 9.617e-11

# Interpretation:
#
# Box's M test indicates that the homogeneity of covariance matrices assumption
# is violated (p < 0.05). However, MANOVA is relatively robust to this violation,
# particularly with equal group sizes.

# 4) Multivariate outliers (Mahalanobis)
mahal_out <- mahalanobis_distance(
  iris[, c("Sepal.Length", "Petal.Length")]
)
table(mahal_out$is.outlier)
#- FALSE
#-   150

# Interpretation:
#
# No multivariate outliers were detected.

# 5) Linearity — scatterplots for each species
p_setosa <- iris %>%
  filter(Species == "setosa") %>%
  ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  ggtitle("setosa")

p_versicolor <- iris %>%
  filter(Species == "versicolor") %>%
  ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  ggtitle("versicolor")

p_virginica <- iris %>%
  filter(Species == "virginica") %>%
  ggplot(aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point() +
  ggtitle("virginica")

grid.arrange(p_setosa, p_versicolor, p_virginica, ncol = 3)

# Interpretation:
#
# The scatterplots indicate linear relationships between Sepal.Length and
# Petal.Length within each species.
# No curvature or non-linear patterns are present.

# 6) Multicollinearity test
cor.test(
  iris$Sepal.Length,
  iris$Petal.Length,
  method = "pearson"
)
#-         Pearson's product-moment correlation
#-
#- data:  iris$Sepal.Length and iris$Petal.Length
#- t = 21.646, df = 148, p-value < 2.2e-16
#- alternative hypothesis: true correlation is not equal to 0
#- 95 percent confidence interval:
#-  0.8270363 0.9055080
#- sample estimates:
#-       cor
#- 0.8717538

# Interpretation:
#
# A strong positive correlation (r = 0.872, p < 0.001) exists between Sepal.Length
# and Petal.Length, but it is below the threshold (r < 0.9) for multicollinearity concerns.

# -----------------------------------------------------------------------------
# Post-hoc tests
# -----------------------------------------------------------------------------

# Univariate ANOVA for Sepal.Length
aov_sepal <- aov(Sepal.Length ~ Species, data = iris)
summary(aov_sepal)
#-              Df Sum Sq Mean Sq F value Pr(>F)
#- Species       2  63.21  31.606   119.3 <2e-16 ***
#- Residuals   147  38.96   0.265
#- ---
#- Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretation:
#
# There is a significant effect of Species on Sepal.Length (F(2, 147) = 119.3, p < 0.001).

TukeyHSD(aov_sepal)
#-   Tukey multiple comparisons of means
#-     95% family-wise confidence level
#-
#- Fit: aov(formula = Sepal.Length ~ Species, data = iris)
#-
#- $Species
#-                       diff       lwr       upr p adj
#- versicolor-setosa    0.930 0.6862273 1.1737727     0
#- virginica-setosa     1.582 1.3382273 1.8257727     0
#- virginica-versicolor 0.652 0.4082273 0.8957727     0

# Interpretation:
#
# The Tukey HSD test shows that all pairwise comparisons between species for
# Sepal.Length are significant (p < 0.001). Setosa has the shortest Sepal.Length,
# followed by versicolor, and virginica has the longest.

# Univariate ANOVA for Petal.Length
aov_petal <- aov(Petal.Length ~ Species, data = iris)

summary(aov_petal)
#-              Df Sum Sq Mean Sq F value Pr(>F)
#- Species       2  437.1  218.55    1180 <2e-16 ***
#- Residuals   147   27.2    0.19
#- ---
#- Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretation:
#
# There is a significant effect of Species on Petal.Length (F(2, 147) = 1180, p < 0.001).

TukeyHSD(aov_petal)
#-   Tukey multiple comparisons of means
#-     95% family-wise confidence level
#-
#- Fit: aov(formula = Petal.Length ~ Species, data = iris)
#-
#- $Species
#-                       diff     lwr     upr p adj
#- versicolor-setosa    2.798 2.59422 3.00178     0
#- virginica-setosa     4.090 3.88622 4.29378     0
#- virginica-versicolor 1.292 1.08822 1.49578     0

# Interpretation:
#
# The Tukey HSD test shows that all pairwise comparisons between species for
# Petal.Length are significant (p < 0.001). Setosa has the shortest Petal.Length,
# followed by versicolor, and virginica has the longest.

# -----------------------------------------------------------------------------
# Linear Discriminant Analysis
# -----------------------------------------------------------------------------

# LDA
lda_fit <- lda(Species ~ Sepal.Length + Petal.Length, data = iris)
lda_fit
#- Call:
#- lda(Species ~ Sepal.Length + Petal.Length, data = iris)
#-
#- Prior probabilities of groups:
#-     setosa versicolor  virginica
#-  0.3333333  0.3333333  0.3333333
#-
#- Group means:
#-            Sepal.Length Petal.Length
#- setosa            5.006        1.462
#- versicolor        5.936        4.260
#- virginica         6.588        5.552
#-
#- Coefficients of linear discriminants:
#-                    LD1        LD2
#- Sepal.Length -1.658896  2.4617343
#- Petal.Length  3.427642 -0.9282016
#-
#- Proportion of trace:
#-    LD1    LD2
#- 0.9987 0.0013

# Interpretation:
#
# The LDA results show that Sepal.Length and Petal.Length effectively discriminate
# between the three species. The first linear discriminant (LD1) accounts for
# 99.87% of the variance, indicating that most of the separation between species
# can be explained along this axis.

lda_scores <- data.frame(
  Species = iris$Species,
  predict(lda_fit)$x
)

ggplot(lda_scores, aes(x = LD1, y = LD2, color = Species)) +
  geom_point(size = 3) +
  labs(title = "LDA Plot: Sepal.Length & Petal.Length by Species")

# Interpretation:
#
# The LDA plot demonstrates excellent discrimination among the three Iris
# species. LD1 clearly separates setosa from the other two species, and also
# differentiates versicolor from virginica. LD2 plays only a minor role,
# consistent with the proportion of trace values.

# =============================================================================
# 3) PERMANOVA
# =============================================================================

# An owner of a winery wants to know whether three grape-growing countries
# (Italy, Portugal, Spain) produce wines that differ in their overall chemical
# composition (pH, alcohol percentage, sulphates, polyphenols, magnesium). Use
# PERMANOVA to test for group differences.

# Create a data frame.
# pH = 3.2, 3.3, 3.1, 3.6, 3.7, 3.5, 3.0, 3.1, 3.2),
# Alcohol % = 12.5, 12.7, 12.4, 13.2, 13.1, 13.4, 11.9, 12.0, 12.1,
# sulphates = 0.7, 0.75, 0.72, 0.65, 0.61, 0.68, 0.8, 0.82, 0.79,
# polyphenols = 2.5, 2.7, 2.4, 3.1, 3.0, 3.2, 2.0, 2.1, 2.3,
# magnesium = 95, 100, 92, 120, 118, 121, 88, 90, 89,
# countries = Italy, Portugal, Spain

wine_data <- data.frame(
  pH = c(3.2, 3.3, 3.1, 3.6, 3.7, 3.5, 3.0, 3.1, 3.2),
  Alcohol = c(12.5, 12.7, 12.4, 13.2, 13.1, 13.4, 11.9, 12.0, 12.1),
  Sulphates = c(0.7, 0.75, 0.72, 0.65, 0.61, 0.68, 0.8, 0.82, 0.79),
  Polyphenols = c(2.5, 2.7, 2.4, 3.1, 3.0, 3.2, 2.0, 2.1, 2.3),
  Magnesium = c(95, 100, 92, 120, 118, 121, 88, 90, 89),
  Country = factor(c(
    "Italy", "Italy", "Italy",
    "Portugal", "Portugal", "Portugal",
    "Spain", "Spain", "Spain"
  ))
)

wine_data

# Prepare the distance matrix using Euclidean distance
wine_dist <- dist(wine_data[, 1:5], method = "euclidean")

# Perform PERMANOVA
set.seed(123)

wine_perm <- adonis2(
  wine_data[, 1:5] ~ Country,
  data = wine_data,
  permutations = 999,
  method = "euclidean"
)

wine_perm
#- Permutation test for adonis under reduced model
#- Permutation: free
#- Number of permutations: 999
#-
#- adonis2(formula = wine_data[, 1:5] ~ Country, data = wine_data, permutations = 999, method = "euclidean")
#-          Df SumOfSqs      R2      F Pr(>F)
#- Model     2  1565.06 0.97531 118.49  0.004 **
#- Residual  6    39.62 0.02469
#- Total     8  1604.68 1.00000
#- ---
#- Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretation:
#
# The PERMANOVA results indicate a significant effect of Country on the overall
# chemical composition of the wines (F(2, 6) = 118.49, p = 0.001). This suggests that
# the wines from Italy, Portugal, and Spain differ significantly in their combined
# chemical profiles.

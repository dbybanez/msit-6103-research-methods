# MSIT 6103 Research Methods
# R Challenge

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: December 13, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- II. Import datasets
# -- 1. Student Performance Datasets (Nabeel 2025)
# ---- 1A. Check collinearity among predictor variables
# ---- 1B. Fit the model and report goodness of fit
# ---- 1C. Diagnostic plot to assess whether the model p-value is reliable
# -- 2. Penguins Dataset (Gorman & Palmer)
# ---- 2A. Identify the strongest positively correlated morphometric variables
# ---- 2B. Linear Discriminant Analysis (LDA)
# ---- 2C. What variable separates Gentoo from the other two species?
# ---- 2D. Hypotheses about penguin species differences

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
if (!requireNamespace("ggcorrplot", quietly = TRUE)) {
  install.packages("ggcorrplot")
}
if (!requireNamespace("Hmisc", quietly = TRUE)) {
  install.packages("Hmisc")
}
if (!requireNamespace("sandwich", quietly = TRUE)) {
  install.packages("sandwich")
}
if (!requireNamespace("lmtest", quietly = TRUE)) {
  install.packages("lmtest")
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
library(stats)
library(ggcorrplot)
library(Hmisc)
library(sandwich)
library(lmtest)
library(corrplot)

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
# II. Import datasets
# =============================================================================

Student_Grades <- read.csv(p("data", "Student_Grades.csv"))
Student_Metrics <- read.csv(p("data", "Student_Metrics.csv"))

str(Student_Grades)
#- 'data.frame':   4999 obs. of  3 variables:
#-  $ student_id : int  1 2 3 4 5 6 7 8 9 10 ...
#-  $ total_score: num  97.9 83.9 100 100 92 97.5 100 96.1 69.8 80.3 ...
#-  $ grade      : chr  "A" "B" "A" "A" ...

summary(Student_Grades)
#-   student_id    total_score        grade
#- Min.   :   1   Min.   : 14.60   Length:4999
#- 1st Qu.:1250   1st Qu.: 74.70   Class :character
#- Median :2500   Median : 88.10   Mode  :character
#- Mean   :2500   Mean   : 84.65
#- 3rd Qu.:3750   3rd Qu.:100.00
#- Max.   :4999   Max.   :100.00

str(Student_Metrics)
#- 'data.frame':   4999 obs. of  3 variables:
#-  $ weekly_self_study_hours: num  18.5 14 19.5 25.7 13.4 13.4 26.1 20.4 11.7 18.8 ...
#-  $ attendance_percentage  : num  95.6 80 86.3 70.2 81.9 65.1 81.8 100 100 67.6 ...
#-  $ class_participation    : num  3.8 2.5 5.3 7 6.9 5 5.9 4 8.2 6 ...

summary(Student_Metrics)
#- weekly_self_study_hours attendance_percentage class_participation
#- Min.   : 0.00           Min.   : 50.00        Min.   : 0.000
#- 1st Qu.:10.40           1st Qu.: 78.20        1st Qu.: 4.600
#- Median :15.10           Median : 84.60        Median : 6.000
#- Mean   :15.08           Mean   : 84.65        Mean   : 5.993
#- 3rd Qu.:19.70           3rd Qu.: 91.70        3rd Qu.: 7.400
#- Max.   :40.00           Max.   :100.00        Max.   :10.000

data(penguins)

# =============================================================================
# 1. Student Performance Datasets (Nabeel 2025)
# =============================================================================

# sanity checks
nrow(Student_Grades) # 4999
nrow(Student_Metrics) # 4999

# Merge datasets
student_df <- bind_cols(Student_Grades, Student_Metrics)

# Check merged dataset
str(student_df)
#- 'data.frame':   4999 obs. of  6 variables:
#-  $ student_id             : int  1 2 3 4 5 6 7 8 9 10 ...
#-  $ total_score            : num  97.9 83.9 100 100 92 97.5 100 96.1 69.8 80.3 ...
#-  $ grade                  : chr  "A" "B" "A" "A" ...
#-  $ weekly_self_study_hours: num  18.5 14 19.5 25.7 13.4 13.4 26.1 20.4 11.7 18.8 ...
#-  $ attendance_percentage  : num  95.6 80 86.3 70.2 81.9 65.1 81.8 100 100 67.6 ...
#-  $ class_participation    : num  3.8 2.5 5.3 7 6.9 5 5.9 4 8.2 6 ...

summary(student_df)
#-    student_id    total_score        grade           weekly_self_study_hours attendance_percentage class_participation
#-  Min.   :   1   Min.   : 14.60   Length:4999        Min.   : 0.00           Min.   : 50.00        Min.   : 0.000
#-  1st Qu.:1250   1st Qu.: 74.70   Class :character   1st Qu.:10.40           1st Qu.: 78.20        1st Qu.: 4.600     
#-  Median :2500   Median : 88.10   Mode  :character   Median :15.10           Median : 84.60        Median : 6.000
#-  Mean   :2500   Mean   : 84.65                      Mean   :15.08           Mean   : 84.65        Mean   : 5.993
#-  3rd Qu.:3750   3rd Qu.:100.00                      3rd Qu.:19.70           3rd Qu.: 91.70        3rd Qu.: 7.400
#-  Max.   :4999   Max.   :100.00                      Max.   :40.00           Max.   :100.00        Max.   :10.000

# -----------------------------------------------------------------------------
# 1A. Check collinearity among predictor variables
# -----------------------------------------------------------------------------

# Correlation matrix (predictors only)
predictors <- student_df %>%
  dplyr::select(
    weekly_self_study_hours,
    attendance_percentage,
    class_participation
  )

cor_mat <- cor(predictors, use = "complete.obs", method = "pearson")
round(cor_mat, 3)
#-                         weekly_self_study_hours attendance_percentage class_participation
#- weekly_self_study_hours                   1.000                -0.019              -0.034
#- attendance_percentage                    -0.019                 1.000              -0.014
#- class_participation                      -0.034                -0.014               1.000

# Interpretation:
#
# Based on the correlation matrix, there are no strong correlations among the 
# predictor variables. All correlation coefficients are close to zero, 
# indicating weak linear relationships between the predictors.

# Check p-values for correlations
rc <- rcorr(as.matrix(predictors)) 
round(rc$r, 3)
rc$P
#-                         weekly_self_study_hours attendance_percentage class_participation
#- weekly_self_study_hours                      NA             0.1774947          0.01534707
#- attendance_percentage                0.17749465                    NA          0.33828082
#- class_participation                  0.01534707             0.3382808                  NA

# Interpretation:
#
# The p-values from the rcorr function indicate that none of the correlations
# among the predictor variables are statistically significant. This further
# supports the conclusion that there is no significant collinearity among the
# predictors.

# Fit linear model to calculate VIF
m1 <- lm(
  total_score ~ weekly_self_study_hours + attendance_percentage + class_participation,
  data = student_df
)
vif(m1)
#- weekly_self_study_hours   attendance_percentage     class_participation 
#-                1.001560                1.000566                1.001379 

# Interpretation:
#
# The Variance Inflation Factor (VIF) values for all predictor variables are
# very close to 1, indicating that there is no multicollinearity among the
# predictors. A VIF value below 5 is generally considered acceptable, so these
# results confirm that collinearity is not a concern in this dataset.

# -----------------------------------------------------------------------------
# 1B. Fit the model and report goodness of fit
# -----------------------------------------------------------------------------

summary(m1)
#- Call:
#- lm(formula = total_score ~ weekly_self_study_hours + attendance_percentage +
#-     class_participation, data = student_df)
#- 
#- Residuals:
#-     Min      1Q  Median      3Q     Max
#- -43.029  -5.777   0.782   6.399  30.067
#- 
#- Coefficients:
#-                         Estimate Std. Error t value Pr(>|t|)    
#- (Intercept)             59.48247    1.26216  47.128   <2e-16 ***
#- weekly_self_study_hours  1.80898    0.01856  97.442   <2e-16 ***
#- attendance_percentage   -0.02160    0.01354  -1.595    0.111
#- class_participation     -0.04811    0.06487  -0.742    0.458
#- ---
#- Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#- 
#- Residual standard error: 9.009 on 4995 degrees of freedom
#- Multiple R-squared:  0.656,     Adjusted R-squared:  0.6558
#- F-statistic:  3175 on 3 and 4995 DF,  p-value: < 2.2e-16

# Interpretation:
#
# The linear regression model indicates that "weekly_self_study_hours" is a
# significant predictor of "total_score" (p < 0.001), while "attendance_percentage"
# and "class_participation" are not statistically significant predictors
# (p > 0.05). The model explains approximately 65.6% of the variance in total scores,
# as indicated by the R-squared value of 0.656.

# Adjusted R-squared: 0.6558
#
# The goodness of fit of the model was assessed using the adjusted coefficient 
# of determination. The model achieved an Adjusted R² of 0.656, indicating that
# approximately 65.6% of the variability in total student performance scores 
# is explained by the included predictors.

# -----------------------------------------------------------------------------
# 1C. Diagnostic plot to assess whether the model p-value is reliable
# -----------------------------------------------------------------------------

# Diagnostic plots
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))

# Save diagnostic plots
png(filename = file.path(plot_dir, "student_performance_diagnostics_dbybanez.png"),
    width = 8, height = 6, units = "in", res = 300)
par(mfrow = c(2, 2))
plot(m1)
par(mfrow = c(1, 1))
dev.off()

# Residuals vs Fitted interpretation:
#
# The Residuals vs Fitted plot shows a clear systematic pattern and 
# non-constant variance, indicating violations of the homoscedasticity and 
# linearity assumptions.

# Normal Q–Q Plot interpretation:
#
# The Q–Q plot indicates approximate normality of residuals in the central 
# range, with deviations in the tails, suggesting mild departures from normality.

# Scale–Location Plot interpretation:
# 
# The Scale–Location plot shows a pronounced pattern, confirming 
# heteroscedasticity and violation of the constant variance assumption.

# Residuals vs Leverage interpretation:
# 
# The Residuals vs Leverage plot shows no highly influential observations, 
# indicating that the model fit is not driven by extreme cases.

# OVERALL model diagnostics interpretation:
#
# Diagnostic plots reveal violations of the homoscedasticity and linearity 
# assumptions, particularly at higher fitted values. While residuals are 
# approximately normal and no influential observations are present, 
# heteroscedasticity suggests that standard errors and p-values may be 
# underestimated. However, given the large sample size, the overall model 
# significance remains meaningful, though it should be interpreted with caution.

# Additional robust standard errors using sandwich package
coeftest(m1, vcov = vcovHC(m1, type = "HC3"))
#- t test of coefficients:
#- 
#-                          Estimate Std. Error t value Pr(>|t|)
#- (Intercept)             59.482468   1.295890 45.9009   <2e-16 ***
#- weekly_self_study_hours  1.808981   0.021785 83.0363   <2e-16 ***
#- attendance_percentage   -0.021603   0.013846 -1.5602   0.1188
#- class_participation     -0.048110   0.065267 -0.7371   0.4611
#- ---
#- Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretation:
#
# The robust standard errors confirm that "weekly_self_study_hours" remains a
# significant predictor of "total_score" (p < 0.001), while "attendance_percentage"
# and "class_participation" are still not statistically significant predictors
# (p > 0.05). This suggests that despite the violations of homoscedasticity, the main findings
# regarding predictor significance are robust.

# =============================================================================
# 2. Penguins Dataset (Gorman & Palmer)
# =============================================================================

str(penguins)
#- 'data.frame':   344 obs. of  8 variables:
#-  $ species    : Factor w/ 3 levels "Adelie","Chinstrap",..: 1 1 1 1 1 1 1 1 1 1 ...
#-  $ island     : Factor w/ 3 levels "Biscoe","Dream",..: 3 3 3 3 3 3 3 3 3 3 ...
#-  $ bill_len   : num  39.1 39.5 40.3 NA 36.7 39.3 38.9 39.2 34.1 42 ...
#-  $ bill_dep   : num  18.7 17.4 18 NA 19.3 20.6 17.8 19.6 18.1 20.2 ...
#-  $ flipper_len: int  181 186 195 NA 193 190 181 195 193 190 ...
#-  $ body_mass  : int  3750 3800 3250 NA 3450 3650 3625 4675 3475 4250 ...
#-  $ sex        : Factor w/ 2 levels "female","male": 2 1 1 NA 1 2 1 2 NA NA ...
#-  $ year       : int  2007 2007 2007 2007 2007 2007 2007 2007 2007 2007 ...

summary(penguins)
#-       species          island       bill_len        bill_dep      flipper_len      body_mass        sex           year     
#-  Adelie   :152   Biscoe   :168   Min.   :32.10   Min.   :13.10   Min.   :172.0   Min.   :2700   female:165   Min.   :2007  
#-  Chinstrap: 68   Dream    :124   1st Qu.:39.23   1st Qu.:15.60   1st Qu.:190.0   1st Qu.:3550   male  :168   1st Qu.:2007  
#-  Gentoo   :124   Torgersen: 52   Median :44.45   Median :17.30   Median :197.0   Median :4050   NA's  : 11   Median :2008  
#-                                  Mean   :43.92   Mean   :17.15   Mean   :200.9   Mean   :4202                Mean   :2008  
#-                                  3rd Qu.:48.50   3rd Qu.:18.70   3rd Qu.:213.0   3rd Qu.:4750                3rd Qu.:2009  
#-                                  Max.   :59.60   Max.   :21.50   Max.   :231.0   Max.   :6300                Max.   :2009  
#-                                  NA's   :2       NA's   :2       NA's   :2       NA's   :2

# Clean dataset by removing rows with missing values in morphometric columns
morph_cols <- c("bill_len", "bill_dep", "flipper_len", "body_mass")
peng_clean <- penguins %>%
  filter(if_all(all_of(morph_cols), ~ !is.na(.)))

# Compare row counts
nrow(penguins) # 344
nrow(peng_clean) # 342

# Confirm no NA remains in morphometric columns
colSums(is.na(peng_clean[morph_cols]))
#-   bill_len    bill_dep flipper_len   body_mass
#-          0           0           0           0 

# -----------------------------------------------------------------------------
# 2A. Identify the strongest positively correlated morphometric variables
# -----------------------------------------------------------------------------

# Compute pairwise correlations + p-values

# Select morphometric variables only
morph_data <- peng_clean %>%
  dplyr::select(all_of(morph_cols))

# Generate all variable pairs
pairs <- combn(morph_cols, 2, simplify = FALSE)

# Compute correlations and p-values
cor_results <- purrr::map_dfr(pairs, function(v) {
  test <- cor.test(
    morph_data[[v[1]]],
    morph_data[[v[2]]],
    method = "pearson"
  )
  tibble(
    var1 = v[1],
    var2 = v[2],
    r = unname(test$estimate),
    p_value = test$p.value
  )
})

# View results sorted by strongest positive correlation
cor_results %>%
  arrange(desc(r))
#- # A tibble: 6 x 4
#-   var1        var2             r   p_value
#-   <chr>       <chr>        <dbl>     <dbl>
#- 1 flipper_len body_mass    0.871 4.37e-107
#- 2 bill_len    flipper_len  0.656 1.74e- 43
#- 3 bill_len    body_mass    0.595 3.81e- 34
#- 4 bill_len    bill_dep    -0.235 1.12e-  5
#- 5 bill_dep    body_mass   -0.472 2.28e- 20
#- 6 bill_dep    flipper_len -0.584 1.23e- 32

cor_matrix <- cor(morph_data, method = "pearson")

corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.9
)
# Save correlation plot
png(filename = file.path(plot_dir, "penguin_morph_corrplot_dbybanez.png"),
    width = 6, height = 5, units = "in", res = 300)
corrplot(
  cor_matrix,
  method = "number",
  type = "upper",
  tl.col = "black",
  tl.srt = 45,
  number.cex = 0.9
)
dev.off()

# Interpretation:
#
# The strongest positive correlation among the morphometric variables is
# between "flipper_len" and "body_mass" with a Pearson correlation coefficient
# of approximately 0.871 (p < 0.001). This indicates a strong positive linear
# relationship between flipper length and body mass in penguins. Which means,
# penguins with longer flippers tend to have higher body mass.

# -----------------------------------------------------------------------------
# 2B. Linear Discriminant Analysis (LDA)
# -----------------------------------------------------------------------------

# Fit LDA model
lda_fit <- lda(
  species ~ bill_len + bill_dep + flipper_len + body_mass,
  data = peng_clean
)

lda_fit
#- Call:
#- lda(species ~ bill_len + bill_dep + flipper_len + body_mass, 
#-     data = peng_clean)
#- 
#- Prior probabilities of groups:
#-    Adelie Chinstrap    Gentoo 
#- 0.4415205 0.1988304 0.3596491 
#- 
#- Group means:
#-           bill_len bill_dep flipper_len body_mass
#- Adelie    38.79139 18.34636    189.9536  3700.662
#- Chinstrap 48.83382 18.42059    195.8235  3733.088
#- Gentoo    47.50488 14.98211    217.1870  5076.016
#- 
#- Coefficients of linear discriminants:
#-                     LD1          LD2
#- bill_len    -0.08832666 -0.417870885
#- bill_dep     1.03730494 -0.021004854
#- flipper_len -0.08616282  0.013474680
#- body_mass   -0.00129952  0.001711436
#- 
#- Proportion of trace:
#-   LD1   LD2
#- 0.866 0.134

# Interpretation:
#
# The LDA model was fitted to classify penguin species based on their
# morphometric measurements. The model identified two linear discriminants (LD1
# and LD2) that explain approximately 86.6% and 13.4% of the variance,
# respectively. The coefficients indicate how each morphometric variable
# contributes to the discriminant functions, with "bill_dep" having a strong
# positive contribution to LD1.

# Prepare data for plotting
lda_scores <- predict(lda_fit)$x %>% as.data.frame()

lda_plot_df <- bind_cols(
  peng_clean %>% dplyr::select(species),
  lda_scores
)

# Recreate LDA plot (white background + my name)
ggplot(lda_plot_df, aes(x = LD1, y = LD2, color = species)) +
  geom_point(alpha = 0.8, size = 2) +
  theme_bw() +
  labs(
    title = "Linear Discriminant Analysis of Penguin Species by David Benjamin Ybañez",
    x = "Linear Discriminant 1",
    y = "Linear Discriminant 2",
    color = "Species"
  )
# Save plot
png(filename = file.path(plot_dir, "penguin_lda_plot_dbybanez.png"),
    width = 8, height = 6, units = "in", res = 300)
ggplot(lda_plot_df, aes(x = LD1, y = LD2, color = species)) +
  geom_point(alpha = 0.8, size = 2) +
  theme_bw() +
  labs(
    title = "Linear Discriminant Analysis of Penguin Species by David Benjamin Ybañez",
    x = "Linear Discriminant 1",
    y = "Linear Discriminant 2",
    color = "Species"
  )
dev.off()

# LDA plot interpretation:
#
# The LDA plot shows clear separation among the three penguin species, with 
# Gentoo forming a distinct cluster primarily along the first linear 
# discriminant (LD1). Adelie and Chinstrap show partial overlap but remain 
# separable along the second discriminant (LD2). Overall, the morphometric 
# variables provide strong discriminatory power between species.

# -----------------------------------------------------------------------------
# 2C. What variable separates Gentoo from the other two species? 
# -----------------------------------------------------------------------------

# Key evidence from lda_fit
# Group means (Gentoo vs others)
# - Flipper length:
#   - Gentoo ≈ 217 mm
#   - Adelie ≈ 190 mm
#   - Chinstrap ≈ 196 mm
# - Body mass:
#   - Gentoo ≈ 5076 g
#   - Adelie ≈ 3701 g
#   - Chinstrap ≈ 3733 g
# 
# Correlation result (from 2A)
# - flipper_len <-> body_mass: r = 0.871 (very strong)
# 
# LDA structure
# - LD1 explains most separation (86.6%)
# - Gentoo is separated mainly along LD1
# - Size-related variables dominate Gentoo separation

# Flipper length is the primary variable separating Gentoo penguins from Adelie
# and Chinstrap. Gentoo penguins have substantially longer flippers, which is 
# strongly associated with higher body mass and drives their separation along 
# the first discriminant axis.
#
# This is supported by the LDA coefficients, where flipper length has a
# significant negative coefficient for LD1, indicating that as flipper length
# increases, the likelihood of being classified as Gentoo also increases.
# 
# Body mass reinforces this separation but is closely coupled with flipper length.

# -----------------------------------------------------------------------------
# 2D. Hypotheses about penguin species differences
# -----------------------------------------------------------------------------

# Hypothesis 1:
# Gentoo penguins have significantly greater flipper length and body mass than 
# Adelie and Chinstrap penguins, indicating species-level differences in body 
# size related to locomotion and foraging behavior.

# Hypothesis 2:
# Flipper length and body mass scale positively across penguin species; however, 
# Gentoo penguins occupy a higher position along this scaling relationship 
# compared to Adelie and Chinstrap.

# Optional hypothesis:
# Differences in bill dimensions between Adelie and Chinstrap penguins may 
# reflect species-level variation in feeding morphology, contributing to their 
# partial separation in discriminant space.

# Rationale for proposed hypotheses
#
# The proposed hypotheses are based on the strong positive correlation between
# flipper length and body mass and the clear separation of Gentoo penguins along
# the first linear discriminant in the LDA results.
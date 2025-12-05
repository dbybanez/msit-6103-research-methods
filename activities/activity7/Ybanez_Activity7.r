# MSIT 6103 Research Methods
# Activity 7

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: November 7, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages
# -- 1) Speed Limits and Traffic Deaths
# -- 2) Trees dataset (correlation between Height and Volume)
# -- 3) Telomere Inheritance

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
# 1) ANCOVA: mole-rat energy vs caste, adjusting for body mass
# =============================================================================

# Analyze a factor while adjusting for a covariate, comparing energy
# expenditure of two castes of naked mole-rat while adjusting for differences
# in body mass using analysis of covariance ANCOVA. Import MoleRatLayabouts.csv
# and inspect it.

# Packages
library(tidyverse)
library(car) # for Type III Anova()
library(broom) # for augment() diagnostics (used later)

# 1) Import & inspect
moles <- read.csv(p("data", "MoleRatLayabouts.csv"), stringsAsFactors = FALSE)
str(moles)
glimpse(moles)
names(moles)

# (1.a) Create a scatter plot.

ggplot(moles, aes(x = lnMass, y = lnEnergy, color = caste)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(
    title = "Energy Expenditure vs Body Mass by Caste",
    x = "Body mass (lnMass)",
    y = "Energy expenditure (lnEnergy)"
  ) +
  theme_classic()

# Interpretation:
# The scatter plot indicates that energy expenditure increases with body mass
# for both lazy and worker mole-rats. However, workers exhibit consistently
# higher energy expenditure at any given body mass, suggesting a
# caste-related difference in metabolic activity. The roughly parallel
# regression lines imply that both castes share a similar rate of increase (slope),
# differing mainly in their baseline energy expenditure (intercept).

# (1.b) Fit models to the data, beginning with the model lacking an interaction
# term. Use lm() because caste and mass are fixed effects. Save the predicted
# values in the data frame.

fit0 <- lm(lnEnergy ~ lnMass + caste, data = moles)
moles$fit0 <- predict(fit0)
summary(fit0)

# Residuals:
# Min       1Q   Median       3Q      Max
# -0.73388 -0.19371  0.01317  0.17578  0.47673
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept) -0.09687    0.94230  -0.103   0.9188
# lnMass       0.89282    0.19303   4.625 5.89e-05 ***
# casteworker  0.39334    0.14611   2.692   0.0112 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.2966 on 32 degrees of freedom
# Multiple R-squared:  0.409,     Adjusted R-squared:  0.3721
# F-statistic: 11.07 on 2 and 32 DF,  p-value: 0.0002213

# Visualize the reduced model fit (optional)
ggplot(moles, aes(x = lnMass, y = lnEnergy, color = caste)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(y = fit0), linetype = "dashed", linewidth = 0.9) +
  theme_classic()

# Interpretation:
#
# - Body mass (lnMass) is a strong and significant predictor of energy
# expenditure, heavier mole-rats use more energy.
# - Caste also has a significant effect even after accounting for body size:
# workers expend more energy than lazy individuals of the same mass.
# - Because this reduced model assumes no interaction, we are interpreting
# the slopes for lnMass as parallel, the two groups increase energy at roughly
# the same rate with increasing body mass, differing only in their intercepts
# (baseline energy levels).

# Conclusion:
#
# The reduced ANCOVA model without an interaction term indicates that both body
# mass and caste significantly influence energy expenditure in naked mole-rats.
# Larger body mass is associated with higher energy expenditure, and workers
# exhibit higher adjusted energy levels than lazy individuals. The approximately
# parallel regression lines suggest that the rate at which energy expenditure
# increases with body mass is similar across castes, differing mainly in overall
# level rather than slope.

# (1.c) Fit the full and include the interaction term and visualize.
fit1 <- lm(lnEnergy ~ lnMass * caste, data = moles) # includes lnMass:caste
moles$fit1 <- predict(fit1)
summary(fit1)
# Residuals:
# Min       1Q   Median       3Q      Max
# -0.72004 -0.17990  0.05631  0.19551  0.43128
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)          1.2939     1.6691   0.775   0.4441
# lnMass               0.6069     0.3428   1.771   0.0865 .
# casteworker         -1.5713     1.9518  -0.805   0.4269
# lnMass:casteworker   0.4186     0.4147   1.009   0.3206
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 0.2965 on 31 degrees of freedom
# Multiple R-squared:  0.4278,    Adjusted R-squared:  0.3725
# F-statistic: 7.727 on 3 and 31 DF,  p-value: 0.0005391

# Visualize full model fitted curves per group
ggplot(moles, aes(x = lnMass, y = lnEnergy, color = caste)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(y = fit1), linetype = "dashed", linewidth = 0.9) +
  theme_classic()

# Interpretation:
#
# Including the interaction term allows each caste to have its own slope for the
# relationship between body mass and energy expenditure.
#
# Although the worker line appears slightly steeper, the interaction term (lnMass:casteworker)
# is not statistically significant (p = 0.32). This means
# the difference in slopes is likely due to random variation rather than a
# true difference in how body mass affects energy expenditure between castes.
#
# Both castes follow the same general pattern: as body mass increases, energy
# expenditure increases, but workers tend to maintain higher energy levels overall.

# (1.d) Compare the models with and without the interaction term using anova
# Determine whether the full model, with an interaction term, is a significantly
# better fit than the reduced model, which does not include the interaction term.

anova(fit0, fit1)

# Analysis of Variance Table
#
# Model 1: lnEnergy ~ lnMass + caste
# Model 2: lnEnergy ~ lnMass * caste
# Res.Df    RSS Df Sum of Sq      F Pr(>F)
# 1     32 2.8145
# 2     31 2.7249  1  0.089557 1.0188 0.3206

# Interpretation:
#
# The ANOVA comparison indicates that adding the interaction term between
# body mass and caste does not significantly improve the model fit (p = 0.3206).
# This suggests that the relationship between body mass and energy expenditure
# is similar for both castes, supporting the conclusion from the reduced model.
# Therefore, the simpler model without the interaction is preferred for
# parsimony.

# (1.e) Test different intercepts (equal slopes) with Type III sums of squares
# Use sum-to-zero contrasts for the factor, then car::Anova(type = "III")
fit_type3 <- lm(lnEnergy ~ lnMass + caste,
  data = moles,
  contrasts = list(caste = contr.sum)
)
Anova(fit_type3, type = "III")

# Anova Table (Type III tests)
#
# Response: lnEnergy
# Sum Sq Df F value    Pr(>F)
# (Intercept) 0.00111  1  0.0126    0.9112
# lnMass      1.88152  1 21.3923 5.887e-05 ***
# caste       0.63747  1  7.2478    0.0112 *
# Residuals   2.81450 32
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# Interpretation:
#
# The Type III ANOVA results confirm that body mass (lnMass) is a highly
# significant predictor of energy expenditure (p < 0.001). Additionally,
# caste also has a significant effect (p = 0.0112), indicating that even
# after adjusting for body mass, workers expend more energy than lazy individuals.

# Conclusion:
#
# The ANCOVA analysis reveals that both body mass and caste significantly
# influence energy expenditure in naked mole-rats. The lack of a significant
# interaction term suggests that the effect of body mass on energy expenditure
# is consistent across castes, with workers maintaining higher energy levels
# overall.

# (1.f) Create a residual plot and normal quantile plot of residuals.
par(mfrow = c(1, 2))
plot(residuals(fit1) ~ fitted(fit1))
abline(h = 0, lty = 2) # residual vs fitted
qqnorm(residuals(fit1), pch = 16)
qqline(residuals(fit1)) # normal Q-Q
par(mfrow = c(1, 1))

# Interpretation:
#
# The residual versus fitted plot shows that the residuals are randomly
# distributed around zero, with no clear pattern or funnel shape. This
# indicates that the assumptions of linearity and constant variance (homoscedasticity)
# are satisfied. The normal Q-Q plot shows that most points fall closely along
# the diagonal line, suggesting that the residuals are approximately normally
# distributed. Minor deviations at the tails are acceptable and do not indicate
# serious departures from normality.

# Conclusion:
#
# The diagnostic plots confirm that the ANCOVA model meets the assumptions of
# linearity, homoscedasticity, and normality. Therefore, the model provides a
# reliable fit for analyzing the relationship between body mass and energy
# expenditure across the two mole-rat castes.

# =============================================================================
# 2) Mammals: brain vs body size
# =============================================================================

# Larger animals tend to have larger brains. But is the increase in brain size
# proportional to the increase in body size? A set of data on body and brain
# size of 62 mammal species was collated by Allison and Cicchetti (1976), and
# these data are in the data set “mammals.csv”. The fi le contains columns
# giving the species name, the average body mass (in kg) and average brain
# size (in g) for each species.

library(tidyverse)

mammals <- read.csv(p("data", "mammals.csv"), stringsAsFactors = FALSE)
glimpse(mammals) # columns: name, body_mass_kg, brain_mass_g

# (2.a) Plot brain size against body size. Is the relationship linear?

ggplot(mammals, aes(x = body_mass_kg, y = brain_mass_g)) +
  geom_point() +
  labs(x = "Body mass (kg)", y = "Brain size (g)") +
  theme_classic()

# Interpretation:
#
# The scatter plot displays brain size (g) against body mass (kg) for 62 mammal species.
# The relationship is not linear in the raw data — most points are clustered near
# the origin (small-bodied species), while a few large-bodied species (such as
# whales and elephants) extend far to the right with disproportionately large body masses.
#
# This creates a curved or exponential pattern, where brain size increases with body
# size, but not at a constant rate. The points suggest that the rate of increase in brain
# size slows down as body mass becomes very large — in other words, larger animals have
# bigger brains, but the increase is less than proportional to body mass.

# (2.b) Find a transformation (for either or both variables) that makes the
# relationship between these two variables linear.

mammals <- mammals %>%
  mutate(
    log_body  = log(body_mass_kg),
    log_brain = log(brain_mass_g)
  )

ggplot(mammals, aes(x = log_body, y = log_brain)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "log(Body mass)", y = "log(Brain size)") +
  theme_classic()

# Interpretation:
#
# The logarithmic transformation of both body mass and brain size successfully linearized
# the relationship between the two variables. This transformation reveals a strong linear
# association on the log scale, confirming that brain size scales predictably with body mass
# across mammal species according to an allometric (power-law) relationship.

# (2.c) Is there statistical evidence that brain size is correlated with body
# size? Assume that the species data are independent.

cor.test(mammals$log_body, mammals$log_brain)


# (2.d) What line best predicts (transformed) brain size from (transformed) body size?
mod_mam <- lm(log_brain ~ log_body, data = mammals)
summary(mod_mam)

# (2.e) Based on your answer in (2.d), what is the predicted change in
# log-brain size accompanying an increase of 3 units of log- body size?

coef(mod_mam)["log_body"] * 3
# (Multiply slope by 3. If you want the multiplicative change on original scale: exp(slope*3).)

# Interpretation:

# (2.f) Make a residual plot using the regression fitted to the transformed variables.
# Do the data look like they match the assumptions of linear regression?

par(mfrow = c(1, 2))
plot(residuals(mod_mam) ~ fitted(mod_mam))
abline(h = 0, lty = 2)
qqnorm(residuals(mod_mam), pch = 16)
qqline(residuals(mod_mam))
par(mfrow = c(1, 1))

# Interpretation:

# (2.g) [OPTIONAL – hint: try using filter() from the dplyr package.] Which species
# has the highest brain size relative to that predicted by its body size? Which
# species has the smallest brain relative to that predicted by its body size?

mammals <- mammals %>%
  mutate(
    pred_brain = exp(predict(mod_mam)),
    res = brain_mass_g - pred_brain
  )

mammals %>%
  filter(res == max(res)) %>%
  select(name, brain_mass_g, pred_brain, res)

mammals %>%
  filter(res == min(res)) %>%
  select(name, brain_mass_g, pred_brain, res)

# Interpretation:

# =============================================================================
# 3) ToothGrowth: dose & delivery method
# =============================================================================

# Using the ToothGrowth built-in dataset describing tooth growth in guinea pigs
# under different vitamin C treatments.

library(tidyverse)
data("ToothGrowth", package = "datasets")

summary(ToothGrowth)
# len        supp         dose
# Min.   : 4.20   OJ:30   Min.   :0.500
# 1st Qu.:13.07   VC:30   1st Qu.:0.500
# Median :19.25           Median :1.000
# Mean   :18.81           Mean   :1.167
# 3rd Qu.:25.27           3rd Qu.:2.000
# Max.   :33.90           Max.   :2.000

df <- ToothGrowth %>%
  mutate(
    supp = factor(supp), # OJ vs VC
    dose = as.numeric(dose)
  )

# 3.a) Are higher doses of vitamin C beneficial for tooth growth?

mod_tg <- lm(len ~ dose, data = df)
summary(mod_tg)

# Residuals:
# Min      1Q  Median      3Q     Max
# -8.4496 -2.7406 -0.7452  2.8344 10.1139
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)   7.4225     1.2601    5.89 2.06e-07 ***
# dose          9.7636     0.9525   10.25 1.23e-14 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# Residual standard error: 4.601 on 58 degrees of freedom
# Multiple R-squared:  0.6443,    Adjusted R-squared:  0.6382
# F-statistic: 105.1 on 1 and 58 DF,  p-value: 1.233e-14

# Interpretation:
# Yes, the analysis indicates that higher doses of vitamin C are beneficial for tooth growth.
# The positive relationship between dose and tooth length suggests that as the dose
# of vitamin C increases, tooth length also increases significantly.
# The positive coefficient for dose (9.7636) indicates that for each additional
# milligram of vitamin C, tooth length increases by approximately 9.76 units on average.
# The p-value (< 0.001) suggests this effect is statistically significant.

# 3.b) Does the method of administration (orange juice, OJ, or ascorbic acid, VC)
# influence the effect of the dose?

mod_tg_int <- lm(len ~ dose * supp, data = df)
anova(mod_tg, mod_tg_int) # test if adding interaction improves fit
# Analysis of Variance Table
# Model 1: len ~ dose
# Model 2: len ~ dose * supp
# Res.Df     RSS Df Sum of Sq      F   Pr(>F)
# 1     58 1227.91
# 2     56  933.63  2    294.27 8.8253 0.000466 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

summary(mod_tg_int)
# Residuals:
# Min      1Q  Median      3Q     Max
# -8.2264 -2.8462  0.0504  2.2893  7.9386
#
# Coefficients:
# Estimate Std. Error t value Pr(>|t|)
# (Intercept)   11.550      1.581   7.304 1.09e-09 ***
# dose           7.811      1.195   6.534 2.03e-08 ***
# suppVC        -8.255      2.236  -3.691 0.000507 ***
# dose:suppVC    3.904      1.691   2.309 0.024631 *
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
#
# Residual standard error: 4.083 on 56 degrees of freedom
# Multiple R-squared:  0.7296,    Adjusted R-squared:  0.7151
# F-statistic: 50.36 on 3 and 56 DF,  p-value: 6.521e-16

# Interpretation:

# Yes, the interaction between dose and supplement type is statistically significant,
# indicating that the effect of dose on tooth length varies depending on whether
# the vitamin C is administered as OJ or VC.

# The significant interaction term (dose:suppVC, p = 0.0246) indicates that
# the effect of dose on tooth length differs between the two supplement types.
# Specifically, the positive coefficient (3.904) suggests that the increase
# in tooth length per unit dose is greater when vitamin C is delivered as VC
# compared to OJ.

# 3.c) What would be the predicted tooth length of a guinea pig given 1 mg of
# vitamin C as ascorbic acid?

newdat <- data.frame(dose = 1, supp = factor("VC", levels = levels(df$supp)))
predict(mod_tg_int, newdat, interval = "prediction")
# fit      lwr      upr
# 1 15.01071 6.686416 23.33501

# Interpretation:
# The predicted tooth length is approximately 15.01 units, with a 95% prediction interval
# ranging from 6.69 to 23.34 units. This suggests that we can be 95% confident that the
# true tooth length for a guinea pig receiving 1 mg of vitamin C as ascorbic acid falls
# within this interval.

# =============================================================================
# 4) ilri.sheep: is weaning weight a function of age at weaning?
# =============================================================================

# Use the ilri.sheep dataset, also from the agridat package, to answer the
# question: Is the weight of lambs at weaning a function of their age at weaning?,
# with the hypothesis that lambs that are weaned later are also heavier.
# Test all assumptions and visualize the relationship.

library(agridat)
library(tidyverse)
library(broom)

data(ilri.sheep)
# Inspect; the dataset includes lamb traits. We'll assume columns named like:
# weanwt (weaning weight), weanage (age at weaning), etc.
glimpse(ilri.sheep)

# Keep relevant columns & drop missings
sheep <- ilri.sheep %>%
  select(weanwt, weanage) %>%
  drop_na()

# Visual check
ggplot(sheep, aes(x = weanage, y = weanwt)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Age at weaning", y = "Weaning weight") +
  theme_classic()

# Fit simple regression
m_sheep <- lm(weanwt ~ weanage, data = sheep)
summary(m_sheep)

# Assumption checks (residuals vs fitted; normal Q-Q)
par(mfrow = c(1, 2))
plot(residuals(m_sheep) ~ fitted(m_sheep))
abline(h = 0, lty = 2)
qqnorm(residuals(m_sheep), pch = 16)
qqline(residuals(m_sheep))
par(mfrow = c(1, 1))



# End of Activity 7

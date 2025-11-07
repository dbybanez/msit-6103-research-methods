# MSIT 6103 Research Methods
# Activity 7 (Practice - Regression and Linear Models)

# By: David Ybanez (MSIT 2) University of San Carlos
# Date: October 18, 2025

# Contents include:
# -- I. Initial setup for working directory and load packages

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


# ------ 1. Linear Regression ---------------------------------

# Practice

# Compare the fits of the null and univariate regression models to data on the relationship between stability of plant biomass production and the initial number of plant species assigned to plots.

# Load data
prairie <- read.csv(p("data", "PlantDiversityAndStability.csv"), stringsAsFactors = FALSE)
head(prairie)

library(ggplot2)

prairie$logStability <- log(prairie$biomassStability)

# Fit reduced model
prairieNullModel <- lm(logStability ~ 1, data = prairie)

ggplot(prairie, aes(nSpecies, logStability)) +
  geom_point(size = 3, shape = 1, col = "firebrick") +
  geom_smooth(method = "lm", formula = y ~ 1, se = FALSE, col = "black") +
  xlim(0, 16) +
  labs(x = "Species number treatment", y = "Log-transformed ecosystem stability") +
  theme_classic()

# Fit full model

prairieRegression <- lm(logStability ~ nSpecies, data = prairie)
ggplot(prairie, aes(nSpecies, logStability)) +
  geom_point(size = 3, shape = 1, col = "firebrick") +
  geom_smooth(method = "lm", se = FALSE, col = "black") +
  xlim(0, 16) +
  labs(x = "Species number treatment", y = "Log-transformed ecosystem stability") +
  theme_classic()

# F-test of improvement in fit

anova(prairieRegression)

# ------ 2. Ordinary Least Squares (OLS) Regression ---------------------------------

predictor_variable <- c(1, 2, 3, 4, 5)
response_variable <- c(2, 4, 5, 4, 5)

ols_regression_model <- lm(response_variable ~ predictor_variable)

summary(ols_regression_model)


# ------ 3. One-way ANCOVA-----------------------------------------------------------
install.packages("ggpubr")
install.packages("rstatix")
install.packages("broom")
library(tidyverse)
library(ggpubr)
library(rstatix)
library(broom)

install.packages("datarium")
library(datarium)

data("anxiety", package = "datarium")
anxiety <- anxiety %>%
  select(id, group, t1, t3) %>%
  rename(pretest = t1, posttest = t3)
anxiety[14, "posttest"] <- 19

set.seed(123)
anxiety %>% sample_n_by(group, size = 1)

library(ggpubr)
ggscatter(anxiety, x = "pretest", y = "posttest", color = "group", add = "reg.line") + stat_regline_equation(aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~~"), color = group))

anxiety %>% anova_test(posttest ~ group * pretest)

model <- lm(posttest ~ pretest + group, data = anxiety)
model.metrics <- augment(model, se_fit = TRUE) %>% select(-.hat, -.sigma, -.fitted, -.se.fit)

head(model.metrics, 3)

shapiro_test(model.metrics$.resid)

model.metrics %>%
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

model.metrics %>% levene_test(.resid ~ group)

library(rstatix) # Run the ANCOVA
res.aov <- anxiety %>% anova_test(posttest ~ pretest + group)

# View ANOVA table
get_anova_table(res.aov)

install.packages("emmeans")
library(emmeans)
library(rstatix)

pwc <- anxiety %>% emmeans_test(posttest ~ group, # Group comparison
  covariate = pretest, # Covariate
  p.adjust.method = "bonferroni"
)

pwc

get_emmeans(pwc)

pwc <- pwc %>% add_xy_position(x = "group", fun = "mean_se")
ggline(get_emmeans(pwc), x = "group", y = "emmean") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  stat_pvalue_manual(pwc, hide.ns = TRUE, tip.length = FALSE) +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption = get_pwc_label(pwc)
  )

# ------ 4. Two-way ANCOVA-----------------------------------------------------------
library(tidyverse)
library(rstatix)
library(dplyr)
library(broom)
library(ggpubr)

data("stress", package = "datarium")
stress %>% sample_n_by(treatment, exercise)

stress %>% anova_test(score ~ age + treatment + exercise + treatment * exercise + age * treatment + age * exercise + age * exercise * treatment)

model <- lm(score ~ age + treatment * exercise, data = stress)
model
model.metrics <- augment(model, se_fit = TRUE) %>% select(-.hat, -.sigma, -.fitted, -.se.fit)

head(model.metrics, 3)

shapiro_test(model.metrics$.resid)

levene_test(.resid ~ treatment * exercise, data = model.metrics)

model.metrics %>%
  filter(abs(.std.resid) > 3) %>%
  as.data.frame()

res.aov <- stress %>% anova_test(score ~ age + treatment * exercise)
get_anova_table(res.aov)

stress %>%
  group_by(exercise) %>%
  anova_test(score ~ age + treatment)

pwc <- stress %>%
  group_by(exercise) %>%
  emmeans_test(
    score ~ treatment,
    covariate = age,
    p.adjust.method = "bonferroni"
  )

pwc %>% filter(exercise == "high")

stress %>%
  group_by(treatment) %>%
  anova_test(score ~ age + exercise)

pwc2 <- stress %>%
  group_by(treatment) %>%
  emmeans_test(score ~ exercise, covariate = age, p.adjust.method = "bonferroni") %>%
  select(-df, -statistic, -p)

pwc2 %>% filter(treatment == "yes")

emm <- get_emmeans(pwc)

lp <- ggline(
  get_emmeans(pwc),
  x = "exercise", y = "emmean",
  color = "treatment", palette = "jco"
) + geom_errorbar(
  data = emm,
  aes(ymin = conf.low, ymax = conf.high, color = treatment),
  width = 0.1
)

pwc_xy <- pwc %>% add_xy_position(
  x = "exercise", group = "treatment",
  step.increase = 0.2
)

ann <- pwc_xy %>% dplyr::filter(exercise == "high", p.adj <= 0.05)

lp +
  {
    if (nrow(ann) > 0) {
      stat_pvalue_manual(ann,
        hide.ns = TRUE,
        tip.length = 0, bracket.size = 0
      )
    } else {
      NULL
    }
  } +
  labs(
    subtitle = get_test_label(res.aov, detailed = TRUE),
    caption  = get_pwc_label(pwc)
  )


# End of Activity 7 (Practice)

# ------ X. Sample run ---------------------- ---------------------------------
n <- 15
a <- 0
b <- 1.5
sigma2 <- 25
x <- 1:15
set.seed(15)
eps <- rnorm(n, mean = 0, sd = sqrt(sigma2))
y <- a + b * x + eps

model5 <- lm(formula = y ~ x)
summary(model5)

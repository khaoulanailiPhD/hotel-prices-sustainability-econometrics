
# ============================================================
# EMPIRICAL ANALYSIS: HOTEL PRICES AND SUSTAINABILITY
# ============================================================
# Research project:
# The Role of Sustainability Certifications and Customer
# Reviews in Hotel Prices
#
# Objective:
# Analyse the relationship between sustainability certification,
# customer review topics and hotel prices.
#
# Methods:
# - Data cleaning and sample construction
# - Multiple linear regression (OLS)
# - Interaction effects and heterogeneous effects
# - Average marginal effects (AME)
# - Bootstrap inference and confidence intervals
# - Robust standard errors
#
# Author: Khaoula Naili
# ============================================================


# ============================================================
# 01. ENVIRONMENT AND PACKAGES
# ============================================================

required_packages <- c(
  "dplyr",
  "readxl",
  "readr",
  "boot",
  "sandwich",
  "lmtest",
  "stargazer",
  "car"
)

invisible(lapply(required_packages, function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    install.packages(pkg)
  }
  library(pkg, character.only = TRUE)
}))

set.seed(12345)

# Project directories
output_dir <- "empirical_results"

if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}


# ============================================================
# 02. DATA IMPORT AND PREPARATION
# ============================================================

data_path <- paste0(
  "data.xlsx"
)

data <- read_excel(data_path)

# Initial inspection
dim(data)
names(data)
str(data)


# ------------------------------------------------------------
# 2.1. Price cleaning and dependent variable construction
# ------------------------------------------------------------

# Assumption: prices use a decimal point and commas, if present,
# are thousands separators. Adjust the locale if necessary.

data$discounted_price <- readr::parse_number(
  as.character(data$discounted_price),
  locale = readr::locale(
    decimal_mark = ".",
    grouping_mark = ","
  )
)

# Remove invalid and non-positive prices
data <- data %>%
  filter(
    !is.na(discounted_price),
    discounted_price > 0
  )

# Logarithmic transformation
data <- data %>%
  mutate(
    log_Price = log(discounted_price)
  )


# ------------------------------------------------------------
# 2.2. Sample selection
# ------------------------------------------------------------

data <- data %>%
  filter(
    experience_count >= 150,
    Sample_Size == 150
  )

# Number of observations after filtering
cat("Number of observations:", nrow(data), "\n")


# ------------------------------------------------------------
# 2.3. Variable type conversion
# ------------------------------------------------------------

factor_variables <- c(
  "availability_count",
  "sustainability_certificate",
  "metro_proximity",
  "arrondissement",
  "stars"
)

data <- data %>%
  mutate(
    across(
      all_of(factor_variables),
      as.factor
    )
  )


# ------------------------------------------------------------
# 2.4. Reference categories
# ------------------------------------------------------------

set_reference <- function(x, reference) {
  
  if (reference %in% levels(x)) {
    relevel(x, ref = reference)
  } else {
    warning(
      paste("Reference category", reference,
            "not found in variable.")
    )
    x
  }
}

data <- data %>%
  mutate(
    sustainability_certificate =
      set_reference(sustainability_certificate, "0"),
    
    stars = set_reference(stars, "0"),
    
    metro_proximity =
      set_reference(metro_proximity, "0"),
    
    availability_count =
      set_reference(availability_count, "0")
  )


# ------------------------------------------------------------
# 2.5. Remove irrelevant variables
# ------------------------------------------------------------

# Exclude all Title-related topics
title_topics <- grep(
  "_Title$",
  names(data),
  value = TRUE
)

cat("\nExcluded Title variables:\n")
print(title_topics)

# Variables excluded from the analysis
drop_variables <- c(
  "...1",
  "Hotel Link",
  "address",
  "availability_text",
  "rating_value",
  "Sample_Size",
  "discounted_price",
  "situation_geo",
  title_topics
)

drop_variables <- intersect(
  drop_variables,
  names(data)
)

data <- data %>%
  select(-all_of(drop_variables))


# ============================================================
# 03. DESCRIPTIVE STATISTICS AND DATA VALIDATION
# ============================================================

# ------------------------------------------------------------
# 3.1. Summary statistics
# ------------------------------------------------------------

summary(data$log_Price)

summary(data$distance_km)

table(data$sustainability_certificate)

table(data$stars)

table(data$arrondissement)

# Certification distribution
certification_distribution <- data %>%
  count(sustainability_certificate) %>%
  mutate(
    Percentage = 100 * n / sum(n)
  )

print(certification_distribution)


# ------------------------------------------------------------
# 3.2. Missing values
# ------------------------------------------------------------

missing_values <- data %>%
  summarise(
    across(
      everything(),
      ~ sum(is.na(.))
    )
  )

print(missing_values)

write.csv(
  missing_values,
  file.path(output_dir, "missing_values.csv"),
  row.names = FALSE
)


# ============================================================
# 04. ECONOMETRIC SPECIFICATIONS
# ============================================================

# ------------------------------------------------------------
# 4.1. Define explanatory variables
# ------------------------------------------------------------

control_variables <- c(
  "note",
  "arrondissement",
  "stars",
  "distance_km",
  "availability_count",
  "metro_proximity"
)

review_topics <- c(
  "Value for Money_Positive",
  "Facilities_Positive",
  "Service Quality_Negative",
  "Neighborhood_Positive",
  "Mobility and Location_Negative",
  "Atmosphere and Ambiance_Negative"
)

treatment_variable <- "sustainability_certificate"

# Check variable availability
required_variables <- c(
  "log_Price",
  control_variables,
  treatment_variable,
  review_topics
)

missing_variables <- setdiff(
  required_variables,
  names(data)
)

if (length(missing_variables) > 0) {
  stop(
    paste(
      "Missing variables:",
      paste(missing_variables, collapse = ", ")
    )
  )
}


# ------------------------------------------------------------
# 4.2. Construct a common estimation sample
# ------------------------------------------------------------

# All specifications use the same observations.

model_variables <- required_variables

model_df <- data %>%
  select(all_of(model_variables)) %>%
  filter(
    if_all(everything(), ~ !is.na(.))
  )

cat(
  "\nEstimation sample size:",
  nrow(model_df),
  "\n"
)


# ------------------------------------------------------------
# 4.3. Formula construction function
# ------------------------------------------------------------

quote_term <- function(x) {
  
  if (grepl(":", x, fixed = TRUE)) {
    
    parts <- strsplit(x, ":", fixed = TRUE)[[1]]
    
    return(
      paste(
        sprintf("`%s`", parts),
        collapse = ":"
      )
    )
    
  }
  
  sprintf("`%s`", x)
}


build_formula <- function(response, terms) {
  
  rhs <- paste(
    vapply(terms, quote_term, character(1)),
    collapse = " + "
  )
  
  as.formula(
    paste(response, "~", rhs)
  )
}


# ------------------------------------------------------------
# 4.4. Define interaction terms
# ------------------------------------------------------------

# Review topics × hotel stars
interaction_stars <- paste(
  review_topics,
  "stars",
  sep = ":"
)

# Review topics × sustainability certification
interaction_certification <- paste(
  review_topics,
  treatment_variable,
  sep = ":"
)


# ============================================================
# 05. ESTIMATION OF ECONOMETRIC MODELS
# ============================================================

# ------------------------------------------------------------
# MODEL 1: Baseline model without certification
# ------------------------------------------------------------

model_1_terms <- c(
  control_variables,
  review_topics
)

model_1 <- lm(
  build_formula("log_Price", model_1_terms),
  data = model_df
)


# ------------------------------------------------------------
# MODEL 2: Main effect of sustainability certification
# ------------------------------------------------------------

model_2_terms <- c(
  control_variables,
  review_topics,
  treatment_variable
)

model_2 <- lm(
  build_formula("log_Price", model_2_terms),
  data = model_df
)


# ------------------------------------------------------------
# MODEL 3: Heterogeneous effects by hotel stars
# ------------------------------------------------------------

model_3_terms <- c(
  model_2_terms,
  interaction_stars
)

model_3 <- lm(
  build_formula("log_Price", model_3_terms),
  data = model_df
)


# ------------------------------------------------------------
# MODEL 4: Heterogeneous effects by certification
# ------------------------------------------------------------

model_4_terms <- c(
  model_2_terms,
  interaction_certification
)

model_4 <- lm(
  build_formula("log_Price", model_4_terms),
  data = model_df
)


# ------------------------------------------------------------
# MODEL 5: Full specification
# ------------------------------------------------------------

# Includes both interaction structures:
# Review topics × Stars
# Review topics × Sustainability certification

model_5_terms <- c(
  model_2_terms,
  interaction_stars,
  interaction_certification
)

model_5 <- lm(
  build_formula("log_Price", model_5_terms),
  data = model_df
)


# ============================================================
# 06. MODEL COMPARISON AND STATISTICAL INFERENCE
# ============================================================

# ------------------------------------------------------------
# 6.1. Model summaries
# ------------------------------------------------------------

model_list <- list(
  "Model 1: Baseline" = model_1,
  "Model 2: Certification" = model_2,
  "Model 3: Stars interactions" = model_3,
  "Model 4: Certification interactions" = model_4,
  "Model 5: Full specification" = model_5
)

for (model_name in names(model_list)) {
  
  cat("\n========================================\n")
  cat(model_name, "\n")
  cat("========================================\n")
  
  print(summary(model_list[[model_name]]))
}


# ------------------------------------------------------------
# 6.2. Nested model F-tests
# ------------------------------------------------------------

# Formal comparisons of nested specifications

f_test_1_2 <- anova(model_1, model_2)

f_test_2_3 <- anova(model_2, model_3)

f_test_2_4 <- anova(model_2, model_4)

f_test_2_5 <- anova(model_2, model_5)

print(f_test_1_2)
print(f_test_2_3)
print(f_test_2_4)
print(f_test_2_5)


# ------------------------------------------------------------
# 6.3. Heteroskedasticity-robust standard errors
# ------------------------------------------------------------

# HC2 covariance matrix for the full specification

vcov_hc2 <- vcovHC(
  model_5,
  type = "HC2"
)

robust_results <- coeftest(
  model_5,
  vcov. = vcov_hc2
)

print(robust_results)


# ------------------------------------------------------------
# 6.4. Multicollinearity diagnostics
# ------------------------------------------------------------

vif_results <- car::vif(model_5)

print(vif_results)


# ------------------------------------------------------------
# 6.5. Regression table
# ------------------------------------------------------------

robust_se <- sqrt(
  diag(vcov_hc2)
)

robust_p <- robust_results[, 4]

stargazer(
  model_1,
  model_2,
  model_3,
  model_4,
  model_5,
  
  type = "latex",
  
  title = "OLS Regression Results",
  
  column.labels = c(
    "Baseline",
    "Certification",
    "Stars",
    "Certification interactions",
    "Full model"
  ),
  
  se = list(
    NULL,
    NULL,
    NULL,
    NULL,
    robust_se
  ),
  
  p = list(
    NULL,
    NULL,
    NULL,
    NULL,
    robust_p
  ),
  
  dep.var.labels = "Log hotel price",
  
  model.numbers = FALSE,
  
  digits = 4,
  
  out = file.path(
    output_dir,
    "regression_results.tex"
  )
)


# ============================================================
# 07. AVERAGE MARGINAL EFFECT OF CERTIFICATION
# ============================================================

# The AME is calculated using counterfactual predictions.
#
# For each hotel:
#   1. Predict price with certification = 1.
#   2. Predict price with certification = 0.
#   3. Calculate the difference.
#   4. Average individual differences.


# ------------------------------------------------------------
# 7.1. Construct counterfactual datasets
# ------------------------------------------------------------

df_certified <- model_df

df_noncertified <- model_df

df_certified$sustainability_certificate <- factor(
  "1",
  levels = levels(model_df$sustainability_certificate)
)

df_noncertified$sustainability_certificate <- factor(
  "0",
  levels = levels(model_df$sustainability_certificate)
)


# ------------------------------------------------------------
# 7.2. Counterfactual predictions
# ------------------------------------------------------------

pred_certified <- predict(
  model_5,
  newdata = df_certified
)

pred_noncertified <- predict(
  model_5,
  newdata = df_noncertified
)


# ------------------------------------------------------------
# 7.3. Individual treatment effects
# ------------------------------------------------------------

individual_effect <- (
  pred_certified - pred_noncertified
)


# Average marginal effect in log points
AME_log <- mean(
  individual_effect,
  na.rm = TRUE
)

# Convert log effect into percentage
AME_percent <- (
  exp(AME_log) - 1
) * 100


cat("\nAverage marginal effect (log points):\n")
print(AME_log)

cat("\nAverage marginal effect (percentage):\n")
print(AME_percent)


# ============================================================
# 08. BOOTSTRAP INFERENCE FOR THE AME
# ============================================================

# Bootstrap procedure:
# - Resample observations with replacement.
# - Re-estimate the full econometric specification.
# - Recalculate counterfactual predictions.
# - Calculate the AME for each replication.


# ------------------------------------------------------------
# 8.1. Bootstrap parameters
# ------------------------------------------------------------

B <- 2000

set.seed(12345)


# ------------------------------------------------------------
# 8.2. Bootstrap function
# ------------------------------------------------------------

boot_AME <- function(data, indices) {
  
  # Resample observations
  d <- data[indices, , drop = FALSE]
  
  # Avoid estimation problems when a categorical variable
  # has only one observed category in a bootstrap sample.
  
  categorical_variables <- c(
    "arrondissement",
    "stars",
    "availability_count",
    "metro_proximity",
    "sustainability_certificate"
  )
  
  insufficient_variation <- any(
    vapply(
      d[categorical_variables],
      function(x) {
        nlevels(droplevels(x)) < 2
      },
      logical(1)
    )
  )
  
  if (insufficient_variation) {
    return(NA_real_)
  }
  
  # Re-estimate full model
  model_boot <- tryCatch(
    
    lm(
      build_formula(
        "log_Price",
        model_5_terms
      ),
      data = d
    ),
    
    error = function(e) NULL
  )
  
  if (is.null(model_boot)) {
    return(NA_real_)
  }
  
  # Counterfactual datasets
  d_cert <- d
  
  d_noncert <- d
  
  d_cert$sustainability_certificate <- factor(
    "1",
    levels = levels(d$sustainability_certificate)
  )
  
  d_noncert$sustainability_certificate <- factor(
    "0",
    levels = levels(d$sustainability_certificate)
  )
  
  # Predictions
  predictions_cert <- tryCatch(
    predict(model_boot, newdata = d_cert),
    error = function(e) NULL
  )
  
  predictions_noncert <- tryCatch(
    predict(model_boot, newdata = d_noncert),
    error = function(e) NULL
  )
  
  if (
    is.null(predictions_cert) ||
    is.null(predictions_noncert)
  ) {
    return(NA_real_)
  }
  
  # Individual effects
  effects <- predictions_cert - predictions_noncert
  
  # Return AME in log points
  mean(effects, na.rm = TRUE)
}


# ------------------------------------------------------------
# 8.3. Run bootstrap
# ------------------------------------------------------------

bootstrap_results <- boot(
  data = model_df,
  statistic = boot_AME,
  R = B
)

# Remove failed replications
bootstrap_values <- bootstrap_results$t[, 1]

bootstrap_values <- bootstrap_values[
  is.finite(bootstrap_values)
]

cat(
  "\nSuccessful bootstrap replications:",
  length(bootstrap_values),
  "out of",
  B,
  "\n"
)


# ------------------------------------------------------------
# 8.4. Bootstrap standard error
# ------------------------------------------------------------

bootstrap_SE <- sd(
  bootstrap_values
)

print(bootstrap_SE)


# ------------------------------------------------------------
# 8.5. Confidence intervals
# ------------------------------------------------------------

# Normal approximation
CI_normal <- c(
  AME_log - 1.96 * bootstrap_SE,
  AME_log + 1.96 * bootstrap_SE
)

# Percentile confidence interval
CI_percentile <- quantile(
  bootstrap_values,
  probs = c(0.025, 0.975)
)

# BCa confidence interval
CI_BCa <- tryCatch(
  boot.ci(
    bootstrap_results,
    type = "bca",
    conf = 0.95
  ),
  error = function(e) NULL
)


# ============================================================
# 09. CONVERSION INTO PERCENTAGE EFFECTS
# ============================================================

# Transform the AME and confidence interval endpoints.
# The standard error remains reported in log points.

AME_percentage <- (
  exp(AME_log) - 1
) * 100

CI_normal_percentage <- (
  exp(CI_normal) - 1
) * 100

CI_percentile_percentage <- (
  exp(CI_percentile) - 1
) * 100

if (
  !is.null(CI_BCa) &&
  !is.null(CI_BCa$bca)
) {
  
  CI_BCa_percentage <- (
    exp(CI_BCa$bca[4:5]) - 1
  ) * 100
  
} else {
  
  CI_BCa_percentage <- c(
    NA_real_,
    NA_real_
  )
  
}


# ------------------------------------------------------------
# 9.1. Bootstrap-based significance test
# ------------------------------------------------------------

z_boot <- AME_log / bootstrap_SE

p_value_boot <- 2 * pnorm(
  -abs(z_boot)
)


# ============================================================
# 10. FINAL RESULTS AND EXPORT
# ============================================================

AME_results <- data.frame(
  
  Effect = "Sustainability certification",
  
  AME_log = AME_log,
  
  AME_percentage = AME_percentage,
  
  Bootstrap_SE_log = bootstrap_SE,
  
  Normal_CI_lower = CI_normal_percentage[1],
  
  Normal_CI_upper = CI_normal_percentage[2],
  
  Percentile_CI_lower =
    CI_percentile_percentage[1],
  
  Percentile_CI_upper =
    CI_percentile_percentage[2],
  
  BCa_CI_lower =
    CI_BCa_percentage[1],
  
  BCa_CI_upper =
    CI_BCa_percentage[2],
  
  Bootstrap_p_value = p_value_boot
  
)

print(AME_results)


# ------------------------------------------------------------
# 10.1. Export AME results
# ------------------------------------------------------------

write.csv(
  AME_results,
  file.path(
    output_dir,
    "AME_bootstrap_results.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 10.2. Export model comparison
# ------------------------------------------------------------

model_comparison <- data.frame(
  
  Model = names(model_list),
  
  Observations = vapply(
    model_list,
    nobs,
    numeric(1)
  ),
  
  R_squared = vapply(
    model_list,
    function(m) summary(m)$r.squared,
    numeric(1)
  ),
  
  Adjusted_R_squared = vapply(
    model_list,
    function(m) summary(m)$adj.r.squared,
    numeric(1)
  ),
  
  AIC = vapply(
    model_list,
    AIC,
    numeric(1)
  ),
  
  BIC = vapply(
    model_list,
    BIC,
    numeric(1)
  )
  
)

print(model_comparison)

write.csv(
  model_comparison,
  file.path(
    output_dir,
    "model_comparison.csv"
  ),
  row.names = FALSE
)


# ------------------------------------------------------------
# 10.3. Save model summaries
# ------------------------------------------------------------

capture.output(
  lapply(model_list, summary),
  file = file.path(
    output_dir,
    "model_summaries.txt"
  )
)


# ------------------------------------------------------------
# 10.4. Save session information
# ------------------------------------------------------------

capture.output(
  sessionInfo(),
  file = file.path(
    output_dir,
    "session_info.txt"
  )
)


# ============================================================
# END OF EMPIRICAL ANALYSIS
# ============================================================

cat(
  "\nAnalysis completed successfully.\n",
  "Results saved in:", output_dir, "\n"
)

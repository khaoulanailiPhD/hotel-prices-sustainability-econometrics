
# ============================================================
# PROJECT: HOTEL PRICES, SUSTAINABILITY AND CUSTOMER REVIEWS
# Econometric analysis using R
#
# Objective:
# Analyse the determinants of hotel prices, with particular
# attention to sustainability certification and review topics.
#
# Outcome variable: log_Price
#
# Models:
#   Model 1: Baseline model
#   Model 2: Sustainability certification
#   Model 3: Positive review topics
#   Model 4: Negative review topics (Sample_Size = 30)
#   Model 5: Selected positive and negative topics
#   Model 6: Interaction effects
#
# Robust inference: HC2 standard errors
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "dplyr",
  "readxl",
  "lmtest",
  "sandwich",
  "broom"
)

# Install missing packages
missing_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

# Load packages
invisible(lapply(packages, library, character.only = TRUE))


# ============================================================
# 1. DATA IMPORT AND PREPARATION
# ============================================================

# 1.1 Import Excel file
data_raw <- readxl::read_excel(file.choose())

# Preserve the imported data
data <- data_raw


# ------------------------------------------------------------
# 1.2 Clean price variable
# ------------------------------------------------------------

# IMPORTANT:
# Check the original price format before using parse_number.
# This example assumes that prices use a decimal point.

data <- data %>%
  mutate(
    discounted_price = as.numeric(discounted_price)
  )

# Remove missing, zero and negative prices
data <- data %>%
  filter(
    !is.na(discounted_price),
    discounted_price > 0
  )

# Logarithm of hotel price
data <- data %>%
  mutate(
    log_Price = log(discounted_price)
  )


# ------------------------------------------------------------
# 1.3 Preserve price-cleaned data for sample comparisons
# ------------------------------------------------------------

# This dataset is preserved before applying the experience
# and Sample_Size restrictions used in the main analysis.

data_full <- data


# ------------------------------------------------------------
# 1.4 Main estimation sample
# ------------------------------------------------------------

data <- data %>%
  filter(
    experience_count >= 150,
    Sample_Size == 150
  )


# ------------------------------------------------------------
# 1.5 Convert categorical variables into factors
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
# 1.6 Remove topic title variables
# ------------------------------------------------------------

data <- data %>%
  select(
    -matches("_Title$")
  )


# ------------------------------------------------------------
# 1.7 Define estimation datasets
# ------------------------------------------------------------

# Main sample: Sample_Size = 150 and experience_count >= 150
df_subset <- data

# Comparison sample: Sample_Size = 30
# Note: no experience_count restriction is applied here.
df_subset30 <- data_full %>%
  filter(Sample_Size == 30)


# ------------------------------------------------------------
# 1.8 Define reference categories
# ------------------------------------------------------------

reference_variables <- c(
  "sustainability_certificate",
  "stars",
  "metro_proximity",
  "availability_count"
)

for (variable in reference_variables) {
  
  if (variable %in% names(df_subset)) {
    
    df_subset[[variable]] <- relevel(
      as.factor(df_subset[[variable]]),
      ref = "0"
    )
    
  }
}

for (variable in reference_variables) {
  
  if (variable %in% names(df_subset30)) {
    
    df_subset30[[variable]] <- relevel(
      as.factor(df_subset30[[variable]]),
      ref = "0"
    )
    
  }
}


# ============================================================
# 2. STEPWISE REGRESSION — AIC-BASED SELECTION
# ============================================================

# The step() function performs AIC-based model selection.
# It does NOT perform F-test-based variable selection.


# ------------------------------------------------------------
# 2.1 Identify candidate explanatory variables
# ------------------------------------------------------------

excluded_variables <- c(
  "log_Price",
  "discounted_price",
  "experience_count",
  "Sample_Size",
  "hotel_id",
  "hotel_name",
  "review_text"
)

candidate_variables <- setdiff(
  names(df_subset),
  excluded_variables
)

# Remove variables ending in "_Title"
candidate_variables <- candidate_variables[
  !grepl("_Title$", candidate_variables)
]

# Remove variables with missing values only if they are
# completely empty; ordinary missing observations are handled
# by lm() through its default na.action.
candidate_variables <- candidate_variables[
  vapply(
    df_subset[candidate_variables],
    function(x) !all(is.na(x)),
    logical(1)
  )
]


# ------------------------------------------------------------
# 2.2 Construct initial and full models
# ------------------------------------------------------------

initial_model <- lm(
  log_Price ~ 1,
  data = df_subset
)

full_formula <- reformulate(
  candidate_variables,
  response = "log_Price"
)

full_model <- lm(
  full_formula,
  data = df_subset
)


# ------------------------------------------------------------
# 2.3 Stepwise selection
# ------------------------------------------------------------

step_mod_AIC_noTitle <- step(
  initial_model,
  scope = list(
    lower = formula(initial_model),
    upper = formula(full_model)
  ),
  direction = "both",
  trace = TRUE
)

# Selected model
summary(step_mod_AIC_noTitle)

# Selection path
step_mod_AIC_noTitle$anova


# ------------------------------------------------------------
# 2.4 Model selection criteria
# ------------------------------------------------------------

selection_results <- data.frame(
  Model = "Stepwise AIC",
  Observations = nobs(step_mod_AIC_noTitle),
  Adjusted_R2 = summary(step_mod_AIC_noTitle)$adj.r.squared,
  AIC = AIC(step_mod_AIC_noTitle),
  BIC = BIC(step_mod_AIC_noTitle)
)

print(selection_results)

write.csv(
  selection_results,
  "results_stepwise_AIC.csv",
  row.names = FALSE
)


# ============================================================
# 3. ECONOMETRIC MODEL SPECIFICATIONS
# ============================================================


# ------------------------------------------------------------
# 3.1 Model 1 — Baseline specification
# ------------------------------------------------------------

model1 <- lm(
  log_Price ~
    note +
    factor(arrondissement) +
    factor(stars) +
    distance_km +
    availability_count +
    metro_proximity,
  data = df_subset
)


# ------------------------------------------------------------
# 3.2 Model 2 — Sustainability certification
# ------------------------------------------------------------

model2 <- lm(
  log_Price ~
    note +
    factor(arrondissement) +
    factor(stars) +
    distance_km +
    availability_count +
    metro_proximity +
    sustainability_certificate,
  data = df_subset
)


# ------------------------------------------------------------
# 3.3 Model 3 — Positive review topics
# ------------------------------------------------------------

model3 <- lm(
  log_Price ~
    note +
    factor(arrondissement) +
    factor(stars) +
    distance_km +
    availability_count +
    metro_proximity +
    sustainability_certificate +
    `Room_Positive` +
    `Facilities_Positive` +
    `Mobility and Location_Positive` +
    `Staff and Professionalism_Positive` +
    `Service Quality_Positive` +
    `Amenities_Positive` +
    `Value for Money_Positive` +
    `Atmosphere and Ambiance_Positive` +
    `Empathy and Personalization_Positive` +
    `Neighborhood_Positive`,
  data = df_subset
)


# ------------------------------------------------------------
# 3.4 Model 4 — Negative review topics
#      Sample_Size = 30
# ------------------------------------------------------------

model4 <- lm(
  log_Price ~
    note +
    factor(arrondissement) +
    factor(stars) +
    distance_km +
    availability_count +
    metro_proximity +
    `Room_Negative` +
    `Facilities_Negative` +
    `Mobility and Location_Negative` +
    `Staff and Professionalism_Negative` +
    `Service Quality_Negative` +
    `Amenities_Negative` +
    `Value for Money_Negative` +
    `Atmosphere and Ambiance_Negative` +
    `Empathy and Personalization_Negative` +
    `Neighborhood_Negative`,
  data = df_subset30
)


# ------------------------------------------------------------
# 3.5 Model 5 — Selected positive and negative topics
#      Sample_Size = 150
# ------------------------------------------------------------

model5 <- lm(
  log_Price ~
    note +
    factor(arrondissement) +
    factor(stars) +
    distance_km +
    availability_count +
    metro_proximity +
    sustainability_certificate +
    `Value for Money_Positive` +
    `Facilities_Positive` +
    `Service Quality_Negative` +
    `Neighborhood_Positive` +
    `Mobility and Location_Negative` +
    `Atmosphere and Ambiance_Negative`,
  data = df_subset
)


# ------------------------------------------------------------
# 3.6 Model 6 — Interaction effects
# ------------------------------------------------------------

model6 <- lm(
  log_Price ~
    note +
    factor(arrondissement) +
    factor(stars) +
    distance_km +
    availability_count +
    metro_proximity +
    sustainability_certificate +
    
    # Review topics
    `Value for Money_Positive` +
    `Facilities_Positive` +
    `Service Quality_Negative` +
    `Neighborhood_Positive` +
    `Mobility and Location_Negative` +
    `Atmosphere and Ambiance_Negative` +
    
    # Interactions with hotel stars
    `Value for Money_Positive`:factor(stars) +
    `Facilities_Positive`:factor(stars) +
    `Service Quality_Negative`:factor(stars) +
    `Neighborhood_Positive`:factor(stars) +
    `Mobility and Location_Negative`:factor(stars) +
    `Atmosphere and Ambiance_Negative`:factor(stars) +
    
    # Interactions with sustainability certification
    `Value for Money_Positive`:sustainability_certificate +
    `Facilities_Positive`:sustainability_certificate +
    `Service Quality_Negative`:sustainability_certificate +
    `Neighborhood_Positive`:sustainability_certificate +
    `Mobility and Location_Negative`:sustainability_certificate +
    `Atmosphere and Ambiance_Negative`:sustainability_certificate,
  
  data = df_subset
)


# ============================================================
# 4. MODEL COMPARISON
# ============================================================

models_original <- list(
  Model1 = model1,
  Model2 = model2,
  Model3 = model3,
  Model4 = model4,
  Model5 = model5,
  Model6 = model6
)


# ------------------------------------------------------------
# 4.1 Extract model statistics
# ------------------------------------------------------------

model_comparison <- bind_rows(
  lapply(names(models_original), function(model_name) {
    
    model <- models_original[[model_name]]
    
    data.frame(
      Model = model_name,
      Observations = nobs(model),
      R2 = summary(model)$r.squared,
      Adjusted_R2 = summary(model)$adj.r.squared,
      AIC = AIC(model),
      BIC = BIC(model)
    )
    
  })
)

print(model_comparison)

write.csv(
  model_comparison,
  "model_comparison.csv",
  row.names = FALSE
)


# ------------------------------------------------------------
# 4.2 Display model summaries
# ------------------------------------------------------------

for (model_name in names(models_original)) {
  
  cat("\n====================================\n")
  cat("RESULTS:", model_name, "\n")
  cat("====================================\n")
  
  print(summary(models_original[[model_name]]))
  
}


# ============================================================
# 5. HC2 ROBUST STANDARD ERRORS
# ============================================================

# HC2 heteroskedasticity-consistent covariance estimator.
# Each model is estimated with its own original sample.


# ------------------------------------------------------------
# 5.1 Calculate robust inference
# ------------------------------------------------------------

models_HC2 <- lapply(
  models_original,
  function(model) {
    
    lmtest::coeftest(
      model,
      vcov. = sandwich::vcovHC(
        model,
        type = "HC2"
      )
    )
    
  }
)


# ------------------------------------------------------------
# 5.2 Display HC2 results
# ------------------------------------------------------------

for (model_name in names(models_HC2)) {
  
  cat("\n====================================\n")
  cat("HC2 ROBUST RESULTS:", model_name, "\n")
  cat("====================================\n")
  
  print(models_HC2[[model_name]])
  
}


# ------------------------------------------------------------
# 5.3 Export HC2 results
# ------------------------------------------------------------

for (model_name in names(models_HC2)) {
  
  robust_table <- as.data.frame(
    models_HC2[[model_name]]
  )
  
  robust_table$Variable <- rownames(robust_table)
  
  rownames(robust_table) <- NULL
  
  robust_table <- robust_table %>%
    select(Variable, everything())
  
  write.csv(
    robust_table,
    paste0(model_name, "_HC2_results.csv"),
    row.names = FALSE
  )
  
}


# ============================================================
# 6. END OF ANALYSIS
# ============================================================

cat("\n====================================\n")
cat("ECONOMETRIC ANALYSIS COMPLETED\n")
cat("====================================\n")

cat("\nGenerated files:\n")
cat("- results_stepwise_AIC.csv\n")
cat("- model_comparison.csv\n")
cat("- Model1_HC2_results.csv\n")
cat("- Model2_HC2_results.csv\n")
cat("- Model3_HC2_results.csv\n")
cat("- Model4_HC2_results.csv\n")
cat("- Model5_HC2_results.csv\n")
cat("- Model6_HC2_results.csv\n")

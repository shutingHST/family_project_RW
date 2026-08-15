# =====================================================
# POST HOC SUPPRESSION ANALYSIS FOR PVWM EFFECTS
# =====================================================

# Load required libraries
library(tidyverse)
library(broom)

# -----------------------------------------------------
# PART 1: SINGLE SUPPRESSOR ANALYSIS
# -----------------------------------------------------

# Function to run single suppressor analysis
analyze_single_suppressor <- function(data, outcome, predictor, suppressors, 
                                      covariates = c("Q19_P_Edu", "hlp", "Age_Months")) {
  
  results_list <- list()
  
  # Baseline model (predictor alone with covariates)
  baseline_formula <- as.formula(paste(outcome, "~", 
                                       paste(covariates, collapse = " + "), "+", predictor))
  baseline_model <- lm(baseline_formula, data = data)
  baseline_coef <- tidy(baseline_model) %>% 
    filter(term == predictor) %>% 
    pull(estimate)
  
  cat("\n========================================\n")
  cat("OUTCOME:", toupper(outcome), "\n")
  cat("========================================\n")
  cat("\nBaseline Model (", predictor, "with covariates only):\n")
  cat("----------------------------------------\n")
  print(summary(baseline_model)$coefficients)
  cat("\nPVWM coefficient:", round(baseline_coef, 4), "\n")
  
  # Test each potential suppressor
  for (suppressor in suppressors) {
    cat("\n\nModel with", toupper(suppressor), "as suppressor:\n")
    cat("----------------------------------------\n")
    
    formula <- as.formula(paste(outcome, "~", 
                                paste(covariates, collapse = " + "), "+", 
                                predictor, "+", suppressor))
    model <- lm(formula, data = data)
    
    # Print model summary
    print(summary(model)$coefficients)
    
    # Extract PVWM coefficient
    pvwm_coef <- tidy(model) %>% 
      filter(term == predictor) %>% 
      pull(estimate)
    
    # Calculate change in PVWM coefficient
    coef_change <- pvwm_coef - baseline_coef
    percent_change <- (abs(pvwm_coef) - abs(baseline_coef)) / abs(baseline_coef) * 100
    
    cat("\nPVWM coefficient:", round(pvwm_coef, 4))
    cat("\nChange from baseline:", round(coef_change, 4))
    cat("\nPercent change in magnitude:", round(percent_change, 2), "%")
    
    # Check for suppression (sign change or magnitude increase)
    if (sign(pvwm_coef) != sign(baseline_coef)) {
      cat("\n*** SIGN REVERSAL DETECTED ***")
    }
    if (abs(pvwm_coef) > abs(baseline_coef)) {
      cat("\n*** SUPPRESSION EFFECT (magnitude increased) ***")
    }
    
    results_list[[suppressor]] <- list(
      model = model,
      pvwm_coef = pvwm_coef,
      change = coef_change,
      percent_change = percent_change
    )
  }
  
  return(results_list)
}

# Define other parent predictors as potential suppressors
suppressors <- c("PCRF", "PRAN", "PNWR", "PCPA", "PCMA")

# Define covariates
covariates <- c("Q19_P_Edu", "hlp", "Age_Months")

# Analyze CMA as outcome
cat("\n\n################################################\n")
cat("ANALYSIS 1: PVWM -> CMA (Child Contextual Morphological Awareness)\n")
cat("################################################\n")
cma_results <- analyze_single_suppressor(
  data = data_parent_children6,
  outcome = "cma",
  predictor = "PVWM",
  suppressors = suppressors,
  covariates = covariates
)

# Analyze CPA as outcome
cat("\n\n################################################\n")
cat("ANALYSIS 2: PVWM -> CPA (Child Contextual Phonological Awareness)\n")
cat("################################################\n")
cpa_results <- analyze_single_suppressor(
  data = data_parent_children6,
  outcome = "cpa",
  predictor = "PVWM",
  suppressors = suppressors,
  covariates = covariates
)

# Analyze NWR as outcome
cat("\n\n################################################\n")
cat("ANALYSIS 3: PVWM -> NWR (Child Nonword Repetition)\n")
cat("################################################\n")
nwr_results <- analyze_single_suppressor(
  data = data_parent_children6,
  outcome = "nwr",
  predictor = "PVWM",
  suppressors = suppressors,
  covariates = covariates
)

# -----------------------------------------------------
# PART 2: MULTIPLE SUPPRESSOR ANALYSIS
# -----------------------------------------------------

cat("\n\n################################################\n")
cat("MULTIPLE SUPPRESSOR ANALYSIS\n")
cat("################################################\n")

# Function for multiple suppressor analysis
analyze_multiple_suppressors <- function(data, outcome, predictor, suppressors, 
                                         covariates = c("Q19_P_Edu", "hlp", "Age_Mnths")) {
  
  # Baseline model
  baseline_formula <- as.formula(paste(outcome, "~", 
                                       paste(covariates, collapse = " + "), "+", predictor))
  baseline_model <- lm(baseline_formula, data = data)
  baseline_coef <- tidy(baseline_model) %>% 
    filter(term == predictor) %>% 
    pull(estimate)
  
  cat("\n========================================\n")
  cat("OUTCOME:", toupper(outcome), "\n")
  cat("========================================\n")
  cat("\nBaseline PVWM coefficient:", round(baseline_coef, 4), "\n")
  
  # Test combinations of 2 suppressors
  cat("\n\nTwo-Suppressor Models:\n")
  cat("----------------------------------------\n")
  
  two_suppressor_results <- list()
  
  for (i in 1:(length(suppressors)-1)) {
    for (j in (i+1):length(suppressors)) {
      sup_combo <- c(suppressors[i], suppressors[j])
      formula <- as.formula(paste(outcome, "~", 
                                  paste(covariates, collapse = " + "), "+",
                                  predictor, "+", 
                                  paste(sup_combo, collapse = " + ")))
      model <- lm(formula, data = data)
      
      pvwm_coef <- tidy(model) %>% 
        filter(term == predictor) %>% 
        pull(estimate)
      
      combo_name <- paste(sup_combo, collapse = " + ")
      cat("\nModel with", combo_name, ":\n")
      cat("PVWM coefficient:", round(pvwm_coef, 4))
      cat(" (change:", round(pvwm_coef - baseline_coef, 4), ")")
      
      if (sign(pvwm_coef) != sign(baseline_coef)) {
        cat(" *** SIGN REVERSAL ***")
      }
      if (abs(pvwm_coef) > abs(baseline_coef)) {
        cat(" *** SUPPRESSION ***")
      }
      cat("\n")
      
      two_suppressor_results[[combo_name]] <- pvwm_coef
    }
  }
  
  # Test combinations of 3 suppressors
  cat("\n\nThree-Suppressor Models:\n")
  cat("----------------------------------------\n")
  
  three_suppressor_results <- list()
  
  for (i in 1:(length(suppressors)-2)) {
    for (j in (i+1):(length(suppressors)-1)) {
      for (k in (j+1):length(suppressors)) {
        sup_combo <- c(suppressors[i], suppressors[j], suppressors[k])
        formula <- as.formula(paste(outcome, "~", 
                                    paste(covariates, collapse = " + "), "+",
                                    predictor, "+", 
                                    paste(sup_combo, collapse = " + ")))
        model <- lm(formula, data = data)
        
        pvwm_coef <- tidy(model) %>% 
          filter(term == predictor) %>% 
          pull(estimate)
        
        combo_name <- paste(sup_combo, collapse = " + ")
        cat("\nModel with", combo_name, ":\n")
        cat("PVWM coefficient:", round(pvwm_coef, 4))
        cat(" (change:", round(pvwm_coef - baseline_coef, 4), ")")
        
        if (sign(pvwm_coef) != sign(baseline_coef)) {
          cat(" *** SIGN REVERSAL ***")
        }
        if (abs(pvwm_coef) > abs(baseline_coef)) {
          cat(" *** SUPPRESSION ***")
        }
        cat("\n")
        
        three_suppressor_results[[combo_name]] <- pvwm_coef
      }
    }
  }
  
  # Test combinations of 4 suppressors
  cat("\n\nFour-Suppressor Models:\n")
  cat("----------------------------------------\n")
  
  for (i in 1:(length(suppressors)-3)) {
    for (j in (i+1):(length(suppressors)-2)) {
      for (k in (j+1):(length(suppressors)-1)) {
        for (l in (k+1):length(suppressors)) {
          sup_combo <- c(suppressors[i], suppressors[j], suppressors[k], suppressors[l])
          formula <- as.formula(paste(outcome, "~", 
                                      paste(covariates, collapse = " + "), "+",
                                      predictor, "+", 
                                      paste(sup_combo, collapse = " + ")))
          model <- lm(formula, data = data)
          
          pvwm_coef <- tidy(model) %>% 
            filter(term == predictor) %>% 
            pull(estimate)
          
          combo_name <- paste(sup_combo, collapse = " + ")
          cat("\nModel with", combo_name, ":\n")
          cat("PVWM coefficient:", round(pvwm_coef, 4))
          cat(" (change:", round(pvwm_coef - baseline_coef, 4), ")")
          
          if (sign(pvwm_coef) != sign(baseline_coef)) {
            cat(" *** SIGN REVERSAL ***")
          }
          if (abs(pvwm_coef) > abs(baseline_coef)) {
            cat(" *** SUPPRESSION ***")
          }
          cat("\n")
        }
      }
    }
  }
  
  # Test all suppressors together
  cat("\n\nAll Suppressors Model:\n")
  cat("----------------------------------------\n")
  
  formula <- as.formula(paste(outcome, "~", 
                              paste(covariates, collapse = " + "), "+",
                              predictor, "+", 
                              paste(suppressors, collapse = " + ")))
  model <- lm(formula, data = data)
  
  print(summary(model)$coefficients)
  
  pvwm_coef <- tidy(model) %>% 
    filter(term == predictor) %>% 
    pull(estimate)
  
  cat("\nPVWM coefficient:", round(pvwm_coef, 4))
  cat("\nChange from baseline:", round(pvwm_coef - baseline_coef, 4))
  
  if (sign(pvwm_coef) != sign(baseline_coef)) {
    cat("\n*** SIGN REVERSAL DETECTED ***")
  }
  if (abs(pvwm_coef) > abs(baseline_coef)) {
    cat("\n*** SUPPRESSION EFFECT ***")
  }
  
  return(model)
}

# Run multiple suppressor analysis for CMA
cma_multiple <- analyze_multiple_suppressors(
  data = data_parent_children6,
  outcome = "cma",
  predictor = "PVWM",
  suppressors = suppressors,
  covariates = covariates
)

# Run multiple suppressor analysis for CPA
cpa_multiple <- analyze_multiple_suppressors(
  data = data_parent_children6,
  outcome = "cpa",
  predictor = "PVWM",
  suppressors = suppressors,
  covariates = covariates
)

# Run multiple suppressor analysis for NWR
nwr_multiple <- analyze_multiple_suppressors(
  data = data_parent_children6,
  outcome = "nwr",
  predictor = "PVWM",
  suppressors = suppressors,
  covariates = covariates
)

# -----------------------------------------------------
# PART 3: SUMMARY TABLE
# -----------------------------------------------------

cat("\n\n################################################\n")
cat("SUMMARY OF SUPPRESSION EFFECTS\n")
cat("################################################\n")

# Create summary function
create_summary <- function(single_results, outcome) {
  summary_df <- data.frame(
    Outcome = outcome,
    Suppressor = names(single_results),
    PVWM_Coefficient = sapply(single_results, function(x) round(x$pvwm_coef, 4)),
    Change = sapply(single_results, function(x) round(x$change, 4)),
    Percent_Change = sapply(single_results, function(x) round(x$percent_change, 2)),
    row.names = NULL
  )
  return(summary_df)
}

# Create summaries
cma_summary <- create_summary(cma_results, "CMA")
cpa_summary <- create_summary(cpa_results, "CPA")
nwr_summary <- create_summary(nwr_results, "NWR")

# Combine and print
full_summary <- rbind(cma_summary, cpa_summary, nwr_summary)
print(full_summary)

# -----------------------------------------------------
# PART 4: VISUALIZATION
# -----------------------------------------------------

# Plot coefficient changes
library(ggplot2)

plot_data <- full_summary %>%
  mutate(Suppressor = factor(Suppressor, levels = c("PCRF", "PRAN", "PNWR", "PCPA", "PCMA")),
         Outcome = factor(Outcome, levels = c("CMA", "CPA", "NWR")))

# Plot 1: PVWM coefficients with different suppressors
p1 <- ggplot(plot_data, aes(x = Suppressor, y = PVWM_Coefficient, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "PVWM Coefficients with Different Suppressors",
       subtitle = "Controlling for Q19_P_Edu, hlp, and Age_Mnths",
       x = "Suppressor Variable",
       y = "PVWM Coefficient") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p1)

# Plot 2: Change in PVWM coefficient
p2 <- ggplot(plot_data, aes(x = Suppressor, y = Change, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Change in PVWM Coefficient with Suppressors",
       subtitle = "Difference from baseline model",
       x = "Suppressor Variable",
       y = "Change in Coefficient") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p2)

# Plot 3: Percent change
p3 <- ggplot(plot_data, aes(x = Suppressor, y = Percent_Change, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  theme_minimal() +
  labs(title = "Percent Change in PVWM Coefficient Magnitude",
       x = "Suppressor Variable",
       y = "Percent Change (%)") +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 45, hjust = 1))

print(p3)

# -----------------------------------------------------
# PART 5: DETAILED COMPARISON TABLE
# -----------------------------------------------------

cat("\n\n################################################\n")
cat("DETAILED SUPPRESSION ANALYSIS\n")
cat("################################################\n")

# Function to classify suppression type
classify_suppression <- function(baseline, suppressed) {
  if (sign(baseline) != sign(suppressed)) {
    return("Classical Suppression (sign reversal)")
  } else if (abs(suppressed) > abs(baseline)) {
    return("Enhancement Effect")
  } else if (abs(suppressed) < abs(baseline)) {
    return("Reduction Effect")
  } else {
    return("No Change")
  }
}

# Create detailed analysis for each outcome
for (outcome in c("cma", "cpa", "nwr")) {
  cat("\n\nOutcome:", toupper(outcome), "\n")
  cat("="*50, "\n")
  
  results <- switch(outcome,
                    "cma" = cma_results,
                    "cpa" = cpa_results,
                    "nwr" = nwr_results)
  
  # Get baseline coefficient
  baseline_formula <- as.formula(paste(outcome, "~", 
                                       paste(covariates, collapse = " + "), "+", "PVWM"))
  baseline_model <- lm(baseline_formula, data = data_parent_children6)
  baseline_coef <- coef(baseline_model)["PVWM"]
  
  cat("Baseline PVWM coefficient:", round(baseline_coef, 4), "\n\n")
  
  for (sup in names(results)) {
    suppression_type <- classify_suppression(baseline_coef, results[[sup]]$pvwm_coef)
    cat(sprintf("%-6s: coef = %7.4f, change = %7.4f (%s)\n", 
                sup, results[[sup]]$pvwm_coef, results[[sup]]$change, suppression_type))
  }
}
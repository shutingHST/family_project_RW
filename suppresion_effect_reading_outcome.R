child_des<-read.spss("data/Children_Dataset.sav", to.data.frame=TRUE)%>%
  dplyr::select(1,2,4,6,9,13,14,15,16,17,18,19,20,21,25,26,28)%>%
  ##remove space in CID
  mutate(CID=gsub(" ", "", CID))%>%
  rename(crf=T3_CRF_AVG_PER_MIN,rdn=T4_RDN_PER_SEC,rpn=T5_RPN_PER_SEC,cpa=T8_PA_Overall,cma=T9_MA_SUM ,
         nwr=T7_NWR_SUM,vwm=T11_VWM_SUM,cwr=T2_CWR_SUM)%>%
  mutate(ran=rdn+rpn)%>%
  filter(CID%in%data_parent_children6$CID)%>%
  select(CID,Gender ,Grade ,  Age_Months,cwr,crf, ran, cpa, cma, nwr, vwm)

# =====================================================
# POST HOC SUPPRESSION ANALYSIS FOR CPA EFFECTS
# =====================================================

# Load required libraries
library(tidyverse)
library(broom)

# -----------------------------------------------------
# PART 1: SINGLE SUPPRESSOR ANALYSIS
# -----------------------------------------------------

# Function to run single suppressor analysis
analyze_single_suppressor <- function(data, outcome, predictor, suppressors, covariate = "Age_Months") {
  
  results_list <- list()
  
  # Baseline model (predictor alone with covariate)
  baseline_formula <- as.formula(paste(outcome, "~", covariate, "+", predictor))
  baseline_model <- lm(baseline_formula, data = data)
  baseline_coef <- tidy(baseline_model) %>% 
    filter(term == predictor) %>% 
    pull(estimate)
  
  cat("\n========================================\n")
  cat("OUTCOME:", outcome, "\n")
  cat("========================================\n")
  cat("\nBaseline Model (", predictor, "alone):\n")
  cat("----------------------------------------\n")
  print(summary(baseline_model)$coefficients)
  cat("\nCPA coefficient:", round(baseline_coef, 4), "\n")
  
  # Test each potential suppressor
  for (suppressor in suppressors) {
    cat("\n\nModel with", suppressor, "as suppressor:\n")
    cat("----------------------------------------\n")
    
    formula <- as.formula(paste(outcome, "~", covariate, "+", predictor, "+", suppressor))
    model <- lm(formula, data = data)
    
    # Print model summary
    print(summary(model)$coefficients)
    
    # Extract CPA coefficient
    cpa_coef <- tidy(model) %>% 
      filter(term == predictor) %>% 
      pull(estimate)
    
    # Calculate change in CPA coefficient
    coef_change <- cpa_coef - baseline_coef
    percent_change <- (abs(cpa_coef) - abs(baseline_coef)) / abs(baseline_coef) * 100
    
    cat("\nCPA coefficient:", round(cpa_coef, 4))
    cat("\nChange from baseline:", round(coef_change, 4))
    cat("\nPercent change in magnitude:", round(percent_change, 2), "%")
    
    # Check for suppression (sign change or magnitude increase)
    if (sign(cpa_coef) != sign(baseline_coef)) {
      cat("\n*** SIGN REVERSAL DETECTED ***")
    }
    if (abs(cpa_coef) > abs(baseline_coef)) {
      cat("\n*** SUPPRESSION EFFECT (magnitude increased) ***")
    }
    
    results_list[[suppressor]] <- list(
      model = model,
      cpa_coef = cpa_coef,
      change = coef_change,
      percent_change = percent_change
    )
  }
  
  return(results_list)
}

# Define suppressors to test
suppressors <- c("cma", "ran", "nwr", "vwm")

# Analyze CWR as outcome
cat("\n\n################################################\n")
cat("ANALYSIS 1: CPA -> CWR (Contextual Word Recognition)\n")
cat("################################################\n")
cwr_results <- analyze_single_suppressor(
  data = child_des,  # Replace with your data frame name
  outcome = "cwr",
  predictor = "cpa",
  suppressors = suppressors
)

# Analyze CRF as outcome
cat("\n\n################################################\n")
cat("ANALYSIS 2: CPA -> CRF (Contextual Reading Fluency)\n")
cat("################################################\n")
crf_results <- analyze_single_suppressor(
  data = child_des,  # Replace with your data frame name
  outcome = "crf",
  predictor = "cpa",
  suppressors = suppressors
)

# -----------------------------------------------------
# PART 2: MULTIPLE SUPPRESSOR ANALYSIS
# -----------------------------------------------------

cat("\n\n################################################\n")
cat("MULTIPLE SUPPRESSOR ANALYSIS\n")
cat("################################################\n")

# Function for multiple suppressor analysis
analyze_multiple_suppressors <- function(data, outcome, predictor, suppressors, covariate = "Age_Months") {
  
  # Baseline model
  baseline_formula <- as.formula(paste(outcome, "~", covariate, "+", predictor))
  baseline_model <- lm(baseline_formula, data = data)
  baseline_coef <- tidy(baseline_model) %>% 
    filter(term == predictor) %>% 
    pull(estimate)
  
  cat("\n========================================\n")
  cat("OUTCOME:", outcome, "\n")
  cat("========================================\n")
  cat("\nBaseline CPA coefficient:", round(baseline_coef, 4), "\n")
  
  # Test combinations of 2 suppressors
  cat("\n\nTwo-Suppressor Models:\n")
  cat("----------------------------------------\n")
  
  for (i in 1:(length(suppressors)-1)) {
    for (j in (i+1):length(suppressors)) {
      sup_combo <- c(suppressors[i], suppressors[j])
      formula <- as.formula(paste(outcome, "~", covariate, "+", predictor, "+", 
                                  paste(sup_combo, collapse = " + ")))
      model <- lm(formula, data = data)
      
      cpa_coef <- tidy(model) %>% 
        filter(term == predictor) %>% 
        pull(estimate)
      
      cat("\nModel with", paste(sup_combo, collapse = " + "), ":\n")
      cat("CPA coefficient:", round(cpa_coef, 4))
      cat(" (change:", round(cpa_coef - baseline_coef, 4), ")")
      
      if (sign(cpa_coef) != sign(baseline_coef)) {
        cat(" *** SIGN REVERSAL ***")
      }
      cat("\n")
    }
  }
  
  # Test combinations of 3 suppressors
  cat("\n\nThree-Suppressor Models:\n")
  cat("----------------------------------------\n")
  
  for (i in 1:(length(suppressors)-2)) {
    for (j in (i+1):(length(suppressors)-1)) {
      for (k in (j+1):length(suppressors)) {
        sup_combo <- c(suppressors[i], suppressors[j], suppressors[k])
        formula <- as.formula(paste(outcome, "~", covariate, "+", predictor, "+", 
                                    paste(sup_combo, collapse = " + ")))
        model <- lm(formula, data = data)
        
        cpa_coef <- tidy(model) %>% 
          filter(term == predictor) %>% 
          pull(estimate)
        
        cat("\nModel with", paste(sup_combo, collapse = " + "), ":\n")
        cat("CPA coefficient:", round(cpa_coef, 4))
        cat(" (change:", round(cpa_coef - baseline_coef, 4), ")")
        
        if (sign(cpa_coef) != sign(baseline_coef)) {
          cat(" *** SIGN REVERSAL ***")
        }
        cat("\n")
      }
    }
  }
  
  # Test all suppressors together
  cat("\n\nAll Suppressors Model:\n")
  cat("----------------------------------------\n")
  
  formula <- as.formula(paste(outcome, "~", covariate, "+", predictor, "+", 
                              paste(suppressors, collapse = " + ")))
  model <- lm(formula, data = data)
  
  print(summary(model)$coefficients)
  
  cpa_coef <- tidy(model) %>% 
    filter(term == predictor) %>% 
    pull(estimate)
  
  cat("\nCPA coefficient:", round(cpa_coef, 4))
  cat("\nChange from baseline:", round(cpa_coef - baseline_coef, 4))
  
  if (sign(cpa_coef) != sign(baseline_coef)) {
    cat("\n*** SIGN REVERSAL DETECTED ***")
  }
  
  return(model)
}

# Run multiple suppressor analysis for CWR
cwr_multiple <- analyze_multiple_suppressors(
  data = child_des,
  outcome = "cwr",
  predictor = "cpa",
  suppressors = suppressors
)

# Run multiple suppressor analysis for CRF
crf_multiple <- analyze_multiple_suppressors(
  data = child_des,
  outcome = "crf",
  predictor = "cpa",
  suppressors = suppressors
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
    CPA_Coefficient = sapply(single_results, function(x) round(x$cpa_coef, 4)),
    Change = sapply(single_results, function(x) round(x$change, 4)),
    Percent_Change = sapply(single_results, function(x) round(x$percent_change, 2)),
    row.names = NULL
  )
  return(summary_df)
}

# Create summaries
cwr_summary <- create_summary(cwr_results, "CWR")
crf_summary <- create_summary(crf_results, "CRF")

# Combine and print
full_summary <- rbind(cwr_summary, crf_summary)
print(full_summary)

# -----------------------------------------------------
# PART 4: VISUALIZATION (Optional)
# -----------------------------------------------------

# Plot coefficient changes
library(ggplot2)

plot_data <- full_summary %>%
  mutate(Suppressor = factor(Suppressor, levels = c("cma", "ran", "nwr", "vwm")))

p <- ggplot(plot_data, aes(x = Suppressor, y = CPA_Coefficient, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "CPA Coefficients with Different Suppressors",
       x = "Suppressor Variable",
       y = "CPA Coefficient") +
  theme(legend.position = "bottom")

print(p)

# Create change plot
p2 <- ggplot(plot_data, aes(x = Suppressor, y = Percent_Change, fill = Outcome)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_minimal() +
  labs(title = "Percent Change in CPA Coefficient Magnitude",
       x = "Suppressor Variable",
       y = "Percent Change (%)") +
  theme(legend.position = "bottom")

print(p2)
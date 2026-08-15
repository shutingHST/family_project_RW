library(foreign)
library(dplyr)
library(broom)
library(lavaan)


##read dataset

read.csv("data_parent_children.csv") -> data_parent_children


model3_path <- "

PCRF ~~ PRAN + PNWR + PVWM + PCPA + PCMA+hlp+Q19_P_Edu+Age_Months
PRAN ~~ PNWR + PVWM + PCPA + PCMA+hlp+Q19_P_Edu+Age_Months
PNWR ~~ PVWM + PCPA + PCMA+hlp+Q19_P_Edu+Age_Months
PVWM ~~ PCPA + PCMA+hlp+Q19_P_Edu+Age_Months
PCPA ~~ PCMA+hlp+Q19_P_Edu+Age_Months
PCMA~~hlp+Q19_P_Edu+Age_Months
hlp~~Q19_P_Edu+Age_Months
Q19_P_Edu~~Age_Months
PCRF~gender_p
PRAN~gender_p 
PNWR~gender_p 
PVWM ~gender_p
PCPA ~gender_p
PCMA~gender_p
hlp~gender_p
Q19_P_Edu~gender_p
Age_Months~gender_p


##control

# --------------------------------------------------
# Regression paths predicting CRF (indirect only)
# --------------------------------------------------

crf~ a*cpa + b*cma + c*ran + d*nwr + e*vwm+hlp+Q19_P_Edu+Age_Months+gender_p+PCRF+PRAN + PNWR + PVWM + PCPA + PCMA

# --------------------------------------------------
# Level 2 regressions: parent predictors -> mediators
# --------------------------------------------------

ran ~ c1*PCRF + c2*PRAN + c3*PNWR + c4*PVWM + c5*PCPA + c6*PCMA+hlp+Q19_P_Edu+Age_Months+gender_p
nwr ~ d1*PCRF + d2*PRAN + d3*PNWR + d4*PVWM + d5*PCPA + d6*PCMA+hlp+Q19_P_Edu+Age_Months+gender_p
vwm ~ e1*PCRF + e2*PRAN + e3*PNWR + e4*PVWM + e5*PCPA + e6*PCMA+hlp+Q19_P_Edu+Age_Months+gender_p
cpa ~ a1*PCRF + a2*PRAN + a3*PNWR + a4*PVWM + a5*PCPA + a6*PCMA+hlp+Q19_P_Edu+Age_Months+gender_p
cma ~ b1*PCRF + b2*PRAN + b3*PNWR + b4*PVWM + b5*PCPA + b6*PCMA+hlp+Q19_P_Edu+Age_Months+gender_p

# --------------------------------------------------
# Residual covariances among mediators
# --------------------------------------------------

cpa ~~ cma + ran + nwr + vwm
cma ~~ ran + nwr + vwm
ran ~~ nwr+vwm
nwr~~vwm

# --------------------------------------------------
# Indirect Effects Definitions
# --------------------------------------------------

# Indirect effects via CPA (phonological awareness)
ind_cpa_PCRF := a1*a
ind_cpa_PRAN := a2*a
ind_cpa_PNWR := a3*a
ind_cpa_PVWM := a4*a
ind_cpa_PCPA := a5*a
ind_cpa_PCMA := a6*a

# Indirect effects via CMA (morphological awareness)
ind_cma_PCRF := b1*b
ind_cma_PRAN := b2*b
ind_cma_PNWR := b3*b
ind_cma_PVWM := b4*b
ind_cma_PCPA := b5*b
ind_cma_PCMA := b6*b

# Indirect effects via RAN (rapid naming)
ind_ran_PCRF := c1*c
ind_ran_PRAN := c2*c
ind_ran_PNWR := c3*c
ind_ran_PVWM := c4*c
ind_ran_PCPA := c5*c
ind_ran_PCMA := c6*c

# Indirect effects via NWR (nonword repetition)
ind_nwr_PCRF := d1*d
ind_nwr_PRAN := d2*d
ind_nwr_PNWR := d3*d
ind_nwr_PVWM := d4*d
ind_nwr_PCPA := d5*d
ind_nwr_PCMA := d6*d

# Indirect effects via VWM (verbal working memory)
ind_vwm_PCRF := e1*e
ind_vwm_PRAN := e2*e
ind_vwm_PNWR := e3*e
ind_vwm_PVWM := e4*e
ind_vwm_PCPA := e5*e
ind_vwm_PCMA := e6*e

"


fit <- sem(model3_path, data = data_parent_children,se = "bootstrap",cluster = "FID",
            , bootstrap = 2000)

summary(fit, fit.measures = TRUE, standardized = TRUE)

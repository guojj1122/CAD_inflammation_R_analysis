# Main script
# Association of Composite Inflammatory Indices with
# Cerebral Artery Dissection: A Case-Control Study

############################################################
# Data preprocessing and missing data handling

# Missing BMI values were handled before statistical analyses.
# Multiple imputation was performed using the mice package.
# The original BMI variable was retained as "bmi_raw".
# Missing BMI values were handled using multiple imputation.
#
# Height and weight were imputed using predictive mean matching
# (PMM), and BMI was subsequently calculated using:
# BMI = weight (kg) / height (m)^2

# The imputed BMI variable was named "bmi" and was used as the
# covariate in the primary adjusted analyses, including
# multivariable logistic regression, restricted cubic spline
# analysis, ROC analysis, and subgroup analyses. 

# Sensitivity analyses were additionally performed using the
# original non-imputed BMI values to assess the robustness of the findings.

############################################################
# 1. Load packages
library(readxl)
library(dplyr)
library(openxlsx)

############################################################
# 2. Import dataset

df_analysis <- read_excel(
  
  "C:/Users/16946/Desktop/CAD_analysis.xlsx")

############################################################
# 3. Check dataset structure

dim(df_analysis)

names(df_analysis)

summary(df_analysis)

############################################################
# 4. Data type conversion

# Outcome

df_analysis$CAD <-
  as.numeric(df_analysis$CAD)

# Categorical variables

df_analysis$sex <-
  factor(
    df_analysis$sex,
    levels=c(0,1)
  )


df_analysis$hypertension <-
  factor(
    df_analysis$hypertension,
    levels=c(0,1)
  )


df_analysis$diabetes <-
  factor(
    df_analysis$diabetes,
    levels=c(0,1)
  )

############################################################
# 5. Convert continuous variables

numeric_vars <- c(
  
  "age",
  "SBP",
  "DBP",
  "BMI",
  "HbA1c",
  "RBC",
  "PLT",
  "NEU",
  "MON",
  "LYM",
  "TBiL",
  "FPG",
  "TC",
  "TG",
  "HDL_C",
  "LDL_C",
  "ALB",
  "GGT",
  "ALP",
  "ALT",
  "AST",
  "BUN",
  "Cr",
  "UA"
  
)

for(v in numeric_vars){
  
  df_analysis[[v]] <-
    as.numeric(df_analysis[[v]])
  
}

############################################################
# 6. Calculate composite inflammatory indices

# MHR
# monocyte / HDL-C

df_analysis$MHR <-
  df_analysis$MON /
  df_analysis$HDL_C


# NHR

df_analysis$NHR <-
  df_analysis$NEU /
  df_analysis$HDL_C



# NLR

df_analysis$NLR <-
  df_analysis$NEU /
  df_analysis$LYM


# PLR

df_analysis$PLR <-
  df_analysis$PLT /
  df_analysis$LYM


# MLR

df_analysis$MLR <-
  df_analysis$MON /
  df_analysis$LYM


# SII

df_analysis$SII <-
  (df_analysis$PLT *
     df_analysis$NEU) /
  df_analysis$LYM



# SIRI

df_analysis$SIRI <-
  (df_analysis$NEU *
     df_analysis$MON) /
  df_analysis$LYM



############################################################
# 7. Define inflammatory indices

indices <- c(
  "LHR",
  "MHR",
  "NHR",
  "PHR",
  "NLR",
  "PLR",
  "MLR",
  "SII",
  "SIRI"
)

############################################################
# 8. Save prepared dataset

write.xlsx(
  
  df_analysis,
  
  "CAD_analysis.xlsx"
  
)
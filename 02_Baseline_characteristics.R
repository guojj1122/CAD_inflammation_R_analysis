# Supplementary R Script
# Table 1. Baseline characteristics analysis

############################################################
# 1. Load packages
library(readxl)
library(dplyr)
library(car)
library(openxlsx)

############################################################
# 2. Import dataset

df_analysis <- read_excel(
  
  "C:/Users/16946/Desktop/CAD_analysis.xlsx")

############################################################
# Data cleaning
# Convert ALT values with detection-limit notation to numeric

df_analysis$ALT <- as.numeric(
  
  gsub(
    
    "<",
    
    "",
    
    df_analysis$ALT
    
  )
)

############################################################
# 3. Data preparation

# Outcome
# 0 = Control
# 1 = Cerebral artery dissection

df_analysis$CAD <- as.numeric(
  
  df_analysis$CAD
  
)

# Sex
# 0 = Male
# 1 = Female

df_analysis$sex <- factor(
  
  df_analysis$sex,
  
  levels = c(0,1),
  
  labels = c(
    "Male",
    "Female"
  )
  
)

# Hypertension

df_analysis$hypertension <- factor(
  
  df_analysis$hypertension,
  
  levels = c(0,1),
  
  labels = c(
    "No",
    "Yes"
  )
  
)

# Diabetes

df_analysis$diabetes <- factor(
  
  df_analysis$diabetes,
  
  levels = c(0,1),
  
  labels = c(
    "No",
    "Yes"
  )
  
)


############################################################
# 4. Define variables

continuous_vars <- c(
  
  "age",
  "bmi_raw",
  
  "SBP",
  "DBP",
  
  "RBC",
  "HbA1c",
  "PLT",
  "NEU",
  "MON",
  "LYM",
  
  "TBiL",
  "FPG",
  
  "TC",
  "TG",
  "HDL-C",
  "LDL-C",
  
  "ALB",
  "GGT",
  "ALP",
  "ALT",
  "AST",
  
  "BUN",
  "Cr",
  "UA",
  
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

categorical_vars <- c(
  
  "sex",
  "hypertension",
  "diabetes"
  
)

############################################################
# 5. Continuous variables analysis

continuous_results <- data.frame()


for(var in continuous_vars){
  
  
  control <-
    
    df_analysis %>%
    
    filter(CAD == 0) %>%
    
    pull(all_of(var))
  
  
  
  cad <-
    
    df_analysis %>%
    
    filter(CAD == 1) %>%
    
    pull(all_of(var))
  
  
  # Remove missing values
  
  control <- control[!is.na(control)]
  
  cad <- cad[!is.na(cad)]
  
  ##########################################################
  # Levene test for variance equality
  
  temp <- data.frame(
    
    value = c(control,cad),
    
    group = factor(
      
      c(
        
        rep("Control",length(control)),
        
        rep("CAD",length(cad))
        
      )
      
    )
    
  )
  
  levene_result <-
    
    leveneTest(
      
      value ~ group,
      
      data=temp,
      
      center="mean"
      
    )
  
  
  levene_p <-
    
    levene_result$`Pr(>F)`[1]
  
  ##########################################################
  # Independent sample t-test
  
  if(levene_p > 0.05){
    
    
    test <-
      
      t.test(
        
        control,
        cad,
        
        var.equal = TRUE
        
      )
    
    
    variance_method <-
      
      "Equal variances assumed"
    
    
  }else{
    
    test <-
      
      t.test(
        
        control,
        cad,
        
        var.equal = FALSE
        
      )
    
    
    variance_method <-
      
      "Equal variances not assumed"
    
  }

  
  ##########################################################
  # Save results
  
  continuous_results <-
    
    rbind(
      
      continuous_results,
      
      data.frame(
        
        Variable = var,
        
        Control_mean = mean(control),
        
        Control_SD = sd(control),
        
        CAD_mean = mean(cad),
        
        CAD_SD = sd(cad),
        
        Levene_P = levene_p,
        
        Variance_method = variance_method,
        
        t_value = unname(test$statistic),
        
        P_value = test$p.value
        
      )
      
    )
  
}

############################################################
# 6. Categorical variables analysis

categorical_results <- data.frame()

for(var in categorical_vars){
  
  
  tab <-
    
    table(
      
      df_analysis[[var]],
      
      df_analysis$CAD
      
    )
  
  
  chi_test <-
    
    chisq.test(
      
      tab,
      
      correct = FALSE
      
    )
  
  
  categorical_results <-
    
    rbind(
      
      categorical_results,
      
      data.frame(
        
        Variable = var,
        
        Chi_square = unname(
          
          chi_test$statistic
          
        ),
        
        P_value = chi_test$p.value
        
      )
      
    )
  
}

############################################################
# 7. Export results

# Round results to 3 decimal places for display

continuous_results_round <- continuous_results


continuous_results_round[,c(
  "Control_mean",
  "Control_SD",
  "CAD_mean",
  "CAD_SD",
  "t_value",
  "P_value"
)] <-
  
  lapply(
    
    continuous_results_round[,c(
      "Control_mean",
      "Control_SD",
      "CAD_mean",
      "CAD_SD",
      "t_value",
      "P_value"
    )],
    
    function(x){
      
      format(
        round(x,3),
        nsmall = 3
      )
      
    }
    
  )

# Remove Levene test information
continuous_results_round <-
  
  continuous_results_round %>%
  
  dplyr::select(
    
    Variable,
    Control_mean,
    Control_SD,
    CAD_mean,
    CAD_SD,
    t_value,
    P_value
    
  )

############################################################
# Categorical variables
categorical_results_round <- categorical_results


categorical_results_round[,c(
  "Chi_square",
  "P_value"
)] <-
  
  lapply(
    
    categorical_results_round[,c(
      "Chi_square",
      "P_value"
    )],
    
    function(x){
      
      format(
        round(x,3),
        nsmall = 3
      )
      
    }
    
  )



# View

View(continuous_results_round)

View(categorical_results_round)

############################################################
# 8.Export Excel

write.xlsx(
  
  list(
    
    Continuous_variables = continuous_results_round,
    
    Categorical_variables = categorical_results_round
    
  ), "Table1_baseline_results.xlsx")
#ROC analysis
#Diagnostic performance of inflammatory indices

# Load packages
library(readxl)
library(pROC)
library(openxlsx)

############################################################
# 1. Read dataset
df_analysis <- read_excel(
  
  "C:/Users/16946/Desktop/CAD_analysis.xlsx") 

dim(df_analysis)

View(df_analysis) 

summary(df_analysis)

############################################################
# 2. Build combined inflammatory indices model
# Nine inflammatory indices combined model

combined_model <-
  
  glm(
    
    CAD ~
      
      LHR +
      MHR +
      NHR +
      PHR +
      NLR +
      PLR +
      MLR +
      SII +
      SIRI,
    
    data = df_analysis,
    
    family = binomial()
    
  )

############################################################
# Predicted probability of combined model

df_analysis$Combinedmodel <-
  
  predict(
    
    combined_model,
    
    newdata = df_analysis,
    
    type = "response"
    
  )


############################################################
# 3. Define ROC variables

roc_indices <- c(
  
  "LHR",
  "MHR",
  "NHR",
  "PHR",
  "NLR",
  "PLR",
  "MLR",
  "SII",
  "SIRI",
  "Combinedmodel"
  
)

############################################################
# 4. Calculate prevalence
# Used for PPV and NPV
prevalence <-
  
  mean(
    
    df_analysis$CAD
    
  )

############################################################
# 5. Function:
# Test whether AUC > 0.5
auc_p_value <- function(roc_object){
  
  
  auc_value <-
    
    as.numeric(
      
      auc(
        
        roc_object
        
      )
      
    )
  
  
  auc_ci <-
    
    ci.auc(
      
      roc_object
      
    )
  
  # Standard error estimated from 95%CI
  
  se_auc <-
    
    (
      
      auc_ci[3] -
        auc_ci[1]
      
    ) /
    
    (2*1.96)
  
  z_value <-
    
    (
      
      auc_value - 0.5
      
    ) /
    
    se_auc
  
  p_value <-
    
    2 *
    
    (
      
      1 -
        pnorm(
          
          abs(z_value)
          
        )
      
    )
  
  
  
  return(
    
    p_value
    
  )
  
  
}

############################################################
# 6. ROC analysis
roc_results <- data.frame()
for(index in roc_indices){
  
# ROC curve
 
 roc_model <-
   
   roc(
     
     response=df_analysis$CAD,
     
     predictor=df_analysis[[index]],
     
     levels=c(0,1),
     
     direction="<",
     
     ci=TRUE
     
   )
 

# AUC and CI
  auc_value <-
    
    as.numeric(
      
      auc(
        
        roc_model
        
      )
      
    )
  
  auc_ci <-
    
    ci.auc(
      
      roc_model
      
    )
  
  auc_p <-
    
    auc_p_value(
      
      roc_model
      
    )
  

# Optimal cutoff by Youden index
  cutoff <-
    
    coords(
      
      roc_model,
      
      x = "best",
      
      best.method = "youden",
      
      ret = c(
        
        "threshold",
        "sensitivity",
        "specificity"
        
      ),
      
      transpose = FALSE
      
    )
  
  threshold <-
    
    as.numeric(
      
      cutoff["threshold"]
      
    )
  
  sensitivity <-
    
    as.numeric(
      
      cutoff["sensitivity"]
      
    )
  
  
  
  specificity <-
    
    as.numeric(
      
      cutoff["specificity"]
      
    )
  

# PPV and NPV
# For case-control study:
# based on proportion of CAD cases

  PPV <-
    
    (
      
      sensitivity * prevalence
      
    ) /
    
    (
      
      sensitivity * prevalence +
        
        (1-specificity) *
        
        (1-prevalence)
      
    )

  NPV <-
    
    (
      
      specificity *
        
        (1-prevalence)
      
    ) /
    
    (
      
      (1-sensitivity) *
        
        prevalence +
        
        specificity *
        
        (1-prevalence)
      
    )
  

# Likelihood ratios
  LR_positive <-
    
    sensitivity /
    
    (1-specificity)
  
  
  
  LR_negative <-
    
    (1-sensitivity) /
    
    specificity
  
  
  
  DOR <-
    
    LR_positive /
    
    LR_negative
  
  
  
  Youden_index <- sensitivity +specificity -1
  

# Save results
  roc_results <-
    
    rbind(
      
      roc_results,
      
      data.frame(
        
        Index = index,
        
        AUC = auc_value,
        
        AUC_95CI_low = as.numeric(
          
          auc_ci[1]
          
        ),
        
        AUC_95CI_high = as.numeric(
          
          auc_ci[3]
          
        ),
        
        AUC_P = auc_p,
        
        Cutoff = threshold,
        
        Sensitivity = sensitivity,
        
        Specificity = specificity,
        
        PPV = PPV,
        
        NPV = NPV,
        
        LR_positive = LR_positive,
        
        LR_negative = LR_negative,
        
        DOR = DOR,
        
        Youden_index = Youden_index
        
      )
      
    )
  
  
}

############################################################
# 7. Export results
roc_results_round <- roc_results

roc_results_round[,c(2:4,6:14)] <-
  
  lapply(
    
    roc_results_round[,c(2:4,6:14)],
    
    function(x) round(x,3)
    
  )
roc_results_round$AUC_P <-
  
  ifelse(
    
    roc_results$AUC_P < 0.001,
    
    "<0.001",
    
    round(
      roc_results$AUC_P,
      3
    )
    
  )

View(roc_results_round)

write.xlsx(
  
  roc_results_round,
  
  file="ROC_results_round.xlsx"
  
)
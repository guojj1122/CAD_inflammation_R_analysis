############################################################
# Logistic regression analysis
#
# Association between composite inflammatory indices and cerebral artery dissection

############################################################
# 1. Load packages

library(readxl)
library(dplyr)
library(broom)
library(openxlsx)

df_analysis <- read_excel(
  
  "C:/Users/16946/Desktop/CAD_analysis.xlsx") 


############################################################
# 2. Data preparation



# Convert categorical variables to factors


df_analysis$sex <-
  
  factor(
    
    df_analysis$sex,
    
    levels=c(0,1),
    
    labels=c(
      "Male",
      "Female"
    )
    
  )



df_analysis$hypertension <-
  
  factor(
    
    df_analysis$hypertension,
    
    levels=c(0,1),
    
    labels=c(
      "No",
      "Yes"
    )
    
  )



df_analysis$diabetes <-
  
  factor(
    
    df_analysis$diabetes,
    
    levels=c(0,1),
    
    labels=c(
      "No",
      "Yes"
    )
    
  )



############################################################
# 3. Scale transformation of inflammatory indices
#
# For interpretation of OR estimates



df_analysis$MHR_10 <-
  
  df_analysis$MHR * 10



df_analysis$PHR_100 <-
  
  df_analysis$PHR / 100



df_analysis$PLR_100 <-
  
  df_analysis$PLR / 100



df_analysis$MLR_10 <-
  
  df_analysis$MLR * 10



df_analysis$SII_100 <-
  
  df_analysis$SII / 100



############################################################
# 4. Define inflammatory indices


continuous_indices <- c(
  
  "LHR",
  "MHR_10",
  "NHR",
  "PHR_100",
  "NLR",
  "PLR_100",
  "MLR_10",
  "SII_100",
  "SIRI"
  
)


quartile_indices <- continuous_indices



############################################################
# 5. Continuous logistic regression



logistic_results <- data.frame()



for(index in continuous_indices){
  
  
  models <- list(
    
    
    Crude =
      
      glm(
        
        as.formula(
          
          paste(
            
            "CAD ~",
            
            index
            
          )
          
        ),
        
        data=df_analysis,
        
        family=binomial()
        
      ),
    
    
    
    Model1 =
      
      glm(
        
        as.formula(
          
          paste(
            
            "CAD ~",
            
            index,
            
            "+ age + sex + bmi"
            
          )
          
        ),
        
        data=df_analysis,
        
        family=binomial()
        
      ),
    
    
    
    Model2 =
      
      glm(
        
        as.formula(
          
          paste(
            
            "CAD ~",
            
            index,
            
            "+ age + sex + bmi + hypertension + diabetes"
            
          )
          
        ),
        
        data=df_analysis,
        
        family=binomial()
        
      )
    
    
  )
  
  
  
  
  for(model_name in names(models)){
    
    
    result <-
      
      tidy(
        
        models[[model_name]],
        
        exponentiate=TRUE,
        
        conf.int=TRUE
        
      ) %>%
      
      filter(
        
        term == index
        
      )
    
    
    
    logistic_results <-
      
      rbind(
        
        logistic_results,
        
        data.frame(
          
          Index=index,
          
          Model=model_name,
          
          OR=result$estimate,
          
          CI_low=result$conf.low,
          
          CI_high=result$conf.high,
          
          P=result$p.value
          
        )
        
      )
    
    
  }
  
}



############################################################
# 6. Quartile logistic regression


quartile_results <- data.frame()



for(index in quartile_indices){
  
  
  qvar <- paste0(index,"_Q")
  
  
  # Generate quartiles
  
  df_analysis[[qvar]] <-
    
    cut(
      
      df_analysis[[index]],
      
      breaks =
        
        quantile(
          
          df_analysis[[index]],
          
          probs=c(
            
            0,
            0.25,
            0.50,
            0.75,
            1
            
          ),
          
          na.rm=TRUE
          
        ),
      
      labels=c(
        
        "Q1",
        "Q2",
        "Q3",
        "Q4"
        
      ),
      
      include.lowest=TRUE
      
    )
  
  
  
  # Set Q1 as reference
  
  df_analysis[[qvar]] <-
    
    relevel(
      
      factor(df_analysis[[qvar]]),
      
      ref="Q1"
      
    )
  
  
  
  
  ############################################################
  # Three logistic regression models
  
  
  models <- list(
    
    
    Crude =
      
      glm(
        
        as.formula(
          
          paste(
            
            "CAD ~",
            
            qvar
            
          )
          
        ),
        
        data=df_analysis,
        
        family=binomial()
        
      ),
    
    
    
    Model1 =
      
      glm(
        
        as.formula(
          
          paste(
            
            "CAD ~",
            
            qvar,
            
            "+ age + sex + bmi"
            
          )
          
        ),
        
        data=df_analysis,
        
        family=binomial()
        
      ),
    
    
    
    Model2 =
      
      glm(
        
        as.formula(
          
          paste(
            
            "CAD ~",
            
            qvar,
            
            "+ age + sex + bmi + hypertension + diabetes"
            
          )
          
        ),
        
        data=df_analysis,
        
        family=binomial()
        
      )
    
    
  )
  
  
  
  
  ############################################################
  # Extract results
  
  
  for(model_name in names(models)){
    
    
    result <-
      
      tidy(
        
        models[[model_name]],
        
        exponentiate=TRUE,
        
        conf.int=TRUE
        
      ) %>%
      
      filter(
        
        grepl(
          
          qvar,
          
          term
          
        )
        
      )
    
    
    
    quartile_results <-
      
      rbind(
        
        quartile_results,
        
        data.frame(
          
          Index=index,
          
          Model=model_name,
          
          Term=result$term,
          
          OR=result$estimate,
          
          CI_low=result$conf.low,
          
          CI_high=result$conf.high,
          
          P=result$p.value
          
        )
        
      )
    
    
  }
  
  
}


############################################################
# 7. Trend analysis


trend_results <- data.frame()


for(index in quartile_indices){
  
  
  qvar <- paste0(index,"_Q")
  
  
  df_analysis$trend_variable <-
    
    as.numeric(
      df_analysis[[qvar]]
    )
  
  
  
  models <- list(
    
    Crude =
      glm(
        CAD ~ trend_variable,
        data=df_analysis,
        family=binomial()
      ),
    
    
    Model1 =
      glm(
        CAD ~ trend_variable +
          age +
          sex +
          bmi,
        data=df_analysis,
        family=binomial()
      ),
    
    
    Model2 =
      glm(
        CAD ~ trend_variable +
          age +
          sex +
          bmi +
          hypertension +
          diabetes,
        data=df_analysis,
        family=binomial()
      )
    
  )
  
  
  
  for(model_name in names(models)){
    
    
    p_value <-
      
      summary(models[[model_name]])$coefficients[
        
        "trend_variable",
        
        "Pr(>|z|)"
        
      ]
    
    
    trend_results <-
      
      rbind(
        
        trend_results,
        
        data.frame(
          
          Index=index,
          
          Model=model_name,
          
          P_for_trend=p_value
          
        )
        
      )
    
    
  }
  
}


############################################################
# 8. FDR correction


logistic_results$FDR_P <-
  
  p.adjust(
    
    logistic_results$P,
    
    method="BH"
    
  )



quartile_results$FDR_P <-
  
  p.adjust(
    
    quartile_results$P,
    
    method="BH"
    
  )



trend_results$FDR_P <-
  
  p.adjust(
    
    trend_results$P_for_trend,
    
    method="BH"
    
  )



############################################################
# 9. Format results for display

format_p <- function(x){
  
  ifelse(
    
    x < 0.001,
    
    "<0.001",
    
    format(
      
      round(x,3),
      
      nsmall=3
      
    )
    
  )
  
}



logistic_results_round <-
  
  logistic_results


logistic_results_round[,c(
  
  "OR",
  "CI_low",
  "CI_high",
  "P",
  "FDR_P"
  
)] <-
  
  lapply(
    
    logistic_results_round[,c(
      
      "OR",
      "CI_low",
      "CI_high",
      "P",
      "FDR_P"
      
    )],
    
    function(x){
      
      format(
        
        round(x,3),
        
        nsmall=3
        
      )
      
    }
    
  )



quartile_results_round <-
  
  quartile_results



quartile_results_round[,c(
  
  "OR",
  "CI_low",
  "CI_high",
  "P",
  "FDR_P"
  
)] <-
  
  lapply(
    
    quartile_results_round[,c(
      
      "OR",
      "CI_low",
      "CI_high",
      "P",
      "FDR_P"
      
    )],
    
    function(x){
      
      format(
        
        round(x,3),
        
        nsmall=3
        
      )
      
    }
    
  )



trend_results_round <-
  
  trend_results



trend_results_round[,c(
  
  "P_for_trend",
  "FDR_P"
  
)] <-
  
  lapply(
    
    trend_results_round[,c(
      
      "P_for_trend",
      "FDR_P"
      
    )],
    
    function(x){
      
      format(
        
        round(x,3),
        
        nsmall=3
        
      )
      
    }
    
  )

############################################################
# 10. Preview

View(logistic_results_round)

View(quartile_results_round)

View(trend_results_round)



############################################################
# 11. Export


write.xlsx(
  
  list(
    
    Continuous_logistic =
      logistic_results_round,
    
    
    Quartile_logistic =
      quartile_results_round,
    
    
    Trend =
      trend_results_round
    
    
  ),
  
  "C:/Users/16946/Desktop/Logistic_results.xlsx"
  
)
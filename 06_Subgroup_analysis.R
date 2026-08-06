############################################################
# Subgroup analysis and forest plot
#
# Association between PLR and cerebral artery dissection

library(readxl)
library(dplyr)
library(forestploter)
library(grid)



# =========================
# 1. Read data
# =========================


df <- read_excel(
  "C:/Users/16946/Desktop/CAD_analysis.xlsx"
)



# =========================
# 2. Data preparation
# =========================


df <- df %>%
  mutate(
    
    CAD = as.numeric(CAD),
    
    sex = case_when(
      sex == 1 ~ "Male",
      sex == 0 ~ "Female",
      TRUE ~ NA_character_
    ),
    
    sex = factor(
      sex,
      levels = c("Female","Male")
    ),
    
    
    age_group = case_when(
      age <45 ~ "<45 years",
      age >=45 ~ "≥45 years",
      TRUE ~ NA_character_
    ),
    
    age_group = factor(
      age_group,
      levels=c(
        "<45 years",
        "≥45 years"
      )
    ),
    
    
    BMI_group = case_when(
      bmi <25 ~ "<25 kg/m²",
      bmi >=25 ~ "≥25 kg/m²",
      TRUE ~ NA_character_
    ),
    
    BMI_group = factor(
      BMI_group,
      levels=c(
        "<25 kg/m²",
        "≥25 kg/m²"
      )
    ),
    
    
    hypertension = case_when(
      hypertension ==1 ~ "Yes",
      hypertension ==0 ~ "No",
      TRUE ~ NA_character_
    ),
    
    hypertension = factor(
      hypertension,
      levels=c(
        "No",
        "Yes"
      )
    ),
    
    
    diabetes = case_when(
      diabetes ==1 ~ "Yes",
      diabetes ==0 ~ "No",
      TRUE ~ NA_character_
    ),
    
    diabetes = factor(
      diabetes,
      levels=c(
        "No",
        "Yes"
      )
    ),
    
    
    PLR_100 = PLR/100
    
  )



# =========================
# 3. Dataset for subgroup analysis
# =========================


df_plr <- df %>%
  select(
    CAD,
    PLR,
    PLR_100,
    age,
    age_group,
    sex,
    bmi,
    BMI_group,
    hypertension,
    diabetes
  ) %>%
  filter(
    complete.cases(
      CAD,
      PLR_100,
      age,
      age_group,
      sex,
      bmi,
      BMI_group,
      hypertension,
      diabetes
    )
  ) %>%
  droplevels()



table(df_plr$age_group, df_plr$CAD)
table(df_plr$sex, df_plr$CAD)
table(df_plr$BMI_group, df_plr$CAD)
table(df_plr$hypertension, df_plr$CAD)
table(df_plr$diabetes, df_plr$CAD)



# =========================
# 4. Helper functions
# =========================


fmt_p <- function(p){
  
  if(is.na(p)) return("")
  
  if(p <0.001) return("<0.001")
  
  if(p >0.999) return(">0.999")
  
  sprintf("%.3f",p)
  
}



fmt_or <- function(or,lcl,ucl){
  
  sprintf(
    "%.3f (%.3f, %.3f)",
    or,
    lcl,
    ucl
  )
  
}
  
  get_or <- function(fit, exposure = "PLR_use") {
    
    sm <- summary(fit)$coefficients
    
    beta <- sm[exposure, "Estimate"]
    
    p <- sm[exposure, "Pr(>|z|)"]
    
    
    # 使用profile likelihood CI
    CI <- suppressMessages(
      confint(fit, parm = exposure)
    )
    
    
    OR <- exp(beta)
    
    LCL <- exp(CI[1])
    
    UCL <- exp(CI[2])
    
    
    data.frame(
      estimate = OR,
      conf.low = LCL,
      conf.high = UCL,
      ORCI = fmt_or(OR, LCL, UCL),
      P = fmt_p(p)
    )
  }
  


# =========================
# 5. Overall model
# =========================


fit_all <- glm(
  
  CAD ~ PLR_100+
    age+
    sex+
    bmi+
    hypertension+
    diabetes,
  
  data=df_plr,
  
  family=binomial()
  
)



res_all0 <- get_or(
  fit_all,
  "PLR_100"
)



res_all <- data.frame(
  
  Subgroup="All patients",
  
  TotalPct=sprintf(
    "%d (100.00)",
    nrow(df_plr)
  ),
  
  ORCI=res_all0$ORCI,
  
  P=res_all0$P,
  
  Pint="",
  
  estimate=res_all0$estimate,
  
  conf.low=res_all0$conf.low,
  
  conf.high=res_all0$conf.high
  
)



# =========================
# 6. Subgroup analysis
# =========================


subgroup_result <- function(
    data,
    subgroup,
    subgroup_label
){
  
  
  covars <- c(
    "age",
    "sex",
    "bmi",
    "hypertension",
    "diabetes"
  )
  
  
  if(subgroup=="age_group"){
    
    adjust_vars <- setdiff(
      covars,
      "age"
    )
    
  } else if(subgroup=="BMI_group"){
    
    adjust_vars <- setdiff(
      covars,
      "bmi"
    )
    
  } else {
    
    adjust_vars <- setdiff(
      covars,
      subgroup
    )
    
  }
  
  
  levs <- levels(data[[subgroup]])
  
  
  res_list <- list()
  
  
  for(lv in levs){
    
    
    d <- data %>%
      filter(
        .data[[subgroup]]==lv
      ) %>%
      droplevels()
    
    
    adjust_vars_use <- adjust_vars[
      sapply(
        adjust_vars,
        function(v){
          
          if(is.factor(d[[v]])){
            
            n_distinct(d[[v]])>=2
            
          }else{
            
            TRUE
            
          }
          
        }
      )
    ]
    
    
    fit <- glm(
      
      reformulate(
        c(
          "PLR_100",
          adjust_vars_use
        ),
        response="CAD"
      ),
      
      data=d,
      
      family=binomial()
      
    )
    
    
    tmp <- get_or(
      fit,
      "PLR_100"
    )
    
    
    res_list[[lv]] <- data.frame(
      
      Subgroup=paste0(
        "   ",
        lv
      ),
      
      TotalPct=sprintf(
        "%d (%.2f)",
        nrow(d),
        100*nrow(d)/nrow(data)
      ),
      
      ORCI=tmp$ORCI,
      
      P=tmp$P,
      
      Pint="",
      
      estimate=tmp$estimate,
      
      conf.low=tmp$conf.low,
      
      conf.high=tmp$conf.high
      
    )
    
  }
  
  
  res_detail <- bind_rows(res_list)
  
  
  fit0 <- glm(
    
    reformulate(
      c(
        "PLR_100",
        subgroup,
        adjust_vars
      ),
      response="CAD"
    ),
    
    data=data,
    
    family=binomial()
    
  )
  
  
  fit1 <- glm(
    
    as.formula(
      paste(
        "CAD ~ PLR_100*",
        subgroup,
        "+",
        paste(
          adjust_vars,
          collapse=" + "
        )
      )
    ),
    
    data=data,
    
    family=binomial()
    
  )
  
  
  p_int <- anova(
    fit0,
    fit1,
    test="Chisq"
  )$`Pr(>Chi)`[2]
  
  
  header <- data.frame(
    
    Subgroup=subgroup_label,
    
    TotalPct="",
    
    ORCI="",
    
    P="",
    
    Pint=fmt_p(p_int),
    
    estimate=NA,
    
    conf.low=NA,
    
    conf.high=NA
    
  )
  
  
  bind_rows(
    header,
    res_detail
  )
  
}



# =========================
# 7. Combine results
# =========================


plot_df <- bind_rows(
  
  res_all,
  
  subgroup_result( df_plr, "sex", "Sex"),
  
  
  # subgroup_result(df_plr, "age_group", "Age"),
  
  subgroup_result( df_plr,"BMI_group", "BMI"),
  
  subgroup_result(df_plr,"hypertension","Hypertension"),
  
  #subgroup_result(df_plr,"diabetes","Diabetes"))


plot_df <- plot_df %>%
  mutate(
    `Forest plot` =
      paste(
        rep(" ",
            22),
        collapse=""
      )
  )

# =========================
# 8. Forest table
# =========================

# =========================
# Forest table
# =========================
# =========================
# Forest table
# =========================

plot_df_show <- plot_df %>%
  dplyr::select(
    Subgroup,
    TotalPct,
    `Forest plot`,
    ORCI,
    P,
    Pint
  )
 

names(plot_df_show) <- c(
  "Subgroup",
  "Total (%)",
  "Forest plot",
  "OR (95% CI)",
  "P value",
  "P for interaction"
)


# =========================
# 9. Forest plot
# =========================


tm <- forest_theme(
  
  base_size=10,
  
  base_family="serif",
  
  ci_pch = 15,
  ci_lwd = 1.5,
  ci_col = "#377eb8",
  ci_fill = "#377eb8",
  ci_Theight = 0.20,
  
  refline_gp=gpar(
    col="black",
    lty="dashed",
    lwd=1
  )
  
)



p_PLR <- forest(
  
  plot_df_show,
  
  est=plot_df$estimate,
  
  lower=plot_df$conf.low,
  
  upper=plot_df$conf.high,
  
  ci_column=3,
  
  ref_line=1,
  
  xlim=c(0,4),
  
  ticks_at=0:4,
  
  xlab="PLR",
  
  theme=tm
  
)

p_PLR

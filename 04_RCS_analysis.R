############################################################
# Restricted Cubic Spline Analysis
# Association between composite inflammatory indices and CAD

rm(list = ls())

library(rms)
library(ggplot2)
library(dplyr)
library(readxl)  

# Read data
df_analysis <- read_excel("C:/Users/16946/Desktop/CAD_analysis.xlsx")

# Convert categorical variables to factors
df_analysis <- df_analysis %>% mutate(
  diabetes = as.factor(diabetes),
  hypertension = as.factor(hypertension),
  sex = as.factor(sex)
)

cat("Sample size =", nrow(df_analysis), "\n")

# Set datadist
dd <- datadist(df_analysis)
options(datadist = "dd")

# Set reference value (median of PHR)
ref_PHR <- median(df_analysis$PHR, na.rm = TRUE)

# RCS logistic regression model
fit_rcs <- lrm(
  CAD ~ rcs(PHR, 4) + hypertension + diabetes + sex + age + bmi,
  data = df_analysis,
  x = TRUE,
  y = TRUE
)

print(fit_rcs)
anova(fit_rcs)

# Extract P values
a <- anova(fit_rcs)
rn <- rownames(a)

overall_p <- a[which(rn == "PHR"), "P"]
nonlinear_p <- a[grep("Nonlinear", rn)[1], "P"]

fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 0.001) return("<0.001")
  sprintf("%.3f", p)
}

overall_p_txt <- fmt_p(overall_p)
nonlinear_p_txt <- fmt_p(nonlinear_p)

# Prediction
if (!exists("Predict", where = "package:rms")) {
  cat("Error: Predict function not found\n")
} else {
  pred <- Predict(
    fit_rcs,
    PHR,
    fun = exp,
    ref.zero = TRUE,
    conf.int = 0.95
  )
  
  pred_df <- as.data.frame(pred)
  pred_df_plot <- pred_df
  
  # X-axis range
  x_low <- min(pred_df_plot$PHR, na.rm = TRUE)
  x_high <- max(pred_df_plot$PHR, na.rm = TRUE)
  
  # Label position
  label_x_position <- ref_PHR + (x_high - x_low) * 0.05
  
  if(label_x_position > x_high) {
    label_x_position <- x_low + (x_high - x_low) * 0.7
  }
  
  # Y-axis position
  y_max <- max(pred_df_plot$upper, na.rm = TRUE)
  label_y_overall <- y_max * 0.96
  label_y_nonlinear <- y_max * 0.89
  
  # Plot
  p <- ggplot(pred_df_plot, aes(x = PHR, y = yhat)) +
    geom_ribbon(aes(ymin = lower, ymax = upper),
                fill = "#E6A0A0", alpha = 0.65) +
    geom_line(color = "#EF3B2C", linewidth = 1.1) +
    geom_hline(yintercept = 1,
               linetype = "dashed",
               color = "black",
               linewidth = 0.8) +
    geom_vline(xintercept = ref_PHR,
               linetype = "dashed",
               color = "black",
               linewidth = 0.8) +
    annotate(
      "text",
      x = label_x_position,
      y = label_y_overall,
      hjust = 0,
      vjust = 1,
      size = 4,
      fontface = "bold",
      parse = TRUE,
      label = paste0("bolditalic(P)~bold('for overall ", overall_p_txt, "')")
    ) +
    annotate(
      "text",
      x = label_x_position,
      y = label_y_nonlinear,
      hjust = 0,
      vjust = 1,
      size = 4,
      fontface = "bold",
      parse = TRUE,
      label = paste0("bolditalic(P)~bold('for nonlinearity ", nonlinear_p_txt, "')")
    ) +
    annotate(
      "text",
      x = ref_PHR,
      y = 0.93,
      label = round(ref_PHR, 2),
      color = "black",
      size = 5,
      fontface = "bold"
    ) +
    labs(
      x = "PHR",
      y = "OR (95% CI)"
    ) +
    coord_cartesian(
      xlim = c(x_low, x_high),
      ylim=c(
        min(pred_df_plot$lower)*0.95,
        max(pred_df_plot$upper)*1.05
    )) +
    theme_classic() +
    theme(
      text = element_text(size = 12, face = "bold"),
      axis.title = element_text(size = 12, face = "bold"),
      axis.text = element_text(size = 12, face = "bold", color = "black"),
      axis.line = element_line(color = "black", linewidth = 0.8),
      axis.ticks = element_line(color = "black", linewidth = 0.8)
    )
  
  print(p)  
}
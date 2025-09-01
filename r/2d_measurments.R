# Load libraries
library(dplyr)
library(ggplot2)

# Function to analyze and visualize a dataset
analyze_region <- function(file_path, region_name) {
  data <- read.csv(file_path)
  data_clean <- data %>% filter(!is.na(MFT))
  
  # Ratios
  ratios <- data_clean %>%
    mutate(
      ratio_EH_MFT = ifelse(!is.na(EH) & MFT > 0, EH / MFT, NA),
      ratio_HH_MFT = ifelse(!is.na(HH) & MFT > 0, HH / MFT, NA)
    )
  
  cat(paste("\nRegion:", region_name, "\n"))
  cat("EH/MFT Ratio Summary:\n")
  print(summary(ratios$ratio_EH_MFT))
  cat("SD:", sd(ratios$ratio_EH_MFT, na.rm = TRUE), "\n")
  cat("HH/MFT Ratio Summary:\n")
  print(summary(ratios$ratio_HH_MFT))
  cat("SD:", sd(ratios$ratio_HH_MFT, na.rm = TRUE), "\n")
  
  # Regressions
  if (sum(complete.cases(data_clean$HH, data_clean$MFT)) > 1) {
    model_hh <- lm(MFT ~ HH, data = data_clean)
    cat("MFT ~ HH Regression:\n")
    print(summary(model_hh))
    
    preds_hh <- predict(model_hh)
    rmse_hh <- sqrt(mean((data_clean$MFT[complete.cases(data_clean$HH, data_clean$MFT)] - preds_hh)^2, na.rm = TRUE))
    cat("RMSE (HH model):", rmse_hh, "\n")
  } else {
    cat("Insufficient data for HH regression.\n")
  }
  
  if (sum(complete.cases(data_clean$EH, data_clean$MFT)) > 1) {
    model_eh <- lm(MFT ~ EH, data = data_clean)
    cat("MFT ~ EH Regression:\n")
    print(summary(model_eh))
    
    preds_eh <- predict(model_eh)
    rmse_eh <- sqrt(mean((data_clean$MFT[complete.cases(data_clean$EH, data_clean$MFT)] - preds_eh)^2, na.rm = TRUE))
    cat("RMSE (EH model):", rmse_eh, "\n")
  } else {
    cat("Insufficient data for EH regression.\n")
  }
  
  # Scatter plot for MFT vs HH
  if (sum(complete.cases(data_clean$HH, data_clean$MFT)) > 1) {
    hh_plot <- ggplot(data_clean, aes(x = HH, y = MFT)) +
      geom_point(color = "#1f77b4") +
      geom_smooth(method = "lm", color = "#ff7f0e", se = FALSE) +
      labs(title = paste("MFT vs HH -", region_name),
           x = "Head Height (HH)", y = "Fork Length (MFT)") +
      theme_minimal()
    print(hh_plot)
  }
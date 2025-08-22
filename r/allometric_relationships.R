library(readxl)
library(tidyr)
library(dplyr)
library(stats)
library(ggplot2)
library(GGally)
library(gridExtra)

sheets <- c(
  "pink_snapper_measurements_abrol",
  "pink_snapper_measurements_swc",
  "pink_snapper_measurements_geogr"
)

sheet_titles <- c(
  "pink_snapper_measurements_abrol" = "Abrolhos_Islands",
  "pink_snapper_measurements_swc" = "Southwest_Corner",
  "pink_snapper_measurements_geogr" = "Geographe_Bay"
)

for (sheet in sheets) {
  title <- sheet_titles[sheet]
  
  data <- read_excel("snapper_data_master.xlsx", sheet = sheet)
  
  data <- data %>%
    mutate(MeasurementKey = gsub("^[0-9]+\\. ", "", MeasurementKey),
           MeasurementKey = gsub("\\(.*\\)", "", MeasurementKey),
           MeasurementKey = trimws(MeasurementKey),
           MeasurementKey = gsub(" ", "_", MeasurementKey))
  
  wide_data <- data %>%
    pivot_wider(id_cols = UniqueID, names_from = MeasurementKey, values_from = Length) %>%
    select(UniqueID, MFT = Mouth_to_Fork_of_the_Tail, HH = Head_Height, EH = Eye_Height,
           EF1 = Fin_attachement_Eye_to_side_Fin, EF2 = Fin_attachement_Eye_to_bottom_Fin,
           EBF = Eye_to_Back_Fin) %>%
    na.omit()
  
  # Univariable LMs for measurements vs MFT with plots
  measurements <- c("HH", "EH", "EF1", "EF2", "EBF")
  for (meas in measurements) {
    lm_model <- lm(as.formula(paste("MFT ~", meas)), data = wide_data)
    print(paste("LM MFT ~", meas, "-", title))
    print(summary(lm_model))
    
    p <- ggplot(wide_data, aes(x = !!sym(meas), y = MFT)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      ggtitle(paste("MFT vs", meas, "-", title)) +
      theme_minimal()
    print(p)
    ggsave(filename = paste0("MFT_vs_", meas, "_", title, ".png"), plot = p, path = getwd(), width = 6, height = 4)
  }
  
  # Pairs plot for predictors
  predictors_lengths <- wide_data %>% select(HH, EH, EF1, EF2, EBF)
  p_pairs_lengths <- ggpairs(predictors_lengths) + ggtitle(paste("Pairs Plot: Lengths -", title))
  print(p_pairs_lengths)
  ggsave(filename = paste0("Pairs_Plot_Lengths_", title, ".png"), plot = p_pairs_lengths, path = getwd(), width = 8, height = 8)
  
  # Multivariable LM for measurements
  lm_full_lengths <- lm(MFT ~ HH + EH + EF1 + EF2 + EBF, data = wide_data)
  print(paste("Full LM Lengths -", title))
  print(summary(lm_full_lengths))
  
  # Diagnostic plots for full model
  png(filename = paste0("Diagnostics_Full_LM_Lengths_", title, ".png"), width = 800, height = 600)
  par(mfrow = c(2, 2))
  plot(lm_full_lengths, main = paste("Diagnostics: Full LM Lengths -", title))
  dev.off()
  
  # Fitted vs Actual
  wide_data$fitted_full <- predict(lm_full_lengths)
  p_full <- ggplot(wide_data, aes(x = fitted_full, y = MFT)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ggtitle(paste("Fitted vs Actual: Full LM Lengths -", title)) +
    theme_minimal()
  print(p_full)
  ggsave(filename = paste0("Fitted_vs_Actual_Full_LM_Lengths_", title, ".png"), plot = p_full, path = getwd(), width = 6, height = 4)
  
  # Stepwise with dropouts
  lm_step_lengths <- step(lm_full_lengths, direction = "backward")
  print(paste("Stepwise LM Lengths -", title))
  print(summary(lm_step_lengths))
  
  # Diagnostic plots for stepwise model
  png(filename = paste0("Diagnostics_Stepwise_LM_Lengths_", title, ".png"), width = 800, height = 600)
  par(mfrow = c(2, 2))
  plot(lm_step_lengths, main = paste("Diagnostics: Stepwise LM Lengths -", title))
  dev.off()
  
  # Fitted vs Actual for stepwise
  wide_data$fitted_step <- predict(lm_step_lengths)
  p_step <- ggplot(wide_data, aes(x = fitted_step, y = MFT)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ggtitle(paste("Fitted vs Actual: Stepwise LM Lengths -", title)) +
    theme_minimal()
  print(p_step)
  ggsave(filename = paste0("Fitted_vs_Actual_Stepwise_LM_Lengths_", title, ".png"), plot = p_step, path = getwd(), width = 6, height = 4)
  
  # Ratios
  wide_data <- wide_data %>%
    mutate(EH_HH = EH / HH,
           EF1_HH = EF1 / HH,
           EF2_HH = EF2 / HH,
           EBF_HH = EBF / HH,
           EF1_EH = EF1 / EH,
           EF2_EH = EF2 / EH,
           EBF_EH = EBF / EH)
  
  # Univariable LMs for ratios vs MFT with plots
  ratios <- c("EH_HH", "EF1_HH", "EF2_HH", "EBF_HH", "EF1_EH", "EF2_EH", "EBF_EH")
  for (ratio in ratios) {
    lm_model <- lm(as.formula(paste("MFT ~", ratio)), data = wide_data)
    print(paste("LM MFT ~", ratio, "-", title))
    print(summary(lm_model))
    
    p <- ggplot(wide_data, aes(x = !!sym(ratio), y = MFT)) +
      geom_point() +
      geom_smooth(method = "lm", se = TRUE) +
      ggtitle(paste("MFT vs", ratio, "-", title)) +
      theme_minimal()
    print(p)
    ggsave(filename = paste0("MFT_vs_", ratio, "_", title, ".png"), plot = p, path = getwd(), width = 6, height = 4)
  }
  
  # Pairs plot for ratios
  predictors_ratios <- wide_data %>% select(EH_HH, EF1_HH, EF2_HH, EBF_HH, EF1_EH, EF2_EH, EBF_EH)
  p_pairs_ratios <- ggpairs(predictors_ratios) + ggtitle(paste("Pairs Plot: Ratios -", title))
  print(p_pairs_ratios)
  ggsave(filename = paste0("Pairs_Plot_Ratios_", title, ".png"), plot = p_pairs_ratios, path = getwd(), width = 8, height = 8)
  
  # Multivariable LM for ratios
  lm_full_ratios <- lm(MFT ~ EH_HH + EF1_HH + EF2_HH + EBF_HH + EF1_EH + EF2_EH + EBF_EH, data = wide_data)
  print(paste("Full LM Ratios -", title))
  print(summary(lm_full_ratios))
  
  # Diagnostic plots for full ratios
  png(filename = paste0("Diagnostics_Full_LM_Ratios_", title, ".png"), width = 800, height = 600)
  par(mfrow = c(2, 2))
  plot(lm_full_ratios, main = paste("Diagnostics: Full LM Ratios -", title))
  dev.off()
  
  # Fitted vs Actual for full ratios
  wide_data$fitted_full_ratios <- predict(lm_full_ratios)
  p_full_ratios <- ggplot(wide_data, aes(x = fitted_full_ratios, y = MFT)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ggtitle(paste("Fitted vs Actual: Full LM Ratios -", title)) +
    theme_minimal()
  print(p_full_ratios)
  ggsave(filename = paste0("Fitted_vs_Actual_Full_LM_Ratios_", title, ".png"), plot = p_full_ratios, path = getwd(), width = 6, height = 4)
  
  # Stepwise with dropouts for ratios
  lm_step_ratios <- step(lm_full_ratios, direction = "backward")
  print(paste("Stepwise LM Ratios -", title))
  print(summary(lm_step_ratios))
  
  # Diagnostic plots for stepwise ratios
  png(filename = paste0("Diagnostics_Stepwise_LM_Ratios_", title, ".png"), width = 800, height = 600)
  par(mfrow = c(2, 2))
  plot(lm_step_ratios, main = paste("Diagnostics: Stepwise LM Ratios -", title))
  dev.off()
  
  # Fitted vs Actual for stepwise ratios
  wide_data$fitted_step_ratios <- predict(lm_step_ratios)
  p_step_ratios <- ggplot(wide_data, aes(x = fitted_step_ratios, y = MFT)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ggtitle(paste("Fitted vs Actual: Stepwise LM Ratios -", title)) +
    theme_minimal()
  print(p_step_ratios)
  ggsave(filename = paste0("Fitted_vs_Actual_Stepwise_LM_Ratios_", title, ".png"), plot = p_step_ratios, path = getwd(), width = 6, height = 4)
}

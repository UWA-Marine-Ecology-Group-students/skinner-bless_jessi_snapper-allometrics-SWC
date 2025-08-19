library(readxl)
library(tidyr)
library(dplyr)
library(corrplot)
library(GGally)
library(ggplot2)

sheets <- c(
  "pink_snapper_measurements_abrol",
  "pink_snapper_measurements_swc",
  "pink_snapper_measurements_geogr"
)

sheet_titles <- c(
  "pink_snapper_measurements_abrol" = "Abrolhos Islands",
  "pink_snapper_measurements_swc" = "Southwest Corner",
  "pink_snapper_measurements_geogr" = "Geographe Bay"
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
           EBF = Eye_to_Back_Fin)
  
  predictors_lengths <- wide_data %>% select(HH, EH, EF1, EF2, EBF)
  
  cor_lengths <- cor(predictors_lengths, use = "pairwise.complete.obs")
  print(cor_lengths)
  par(mar = c(5.1, 4.1, 6.1, 2.1))
  corrplot(cor_lengths, method = "number")
  title(main = paste("Correlation Matrix: Lengths -", title), line = 3, cex.main = 1.2)
  print(ggpairs(predictors_lengths) + ggtitle(paste("Pairs Plot: Lengths -", title)) + theme(plot.title = element_text(hjust = 0.5)))
  
  wide_data <- wide_data %>%
    mutate(EH_HH = EH / HH,
           EF1_HH = EF1 / HH,
           EF2_HH = EF2 / HH,
           EBF_HH = EBF / HH,
           EF1_EH = EF1 / EH,
           EF2_EH = EF2 / EH,
           EBF_EH = EBF / EH)
  
  predictors_ratios <- wide_data %>% select(EH_HH, EF1_HH, EF2_HH, EBF_HH, EF1_EH, EF2_EH, EBF_EH)
  
  cor_ratios <- cor(predictors_ratios, use = "pairwise.complete.obs", method = "pearson")
  par(oma = c(4, 4, 8, 2))
  print(cor_ratios)
  par(mar = c(5.1, 4.1, 0, 2.1))
  corrplot(cor_ratios, method = "number")
  title(main = paste("Correlation Matrix: Ratios -", title), line = 3, cex.main = 1.2)
  print(ggpairs(predictors_ratios) + ggtitle(paste("Pairs Plot: Ratios -", title)) + theme(plot.title = element_text(hjust = 0.5)))
}


# Install packages if needed: install.packages(c("readxl", "tidyr", "dplyr", "corrplot", "GGally"))
library(readxl)
library(tidyr)
library(dplyr)
library(corrplot)
library(GGally)

# Load data
data <- read_excel("pink_snapper_measurements_abrolhos.xlsx", sheet = "abrolhos")

# Clean MeasurementKey to short names
data <- data %>%
  mutate(MeasurementKey = gsub("^[0-9]+\\. ", "", MeasurementKey),
         MeasurementKey = gsub("\\(.*\\)", "", MeasurementKey) %>%
           trimws() %>%
           gsub(" ", "_", .))

# Pivot wider
wide_data <- data %>%
  pivot_wider(id_cols = UniqueID, names_from = MeasurementKey, values_from = Length) %>%
  select(UniqueID, MFT = Mouth_to_Fork_of_the_Tail, HH = Head_Height, EH = Eye_Height,
         EF1 = Fin_attachement_Eye_to_side_Fin, EF2 = Fin_attachement_Eye_to_bottom_Fin,
         EBF = Eye_to_Back_Fin)

# Predictors for part 1: lengths
predictors_lengths <- wide_data %>% select(HH, EH, EF1, EF2, EBF)

# Correlation among lengths
cor_lengths <- cor(predictors_lengths, use = "pairwise.complete.obs")
print(cor_lengths)
corrplot(cor_lengths, method = "number")
ggpairs(predictors_lengths)

# Ratios for part 2 (e.g., relative to HH or EH)
wide_data <- wide_data %>%
  mutate(EH_HH = EH / HH,
         EF1_HH = EF1 / HH,
         EF2_HH = EF2 / HH,
         EBF_HH = EBF / HH,
         EF1_EH = EF1 / EH,
         EF2_EH = EF2 / EH,
         EBF_EH = EBF / EH)

# Predictors for part 2: ratios
predictors_ratios <- wide_data %>% select(EH_HH, EF1_HH, EF2_HH, EBF_HH, EF1_EH, EF2_EH, EBF_EH)

# Correlation among ratios
cor_ratios <- cor(predictors_ratios, use = "pairwise.complete.obs", method = "pearson")
print(cor_ratios)
corrplot(cor_ratios, method = "number")
ggpairs(predictors_ratios)


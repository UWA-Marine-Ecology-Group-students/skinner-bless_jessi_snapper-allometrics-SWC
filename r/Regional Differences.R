library(dplyr)
library(tidyr)
library(ggplot2)

# Add regions
pink_snapper_measurements_abrol$region <- "Abrolhos"
pink_snapper_measurements_swc$region <- "SWC"
pink_snapper_measurements_geogr$region <- "Geographe"

# Combine
all_data <- bind_rows(pink_snapper_measurements_abrol, pink_snapper_measurements_swc, pink_snapper_measurements_geogr)

# Assign Meas
all_data$Meas <- NA_character_
all_data$Meas[grepl("Mouth to Fork", all_data$MeasurementKey)] <- "MFT"
all_data$Meas[grepl("Head Height", all_data$MeasurementKey)] <- "HH"
all_data$Meas[grepl("Eye Height", all_data$MeasurementKey)] <- "EH"
all_data$Meas[grepl("Eye to side Fin", all_data$MeasurementKey)] <- "EF1"
all_data$Meas[grepl("Eye to bottom Fin", all_data$MeasurementKey)] <- "EF2"
all_data$Meas[grepl("Eye to Back Fin", all_data$MeasurementKey)] <- "EBF"

# Wide format
wide_data <- all_data %>%
  select(UniqueID, region, Meas, Length) %>%
  pivot_wider(names_from = Meas, values_from = Length) %>%
  filter(!is.na(MFT))

# ANOVA MFT
anova_mft <- aov(MFT ~ region, data = wide_data)
print(summary(anova_mft))

# Boxplot MFT
box_plot <- ggplot(wide_data, aes(x = region, y = MFT)) +
  geom_boxplot() +
  labs(title = "MFT by Region", x = "Region", y = "MFT") +
  theme_minimal()
print(box_plot)

# Deps
deps <- c("HH", "EH", "EF1", "EF2", "EBF")

# Loop
for (dep in deps) {
  model_base <- lm(as.formula(paste(dep, "~ MFT")), data = wide_data, na.action = na.omit)
  model_region <- lm(as.formula(paste(dep, "~ MFT + region")), data = wide_data, na.action = na.omit)
  model_interact <- lm(as.formula(paste(dep, "~ MFT * region")), data = wide_data, na.action = na.omit)
  omitted_ids <- wide_data[is.na(wide_data[[dep]]), "UniqueID"]
  cat("Omitted UniqueIDs for", dep, ":", paste(omitted_ids$UniqueID, collapse = ", "), "\n")
  
  cat("\nFor", dep, ":\n")
  print(anova(model_base, model_region))
  print(anova(model_region, model_interact))
  print(summary(model_interact))
  
  scatter_plot <- ggplot(wide_data, aes(x = MFT, y = .data[[dep]], color = region)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(title = paste(dep, "vs MFT by Region"), x = "MFT", y = dep) +
    theme_minimal()
  print(scatter_plot)
}
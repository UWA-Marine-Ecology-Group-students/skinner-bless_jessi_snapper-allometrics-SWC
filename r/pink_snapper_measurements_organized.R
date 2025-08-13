# Filter for pink snapper and non-empty/non-NA FishID
lengths_snapper <- `swc_lengths` %>% 
  filter(Species == "auratus" & FishID != "" & !is.na(FishID))

points_snapper <- `swc_points` %>% 
  filter(Species == "auratus" & FishID != "" & !is.na(FishID))

# Merge datasets using OpCode with many-to-many relationship
data <- lengths_snapper %>%
  left_join(points_snapper, by = "OpCode", relationship = "many-to-many") %>%  # Allow many-to-many
  select(OpCode, FishID = FishID.x, MeasurementKey, Length, ImageCol, ImageRow, MidX, MidY, MidZ)

# Create UniqueID by concatenating OpCode and FishID with underscore
data$UniqueID <- paste(data$OpCode, data$FishID, sep = "_")

# Remove duplicates by UniqueID and MeasurementKey (keep first row)
data <- data %>% 
  group_by(UniqueID, MeasurementKey) %>% 
  slice(1) %>% 
  ungroup()

# Select final columns
data <- data %>% select(UniqueID, MeasurementKey, Length, ImageCol, ImageRow, MidX, MidY, MidZ)

# Save organized dataset
write_csv(data, "pink_snapper_measurements_swc.csv")

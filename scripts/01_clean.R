# =============================================================================
# 01_clean.R  –  DeliveryPulse: Data Cleaning
# =============================================================================

# Set user library path so installed packages are found
userlib <- file.path(Sys.getenv("USERPROFILE"), "R", "win-library", "4.6")
.libPaths(c(userlib, .libPaths()))

# Load geosphere for geographic distance calculation
library(geosphere)

# -----------------------------------------------------------------------------
# 1. Load raw data
# -----------------------------------------------------------------------------
raw_data <- read.csv("data/raw/food_delivery.csv", stringsAsFactors = FALSE)

# -----------------------------------------------------------------------------
# 2. Print dim(), str() and missing-value counts BEFORE cleaning
# -----------------------------------------------------------------------------
cat("============================================================\n")
cat("RAW DATA – BEFORE CLEANING\n")
cat("============================================================\n")
cat("\ndim():\n")
print(dim(raw_data))

cat("\nstr():\n")
str(raw_data)

cat("\nMissing values per column (colSums of is.na):\n")
print(colSums(is.na(raw_data)))

# -----------------------------------------------------------------------------
# 3. Trim whitespace and handle "NaN"/empty strings across character columns
# -----------------------------------------------------------------------------
char_cols <- sapply(raw_data, is.character)
raw_data[char_cols] <- lapply(raw_data[char_cols], trimws)

# Clean "conditions " prefix if present in Weatherconditions
raw_data$Weatherconditions <- gsub("^conditions\\s+", "", raw_data$Weatherconditions)

# Convert literal "NaN", "NaNs", or "" in character columns to NA
for (col in names(raw_data)) {
  if (is.character(raw_data[[col]])) {
    raw_data[[col]][raw_data[[col]] %in% c("NaN", "NaNs", "")] <- NA
  }
}

# Ensure multiple_deliveries handles NA/NaN properly
raw_data$multiple_deliveries[raw_data$multiple_deliveries %in% c("NaN", "NaNs")] <- NA
raw_data$multiple_deliveries <- as.character(raw_data$multiple_deliveries)
raw_data$multiple_deliveries[is.na(raw_data$multiple_deliveries) | raw_data$multiple_deliveries == "NaN"] <- NA

# Convert Delivery_person_Age and Delivery_person_Ratings to numeric
raw_data$Delivery_person_Age     <- as.numeric(raw_data$Delivery_person_Age)
raw_data$Delivery_person_Ratings <- as.numeric(raw_data$Delivery_person_Ratings)

# -----------------------------------------------------------------------------
# 4. Convert categorical columns to factors
# -----------------------------------------------------------------------------
factor_cols <- c("Weatherconditions", "Road_traffic_density", "Type_of_order",
                 "Type_of_vehicle", "Festival", "City", "multiple_deliveries",
                 "Vehicle_condition")

raw_data[factor_cols] <- lapply(raw_data[factor_cols], as.factor)

# -----------------------------------------------------------------------------
# 5. Compute distance_km via Haversine formula; remove implausible rows
#    (0 km = same point / GPS error;  >50 km = outlier ping)
# -----------------------------------------------------------------------------
# geosphere::distHaversine calculates great-circle distance in metres
raw_data$distance_km <- distHaversine(
  cbind(raw_data$Restaurant_longitude,  raw_data$Restaurant_latitude),
  cbind(raw_data$Delivery_location_longitude, raw_data$Delivery_location_latitude)
) / 1000

n_before_dist <- nrow(raw_data)
raw_data <- raw_data[!is.na(raw_data$distance_km) &
                       raw_data$distance_km > 0 &
                       raw_data$distance_km <= 50, ]
n_after_dist <- nrow(raw_data)
cat("\n------------------------------------------------------------\n")
cat(sprintf("Rows removed by distance filter (0 km or >50 km): %d\n",
            n_before_dist - n_after_dist))
cat(sprintf("Rows remaining after distance filter: %d\n", n_after_dist))

# -----------------------------------------------------------------------------
# 6. Parse Time_taken(min) – strip the leading "(min) " text, make numeric
# -----------------------------------------------------------------------------
raw_data$Time_taken_min <- as.numeric(
  gsub("\\(min\\)\\s*", "", raw_data$Time_taken.min.)
)

# -----------------------------------------------------------------------------
# 7. Create binary target Late (1/0) and factor LateF
#    Bernoulli trial: 1 if delivery took > 30 minutes, 0 otherwise
# -----------------------------------------------------------------------------
raw_data$Late  <- as.integer(raw_data$Time_taken_min > 30)
raw_data$LateF <- factor(
  ifelse(raw_data$Late == 1, "Late", "OnTime"),
  levels = c("OnTime", "Late")
)

# -----------------------------------------------------------------------------
# 8. Drop rows with NA in any modelling column; report final row count
# -----------------------------------------------------------------------------
modelling_cols <- c("distance_km", "Delivery_person_Age", "Delivery_person_Ratings",
                    "Road_traffic_density", "Weatherconditions", "Type_of_vehicle",
                    "Festival", "multiple_deliveries", "City", "Time_taken_min",
                    "Late", "LateF")

n_before_na <- nrow(raw_data)
clean_data <- raw_data[complete.cases(raw_data[, modelling_cols]), ]

# Drop unused levels created by filtering NAs
clean_data[factor_cols] <- lapply(clean_data[factor_cols], droplevels)
n_after_na <- nrow(clean_data)

cat(sprintf("\nRows removed by NA drop (modelling columns): %d\n",
            n_before_na - n_after_na))
cat(sprintf("Final row count after all cleaning: %d\n\n", n_after_na))

# -----------------------------------------------------------------------------
# 9. Save cleaned data and print summary
# -----------------------------------------------------------------------------
write.csv(clean_data, "data/cleaned/delivery_clean.csv", row.names = FALSE)
# Also save directly in data/ for easy reference if needed
write.csv(clean_data, "data/delivery_clean.csv", row.names = FALSE)
cat("Saved: data/cleaned/delivery_clean.csv and data/delivery_clean.csv\n\n")

cat("============================================================\n")
cat("CLEAN DATA – summary(clean_data)\n")
cat("============================================================\n")
print(summary(clean_data[, c(modelling_cols, "Type_of_order", "Vehicle_condition")]))

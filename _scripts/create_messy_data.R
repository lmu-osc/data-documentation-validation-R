# Create messy Palmer Penguins dataset for teaching data validation
# This script creates a realistic "messy" version of the Palmer Penguins data
# to help learners practice data validation and quality checking.

library(palmerpenguins)
library(dplyr)
library(readr)

# Load clean data
data("penguins")

# Save clean version first
write_csv(penguins, "data/penguins_clean.csv")

# Create messy version with realistic data quality issues
set.seed(2024)  # For reproducibility

penguins_messy <- penguins %>%
  mutate(
    # 1. TYPOS IN SPECIES NAMES (common data entry error)
    species = case_when(
      row_number() %in% c(5, 145, 234) ~ paste0(species, " "),  # trailing spaces
      row_number() %in% c(23, 167) ~ tolower(as.character(species)),  # wrong case
      row_number() == 67 ~ "Gentoo penguin",  # extra text
      row_number() == 189 ~ "Adelei",  # typo
      TRUE ~ as.character(species)
    ),

    # 2. INCONSISTENT ISLAND NAMES (common in manual transcription)
    island = case_when(
      row_number() %in% c(45, 112) ~ paste0(" ", island),  # leading space
      row_number() %in% c(87, 201) ~ paste0(island, " "),  # trailing space
      row_number() == 156 ~ "biscoe",  # wrong case
      row_number() == 278 ~ "Torgerson",  # typo (should be Torgersen)
      TRUE ~ as.character(island)
    ),

    # 3. IMPOSSIBLE MEASUREMENTS (data entry or sensor errors)
    bill_length_mm = case_when(
      row_number() == 12 ~ -5.2,      # impossible negative
      row_number() == 89 ~ 250.5,     # impossibly large (should be ~30-60mm)
      row_number() == 156 ~ 0,        # impossible zero
      row_number() == 223 ~ 999,      # placeholder value not removed
      TRUE ~ bill_length_mm
    ),

    bill_depth_mm = case_when(
      row_number() == 45 ~ -2.1,      # impossible negative
      row_number() == 178 ~ 99.9,     # placeholder not removed
      TRUE ~ bill_depth_mm
    ),

    flipper_length_mm = case_when(
      row_number() == 67 ~ 0,         # impossible zero
      row_number() == 289 ~ 999,      # placeholder
      TRUE ~ flipper_length_mm
    ),

    # 4. OUT-OF-RANGE BUT POSSIBLE VALUES (outliers vs errors)
    body_mass_g = case_when(
      row_number() == 34 ~ 15000,     # way too heavy (real range: 2700-6300g)
      row_number() == 201 ~ 500,      # way too light
      row_number() == 145 ~ 10000,    # suspicious outlier
      TRUE ~ body_mass_g
    ),

    # 5. INCONSISTENT SEX CODING (multiple formats)
    sex = case_when(
      row_number() %in% c(18, 92, 187) ~ "M",         # should be "male"
      row_number() %in% c(76, 143, 256) ~ "F",        # should be "female"
      row_number() %in% c(129, 234) ~ "Male",         # wrong capitalization
      row_number() == 198 ~ "Female",                 # wrong capitalization
      row_number() == 287 ~ "m",                      # lowercase
      row_number() == 312 ~ "MALE",                   # uppercase
      TRUE ~ as.character(sex)
    ),

    # 6. INVALID YEARS (outside study period or typos)
    year = case_when(
      row_number() == 99 ~ 2020,      # after study period (2007-2009)
      row_number() == 234 ~ 2006,     # before study period
      row_number() == 178 ~ 207,      # typo (missing 0)
      row_number() == 301 ~ 20009,    # extra digit
      TRUE ~ as.numeric(year)
    ),

    # 7. ADDITIONAL MISSING VALUES (common in field data collection)
    bill_length_mm = if_else(row_number() %in% c(78, 165, 299, 320), NA_real_, bill_length_mm),
    bill_depth_mm = if_else(row_number() %in% c(123, 245), NA_real_, bill_depth_mm),
    flipper_length_mm = if_else(row_number() %in% c(45, 234, 289), NA_real_, flipper_length_mm),
    body_mass_g = if_else(row_number() %in% c(56, 178, 267), NA_real_, body_mass_g)
  )

# Save messy version
write_csv(penguins_messy, "data/penguins_messy.csv")

# Print summary
cat("\n=== MESSY DATA CREATION SUMMARY ===\n\n")

cat("Dataset dimensions:\n")
cat("  Clean dataset: ", nrow(penguins), "rows ×", ncol(penguins), "columns\n")
cat("  Messy dataset: ", nrow(penguins_messy), "rows ×", ncol(penguins_messy), "columns\n\n")

cat("Data quality issues introduced:\n\n")

cat("SPECIES (categorical):\n")
cat("  • Trailing spaces\n")
cat("  • Lowercase entries (should be capitalized)\n")
cat("  • Extra text ('Gentoo penguin')\n")
cat("  • Typos ('Adelei' instead of 'Adelie')\n\n")

cat("ISLAND (categorical):\n")
cat("  • Leading/trailing spaces\n")
cat("  • Lowercase entries\n")
cat("  • Typos ('Torgerson' instead of 'Torgersen')\n\n")

cat("BILL LENGTH (numeric):\n")
cat("  • Impossible negative (-5.2 mm)\n")
cat("  • Impossibly large (250.5 mm, real max ~60 mm)\n")
cat("  • Impossible zero\n")
cat("  • Placeholder value (999)\n")
cat("  • Additional missing values\n\n")

cat("BILL DEPTH (numeric):\n")
cat("  • Impossible negative (-2.1 mm)\n")
cat("  • Placeholder value (99.9)\n")
cat("  • Additional missing values\n\n")

cat("FLIPPER LENGTH (numeric):\n")
cat("  • Impossible zero\n")
cat("  • Placeholder value (999)\n")
cat("  • Additional missing values\n\n")

cat("BODY MASS (numeric):\n")
cat("  • Extreme outliers (15000g, 10000g; real max ~6300g)\n")
cat("  • Impossibly low (500g, real min ~2700g)\n")
cat("  • Additional missing values\n\n")

cat("SEX (categorical):\n")
cat("  • Using 'M'/'F' instead of 'male'/'female'\n")
cat("  • Wrong capitalization (Male/Female)\n")
cat("  • All lowercase (m) or uppercase (MALE)\n\n")

cat("YEAR (numeric):\n")
cat("  • After study period (2020, should be 2007-2009)\n")
cat("  • Before study period (2006)\n")
cat("  • Missing digit (207 instead of 2007)\n")
cat("  • Extra digit (20009 instead of 2009)\n\n")

cat("========================================\n")
cat("Files created:\n")
cat("  ✓ data/penguins_clean.csv\n")
cat("  ✓ data/penguins_messy.csv\n\n")

cat("These errors represent common real-world data quality issues:\n")
cat("  • Data entry mistakes (typos, case inconsistencies)\n")
cat("  • Whitespace problems (leading/trailing spaces)\n")
cat("  • Placeholder values not removed (999, 99.9)\n")
cat("  • Sensor/measurement errors (impossible values)\n")
cat("  • Field data collection gaps (missing values)\n")
cat("  • Transcription errors (digit mistakes)\n\n")
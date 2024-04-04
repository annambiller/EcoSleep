#load necessary libraries
# Load necessary libraries
library(readr)
library(dplyr)
library(tidyr)
library(broom)

#load file from folder
setwd("/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/eeg/derived/derived-all/stats")

# read EEG data from folder
eeg_data <- read_csv("2024.04.02.eeg_data.csv")

# first dsiregard that some people have >2 nights of EEG
# Filter out night 3 data
eeg_data_filtered = eeg_data %>% filter(night_number!=3)

# Prepare data frame for results
results <- data.frame(variable = character(), normality_p = numeric(), comparison_p = numeric(), test_used = character())

# List of variables to test, excluding 'ID', 'date', and 'night_number'
variables <- setdiff(names(eeg_data_filtered), c("ID", "date", "night_number"))

###### analysis without a small constant and test according to normality #####
for (var in variables) {
  # Prepare data for this variable
  data_night1 <- eeg_data_filtered %>% filter(night_number == 1) %>% select(ID, !!sym(var))
  data_night2 <- eeg_data_filtered %>% filter(night_number == 2) %>% select(ID, !!sym(var))
  
  # Merge by ID to ensure we're comparing the same participants
  data_merged <- merge(data_night1, data_night2, by = "ID", suffixes = c("_night1", "_night2"))
  
  # Perform Shapiro-Wilk normality test on combined nights data
  normality_test <- shapiro.test(c(data_merged[[paste0(var, "_night1")]], data_merged[[paste0(var, "_night2")]]))
  
  # Determine which statistical test to use based on normality test outcome
  if (normality_test$p.value > 0.05) {
    # Data are normally distributed, use paired t-test
    comparison_test <- t.test(data_merged[[paste0(var, "_night1")]], data_merged[[paste0(var, "_night2")]], paired = TRUE)
    test_used <- "Paired t-test"
  } else {
    # Non-normal distribution, use Wilcoxon signed-rank test
    comparison_test <- wilcox.test(data_merged[[paste0(var, "_night1")]], data_merged[[paste0(var, "_night2")]], paired = TRUE)
    test_used <- "Wilcoxon signed-rank test"
  }
  
  # Check if the test is significant
  significant <- ifelse(comparison_test$p.value < 0.05, "Yes", "No")
  
  # Append results
  results <- rbind(results, data.frame(variable = var, normality_p = normality_test$p.value, comparison_p = comparison_test$p.value, test_used = test_used, significant = significant))
}

# Print the results
print(results)

###### analysis with adding a small constant #####
for (var in variables) {
  # Prepare data for this variable
  data_night1 <- eeg_data_filtered %>% filter(night_number == 1) %>% select(ID, !!sym(var))
  data_night2 <- eeg_data_filtered %>% filter(night_number == 2) %>% select(ID, !!sym(var))
  
  # Merge by ID to ensure we're comparing the same participants
  data_merged <- merge(data_night1, data_night2, by = "ID", suffixes = c("_night1", "_night2"))
  
  # Perform Shapiro-Wilk normality test on combined nights data
  normality_test <- shapiro.test(c(data_merged[[paste0(var, "_night1")]], data_merged[[paste0(var, "_night2")]]))
  
  # Determine which statistical test to use based on normality test outcome
  if (normality_test$p.value > 0.05) {
    # Data are normally distributed, use paired t-test
    comparison_test <- t.test(data_merged[[paste0(var, "_night1")]], data_merged[[paste0(var, "_night2")]], paired = TRUE)
    test_used <- "Paired t-test"
  } else {
    # Non-normal distribution, use Wilcoxon signed-rank test
    
    # Add a small constant to address zeros
    small_constant <- 0.001
    data_merged[[paste0(var, "_night1")]] <- data_merged[[paste0(var, "_night1")]] + small_constant
    data_merged[[paste0(var, "_night2")]] <- data_merged[[paste0(var, "_night2")]] + small_constant
    
    comparison_test <- wilcox.test(data_merged[[paste0(var, "_night1")]], data_merged[[paste0(var, "_night2")]], paired = TRUE)
    test_used <- "Wilcoxon signed-rank test"
  }
  
  # Check if the test is significant
  significant <- ifelse(comparison_test$p.value < 0.05, "Yes", "No")
  
  # Append results
  results <- rbind(results, data.frame(variable = var, normality_p = normality_test$p.value, comparison_p = comparison_test$p.value, test_used = test_used, significant = significant))
}

# Print the results
print(results)

###### analysis only with paired t-test and one-sided #####

###### simple differences between night 1 and night 2 per individual #####
# Spread the data to wide format to easily calculate differences
eeg_data_filtered <- eeg_data %>%
  filter(night_number %in% c(1, 2)) %>%
  select(-date)

eeg_data_wide <- eeg_data_filtered %>%
  pivot_wider(names_from = night_number, values_from = c(TIB:Lat_REM, `%N1`:`%NREM`, SE:SME), 
              values_fill = list(TIB = NA, SPT = NA, WASO = NA, TST = NA, N1 = NA, N2 = NA, N3 = NA, REM = NA, NREM = NA, SOL = NA, `%N1` = NA, `%N2` = NA, `%N3` = NA, `%REM` = NA, `%NREM` = NA, SE = NA, SME = NA)) 

# Calculate differences
eeg_diffs <- eeg_data_wide %>%
  mutate(across(ends_with("_2"), ~ . - get(sub("2$", "1", cur_column())), .names = "diff_{.col}"))

# Keep only difference columns and ID
eeg_diffs_filtered <- eeg_diffs %>%
  select(ID, starts_with("diff"))

# Rename columns to remove 'diff_' prefix for clarity
colnames(eeg_diffs_filtered) <- sub("diff_", "", colnames(eeg_diffs_filtered))

# Rename ID to study_ID
eeg_diffs_filtered <- eeg_diffs_filtered %>%
  rename(study_ID = ID)

  # Save the file with the dynamically generated filename
  write_csv(eeg_diffs_filtered, "/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/eeg/derived/derived-all/stats/eeg_diffs_filtered.csv")

# Rename ID to study_ID in eeg_data_wide
  eeg_data_wide <- eeg_data_wide %>%
    rename(study_ID = ID)

  # Save the file with the dynamically generated filename
  write_csv(eeg_data_wide, "/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/eeg/derived/derived-all/stats/eeg_data_wide.csv")
  
##### add subjective parameters from sleep diary to this #####
sleep_diary <- read_csv("/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all/sleep_diary_df.csv")
sleep_diary_differences <- read_csv("/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all/sleep_diary_differences.csv")
sleep_diary_differences_wide <- read_csv("/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all/sleep_diary_differences_wide.csv")

#merge sleep data wide with a wide form of sleep_diary which currently doesn't exist yet

#merge eeg_diffs_filtered and sleep_diary_differences_wide by study_ID
all_sleep_diffs <- merge(eeg_diffs_filtered, sleep_diary_differences_wide, by="study_ID")

  

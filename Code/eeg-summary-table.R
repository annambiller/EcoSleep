#load libraries
library(purrr) # For functional programming tools
library(readr) #For reading data
library(tidyr) #For pivoting data
library(dplyr) #For data manipluation
library(stringr) # For string manipulation
library(gt) #For nice tables

#load file from folder
setwd("/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/eeg/derived/derived-all/stats")

# read EEG data from folder
eeg_data <- read_csv("2024.04.02.eeg_data.csv")


####### DESCRIPTIVE EEG STATS grouped by ID #######
# Function to calculate descriptive statistics for each column in a data frame
calculate_stats <- function(df) {
  df %>%
    summarise(across(
      .cols = where(is.numeric),
      .fns = list(
        Mean = ~mean(., na.rm = TRUE),
        SD = ~sd(., na.rm = TRUE),
        Min = ~min(., na.rm = TRUE),
        Median = ~median(., na.rm = TRUE),
        Max = ~max(., na.rm = TRUE)
      ),
      .names = "{.col}_{.fn}"
    ))
}

# Group by ID and then calculate descriptive statistics for each group
descriptive_stats_by_id <- eeg_data %>% # use 'eeg_data' from the previous steps
  group_by(ID) %>%
  calculate_stats() %>%
  ungroup() # Ensure the result is no longer grouped

# View the first few rows of the descriptive statistics
print(descriptive_stats_by_id)

####### DESCRIPTIVE EEG STATS not grouped by ID #######
# Group by night_number and then calculate descriptive statistics for each group
descriptive_stats <- eeg_data %>% 
  calculate_stats() 

# Prepare the data frame for reshaping (pivoting)
# Select the mean and standard deviation columns for specific variables
specific_stats <- descriptive_stats %>%
  select(N1_Mean, '%N1_Mean', N2_Mean,'%N2_Mean', N3_Mean, '%N3_Mean', 
         REM_Mean,'%REM_Mean', '%NREM_Mean', NREM_Mean, SOL_Mean, TST_Mean, WASO_Mean,
         N1_SD, '%N1_SD', N2_SD, '%N2_SD', N3_SD,'%N3_SD', REM_SD,'%REM_SD', 
         NREM_SD,'%NREM_SD', SOL_SD, TST_SD, WASO_SD)


# Reshape the selected statistics into a long format, maintaining the 'ID' for reference
long_format_stats <- specific_stats %>%
  pivot_longer(cols = everything(),
               names_to = "Statistic",
               values_to = "Value")

# Separate the 'Statistic' column into 'Variable' and 'Stat_Type' (Mean or SD)
long_format_stats <- long_format_stats %>%
  separate(Statistic, into = c("Variable", "Stat_Type"), sep = "_")

# Arrange and display the table for review
long_format_stats <- long_format_stats %>%
  arrange(Variable, Stat_Type)

# Pivot the data back to wide format with columns for "Mean" and "SD"
wide_format_stats <- long_format_stats %>%
  pivot_wider(names_from = Stat_Type, values_from = Value)

#Change the mean to hours to facilitate understanding the data
wide_format_stats$Mean_h <- wide_format_stats$Mean/60


# Making a nice table out of this #
gt_table <- wide_format_stats %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics",
    #subtitle = "Mean and Standard Deviation for Sleep Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    Mean = "Mean",
    SD = "Standard Deviation"
  )


# Print the gt table
print(gt_table)



########## SUMMARY STATISTICS grouped by night number ##########
# Group by night_number and then calculate descriptive statistics for specific variables
descriptive_stats_by_night <- eeg_data %>%
  group_by(night_number) %>%
  summarise(across(c(N1,'%N1',N2,'%N2',N3,'%N3',REM,'%REM',NREM,'%NREM',SOL,TST,WASO), 
                   list(Mean = ~mean(as.numeric(.), na.rm = TRUE), 
                        SD = ~sd(as.numeric(.), na.rm = TRUE)),
                   .names = "{.col}_{.fn}"))

# Pivot to long format
long_format_stats_by_night <- descriptive_stats_by_night %>%
  pivot_longer(cols = -night_number, names_to = "Statistic", values_to = "Value") %>%
  separate(Statistic, into = c("Variable", "Stat_Type"), sep = "_") %>%
  arrange(Variable, Stat_Type)

# Pivot back to wide format
wide_format_stats_by_night <- long_format_stats_by_night %>%
  pivot_wider(names_from = Stat_Type, values_from = Value)

# Convert Mean to hours
wide_format_stats_by_night$Mean_h <- wide_format_stats_by_night$Mean/60

# Rearrange so that 'mean_h' comes immediately after 'mean' for better readibility
wide_format_stats_by_night <- wide_format_stats_by_night %>%
  select(night_number,Variable, Mean, Mean_h, everything())

# Making a nice table out of this #
#only select nights 1 and 2 since night 3 was only in one participant
wide_format_stats_filtered <- wide_format_stats_by_night %>%
  filter(night_number != 3)

gt_table_by_night <- wide_format_stats_filtered %>%
  gt() %>%
  tab_header(
    title = "Descriptive Statistics",
    #subtitle = "Mean and Standard Deviation for Sleep Variables"
  ) %>%
  cols_label(
    Variable = "Variable",
    Mean = "Mean",
    SD = "Standard Deviation",
    night_number = "Night #"
  )


# Print the gt table
print(gt_table_by_night)




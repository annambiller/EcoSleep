# EMA data reading and preprocessing

# Load necessary libraries
library(readr) #For reading data
library(tidyr) #For pivoting data
library(dplyr) #For data manipluation
library(stringr) # For string manipulation

setwd("/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all")

##### READING SINGLE FILES #####
# find columns with sleep diary
#import data
data=read.csv('data_group1-m1.csv')


#select columns with sleep variables
sleep_diary <- data[,5:30]

#sleep_diary <- sleep_diary %>% filter(field_fum_py3q_2d80e0_ != "NA")

# Formatting data #
#rename the column names
sleep_diary <- sleep_diary %>%
  rename_with(~ "response_time", 1) %>%
  rename_with(~ "ID", 2) %>%
  rename_with(~ "bedtime", 6) %>%
  rename_with(~ "SOL", 7) %>%
  rename_with(~ "WASO_number", 8) %>%
  rename_with(~ "WASO_min", 9) %>%
  rename_with(~ "sleep_off", 10) %>%
  rename_with(~ "bed_out", 11) %>%
  rename_with(~ "snoozing_number", 12) %>%
  rename_with(~ "snoozing_duration_each", 13) %>%
  rename_with(~ "sleep_quality", 14) %>%
  rename_with(~ "restful_feeling_wakeup", 15) %>%
  rename_with(~ "sleepiness_rating", 16) %>%
  rename_with(~ "sleep_influences", 18) %>%
  rename_with(~ "sleep_influences_specs", 19) %>%
  rename_with(~ "sleep_influences_specs_time", 20) %>%
  rename_with(~ "daytype", 21) %>%
  rename_with(~ "prebed_activity", 25) %>%
  rename_with(~ "prebed_activity_specs", 26)

# add the ID
sleep_diary$id <- data$field_record_id
sleep_diary$repeat_instance <- data$redcap_repeat_instance
sleep_diary$study_ID <- data$field_fum_py3q_2d80e0_

# matching id and study_ID
lookup.table <- sleep_diary%>%
  select(id, study_ID) %>%
  filter(!is.na(study_ID))

#filter out irrelevant rows


sleep_diary_updated <- sleep_diary 

# Iterate through each row in 'sleep_diary_updated' and replace 'study_ID' with the corresponding value from 'lookup_table'
for (i in 1:nrow(sleep_diary_updated)) {
  id_to_lookup <- sleep_diary_updated[i, "id"]
  lookup_row <- lookup.table[lookup.table$id == id_to_lookup, "study_ID"]
  if (length(lookup_row) > 0) {
    sleep_diary_updated[i, "study_ID"] <- lookup_row
  }
}

#select only real sleep diary questions
sleep <- sleep_diary_updated %>% filter(!is.na(bedtime) & bedtime != "")

#strip irrelevant columns
sleep <- sleep[, c(5:21, 27:29)]

#select only pre bed activities
pre_bed_activities <- sleep_diary_updated %>% filter(!is.na(prebed_activity) & prebed_activity != "")

#strip irrelevant columns
pre_bed_activities <- pre_bed_activities[,24:29]

# Merge the two by study_ID and repeat instances
merged_data <- merge(pre_bed_activities, sleep, by = c("study_ID", "id","repeat_instance"))

#checl which columns are different
#cols_to_check <- setdiff(names(merged_data), names(sleep))


##### READING MULTIPLE FILES ######
# Get all file names in the folder
files <- list.files(path = "/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all", full.names = TRUE, pattern = "\\.csv$")

# Initialize an empty list to store data frames
list_of_dfs <- list()

for (file in files) {
  data <- read.csv(file)
  
  # Select columns with sleep variables
  sleep_diary <- data[,5:30]
  
  # Rename column names
  sleep_diary <- sleep_diary %>%
    rename_with(~ "response_time", 1) %>%
    rename_with(~ "ID", 2) %>%
    rename_with(~ "bedtime", 6) %>%
    rename_with(~ "SOL", 7) %>%
    rename_with(~ "WASO_number", 8) %>%
    rename_with(~ "WASO_min", 9) %>%
    rename_with(~ "sleep_off", 10) %>%
    rename_with(~ "bed_out", 11) %>%
    rename_with(~ "snoozing_number", 12) %>%
    rename_with(~ "snoozing_duration_each", 13) %>%
    rename_with(~ "sleep_quality", 14) %>%
    rename_with(~ "restful_feeling_wakeup", 15) %>%
    rename_with(~ "sleepiness_rating", 16) %>%
    rename_with(~ "sleep_influences", 18) %>%
    rename_with(~ "sleep_influences_specs", 19) %>%
    rename_with(~ "sleep_influences_specs_time", 20) %>%
    rename_with(~ "daytype", 21) %>%
    rename_with(~ "prebed_activity", 25) %>%
    rename_with(~ "prebed_activity_specs", 26)
  
  # Add the ID, repeat_instance, and study_ID
  sleep_diary$id <- data$field_record_id
  sleep_diary$repeat_instance <- data$redcap_repeat_instance
  sleep_diary$study_ID <- data$field_fum_py3q_2d80e0_
  
  # Create lookup.table and perform the lookup
  lookup.table <- sleep_diary %>%
    select(id, study_ID) %>%
    filter(!is.na(study_ID))
  
  for (i in 1:nrow(sleep_diary)) {
    id_to_lookup <- sleep_diary[i, "id"]
    lookup_row <- lookup.table[lookup.table$id == id_to_lookup, "study_ID"]
    if (length(lookup_row) > 0) {
      sleep_diary[i, "study_ID"] <- lookup_row
    }
  }
  
  # Filter and strip irrelevant rows/columns
  sleep <- sleep_diary %>% 
    filter(!is.na(bedtime) & bedtime != "") %>%
    select(5:21, 27:29)
  
  pre_bed_activities <- sleep_diary %>%
    filter(!is.na(prebed_activity) & prebed_activity != "") %>%
    select(24:29)
  
  # Merge pre_bed_activities and sleep data frames by study_ID, id, repeat_instance
  merged_data <- merge(pre_bed_activities, sleep, by = c("study_ID", "id", "repeat_instance"))
  
  # Add the processed data frame to the list
  list_of_dfs[[file]] <- merged_data
}

# Combine all data frames into one
final_df <- bind_rows(list_of_dfs)

write.csv(final_df, "/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all/sleep_diary_df.csv", row.names = FALSE)


##### CALCULATIONS #####

numeric_df <- final_df %>% select(where(is.numeric))
  
numeric_df_long <- numeric_df %>%
  pivot_longer(-c(study_ID, repeat_instance), names_to = "variable", values_to = "value")

# Step 3: Calculate differences
numeric_differences <- numeric_df_long %>%
  group_by(study_ID, variable) %>%
  arrange(study_ID, variable, repeat_instance) %>%
  mutate(difference = value - lag(value)) %>%
  filter(!is.na(difference))

  #save this df
  write.csv(numeric_differences, "/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all/sleep_diary_differences.csv", row.names = FALSE)

numeric_differences_wide <- numeric_differences %>%
  ungroup() %>%
  select(-value) %>%
  pivot_wider(names_from = c(repeat_instance, variable), names_sep = "_", values_from = difference)

  #save this df
  write.csv(numeric_differences_wide, "/Users/amb/Documents/1_Projects/11_EcoSleepProject/Data/ema/all/sleep_diary_differences_wide.csv", row.names = FALSE)

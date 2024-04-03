# Load necessary libraries
library(readr) #For reading data
library(tidyr) #For pivoting data
library(dplyr) #For data manipluation
library(stringr) # For string manipulation


####### READING EEG DATA #######
# Set the working directory to where your files are located
setwd("your/filepath")

# List all files in the directory
files <- list.files(pattern = "\\.csv$") # Adjust pattern based on your file type

# Initialize an empty data frame to store combined data
combined_df <- data.frame()

# Loop through files, read, assign header, pivot, add ID, and combine
for (file in files) {
  # Read the file without assuming a header
  df <- read_csv(file, col_names = FALSE)
  
  # Manually assign column names
  colnames(df) <- c("name", "value")
  
  # Extract ID from file name
  id <- str_extract(file, "^[0-9]+") # Extracts the leading digits which represent the ID
  
  # Extract date from file name
  date_str <- str_extract(file, "[0-9]{8}") # Extracts the date part
  
  # Convert the date string to a date object
  date <- as.Date(date_str, format = "%Y%m%d")
  
  # Convert the data frame from long to wide format
  wide_df <- pivot_wider(df, names_from = name, values_from = value)
  
  # Add the ID and date as a new column to the wide_df
  wide_df$ID <- id
  wide_df$date <- date
  
  # Combine with the main data frame
  combined_df <- bind_rows(combined_df, wide_df)
}

#Add another column with the night number
combined_df <- combined_df %>%
  group_by(ID) %>%
  arrange(date) %>%
  mutate(night_number = row_number()) %>%
  ungroup() %>%
  mutate(night = case_when(
    night_number == 1 ~ "night 1",
    night_number == 2 ~ "night 2",
    night_number == 3 ~ "night 3",
    TRUE ~ as.character(night_number) # Handles cases of more than 3 nights, adjust as necessary
  ))%>%
  select(-night) # Optional: Remove the 'night_number' column if not needed

#Save this as the EEG output file
eeg_data <- combined_df
  
  # Get the current system's date
  current_date <- Sys.Date()
  
  # Format the date as "YYYY.MM.DD"
  formatted_date <- format(current_date, "%Y.%m.%d")
  
  # Create the filename by appending the current date to "eeg_data.csv"
  filename <- paste0(formatted_date, ".eeg_data.csv")
  
  # Save the file with the dynamically generated filename
  write_csv(eeg_data, filename)



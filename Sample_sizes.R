library(dplyr)

groupcomp_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/GroupComposition_complete.csv", header = TRUE, sep = ";")


#### AUDIO DATA ####
audio_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_complete.csv", header = TRUE)

# Ensure the date column is in Date format
groupcomp_df$Date <- as.Date(groupcomp_df$Date)
groupcomp_df$Date <- format(groupcomp_df$Date, "%Y%m%d")

# Perform the join using merge
audio_df <- merge(audio_df, groupcomp_df, by.x = c("id_code", "date"), by.y = c("ID", "Date"), all.x = TRUE)

# Filter for unique rows based on id_code and date and the variable of interest
unique_audio_df <- audio_df %>%
  distinct(id_code, date, .keep_all = TRUE)

# Counts of...
table(unique_audio_df$id_code, unique_audio_df$date)
table(unique_audio_df$date, unique_audio_df$Sex)
table(unique_audio_df$date, unique_audio_df$Position)

# Ensure the data is unique per individual (id_code) and date
unique_audio_df <- audio_df %>%
  distinct(id_code, date, .keep_all = TRUE)

# Total unique individuals per date
audio_total_individuals_per_date <- unique_audio_df %>%
  group_by(date) %>%
  summarize(total_individuals = n(), .groups = 'drop')

# Total counts by Position per date
audio_positions_per_date <- unique_audio_df %>%
  group_by(date, Position) %>%
  summarize(position_count = n(), .groups = 'drop')

# Total counts by Sex per date
audio_sex_per_date <- unique_audio_df %>%
  group_by(date, Sex) %>%
  summarize(sex_count = n(), .groups = 'drop')

# Print results
print(audio_total_individuals_per_date, n = nrow(audio_total_individuals_per_date))
print(audio_positions_per_date, n = nrow(audio_positions_per_date))
print(audio_sex_per_date, n = nrow(audio_sex_per_date))



#### GPS DATA ####
gps_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/gps_df_files.csv", header = TRUE, sep = ";")

# Perform the join using merge
gps_df <- merge(gps_df, groupcomp_df, by.x = c("id_code", "date"), by.y = c("ID", "Date"), all.x = TRUE)


# Filter for unique rows based on id_code and date and the variable of interest
unique_gps_df <- gps_df %>%
  distinct(id_code, date, .keep_all = TRUE)

# Counts of...
table(unique_gps_df$id_code, unique_gps_df$date)
table(unique_audio_df$date, unique_audio_df$Sex)
table(unique_audio_df$date, unique_audio_df$Position)

# Total unique individuals per date
gps_total_individuals_per_date <- unique_gps_df %>%
  group_by(date) %>%
  summarize(total_individuals = n(), .groups = 'drop')

# Total counts by Position per date
gps_positions_per_date <- unique_gps_df %>%
  group_by(date, Position) %>%
  summarize(position_count = n(), .groups = 'drop')

# Total counts by Sex per date
gps_sex_per_date <- unique_gps_df %>%
  group_by(date, Sex) %>%
  summarize(sex_count = n(), .groups = 'drop')

# Print results
print(gps_total_individuals_per_date, n = nrow(gps_total_individuals_per_date))
print(gps_positions_per_date, n = nrow(gps_positions_per_date))
print(gps_sex_per_date, n = nrow(gps_sex_per_date))

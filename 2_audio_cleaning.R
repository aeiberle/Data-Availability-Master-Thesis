# Load necessary library
library(dplyr)
library(lubridate)

# Set Up Directory Paths
base_dir <- "/mnt/EAS_ind/aeiberle/data/"
labels_synched <- read.csv(paste0(base_dir, "labels_synched_2023.csv"))

# print all label_name's from the df
unique(labels_synched$label_name)

# Replacing label_name's that contain "Playback" with "Recruitment Playback Track" 
labels_synched$label_name <- ifelse(grepl("Playback", labels_synched$label_name), "Recruitment Playback Track", labels_synched$label_name)

# Replacing label_name's that contain "fu xx+yy" with "zz" 
labels_synched$label_name <- ifelse(
  labels_synched$label_name %in% c("fu soc+s", "fu sn+soc"), "s",
  ifelse(
    labels_synched$label_name %in% c("fu sn+cc", "fu cc+s", "fu cc+agg"), "cc",
    ifelse(
      labels_synched$label_name == "fu soc+agg", "soc",
      ifelse(
        labels_synched$label_name == "fu s+ld", "ld",
        labels_synched$label_name
      )
    )
  )
)

labels_synched$label_name <- ifelse(grepl("rolling", labels_synched$label_name), "al", labels_synched$label_name)
labels_synched$label_name <- ifelse(grepl("chat", labels_synched$label_name), "agg", labels_synched$label_name)
labels_synched$label_name <- ifelse(grepl("sn", labels_synched$label_name), "s", labels_synched$label_name)

# Replacing all 'label_name's that are not needed further
labels_synched$label_name <- ifelse(grepl("nf|synch|eating|START|END|start|bir|oor|2023|beep", labels_synched$label_name), NA, labels_synched$label_name)

# Remove unwanted characters while keeping the first character
labels_synched$label_name <- gsub("^[^A-Za-z]*(.*)$", "\\1", labels_synched$label_name)
labels_synched$label_name <- gsub(" ?[x%*?] ?", "", labels_synched$label_name)

# Print modified label_name's to check the results
unique(labels_synched$label_name)

# Set options to display microseconds
options(digits.secs = 6)

# Convert t0_UTC and tf_UTC to POSIXct format
labels_synched$t0_UTC <- ymd_hms(labels_synched$t0_UTC, tz = "UTC")
labels_synched$tmid_UTC <- ymd_hms(labels_synched$tmid_UTC, tz = "UTC")
labels_synched$tf_UTC <- ymd_hms(labels_synched$tf_UTC, tz = "UTC")

# Create new dataframe for rows where label_name contains "sq" for "sequence"
sq_rows <- labels_synched %>% filter(grepl("sq ", label_name))

# Create new dataframe for changing the t0_UTC for sq labels
sq_label_t0 <- sq_rows %>%
  mutate(
    label_part1 = sub("sq (.*)\\+.*", "\\1", label_name),
    half_duration = duration / 2) %>%
  mutate(
    tf_UTC = t0_UTC + half_duration,
    label_name = label_part1,
    duration = half_duration) %>%
  select(-label_part1, -half_duration)

# Create new dataframe for changing the tf_UTC for sq labels
sq_label_tf <- sq_rows %>%
  mutate(
    label_part2 = sub("sq .*\\+(.*)", "\\1", label_name),
    half_duration = duration / 2) %>%
  mutate(
    t0_UTC = t0_UTC + half_duration,
    label_name = label_part2,
    duration = half_duration) %>%
  select(-label_part2, -half_duration)

# Combine the new dataframes for sq_label_t0 and sq_label_tf
new_label_rows <- bind_rows(sq_label_t0, sq_label_tf)

# Calculate the new tmid_UTC times as the midpoint between t0_UTC and tf_UTC for sq rows
new_label_rows$tmid_UTC <- new_label_rows$t0_UTC + 
  (new_label_rows$tf_UTC - new_label_rows$t0_UTC) / 2

# Combine the new rows with the original dataframe
labels_synched_2 <- bind_rows(labels_synched, new_label_rows)

# Remove rows with 'sq' in the label_name
labels_synched_2 <- labels_synched_2 %>% filter(!grepl("sq", label_name))

# Remove rows that have NA label names
cleaned_audio_df <- labels_synched_2[!is.na(labels_synched_2$label_name), ]
cleaned_audio_df$X <- NULL

# Rename ID_Code
cleaned_audio_df$id_code <- ifelse(grepl("VSIF021", cleaned_audio_df$id_code), "VSIM021", cleaned_audio_df$id_code)
cleaned_audio_df$id_code <- ifelse(grepl("VSIF10", cleaned_audio_df$id_code), "VSIF010", cleaned_audio_df$id_code)

unique(cleaned_audio_df$label_name)

# Save cleaned DataFrame to a new CSV file
# write.csv(cleaned_audio_df, file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_preGD.csv", row.names = FALSE)

# Set Up Directory Paths
base_dir <- "/mnt/EAS_ind/aeiberle/data/"
pre_audio_df <- read.csv(paste0(base_dir, "labels_synched_2023_preGD.csv"))
GD_audio_df <- read.csv(paste0(base_dir, "GD_20230722_audio_synched.csv"))

GD_audio_df$X <- NULL

unique(GD_audio_df$label_name)

# Replacing label_names and remove "nf"
GD_audio_df$label_name <- gsub(" nf", "", GD_audio_df$label_name)

GD_audio_df$label_name <- ifelse(grepl("play", GD_audio_df$label_name), "Recruitment Playback Track", GD_audio_df$label_name)
GD_audio_df$label_name <- ifelse(grepl("chat", GD_audio_df$label_name), "agg", GD_audio_df$label_name)
GD_audio_df$label_name <- ifelse(grepl("sn", GD_audio_df$label_name), "s", GD_audio_df$label_name)
GD_audio_df$label_name <- ifelse(grepl("rec", GD_audio_df$label_name), "agg", GD_audio_df$label_name)

GD_audio_df$label_name <- ifelse(grepl("synch|sync|eating|START|END|start|STOP|bir|oor|2023|beep|Marker 79", GD_audio_df$label_name), NA, GD_audio_df$label_name)

# Remove rows that have NA label names
GD_audio_df <- GD_audio_df[!is.na(GD_audio_df$label_name), ]

unique(GD_audio_df$label_name)

# Combine the two data frames by appending rows
complete_audio_df <- rbind(pre_audio_df, GD_audio_df)

unique(complete_audio_df$label_name)


# Write complete_audio_df to a new CSV file
# write.csv(complete_audio_df, file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_complete.csv", row.names = FALSE)

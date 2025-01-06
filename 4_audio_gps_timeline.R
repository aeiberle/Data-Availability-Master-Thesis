
# Read in the CSV files
audio_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_preGD.csv", header = TRUE, sep = ",")
gps_df <- read.csv(file = "/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/distance_to_speaker.csv", header = TRUE, sep = ",")

# gps_df_SI <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_20230615/gps_spdf_20230615.csv", header = TRUE, sep = ",")
# gps_df_BS1 <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_20230624/gps_spdf_20230624.csv", header = TRUE, sep = ",")
# gps_df_BS2 <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_20230716/gps_spdf_20230716.csv", header = TRUE, sep = ",")
# 
# # Combine gps_df_BS1 and gps_df_BS2 into one dataframe
# gps_df_combined <- bind_rows(gps_df_SI, gps_df_BS1, gps_df_BS2)

# Remove microseconds from audio_df timestamps and convert to POSIXct
audio_df$t0_UTC <- as.POSIXct(sub("\\..*", "", audio_df$t0_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
audio_df$tmid_UTC <- as.POSIXct(sub("\\..*", "", audio_df$tmid_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
audio_df$tf_UTC <- as.POSIXct(sub("\\..*", "", audio_df$tf_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

gps_df$dattime <- gsub("T", " ", gsub("Z", "", gps_df$dattime))

gps_df$dattime <- as.POSIXct(gps_df$dattime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

# Perform a left join operation to match audio_df to gps_df
matched_df <- merge(gps_df, audio_df, by.x = c("ID", "dattime"), by.y = c("id_code", "t0_UTC"), all.x = TRUE)

# Write matched_df to a new CSV file
write.csv(matched_df, file = "/mnt/EAS_ind/aeiberle/data/audio_gps_timeline.csv", row.names = FALSE)

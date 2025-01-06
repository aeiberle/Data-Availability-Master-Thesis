#library(cocomo)
library(dplyr)
library(ggplot2)
library(gridExtra)


# Set Up Directory Paths
root_dir <- "/mnt/EAS_shared/meerkat/working/newdata/TO_ARCHIVE/meerkat_movecomm_2023/"
alt_root_dir <- "/mnt/EAS_ind/aeiberle/data/"
output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/"


#### Combine dataframes ####
# Load CSV containing all gps_data with speaker~individ distance and individ~individ distance
gps_aggregated_all <- read.csv("/mnt/EAS_ind/aeiberle/data/aggregated_gps_data_all.csv", sep = ",")

# Load CSV containing audio data
audio_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_preGD.csv", header = TRUE, sep = ",")

# Remove microseconds from audio_df timestamps and convert to POSIXct
audio_df$t0_UTC <- as.POSIXct(sub("\\..*", "", audio_df$t0_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
audio_df$tmid_UTC <- as.POSIXct(sub("\\..*", "", audio_df$tmid_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
audio_df$tf_UTC <- as.POSIXct(sub("\\..*", "", audio_df$tf_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

gps_aggregated_all$dattime <- as.POSIXct(gps_aggregated_all$dattime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


audio_gps_distances_speaker <- merge(gps_aggregated_all, audio_df, by.x = c("dattime", "ID"), by.y = c("t0_UTC", "id_code"), all.x = TRUE)

colnames(audio_gps_distances_speaker)[colnames(audio_gps_distances_speaker) == "dist"] <- "dist_speaker"
colnames(audio_gps_distances_speaker)[colnames(audio_gps_distances_speaker) == "Z_dist"] <- "Z_dist_speaker"

#write.csv2(audio_gps_distances_speaker, file = "/mnt/EAS_ind/aeiberle/data/audio_gps_distances_speaker_new.csv", row.names = FALSE)



# Define Specific Filenames for Each Date
specific_filenames_20230615 <- c("SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46.txt",
                                 "SI_VECM009_SH_H_Axy_030_20230613-20230615_S24.txt",
                                 "SI_VZUF048_GX011333_HERO9 Black-GPS5.txt",
                                 "SI_VSIF013_GX010795_HERO9 Black-GPS5.txt")

specific_filenames_20230701 <- c("SI_VECM009_SH+H_Axy029_S1.txt",
                                 "SI_VSIF023_RS_Axy026_S1.txt",
                                 "SI_VSIF013_GX010879_HERO9 Black-GPS5.txt",
                                 "SI_VSIM015_RR+MB_Axy016_S1.txt",
                                 "SI_VSIM021_RT_Axy036_S1.txt",
                                 "SI_VZUF048_GH011629_HERO9 Black-GPS5.txt")

specific_filenames_20230624 <- c("BS_VBSF002_RS_RT_Axy_035_20230624-20230628_S1.txt",
                                 "BS_VBSF008_LT_Axy_015_20230624-20230628_Axy015_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_003_20230624-20230628_S1.txt",
                                 "BS_VBSF001_RS_TB_Axy_022_20230624-20230628_S1.txt",
                                 "BS_VHMM036_RR_LT_Axy_021_20230624-20230628_Axy021_S1.txt",
                                 "BS_VMPF035_GPS_20230624_Axy063_S3.txt")

specific_filenames_20230716 <- c("BS_VBSF008_LT_Axy_001_20230716-20230717_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_006_20230712-20230717_S1.txt",
                                 "BS_VHMM036_RR_LT_Axy_010_20230714-20230717_S1.txt",
                                 "BS_VBSF001_GPS_20230716_Axy022_S5.txt",
                                 "BS_VMPF035_GPS_20230716_Axy021_S2.txt")

specific_filenames_20230621 <- c("GD_VGDM015_20230621_GARMIN.txt",
                                 "GD_VGDF007_20230621_GX010835_HERO9 Black-GPS5.txt")

specific_filenames_20230722 <- c("GD_FOCAL1_Axy021_S1.txt",
                                 "GD_FOCAL2_20230722_GARMIN.txt")

specific_filenames_20230720 <- c("ZU_VZUF028_RC+MB+TB_Axy021_S4.txt",
                                 "ZU_VZUF051_RR+TB_Axy10_S1.txt",
                                 "ZU_VZUF052_RR+RT_Axy 019_S1.txt",
                                 "ZU_VZUF070_RT_Axy025_S1.txt",
                                 "ZU_VZUM063_SH+RT_Axy028_S3.txt",
                                 "ZU_VZUM064_SH+LT_Axy022_S7.txt",
                                 "ZU_VZUM066_SH+RS_Axy011_S1.txt",
                                 "ZU_VZUM067_TB_Axy007_S1.txt")

specific_filenames_20230730 <- c("ZU_VZUF051_RR+TB_Axy10_S1.txt",
                                 "ZU_VZUF052_RR+RT_Axy019_S1.txt",
                                 "ZU_VZUF070_RT_Axy033_S1.txt",
                                 "ZU_VZUM066_SH+RS_Axy012_S1.txt",
                                 "ZU_VZUM067_TB_Axy007_S1.txt")


# Combine All Specific Filenames into a List
date_specific_files <- list(
  "2023-06-15" = specific_filenames_20230615,
  "2023-07-01" = specific_filenames_20230701,
  "2023-06-24" = specific_filenames_20230624,
  "2023-07-16" = specific_filenames_20230716,
  "2023-06-21" = specific_filenames_20230621,
  "2023-07-22" = specific_filenames_20230722,
  "2023-07-20" = specific_filenames_20230720,
  "2023-07-30" = specific_filenames_20230730)


# complete_audio_gps_distances_speaker <- ggplot(data = audio_gps_distances_speaker, aes(x = re_numeric, y = Z_dist_speaker)) + 
#   geom_smooth(method = "gam") + 
#   geom_point() +
#   geom_vline(xintercept = 0, linetype = "dotted", color = "red", linewidth = 1.5) + 
#   geom_vline(xintercept = c(-60, 60), linetype = "dotted", color = "grey", linewidth = 1) + 
#   ggtitle("all normalized distances speaker") + 
#   theme_minimal() + 
#   theme(plot.background = element_rect(fill = "white", color = NA),
#         panel.background = element_rect(fill = "white", color = NA),
#         panel.grid.major = element_line(color = "grey90"),
#         panel.grid.minor = element_line(color = "grey80"),
#         plot.title = element_text(size = 14),
#         axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         legend.position = "right")
# 
# print(complete_audio_gps_distances_speaker)
# 
# #Save the plot
# #ggsave(filename = paste0(output_dir, "speaker_distance_normalized_all_plot.png"), plot = complete_audio_gps_distances_speaker, width = 18, height = 9)



#### Signal Propagation plot inter-individual distance ####
# Plotting any first call of an individual after the start of the PB in relation to distance inter-individual 
# --> df = audio_gps_distances_speaker

# Step 1: Trim the data to keep re_numeric between 0 and 600
audio_gps_distances_speaker_NEW <- subset(audio_gps_distances_speaker, re_numeric >= 0 & re_numeric <= 120)

# Keep only the integer part of re_numeric (truncate decimals)
audio_gps_distances_speaker_NEW$re_numeric <- trunc(audio_gps_distances_speaker_NEW$re_numeric)


# Step 2: Filter for the first row per timestamp and ID
audio_gps_distances_speaker_NEW_filtered <- audio_gps_distances_speaker_NEW %>%
  group_by(ID, dattime) %>%
  slice_head(n = 1) %>%  # Keep the first row for each group (timestamp + ID)
  ungroup()


# Step 3: Filter rows where 'label_name' is not NA or empty
audio_gps_distances_speaker_NEW_filtered <- subset(audio_gps_distances_speaker_NEW_filtered, !is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track")


# Step 4: Plot the data
signals_speaker_after_PB <- ggplot(audio_gps_distances_speaker_NEW_filtered, aes(x = re_numeric, y = dist_speaker, colour = label_name)) +
  geom_point() +
  geom_smooth(method = "gam") + 
  labs(title = "Calls after Playback",
       x = "re_numeric", 
       y = "dist_speaker") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right") + 
  ylim(c(min(audio_gps_distances_speaker_NEW_filtered$dist_speaker), max(audio_gps_distances_speaker_NEW_filtered$dist_speaker)))


print(signals_speaker_after_PB)

#Save the plot
#ggsave(filename = paste0(output_dir, "signal_propagation_speaker_after_PB_calls_plot.png"), plot = signals_speaker_after_PB, width = 18, height = 9)



# removing close calls
audio_gps_distances_speaker_NEW_filtered_no_cc <- subset(audio_gps_distances_speaker_NEW_filtered, label_name != "cc")

signals_speaker_after_PB_no_cc <- ggplot(audio_gps_distances_speaker_NEW_filtered_no_cc, aes(x = re_numeric, y = dist_speaker, colour = label_name)) +
  geom_point() +
  geom_smooth(method = "gam") + 
  labs(title = "Calls after Playback no cc",
       x = "re_numeric", 
       y = "dist_speaker") +
  theme_minimal() + 
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right") + 
  ylim(c(min(audio_gps_distances_speaker_NEW_filtered$dist_speaker), max(audio_gps_distances_speaker_NEW_filtered$dist_speaker)))


print(signals_speaker_after_PB_no_cc)

#Save the plot
#ggsave(filename = paste0(output_dir, "signal_propagation_speaker_after_PB_no_cc_plot.png"), plot = signals_speaker_after_PB_no_cc, width = 18, height = 9)


#library(gridExtra)
#signals_distances_speaker <- grid.arrange(signals_speaker_after_PB, signals_speaker_after_PB_no_cc, ncol = 1, nrow = 2)

#Save the plot
#ggsave(filename = paste0(output_dir, "signal_propagation_speaker_after_PB_120s_comparison_plot.png"), plot = signals_distances_speaker, width = 18, height = 9)






#### Signal Propagation plot speaker distance each date in a grid ####
# Plotting any first call of an individual after the start of the PB in relation to distance inter-individual
# --> df = audio_gps_distances_speaker
plot_list <- list()

# Loop through each unique date and save the plot for each
for (date_val in unique(audio_gps_distances_speaker_NEW_filtered$date)) {
  # Filter data for the specific date
  data_for_date <- subset(audio_gps_distances_speaker_NEW_filtered, date == date_val)

  # Create the plot for this date
  signal_plot_date <- ggplot(data_for_date, aes(x = re_numeric, y = dist_speaker)) +
    geom_point() +
    geom_smooth(method = "gam") +
    labs(title = paste("Calls after Playback - Date:", date_val),
         x = "re_numeric",
         y = "dist_speaker") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "grey90"),
          panel.grid.minor = element_line(color = "grey80"),
          plot.title = element_text(size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12),
          legend.position = "right") +
    ylim(c(min(data_for_date$dist_speaker), max(data_for_date$dist_speaker))) # Adjust y-limits to data range

  # Store the plot in the list
  plot_list[[paste0("signal_plot_date_120s_", date_val)]] <- signal_plot_date

  # Save the plot as a file (using the date value in the filename)
  # ggsave(filename = paste0(output_dir, "signal_plot_date_", date_val, ".png"), plot = signal_plot_date, width = 18, height = 9)
}


label_plot_date <- ggplot(audio_gps_distances_speaker_NEW, aes(x = re_numeric, y = dist_speaker)) +
  geom_point() +
  geom_smooth(method = "gam") +
  labs(title = paste("Calls after Playback - Date:", date_val),
       x = "re_numeric",
       y = "dist_speaker") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12),
        legend.position = "right") +
  ylim(c(min(data_for_date$dist_speaker), max(data_for_date$dist_speaker)))+
  facet_grid(~label_name)

print(label_plot_date)

# Step 5: Arrange all the plots in a grid
grid.arrange(grobs = plot_list, ncol = 2, nrow = 3)  # Set the number of columns in the grid

# Step 6: Save the combined grid of plots as a single file
#ggsave(filename = paste0(output_dir, "signal_plot_date_120s_grid.png"), plot = grid.arrange(grobs = plot_list, ncol = 1, nrow = 3), width = 12, height = 8)

# Counts ####
# Counts of call types on trial
table(audio_gps_distances_speaker_NEW_filtered$label_name, audio_gps_distances_speaker_NEW_filtered$trial)



# #### Signal Propagation plot inter-individual distance ####
# # Plotting any first call of an individual after the start of the PB --> df = audio_gps_distances_dyadic_NEW
# audio_gps_distances_dyadic_NEW <- big_df
# 
# # Step 1: Trim the data to make re_numeric start from 0
# audio_gps_distances_dyadic_NEW <- audio_gps_distances_dyadic_NEW %>%
#   group_by(id_code) %>%
#   mutate(re_numeric = re_numeric - min(re_numeric)) %>%  # Shift re_numeric so it starts at 0
#   ungroup()
# 
# audio_gps_distances_dyadic_NEW <- subset(audio_gps_distances_dyadic_NEW, re_numeric > 0)
# 
# # Step 2: Filter for the first row per timestamp and id_code
# audio_gps_distances_dyadic_NEW_filtered <- audio_gps_distances_dyadic_NEW %>%
#   group_by(id_code, timestamps) %>%
#   slice_head(n = 1) %>%  # Keep the first row for each group (timestamp + id_code)
#   ungroup()
# 
# # Step 3: Filter data for re_numeric between 0 and 600
# audio_gps_distances_dyadic_NEW_filtered_window <- audio_gps_distances_dyadic_NEW_filtered %>%
#   filter(re_numeric >= 0 & re_numeric <= 120)
# 
# # Step 4: Plot the data
# signals_after_PB <- ggplot(audio_gps_distances_dyadic_NEW_filtered_window, aes(x = re_numeric, y = mean_dist_individual)) +
#   geom_point() +
#   geom_smooth(method = "gam") + 
#   labs(title = "Calls after Playback",
#        x = "re_numeric", 
#        y = "Z_dist_speaker") +
#   theme_minimal() + 
#   theme(plot.background = element_rect(fill = "white", color = NA),
#         panel.background = element_rect(fill = "white", color = NA),
#         panel.grid.major = element_line(color = "grey90"),
#         panel.grid.minor = element_line(color = "grey80"),
#         plot.title = element_text(size = 14),
#         axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         legend.position = "right")
# 
# print(signals_after_PB)
# 
# 
# # removing close calls
# audio_gps_distances_dyadic_NEW_filtered_window_no_cc <- subset(audio_gps_distances_dyadic_NEW_filtered_window, label_name != "cc")
# 
# signals_after_PB_no_cc <- ggplot(audio_gps_distances_dyadic_NEW_filtered_window_no_cc, aes(x = re_numeric, y = mean_dist_individual)) +
#   geom_point() +
#   geom_smooth(method = "gam") + 
#   labs(title = "Calls after Playback no cc",
#        x = "re_numeric", 
#        y = "Z_dist_speaker") +
#   theme_minimal() + 
#   theme(plot.background = element_rect(fill = "white", color = NA),
#         panel.background = element_rect(fill = "white", color = NA),
#         panel.grid.major = element_line(color = "grey90"),
#         panel.grid.minor = element_line(color = "grey80"),
#         plot.title = element_text(size = 14),
#         axis.title.x = element_text(size = 14),
#         axis.title.y = element_text(size = 14),
#         axis.text.x = element_text(size = 12),
#         axis.text.y = element_text(size = 12),
#         legend.title = element_text(size = 14),
#         legend.text = element_text(size = 12),
#         legend.position = "right")
# 
# print(signals_after_PB_no_cc)
# 
# #Save the plot
# #ggsave(filename = paste0(output_dir, "signal_propagation_after_PB_plot.png"), plot = signals_after_PB_no_cc, width = 18, height = 9)




# #### Combine dataframes ####
# # Load CSV containing all gps_data with speaker~individ distance and individ~individ distance
# gps_aggregated_all <- read.csv("/mnt/EAS_ind/aeiberle/data/aggregated_gps_data_all.csv", sep = ",")
# 
# # merge dataframes meean_dist + gps_aggregated_all
# gps_distances_speaker_dyadic <- merge(gps_aggregated_all, meean_dist, by.x = c("dattime"), by.y = c("timestamps"), all.x = TRUE)
# 
# colnames(gps_distances_speaker_dyadic)[colnames(gps_distances_speaker_dyadic) == "mean_dist"] <- "mean_dist_individ"
# colnames(gps_distances_speaker_dyadic)[colnames(gps_distances_speaker_dyadic) == "dist"] <- "dist_speaker"
# 
# #write.csv2(gps_distances_speaker_dyadic, file = "/mnt/EAS_ind/aeiberle/data/gps_distances_speaker_dyadic.csv", row.names = FALSE)
# 
# 
# # Load CSV containing audio data
# audio_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_new.csv", header = TRUE, sep = ",")
# 
# # Remove microseconds from audio_df timestamps and convert to POSIXct
# audio_df$t0_UTC <- as.POSIXct(sub("\\..*", "", audio_df$t0_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# audio_df$tmid_UTC <- as.POSIXct(sub("\\..*", "", audio_df$tmid_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# audio_df$tf_UTC <- as.POSIXct(sub("\\..*", "", audio_df$tf_UTC), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# gps_distances_speaker_dyadic$dattime <- as.POSIXct(gps_distances_speaker_dyadic$dattime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
# 
# 
# audio_gps_distances_speaker_dyadic <- merge(gps_distances_speaker_dyadic, audio_df, by.x = c("dattime", "ID"), by.y = c("t0_UTC", "id_code"), all.x = TRUE)
# 
# #write.csv2(audio_gps_distances_speaker_dyadic, file = "/mnt/EAS_ind/aeiberle/data/audio_gps_distances_speaker_dyadic.csv", row.names = FALSE)
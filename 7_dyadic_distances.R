#### LOOP for displaying kernel densities plot for a given time window ####
### using all available GPS data

library(sp)
library(geosphere)
library(dplyr)
library(ggplot2)
library("scales")


# Spatial Discretization Function
spatialDiscr <- function(x, y, step) {
  currentPoint <- which(!is.na(x))[1]
  points <- currentPoint
  t <- currentPoint + 1
  dist = 0
  while (t <= length(x)) {
    while (dist < step & t <= length(x)) {
      if (is.na(x[t])) {
        t <- t + 1
        next()
      }
      dist <- sqrt((x[t] - x[currentPoint])^2 + (y[t] - y[currentPoint])^2)
      t <- t + 1
    }
    currentPoint <- t - 1
    points <- c(points, currentPoint)
    dist <- 0
  }
  points
}


# Sample GPS every 50 m; possible to change it
distance <- 50


# Set Up Directory Paths
root_dir <- "/mnt/EAS_shared/meerkat/working/newdata/TO_ARCHIVE/meerkat_movecomm_2023/"
alt_root_dir <- "/mnt/EAS_ind/aeiberle/data/"
output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/"

# Define Specific Filenames for Each Date
specific_filenames_20230615 <- c("SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46.txt",
                                 "SI_VECM009_SH_H_Axy_030_20230613-20230615_S24.txt",
                                 "SI_VZUF048_GX011333_HERO9 Black-GPS5.txt",
                                 "SI_VSIF013_GX010795_HERO9 Black-GPS5.txt"
)

specific_filenames_20230624 <- c("BS_VBSF002_RS_RT_Axy_035_20230624-20230628_S1.txt",
                                 "BS_VBSF008_LT_Axy_015_20230624-20230628_Axy015_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_003_20230624-20230628_S1.txt",
                                 "BS_VBSF001_RS_TB_Axy_022_20230624-20230628_S1.txt",
                                 "BS_VHMM036_RR_LT_Axy_021_20230624-20230628_Axy021_S1.txt",
                                 "BS_VMPF035_GPS_20230624_Axy063_S3.txt"
)

specific_filenames_20230716 <- c("BS_VBSF008_LT_Axy_001_20230716-20230717_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_006_20230712-20230717_S1.txt",
                                 "BS_VHMM036_RR_LT_Axy_010_20230714-20230717_S1.txt",
                                 "BS_VBSF001_GPS_20230716_Axy022_S5.txt",
                                 "BS_VMPF035_GPS_20230716_Axy021_S2.txt"
)



specific_filenames_20230722 <- c("GD_FOCAL1_Axy021_S1.txt",
                                 "GD_FOCAL2_20230722_GARMIN.txt"
)


specific_filenames_20230621 <- c("GD_VGDM015_20230621_GARMIN.txt",
                                 "GD_VGDF007_20230621_GX010835_HERO9 Black-GPS5.txt"
)


specific_filenames_20230701 <- c("SI_VECM009_SH+H_Axy029_S1.txt",
                                 "SI_VSIF023_RS_Axy026_S1.txt",
                                 "SI_VSIF013_GX010879_HERO9 Black-GPS5.txt",
                                 "SI_VSIM015_RR+MB_Axy016_S1.txt",
                                 "SI_VSIM021_RT_Axy036_S1.txt",
                                 "SI_VZUF048_GH011629_HERO9 Black-GPS5.txt"
)

specific_filenames_20230720 <- c("ZU_VZUF028_RC+MB+TB_Axy021_S4.txt",
                                 "ZU_VZUF051_RR+TB_Axy10_S1.txt",
                                 "ZU_VZUF052_RR+RT_Axy 019_S1.txt",
                                 "ZU_VZUF070_RT_Axy025_S1.txt",
                                 "ZU_VZUM063_SH+RT_Axy028_S3.txt",
                                 "ZU_VZUM064_SH+LT_Axy022_S7.txt",
                                 "ZU_VZUM066_SH+RS_Axy011_S1.txt",
                                 "ZU_VZUM067_TB_Axy007_S1.txt"
)

specific_filenames_20230730 <- c("ZU_VZUF051_RR+TB_Axy10_S1.txt",
                                 "ZU_VZUF052_RR+RT_Axy019_S1.txt",
                                 "ZU_VZUF070_RT_Axy033_S1.txt",
                                 "ZU_VZUM066_SH+RS_Axy012_S1.txt",
                                 "ZU_VZUM067_TB_Axy007_S1.txt"
                                 
)


# Combine All Specific Filenames into a List
date_specific_files <- list(
  "2023-06-15" = specific_filenames_20230615,
  "2023-06-24" = specific_filenames_20230624,
  "2023-07-16" = specific_filenames_20230716,
  "2023-07-22" = specific_filenames_20230722,
  "2023-06-21" = specific_filenames_20230621,
  "2023-07-01" = specific_filenames_20230701,
  "2023-07-20" = specific_filenames_20230720,
  "2023-07-30" = specific_filenames_20230730
)


# Load CSV containing start and end times for each date
time_window <- read.csv("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/meta_data/time_startend_info-copy.csv", sep = ";")

#load playback metadata
playback_details <- read.csv("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/meta_data/PB_time_startend_info-copy.csv", sep = ";")

# Assuming the CSV has columns "Date", "t0" (start time), and "t1" (end time)
# Convert the Date column to POSIXlt to match the date format
time_window$Date <- as.POSIXlt(time_window$Date, format = "%Y-%m-%d", tz = "UTC")

#make a data frame for combined data
aggregated_gps_data_all <- data.frame()

#date <- "2023-07-01"
# Loop through each date
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and its subfolders and check if the date is 2023-06-15 to use the alternate root directory
  
    all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  
  
  # Filter files based on specific filenames for the current date
  file_list <- all_files[basename(all_files) %in% specific_filenames]
  
  # Find the row in the CSV that matches the current date
  time_window_row <- time_window[time_window$Date == as.POSIXlt(date, format = "%Y-%m-%d", tz = "UTC"), ]
  
  # Ensure the matching row exists
  if (nrow(time_window_row) == 1) {
    start_time <- as.POSIXlt(paste(date, time_window_row$t0), tz = "UTC")
    end_time <- as.POSIXlt(paste(date, time_window_row$t1), tz = "UTC")
  } else {
    warning(paste("No matching time window found for date:", date))
    next  # Skip to the next iteration if no matching time window is found
  }
  
  # Initialize an empty data frame to store aggregated GPS data for the current date
  aggregated_gps_data <- data.frame(V1 = as.POSIXlt(character()),
                                    V2 = numeric(),
                                    V3 = numeric(),
                                    stringsAsFactors = FALSE)
  
  # Loop through each file found
  for (file_GPS in file_list) {
    
    #get filename
    file_name <- basename(file_GPS)
    
    ID <- strsplit(file_name, "_" )[[1]][2]
    # Load the GPS file
    gps <- read.delim(file = file_GPS, header = FALSE)
    
    # Prepare the data
    gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
    gps <- gps[, -2]
    colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
    
    gps$V2 <- as.numeric(as.character(gps$V2))
    gps$V3 <- as.numeric(as.character(gps$V3))
    
    gps <- gps[complete.cases(gps), ]
    gps$ID <- ID
    
    # Convert the V1 column to POSIXlt
    gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
                                                "%Y/%m/%d %H:%M:%OS",
                                                "%Y-%m-%d %H:%M",
                                                "%Y/%m/%d %H:%M",
                                                "%Y-%m-%d",
                                                "%Y/%m/%d"), tz = "UTC")
    
    # Filter by start and end time
    gps <- gps[gps$V1 > start_time & gps$V1 < end_time, ]
    
    # Combine with the aggregated data
    aggregated_gps_data <- rbind(aggregated_gps_data, gps)
  }

 
  #get lat and long for speaker position on the day
  lat <- playback_details[which(playback_details$Date == date), "lat"]
  lon <- playback_details[which(playback_details$Date == date), "lon"]
  
  lat <- as.numeric(sub(",", ".", lat, fixed = TRUE))
  lon <- as.numeric(sub(",", ".", lon, fixed = TRUE))
  #get PB time for the playback
  PB_time <- playback_details[which(playback_details$Date == date), "t0"]
  PB_time <- as.POSIXct(paste(date, PB_time), tz = "UTC")
  
  #get group
  group <- playback_details[which(playback_details$Date == date), "Group"]
  
  #get treatment
  treatment <- playback_details[which(playback_details$Date == date), "Type"]

  #calculate distance to speaker for every point
  for (i in 1:nrow(aggregated_gps_data)){
  aggregated_gps_data$dist[i] <- distm(c(aggregated_gps_data$V2[i], aggregated_gps_data$V3[i]), c(lat, lon), fun = distGeo)
  }
  
  
  #trim the dataframe and rename columns
  aggregated_gps_data <- aggregated_gps_data[ , c(1:3, 9:10)]
  colnames(aggregated_gps_data) <- c("dattime", "lat", "lon", "ID", "dist" )
  
  #scaling of the distances between 0 and 1
  #aggregated_gps_data$Z_dist <- scale(aggregated_gps_data$dist, center = T, scale = F)
  aggregated_gps_data$Z_dist <- rescale(aggregated_gps_data$dist, to=c(0,1))
  
  #formatting of datttime
  aggregated_gps_data$dattime <- as.POSIXct(aggregated_gps_data$dattime, tz = "UTC")
  aggregated_gps_data$numeric <- as.numeric(aggregated_gps_data$dattime)
  
  #calculate time relative to PB
  aggregated_gps_data$re_numeric <- aggregated_gps_data$numeric - as.numeric(PB_time)
  
  #add more trial details
  aggregated_gps_data$group <- group
  aggregated_gps_data$treatment <- treatment
  aggregated_gps_data$trial <- paste(date, group)
  
  
  #trim everything to 10 min interval
  aggregated_gps_data <- subset(aggregated_gps_data, abs(re_numeric) < 601)
  trial <- paste(date, group)
 
  
  #combine all in one big dataframe
  aggregated_gps_data_all <- rbind(aggregated_gps_data_all, aggregated_gps_data)
  #trim everything to 10 min interval
  aggregated_gps_data_all <- subset(aggregated_gps_data_all, abs(re_numeric) < 601)
  
  
  #plot the deployment. Change in distances to the speaker over time
  print(ggplot(data = aggregated_gps_data, aes(x = re_numeric, y = dist, colour = ID)) + geom_smooth() + 
          geom_vline(xintercept = 0, linetype="dotted", 
                     color = "red", size=1.5) + ggtitle(date))
}
    

  
 # rescale the distance to speaker data between 0 and 1 
aggregated_gps_data_all <- aggregated_gps_data_all %>%
  group_by(trial)%>% 
  mutate(Z_dist = rescale(dist, to=c(0,1)))



#plot stuff
ggplot(data = aggregated_gps_data_all, aes(x = re_numeric, y = Z_dist)) + geom_smooth(method = "gam") + 
        geom_vline(xintercept = 0, linetype="dotted", 
                   color = "red", size=1.5) + geom_vline(xintercept = c(-60, 60), linetype="dotted", 
                                                         color = "grey", size=1) + 
  ggtitle("all combined") + theme_minimal()
  
  
ggplot(data = aggregated_gps_data_all, aes(x = re_numeric, y = Z_dist, colour = treatment)) + geom_smooth() + 
  geom_vline(xintercept = 0, linetype="dotted", 
             color = "black", size=1.5) + geom_vline(xintercept = c(-60, 60), linetype="dotted", 
                                                   color = "grey", size=1) + 
  ggtitle("by_trial") + theme_minimal()


#bin by 10 sec
aggregated_gps_data_all$bins <- cut(aggregated_gps_data_all$re_numeric , breaks = seq(-601, 601, by = 10), labels = FALSE)

#calculate mean and median by trials and bins

library(dplyr)


by_bin <- aggregated_gps_data_all%>%
  group_by(trial, bins)%>% 
  summarise(Mean=mean(dist), Median=median(dist), ZMean = mean(Z_dist))


#plot mean distances by bin
ggplot(data = by_bin, aes(x = bins, y = ZMean)) + geom_smooth(method = "gam") + 
  geom_vline(xintercept = 60, linetype="dotted", 
             color = "black", size=1.5) + geom_vline(xintercept = c(-6, 6), linetype="dotted", 
                                                     color = "grey", size=1) + 
  ggtitle("by_trial") + theme_minimal()


#### convert to X and Y matrix #####
library(cocomo)
# save ew datframe for renaming
aggregated_gps_data_all_test <- aggregated_gps_data_all
#rename column names
colnames(aggregated_gps_data_all_test) <- c("timestamp", "location.lat", "location.long", "individual.local.identifier", "dist", "Z_dist", "numeric", "re_numeric", "group", "treatment", "trial")

#format the data chunks data_frame
data_chunks <- time_window
data_chunks$start <- paste(data_chunks$Date, data_chunks$t0)
data_chunks$end <- paste(data_chunks$Date, data_chunks$t1)
data_chunks <- data_chunks[, c(5,6)]
data_chunks$start <- as.POSIXct(data_chunks$start, tz =  "UTC")
data_chunks$end <- as.POSIXct(data_chunks$end, tz = "UTC")

##    #make Xs and Ys matrices
##    reformat_movebank_to_matrix(
##      aggregated_gps_data_all_test,
##      output_file_path = "/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/meta_data/GPS_matrices.RData",
##      ids = NULL,
##      data_chunks = data_chunks,
##      seconds_per_time_step = 1,
##      start_date = NULL,
##      end_date = NULL,
##      start_time = NULL,
##      end_time = NULL,
##      utm_zone = 34,
##      hemisphere = "south",
##      output_utm = T,
##      output_latlon = T,
##      use_UTC = T,
##      local_timezone = NULL
##    )
##    

#load matrices
load("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/meta_data/GPS_matrices.RData")

#calculate dyadic distances
distances <- get_group_dyadic_distances(xs, ys)
#get means
mean_dist <- apply(distances, 3, mean, na.rm = T )

#add date-times to the mead dyadic distances
meean_dist <- cbind(data.frame(timestamps), mean_dist)
meean_dist$date <- stringr::str_sub(meean_dist$timestamps, 1, 10)
meean_dist$numeric <- as.numeric(meean_dist$timestamps)

#remove NAs
meean_dist <- meean_dist[which(complete.cases(meean_dist)), ]

#plot
dyadic_distance_plot <- ggplot(data = meean_dist, aes(x= numeric, y = mean_dist)) + 
  geom_smooth(method = "gam") + 
  geom_jitter() +
  facet_wrap(~date, scales = "free")

 print(dyadic_distance_plot)

 #Save the plot
#ggsave(filename = paste0(output_dir, "dyadic_distance_plot_grid.png"), plot = dyadic_distance_plot, width = 30, height = 15)

# Write meean_dist to a new CSV file
#write.csv2(meean_dist, file = "/mnt/EAS_ind/aeiberle/data/dyadic_distance_individuals.csv", row.names = FALSE)

 #### Normalize mean_dyadic_distance  ####
 # date <-"2023-06-15"
 # date <-"2023-07-01"
 # date <-"2023-06-24"
 # date <-"2023-07-16"
 # date <-"2023-07-20"
 # date <-"2023-07-30"
 # date <- "2023-06-21"
 # date <- "2023-07-22"
 
 # rescale the dyadic distance to individual data between 0 and 1
 audio_gps_distances_dyadic_GD_2 <- audio_gps_distances_dyadic %>%
   group_by(date.x)%>%
   mutate(Z_dist_individual = rescale(mean_dist_individual, to=c(0,1)))
 
 #get PB time for the playback
 PB_time <- playback_details[which(playback_details$Date == date), "t0"]
 PB_time <- as.POSIXct(paste(date, PB_time), tz = "UTC")
 
 #calculate time relative to PB
 audio_gps_distances_dyadic_GD_2$re_numeric <- audio_gps_distances_dyadic_GD_2$numeric - as.numeric(PB_time)
 
 
 #trim everything to 10 min interval
 audio_gps_distances_dyadic_GD_2 <- subset(audio_gps_distances_dyadic_GD_2, abs(re_numeric) < 601)
 
 
 #plot stuff
 GD_2 <- ggplot(data = audio_gps_distances_dyadic_GD_2, aes(x = re_numeric, y = Z_dist_individual)) +
   geom_smooth(method = "gam") +
   geom_vline(xintercept = 0, linetype="dotted", color = "red", size = 1.5) +
   geom_vline(xintercept = c(-60, 60), linetype="dotted", color = "grey", size = 1) +
   ggtitle("test normalized distances dyadic") +
   theme_minimal()+
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
         legend.position = "right")
 
 print(SI_1)
 print(SI_2)
 
 print(BS_1)
 print(BS_2)
 
 print(ZU_1)
 print(ZU_2)
 
 print(GD_1)
 print(GD_2)
 
 #library(gridExtra)
 normalized_dyadic_distance <- grid.arrange(SI_1, SI_2, BS_1, BS_2, ZU_1, ZU_2, GD_1, GD_2, ncol = 2, nrow = 4)
 
 #Save the plot
 #ggsave(filename = paste0(output_dir, "dyadic_distance_norm_plot_grid.png"), plot = normalized_dyadic_distance, width = 30, height = 15)
 
 
 
 trials_df <- list(audio_gps_distances_dyadic_SI_1, audio_gps_distances_dyadic_SI_2, 
                   audio_gps_distances_dyadic_BS_1, audio_gps_distances_dyadic_BS_2, 
                   audio_gps_distances_dyadic_ZU_1, audio_gps_distances_dyadic_ZU_2, 
                   audio_gps_distances_dyadic_GD_1, audio_gps_distances_dyadic_GD_2)
 
 
 # Initial big data frame
 big_df <- audio_gps_distances_dyadic
 
 # Add empty columns "Z_dist_individual" and "re_numeric" to big_df
 big_df$Z_dist_individual <- NA
 big_df$re_numeric <- NA
 
 # Loop over each small data frame and merge specific columns
 for (i in seq_along(trials_df)) {
   trial_df <- trials_df[[i]]
   
   # Select columns and merge by "timestamps"
   new_df <- trial_df[, c("timestamps", "Z_dist_individual", "re_numeric")]
   
   # Add a unique suffix based on the loop index
   suffix <- paste0("_", i)
   
   # Merge into big_df without overwriting (using suffixes)
   big_df <- merge(big_df, new_df, by = "timestamps", all.x = TRUE, suffixes = c("", suffix))
   
   # Update the "Z_dist_individual" and "re_numeric" columns with the data from the merged dataframe
   # We use the merged column with the suffix (e.g., Z_dist_individual_1, re_numeric_1)
   big_df$Z_dist_individual <- ifelse(is.na(big_df$Z_dist_individual), 
                                      big_df[, paste0("Z_dist_individual", suffix)], 
                                      big_df$Z_dist_individual)
   big_df$re_numeric <- ifelse(is.na(big_df$re_numeric), 
                               big_df[, paste0("re_numeric", suffix)], 
                               big_df$re_numeric)
   
   # Optionally, clean up the temporary columns created during the merge (Z_dist_individual_1, etc.)
   big_df[, paste0("Z_dist_individual", suffix)] <- NULL
   big_df[, paste0("re_numeric", suffix)] <- NULL
 }
 
 audio_gps_distances_dyadic_NEW <- big_df
 
 
 complete_audio_gps_distances_dyadic <- ggplot(data = audio_gps_distances_dyadic_NEW, aes(x = re_numeric, y = Z_dist_individual)) + 
   geom_smooth(method = "gam") + 
   geom_point() +
   geom_vline(xintercept = 0, linetype="dotted", color = "red", size = 1.5) + 
   geom_vline(xintercept = c(-60, 60), linetype="dotted", color = "grey", size = 1) + 
   ggtitle("all normalized distances dyadic") + 
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
         legend.position = "right")
 
 print(complete_audio_gps_distances_dyadic)
 
 #Save the plot
 #ggsave(filename = paste0(output_dir, "dyadic_distance_normalized_all_plot.png"), plot = complete_audio_gps_distances_dyadic, width = 12, height = 6)
 
 
 #### Signal Propagation plot inter-individual distance ####
 # Plotting any first call of an individual after the start of the PB in relation to distance inter-individual --> df = audio_gps_distances_speaker
 
 # Step 1: Trim the data to make re_numeric start from 0
 audio_gps_distances_speaker_NEW <- audio_gps_distances_speaker %>%
   group_by(ID) %>%
   mutate(re_numeric = re_numeric - min(re_numeric)) %>%  # Shift re_numeric so it starts at 0
   ungroup()
 
 audio_gps_distances_speaker_NEW <- subset(audio_gps_distances_speaker_NEW, re_numeric > 0)
 
 # Step 2: Filter for the first row per timestamp and ID
 audio_gps_distances_speaker_NEW_filtered <- audio_gps_distances_speaker_NEW %>%
   group_by(ID, dattime) %>%
   slice_head(n = 1) %>%  # Keep the first row for each group (timestamp + ID)
   ungroup()
 
 # Step 3: Filter data for re_numeric between 0 and 600
 audio_gps_distances_speaker_NEW_filtered_window <- audio_gps_distances_speaker_NEW_filtered %>%
   filter(re_numeric >= 0 & re_numeric <= 120)
 
 # Step 4: Plot the data
 signals_speaker_after_PB <- ggplot(audio_gps_distances_speaker_NEW_filtered_window, aes(x = re_numeric, y = Z_dist)) +
   geom_point() +
   geom_smooth(method = "gam") + 
   labs(title = "Calls after Playback",
        x = "re_numeric", 
        y = "Z_dist_speaker") +
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
         legend.position = "right")
 
 print(signals_after_PB)
 
 
 # removing close calls
 audio_gps_distances_speaker_NEW_filtered_window_no_cc <- subset(audio_gps_distances_speaker_NEW_filtered_window, label_name != "cc")
 
 signals_speaker_after_PB_no_cc <- ggplot(audio_gps_distances_speaker_NEW_filtered_window_no_cc, aes(x = re_numeric, y = Z_dist)) +
   geom_point() +
   geom_smooth(method = "gam") + 
   labs(title = "Calls after Playback no cc",
        x = "re_numeric", 
        y = "Z_dist_speak") +
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
         legend.position = "right")
 
 print(signals_after_PB_no_cc)
 
 #Save the plot
 #ggsave(filename = paste0(output_dir, "signal_propagation_speaker_after_PB_plot.png"), plot = signals_speaker_after_PB_no_cc, width = 12, height = 6)
 

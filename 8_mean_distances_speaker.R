#### Displaying plots for mean distances to speaker for a given time window ####
### using all available GPS data

library(sp)
library(geosphere)
library(dplyr)
library(sf)
library(ggplot2)
library("scales")
library(gganimate)


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
root_dir <- "/mnt/EAS_shared/meerkat/working/newdata/TO_ARCHIVE/"
alt_root_dir <- "/mnt/EAS_ind/aeiberle/data/"
output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/"

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


specific_filenames_20240703 <- c("L_VLF272_GPS_20240703_GARMIN.txt",
                                 "L_VLF297_GPS_20240703_GARMIN.txt",
                                 "L_VGDM015_LR_LS_Axy_031_20240703-20240704_Axy031_S1.txt",
                                 "L_VLF283_LS_RT_Axy_029_20240703-20240707_Axy029_S1.txt",
                                 "L_VLF287_T_MB_Axy_019_20240703-20240707_Axy019_S1.txt",
                                 "L_VLF293_RS_RR_Axy_020_20240703_20240707_Axy020_S1.txt",
                                 "L_VLF294_RS_LR_Axy_010_20240703-20240707_Axy010_S1.txt",
                                 "L_VLF307_RS_SH_Axy_035_20240703-20240703_S1.txt",
                                 "L_VLM298_SH_RT_Axy_022_20240703-20240707_S1.txt",
                                 "L_VLM300_SH_RR_Axy_028_20240703-20240707_S1.txt",
                                 "L_VLM301_RT_TB_Axy_004_20240703-20240707_Axy004_S1.txt",
                                 "L_VLM302_RT_H_Axy_016_20240703-20240707_S1.txt",
                                 "L_VLM303_RT_LT_Axy_011_20240703-20240707_S1.txt",
                                 "L_VUBM050_RR_TB_Axy_006_20240703-20240707_S1.txt")

specific_filenames_20240707 <- c("L_VGDM015_GPS_20240707_GARMIN.txt",
                                 "L_VLF272_GPS_20240707_Axy026_S7.txt",
                                 "L_VLF297_GPS_20240707_Axy034_S7.txt",
                                 "L_VUBM050_GPS_20240707_GARMIN.txt",
                                 "L_VLF283_LS_RT_Axy_029_20240703-20240707_Axy029_S1.txt",
                                 "L_VLF287_T_MB_Axy_019_20240703-20240707_Axy019_S1.txt",
                                 "L_VLF293_RS_RR_Axy_020_20240703_20240707_Axy020_S1.txt",
                                 "L_VLF294_RS_LR_Axy_010_20240703-20240707_Axy010_S1.txt",
                                 "L_VLF307_RS_SH_Axy_007_20240706-20240707_S1.txt",
                                 "L_VLF308_RS_MB_Axy_038_20240705-20240707_Axy038_S1.txt",
                                 "L_VLM298_SH_RT_Axy_022_20240703-20240707_S1.txt",
                                 "L_VLM300_SH_RR_Axy_028_20240703-20240707_S1.txt",
                                 "L_VLM301_RT_TB_Axy_004_20240703-20240707_Axy004_S1.txt",
                                 "L_VLM302_RT_H_Axy_016_20240703-20240707_S1.txt",
                                 "L_VLM303_RT_LT_Axy_011_20240703-20240707_S1.txt",
                                 "L_VUBM050_RR_TB_Axy_006_20240703-20240707_S1.txt")

specific_filenames_20240803 <- c("L_VLF272_GPS_20240803_Axy006_S1.txt",
                                 "L_VLF293_RS_RR_Axy_020_20240803-20240807_Axy020_S1.txt",
                                 "L_VLF294_RS_LR_Axy_035_20240802-20240807_Axy035_S1.txt",
                                 "L_VLF303_RT_LT_Axy_011_20240803-20240807_Axy011_S1.txt",
                                 "L_VLF307_RS_SH_Axy_032_20240803-20240807_Axy032_S1.txt",
                                 "L_VLF308_RS_MB_Axy_028_20240803-20240807_Axy028_S5.txt",
                                 "L_VLM287_T_MB_034_20240803-20240807_Axy034_S1.txt",
                                 "L_VLM298_SH_RT_Axy_036_20240803-20240807_Axy036_S7.txt",
                                 "L_VLM300_SH_RR_Axy003_20240803-20240807_Axy003_S1.txt",
                                 "L_VLM301_RT_TB_Axy019_20240803-20240807_Axy019_S1.txt",
                                 "L_VLM302_RT_H_Axy_007_20240803-20240807_Axy007_S1.txt",
                                 "L_VLM310_RS_LS_Axy_016_20240803-20240807_Axy016_S1.txt",
                                 "L_VUBM050_RR_TB_Axy_038_20240803-20240807_Axy038_S1.txt")

specific_filenames_20240806 <- c("L_VLF272_GPS_20240806_Axy006_S1.txt",
                                 "L_VLF283_LS_RT_Axy_022_20240805-20240807_Axy022_S1.txt",
                                 "L_VGDM015_LR_LS_Axy_026_20240804-20240807_Axy026_S1.txt",
                                 "L_VLF293_RS_RR_Axy_020_20240803-20240807_Axy020_S1.txt",
                                 "L_VLF294_RS_LR_Axy_035_20240802-20240807_Axy035_S1.txt",
                                 "L_VLF303_RT_LT_Axy_011_20240803-20240807_Axy011_S1.txt",
                                 "L_VLF307_RS_SH_Axy_032_20240803-20240807_Axy032_S1.txt",
                                 "L_VLF308_RS_MB_Axy_028_20240803-20240807_Axy028_S5.txt",
                                 "L_VLM287_T_MB_034_20240803-20240807_Axy034_S1.txt",
                                 "L_VLM298_SH_RT_Axy_036_20240803-20240807_Axy036_S7.txt",
                                 "L_VLM300_SH_RR_Axy003_20240803-20240807_Axy003_S1.txt",
                                 "L_VLM301_RT_TB_Axy019_20240803-20240807_Axy019_S1.txt",
                                 "L_VLM302_RT_H_Axy_007_20240803-20240807_Axy007_S1.txt",
                                 "L_VLM310_RS_LS_Axy_016_20240803-20240807_Axy016_S1.txt",
                                 "L_VUBM050_RR_TB_Axy_038_20240803-20240807_Axy038_S1.txt")

specific_filenames_20240717 <- c("AL_VALF008_GPS_20240717_Axy036_S1.txt",
                                 "AL_VALM010_GPS_20240717_Axy035_S1.txt",
                                 "AL_VECM023_GPS_202040717_Axy007_S1.txt",
                                 "AL_VALM011_RS_H_Axy_031_20240717-20240721_S1.txt",
                                 "AL_VALM014_LS_MB_Axy_004_20240717-20240721_S2.txt",
                                 "AL_VALM016_SH_RT_Axy_034_20240717-20240721_S1.txt",
                                 "AL_VALM018_SH_RS_Axy_016_20240717-20240721_S1.txt",
                                 "AL_VALM019_SH_LR_Axy_026_20240717-20240721_S1.txt",
                                 "AL_VLM211_RS_RT_Axy_029_20240717-20240721_S1.txt")

specific_filenames_20240721 <- c("AL_VALF008_GPS_20240721_Axy007_S4.txt",
                                 "AL_VALM011_RS_H_Axy_031_20240717-20240721_S1.txt",
                                 "AL_VALM014_LS_MB_Axy_004_20240717-20240721_S2.txt",
                                 "AL_VALM016_SH_RT_Axy_034_20240717-20240721_S1.txt",
                                 "AL_VALM018_SH_RS_Axy_016_20240717-20240721_S1.txt",
                                 "AL_VALM019_SH_LR_Axy_026_20240717-20240721_S1.txt",
                                 "AL_VLM211_RS_RT_Axy_029_20240717-20240721_S1.txt",
                                 "AL_VECM023_LR_LS_Axy_006_20240719-20240721_S1.txt",
                                 "AL_VALM010_RS_MB_Axy_020_20240720-20240721_S1.txt")

specific_filenames_20240727 <- c("GD_VGDF007_GPS_20240727_Axy007_S5.txt",
                                 "GD_VUBM018_GPS_20240727_Axy036_S4.txt",
                                 "GD_VGDF042_SH_Axy_004_20240727-20240730_S1.txt",
                                 "GD_VGDF047_HT_Axy_006_20240727-20240730_S1.txt",
                                 "GD_VGDF054_MB_RT_Axy_030_20240727-20240730_S1.txt",
                                 "GD_VGDM048_H_LT_Axy_020_20240727-20240730_S1.txt")

specific_filenames_20240730 <- c("GD_VGDF007_GPS_20240730.txt",
                                 "GD_VGDF039_GPS_20240730.txt",
                                 "GD_VGDF042_SH_Axy_004_20240727-20240730_S1.txt",
                                 "GD_VGDF047_HT_Axy_006_20240727-20240730_S1.txt",
                                 "GD_VGDF054_MB_RT_Axy_030_20240727-20240730_S4.txt",
                                 "GD_VGDM048_H_LT_Axy_020_20240727-20240730_S1.txt",
                                 "GD_VGDM055_MB_RR_Axy_31_20240730-20240730_S1.txt")



# Combine All Specific Filenames into a List
date_specific_files <- list(
  "2023-06-15" = specific_filenames_20230615,
  "2023-07-01" = specific_filenames_20230701,
  "2023-06-24" = specific_filenames_20230624,
  "2023-07-16" = specific_filenames_20230716,
  "2023-07-22" = specific_filenames_20230722,
  "2023-06-21" = specific_filenames_20230621,
  "2023-07-20" = specific_filenames_20230720,
  "2023-07-30" = specific_filenames_20230730,
  "2024-07-03" = specific_filenames_20240703,
  "2024-07-07" = specific_filenames_20240707,
  "2024-08-03" = specific_filenames_20240803,
  "2024-08-06" = specific_filenames_20240806,
  "2024-07-17" = specific_filenames_20240717,
  "2024-07-21" = specific_filenames_20240721,
  "2024-07-27" = specific_filenames_20240727,
  "2024-07-30" = specific_filenames_20240730)


# Load CSV containing start and end times for each date
time_window <- read.csv("/mnt/EAS_ind/aeiberle/data/metaData/time_startend_info-copy.csv", sep = ",")

#load playback metadata
playback_details <- read.csv("/mnt/EAS_ind/aeiberle/data/metaData/Copy of PB_time_startend_info-copy.csv", sep = ",")

# Assuming the CSV has columns "Date", "t0" (start time), and "t1" (end time)
# Convert the Date column to POSIXlt to match the date format
time_window$Date <- as.POSIXlt(time_window$Date, format = "%Y-%m-%d", tz = "UTC")

#make a data frame for combined data
aggregated_gps_data_all <- data.frame()
all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE, pattern = ".txt")
#date <- "2023-07-01"
# Loop through each date
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and its subfolders and check if the date is 2023-06-15 to use the alternate root directory
  

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
 
  
   #### Create animation of the deployment #####
 
##   sf_obj <- st_as_sf(aggregated_gps_data, coords = c("lon", "lat"), crs = 4326, agr = "constant")
##   
##   ggplot() +
##     geom_sf(data = sf_obj, aes(color = ID), size = 2) +
##     theme_bw()  
##   
##   # Coordinates for the speaker location
##   speaker <- data.frame(lon = lon, lat = lat)
##   
##   p <- ggplot() +
##     geom_sf(data = sf_obj, aes(color = ID), size = 3) +
##     geom_point(data = speaker, aes(x = lon, y = lat), shape=13, color = "red", size = 6) +
##     theme_bw() + ggtitle(group)+
##   transition_time(re_numeric) +
##     labs(title = "Time: {frame_time}") +
##     shadow_wake(wake_length = 0.1,  alpha = 0.5)  
##   
##   b <- animate(p, nframes = 100, fps = 5)
##   
##  anim_save(paste("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/plots/", trial, ".mp4", sep = ""), b)
  
  #combine all in one big dataframe
  aggregated_gps_data_all <- rbind(aggregated_gps_data_all, aggregated_gps_data)
  #trim everything to 10 min interval
  aggregated_gps_data_all <- subset(aggregated_gps_data_all, abs(re_numeric) < 601)
  
  
  #plot the deployment. Change in distances to the speaker over time
  print(ggplot(data = aggregated_gps_data, aes(x = re_numeric, y = dist, colour = ID)) + geom_smooth() + 
          geom_vline(xintercept = 0, linetype="dotted", 
                     color = "red", size=1.5) +  
          labs(title = date,
               x = "Time Difference to Playback Start [s]",
               y = "Distance") +
          theme_minimal() +
          theme(plot.background = element_rect(fill = "white", color = NA),
                panel.background = element_rect(fill = "white", color = NA),
                panel.grid.major = element_line(color = "grey90"),
                panel.grid.minor = element_line(color = "grey80"),
                plot.title = element_text(size = 16),
                axis.title.x = element_text(size = 16),
                axis.title.y = element_text(size = 16),
                axis.text.x = element_text(size = 14),
                axis.text.y = element_text(size = 14),
                legend.title = element_text(size = 16),
                legend.text = element_text(size = 14),
                legend.position = "right")
        )
}
    

  
 # rescale the distance to speaker data between 0 and 1 
aggregated_gps_data_all <- aggregated_gps_data_all %>%
  group_by(trial)%>% 
  mutate(Z_dist = rescale(dist, to=c(0,1)))

# rename treatment
aggregated_gps_data_all <- aggregated_gps_data_all %>%
  mutate(treatment = recode(treatment, "audio_poop" = "audio_feces"))

#write.csv(aggregated_gps_data_all, file = "/mnt/EAS_ind/aeiberle/data/distance_to_speaker.csv", row.names = FALSE)


output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/NEW/"

#plot mean distance to speaker across all trials
dist_all_plot <- ggplot(data = aggregated_gps_data_all, aes(x = re_numeric, y = Z_dist)) + geom_smooth(method = "gam") + 
  geom_vline(xintercept = 0, linetype = "dotted", color = "red", size = 1.5) + 
  geom_vline(xintercept = c(-60, 60), linetype = "dotted", color = "grey", size = 1) + 
  labs(title = "Change in Distances to the Speaker all Trials combined",
       x = "Time Difference to Playback Start [s]",
       y = "Normalized Distance") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "right")

print(dist_all_plot)
# ggsave(filename = paste0(output_dir, "distance_norm_all_plot.png"), plot = dist_all_plot, width = 18, height = 9)

#plot mean distance to speaker by treatment across all trials
dist_treatment_plot <- ggplot(data = aggregated_gps_data_all, aes(x = re_numeric, y = Z_dist, colour = treatment)) + geom_smooth() + 
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size = 1.5) + 
  geom_vline(xintercept = c(-60, 60), linetype="dotted", color = "grey", size = 1) + 
  labs(title = "Change in Distances to the Speaker by Treatment",
       x = "Time Difference to Playback Start [s]",
       y = "Normalized Distance") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "right")

print(dist_treatment_plot)
# ggsave(filename = paste0(output_dir, "distance_norm_treatment_plot.png"), plot = dist_treatment_plot, width = 18, height = 9)

#plot mean distance to speaker by group
dist_group_plot <- ggplot(data = aggregated_gps_data_all, aes(x = re_numeric, y = Z_dist, colour = group)) + geom_smooth() + 
  geom_vline(xintercept = 0, linetype="dotted", color = "black", size = 1.5) + 
  geom_vline(xintercept = c(-60, 60), linetype="dotted", color = "grey", size = 1) + 
  labs(title = "Change in Distances to the Speaker by Group",
       x = "Time Difference to Playback Start [s]",
       y = "Normalized Distance") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 14),
        legend.position = "right")

print(dist_group_plot)
# ggsave(filename = paste0(output_dir, "distance_norm_group_plot.png"), plot = dist_group_plot, width = 18, height = 9)



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
             color = "black", size = 1.5) + geom_vline(xintercept = c(50, 70), linetype="dotted", 
                                                     color = "grey", size = 1) + 
  ggtitle("all") + theme_minimal()


#### convert to X and Y matrix #####
library(cocomo)
# save new dataframe for renaming
aggregated_gps_data_all_test <- aggregated_gps_data_all
#rename column names
colnames(aggregated_gps_data_all_test) <- c("timestamp",  "location.lat", "location.long", "individual.local.identifier", "dist", "Z_dist", "numeric",  "re_numeric", "group","treatment",  "trial")

#format the data chunks data_frame
data_chunks <- time_window
data_chunks$start <- paste(data_chunks$Date, data_chunks$t0)
data_chunks$end <- paste(data_chunks$Date, data_chunks$t1)
data_chunks <- data_chunks[, c(5,6)]
data_chunks$start <- as.POSIXct(data_chunks$start, tz =  "UTC")
data_chunks$end <- as.POSIXct(data_chunks$end, tz = "UTC")

##    #make Xs and Ys matrices
   reformat_movebank_to_matrix(
      aggregated_gps_data_all_test,
      output_file_path = "/mnt/EAS_ind/aeiberle/data/RDataFiles/GPS_matrices.RData",
      ids = NULL,
      data_chunks = data_chunks,
      seconds_per_time_step = 1,
      start_date = NULL,
      end_date = NULL,
      start_time = NULL,
      end_time = NULL,
      utm_zone = 34,
      hemisphere = "south",
      output_utm = T,
      output_latlon = T,
      use_UTC = T,
      local_timezone = NULL
    )

##    
#load matrices
load("/mnt/EAS_ind/aeiberle/data/RDataFiles/GPS_matrices.RData")

#calculate dyadic distances
distances <- get_group_dyadic_distances(xs, ys)
#get means
mean_dist <- apply(distances, 3, mean, na.rm = T )

#add date-times to the mean dyadic distances
meean_dist <- cbind(data.frame(timestamps), mean_dist)
meean_dist$date <- stringr::str_sub(meean_dist$timestamps, 1, 10)
meean_dist$numeric <- as.numeric(meean_dist$timestamps)

#remove NAs
meean_dist <- meean_dist[which(complete.cases(meean_dist)), ]

#plot
ggplot(data = meean_dist, aes(x= numeric, y = mean_dist))  + geom_smooth(method = "gam")+ geom_jitter()+
  facet_wrap(~date, scales = "free")

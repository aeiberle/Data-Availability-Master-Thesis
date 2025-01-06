#### LOOP for displaying kernel densities plot for a given time window ####
### using all available GPS data

library(sp)
#install.packages("rgdal")
#library(rgdal)
library(dplyr)
library(sf)
library(ggplot2)


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
specific_filenames_20230615 <- c("SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46_NEW.txt",
                                 #"SI_VECM009_SH_H_Axy_030_20230613-20230615_S24_NEW.txt",
                                 "SI_VZUF048_RC_RS_TB_GX011333_HERO9 Black-GPS5.txt",
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

# Combine All Specific Filenames into a List
date_specific_files <- list(
  "2023-06-15" = specific_filenames_20230615,
  "2023-06-24" = specific_filenames_20230624,
  "2023-07-16" = specific_filenames_20230716
)


# Load CSV containing start and end times for each date
time_window <- read.csv(paste0(alt_root_dir,"time_startend_info.csv"), stringsAsFactors = FALSE, sep = ";")

# Assuming the CSV has columns "Date", "t0" (start time), and "t1" (end time)
# Convert the Date column to POSIXlt to match the date format
time_window$Date <- as.POSIXlt(time_window$Date, format = "%Y-%m-%d", tz = "UTC")


#### Kernel Density Plot for each Individual ####
#install.packages("adehabitatHR")
library(adehabitatHR)
#install.packages("kdensity")
library(kdensity)

# Loop through each date
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and its subfolders and check if the date is 2023-06-15 to use the alternate root directory
  if (date == "2023-06-15") {
    all_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
  } else {
    all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  }
  
  
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
  
  # Initialize a list to store kernel density results for each individual
  kernel_density_list <- list()
  
  # Loop through each file found
  for (file_GPS in file_list) {
    # Load the GPS file
    gps <- read.delim(file = file_GPS, header = FALSE)
    
    # Prepare the data
    gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
    gps <- gps[, -2]
    colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
    
    gps$V2 <- as.numeric(as.character(gps$V2))
    gps$V3 <- as.numeric(as.character(gps$V3))
    
    gps <- gps[complete.cases(gps), ]
    
    # Convert the V1 column to POSIXlt
    gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
                                                "%Y/%m/%d %H:%M:%OS",
                                                "%Y-%m-%d %H:%M",
                                                "%Y/%m/%d %H:%M",
                                                "%Y-%m-%d",
                                                "%Y/%m/%d"), tz = "UTC")
    
    gps <- gps[gps$V1 > start_time & gps$V1 < end_time, ]
    
    # Convert to SpatialPointsDataFrame and transform projection
    gps.spdf <- SpatialPointsDataFrame(coords = cbind(gps$V2, gps$V3), 
                                       data = gps, 
                                       proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
    
    gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
    
    # Add id_code as a new column to gps.spdf
    id_code <- sub("^[^_]+_([^_]+).*", "\\1", basename(file_GPS))
    gps.spdf$id_code <- id_code
    
    if (nrow(gps.spdf@data) > 0) {
      
      # Calculate Kernel Density Estimate (KDE)
      kde <- kernelUD(gps.spdf, h = "href")  # 'href' for smoothing parameter
      
      # Store KDE in list for individual plot creation later
      kernel_density_list[[basename(file_GPS)]] <- kde
      
      # Calculate 50% and 80% kernel density areas (e.g., core area and home range)
      kde_50_individual <- getverticeshr(kde, percent = 50)
      kde_80_individual <- getverticeshr(kde, percent = 80)
      
      # Plot the KDE with ggplot2
      kde_50_individual_sf <- st_as_sf(kde_50_individual)
      kde_80_individual_sf <- st_as_sf(kde_80_individual)
      
      # Generate KDE plot for the current file
      kde_plot <- ggplot() +
        geom_sf(data = kde_80_individual_sf, fill = "blue", alpha = 0.3, color = NA, inherit.aes = FALSE) +
        geom_sf(data = kde_50_individual_sf, fill = "red", alpha = 0.3, color = NA, inherit.aes = FALSE) +
        labs(title = paste("Kernel Density Plot for", basename(id_code), "on", date),
             x = "Longitude (UTM Zone 34S)",
             y = "Latitude (UTM Zone 34S)") +
        theme_minimal()+
        theme(plot.background = element_rect(fill = "white", color = NA),
              panel.background = element_rect(fill = "white", color = NA),
              panel.grid.major = element_line(color = "gray80"),
              panel.grid.minor = element_line(color = "gray90"),
              plot.title = element_text(size = 16),
              axis.title.x = element_text(size = 14),
              axis.title.y = element_text(size = 14),
              axis.text.x = element_text(size = 10),
              axis.text.y = element_text(size = 10),
              legend.title = element_text(size = 16),
              legend.text = element_text(size = 14),
              legend.position = "right")
      
      # Print the KDE plot
      print(kde_plot)
      
      # Save the KDE plot for each file if desired
      #ggsave(filename = paste0(output_dir, gsub("-", "", date), "_KDE_plot_", sub(".txt", "", basename(id_code)), ".png"), plot = kde_plot, width = 11, height = 7)
    }
  }
}



#### Kernel Density Plot for each date ####
# Loop through each date
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and its subfolders and check if the date is 2023-06-15 to use the alternate root directory
  if (date == "2023-06-15") {
    all_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
  } else {
    all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  }
  
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
    # Load the GPS file
    gps <- read.delim(file = file_GPS, header = FALSE)
    
    # Prepare the data
    gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
    gps <- gps[, -2]
    colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
    
    gps$V2 <- as.numeric(as.character(gps$V2))
    gps$V3 <- as.numeric(as.character(gps$V3))
    
    gps <- gps[complete.cases(gps), ]
    
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
  
  # Proceed if there is aggregated data
  if (nrow(aggregated_gps_data) > 0) {
    # Convert to SpatialPointsDataFrame and transform projection
    gps.spdf <- SpatialPointsDataFrame(coords = cbind(aggregated_gps_data$V2, aggregated_gps_data$V3), 
                                       data = aggregated_gps_data, 
                                       proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
    
    gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
    
    # Calculate Kernel Density Estimate (KDE)
    kde <- kernelUD(gps.spdf, h = "href")  # 'href' for smoothing parameter
    
    # Calculate 50% and 80% kernel density areas (e.g., core area and home range)
    kde_50_combined <- getverticeshr(kde, percent = 50)
    kde_80_combined <- getverticeshr(kde, percent = 80)
    
    # Plot the KDE with ggplot2
    kde_50_combined_sf <- st_as_sf(kde_50_combined)
    kde_80_combined_sf <- st_as_sf(kde_80_combined)
    
    # Generate KDE plot for the current date
    kde_plot_combined <- ggplot() +
      geom_sf(data = kde_80_combined_sf, fill = "blue", alpha = 0.3, color = NA, inherit.aes = FALSE) +
      geom_sf(data = kde_50_combined_sf, fill = "red", alpha = 0.3, color = NA, inherit.aes = FALSE) +
      labs(title = paste("Kernel Density Plot for All Individuals on", date),
           x = "Longitude (UTM Zone 34S)",
           y = "Latitude (UTM Zone 34S)") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90"),
            plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 10),
            axis.text.y = element_text(size = 10),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            legend.position = "right")
    
    # Print the KDE plot
    print(kde_plot_combined)
    
    # Save the KDE plot for the date
    #ggsave(filename = paste0(output_dir, gsub("-", "", date), "_KDE_plot_combined.png"), plot = kde_plot_combined, width = 11, height = 7)
  }
}


####
# ## Calculate area
# st_area(kde_50_combined_sf)
# st_area(kde_80_combined_sf)


#### Kernel Density Area Plot with bins for each date ####
# Load necessary packages
library(lubridate)

# Load the playback times CSV (replace 'path_to_playback.csv' with the actual file path)
playback_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/PB_time_startend_info.csv", header = TRUE, sep = ";")

# Combine Date and time (t0, t1) into proper POSIXct format
playback_df <- playback_df %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
         t0 = as.POSIXct(paste(Date, t0), format = "%Y-%m-%d %H:%M:%S"),
         t1 = as.POSIXct(paste(Date, t1), format = "%Y-%m-%d %H:%M:%S"))
playback_df$t0seconds <- as.numeric(playback_df$t0)
playback_df$t1seconds <- as.numeric(playback_df$t1)

# Merge the audio data with the playback times based on the 'date' column
audio_day <- audio_df %>%
  left_join(playback_df, by = c("date" = "Date"))

audio_day$t0_timediff <- audio_day$t0_UTCseconds - audio_day$t0seconds
audio_day$tf_timediff <- audio_day$tf_UTCseconds - audio_day$t1seconds


# Loop through each date
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and its subfolders and check if the date is 2023-06-15 to use the alternate root directory
  if (date == "2023-06-15") {
    all_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
  } else {
    all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  }
  
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
  
  # Create "x"-minute intervals/bins
  time_bins <- seq(from = start_time, to = end_time, by = 1.5 * 60)
  
  # Data frame to store areas
  area_results <- data.frame(Time_Bin = time_bins[-length(time_bins)], Area_80 = numeric(length(time_bins) - 1))
  
  for (i in 1:(length(time_bins) - 1)) {
    start_bin <- time_bins[i]
    end_bin <- time_bins[i + 1]
    
    # Initialize an empty data frame to store aggregated GPS data for the current time bin
    aggregated_gps_data <- data.frame(V1 = as.POSIXlt(character()), V2 = numeric(), V3 = numeric(), stringsAsFactors = FALSE)
    
    # Loop through each file and filter GPS points within the current time bin
    for (file_GPS in file_list) {
      gps <- read.delim(file = file_GPS, header = FALSE)
      
      gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
      gps <- gps[, -2]
      colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
      
      gps$V2 <- as.numeric(as.character(gps$V2))
      gps$V3 <- as.numeric(as.character(gps$V3))
      gps <- gps[complete.cases(gps), ]
      
      gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
                                                  "%Y/%m/%d %H:%M:%OS",
                                                  "%Y-%m-%d %H:%M",
                                                  "%Y/%m/%d %H:%M",
                                                  "%Y-%m-%d",
                                                  "%Y/%m/%d"), tz = "UTC")
      
      gps <- gps[gps$V1 >= start_bin & gps$V1 < end_bin, ]
      aggregated_gps_data <- rbind(aggregated_gps_data, gps)
    }
    
    if (nrow(aggregated_gps_data) > 0) {
      gps.spdf <- SpatialPointsDataFrame(coords = cbind(aggregated_gps_data$V2, aggregated_gps_data$V3),
                                         data = aggregated_gps_data,
                                         proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
      
      gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
      
      kde <- kernelUD(gps.spdf, h = "href")
      kde_80_combined <- getverticeshr(kde, percent = 80)
      kde_80_combined_sf <- st_as_sf(kde_80_combined)
      
      if (!is.null(kde_80_combined_sf)) {
        area_results$Area_80[i] <- st_area(kde_80_combined_sf)
      }
    }
  }

  # Plot the area over time
  kde_area_combined_plot <- ggplot(area_results, aes(x = Time_Bin, y = as.numeric(Area_80))) +
    geom_line(color = "turquoise", linewidth = 2) +
    labs(title = paste("Change in 80% KDE Area Over Time on", date),
         x = "Time",
         y = "Area (m^2)") +
    # geom_vline(aes(xintercept = as.numeric(t0)), linetype = "dashed", color = "plum", linewidth = 0.5) +  # Playback start
    # geom_vline(aes(xintercept = as.numeric(t1)), linetype = "dashed", color = "plum", linewidth = 0.5) +  # Playback end
    scale_x_datetime(date_breaks = "2 min", date_labels = "%H:%M") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_line(color = "gray90"),
          plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 10),
          axis.text.y = element_text(size = 10),
          legend.title = element_text(size = 16),
          legend.text = element_text(size = 14),
          legend.position = "right")

  # Print the KDE plot
  print(kde_area_combined_plot)
  
  # Save the KDE plot for the date
  #ggsave(filename = paste0(output_dir, gsub("-", "", date), "_KDE_Area_plot_combined.png"), plot = kde_area_combined_plot, width = 11, height = 7)
}


#### RANDOM STUFF WE DON'T WANT TO RUN ####
# # Loop Through Each Date and its Specific Filenames
# for (date in names(date_specific_files)) {
#   specific_filenames <- date_specific_files[[date]]
#   
#   # Get all files in the root directory and its subfolders and check if the date is 2023-06-15 to use the alternate root directory
#   if (date == "2023-06-15") {
#     all_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
#   } else {
#     all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
#   }
#   
#  
#   # Filter files based on specific filenames for the current date
#   file_list <- all_files[basename(all_files) %in% specific_filenames]
#   
#   all_files[which(basename(all_files) %in% specific_filenames)]
# 
#   # Find the row in the CSV that matches the current date
#   time_window_row <- time_window[time_window$Date == as.POSIXlt(date, format = "%Y-%m-%d", tz = "UTC"), ]
#   
#   # Ensure the matching row exists
#   if (nrow(time_window_row) == 1) {
#     start_time <- as.POSIXlt(paste(date, time_window_row$t0), tz = "UTC")
#     end_time <- as.POSIXlt(paste(date, time_window_row$t1), tz = "UTC")
#   } else {
#     warning(paste("No matching time window found for date:", date))
#     next  # Skip to the next iteration if no matching time window is found
#   }
#   
#   # Loop through each file found
#   for (file_GPS in file_list) {
#     # Load the GPS file
#     gps <- read.delim(file = file_GPS, header = FALSE)
#     
#     # Prepare the data
#     gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
#     gps <- gps[, -2]
#     colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
#     
#     gps$V2 <- as.numeric(as.character(gps$V2))
#     gps$V3 <- as.numeric(as.character(gps$V3))
#     
#     gps <- gps[complete.cases(gps), ]
#     
#     # Convert the V1 column to POSIXlt
#     gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
#                                                 "%Y/%m/%d %H:%M:%OS",
#                                                 "%Y-%m-%d %H:%M",
#                                                 "%Y/%m/%d %H:%M",
#                                                 "%Y-%m-%d",
#                                                 "%Y/%m/%d"), tz = "UTC")
#     
#     gps <- gps[gps$V1 > start_time & gps$V1 < end_time, ]
#     
#     # Convert to SpatialPointsDataFrame and transform projection
#     gps.spdf <- SpatialPointsDataFrame(coords = cbind(gps$V2, gps$V3), 
#                                        data = gps, 
#                                        proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
#     
#     gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
#     
#     # Add id_code as a new column to gps.spdf
#     id_code <- sub("^[^_]+_([^_]+).*", "\\1", basename(file_GPS))
#     gps.spdf$id_code <- id_code
#     
#   }
# }

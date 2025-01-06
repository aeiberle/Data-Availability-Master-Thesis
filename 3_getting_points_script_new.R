#### LOOP for displaying individual trajectories as well as a combined trajectories plot ####
#### and saving the gps points; all for a given time window 

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
root_dir <- "/mnt/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/"
alt_root_dir <- "/mnt/EAS_ind/aeiberle/data/"
output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/"

# Define Specific Filenames for Each Date
specific_filenames_20230615 <- c("SI_VECM009_SH_H_Axy_030_20230613-20230615_S24_NEW.txt",
                                 "SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46_NEW.txt",
                                 "SI_VSIM020_TB_Axy_016_20230611_20230615_S36_NEW.txt",
                                 "SI_VZUF048_RC_RS_TB_GX011333_HERO9 Black-GPS5.txt"
                                 )

specific_filenames_20230701 <- c("VECM009_SH+H_Axy029_S1.txt",
                                 "VSIF023_RS_Axy026_S1.txt",
                                 "VSIM015_RR+MB_Axy016_S1.txt",
                                 "VSIM021_RT_Axy036_S1.txt"
                                 )

specific_filenames_20230624 <- c("BS_VBSF001_RS_TB_Axy_022_20230624-20230628_S1.txt",
                                 "BS_VBSF002_RS_RT_Axy_035_20230624-20230628_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_003_20230624-20230628_S1.txt",
                                 "BS_VBSM007_RT_Axy_037_20230624-20230628_S1.txt",
                                 "BS_VBSF008_LT_Axy_015_20230624-20230628_Axy015_S1.txt",
                                 "BS_VGDM011_RT_LT_Axy_018_20230625-20230628_S1.txt",
                                 "BS_VHMM036_RR_LT_Axy_021_20230624-20230628_Axy021_S1.txt",
                                 "BS_VMPF035_GPS_20230624_Axy063_S3.txt"
                                  )

specific_filenames_20230716 <- c("BS_VBSF001_GPS_20230716_Axy022_S5.txt",
                                 "BS_VBSF008_LT_Axy_001_20230716-20230717_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_006_20230712-20230717_S1.txt",
                                 "BS_VMPF035_GPS_20230716_Axy021_S2.txt"
                                 )

specific_filenames_20230720 <- c("VZUF028_RC+MB+TB_Axy021_S4.txt",
                                 "VZUF051_RR+TB_Axy10_S1.txt",
                                 "VZUF052_RR+RT_Axy 019_S1_NEW.txt",
                                 "VZUF070_RT_Axy025_S1.txt",
                                 "VZUM063_SH+RT_Axy028_S3_NEW.txt",
                                 "VZUM064_SH+LT_Axy022_S7.txt",
                                 "VZUM066_SH+RS_Axy011_S1.txt",
                                 "VZUM067_TB_Axy007_S1.txt"
                                 )

specific_filenames_20230730 <- c("ZU_VZUF051_RR+TB_Axy10_S1.txt",
                                 "ZU_VZUF052_RR+RT_Axy019_S1.txt",
                                 "VZUF070_RT_Axy033_S1.txt",
                                 "ZU_VZUM066_SH+RS_Axy012_S1.txt",
                                 "ZU_VZUM067_TB_Axy007_S1.txt"
                                 )


# Combine All Specific Filenames into a List
date_specific_files <- list(
  "2023-06-15" = specific_filenames_20230615,
  "2023-07-01" = specific_filenames_20230701,
  "2023-06-24" = specific_filenames_20230624,
  "2023-07-16" = specific_filenames_20230716,
  "2023-07-20" = specific_filenames_20230720,
  "2023-07-30" = specific_filenames_20230730
)

# Load the playback times CSV (replace 'path_to_playback.csv' with the actual file path)
playback_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/PB_time_startend_info.csv", header = TRUE, sep = ";")

# Combine Date and time (t0, t1) into proper POSIXct format
playback_df <- playback_df %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
         t0 = as.POSIXct(paste(Date, t0), format = "%Y-%m-%d %H:%M:%S"),
         t1 = as.POSIXct(paste(Date, t1), format = "%Y-%m-%d %H:%M:%S"))

# Initialize Storage for Sample Tables
all_sample_tables <- data.frame()

# Initialize an empty list to store gps.spdf for each file
gps_spdf_list <- list()

# Initialize a list to store plots
plot_list_individual <- list()
plot_list_combined <- list()

# Load CSV containing start and end times for each date
time_window <- read.csv(paste0(alt_root_dir,"time_startend_info.csv"), stringsAsFactors = FALSE, sep = ";")

# Assuming the CSV has columns "Date", "t0" (start time), and "t1" (end time)
# Convert the Date column to POSIXlt to match the date format
time_window$Date <- as.POSIXlt(time_window$Date, format = "%Y-%m-%d", tz = "UTC")

# Loop Through Each Date and its Specific Filenames
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and its subfolders
  if (date == "2023-06-15") {
    # Use only the alternate root directory
    all_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
  } else if (date == "2023-07-20") {
    
    # Combine files from both the alternate root directory and the root directory
    alt_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
    root_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
    all_files <- c(alt_files, root_files)
  } else {
    # Use only the root directory
    all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  }
  
  # # Get all files in the root directory and its subfolders
  # all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  
  # Filter files based on specific filenames for the current date
  file_list <- all_files[basename(all_files) %in% specific_filenames]

  # Initialize a data frame to store combined trajectory data for the current date
  all_trajectories <- data.frame()
  
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
    
    # # Filter the GPS points by date range (you can adjust as needed)
    # start_time <- as.POSIXlt(paste(date, "08:57:00"), tz = "UTC")
    # end_time <- as.POSIXlt(paste(date, "10:00:00"), tz = "UTC")
    
    gps <- gps[gps$V1 > start_time & gps$V1 < end_time, ]
    
    # Convert to SpatialPointsDataFrame and transform projection
    gps.spdf <- SpatialPointsDataFrame(coords = cbind(gps$V3, gps$V2), 
                                       data = gps, 
                                       proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
    
    gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
    
    gps.spdf <- gps.spdf %>% as_tibble()
    
    # Add id_code as a new column to gps.spdf
    id_code <- sub("^[^_]+_([^_]+).*", "\\1", basename(file_GPS))
    gps.spdf$id_code <- id_code
    
    # Extract coordinates
    trajX <- gps.spdf$coords.x1
    trajY <- gps.spdf$coords.x2
    
    # Spatial discretization
    points <- spatialDiscr(gps.spdf$coords.x1, gps.spdf$coords.x2, distance)
    
    # Extract the sample table
    sampleTable <- gps.spdf[points, c("id_code", "coords.x1", "coords.x2")]
    sampleTable <- cbind(1:nrow(sampleTable), sampleTable)
    
    # Assign column names
    colnames(sampleTable) <- c("Nbr", "id_code", "Latitude", "Longitude")
    
    # Append the sampleTable for this file to the overall sample tables
    all_sample_tables <- rbind(all_sample_tables, sampleTable)
    
    # Combine the current gps.spdf data into the combined data for the date
    all_trajectories <- rbind(all_trajectories, gps.spdf)
    
    # Append to the list of gps.spdf
    gps_spdf_list[[file_GPS]] <- gps.spdf
    
    # First, convert the datetime values to numeric (e.g., seconds since start)
    gps.spdf$time_numeric <- as.numeric(difftime(gps.spdf$V1, min(gps.spdf$V1), units = "secs"))
    
    # Create an individual plot for the trajectory
    individual_plot <- ggplot(gps.spdf, aes(x = coords.x1, y = coords.x2)) +
      geom_point(aes(color = time_numeric), size = 2) +  # Use timestamp (V1) to color points
      scale_color_gradient(low = "blue", high = "chocolate1") +
      # geom_segment(aes(xend = lead(coords.x1), yend = lead(coords.x2)),
      #              arrow = arrow(length = unit(0.15, "cm")),
      #              color = "black") +
      labs(title = paste("Trajectory for", basename(id_code),
                         "on", date),
           x = "Longitude",
           y = "Latitude") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90"),
            plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            legend.title = element_text(size = 14),
            legend.text = element_text(size = 12))
    
    # Store the plot in the list
    plot_list_individual[[date]][[file_GPS]] <- individual_plot
    
    # Print all plots in the list
    for (p in plot_list_individual) {
      print(p)  # Displays each plot one by one
    }
    
    # Save the individual plot to the specified output directory (!Don't forget to uncomment to save!)
    #ggsave(filename = paste0(output_dir, gsub("-", "", date), "_trajectory_plot_", sub(".txt", "", basename(id_code)), ".png"), plot = individual_plot)
    
    # Print a message indicating which file has been processed
    print(paste("Processed:", file_GPS))
  }


  # Create a combined trajectories plot for each date
  combined_plot <- ggplot(all_trajectories, aes(x = coords.x1, y = coords.x2, color = id_code)) +
    # geom_segment(aes(xend = lead(coords.x1), yend = lead(coords.x2)), 
    #              arrow = arrow(length = unit(0.15, "cm"))) +
    geom_point(alpha = 0.7) +
    labs(title = paste("Trajectories of different individuals for", date),
         x = "Longitude",
         y = "Latitude") +
    theme_minimal() +
    theme(plot.background = element_rect(fill = "white", color = NA),
          panel.background = element_rect(fill = "white", color = NA),
          panel.grid.major = element_line(color = "gray80"),
          panel.grid.minor = element_line(color = "gray90"),
          plot.title = element_text(size = 16),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 12),
          legend.title = element_text(size = 14),
          legend.text = element_text(size = 12))
  
  # Store the plot in the list
  plot_list_combined[[date]] <- combined_plot
  
  # Comment this part to print all plots in the plotting window, 
  # BUT uncomment it to save the plots; only works either way
  for (q in plot_list_combined) {
    print(q)  # Displays each plot one by one
  }
   
  # Save the combined plot for the date (!Don't forget to uncomment to save!)
  #ggsave(filename = paste0(output_dir, gsub("-", "", date), "_trajectories_plot_combined", ".png"), plot = combined_plot)
  
  # Print a message indicating completion for the date
  print(paste("Completed plotting for date:", date))
}

# Create complete gps data frame for all dates
final_gps_spdf <- do.call(rbind, gps_spdf_list)
#View(final_gps_spdf)

# Write the table to the server directory (!Don't forget to uncomment to save!)
#write.table(final_gps_spdf, file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_2023.csv", sep = ",", quote = F, row.names = F)


# Print if everything completed
print("All processing and plotting completed.")

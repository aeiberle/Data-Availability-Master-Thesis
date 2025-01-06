#### LOOP for displaying individual trajectories as well as a combined trajectories plot ####
#### and saving the gps points; all for a given time window 

library(sp)
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
specific_filenames_20230615 <- c("SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46_NEW.txt")

specific_filenames_20230624 <- c("BS_VBSF002_RS_RT_Axy_035_20230624-29230628_S1.txt",
                                 "BS_VBSF008_LT_Axy_015_20230624-20230628_Axy015_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_003_20230624-20230628_S1.txt")

specific_filenames_20230716 <- c("BS_VBSF008_LT_Axy_001_20230716-20230717_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_006_20230712-20230717_S1.txt",
                                 "BS_VBSF001_GPS_20230716_Axy022_S5.txt",
                                 "BS_VMPF035_GPS_20230716_Axy021_S2.txt")

# Combine All Specific Filenames into a List
date_specific_files <- list(
  "2023-06-15" = specific_filenames_20230615,
  "2023-06-24" = specific_filenames_20230624,
  "2023-07-16" = specific_filenames_20230716
)

# Load the playback times CSV
playback_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/PB_time_startend_info.csv", header = TRUE, sep = ";")

# Combine Date and time (t0, t1) into proper POSIXct format
playback_df <- playback_df %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
         t0 = as.POSIXct(paste(Date, t0), format = "%Y-%m-%d %H:%M:%S"),
         t1 = as.POSIXct(paste(Date, t1), format = "%Y-%m-%d %H:%M:%S"))

# Initialize an empty list to store gps.spdf for each file
gps_spdf_list <- list()

# Initialize lists to store individual and combined plots
plot_list_individual <- list()
plot_list_combined <- list()

# Load CSV containing start and end times for each date
time_window <- read.csv(paste0(alt_root_dir,"time_startend_info.csv"), stringsAsFactors = FALSE, sep = ";")

# Convert the Date column to POSIXlt to match the date format
time_window$Date <- as.POSIXlt(time_window$Date, format = "%Y-%m-%d", tz = "UTC")

# Loop Through Each Date and its Specific Filenames
for (date in names(date_specific_files)) {
  specific_filenames <- date_specific_files[[date]]
  
  # Get all files in the root directory and check for the specific date to use an alternate root directory
  if (date == "2023-06-15") {
    all_files <- list.files(path = alt_root_dir, recursive = TRUE, full.names = TRUE)
  } else {
    all_files <- list.files(path = root_dir, recursive = TRUE, full.names = TRUE)
  }
  
  # Filter files based on specific filenames for the current date
  file_list <- all_files[basename(all_files) %in% specific_filenames]

  # Initialize a data frame to store combined trajectory data for the current date
  all_trajectories <- data.frame()
  
  # Find the row in the playback_df that matches the current date
  playback_times <- playback_df[playback_df$Date == as.Date(date), ]
  
  # Ensure the playback times exist for this date
  if (nrow(playback_times) == 1) {
    playback_start <- playback_times$t0
    playback_end <- playback_times$t1
  } else {
    warning(paste("No playback time window found for date:", date))
    next
  }
  
  # Loop through each GPS file found
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
    
    # Filter the GPS points by date range
    gps <- gps[gps$V1 > playback_start - 600  & gps$V1 < playback_end + 600, ]
    
    # Convert to SpatialPointsDataFrame and transform projection
    gps.spdf <- SpatialPointsDataFrame(coords = cbind(gps$V2, gps$V3), 
                                       data = gps, 
                                       proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
    
    gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
    
    # Combine GPS data for all segments, adding a 'segment' column
    gps_pre <- gps %>% filter(V1 < playback_start) %>% mutate(segment = "Pre-Playback")
    gps_playback <- gps %>% filter(V1 >= playback_start & V1 <= playback_end) %>% mutate(segment = "Playback")
    gps_post <- gps %>% filter(V1 > playback_end) %>% mutate(segment = "Post-Playback")
    
    # Bind all segments into one data frame
    gps_combined <- bind_rows(gps_pre, gps_playback, gps_post)
    
    # Convert time to numeric for gradient within each segment
    gps_combined <- gps_combined %>%
      mutate(time_numeric = as.numeric(difftime(V1, playback_start, units = "secs")))
    
    # Create the plot with color gradients by segment
    individual_plot <- ggplot(gps_combined, aes(x = V2, y = V3)) +
      geom_path(aes(color = segment, group = segment), linewidth = 1, alpha = 0.8) +
      geom_point(aes(color = segment), size = 2, alpha = 1) +
      scale_color_manual(values = c("Pre-Playback" = "darkblue",
                                    "Playback" = "deeppink",
                                    "Post-Playback" = "chocolate1"),
                         name = "Segment") +
      scale_linetype_manual(values = c("solid", "solid", "solid")) + # Use solid lines for each segment
      labs(title = paste("Trajectory for", basename(id_code), "on", date),
           x = "Longitude", y = "Latitude") +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90"),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14))
    
    # Store and display the plot
    plot_list_individual[[date]][[file_GPS]] <- individual_plot
    print(individual_plot)
    
    # Print a message indicating which file has been processed
    print(paste("Processed:", file_GPS))
  }
}

print("All processing and plotting completed.")

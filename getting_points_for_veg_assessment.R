#### Single ####
#FUNCTIONS

library(sp)
# install.packages("rgdal")
# library(rgdal)
library(dplyr)
library(sf)


#function to spatially display a track
spatialDiscr <- function(x,y,step){
  currentPoint <- which(!is.na(x))[1]
  points <- currentPoint
  t <- currentPoint + 1
  dist = 0
  while(t <= length(x)){

    while(dist < step & t <= length(x)){
      if(is.na(x[t])){
        t <- t+1
        next()
      }

      dist <- sqrt( (x[t]-x[currentPoint])^2 + (y[t]-y[currentPoint])^2)
      t <- t+1
    }
    currentPoint <- t-1
    points <- c(points,currentPoint)
    dist <- 0

  }
  points
}


#sample GPS every 50 m; possible to change it
distance <- 50

#loading the gps file that contains the trajectory
#script written for files out of Axitrek units, parts might have to be re-written if using different GPS units

##set working directory to the /meerkat_movecomm_2023 folder so we can later create a loop to go through all the .gpx files

#setwd("~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023/COLLARS/GPS/BS_VHMM036_RR_LT_Axy_021_20230624-20230628")
#setwd("~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023/BS_20230624/BS_FOCAL_GPS_20230624/BS_VMPF035_GPS_20230624")

#("~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023_2/COLLARS/GPS/BS_VBSM005_RS_LS_Axy_006_20230712-20230717")
#setwd("~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023_2/BS_20230716/BS_FOCAL_GPS_20230716/BS_VMPF035_GPS_20230716")

setwd("/mnt/EAS_ind/aeiberle/data/")


## set date not needed if I set working directory properly
# date <- 20230716

# name of the file to load in
file_GPS <- "NEW_SI_VSIM020_TB_Axy_016_20230611_20230615_S36.txt"

# specific_filenames_20230615 <- c("NEW_SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46.txt",
#                                  "NEW_SI_VSIM020_TB_Axy_016_20230611_20230615_S36.txt"
#                                  )


#change path according to file location on your own machine
gps <- read.delim(file = ("NEW_SI_VSIM020_TB_Axy_016_20230611_20230615_S36.txt"), header = FALSE)

gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
gps <- gps[ , -2]
colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

gps$V2 <- as.numeric(as.character(gps$V2))
gps$V3 <- as.numeric(as.character(gps$V3))

gps <- gps[which(complete.cases(gps)), ]

  gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
                                              "%Y/%m/%d %H:%M:%OS",
                                              "%Y-%m-%d %H:%M",
                                              "%Y/%m/%d %H:%M",
                                              "%Y-%m-%d",
                                              "%Y/%m/%d"),tz = "UTC")
  #remove GPS points before and after data collection window.
  #TODO: make this generalizable at some point
  #change time to make it fit the recruitment experiment
  gps <- gps[which(gps$V1  > as.POSIXlt("2023-06-15, 07:00:00", format = "%Y-%m-%d,%H:%M:%OS", tz = "UTC")), ]
  gps <- gps[which(gps$V1  < as.POSIXlt("2023-06-15, 10:00:00", format = "%Y-%m-%d,%H:%M:%OS", tz = "UTC")), ]



#I think this is the line where Lat and Long values are translated into x and y
gps.spdf <- SpatialPointsDataFrame(coords=cbind(gps$V2, gps$V3), data=gps, proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))

gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))

gps.spdf <- gps.spdf %>% as_tibble()
trajX <- gps.spdf$coords.x1
trajY <- gps.spdf$coords.x2

points <- spatialDiscr(gps.spdf$coords.x1, gps.spdf$coords.x2, distance)

# Add id_code as a new column to gps.spdf
id_code <- sub("^SI_([^_]+).*", "\\1", basename(file_GPS))
gps.spdf$id_code <- id_code

sampleTable<- gps.spdf[points, c(2,3)]
sampleTable <- cbind(1:nrow(sampleTable), sampleTable)

colnames(sampleTable) <- c("Nbr","Latitude","Longitude")

library(ggplot2)
gps.spdf %>% ggplot() + geom_point(aes(x=coords.x1, y=coords.x2))


#write the table, modify accordingly
write.table(gps.spdf, file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_20230615/gps_spdf_20230615_VSIM020.csv", sep = ",", quote = F, row.names = F)


# 
# #### LOOP ####
# 
# library(sp)
# #install.packages("rgdal")
# #library(rgdal)
# library(dplyr)
# library(sf)
# library(ggplot2)
# 
# Function to spatially display a track
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


# Sample GPS every 50 m (you can change it)
distance <- 50

# Set the root working directory where all your GPS files are located
root_dir <- "~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/"

# Define output directory for saving plots
output_dir <- "/mnt/EAS_ind/aeiberle/data/"

# Get all the GPS .txt files in the folder and subfolders
all_files <- list.files(path = root_dir, pattern = "Axy.*\\.txt$", recursive = TRUE, full.names = TRUE)

# Specify the exact filenames you want
# specific_filenames_20230615 <- c("SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46.txt",
#                                  "SI_VSIM020_TB_Axy_016_20230611_20230615_S36.txt"
#                                  )

# specific_filenames_20230716 <- c("BS_VBSF008_LT_Axy_001_20230716-20230717_S1.txt",
#                                  "BS_VBSM005_RS_LS_Axy_006_20230712-20230717_S1.txt",
#                                  "BS_VBSF001_GPS_20230716_Axy022_S5.txt",
#                                  "BS_VMPF035_GPS_20230716_Axy021_S2.txt"
#                                  )

specific_filenames_20230624 <- c("BS_VBSF001_RS_TB_Axy_022_20230624-29230628_S1.txt",
                                 "BS_VBSF002_RS_RT_Axy_035_20230624-29230628_S1.txt",
                                 "BS_VBSF008_LT_Axy_015_20230624-20230628_Axy015_S1.txt",
                                 "BS_VBSM005_RS_LS_Axy_003_20230624-20230628_S1.txt",
                                 "BS_VHMM036_RR_LT_Axy_021_20230624-20230628_Axy021_S1.txt",
                                 "BS_VMPF035_GPS_20230624_Axy063_S3.txt"
                                 )

# Filter the list to include only the specific filenames
file_list <- all_files[basename(all_files) %in% specific_filenames_20230615] # DONT FORGET TO CHANGE FILENAMES

# Check the filtered list of files
print(file_list)

all_sample_tables <- data.frame()

# Loop through each file
for (file_GPS in file_list) {

  # Load the GPS file
  gps <- read.delim(file = file_GPS, header = FALSE)

  # Prepare the data
  gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
  gps <- gps[, -2]
  colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")

  gps$V2 <- as.numeric(as.character(gps$V2))
  gps$V3 <- as.numeric(as.character(gps$V3))

  gps <- gps[which(complete.cases(gps)), ]

  # Convert the V1 column to POSIXlt
  gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
                                              "%Y/%m/%d %H:%M:%OS",
                                              "%Y-%m-%d %H:%M",
                                              "%Y/%m/%d %H:%M",
                                              "%Y-%m-%d",
                                              "%Y/%m/%d"), tz = "UTC")

  # Filter the GPS points by date (you can modify this)
  gps <- gps[which(gps$V1  > as.POSIXlt("2023-06-15, 07:00:00", format = "%Y-%m-%d,%H:%M:%OS", tz = "UTC")), ] # DONT FORGET TO CHANGE DATE
  gps <- gps[which(gps$V1  < as.POSIXlt("2023-06-15, 10:00:00", format = "%Y-%m-%d,%H:%M:%OS", tz = "UTC")), ] # DONT FORGET TO CHANGE DATE

  # Convert to SpatialPointsDataFrame and transform projection
  gps.spdf <- SpatialPointsDataFrame(coords = cbind(gps$V2, gps$V3), data = gps, proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
  gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))

  gps.spdf <- gps.spdf %>% as_tibble()

  # Extract coordinates
  trajX <- gps.spdf$coords.x1
  trajY <- gps.spdf$coords.x2

  # Spatial discretization
  points <- spatialDiscr(gps.spdf$coords.x1, gps.spdf$coords.x2, distance)

  # Extract the sample table
  sampleTable <- gps.spdf[points, c(2, 3)]
  sampleTable <- cbind(1:nrow(sampleTable), sampleTable)

  # Assign column names
  colnames(sampleTable) <- c("Nbr", "Latitude", "Longitude")

  # Append the sampleTable for this file to the overall sample tables
  # all_sample_tables <- rbind(all_sample_tables, sampleTable)

  # Generate dynamic filename
  # output_filename <- paste0("/mnt/EAS_ind/aeiberle/data/sampleTable_", sub(".txt", "", file_GPS), ".csv")

  # Create a plot for the trajectory
  # plot <- ggplot(gps.spdf, aes(x = coords.x1, y = coords.x2)) +
                geom_point(color = "black") +
                labs(title = paste("Trajectory for", basename(file_GPS)),
                x = "Longitude",
                y = "Latitude") +
                theme_minimal()+
                theme(plot.background = element_rect(fill = "white", color = NA),
                      panel.background = element_rect(fill = "white", color = NA),
                      panel.grid.major = element_line(color = "gray80"),
                      panel.grid.minor = element_line(color = "gray90"),
                      axis.title.x = element_text(size = 14),
                      axis.title.y = element_text(size = 14),
                      axis.text.x = element_text(size = 12),
                      axis.text.y = element_text(size = 12),
                      legend.title = element_text(size = 14),
                      legend.text = element_text(size = 12))

  # Save the plot to the specified output directory
  # ggsave(filename = paste0(output_dir, "trajectory_plot_", sub(".txt", "", basename(file_GPS)), ".png"), plot = plot)


  # Print a message indicating which file has been processed
  print(paste("Processed:", file_GPS))
}

# View(all_sample_tables)
# # Write the table to the server directory
# write.table(all_sample_tables, file = "/mnt/EAS_ind/aeiberle/data/all_sample_tables_20230615.csv", sep = ",", quote = F, row.names = F)

View(gps.spdf)
# # Write the table to the server directory
# write.table(gps.spdf, file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_20230615.csv", sep = ",", quote = F, row.names = F)

# #### LOOP; all trajectories in one plot ####
# 
# library(sp)
# #install.packages("rgdal")
# library(rgdal)
# library(dplyr)
# library(sf)
# library(ggplot2)
# 
# 
# # Function to spatially display a track
# spatialDiscr <- function(x, y, step) {
#   currentPoint <- which(!is.na(x))[1]
#   points <- currentPoint
#   t <- currentPoint + 1
#   dist = 0
#   while (t <= length(x)) {
#     while (dist < step & t <= length(x)) {
#       if (is.na(x[t])) {
#         t <- t + 1
#         next()
#       }
# 
#       dist <- sqrt((x[t] - x[currentPoint])^2 + (y[t] - y[currentPoint])^2)
#       t <- t + 1
#     }
#     currentPoint <- t - 1
#     points <- c(points, currentPoint)
#     dist <- 0
#   }
#   points
# }
# 
# # Sample GPS every 50 m (you can change this value)
# distance <- 50
# 
# # Set the root working directory where all your GPS files are located
# root_dir <- "~/EAS_shared/meerkat/archive/rawdata/meerkat_movecomm_2023/BS_2023/" # DONT FORGET TO CHANGE WD
# 
# # Get all the GPS .txt files in the folder and subfolders
# all_files <- list.files(path = root_dir, pattern = "Axy.*\\.txt$", recursive = TRUE, full.names = TRUE)
# 
# # Specify the exact filenames you want
# # specific_filenames_20230615 <- c("SI_VSIM012_LS_RT_Axy_004_20230611-20230615_S46.txt",
# #                                  "SI_VSIM020_TB_Axy_016_20230611_20230615_S36.txt"
# #                                  )
# 
# specific_filenames_20230624 <- c("BS_VBSF001_RS_TB_Axy_022_20230624-29230628_S1.txt",
#                                  "BS_VBSF002_RS_RT_Axy_035_20230624-29230628_S1.txt",
#                                  "BS_VBSF008_LT_Axy_015_20230624-20230628_Axy015_S1.txt",
#                                  "BS_VBSM005_RS_LS_Axy_003_20230624-20230628_S1.txt",
#                                  "BS_VHMM036_RR_LT_Axy_021_20230624-20230628_Axy021_S1.txt",
#                                  "BS_VMPF035_GPS_20230624_Axy063_S3.txt"
#                                  )
# 
# # specific_filenames_20230716 <- c("BS_VBSF008_LT_Axy_001_20230716-20230717_S1.txt",
# #                                  "BS_VBSM005_RS_LS_Axy_006_20230712-20230717_S1.txt",
# #                                  "BS_VBSF001_GPS_20230716_Axy022_S5.txt",
# #                                  "BS_VMPF035_GPS_20230716_Axy021_S2.txt"
# #                                  )
# 
# 
# # Filter the list to include only the specific filenames
# file_list <- all_files[basename(all_files) %in% specific_filenames_20230624] # DONT FORGET TO CHANGE FILENAMES DATE
# 
# # Create an empty data frame to store all trajectories
# all_trajectories <- data.frame()
# 
# # Loop through each file
# for (file_GPS in file_list) {
# 
#   # Load the GPS file
#   gps <- read.delim(file = file_GPS, header = FALSE)
# 
#   # Prepare the data
#   gps$V1 <- paste(gps$V1, gps$V2, sep = ",")
#   gps <- gps[, -2]
#   colnames(gps) <- c("V1", "V2", "V3", "V4", "V5", "V6", "V7", "V8")
# 
#   gps$V2 <- as.numeric(as.character(gps$V2))
#   gps$V3 <- as.numeric(as.character(gps$V3))
# 
#   gps <- gps[which(complete.cases(gps)), ]
# 
#   # Convert the V1 column to POSIXlt
#   gps$V1 <- as.POSIXlt(gps$V1, tryFormats = c("%Y-%m-%d,%H:%M:%OS",
#                                               "%Y/%m/%d %H:%M:%OS",
#                                               "%Y-%m-%d %H:%M",
#                                               "%Y/%m/%d %H:%M",
#                                               "%Y-%m-%d",
#                                               "%Y/%m/%d"), tz = "UTC")
# 
#   # Filter the GPS points by date (you can modify this)
#   gps <- gps[which(gps$V1  > as.POSIXlt("2023-06-24, 07:00:00", format = "%Y-%m-%d,%H:%M:%OS", tz = "UTC")), ] # DONT FORGET TO CHANGE DATE
#   gps <- gps[which(gps$V1  < as.POSIXlt("2023-06-24, 10:00:00", format = "%Y-%m-%d,%H:%M:%OS", tz = "UTC")), ] # DONT FORGET TO CHANGE DATE
#   
#   # Add id_code as a new column to gps.spdf
#   id_code <- sub("^BS_([^_]+).*", "\\1", basename(file_GPS))
#   
#   # Convert to SpatialPointsDataFrame and transform projection
#   gps.spdf <- SpatialPointsDataFrame(coords = cbind(gps$V2, gps$V3), data = gps, proj4string = CRS("+proj=longlat +south +zone=34 +datum=WGS84"))
#   gps.spdf <- spTransform(gps.spdf, CRS("+proj=utm +south +zone=34 +datum=WGS84"))
# 
#   gps.spdf <- gps.spdf %>% as_tibble()
#   
#   # Extract coordinates
#   trajX <- gps.spdf$coords.x1
#   trajY <- gps.spdf$coords.x2
# 
#   # Spatial discretization
#   points <- spatialDiscr(gps.spdf$coords.x1, gps.spdf$coords.x2, distance)
# 
#   # Extract the sample table
#   sampleTable <- gps.spdf[points, c(2, 3)]
#   sampleTable <- cbind(1:nrow(sampleTable), sampleTable)
# 
#   # Assign column names
#   colnames(sampleTable) <- c("Nbr", "Latitude", "Longitude")
# 
#   # Create a temporary data frame for the trajectory
#   temp_trajectory <- gps.spdf[points, c("coords.x1", "coords.x2")]
#   temp_trajectory$Individual <- basename(file_GPS)  # Add individual identifier
# 
#   # Add ID to trajectory df
#   temp_trajectory$ID <- id_code
# 
#   # Combine the trajectories
#   all_trajectories <- rbind(all_trajectories, temp_trajectory)
# 
#   # Print a message indicating which file has been processed
#   print(paste("Processed:", file_GPS))
# }
# 
# View(gps.spdf)
# # Write the table to the server directory
# write.table(gps.spdf, file = "/mnt/EAS_ind/aeiberle/data/gps_spdf_20230624.csv", sep = ",", quote = F, row.names = F)
# 
# # # Define the directory where you want to save the plot
# # save_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/"
# # 
# # # Create a combined plot for all individuals
# # combined_plot <- ggplot(all_trajectories, aes(x = coords.x1, y = coords.x2, color = ID)) +
# #   geom_point() +
# #   labs(title = "Trajectories of Different Individuals",
# #        x = "Longitude",
# #        y = "Latitude") +
# #   theme_minimal() +
# #   theme(plot.background = element_rect(fill = "white", color = NA),
# #         panel.background = element_rect(fill = "white", color = NA),
# #         panel.grid.major = element_line(color = "gray80"),
# #         panel.grid.minor = element_line(color = "gray90"),
# #         axis.title.x = element_text(size = 14),
# #         axis.title.y = element_text(size = 14),
# #         axis.text.x = element_text(size = 12),
# #         axis.text.y = element_text(size = 12),
# #         legend.title = element_text(size = 14),
# #         legend.text = element_text(size = 12))
# #   theme(legend.position = "right")
# # 
# # # Optionally save the combined plot
# # ggsave(filename = paste0(save_dir, "combined_trajectory_plot_20230615.png"), plot = combined_plot)

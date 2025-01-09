library(dplyr)
library(geosphere)
library(ggplot2)

 audio_gps_timeline <- read.csv("/mnt/EAS_ind/aeiberle/data/audio_gps_timeline_new.csv")
 
 
 #which trials we are using
 trials_with_audio <- c("2023-06-24 BS", "2023-07-16 BS", "2023-06-15 SI", "2023-07-01 SI", "2023-07-20 ZU", "2023-07-30 ZU")
 
 
 #calls of interest
 calls_int <- c("al", "agg", "ld", "mo", "soc", "s")
 #get trials that we have audio data for
 audio_gps_timeline <- subset(audio_gps_timeline, trial %in% trials_with_audio)
 
 #trim data to include -5:240 sec
 audio_gps_timeline <- subset(audio_gps_timeline, re_numeric > -5)
 
 
 #when and where the first call is produced
 first_call_summary <- data.frame()
 
 #sort by time
 audio_gps_timeline <- audio_gps_timeline[order(audio_gps_timeline$dattime),]
 audio_gps_timeline$re_numeric <- as.integer(audio_gps_timeline$re_numeric)
 
 # ****************************************************************************
 # ****************************************************************************
 
 #test
 
 
 #check where the recruitment playback starts
 all_rec_pb_times <- audio_gps_timeline[which(audio_gps_timeline$label_name == "Recruitment Playback Track"), ] 
 shifted_data_frames <- data.frame()
 # recoding PB time
 for (trial_sel in trials_with_audio) {
   #trial_sel <- "2023-07-20 ZU"
   trial_select <- subset(audio_gps_timeline, trial == trial_sel)
   for (ind in unique(trial_select$ID)) {
     #ind <- "VZUF048"
     ID_select <- subset(trial_select, ID == ind)
     shift_amount <- ID_select[which(ID_select$label_name == "Recruitment Playback Track"), "re_numeric"][1]
     if (is.na(shift_amount)) {
       GPS_ID_select<-  ID_select[ ,1:11] 
       GPS_ID_select$re_numeric <- as.integer(GPS_ID_select$re_numeric)
       GPS_ID_select<-  GPS_ID_select[!duplicated(GPS_ID_select$re_numeric), ]
       AUDIO_ID_select <- ID_select[ ,c(8,12:20)] 
       AUDIO_ID_select <-  AUDIO_ID_select[!duplicated(AUDIO_ID_select$t0_file ), ]
       shifted_ID_select <- full_join(GPS_ID_select, AUDIO_ID_select, join_by(re_numeric) )
       shifted_ID_select <- shifted_ID_select[order(shifted_ID_select$re_numeric),]
       shifted_data_frames <- rbind(shifted_data_frames, shifted_ID_select)
       
       next}
     
     else  {
       GPS_ID_select<-  ID_select[ ,1:11] 
       GPS_ID_select$re_numeric <- as.integer(GPS_ID_select$re_numeric)
       GPS_ID_select<-  GPS_ID_select[!duplicated(GPS_ID_select$re_numeric), ]
       AUDIO_ID_select <- ID_select[ ,c(8,12:20)] 
       AUDIO_ID_select$re_numeric <- AUDIO_ID_select$re_numeric - shift_amount
       AUDIO_ID_select <-  AUDIO_ID_select[!duplicated(AUDIO_ID_select$t0_file ), ]
       shifted_ID_select <- full_join(GPS_ID_select, AUDIO_ID_select, join_by(re_numeric) )
       shifted_ID_select <- shifted_ID_select[order(shifted_ID_select$re_numeric),]
       shifted_data_frames <- rbind(shifted_data_frames, shifted_ID_select)
     }
     
     
     }
 }

 #check where the recruitment playback starts
 all_rec_pb_times_shifted <- shifted_data_frames[which(shifted_data_frames$label_name == "Recruitment Playback Track"), ] 
 
 #################################################################################################
 #################################################################################################
 

 audio_gps_timeline <-  shifted_data_frames
 audio_gps_timeline <- audio_gps_timeline[order(audio_gps_timeline$dattime),]
 audio_gps_timeline <- subset(audio_gps_timeline, re_numeric > -1)
 
 
 
 first_call_summary <- data_frame()
 for (trial_sel in trials_with_audio) {
   #trial_sel <- "2023-07-20 ZU"
   trial_select <- subset(audio_gps_timeline, trial == trial_sel)
  #get idx of callers 
  caller_idx <- which(trial_select$label_name %in% calls_int) 
  
  #get the speaker distances of all individuals at the time of the first call
  all_distances <- trial_select[which(trial_select$re_numeric == trial_select$re_numeric[caller_idx[1]]), "dist"]
  
  #get the distance ranking of the first caller
  all_distances <- sort(all_distances)
  dist_rank <- which(all_distances ==  trial_select$dist[caller_idx[1]])[1]
  
  #get_first_caller location
  lat_1 <- trial_select$lat[caller_idx[1]]
  lon_1 <- trial_select$lon[caller_idx[1]]
  #summarize first caller
  first_call_summary <- rbind(first_call_summary, c(trial_select$ID[caller_idx[1]],
                                 trial_select$dist[caller_idx[1]],
                                 dist_rank,
                                 trial_select$re_numeric[caller_idx[1]],
                                 trial_select$trial[caller_idx[1]],
                                 trial_select$label_name[caller_idx[1]], "first", NA))
  
  #get the second caller idx
  second_caller_idx <- which(trial_select$label_name %in% calls_int & trial_select$ID != trial_select$ID[caller_idx[1]])[1] 
  #get the speaker distances of all individuals at the time of the second call
  all_distances_second <- trial_select[which(trial_select$re_numeric == trial_select$re_numeric[second_caller_idx]), "dist"]
  
  #get the distance ranking of the second caller
  all_distances_second <- sort( all_distances_second)
  dist_rank_sec <- which(all_distances_second ==  trial_select$dist[second_caller_idx])[1]
  
  #get_first_caller location
  lat_2 <- trial_select$lat[second_caller_idx]
  lon_2 <- trial_select$lon[second_caller_idx]
  
  #get the distance from first caller
  prvs_cllr_dist <- distm(c(trial_select$lat[second_caller_idx], trial_select$lon[second_caller_idx]), c(lat_1, lon_1), fun = distGeo) 
  
  #summarize second caller
  first_call_summary <- rbind(first_call_summary, c(trial_select$ID[second_caller_idx],
                                                    trial_select$dist[second_caller_idx],
                                                    dist_rank_sec,
                                                    trial_select$re_numeric[second_caller_idx],
                                                    trial_select$trial[second_caller_idx],
                                                    trial_select$label_name[second_caller_idx], "second", prvs_cllr_dist))
  #get the third caller idx
 third_caller_idx <- which(trial_select$label_name %in% calls_int &
                               trial_select$ID != trial_select$ID[second_caller_idx] &
                               trial_select$re_numeric >  trial_select$re_numeric[second_caller_idx])[1]
  #get the speaker distances of all individuals at the time of the second call
  all_distances_third <- trial_select[which(trial_select$re_numeric == trial_select$re_numeric[third_caller_idx]), "dist"]
  
  #get the distance ranking of the third caller
  all_distances_third <- sort(all_distances_third)
  dist_rank_third <- which(all_distances_third ==  trial_select$dist[third_caller_idx])[1]
  
  #get_first_caller location
  lat_3 <- trial_select$lat[third_caller_idx]
  lon_3 <- trial_select$lon[third_caller_idx]
  
  #get the distance from second caller
  prvs_cllr_dist_2 <- distm(c(trial_select$lat[third_caller_idx], trial_select$lon[third_caller_idx]), c(lat_2, lon_2), fun = distGeo) 
  
  #summarize third caller
  first_call_summary <- rbind(first_call_summary, c(trial_select$ID[third_caller_idx],
                                                    trial_select$dist[third_caller_idx],
                                                    dist_rank_third,
                                                    trial_select$re_numeric[third_caller_idx],
                                                    trial_select$trial[third_caller_idx],
                                                    trial_select$label_name[third_caller_idx], "third", prvs_cllr_dist_2))
  
  }
   
 
 colnames(first_call_summary) <- c("ID", "spk_dist", "dist_rank", "pbk_time", "trial", "call", "order", "prvs_cllr_dist")
 
 #plotting
 
 first_call_summary$order <- as.factor(c("first", "second", "third"))
 first_call_summary$dist_rank <- as.numeric(first_call_summary$dist_rank)
 first_call_summary$pbk_time <- as.numeric(first_call_summary$pbk_time)
 
 # Create a new column for jittered y-values
 first_call_summary <- first_call_summary %>%
   group_by(trial) %>%
   mutate(jittered_y = dist_rank + runif(n(), -0.2, 0.2)) 
 
 # Create the plot with connected lines for each trial
 ggplot(data = first_call_summary, aes(x = order, y = jittered_y, color = factor(trial), group = trial)) +
   geom_point() + 
   geom_line()
 
 #filter the data by
 # 1 - calls less than 30 sec after PB
 # 2 - responses less than 15 sec after previous
 
 
 first_call_summary_filt <- subset(first_call_summary, pbk_time < 30)
 
 # Create the plot with connected lines for each trial
signal_propagation <- ggplot(data = first_call_summary_filt, aes(x = order, y = jittered_y, color = factor(trial), group = trial)) +
     geom_point(size = 3.5) + 
     geom_line(linewidth = 1.5) + 
     labs(title = "Signal Propagation between Individuals after the Playback",
          x = "Caller Order",
          y = "Ranked Spatial Position of Individuals",
          color = "Trial") +
     theme_minimal() +
     theme(plot.background = element_rect(fill = "white", color = NA),
           panel.background = element_rect(fill = "white", color = NA),
           panel.grid.major = element_line(color = "gray60"),
           panel.grid.minor = element_line(color = "gray60"),
           plot.title = element_text(size = 16),
           axis.title.x = element_text(size = 14),
           axis.title.y = element_text(size = 14),
           axis.text.x = element_text(size = 12),
           axis.text.y = element_text(size = 12),
           legend.title = element_text(size = 14),
           legend.text = element_text(size = 12),
           legend.position = "right")

print(signal_propagation)

output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/NEW/"
# ggsave(filename = paste0(output_dir,"signal_propagation.png"), plot = signal_propagation, width = 12, height = 6)

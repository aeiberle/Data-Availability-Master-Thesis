#this script explores several movement metrics for analyzing recruitment events
#it uses the functions from COCOMO package and data imput in the format of movement matrices

#install_github('livingingroups/cocomo', force = T)
library(cocomo)
library(tidyverse)
library(reshape2)


setwd("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments")

#load matrices
load("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/meta_data/GPS_matrices.RData")

#### calculate dyadic distances ####
# for identifying the aggregation of the group as a response to playback
# but independent of the position of the playback speaker
distances <- get_group_dyadic_distances(xs, ys)
#get means
mean_dist <- apply(distances, 3, mean, na.rm = T)

#add date-times to the mead dyadic distances
meean_dist <- cbind(data.frame(timestamps), mean_dist)
meean_dist$date <- stringr::str_sub(meean_dist$timestamps, 1, 10)
meean_dist$numeric <- as.numeric(meean_dist$timestamps)

#remove NAs
meean_dist <- meean_dist[which(complete.cases(meean_dist)), ]
#plot
ggplot(data = meean_dist, aes(x = numeric, y = mean_dist))  + geom_smooth(method = "gam") + geom_jitter() +
  facet_wrap( ~ date, scales = "free")


#### get speed influence ####
#to identify dyadic influences in movement speed in responce to 
#the playback

#load ids
group_ids <- read.csv(paste(getwd(), "/meta_data/ind.csv", sep = ""))

#load dates
trial_dates <- read.csv(paste(getwd(), "/meta_data/Copy of PB_time_startend_info-copy.csv", sep = ""),
                        sep = ",")
trial_dates$dattime <- as.POSIXct(paste(trial_dates$Date, trial_dates$t0, sep = " "), tz = "UTC")

#set time windows before and after playback
buff_time <- 300

speed_summary <- data.frame()
pulls_before <- data.frame()
pulls_after <- data_frame()
#function to generate pairs of individuals
generate_pairs <- function(elements) {
  combn(elements, 2)
}


for (group in unique(group_ids$group)) {
  #get IDs for the current group
  curr_group_ids <- group_ids[which(group_ids$group == group), "id"]
  
  #fine the rown in the matrices that correspond to the IDs
  mat_rows <- which(ids$code %in% curr_group_ids)
  
  #save a vector of individuals that we have data for in this trial
  list_of_individuals <- ids$code[mat_rows]
  
  #get trial dates
  curr_group_dates <- trial_dates[which(trial_dates$Group == group), "dattime"]
  
  
  for (trial in curr_group_dates) {
    PB <- as.POSIXct(trial, tz = "UTC")
    start_t <- as.POSIXct(trial, tz = "UTC") - buff_time
    end_t <- as.POSIXct(trial, tz = "UTC") + buff_time
    
    mat_cols_start <- which(timestamps > start_t & timestamps < PB)
    mat_cols_end <- which(timestamps < end_t & timestamps > PB)
    
    influence_before <- get_turn_and_speed_influence_simplified(
      xs[mat_rows, mat_cols_start],
      ys[mat_rows, mat_cols_start],
      centroid = F,
      "temporal",
      spatial_R = 5,
      t_window = 5
    )
    influence_after <- get_turn_and_speed_influence_simplified(
      xs[mat_rows, mat_cols_end],
      ys[mat_rows, mat_cols_end],
      centroid = F,
      "temporal",
      spatial_R = 5,
      t_window = 5
    )
    #look at speed influence
    
    #format for easy plotting and add metadata
    before <- influence_before[["speed_influence_movement"]]
    row.names(before) <-  list_of_individuals
    before_long <- melt(before)
    before_long$time <- "before"
    before_long$group <- group
    before_long$date <- PB
    
    #format for easy plotting and add metedata
    after <- influence_after[["speed_influence_movement"]]
    row.names(after) <-  list_of_individuals
    after_long <- melt(after)
    after_long$time <- "after"
    after_long$group <- group
    after_long$date <- PB
    
    speed_summary <- rbind(speed_summary, before_long, after_long)

#### heading + speed (work in progress) ######
    
 #  get_heading_and_speed_spatial(x_i = xs[ind_combinations$V1[comb], c(mat_cols_start, mat_cols_end)], 
 #                               y_i = ys[ind_combinations$V1[comb], c(mat_cols_start, mat_cols_end)], 
 #                               t_idxs=1:length(xs[ind_combinations$V1[comb], mat_cols_end]),  R = 10,  forward = T, seconds_per_time_step = 2)
    
####pulls and anchors####

    #get events of successful pulls and anchors in responce to the playback
    
    #generate all possible pairs of individuals in the deployment
    ind_combinations <- as.data.frame(t(generate_pairs(mat_rows)))
    
    #calculate pulls nd ancors after the trial
    for (comb in 1:nrow(ind_combinations)) {
      #check for NAs
      A_Coordinates <- xs[ind_combinations$V1[comb], mat_cols_end]
      B_Coordinates <- xs[ind_combinations$V2[comb], mat_cols_end]
      
      if (length (which(!is.na(A_Coordinates))) < buff_time - 1 ||
          length (which(!is.na(B_Coordinates))) < buff_time - 1) {
        next
      }
      
      temp <- get_pulls_and_anchors(
        xa = xs[ind_combinations$V1[comb], mat_cols_end],
        xb = xs[ind_combinations$V2[comb], mat_cols_end],
        ya = ys[ind_combinations$V1[comb], mat_cols_end],
        yb = ys[ind_combinations$V2[comb], mat_cols_end],
        a = ids$code[ind_combinations$V1[comb]],
        b = ids$code[ind_combinations$V2[comb]],
        noise_thresh = 3,
        plot_results = F, include_final_fission = T
      )
      
      if (length(temp) == 0) {
        temp <- data.frame(matrix(ncol = 0, nrow = 1))
        temp$initiator <- ids$code[ind_combinations$V1[comb]]
        temp$responder <- ids$code[ind_combinations$V2[comb]]
        }
      
      temp$date <- PB
      pulls_after <- bind_rows(pulls_after, temp)
    }
   
    #calculate pulls nd ancors before the trial
    for (comb in 1:nrow(ind_combinations)) {
      #check for NAs
      A_Coordinates <- xs[ind_combinations$V1[comb], mat_cols_start]
      B_Coordinates <- xs[ind_combinations$V2[comb], mat_cols_start]
      
      if (length (which(!is.na(A_Coordinates))) < buff_time - 1 ||
          length (which(!is.na(B_Coordinates))) < buff_time - 1) {
        next
      }
      
      temp <- get_pulls_and_anchors(
        xa = xs[ind_combinations$V1[comb], mat_cols_start],
        xb = xs[ind_combinations$V2[comb], mat_cols_start],
        ya = ys[ind_combinations$V1[comb], mat_cols_start],
        yb = ys[ind_combinations$V2[comb], mat_cols_start],
        a = ids$code[ind_combinations$V1[comb]],
        b = ids$code[ind_combinations$V2[comb]],
        noise_thresh = 3,
        plot_results = F, include_final_fission = T
      )
      
      if (length(temp) == 0) {
        temp <- data.frame(matrix(ncol = 0, nrow = 1))
        temp$initiator <- ids$code[ind_combinations$V1[comb]]
        temp$responder <- ids$code[ind_combinations$V2[comb]]
      }
      
   #  else{
   #    
   #  xx <- xs[, mat_cols_start]
   #  yy <- ys[, mat_cols_start]
   #  #get info from the events table
   #  t1 <- temp$t1[1]
   #  t2 <- temp$t2[1]
   #  t3 <- temp$t3[1]
   #  event_type <- temp$type[1]
   #  initiator <- temp$initiator[1]
   #  responder <- temp$responder[1]
   #  if(is.character(initiator) | is.character(responder)){
   #   if(is.null(ids)){
   #   stop("Define 'ids' so the initiator and responder character strings can be matched with an index value.")
   #  }
   #  else{
   #  initiator <- which(ids==initiator)
   #  responder <- which(ids==responder)
   #  }
   #  }
   #  disparity <- temp$disparity[1]
   #  strength <- temp$strength[1]
   #  
   #  #get x and y data for plot
   #  x_l <- xx[initiator, t1:t3]
   #  y_l <- yy[initiator, t1:t3]
   #  x_f <- xx[responder, t1:t3]
   #  y_f <- yy[responder, t1:t3]
   #  cols_l <- topo.colors(length(x_l))
   #  cols_f <- heat.colors(length(x_f))
   #  
   #  #get x and y boundaries
   #  xmin <- min(c(x_l, x_f), na.rm=T)
   #  xmax <- max(c(x_l, x_f), na.rm=T)
   #  ymin <- min(c(y_l, y_f), na.rm=T)
   #  ymax <- max(c(y_l, y_f), na.rm=T)
   #  
   #  par(mfrow=c(1,3))
   #  #t1
   #  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = event_type,xlab='x',ylab='y')
   #  lines(x_l, y_l, col = 'lightblue')
   #  lines(x_f, y_f, col = 'lightsalmon')
   #  points(xx[initiator,t1], yy[initiator,t1], col = 'darkblue', pch = 19)
   #  points(xx[responder,t1], yy[responder,t1], col = 'darkred', pch = 19)
   #  
   #  #t2
   #  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = paste('disp =', round(disparity, digits=2)),xlab='x',ylab='y')
   #  lines(x_l, y_l, col = 'lightblue')
   #  lines(x_f, y_f, col = 'lightsalmon')
   #  points(xx[initiator,t2], yy[initiator,t2], col = 'darkblue', pch = 19)
   #  points(xx[responder,t2], yy[responder,t2], col = 'darkred', pch = 19)
   #  
   #  #t3
   #  plot(NULL, xlim = c(xmin, xmax), ylim = c(ymin,ymax), asp = 1, main = paste('strength =',round(strength, digits=2)),xlab='x',ylab='y')
   #  lines(x_l, y_l, col = 'lightblue')
   #  lines(x_f, y_f, col = 'lightsalmon')
   #  points(xx[initiator,t3], yy[initiator,t3], col = 'darkblue', pch = 19)
   #  points(xx[responder,t3], yy[responder,t3], col = 'darkred', pch = 19)     
   #    
   #    
   #    
   #  }
   #  
      
      temp$date <- PB
      pulls_before <- bind_rows(pulls_before, temp)
    }
     
  }
  
}

##Adding group composition columns to pulls_before and pulls_after (initiator/responder_sex, initiator/responder_age, initiator/responder_dominance)
group_composition <- read.csv("/mnt/EAS_ind/aeiberle/data/GroupComposition_complete.csv", sep = ";")

pulls_before$initiator_sex <- NA
pulls_before$initiator_age <- NA
pulls_before$initiator_dominance <- NA
pulls_before$responder_sex <- NA
pulls_before$responder_age <- NA
pulls_before$responder_dominance <- NA

pulls_after$initiator_sex <- NA
pulls_after$initiator_age <- NA
pulls_after$initiator_dominance <- NA
pulls_after$responder_sex <- NA
pulls_after$responder_age <- NA
pulls_after$responder_dominance <- NA


# Join columns "Sex", "Age" and "Dominance" from group composition df to pulls_before and pulls_after in corresponding columns based on ID.
pulls_before$initiator_sex <- group_composition$Sex[match(pulls_before$initiator, group_composition$ID)]
pulls_before$initiator_age <- group_composition$Age[match(pulls_before$initiator, group_composition$ID)]
pulls_before$initiator_dominance <- group_composition$Dominance[match(pulls_before$initiator, group_composition$ID)]

pulls_before$responder_sex <- group_composition$Sex[match(pulls_before$responder, group_composition$ID)]
pulls_before$responder_age <- group_composition$Age[match(pulls_before$responder, group_composition$ID)]
pulls_before$responder_dominance <- group_composition$Dominance[match(pulls_before$responder, group_composition$ID)]


pulls_after$initiator_sex <- group_composition$Sex[match(pulls_after$initiator, group_composition$ID)]
pulls_after$initiator_age <- group_composition$Age[match(pulls_after$initiator, group_composition$ID)]
pulls_after$initiator_dominance <- group_composition$Dominance[match(pulls_after$initiator, group_composition$ID)]

pulls_after$responder_sex <- group_composition$Sex[match(pulls_after$responder, group_composition$ID)]
pulls_after$responder_age <- group_composition$Age[match(pulls_after$responder, group_composition$ID)]
pulls_after$responder_dominance <- group_composition$Dominance[match(pulls_after$responder, group_composition$ID)]


#### plotting
#plot speed influence summaries
#group_composition <- read.csv("/mnt/EAS_ind/aeiberle/data/GroupComposition_complete.csv", sep = ";")

for (i in 1:nrow(speed_summary)) {
  speed_summary$sex[i] <- group_composition[which(group_composition$ID == speed_summary$Var1[i])[1], "Sex"]
  speed_summary$status[i] <- group_composition[which(group_composition$ID == speed_summary$Var1[i])[1], "Position"]
  
}


speed_summary$status_dom <-  speed_summary$status
for (i in 1:nrow(speed_summary)) {
  if (speed_summary$status_dom[i] == "Dominant")
  {
    speed_summary$status_dom[i] <- paste(speed_summary$status[i], speed_summary$sex[i])
  }
  
}

ggplot(data = speed_summary, aes(x = status_dom, y = value)) + geom_boxplot(aes(fill = time))



#### Recruitment Time #####
#change playback time (t0) to numeric time
trial_dates$numeric_PB <- as.numeric(trial_dates$dattime)

# Define thresholds for each date
thresholds <- data.frame(
  date = as.Date(trial_dates$Date),
  min_timestamps = as.POSIXct(trial_dates$dattime),
  numeric_PB = as.numeric(trial_dates$numeric_PB)
)

# Filter rows and remove rows before threshold on each date
filtered_df <- meean_dist %>%
  mutate(date = as.Date(timestamps)) %>% 
  left_join(thresholds, by = "date") %>% 
  filter(timestamps >= min_timestamps)


# Load required library
library(dplyr)

# Create recruit_df
recruit_df <- filtered_df %>%
  group_by(date) %>%
  slice(c(1, which.min(mean_dist))) %>% # Select the first row and the row with the minimum mean_dist
  ungroup()

# Get unique dates
unique_dates <- unique(recruit_df$date)

# Initialize recruit_time column
recruit_df$recruit_time <- NA

# Loop over each date to calculate recruit_time
for (date in unique_dates) {
  # Subset rows for the current date
  rows <- which(recruit_df$date == date)
  
  # Calculate recruit_time for the second row by subtracting numeric from numeric_PB
  recruit_df$recruit_time[rows[1]] <- "START"
  recruit_df$recruit_time[rows[2]] <- recruit_df$numeric[rows[2]] - recruit_df$numeric_PB[rows[2]]
}


metadata <- read.csv("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments/meta_data/time_startend_info-copy.csv")
metadata$track <- c("PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_6", "PB_3",
                    "PB_3", "PB_6",
                    "PB_6", "PB_3")
metadata$trial <- paste(metadata$Date, metadata$Group)


# Match rows from metadata to recruit_df based on the Date column
matching_rows <- match(recruit_df$date, metadata$Date)

# Add specific columns from metadata to recruit_df
recruit_df$trial <- metadata$trial[matching_rows]
recruit_df$track <- metadata$track[matching_rows]



##Counts of successful "pulls_after" 
#looking if Dominance is the key factor:

library(dplyr)

# Filter for successful pulls where type == "pull"
successful_pulls_after <- pulls_after %>%
  filter(type == "pull")

# Count initiator and responder dominance for successful pulls
dominance_counts_after <- successful_pulls_after %>%
  count(initiator_dominance, responder_dominance)

print(dominance_counts_after)

##Heatmap Dominance vs Dominance
library(ggplot2)

dominance_heatmap_after <- ggplot(dominance_counts_after, aes(x = initiator_dominance, y = responder_dominance, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Successful Pulls (after PB) by Dominance Levels",
    x = "Dominance Status of Initiator",
    y = "Dominance Status of Responder",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#print(dominance_heatmap_after)


##Heatmap Age vs Age
# Count initiator and responder age for successful pulls
age_counts_after <- successful_pulls_after %>%
  count(initiator_age, responder_age)

print(age_counts_after)

age_heatmap_after <- ggplot(age_counts_after, aes(x = initiator_age, y = responder_age, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Successful Pulls (after PB) by Age Levels",
    x = "Age Status of Initiator",
    y = "Age Status of Responder",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#print(age_heatmap_after)

##Heatmap Sex vs Sex
# Count initiator and responder sex for successful pulls
sex_counts_after <- successful_pulls_after %>%
  count(initiator_sex, responder_sex)

#print(sex_counts_after)

sex_heatmap_after <- ggplot(sex_counts_after, aes(x = initiator_sex, y = responder_sex, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Successful Pulls (after PB) by Sex Levels",
    x = "Sex Status of Initiator",
    y = "Sex Status of Responder",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#print(sex_heatmap_after)

library(gridExtra)
grid.arrange(dominance_heatmap_after, age_heatmap_after, sex_heatmap_after, nrow = 1)



##Counts of successful "pulls_before" 
#looking if Dominance is the key factor:

successful_pulls_before <- pulls_before %>%
  filter(type == "pull")

# Count initiator and responder dominance for successful pulls
dominance_counts_before <- successful_pulls_before %>%
  count(initiator_dominance, responder_dominance)

print(dominance_counts_before)

##Heatmap Dominance vs Dominance
dominance_heatmap_before <- ggplot(dominance_counts_before, aes(x = initiator_dominance, y = responder_dominance, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Successful Pulls (before PB) by Dominance Levels",
    x = "Dominance Status of Initiator",
    y = "Dominance Status of Responder",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#print(dominance_heatmap_before)


##Heatmap Age vs Age
# Count initiator and responder age for successful pulls
age_counts_before <- successful_pulls_before %>%
  count(initiator_age, responder_age)

print(age_counts_before)

age_heatmap_before <- ggplot(age_counts_before, aes(x = initiator_age, y = responder_age, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Successful Pulls (before PB) by Age Levels",
    x = "Age Status of Initiator",
    y = "Age Status of Responder",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#print(age_heatmap_before)

##Heatmap Sex vs Sex
# Count initiator and responder sex for successful pulls
sex_counts_before <- successful_pulls_before %>%
  count(initiator_sex, responder_sex)

print(sex_counts_before)

sex_heatmap_before <- ggplot(sex_counts_before, aes(x = initiator_sex, y = responder_sex, fill = n)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue") +
  labs(
    title = "Heatmap of Successful Pulls (before PB) by Sex Levels",
    x = "Sex Status of Initiator",
    y = "Sex Status of Responder",
    fill = "Count"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

#print(sex_heatmap_before)

grid.arrange(dominance_heatmap_before, age_heatmap_before, sex_heatmap_before, nrow = 1)


##Counts in relation to sample sizes
samplesizes <- read.csv("/mnt/EAS_ind/aeiberle/data/SampleSizes_new_GPSData.csv", sep = ";")

# Calculate the total number of dominant and subordinate individuals across all groups
total_dominant <- sum(samplesizes$num_dominant)
total_subordinate = sum(samplesizes$num_adult + samplesizes$num_subadult + samplesizes$num_yearling)

print(total_dominant)
print(total_subordinate)


# Calculate pull rates
dominance_counts_after <- dominance_counts_after %>%
  mutate(
    pull_rate = case_when(
      initiator_dominance == "Dominant" & responder_dominance == "Dominant" ~ n / total_dominant,
      initiator_dominance == "Dominant" & responder_dominance == "Subordinate" ~ n / total_subordinate,
      initiator_dominance == "Subordinate" & responder_dominance == "Dominant" ~ n / total_dominant,
      initiator_dominance == "Subordinate" & responder_dominance == "Subordinate" ~ n / total_subordinate
    )
  )

print(dominance_counts_after)

ggplot(dominance_counts_after, aes(x = initiator_dominance, y = responder_dominance, fill = pull_rate)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "blue", labels = scales::percent) +
  geom_text(aes(label = scales::percent(pull_rate)), color = "black", size = 4) + 
  labs(
    title = "Heatmap of Pull Rates by Dominance Status",
    x = "Initiator Dominance",
    y = "Responder Dominance",
    fill = "Pull Rate"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid = element_blank()
  )

# Count unique initiators for each group
unique_initiators <- successful_pulls_after %>%
  distinct(initiator, initiator_dominance) %>%
  count(initiator_dominance)

print(unique_initiators)


# ####
# # Create a contingency table of observed counts for initiator and responder dominance
# contingency_table <- table(successful_pulls_after$initiator_dominance, successful_pulls_after$responder_dominance)
# 
# # View the table
# print(contingency_table)
# 
# # Perform the Chi-square test
# chi_square_test <- chisq.test(contingency_table)
# 
# # View the test results
# print(chi_square_test)
# 
# 
# # Create a matrix of observed counts
# observed_matrix <- table(successful_pulls_after$initiator_dominance, successful_pulls_after$responder_dominance)
# 
# # Calculate row and column totals
# row_totals <- apply(observed_matrix, 1, sum)
# col_totals <- apply(observed_matrix, 2, sum)
# grand_total <- sum(observed_matrix)
# 
# # Calculate expected counts
# expected_matrix <- outer(row_totals, col_totals, FUN = "*") / grand_total
# 
# # Normalize the matrix by dividing observed by expected
# normalized_matrix <- observed_matrix / expected_matrix
# 
# # View normalized matrix
# print(normalized_matrix)


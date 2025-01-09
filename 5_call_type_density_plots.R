# Plotting the occurrence of each call type in the form of density ridgeline plots
# The plot should have all call types on the y-axis plotted over the time, ideally having to vertical lines showing the Playback duration
# For each individual on a give trial/day and entire trial/day --> only audio needed
# Adding group composition information to each individual especially the plots with all individuals in it

# Load libraries
library(ggplot2)
#install.packages("ggridges")
library(ggridges)
library(dplyr)
library(lubridate)
library(RColorBrewer)
library(gridExtra)


output_dir <- "/mnt/EAS_ind/aeiberle/data/Plots/NEW/"

# Read in the CSV file of audio data
audio_df_orig <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/labels_synched_2023_preGD.csv", header = TRUE, sep = ",")

# Read in the CSV file of group composition data of each individual
composition_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/GroupComposition_complete.csv", header = TRUE, sep = ";")

composition_df$ID <- as.character(composition_df$ID)
composition_df$Position <- as.character(composition_df$Position)
composition_df$Age <- as.character(composition_df$Age)
composition_df$Dominance <- as.character(composition_df$Dominance)
composition_df$Sex <- as.character(composition_df$Sex)

# Convert 'date' column to Date format and t0_UTC to POSIXct
audio_df_orig <- audio_df_orig %>%
  mutate(date = as.Date(as.character(date), format = "%Y%m%d"),
         t0_UTC = as.POSIXct(t0_UTC, format = "%Y-%m-%d %H:%M:%S"),
         tf_UTC = as.POSIXct(tf_UTC, format = "%Y-%m-%d %H:%M:%S"))
audio_df_orig$t0_UTCseconds <- as.numeric(audio_df_orig$t0_UTC)
audio_df_orig$tf_UTCseconds <- as.numeric(audio_df_orig$tf_UTC)

# Merge data frames based only on rows present in audio_df_orig
audio_df <- merge(audio_df_orig, composition_df, 
                   by.x = c("id_code", "date"), 
                   by.y = c("ID", "Date"), 
                   all.x = TRUE, 
                   all.y = FALSE)

# Get the original columns of audio_df_orig and append the new columns from composition_df
audio_df <- audio_df[, c(names(audio_df_orig), "Position", "Age", "Dominance", "Sex")]

# Load the playback times CSV (replace 'path_to_playback.csv' with the actual file path)
playback_df <- read.csv(file = "/mnt/EAS_ind/aeiberle/data/PB_time_startend_info.csv", header = TRUE, sep = ";")

# Combine Date and time (t0, t1) into proper POSIXct format
playback_df <- playback_df %>%
  mutate(Date = as.Date(as.character(Date), format = "%Y-%m-%d"),
         t0 = as.POSIXct(paste(Date, t0), format = "%Y-%m-%d %H:%M:%S"),
         t1 = as.POSIXct(paste(Date, t1), format = "%Y-%m-%d %H:%M:%S"))
playback_df$t0seconds <- as.numeric(playback_df$t0)
playback_df$t1seconds <- as.numeric(playback_df$t1)

# Define the specific dates to filter
specific_dates <- as.Date(c("2023-06-15", "2023-07-01", "2023-06-24", "2023-07-16", "2023-07-20", "2023-07-30"))

# Filter the data for specific dates
audio_day <- audio_df %>%
  filter(date %in% specific_dates)

# Merge the audio data with the playback times based on the 'date' column
audio_day <- audio_df %>%
  left_join(playback_df, by = c("date" = "Date"))

audio_day$t0_timediff <- audio_day$t0_UTCseconds - audio_day$t0seconds
audio_day$tf_timediff <- audio_day$tf_UTCseconds - audio_day$t1seconds

# Filter data for time differences within -600 to +600 seconds
audio_day <- audio_day %>%
  filter(t0_timediff >= -600 & t0_timediff <= 600)

# Define colors for each call type and generating a palette
call_types <- unique(audio_day$label_name)
num_call_types <- length(call_types)
density_palette_calls <- c("#FF7F00", "#A65628", "#E41A1C", "#FFFF33", "#377EB8",   
                           "#984EA3", "#4DAF4A", "#F781BF", "#11999E")
color_mapping_calls <- setNames(density_palette_calls, call_types)

# Define colors for each social category and generating a palette
social_categories <- unique(audio_day$Position)
num_social_categories <- length(social_categories)
density_palette_pos <- c("#E41A1C", "#FFD422", "#658E67", "#F781BF")
color_mapping_pos <- setNames(density_palette_pos, social_categories)

# Define colors for each age and generating a palette
ages <- unique(audio_day$Age)
num_ages <- length(ages)


#### LOOP: Ridgeline plot for each date with color-coded id_codes ####
# Loop through each specific date
for (specific_date in specific_dates) {
  
  specific_date <- as.Date(specific_date)
  
  # Filter the data for the specific date
  audio_day_combined <- audio_day %>%
    filter(date == specific_date) %>%
    filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track")
  
  # Check if there is data for the specific date
  if (nrow(audio_day_combined) > 0) {
    # Generate the ridgeline plot for the specific date
    combined_density_plot <- ggplot(audio_day_combined, aes(x = t0_UTC, y = label_name, fill = label_name)) +
      geom_density_ridges(alpha = 1, scale = 0.7, show.legend = FALSE) +
      geom_point(aes(x = t0_UTC, y = label_name), size = 1.5, alpha = 1,
                 position = position_jitter(width = 0, height = 0)) +
      geom_vline(aes(xintercept = as.numeric(t0)), linetype = "dashed", color = "red", linewidth = 1) +  # Playback start
      geom_vline(aes(xintercept = as.numeric(t1)), linetype = "dashed", color = "red", linewidth = 1) +  # Playback end
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 min") +
      scale_fill_manual(values = color_mapping_calls, guide = "none") +
      labs(title = paste("Density Ridgeline Plot of Call Types for", specific_date, "with Playback Duration"),
           x = "Time",
           y = "Call Type") +
      theme_ridges() +
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
            legend.text = element_text(size = 12),
            legend.position = "right")
    
    # Print the plot
    print(combined_density_plot)
    
    # Save the plot for the specific date/trial
    # ggsave(filename = paste0(output_dir, gsub("-", "", specific_date), "_density_calls_plot_combined_600s.png"), plot = combined_density_plot, width = 18, height = 9)
    
  }
}


#### LOOP: Ridgeline plot for each date with color-coded id_codes and shapes for social categories ####
for (specific_date in specific_dates) {
  
  specific_date <- as.Date(specific_date)
  
  # Filter the data for the specific date
  audio_day_combined <- audio_day %>%
    filter(date == specific_date) %>%
    filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track")
  
  # Check if there is data for the specific date
  if (nrow(audio_day_combined) > 0) {
    # Generate the ridgeline plot for the specific date
    combined_density_position_plot <- ggplot(audio_day_combined, aes(x = t0_UTC, y = label_name, fill = label_name)) +
      geom_density_ridges(alpha = 1, scale = 0.7, show.legend = FALSE) +
      geom_point(aes(x = t0_UTC, y = label_name), size = 1.5, alpha = 1, 
                 position = position_jitter(width = 0, height = 0)) +
      geom_vline(aes(xintercept = as.numeric(t0)), linetype = "dashed", color = "red", linewidth = 1) +  # Playback start
      geom_vline(aes(xintercept = as.numeric(t1)), linetype = "dashed", color = "red", linewidth = 1) +  # Playback end
      scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 min") +
      scale_fill_manual(values = color_mapping_calls, guide = "none") +
      labs(title = paste("Density Ridgeline Plot of Call Types for", specific_date, "with Playback Duration"),
           x = "Time",
           y = "Call Type") +
      theme_ridges() +
      theme_minimal() +
      theme(plot.background = element_rect(fill = "white", color = NA),
            panel.background = element_rect(fill = "white", color = NA),
            panel.grid.major = element_line(color = "gray80"),
            panel.grid.minor = element_line(color = "gray90"),
            plot.title = element_text(size = 16),
            axis.title.x = element_text(size = 16),
            axis.title.y = element_text(size = 16),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            legend.title = element_text(size = 16),
            legend.text = element_text(size = 14),
            legend.position = "right")
    
    # Print the plot
    print(combined_density_position_plot)
    
    # Save the plot for the specific date/trial
    # ggsave(filename = paste0(output_dir, gsub("-", "", specific_date), "_density_calls_position_plot_combined_600s.png"), plot = combined_density_position_plot, width = 18, height = 9)
    
  }
}


#### LOOP: Ridgeline plot for each individual on a date ####
# Loop through each specific date
for (current_date in specific_dates) {
  
  current_date <- as.Date(current_date)
  
  # Filter the data for the current date
  audio_day_individual <- audio_day %>%
    filter(date == current_date) %>%
    filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track")
  
  unique_individuals <- unique(audio_day_individual$id_code)
  
  # Loop through each individual
  for (current_individual in unique_individuals) {
    # Exclude the specific individual
    if (current_individual == "VSIM021") {
      next  # Skip individual
    }
    
    # Filter the data for the specified individual and current date
    individual_data <- audio_day_individual %>%
      filter(id_code == current_individual)
    
    # Proceed only if there is data for the individual on the given date
    if (nrow(individual_data) > 0) {
      # Generate the ridgeline plot
      individual_density_plot <- ggplot(individual_data, aes(x = t0_UTC, y = label_name, fill = label_name)) +
        geom_density_ridges(alpha = 0.7, scale = 0.5, show.legend = FALSE) +
        geom_point(aes(x = t0_UTC, y = label_name), color = "black", size = 1.5, alpha = 1,
                   position = position_jitter(width = 0, height = 0)) +
        geom_vline(aes(xintercept = as.numeric(t0)), linetype = "dashed", color = "red", linewidth = 1) +  # Playback start
        geom_vline(aes(xintercept = as.numeric(t1)), linetype = "dashed", color = "red", linewidth = 1) +  # Playback end
        scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 min") +
        scale_fill_manual(values = color_mapping_calls, guide = "none") +
        labs(title = paste("Density Ridgeline Plot of Call Types for", current_individual, "on", format(current_date, "%Y-%m-%d")),
             x = "Time",
             y = "Call Type") +
        theme_ridges() +
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
              legend.position = "none")
      
      # Print the plot
      print(individual_density_plot)
      
      # Save the plot for the specific individual on the date/trial
      # ggsave(filename = paste0(output_dir, format(current_date, "%Y%m%d"), "_density_plot_600s_", current_individual, ".png"), plot = individual_density_plot, width = 18, height = 9)
      
    }
  }
}


#### Ridgeline plot for "ld" calls across social categories ####
# Filter the data for the call type "ld"
audio_combined_ld <- audio_day %>%
  filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track") %>%
  filter(label_name == "ld")

# Check if there is data for the call type "ld"
if (nrow(audio_combined_ld) > 0) {
  
  # Generate the ridgeline plot for all data of call type "ld"
  combined_density_lead_plot <- ggplot(audio_combined_ld, aes(x = t0_timediff, y = Position, fill = Position)) +  # y = Position
    geom_density_ridges(alpha = 1, scale = 0.8, show.legend = FALSE, rel_min_height = 0.005, bandwidth = 10) +
    geom_point(aes(x = t0_timediff, y = Position), size = 1.5, alpha = 1,
               position = position_jitter(width = 0, height = 0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
    #scale_x_datetime(date_labels = "%H:%M", date_breaks = "5 min") +
    scale_fill_manual(values = color_mapping_pos, guide = "none") +
    labs(title = "Distribution of 'lead' calls across Social Categories",
         x = "Time Difference to Playback Start [s]",
         y = "Social Category") +
    theme_ridges() +
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
  
  # Print the plot
  print(combined_density_lead_plot)
  
  # Save the plot for the call type "ld"
  # ggsave(filename = paste0(output_dir, "density_calls_lead_social_categories_plot_600s.png"), plot = combined_density_lead_plot, width = 18, height = 9)
  
}


#### Ridgeline plot for all call_types across social categories ####
# Filter the data to remove Recruitment Playback Track
audio_combined <- audio_day %>%
  filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track")

if (nrow(audio_combined) > 0) {
  
  # Generate the ridgeline plot for all call types for social categories
  combined_density_social_categories_plot <- ggplot(audio_combined, aes(x = t0_timediff, y = Position, fill = Position)) +
    geom_density_ridges(alpha = 1, scale = 0.8, show.legend = FALSE, rel_min_height = 0.005, bandwidth = 10) +
    geom_point(aes(x = t0_timediff, y = Position), size = 1.5, alpha = 1,
               position = position_jitter(width = 0, height = 0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
    scale_fill_manual(values = color_mapping_pos, guide = "none") +
    labs(title = "Distribution of Call Types across Social Categories for all Trials",
         x = "Time Difference to Playback Start [s]",
         y = "Social Category") +
    theme_ridges() +
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
  
  # Print the plot
  print(combined_density_social_categories_plot)
  
  # Save the plot for the call type "ld"
  # ggsave(filename = paste0(output_dir, "density_calls_social_categories_plot_600s.png"), plot = combined_density_social_categories_plot, width = 18, height = 9)
  
}


#### Ridgeline plot for all call_types across sexes ####
# Filter data to remove "Recruitment Playback Track" and separate by sex in one step
audio_male <- audio_day %>%
  filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track" & Sex == "male")

audio_female <- audio_day %>%
  filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track" & Sex == "female")


# Generate the ridgeline plot for Male
male_density_plot <- ggplot(audio_male, aes(x = t0_timediff, y = label_name, fill = label_name)) +
  geom_density_ridges(alpha = 1, scale = 0.8, show.legend = FALSE, rel_min_height = 0.005, bandwidth = 10) +
  geom_point(aes(x = t0_timediff, y = label_name), size = 1.5, alpha = 1,
             position = position_jitter(width = 0, height = 0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(values = color_mapping_calls, guide = "none") +
  labs(title = "Distribution of Call Types in Males across all Trials",
       x = "Time Difference to Playback Start [s]",
       y = "Call Type") +
  theme_ridges() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

# Generate the ridgeline plot for Female
female_density_plot <- ggplot(audio_female, aes(x = t0_timediff, y = label_name, fill = label_name)) +
  geom_density_ridges(alpha = 1, scale = 0.8, show.legend = FALSE, rel_min_height = 0.005, bandwidth = 10) +
  geom_point(aes(x = t0_timediff, y = label_name), size = 1.5, alpha = 1,
             position = position_jitter(width = 0, height = 0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
  geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
  scale_fill_manual(values = color_mapping_calls, guide = "none") +
  labs(title = "Distribution of Call Types in Females across all Trials",
       x = "Time Difference to Playback Start [s]",
       y = "Call Type") +
  theme_ridges() +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        panel.background = element_rect(fill = "white", color = NA),
        panel.grid.major = element_line(color = "grey90"),
        panel.grid.minor = element_line(color = "grey80"),
        plot.title = element_text(size = 16),
        axis.title.x = element_text(size = 16),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14))

# Arrange the Male and Female plots side-by-side in a grid
combined_sex_plot <- grid.arrange(male_density_plot, female_density_plot, nrow = 1)

# Optionally save each plot or the combined grid
# ggsave(filename = paste0(output_dir, "density_calls_sex_male_plot_600s.png"), plot = male_density_plot, width = 18, height = 9)
# ggsave(filename = paste0(output_dir, "density_calls_sex_female_plot_600s.png"), plot = female_density_plot, width = 18, height = 9)
# ggsave(filename = paste0(output_dir, "density_calls_sexes_plot_600s.png"), plot = combined_sex_plot, width = 18, height = 9)


#### Ridgeline plot for all call_types of all individuals ####
# Filter the data to remove Recruitment Playback Track
audio_combined_calls_total <- audio_day %>%
  filter(!is.na(label_name) & label_name != "" & label_name != "Recruitment Playback Track")

if (nrow(audio_combined_calls_total) > 0) {
 
  # Generate the ridgeline plot for all data of call types
  combined_density_calls_total_plot <- ggplot(audio_combined_calls_total, aes(x = t0_timediff, y = label_name, fill = label_name)) +
    geom_density_ridges(alpha = 1, scale = 0.8, show.legend = FALSE, rel_min_height = 0.005, bandwidth = 10) +
    geom_point(aes(x = t0_timediff, y = label_name), size = 1.5, alpha = 1,
               position = position_jitter(width = 0, height = 0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 1) +
    geom_vline(xintercept = 10, linetype = "dashed", color = "red", linewidth = 1) +
    scale_fill_manual(values = color_mapping_calls, guide = "none") +
    labs(title = "Distribution of Call Types across all Trials",
         x = "Time Difference to Playback Start [s]",
         y = "Call Type") +
    theme_ridges() +
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
  
  # Print the plot
  print(combined_density_calls_total_plot)
  
  # Save the plot
  # ggsave(filename = paste0(output_dir, "density_calls_total_plot_600s.png"), plot = combined_density_calls_total_plot, width = 18, height = 9)
  
}

# # Counts of calls on position
# table(audio_day$label_name, audio_day$Position)
# table(audio_day$Position, audio_day$id_code)

#analyse the pulls and anchor data

#we transform the events table into matrices and count each pull as 1 point
#  of influence. Fission is -1 point of influence
# anchors are counted as sequential fission and pull. So the
# initiator is getting -1  and the responded +1

load("pulls&anchors.Rdata")


library(tidyverse)
library(igraph)
library(tidygraph)
library(reshape2)
library(ggpubr)
library(rstatix)

pulls_after$phase <- "after"
pulls_before$phase <- "before"

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

#combine all data together
all_pulls_tot <- rbind(pulls_after, pulls_before)

#check how many events we have per trial
event_counts <- as.data.frame(table(all_pulls_tot$date))

#remove trials with less than 5 events
high_count_trials <- event_counts[which(table(all_pulls_tot$date) > 5), 1]
all_pulls_tot <- subset(all_pulls_tot, date  %in% high_count_trials)

head(all_pulls_tot)

library(tidyverse)

# Create a list to store matrices for each date
influence_matrices <- list()

# Get unique dates
unique_dates <- unique(all_pulls_tot$date)

# Iterate over unique dates
for (day in unique_dates) {
  # Filter data for the current date
  date_data_all <- subset(all_pulls_tot, date == day)
     
  for (condition in unique(date_data_all$phase)) {
    date_data <- subset(date_data_all, phase == condition)
  # Get unique individuals and sort alphabetically
  individuals <- sort(unique(c(date_data$initiator, date_data$responder)))
  
  # Create an empty matrix with individuals as rows and columns, initialize with NA
  matrix <- matrix(NA, nrow = length(individuals), ncol = length(individuals), dimnames = list(individuals, individuals))
  
  # Update scores based on events
  for (i in 1:nrow(date_data)) {
    initiator <- date_data$initiator[i]
    responder <- date_data$responder[i]
    type <- date_data$type[i]
    
    if (is.na(type) || is.na(initiator) || is.na(responder) || initiator == responder) {
      next # Skip if any value is NA or initiator and responder are the same
    }
    
    if (type == "pull") {
      if (is.na(matrix[initiator, responder])) {
        matrix[initiator, responder] <- 0
      }
      matrix[initiator, responder] <- matrix[initiator, responder] + 1
    } else if (type == "fission") {
      if (is.na(matrix[initiator, responder])) {
        matrix[initiator, responder] <- 0
      }
      matrix[initiator, responder] <- matrix[initiator, responder] - 1
    } else if (type == "anchor") {
      if (is.na(matrix[initiator, responder])) {
        matrix[initiator, responder] <- 0
      }
      matrix[initiator, responder] <- matrix[initiator, responder] - 1
      if (is.na(matrix[responder, initiator])) {
        matrix[responder, initiator] <- 0
      }
      matrix[responder, initiator] <- matrix[responder, initiator] + 1
    }
  }
  #make sure all values are positive
  matrix <- matrix+10
##  # Normalize the matrix by row (individual influence)
##  for (row_index in 1:nrow(matrix)) {
##    row_sum <- sum(matrix[row_index, ], na.rm = TRUE) #Sum of the row ignoring NAs
##    if (row_sum != 0) { #Avoid division by 0
##      matrix[row_index, ] <- matrix[row_index, ] / row_sum
##    }
##  }
  
     #normalization_method == "mean" (global influence)
    matrix_mean <- mean(matrix, na.rm = TRUE)
    matrix_sd <- sd(matrix, na.rm = TRUE)
    if (matrix_sd != 0) {
      matrix <- (matrix - matrix_mean) / matrix_sd}
  
  # Add the matrix to the list
  influence_matrices[[paste(as.character(day), condition)]] <- matrix
}
 }

# Plotting Heatmaps using ggplot2
for (date in names(influence_matrices)) {
  matrix_data <- influence_matrices[[date]]
  
  # Melt the matrix for ggplot2
  melted_matrix <- melt(matrix_data, na.rm = TRUE)
  colnames(melted_matrix) <- c("Initiator", "Responder", "Value")
  
  # Calculate the median of the non-NA values
  matrix_median <- mean(matrix_data, na.rm = TRUE)
  
  # Create the heatmap
  heatmap <- ggplot(melted_matrix, aes(x = Responder, y = Initiator, fill = Value)) +
    geom_tile() +
    scale_fill_gradient2(low = "blue", mid = "yellow", high = "red", midpoint =  matrix_median, na.value = "grey") + # Color scale
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(face="bold"),
          plot.title = element_text(face="bold")) + # Rotate x-axis labels
    labs(title = paste("Influence Matrix for", date), x = "Responder", y = "Initiator") +
    coord_equal()
  
  print(heatmap) # Print the plot for each date
}


# Create data frame with batch and condition extracted
combined_df <- data.frame()
for (matrix_name in names(influence_matrices)) {
  batch <- str_extract(matrix_name, "^[0-9]+") # Extract batch number
  condition <- str_extract(matrix_name, "(before|after)$") # Extract condition
  df <- data.frame(value = as.vector(influence_matrices[[matrix_name]]), batch = batch, condition = condition)
  combined_df <- rbind(combined_df, df)
}


#Convert batch to factor for correct plotting order
combined_df$batch <- factor(combined_df$batch)
combined_df$condition <- factor(combined_df$condition, levels = c("before", "after")) #Order the conditions

# Plot distribution of influences before and after
ggplot(combined_df, aes(x = value, color = condition, fill = condition)) +
  geom_density(alpha = 0.5) +
  #facet_wrap(~ batch, scales = "free") + # Facet by batch
  labs(title = "Density Plots by Condition",
       x = "Value",
       y = "Density",
       color = "Condition",
       fill = "Condition") +
  scale_fill_manual(values=c("#F8766D", "#00BFC4")) + #Set colors for the fill
  scale_color_manual(values=c("#F8766D", "#00BFC4")) + #Set colors for the lines
  theme_bw()



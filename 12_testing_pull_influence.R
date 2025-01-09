## Analysis of the pulls and anchor data

# Transform the events table into matrices and count each pull as 1 point of influence. 
# Fission is -1 point of influence
# Anchors are counted as sequential fission and pull
# Initiator is getting -1 and the responded +1

load("/mnt/EAS_ind/aeiberle/data/RDataFiles/pulls&anchors.Rdata")

library(tidyverse)
library(igraph)
library(tidygraph)
library(reshape2)
library(ggpubr)
library(rstatix)
library(glmmTMB)
library(emmeans)
library(DHARMa)
library(sjPlot)
library(performance)
library(gridExtra)

pulls_after$phase <- "after"
pulls_before$phase <- "before"

#combine all data together
all_pulls_tot <- rbind(pulls_after, pulls_before)

#check how many events we have per trial
event_counts <- as.data.frame(table(all_pulls_tot$date))

#remove trials with less than 5 events
high_count_trials <- event_counts[which(table(all_pulls_tot$date) > 5), 1]
all_pulls_tot <- subset(all_pulls_tot, date  %in% high_count_trials)

head(all_pulls_tot)

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
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint =  matrix_median, na.value = "grey") + # Color scale
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


# Calculate observed means
observed_means <- combined_df %>%
  group_by(condition) %>%
  summarise(mean_value = mean(value, na.rm = T), .groups = "drop")
observed_means <- data.frame(observed_means)

# Plot destribution of infuences before and after
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

Sys.sleep(1)

#get individual influence means by condition
# Create the combined data frame
combined_df <- data.frame()

for (matrix_name in names(influence_matrices)) {
  trial_number <- as.numeric(str_extract(matrix_name, "^[0-9]+"))
  condition <- str_extract(matrix_name, "(before|after)$")
  
  influence_matrix <- influence_matrices[[matrix_name]]
  individual_ids <- rownames(influence_matrix)
  individual_means <- rowMeans(influence_matrix, na.rm = T)
  
  df <- data.frame(
    individual_id = individual_ids,
    mean_influence = individual_means,
    trial = trial_number,
    condition = condition
  )
  
  combined_df <- rbind(combined_df, df)
}


#remove those strange 9 influence values
combined_df <- filter(combined_df, mean_influence != 9)

#Convert to factor the condition variable
combined_df$condition <- factor(combined_df$condition, levels = c("before", "after"))

# Calculate individual differences
individual_differences <- combined_df %>%
  pivot_wider(names_from = condition, values_from = mean_influence) %>%
  mutate(influence_diff = after - before)

mean(individual_differences$influence_diff, na.rm = T)

#plot of the individual differences distribution
ggplot(individual_differences, aes(x = influence_diff)) +
  geom_density(fill="skyblue", color="black") +
  #facet_wrap(~ trial) +
  labs(title = "Individual Differences in Influence",
       x="Influence Difference (After - Before)",
       y="Density") +
  theme_bw()


#load individual data
ind_data <- read.csv("/mnt/EAS_ind/aeiberle/data/GroupComposition_complete.csv", sep = ";")

#match sex age and dominance to the influence data
individual_differences$Age  <- NA
individual_differences$Sex  <- NA
individual_differences$Dominance <- NA

for (i in 1:nrow(individual_differences)) {
  ID <- individual_differences$individual_id[i]
  row_to_match <- which(ind_data$ID == ID)[1]
  individual_differences$Age[i] <- ind_data[row_to_match, "Age"]
  individual_differences$Sex[i] <- ind_data[row_to_match, "Sex"]
  individual_differences$Dominance[i] <- ind_data[row_to_match, "Dominance"]   
}

individual_differences$dom_age <- individual_differences$Age
for (i in 1:nrow(individual_differences)) {
  if(individual_differences$Dominance[i] == "Dominant") {individual_differences$dom_age[i] <- "Dominant"}
  if(individual_differences$Age[i] == "Yearling") {individual_differences$dom_age[i] <- "Adult"}
}


####STATS####

output_dir <- "/mnt/EAS_ind/aeiberle/data/Models/"

#Testing the change in influence between Control and Playback conditions
#We set nested random effects as we have repeated measures by trial and also by individuals  

# we cant fit one full factorial model with Dominance, Sex and Age since some combinations are missing and we
# lose categories
# so we fit two separate models with categories that can vary across. Sex and dominance can be copard since we have
# males and females of both categories

#dom*sex model 1

#lets make a simple model without interactions
model_dom_sex_basic <- glmmTMB(influence_diff ~ Dominance + Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)
#look at VIF values to make sure we do not have factors that correlate with each other and can affcet the outcome
check_collinearity(model_dom_sex_basic)

#now a proper model with interaction
model_dom_sex <- glmmTMB(influence_diff ~ Dominance * Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)

#check model assumptions
r <- simulateResiduals(model_dom_sex, n = 1000, plot = TRUE)

#check zero inflation
testZeroInflation(model_dom_sex)


interaction_plot_model1 <- plot_model(model_dom_sex, 
                               type = "int", 
                               title = "Interaction Plot: Dominance and Sex",
                               dot.size = 3,
                               line.size = 1) +
  ylab("Difference in Influence") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(interaction_plot_model1)


default_plot_model1 <- plot_model(model_dom_sex, 
                           title = "Effects of Dominance and Sex on Influence Difference",
                           dot.size = 3,
                           line.size = 1) +
  xlab("Predictors") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(default_plot_model1)

model_1 <- grid.arrange(interaction_plot_model1, default_plot_model1, ncol = 2)

# Save model plots
# ggsave(filename = paste0(output_dir,"interaction_model1_dom_sex_plot.png"), plot = interaction_plot_model1, width = 4, height = 4)
# ggsave(filename = paste0(output_dir,"default_model1_dom_sex_plot.png"), plot = default_plot_model1, width = 8, height = 4)
# ggsave(filename = paste0(output_dir,"model1_dom_sex_plot.png"), plot = model_1, width = 8, height = 4)

tab_model(model_dom_sex)
# we do not have any significant effects or interactions so there is no need for us to do post hoc tests




#sex*age model 2

# VIF check
model_age_sex_basic <- glmmTMB(influence_diff ~ Age + Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)
check_collinearity(model_age_sex_basic)

#now a proper model with interaction
model_age_sex <- glmmTMB(influence_diff ~ Age * Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)

r <- simulateResiduals(model_age_sex, n = 1000, plot = TRUE)
testZeroInflation(model_age_sex)


interaction_plot_model2 <- plot_model(model_age_sex, 
                               type = "int", 
                               title = "Interaction Plot: Age and Sex",
                               dot.size = 3,
                               line.size = 1) +
  ylab("Difference in Influence") + 
  xlab("Age Class") + 
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


default_plot_model2 <- plot_model(model_age_sex, 
                           title = "Effects of Age and Sex on Influence Difference",
                           dot.size = 3,
                           line.size = 1) +
  xlab("Predictors") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


print(interaction_plot_model2)
print(default_plot_model2)

model_2 <- grid.arrange(interaction_plot_model2, default_plot_model2, ncol = 2)

# Save model plots
# ggsave(filename = paste0(output_dir,"interaction_model2_age_sex_plot.png"), plot = interaction_plot_model2, width = 6, height = 4)
# ggsave(filename = paste0(output_dir,"default_model2_age_sex_plot.png"), plot = default_plot_model2, width = 8, height = 4)
# ggsave(filename = paste0(output_dir,"model1_dom_sex_plot.png"), plot = model_1, width = 8, height = 4)

tab_model(model_age_sex)


#Yearlings appear to be loosing influence (barely significant but lest do a post hoc just for fun
#making an emmeans grid and specifying what to test
# we are only interested in the significant effects so will add AGE
EMM <- emmeans(model_age_sex, ~ Age )
pairs(EMM, adjust="tukey")
plot(EMM, comparisons = TRUE)
#the results show that the Yearling are loosing influence in comparison to adults but no to subadults



##Testing the absolute influence BEFORE Playback conditions

#dom*sex model 3
model_dom_sex_abs <- glmmTMB(after ~ Dominance * Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)

r <- simulateResiduals(model_dom_sex_abs, n = 1000, plot = TRUE)
testZeroInflation(model_dom_sex_abs)

plot_model(model_dom_sex_abs, type = "int")
plot_model(model_dom_sex_abs)
tab_model(model_dom_sex_abs)


#sex*age model 4
model_age_sex_abs <- glmmTMB(after ~ Age * Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)

r <- simulateResiduals(model_age_sex_abs, n = 1000, plot = TRUE)
testZeroInflation(model_age_sex_abs)

plot_model(model_age_sex_abs, type = "int")
plot_model(model_age_sex_abs)
tab_model(model_age_sex_abs)


##Testing the absolute influence AFTER Playback conditions

#dom*sex model 5
model_dom_sex_base <- glmmTMB(before ~ Dominance * Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)

r <- simulateResiduals(model_dom_sex_base, n = 1000, plot = TRUE)
testZeroInflation(model_dom_sex_base)

plot_model(model_dom_sex_base, type = "int")
plot_model(model_dom_sex_base)
tab_model(model_dom_sex_base)


#sex*age model 6
model_age_sex_base <- glmmTMB(before ~ Age * Sex  + (1|individual_id) + (1|trial), data = individual_differences, family = gaussian)

r <- simulateResiduals(model_age_sex_base, n = 1000, plot = TRUE)
testZeroInflation(model_age_sex_base)

plot_model(model_age_sex_base, type = "int")
plot_model(model_age_sex_base)
tab_model(model_age_sex_base)

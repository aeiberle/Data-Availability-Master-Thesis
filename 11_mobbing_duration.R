# this script calculates the mobbing duration according to individual distances to the playback speaker
#install.packages("zoo")
library(zoo)
library(dplyr)
#install.packages("ggpubr")
library(ggpubr)
library(performance)

setwd("/mnt/EAS_ind/aeiberle/data/")

#setting the distance from the speaker
dist_tresh <- 3

count_below_threshold <- function(distances) {
  below_threshold <- FALSE
  count <- 0
  
  for (distance in distances) {
    if (distance < dist_tresh) {
      if (!below_threshold) {
        below_threshold <- TRUE
        count <- 1
      } else {
        count <- count + 1
      }
    } else {
      below_threshold <- FALSE
    }
  }
  
  return(count)
}


#load data
GPS_data <- read.csv("distance_to_speaker.csv")

#only select the time after playback
GPS_data <- subset(GPS_data, re_numeric > -10)
#GPS_data <- subset(GPS_data, re_numeric < 301)

#count all fixes below 2, from the speaker
agg_tbl <- GPS_data %>% group_by(treatment, trial, ID) %>% 
  summarise(count_below_3 = sum(dist < dist_tresh))

#plot overall counts by treatments
ggplot(data = agg_tbl, aes(x = treatment, y = count_below_3)) + geom_boxplot()


all_mob_time_summary <- data.frame()

for (trial_ID in unique(GPS_data$trial)) {
  #trial_ID <- "2023-07-01 SI"
  trial_select <- subset(GPS_data, trial == trial_ID)
  for (ind in unique(trial_select$ID)) {
    #ind <- "VZUF048"
    ind_select <- subset(trial_select, ID == ind)
    #remove duplicate dattime valies to subsample GoPro GPS
    ind_select <-  ind_select[!duplicated(ind_select$dattime), ]
     
    #apply rolling mean to smooth GPS errors
    ind_distances <- apply(embed(ind_select$dist, 5), 1, mean)
    
    #add rolling means to the dataframe
    ind_select<- ind_select %>%
      mutate(ma2=rollapply(dist,5,mean,align='right',fill=NA))
    
    
    #count time spent by the speaker 
    sec_mobbing <- count_below_threshold(ind_distances)
    
    ind_summary <- c(trial_ID, ind_select$group[1], ind, ind_select$treatment[1], 
                     ind_distances[1], max(ind_distances), min(ind_distances), which(ind_select$ma2 < dist_tresh)[1],  sec_mobbing )
    all_mob_time_summary <- rbind(all_mob_time_summary, ind_summary)
  }
  
}

#set proper column names
colnames(all_mob_time_summary) <- c("trial", "group", "ID", "treatment",
                                 "start_dist", "max_dist", "min_dist", "TOA","duration")

#make things numeric
all_mob_time_summary$start_dist <- as.numeric(all_mob_time_summary$start_dist)
all_mob_time_summary$max_dist <- as.numeric(all_mob_time_summary$max_dist)
all_mob_time_summary$min_dist <- as.numeric(all_mob_time_summary$min_dist)
all_mob_time_summary$duration <- as.numeric(all_mob_time_summary$duration)
all_mob_time_summary$TOA <- as.numeric(all_mob_time_summary$TOA)
all_mob_time_summary$ID <- as.factor(all_mob_time_summary$ID)
all_mob_time_summary$treatment <- as.factor(all_mob_time_summary$treatment)


#remove instances where starting point already below 2 m treshold
all_mob_time_summary <- subset(all_mob_time_summary, start_dist > dist_tresh)
all_mob_time_summary <- subset(all_mob_time_summary, duration > 0)



metadata <- read.csv("/mnt/EAS_ind/aeiberle/data/metaData/time_startend_info-copy.csv")
metadata$track <- c("PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_3", "PB_6",
                    "PB_6", "PB_3",
                    "PB_3", "PB_6",
                    "PB_6", "PB_3")
metadata$trial <- paste(metadata$Date, metadata$Group)


#add track information
all_mob_time_summary <- inner_join(all_mob_time_summary, metadata[, 5:6], by = "trial")

#add group_year
all_mob_time_summary$group_year <- paste(all_mob_time_summary$group, 
                                         substr(all_mob_time_summary$trial, 1, 4))

#plot overall counts by treatments
Wilcoxon_mobbing_duration <- ggplot(data = all_mob_time_summary, aes(x = treatment, y = duration)) + 
  geom_boxplot() + 
  geom_point(aes(color = treatment)) + 
  stat_compare_means(method = "wilcox.test") + 
  ylab("Mobbing Duration") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(Wilcoxon_mobbing_duration)

#plot TOA by treatment
ggplot(data = all_mob_time_summary, aes(x = treatment, y = TOA)) + geom_boxplot() + 
  geom_point(color = "grey") + stat_compare_means(method = "wilcox.test")


# Calculate and store the 90% quantile value
threshold_90 <- quantile(all_mob_time_summary$TOA, probs = 0.9)

all_mob_time_summary_filtered <- all_mob_time_summary[all_mob_time_summary$TOA <= threshold_90, ]


#plot TOA = Recruitment by treatment
Wilcoxon_recruitment_duration <- ggplot(data = all_mob_time_summary_filtered, aes(x = treatment, y = TOA)) + 
  geom_boxplot() + 
  geom_point(aes(color = treatment)) + 
  stat_compare_means(method = "wilcox.test") + 
  ylab("Recruitment Duration") +
  xlab("Treatment") +
  labs(color = "Treatment") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))


print(Wilcoxon_recruitment_duration)


output_dir <- "/mnt/EAS_ind/aeiberle/data/Models/"

# ggsave(filename = paste0(output_dir,"Wilcoxon_mobbing_duration_plot.png"), plot = Wilcoxon_mobbing_duration, width = 8, height = 4)
# ggsave(filename = paste0(output_dir,"Wilcoxon_recruitment_duration_plot.png"), plot = Wilcoxon_recruitment_duration, width = 8, height = 4)


#STATS
library(glmmTMB)
library(sjPlot)
library(DHARMa)
library(emmeans)


all_mob_time_summary$track <- as.factor(all_mob_time_summary$track)

##GLMM "Mobbing Duration"
#duration
simple_model <- glmmTMB(data = all_mob_time_summary, formula = duration ~ treatment + track + (1|ID), family = "lognormal")
simulationOutput <- simulateResiduals(fittedModel = simple_model, plot = T)
summary(simple_model)
check_collinearity(simple_model)


full_model <- glmmTMB(data = all_mob_time_summary, formula = duration ~ treatment * track + (1|ID), family = "lognormal")
#run model diagnostic with DHARMa
simulationOutput <- simulateResiduals(fittedModel = full_model, plot = T)
summary(full_model)


mobbing_interaction_plot_model1 <- plot_model(full_model,
                                              type = "int", 
                                              title = "Mobbing - Interaction Plot: Treatment and Track",
                                              dot.size = 3,
                                              line.size = 1) +
  ylab("Mobbing Duration") +
  xlab("Treatment") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(mobbing_interaction_plot_model1)


mobbing_estimates_plot_model1 <- plot_model(full_model,
                                            type = "est", 
                                            title = "Effects of Treatment and Track on Mobbing Duration",
                                            dot.size = 3,
                                            line.size = 1) +
  ylab("Mobbing Duration") +
  xlab("Predictors") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(mobbing_estimates_plot_model1)

# Save model plots
# ggsave(filename = paste0(output_dir,"interaction_model_mobbing_plot.png"), plot = mobbing_interaction_plot_model1, width = 4, height = 4, dpi = 300)
# ggsave(filename = paste0(output_dir,"estimates_model_mobbing_plot.png"), plot = mobbing_estimates_plot_model1, width = 8, height = 4, dpi = 300)


EMM <- emmeans(full_model, poly ~ treatment | track)
EMM
EMM_plot <- plot(EMM) +
  ylab("Treatment") +
  xlab("emmean")

print(EMM_plot)

# ggsave(filename = paste0(output_dir,"EMM_mobbing_plot.png"), plot = EMM_plot, width = 8, height = 4, dpi = 300)

tab_model(full_model)



#äGLMM "TOA data"
all_mob_time_summary_filtered$track <- as.factor(all_mob_time_summary_filtered$track)

#time of arrival = Recruitment Duration
simple_model_2_filtered <- glmmTMB(data = all_mob_time_summary_filtered, formula = TOA ~ treatment + track + (1|ID), family = "lognormal")
simulationOutput_2_filtered <- simulateResiduals(fittedModel = simple_model_2_filtered, plot = T)
summary(simple_model_2_filtered)
check_collinearity(simple_model_2_filtered)


full_model_2_filtered <- glmmTMB(data = all_mob_time_summary_filtered, formula = TOA ~ treatment * track + (1|ID), family = "lognormal")
simulationOutput_3_filtered <- simulateResiduals(fittedModel = full_model_2_filtered, plot = T)
summary(full_model_2_filtered)


recruitment_interaction_plot_model1 <- plot_model(full_model_2_filtered,
                                              type = "int", 
                                              title = "Recruitment - Interaction Plot: Treatment and Track",
                                              dot.size = 3,
                                              line.size = 1) +
  ylab("Recruitment Duration") +
  xlab("Treatment") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(recruitment_interaction_plot_model1)


recruitment_estimates_plot_model1 <- plot_model(full_model_2_filtered,
                                            type = "est", 
                                            title = "Effects of Treatment and Track on Recruitment Duration",
                                            dot.size = 3,
                                            line.size = 1) +
  ylab("Recruitment Duration") +
  xlab("Predictors") +
  theme_classic() + 
  theme(plot.title = element_text(size = 10),
        axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10),
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 8))

print(recruitment_estimates_plot_model1)

# Save model plots
# ggsave(filename = paste0(output_dir,"interaction_model_recruitment_plot.png"), plot = recruitment_interaction_plot_model1, width = 5, height = 4, dpi = 300)
# ggsave(filename = paste0(output_dir,"estimates_model_recruitment_plot.png"), plot = recruitment_estimates_plot_model1, width = 8, height = 4, dpi = 300)



EMM_2_filtered <- emmeans(full_model_2_filtered, poly ~ treatment | track)

EMM_2_plot <- plot(EMM_2_filtered) +
  ylab("Treatment") +
  xlab("emmean")

print(EMM_2_plot)

# ggsave(filename = paste0(output_dir,"EMM_recruitment_plot.png"), plot = EMM_2_plot, width = 8, height = 4, dpi = 300)

tab_model(full_model_2_filtered)

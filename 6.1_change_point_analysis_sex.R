## Stats for the call density plots between sexes

setwd("/mnt/EAS_ind/aeiberle/data/RDataFiles/")

#load all audio data
load("all_call_data.Rdata")

library(dplyr)
library(changepoint)
library(ggplot2)
library(patchwork)

#perform binning with custom breaks
all_call_data <- all_call_data %>% 
  mutate(time_bins = cut(t0_timediff, breaks=seq(-600, 600, by = 30), include.lowest = T, ordered_result = T, labels = F))


test <- all_call_data %>%
  group_by(date, id_code,  time_bins, Sex) %>%
  count(label_name)

#add pre-post playback indicator
test$PB_index <- ifelse( test$time_bins < 20, "pre", "post")


test_2 <- all_call_data %>%
  group_by(date,  time_bins, Sex) %>%
  count(label_name)
all_cpts <- data.frame()


for (day in unique(test_2$date)) {
  date_select <- subset(test_2, date == day)
  for (sex in unique(date_select$Sex)) {
    sex_select <- subset(date_select, Sex == sex)
    for (call_type in unique(sex_select$label_name)) {
      ld_select <- subset(sex_select, label_name == call_type)
      ld_select <- as.data.frame(ld_select)
      call_mat <- matrix(data = 0, nrow = 1, ncol = 40)
      for (i in 1:nrow(ld_select)) {
        cell <- ld_select$time_bins[i]
        call_mat[1, cell] <- ld_select[i, "n"]
      }
      res_callrate <- cpt.meanvar(call_mat[1, ], penalty = "BIC", Q = 5, method = "PELT") # Allow up to 10 change points
      if (length(cpts(res_callrate)) < 1) { next }
      type_cpts <- as.data.frame(cbind(cpts(res_callrate), call_type, day, sex))
      all_cpts <- bind_rows(all_cpts, type_cpts)
      all_cpts <- subset(all_cpts, call_type != "unk")
    }
  }
}


all_cpts$V1 <- as.numeric(all_cpts$V1)
colnames(all_cpts) <- c("change", "call_type", "day", "sex")


#plot cumulative change per call_type
ggplot(data = all_cpts, aes(x = change )) + geom_density() + facet_grid(call_type ~ sex)


#generate permuted data sets 
set.seed = 42
#make a list to store everything
iterations <- 1000
random_data <-list()


iter <- 1
#for each individual we will reshuffle the 30 sec bins in order to maintain call bouts but detach them from the playback time


while (iter <= iterations) {
  rand_data <- data.frame()
  
  for (day in unique(all_call_data$date)) {
    date_select <- subset(all_call_data, date == day)
    for (ind in unique(date_select$id_code)) {
      for (sex in unique(date_select$Sex)) {
        ind_sex_select <- subset(date_select, id_code == ind & Sex == sex)
        # Generate random lookup table
        lookup_table <- as.data.frame(cbind(c(1:60), sample(1:40, 40, replace = FALSE)))
        colnames(lookup_table) <- c("time_bins", "rand_time_bins")
        ind_sex_select <- ind_sex_select %>%
          inner_join(lookup_table, by = "time_bins")
        rand_data <- rbind(rand_data, ind_sex_select)
      }
    }
  }
  
  # Group random data by date, rand_time_bins, and Sex
  test_2_rand <- rand_data %>%
    group_by(date, rand_time_bins, Sex) %>%
    count(label_name)
  
  # Initialize an empty data frame for this iteration
  all_cpts_rand <- data.frame()
  
  for (day in unique(test_2_rand$date)) {
    date_select <- subset(test_2_rand, date == day)
    for (sex in unique(date_select$Sex)) {
      sex_select <- subset(date_select, Sex == sex)
      for (call_type in unique(sex_select$label_name)) {
        ld_select <- subset(sex_select, label_name == call_type)
        ld_select <- as.data.frame(ld_select)
        call_mat <- matrix(data = 0, nrow = 1, ncol = 40)
        for (i in 1:nrow(ld_select)) {
          cell <- ld_select$rand_time_bins[i]
          call_mat[1, cell] <- ld_select[i, "n"]
        }
        res_callrate <- cpt.meanvar(call_mat[1, ], penalty = "BIC", Q = 5, method = "PELT")
        if (length(cpts(res_callrate)) < 1) { next }
        type_cpts_rand <- as.data.frame(cbind(cpts(res_callrate), call_type, day, sex))
        all_cpts_rand <- bind_rows(all_cpts_rand, type_cpts_rand)
      }
    }
  }
  
  all_cpts_rand$V1 <- as.numeric(all_cpts_rand$V1)
  colnames(all_cpts_rand) <- c("change", "call_type", "day", "sex")
  all_cpts_rand <- subset(all_cpts_rand, call_type != "unk")
  
  random_data[[iter]] <- all_cpts_rand
  
  iter <- iter + 1
}


#plot cumulative change per call_type
p <- ggplot() 


for (lay in 1:999) {
 p <- p + geom_density(data = random_data[[lay]], aes(x = change, color = sex), adjust = 0.4, alpha = 0.5)
}

p <- p + geom_density(data = all_cpts, aes(x = change, color = sex), adjust = 0.4, linewidth = 1) + 
  geom_vline(xintercept = 20, linetype = "dotted", color = "red", linewidth = 0.75) +
  facet_wrap(call_type ~ sex, nrow = 1 )

p



#collect change counts for ld calls in random data in 60 sec before and 60 sec after the PB
#and calculate pseudo-pvalues
summary_p_sex <- data.frame()
plots_list_sex <- list()

# Iterate over call types
for (call_type in c("agg", "al", "cc", "ld", "mo", "s", "soc")) {
  
  for (sex in c("male", "female")) {  # Separate computations for male and female
    
    after_pb <- c()
    before_pb <- c()
    
    # Loop over each iteration of the random data
    for (lay in 1:999) {
      data <- random_data[[lay]]
      
      # Count occurrences for the given sex and call type in the "after" and "before" segments
      after_pb <- c(after_pb, length(which(data$call_type == call_type & data$change %in% c(20,21) & data$sex == sex)))
      before_pb <- c(before_pb, length(which(data$call_type == call_type & data$change %in% c(18,19) & data$sex == sex)))
    }
    
    # Get counts for real data (by sex)
    after_data <- length(which(all_cpts$call_type == call_type & all_cpts$change %in% c(20,21) & all_cpts$sex == sex))
    before_data <- length(which(all_cpts$call_type == call_type & all_cpts$change %in% c(18,19) & all_cpts$sex == sex))
    
    # Calculate the number of permuted counts greater than or equal to the observed count
    num_greater_or_equal_after <- sum(after_pb >= after_data)
    num_greater_or_equal_before <- sum(before_pb >= before_data)
    
    # Calculate the pseudo p-value
    pseudo_p_after <- (num_greater_or_equal_after + 1) / (length(after_pb) + 1)
    pseudo_p_before <- (num_greater_or_equal_before + 1) / (length(before_pb) + 1)
    
    # Format the p-values to 3 decimal places for display
    pseudo_p_after <- format(pseudo_p_after, digits = 3, nsmall = 3)
    pseudo_p_before <- format(pseudo_p_before, digits = 3, nsmall = 3)
    
    # Append results to summary table
    summary_p_sex <- bind_rows(
      summary_p_sex,
      data.frame(
        call_type = call_type,
        sex = sex,
        P_val_after = pseudo_p_after,
        P_val_before = pseudo_p_before
      )
    )
    
    # Create plots for each call type and sex
    plots_list_sex[[paste(call_type, sex, sep = "_")]] <- ggplot(data.frame(value = after_pb), aes(x = value)) +
      geom_density(bw = 0.5) +
      geom_vline(xintercept = after_data, color = "red", linetype = "dashed", linewidth = 1) +
      ggtitle(paste(call_type, sex, "P-value =", pseudo_p_after, sep = " "))
  }
}

# View summary table
print(summary_p_sex)

# Visualize the density plots by call type and sex
call_rates_plot_sexes <- wrap_plots(plots_list_sex, ncol = 4)
print(call_rates_plot_sexes)

output_dir <- "/mnt/EAS_ind/aeiberle/data/Models/"
# ggsave(filename = paste0(output_dir,"call_rate_change_sexes.png"), plot = call_rates_plot_sexes, width = 18, height = 9)

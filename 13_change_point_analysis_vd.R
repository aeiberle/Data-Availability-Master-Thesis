#stats for the call density plots

setwd("/mnt/EAS_ind/vdemartsev/analysis/Meerkats/recruitment_experiments")

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
  group_by(date, id_code,  time_bins) %>%
  count(label_name)

#add pre-post playback indicator
test$PB_index <- ifelse( test$time_bins < 20, "pre", "post")




library(changepoint)

test_2 <- all_call_data %>%
  group_by(date,  time_bins) %>%
  count(label_name)
all_cpts <- data.frame()

for (day in unique(test_2$date)) {
  date_select <- subset(test_2, date == day)
  for (call_type in unique(date_select$label_name)){
  ld_select <- subset(date_select, label_name == call_type)
  ld_select<- as.data.frame(ld_select)
  call_mat <- matrix(data = 0, nrow = 1, ncol = 40)
  for (i in 1:nrow(ld_select)) {
    cell <- ld_select$time_bins[i]
    call_mat[1, cell] <- ld_select[i, "n"]
    }
  res_callrate <- cpt.meanvar(call_mat[1, ], penalty = "BIC", Q=5, method="PELT") #allow for up to 10 change points
  #plot(res_callrate)
  if(length(cpts(res_callrate)) < 1 ) {next}
  type_cpts <- as.data.frame(cbind(cpts(res_callrate), call_type, day))
  all_cpts <- bind_rows (all_cpts, type_cpts)
  all_cpts <- subset(all_cpts, call_type != "unk")
}
}


all_cpts$V1 <- as.numeric(all_cpts$V1)
colnames(all_cpts) <- c("change", "call_type", "day")

#plot cumulative change per call_type
cumulative_change <- ggplot(data = all_cpts, aes(x = change )) + geom_density() + 
  labs(title = paste("Distribution in Changes for each Call Type"), x = "Change", y = "Density") +
  facet_grid(~call_type)

# output_dir <- "/mnt/EAS_ind/aeiberle/data/Models/"
# ggsave(filename = paste0(output_dir,"cumulative_changes.png"), plot = cumulative_change, width = 12, height = 6)


#generate permuted data sets 
set.seed = 42
#make a list to store everything
iterations <- 500
random_data <-list()



iter <- 1

while (iter < iterations) {
#for each individual we will reshuffle the 30 sec bins in order to maintain call bouts but detach them from the playback time

rand_data <- data.frame()
for (day in unique(all_call_data$date)) {
  date_select <- subset(all_call_data, date == day)
  for (ind in unique(date_select$id_code)) {
    ind_select <- subset(date_select, id_code == ind)
    #generate random lookup table
    lookup_table <- as.data.frame(cbind(c(1:60), sample(1:40,40, replace=FALSE)))
    colnames(lookup_table) <- c("time_bins", "rand_time_bins")
    #replace bins in the data
    ind_select <- ind_select %>% inner_join( lookup_table, 
                                  by='time_bins')
    rand_data <- rbind(rand_data, ind_select)
  }
}


test_2_rand <- rand_data %>%
  group_by(date,  rand_time_bins) %>%
  count(label_name)


all_cpts_rand <- data.frame()

for (day in unique(test_2_rand$date)) {
  date_select <- subset(test_2_rand, date == day)
  for (call_type in unique(date_select$label_name)){
    ld_select <- subset(date_select, label_name == call_type)
    ld_select<- as.data.frame(ld_select)
    call_mat <- matrix(data = 0, nrow = 1, ncol = 40)
    for (i in 1:nrow(ld_select)) {
      cell <- ld_select$rand_time_bins[i]
      call_mat[1, cell] <- ld_select[i, "n"]
    }
    res_callrate <- cpt.meanvar(call_mat[1, ], penalty = "BIC", Q=5, method="PELT") #allow for up to 10 change points
    #plot(res_callrate)
    if(length(cpts(res_callrate)) < 1 ) {next}
    type_cpts_rand <- as.data.frame(cbind(cpts(res_callrate), call_type, day))
    all_cpts_rand <- bind_rows (all_cpts_rand, type_cpts_rand)
   
  }
}
all_cpts_rand$V1 <- as.numeric(all_cpts_rand$V1)
colnames(all_cpts_rand) <- c("change", "call_type", "day")
all_cpts_rand <- subset(all_cpts_rand, call_type != "unk")
random_data [[iter]] <- all_cpts_rand
iter <- iter+1
}



#plot cumulative change per call_type
p <- ggplot() 


for (lay in 1:499) {
 p <- p + geom_density(data = random_data[[lay]], aes(x = change), , adjust = 0.4, colour = "lightgrey", alpha = 0.5)
}

p <- p +   geom_density(data = all_cpts, aes(x = change), adjust = 0.4, colour = "black", linewidth = 1) + 
  geom_vline(xintercept = 20, linetype="dotted", color = "red", linewidth = 0.75) +
  labs(title = paste("Distribution in Changes for each Call Type"),
       x = "change",
       y = "density") +
  facet_wrap(~call_type, nrow = 1 )

p



#collect change counts for ld calls in random data in 60 sec before and 60 sec after the PB
#and calcuate pseudo-pvalues
summary_p <- data.frame()
plots_list <- list()
for (call_type in c("agg", "al", "cc", "ld", "mo", "s", "soc")) {

after_pb <- c()
before_pb <- c()

for (lay in 1:499) {
data <-  random_data[[lay]]
after_pb <- c(after_pb, length(which(data$call_type == call_type & data$change %in% c(20,21))))
before_pb <- c(before_pb, length(which(data$call_type == call_type & data$change %in% c(18,19))))
}
#get counts for real data
after_data <- length(which(all_cpts$call_type == call_type & all_cpts$change %in% c(20,21)))
before_data <- length(which(all_cpts$call_type == call_type & all_cpts$change %in% c(18,19)))
  
  # Calculate the number of permuted counts greater than or equal to the observed count
  num_greater_or_equal_after <- sum(after_pb >= after_data )
  num_greater_or_equal_before <- sum(before_pb >= before_data )
  
  # Calculate the pseudo p-value
  pseudo_p_after <- (num_greater_or_equal_after + 1) / (length(after_pb) + 1)
  pseudo_p_before <- (num_greater_or_equal_before + 1) / (length(before_pb) + 1)
  
 summary_p <- bind_rows(summary_p, data.frame(t(c(call_type,pseudo_p_after, pseudo_p_before ))))
 
 plots_list[[call_type]] <-  ggplot (data.frame(value = after_pb), aes(x = value)) +
   geom_density(bw = 0.5) + 
   geom_vline(xintercept = after_data, color = "red", linetype = "dashed", linewidth = 1) +
    ggtitle(paste(call_type, "P-value =", pseudo_p_after, sep =" "))
 
  }
 
#this table summarizes the p_values of the segments before and after the playback 
colnames(summary_p) <- c("call_type", "P-val_after", "P-val_before")

call_rates_plot <- wrap_plots(plots_list, ncol = 4)
print(call_rates_plot)

output_dir <- "/mnt/EAS_ind/aeiberle/data/Models/"
# ggsave(filename = paste0(output_dir,"call_rate_change2.png"), plot = call_rates_plot, width = 18, height = 9)

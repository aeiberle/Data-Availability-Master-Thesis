
total_synch_info <- read.csv("/mnt/EAS_ind/aeiberle/data/total_synch_info_2023.csv", sep = ";")

# write.csv(total_synch_info, "/mnt/EAS_ind/aeiberle/data/total_synch_info_2023.csv", row.names = FALSE)

# # delete columns with NA values
# cols_with_na <- colSums(is.na(total_synch_info)) > 0
# total_synch_info_cleaned <- total_synch_info[, !cols_with_na]

write.csv(total_synch_info_cleaned, "/mnt/EAS_ind/aeiberle/data/total_synch_info_cleaned_2023.csv", row.names = FALSE)

#####
# time_startend_info <- read.csv("/mnt/EAS_ind/aeiberle/data/time_startend_info.csv", sep = ";")
# 
# write.csv(time_startend_info, "/mnt/EAS_ind/aeiberle/data/time_startend_info.csv", row.names = FALSE)
# 
# # # delete columns with NA values
# # cols_with_na <- colSums(is.na(time_startend_info)) > 0
# # total_synch_info_cleaned <- time_startend_info[, !cols_with_na]
# 
# write.csv(time_startend_info, "/mnt/EAS_ind/aeiberle/data/time_startend_info.csv", row.names = FALSE)

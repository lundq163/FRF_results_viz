# to merge ADHD score & subgroups
library(dplyr)

# Read CSV files
adhd_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ABCD_ADHD_prevalence.csv")
arms1_fluid_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS1_comb_fluid_comm.csv")
arms1_flanker_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS1_comb_flanker_comm.csv")
arms1_list_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS1_comb_list_comm.csv")
arms2_fluid_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS2_comb_fluid_comm.csv")
arms2_flanker_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS2_comb_flanker_comm.csv")
arms2_list_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS2_comb_list_comm.csv")

# Merge the two data frames based on a common subject identifier
merged_arms1_fluid_data <- merge(adhd_data, arms1_fluid_subgroup_data, by.x = "subjectkey", by.y = "subject_id")
merged_arms1_flanker_data <- merge(adhd_data, arms1_flanker_subgroup_data, by.x = "subjectkey", by.y = "subject_id")
merged_arms1_list_data <- merge(adhd_data, arms1_list_subgroup_data, by.x = "subjectkey", by.y = "subject_id")
merged_arms2_fluid_data <- merge(adhd_data, arms2_fluid_subgroup_data, by.x = "subjectkey", by.y = "subject_id")
merged_arms2_flanker_data <- merge(adhd_data, arms2_flanker_subgroup_data, by.x = "subjectkey", by.y = "subject_id")
merged_arms2_list_data <- merge(adhd_data, arms2_list_subgroup_data, by.x = "subjectkey", by.y = "subject_id")

# Save the merged data to a new CSV file
write.csv(merged_arms1_fluid_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_fluid_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_arms1_flanker_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_flanker_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_arms1_list_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_arms2_fluid_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_fluid_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_arms2_flanker_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_flanker_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_arms2_list_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS2_merged.csv", row.names = FALSE)

#  function to group data based on community codes

ADHDscores_community_proc <- function(input_file, output_directory) {
  ADHDscores_task_merged <- read.csv(input_file)
  # group by community and summarize data
  grouped_communities_ADHDscores_task <- ADHDscores_task_merged %>% 
    group_by(community) %>% 
    summarise(count = n())
  summary_file <- paste0(output_directory, "summary_data_", basename(input_file))
  write.csv(grouped_communities_ADHDscores_task, summary_file, row.names = FALSE)
  # get unique community values
  unique_communities_ADHDscores_task <- (grouped_communities_ADHDscores_task$community)
  # Loop through unique communities and save data to separate files
  for (community_value in unique_communities_ADHDscores_task) {
    # Create a subset for the current community
    community_subset <- ADHDscores_task_merged %>%
      filter(community == community_value)
    
    # Define the output file name (customize as needed)
    output_file <- paste0(output_directory, "community_", community_value, "_", basename(input_file))
    
    if (nrow(community_subset) > 101) {
      write.csv(community_subset, file = output_file, row.names = FALSE)
    } else {
      print(sprintf("Community %d for %s has less than 100 subjects, so not going to include in analysis.", community_value, basename(input_file)))
    }
  }
}

# usage
input_files <- c("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_fluid_ARMS1_merged.csv",
                 "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_flanker_ARMS1_merged.csv",
                 "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS1_merged.csv",
                 "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_fluid_ARMS2_merged.csv",
                 "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_flanker_ARMS2_merged.csv",
                 "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS2_merged.csv")

output_directory <- "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/unique_communities/"

for (input_file in input_files) {
  ADHDscores_community_proc(input_file, output_directory)
}



## Visualization 

library(dplyr)
setwd("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/unique_communities/")

comm1_flanker_arm1 <- read.csv("community_1_ADHDscores_flanker_ARMS1_merged.csv")
comm1_flanker_arm2 <- read.csv("community_1_ADHDscores_flanker_ARMS2_merged.csv")
comm1_fluid_arm1 <- read.csv("community_1_ADHDscores_fluid_ARMS1_merged.csv")
comm1_fluid_arm2 <- read.csv("community_1_ADHDscores_fluid_ARMS2_merged.csv")
comm1_list_arm1 <- read.csv("community_1_ADHDscores_list_ARMS1_merged.csv")
comm1_list_arm2 <- read.csv("community_1_ADHDscores_list_ARMS2_merged.csv")


comm2_flanker_arm1 <- read.csv("community_2_ADHDscores_flanker_ARMS1_merged.csv")
comm2_flanker_arm2 <- read.csv("community_2_ADHDscores_flanker_ARMS2_merged.csv")
comm2_fluid_arm1 <- read.csv("community_2_ADHDscores_fluid_ARMS1_merged.csv")
comm2_fluid_arm2 <- read.csv("community_2_ADHDscores_fluid_ARMS2_merged.csv")
comm2_list_arm1 <- read.csv("community_2_ADHDscores_list_ARMS1_merged.csv")
comm2_list_arm2 <- read.csv("community_2_ADHDscores_list_ARMS2_merged.csv")


comm3_flanker_arm1 <- read.csv("community_3_ADHDscores_flanker_ARMS1_merged.csv")
comm3_flanker_arm2 <- read.csv("community_3_ADHDscores_flanker_ARMS2_merged.csv")
comm3_fluid_arm1 <- read.csv("community_3_ADHDscores_fluid_ARMS1_merged.csv")
comm3_fluid_arm2 <- read.csv("community_3_ADHDscores_fluid_ARMS2_merged.csv")
comm3_list_arm1 <- read.csv("community_3_ADHDscores_list_ARMS1_merged.csv")
comm3_list_arm2 <- read.csv("community_3_ADHDscores_list_ARMS2_merged.csv")


comm4_flanker_arm1 <- read.csv("community_4_ADHDscores_flanker_ARMS1_merged.csv")
comm4_flanker_arm2 <- read.csv("community_4_ADHDscores_flanker_ARMS2_merged.csv")
comm4_fluid_arm1 <- read.csv("community_4_ADHDscores_fluid_ARMS1_merged.csv")
comm4_fluid_arm2 <- read.csv("community_4_ADHDscores_fluid_ARMS2_merged.csv")
comm4_list_arm1 <- read.csv("community_4_ADHDscores_list_ARMS1_merged.csv")
comm4_list_arm2 <- read.csv("community_4_ADHDscores_list_ARMS2_merged.csv")

comm5_flanker_arm1 <- read.csv("community_5_ADHDscores_flanker_ARMS1_merged.csv")
comm5_flanker_arm2 <- read.csv("community_5_ADHDscores_flanker_ARMS2_merged.csv")
comm5_fluid_arm1 <- read.csv("community_5_ADHDscores_fluid_ARMS1_merged.csv")
comm5_fluid_arm2 <- read.csv("community_5_ADHDscores_fluid_ARMS2_merged.csv")
comm5_list_arm1 <- read.csv("community_5_ADHDscores_list_ARMS1_merged.csv")
comm5_list_arm2 <- read.csv("community_5_ADHDscores_list_ARMS2_merged.csv")


comm6_flanker_arm1 <- read.csv("community_6_ADHDscores_flanker_ARMS1_merged.csv")
comm6_flanker_arm2 <- read.csv("community_6_ADHDscores_flanker_ARMS2_merged.csv")
comm6_fluid_arm1 <- read.csv("community_6_ADHDscores_fluid_ARMS1_merged.csv")
comm6_fluid_arm2 <- read.csv("community_6_ADHDscores_fluid_ARMS2_merged.csv")
comm6_list_arm1 <- read.csv("community_6_ADHDscores_list_ARMS1_merged.csv")
comm6_list_arm2 <- read.csv("community_6_ADHDscores_list_ARMS2_merged.csv")


comm7_flanker_arm1 <- read.csv("community_7_ADHDscores_flanker_ARMS1_merged.csv")
comm7_flanker_arm2 <- read.csv("community_7_ADHDscores_flanker_ARMS2_merged.csv")
comm7_fluid_arm1 <- read.csv("community_7_ADHDscores_fluid_ARMS1_merged.csv")
comm7_fluid_arm2 <- read.csv("community_7_ADHDscores_fluid_ARMS2_merged.csv")
comm7_list_arm1 <- read.csv("community_7_ADHDscores_list_ARMS1_merged.csv")
comm7_list_arm2 <- read.csv("community_7_ADHDscores_list_ARMS2_merged.csv")


comm8_flanker_arm1 <- read.csv("community_8_ADHDscores_flanker_ARMS1_merged.csv")
comm8_flanker_arm2 <- read.csv("community_8_ADHDscores_flanker_ARMS2_merged.csv")
comm8_fluid_arm1 <- read.csv("community_8_ADHDscores_fluid_ARMS1_merged.csv")
#comm8_fluid_arm2 has less than 100 subjects. not including in analysis
#comm8_fluid_arm2 <- read.csv("community_8_ADHDscores_fluid_ARMS2_merged.csv")
comm8_list_arm1 <- read.csv("community_8_ADHDscores_list_ARMS1_merged.csv")

#comm9_flanker_arm2 has less than 100 subjects. not including in analysis
#comm9_flanker_arm2 <- read.csv("community_9_ADHDscores_flanker_ARMS2_merged.csv")
comm9_fluid_arm1 <- read.csv("community_9_ADHDscores_fluid_ARMS1_merged.csv")
#comm9_fluid_arm2 has less than 100 subjects. not including in analysis
#comm9_fluid_arm2 <- read.csv("community_9_ADHDscores_fluid_ARMS2_merged.csv")
comm9_list_arm1 <- read.csv("community_9_ADHDscores_list_ARMS1_merged.csv")

#comm10 has less than 100 subjects. not including in analysis 
#comm10_flanker_arm2 <- read.csv("community_10_ADHDscores_flanker_ARMS2_merged.csv")
#comm10_fluid_arm1 <- read.csv("community_10_ADHDscores_fluid_ARMS1_merged.csv")
#comm10_list_arm1 <- read.csv("community_10_ADHDscores_list_ARMS1_merged.csv")

#comm11,12,13_list_arm1 has less than 100 subjects. not including in analysis 
#comm11_list_arm1 <- read.csv("community_11_ADHDscores_list_ARMS1_merged.csv")

#comm12_list_arm1 <- read.csv("community_12_ADHDscores_list_ARMS1_merged.csv")

#comm13_list_arm1 <- read.csv("community_13_ADHDscores_list_ARMS1_merged.csv")


#heading
col_names <- names(comm1_list_arm2)
print(col_names)

#library(dplyr)
#install.packages("cli")
library(purrr)

# List of data frames
data_frames <- list(comm1_flanker_arm1,comm1_flanker_arm2,comm1_fluid_arm1,comm1_fluid_arm2,comm1_list_arm1,
                    comm2_flanker_arm1,comm2_flanker_arm2,comm2_fluid_arm1,comm2_fluid_arm2,comm2_list_arm1,
                    comm3_flanker_arm1,comm3_flanker_arm2,comm3_fluid_arm1,comm3_fluid_arm2,comm3_list_arm1,
                    comm4_flanker_arm1,comm4_flanker_arm2,comm4_fluid_arm1,comm4_fluid_arm2,comm4_list_arm1,
                    comm5_flanker_arm1,comm5_flanker_arm2,comm5_fluid_arm1,comm5_fluid_arm2,comm5_list_arm1,
                    comm6_flanker_arm1,comm6_flanker_arm2,comm6_fluid_arm1,comm6_fluid_arm2,comm6_list_arm1,
                    comm7_flanker_arm1,comm7_flanker_arm2,comm7_fluid_arm1,comm7_fluid_arm2,comm7_list_arm1,
                    comm8_flanker_arm1,comm8_flanker_arm2,comm8_fluid_arm1,comm8_list_arm1,
                    comm9_fluid_arm1,comm9_list_arm1)

# Create a function to analyze each data frame
analyze_data_frame <- function(data) {
  data_selected <- data %>% select(subjectkey, starts_with("ADHDcomposite"), community)
  
  summary_data <- data_selected %>%
    group_by(community) %>%
    summarize(
      across(starts_with("ADHDcomposite"),  
             list(
               Mean = ~mean(., na.rm = TRUE),
               Median = ~median(., na.rm = TRUE),
               StdDev = ~sd(., na.rm = TRUE),
               Min = ~min(., na.rm = TRUE),
               Max = ~max(., na.rm = TRUE)
             ),
             .names = "ADHDcomposite_{.col}_{.fn}"
      )
    )
  return(summary_data)
}

# Use purrr::map to apply the analysis function to each data frame
results_list <- purrr::map(data_frames, analyze_data_frame)
combined_results <- bind_rows(results_list)
write.csv(combined_results, "combined_results.csv", row.names = FALSE)

#BOXPLOT####################################

# Load necessary library
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the CSV file
data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS2_merged.csv")

communities_more_than_100 <- names(table(data$community))[table(data$community) > 100]

# Filter the dataframe to include only the communities with more than 100 participants
filtered_data <- data[data$community %in% communities_more_than_100, ]

# Convert community to a factor to ensure proper ordering on the x-axis
filtered_data$community <- factor(filtered_data$community)

# Create the box plot for ADHDcomposite score
ggplot(filtered_data, aes(x = community, y = ADHDcomposite, fill = community)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., digits = 3)),
               position=position_dodge(width=0.75), vjust=1) +
  labs(x = "Community", y = "ADHD Composite Score", title = "List ARMS2 ADHD Composite Score by Community")


#new visualizations for ADHD labels######
ctrl_subjects <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd1 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd2 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd3 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
only_adhd4 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd12 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
adhd13 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd14 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd23 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd24 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd34 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd123 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd124 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd134 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd234 <- filtered_data %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd1234 <- filtered_data %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")

groups <- list(filtered_data,ctrl_subjects,only_adhd1,only_adhd2,only_adhd3,only_adhd4,adhd12,adhd13,adhd14,adhd23,adhd24,adhd34,adhd123,adhd124,adhd134,adhd234,adhd1234)
for (df in groups){  
  if (nrow(df) > 0){
    first_row <- head(df,1)
    print(first_row)
    print(nrow(df))
  }
}
# Create summary datasets for each ADHD label count
summary_data_ctrl_adhd4 <- ctrl_subjects %>%
   group_by(community, ADHD4) %>%
   summarise(count = n()) %>%
   filter(ADHD4 == "Ctrl") %>%
   ungroup()

summary_data_ADHD1 <- filtered_data %>%
  group_by(community, ADHD1) %>%
  summarise(count = n()) %>%
  filter(ADHD1 == "ADHD") %>%
  mutate(ADHD1 = ifelse(ADHD1 == "ADHD", paste0(ADHD1, "1"), ADHD1)) %>%
  ungroup()

summary_data_ADHD2 <- filtered_data %>%
  group_by(community, ADHD2) %>%
  summarise(count = n()) %>%
  filter(ADHD2 == "ADHD") %>%
  mutate(ADHD2 = ifelse(ADHD2 == "ADHD", paste0(ADHD2, "2"), ADHD2)) %>%
  ungroup()

summary_data_ADHD3 <- filtered_data %>%
  group_by(community, ADHD3) %>%
  summarise(count = n()) %>%
  filter(ADHD3 == "ADHD") %>%
  mutate(ADHD3 = ifelse(ADHD3 == "ADHD", paste0(ADHD3, "3"), ADHD3)) %>%
  ungroup()

summary_data_ADHD4 <- filtered_data %>%
  group_by(community, ADHD4) %>%
  summarise(count = n()) %>%
  filter(ADHD4 == "ADHD") %>%
  mutate(ADHD4 = ifelse(ADHD4 == "ADHD", paste0(ADHD4, "4"), ADHD4)) %>%
  ungroup()

summary_data_combined <- bind_rows(summary_data_ctrl_adhd4, summary_data_ADHD1, summary_data_ADHD2, summary_data_ADHD3,summary_data_ADHD4)

agg_data_test <- summary_data_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4) %>%
  summarise(count = sum(count))

agg_data_tidy <- agg_data_test %>%
  pivot_longer(cols = starts_with("ADHD"), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)

# Plot using ggplot with the combined summary dataset
ggplot(agg_data_tidy, aes(x = community, y = count, fill = ADHD_label)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = count), vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(x = "Community", y = "Count", title = "ADHD Label Counts by Community for List ARMS2") +
  theme_minimal()






######## DOING IT AGAIN BUT WITH TEMPERMENT METRICS ##############

# Read CSV files
temp_arms1_test_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS1_test_FRF_UPPS_short_for_FRF.csv")
temp_fixed_arms1_test_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS1_test_FRF_UPPS_short_for_FRF_fixed.csv")
temp_arms2_test_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS2_test_FRF_UPPS_short_for_FRF.csv")
temp_fixed_arms2_test_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS2_test_FRF_UPPS_short_for_FRF_fixed.csv")

arms1_fluid_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS1_comb_fluid_comm.csv")
arms1_flanker_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS1_comb_flanker_comm.csv")
arms1_list_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS1_comb_list_comm.csv")
arms2_fluid_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS2_comb_fluid_comm.csv")
arms2_flanker_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS2_comb_flanker_comm.csv")
arms2_list_subgroup_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ARMS2_comb_list_comm.csv")

# Merge the two data frames based on a common subject identifier
merged_temp_arms1_fluid_data <- merge(temp_arms1_test_data, arms1_fluid_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_fixed_arms1_fluid_data <- merge(temp_fixed_arms1_test_data, arms1_fluid_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_arms1_flanker_data <- merge(temp_arms1_test_data, arms1_flanker_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_fixed_arms1_flanker_data <- merge(temp_fixed_arms1_test_data, arms1_flanker_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_arms1_list_data <- merge(temp_arms1_test_data, arms1_list_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_fixed_arms1_list_data <- merge(temp_fixed_arms1_test_data, arms1_list_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_arms2_fluid_data <- merge(temp_arms2_test_data, arms2_fluid_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_fixed_arms2_fluid_data <- merge(temp_fixed_arms2_test_data, arms2_fluid_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_arms2_flanker_data <- merge(temp_arms2_test_data, arms2_flanker_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_fixed_arms2_flanker_data <- merge(temp_fixed_arms2_test_data, arms2_flanker_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_arms2_list_data <- merge(temp_arms2_test_data, arms2_list_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")
merged_temp_fixed_arms2_list_data <- merge(temp_fixed_arms2_test_data, arms2_list_subgroup_data, by.x = "src_subject_id.baseline_year_1_arm_1.x", by.y = "subject_id")


write.csv(merged_temp_arms1_fluid_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fluid_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_temp_fixed_arms1_fluid_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_temp_arms1_flanker_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_flanker_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_temp_fixed_arms1_flanker_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_flanker_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_temp_arms1_list_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_list_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_temp_fixed_arms1_list_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_list_ARMS1_merged.csv", row.names = FALSE)
write.csv(merged_temp_arms2_fluid_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fluid_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_temp_fixed_arms2_fluid_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_temp_arms2_flanker_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_flanker_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_temp_fixed_arms2_flanker_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_flanker_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_temp_arms2_list_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_list_ARMS2_merged.csv", row.names = FALSE)
write.csv(merged_temp_fixed_arms2_list_data, "/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_list_ARMS2_merged.csv", row.names = FALSE)

library(ggplot2)

# Read the CSV file
data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS1_merged.csv")

communities_more_than_100 <- names(table(data$community))[table(data$community) > 100]

# Filter the dataframe to include only the communities with more than 100 participants
filtered_data <- data[data$community %in% communities_more_than_100, ]

# Convert community to a factor to ensure proper ordering on the x-axis
filtered_data$community <- factor(filtered_data$community)

# Create the box plot for ADHDcomposite score
ggplot(filtered_data, aes(x = community, y = nihtbx_totalcomp_agecorrected.baseline_year_1_arm_1, fill = community)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = "text", aes(label = round(..y.., digits = 3)),
               position=position_dodge(width=0.75), vjust=-1) +
  labs(x = "Community", y = "nihtbx_totalcomp_agecorrected", title = "Fluid ARMS1 nihtbx_totalcomp_agecorrected fixed Score by Community")










# ARM2!!!!!!! NOT REALLY USING ATM
data2 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_flanker_ARMS2_merged.csv")

communities_more_than_100 <- names(table(data2$community))[table(data2$community) > 100]

# Filter the dataframe to include only the communities with more than 100 participants
filtered_data2 <- data2[data2$community %in% communities_more_than_100, ]

# Convert community to a factor to ensure proper ordering on the x-axis
filtered_data2$community <- factor(filtered_data2$community)

# Create the box plot
ggplot(filtered_data2, aes(x = community, y = ADHDcomposite, fill = community)) +
  geom_boxplot() +
  labs(x = "Community", y = "ADHD Composite Score", title = "flanker arms2 ADHD Composite Score by Community")







# Not working atm!!!!!!!!!
# Load necessary library
library(ggplot2)

# Read the CSV file
data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_flanker_ARMS1_merged.csv")

# Convert community to a factor to ensure proper ordering on the x-axis
data$community <- factor(data$community)

# Create a summary table to calculate sample size of each community
summary_table <- data.frame(table(data$community))

# Create the box plot
ggplot(data, aes(x = community, y = ADHDcomposite, fill = community)) +
  geom_boxplot() +
  labs(x = "Community", y = "ADHD Composite Score", title = "Box Plot of ADHD Composite Score by Community") +
  geom_text(data = summary_table, aes(label = summary_table$Freq, x = as.numeric(summary_table$Var1), y = Inf), vjust = -0.5, size = 3.5)

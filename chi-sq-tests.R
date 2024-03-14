library(ggplot2)
library(dplyr)
library(tidyr)

data_arms1 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS1_merged.csv")
data_arms2 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS2_merged.csv")

communities_more_than_100_arms1 <- names(table(data_arms1$community))[table(data_arms1$community) > 100]
communities_more_than_100_arms2 <- names(table(data_arms2$community))[table(data_arms2$community) > 100]


# Filter the dataframe to include only the communities with more than 100 participants
filtered_data_arms1 <- data_arms1[data_arms1$community %in% communities_more_than_100_arms1, ]
filtered_data_arms2 <- data_arms2[data_arms2$community %in% communities_more_than_100_arms2, ]


# Convert community to a factor to ensure proper ordering on the x-axis
filtered_data_arms1$community <- factor(filtered_data_arms1$community)
filtered_data_arms2$community <- factor(filtered_data_arms2$community)


arms1_ctrl_subjects <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms1_only_adhd1 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms1_only_adhd2 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms1_only_adhd3 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms1_only_adhd4 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm1_adhd12 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arsm1_adhd13 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arsm1_adhd14 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm1_adhd23 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arsm1_adhd24 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm1_adhd34 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arsm1_adhd123 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arsm1_adhd124 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm1_adhd134 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arsm1_adhd234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arsm1_adhd1234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")

arms2_ctrl_subjects <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms2_only_adhd1 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms2_only_adhd2 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms2_only_adhd3 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms2_only_adhd4 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm2_adhd12 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arsm2_adhd13 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arsm2_adhd14 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm2_adhd23 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arsm2_adhd24 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm2_adhd34 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arsm2_adhd123 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arsm2_adhd124 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arsm2_adhd134 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arsm2_adhd234 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arsm2_adhd1234 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")

groups_arms1 <- list(filtered_data_arms1,arms1_ctrl_subjects,arms1_only_adhd1,arms1_only_adhd2,arms1_only_adhd3,arms1_only_adhd4,arsm1_adhd12,arsm1_adhd13,arsm1_adhd14,arsm1_adhd23,arsm1_adhd24,arsm1_adhd34,arsm1_adhd123,arsm1_adhd124,arsm1_adhd134,arsm1_adhd234,arsm1_adhd1234)
groups_arms2 <- list(filtered_data_arms2,arms2_ctrl_subjects,arms2_only_adhd1,arms2_only_adhd2,arms2_only_adhd3,arms2_only_adhd4,arsm2_adhd12,arsm2_adhd13,arsm2_adhd14,arsm2_adhd23,arsm2_adhd24,arsm2_adhd34,arsm2_adhd123,arsm2_adhd124,arsm2_adhd134,arsm2_adhd234,arsm2_adhd1234)

for (df in groups_arms1){  
  if (nrow(df) > 0){
    first_row <- head(df,1)
    print(first_row)
    print(nrow(df))
  }
}

for (df in groups_arms2){  
  if (nrow(df) > 0){
    first_row <- head(df,1)
    print(first_row)
    print(nrow(df))
  }
}
# Create summary datasets for each ADHD label count
summary_data_arms1_ctrl_adhd4 <- arms1_ctrl_subjects %>%
  group_by(community, Ctrl = ADHD4) %>%
  summarise(count = n()) %>%
  filter(Ctrl == "Ctrl") %>%
  ungroup()

summary_data_arms1_ADHD1 <- filtered_data_arms1 %>%
  group_by(community, ADHD1) %>%
  summarise(count = n()) %>%
  filter(ADHD1 == "ADHD") %>%
  mutate(ADHD1 = ifelse(ADHD1 == "ADHD", paste0(ADHD1, "1"), ADHD1)) %>%
  ungroup()

summary_data_arms1_ADHD2 <- filtered_data_arms1 %>%
  group_by(community, ADHD2) %>%
  summarise(count = n()) %>%
  filter(ADHD2 == "ADHD") %>%
  mutate(ADHD2 = ifelse(ADHD2 == "ADHD", paste0(ADHD2, "2"), ADHD2)) %>%
  ungroup()

summary_data_arms1_ADHD3 <- filtered_data_arms1 %>%
  group_by(community, ADHD3) %>%
  summarise(count = n()) %>%
  filter(ADHD3 == "ADHD") %>%
  mutate(ADHD3 = ifelse(ADHD3 == "ADHD", paste0(ADHD3, "3"), ADHD3)) %>%
  ungroup()

summary_data_arms1_ADHD4 <- filtered_data_arms1 %>%
  group_by(community, ADHD4) %>%
  summarise(count = n()) %>%
  filter(ADHD4 == "ADHD") %>%
  mutate(ADHD4 = ifelse(ADHD4 == "ADHD", paste0(ADHD4, "4"), ADHD4)) %>%
  ungroup()

summary_data_arms2_ctrl_adhd4 <- arms2_ctrl_subjects %>%
  group_by(community, Ctrl = ADHD4) %>%
  summarise(count = n()) %>%
  filter(Ctrl == "Ctrl") %>%
  ungroup()

summary_data_arms2_ADHD1 <- filtered_data_arms2 %>%
  group_by(community, ADHD1) %>%
  summarise(count = n()) %>%
  filter(ADHD1 == "ADHD") %>%
  mutate(ADHD1 = ifelse(ADHD1 == "ADHD", paste0(ADHD1, "1"), ADHD1)) %>%
  ungroup()

summary_data_arms2_ADHD2 <- filtered_data_arms2 %>%
  group_by(community, ADHD2) %>%
  summarise(count = n()) %>%
  filter(ADHD2 == "ADHD") %>%
  mutate(ADHD2 = ifelse(ADHD2 == "ADHD", paste0(ADHD2, "2"), ADHD2)) %>%
  ungroup()

summary_data_arms2_ADHD3 <- filtered_data_arms2 %>%
  group_by(community, ADHD3) %>%
  summarise(count = n()) %>%
  filter(ADHD3 == "ADHD") %>%
  mutate(ADHD3 = ifelse(ADHD3 == "ADHD", paste0(ADHD3, "3"), ADHD3)) %>%
  ungroup()

summary_data_arms2_ADHD4 <- filtered_data_arms2 %>%
  group_by(community, ADHD4) %>%
  summarise(count = n()) %>%
  filter(ADHD4 == "ADHD") %>%
  mutate(ADHD4 = ifelse(ADHD4 == "ADHD", paste0(ADHD4, "4"), ADHD4)) %>%
  ungroup()

summary_data_arms1_combined <- bind_rows(summary_data_arms1_ctrl_adhd4, summary_data_arms1_ADHD1, summary_data_arms1_ADHD2, summary_data_arms1_ADHD3,summary_data_arms1_ADHD4)
summary_data_arms2_combined <- bind_rows(summary_data_arms2_ctrl_adhd4, summary_data_arms2_ADHD1, summary_data_arms2_ADHD2, summary_data_arms2_ADHD3,summary_data_arms2_ADHD4)


agg_data_arms1_test <- summary_data_arms1_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4,Ctrl) %>%
  summarise(count = sum(count))

agg_data_arms2_test <- summary_data_arms2_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4,Ctrl) %>%
  summarise(count = sum(count))

agg_data_arms1_tidy <- agg_data_arms1_test %>%
  pivot_longer(cols = c(ADHD1,ADHD2,ADHD3,ADHD4,Ctrl), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)

agg_data_arms2_tidy <- agg_data_arms2_test %>%
  pivot_longer(cols = c(ADHD1,ADHD2,ADHD3,ADHD4,Ctrl), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)

proportion_data_arms1 <- agg_data_arms1_tidy %>%
  group_by(community) %>%
  mutate(group_total = sum(count),
         proportion = count/group_total) %>%
  ungroup()

proportion_data_arms2 <- agg_data_arms2_tidy %>%
  group_by(community) %>%
  mutate(group_total = sum(count),
         proportion = count/group_total) %>%
  ungroup()

# test for chi squared calculation 
chi_sq_test <- function(observed, expected) {
  chisq_result <- chisq.test(observed, p = expected)
  return(chisq_result$statistic)
}

comparison_results <- list()

for (comm1 in unique(proportion_data_arms1$community)) {
  # Subset the data for the current community in arms1
  comm1_data <- proportion_data_arms1 %>%
    filter(community == comm1) %>%
    select(ADHD_label, proportion)
  
  # Loop over communities in proportion_data_arms2
  for (comm2 in unique(proportion_data_arms2$community)) {
    # Subset the data for the current community in arms2
    comm2_data <- proportion_data_arms2 %>%
      filter(community == comm2) %>%
      select(ADHD_label, proportion)
    
    # Perform the chi-squared test and store the result
    comparison_results[[paste0(comm1, "_vs_", comm2)]] <- chi_sq_test(comm1_data$proportion, comm2_data$proportion)
  }
}

# Create a data frame with keys and sorted values
sorted_results_df <- data.frame(
  comparison = names(comparison_results),
  chi_squared = unname(sapply(comparison_results, function(x) x)),
  row.names = NULL
)
sorted_results_df <- sorted_results_df[order(sorted_results_df$chi_squared), ]


###Viz###

library(dplyr)
library(tidyr)
library(ggplot2)

# Assuming you have the comparison_results list

# Convert the list to a data frame
comparison_df <- data.frame(
  comparison = names(comparison_results),
  chi_squared = unname(sapply(comparison_results, function(x) x))
)

# Separate the comparison names into arm1_comm and arm2_comm
comparison_df <- comparison_df %>%
  separate(comparison, into = c("arm1_comm", "arm2_comm"), sep = "_vs_")

# Create a matrix of chi-squared values
chi_squared_matrix <- xtabs(chi_squared ~ arm1_comm + arm2_comm, data = comparison_df)

# Plot the matrix
ggplot(comparison_df, aes(arm1_comm, arm2_comm, fill = chi_squared)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red", trans = "log") +
  labs(x = "Communities (Arms 1)", y = "Communities (Arms 2)", fill = "Chi-squared Statistic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


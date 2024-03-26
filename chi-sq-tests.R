library(ggplot2)
library(dplyr)
library(tidyr)

#load merged adhd data
data_arms1 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_fluid_ARMS1_merged.csv")
data_arms2 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_fluid_ARMS2_merged.csv")


#only keep communities with >100 participants 
communities_more_than_100_arms1 <- names(table(data_arms1$community))[table(data_arms1$community) > 100]
communities_more_than_100_arms2 <- names(table(data_arms2$community))[table(data_arms2$community) > 100]

filtered_data_arms1 <- data_arms1[data_arms1$community %in% communities_more_than_100_arms1, ]
filtered_data_arms2 <- data_arms2[data_arms2$community %in% communities_more_than_100_arms2, ]


#convert community to a factor to ensure proper ordering on the x-axis
filtered_data_arms1$community <- factor(filtered_data_arms1$community)
filtered_data_arms2$community <- factor(filtered_data_arms2$community)


#check label distribution counts for arms1 then arms2
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
arms1_adhd12 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms1_adhd13 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms1_adhd14 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arms1_adhd23 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms1_adhd24 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arms1_adhd34 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arms1_adhd123 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms1_adhd124 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arms1_adhd134 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arms1_adhd234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arms1_adhd1234 <- filtered_data_arms1 %>%
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
arms2_adhd12 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
arms2_adhd13 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms2_adhd14 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arms2_adhd23 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms2_adhd24 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arms2_adhd34 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arms2_adhd123 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
arms2_adhd124 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
arms2_adhd134 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arms2_adhd234 <- filtered_data_arms2 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
arms2_adhd1234 <- filtered_data_arms2 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")

groups_arms1 <- list(filtered_data_arms1,arms1_ctrl_subjects,arms1_only_adhd1,arms1_only_adhd2,arms1_only_adhd3,arms1_only_adhd4,arms1_adhd12,arms1_adhd13,arms1_adhd14,arms1_adhd23,arms1_adhd24,arms1_adhd34,arms1_adhd123,arms1_adhd124,arms1_adhd134,arms1_adhd234,arms1_adhd1234)
groups_arms2 <- list(filtered_data_arms2,arms2_ctrl_subjects,arms2_only_adhd1,arms2_only_adhd2,arms2_only_adhd3,arms2_only_adhd4,arms2_adhd12,arms2_adhd13,arms2_adhd14,arms2_adhd23,arms2_adhd24,arms2_adhd34,arms2_adhd123,arms2_adhd124,arms2_adhd134,arms2_adhd234,arms2_adhd1234)

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


#create summary datasets for each ADHD label count. first arms1 then arms2
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


#aggregate summary data for arms1 then arms2
agg_data_arms1_test <- summary_data_arms1_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4,Ctrl) %>%
  summarise(count = sum(count))

agg_data_arms2_test <- summary_data_arms2_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4,Ctrl) %>%
  summarise(count = sum(count))


#reformat columns and drop na values
agg_data_arms1_tidy <- agg_data_arms1_test %>%
  pivot_longer(cols = c(ADHD1,ADHD2,ADHD3,ADHD4,Ctrl), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)

agg_data_arms2_tidy <- agg_data_arms2_test %>%
  pivot_longer(cols = c(ADHD1,ADHD2,ADHD3,ADHD4,Ctrl), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)


#calculate proportion values for each label count
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


#plot horizontal stacked bar chart for adhd label proportions 
ggplot(proportion_data_arms1, aes(fill=ADHD_label, y=proportion, x=community)) +
  geom_bar(position="fill", stat="identity") +
  labs(x = "Community", y = "Label Proportion", title = "ADHD Label proportion for Fluid Arms1") +
  coord_flip()

ggplot(proportion_data_arms2, aes(fill=ADHD_label, y=proportion, x=community)) +
  geom_bar(position="fill", stat="identity") +
  labs(x = "Community", y = "Label Proportion", title = "ADHD Label proportion for Fluid Arms2") +
  coord_flip()


########### chi squared analysis ############


#calculate the totals for proportion data
data_with_totals <- proportion_data_arms1 %>%
  group_by(community) %>%
  mutate(ADHD1_total = sum(proportion[ADHD_label == "ADHD1"]),
         ADHD2_total = sum(proportion[ADHD_label %in% c("ADHD1", "ADHD2")]),
         ADHD3_total = sum(proportion[ADHD_label %in% c("ADHD1", "ADHD2", "ADHD3")]),
         ADHD4_total = proportion[ADHD_label == "ADHD4"]) %>%
  ungroup()


#create the proportion bar chart bar chart !!!!!!!!!!!!! not using this one !!!!!!!!!!!!!!
ggplot(data_with_totals, aes(x = ADHD_label, y = proportion, fill = ADHD_label)) +
  geom_col(position = "dodge") +
  facet_wrap(~community) +
  labs(x = "ADHD Label", y = "Proportion") +
  scale_fill_discrete(labels = c("ADHD1_total" = "ADHD1 Total",
                                 "ADHD2_total" = "ADHD2 Total",
                                 "ADHD3_total" = "ADHD3 Total",
                                 "ADHD4_total" = "ADHD4",
                                 "ADHD1" = "ADHD1",
                                 "ADHD2" = "ADHD2",
                                 "ADHD3" = "ADHD3",
                                 "ADHD4" = "ADHD4",
                                 "Ctrl" = "Control"))


#test for chi squared calculation 
chi_sq_test <- function(observed, expected) {
  chisq_result <- chisq.test(observed, p = expected)
  return(chisq_result$statistic)
}

comparison_results <- list()


#across arms
for (comm1 in unique(proportion_data_arms1$community)) {
  #subset the data for the current community in arms1
  comm1_data <- proportion_data_arms1 %>%
    filter(community == comm1) %>%
    select(ADHD_label, proportion, group_total)
  
  #loop over communities in proportion_data_arms2
  for (comm2 in unique(proportion_data_arms2$community)) {
    #subset the data for the current community in arms2
    comm2_data <- proportion_data_arms2 %>%
      filter(community == comm2) %>%
      select(ADHD_label, proportion)
    
    #perform the chi-squared test and store the result
    comparison_results[[paste0(comm1, "_vs_", comm2)]] <- chi_sq_test((comm1_data$proportion*comm1_data$group_total), comm2_data$proportion)
  }
}


#within arms !!!!!!!!!! dont run this if already running above "across arms" !!!!!!!!!!!!!
for (comm1 in unique(proportion_data_arms2$community)) {
  #subset the data for the current community in arms1
  comm1_data <- proportion_data_arms2 %>%
    filter(community == comm1) %>%
    select(ADHD_label, proportion, group_total)
  
  #loop over communities in proportion_data_arms2
  for (comm2 in unique(proportion_data_arms2$community)) {
    #subset the data for the current community in arms2
    comm2_data <- proportion_data_arms2 %>%
      filter(community == comm2) %>%
      select(ADHD_label, proportion)
    
    #perform the chi-squared test and store the result
    comparison_results[[paste0(comm1, "_vs_", comm2)]] <- chi_sq_test((comm1_data$proportion*comm1_data$group_total), comm2_data$proportion)
  }
}


#create a data frame to sort comparison results from most to least similar
sorted_results_df <- data.frame(
  comparison = names(comparison_results),
  chi_squared = unname(sapply(comparison_results, function(x) x)),
  row.names = NULL
)

sorted_results_df <- sorted_results_df[order(sorted_results_df$chi_squared), ]


################# matrix viz for community chi squared metric #################


#convert the list to a data frame
comparison_df <- data.frame(
  comparison = names(comparison_results),
  chi_squared = unname(sapply(comparison_results, function(x) x))
)

#separate the comparison names into arm1_comm and arm2_comm
comparison_df <- comparison_df %>%
  separate(comparison, into = c("arm1_comm", "arm2_comm"), sep = "_vs_")

#create a matrix of chi-squared values
chi_squared_matrix <- xtabs(chi_squared ~ arm1_comm + arm2_comm, data = comparison_df)

#plot the matrix
ggplot(comparison_df, aes(arm1_comm, arm2_comm, fill = chi_squared)) +
  geom_tile() +
  scale_fill_gradient(low = "green", high = "red") +
  labs(x = "Communities (Arms 1)", y = "Communities (Arms 2)", fill = "Chi-squared Statistic") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))


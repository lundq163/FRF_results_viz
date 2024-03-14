library(ggplot2)
library(dplyr)
library(tidyr)

data_arms1 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS1_merged.csv")
data_arms2 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/ADHDscores_list_ARMS2_merged.csv")

communities_more_than_100_arms1 <- names(table(data_arms1$community))[table(data_arms1$community) > 100]
communities_more_than_100_arms2 <- names(table(data_arms2$community))[table(data_arms2$community) > 100]


# Filter the dataframe to include only the communities with more than 100 participants
filtered_data_arms1 <- data_arms1[data_arms1$community %in% communities_more_than_100_arms1, ]
filtered_data_arms1 <- data_arms1[data_arms1$community %in% communities_more_than_100_arms1, ]


# Convert community to a factor to ensure proper ordering on the x-axis
filtered_data_arms1$community <- factor(filtered_data_arms1$community)
filtered_data_arms1$community <- factor(filtered_data_arms1$community)


ctrl_subjects <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd1 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd2 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd3 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
only_adhd4 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd12 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
adhd13 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd14 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd23 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd24 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd34 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd123 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd124 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd134 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd1234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")

ctrl_subjects <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd1 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd2 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
only_adhd3 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
only_adhd4 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd12 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "Ctrl")
adhd13 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd14 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd23 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd24 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd34 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd123 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "Ctrl")
adhd124 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "Ctrl" & ADHD4 == "ADHD")
adhd134 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "Ctrl" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "Ctrl" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")
adhd1234 <- filtered_data_arms1 %>%
  filter(ADHD1 == "ADHD" & ADHD2 == "ADHD" & ADHD3 == "ADHD" & ADHD4 == "ADHD")

groups_arms1 <- list(filtered_data_arms1,ctrl_subjects,only_adhd1,only_adhd2,only_adhd3,only_adhd4,adhd12,adhd13,adhd14,adhd23,adhd24,adhd34,adhd123,adhd124,adhd134,adhd234,adhd1234)
groups_arms1 <- list(filtered_data_arms1,ctrl_subjects,only_adhd1,only_adhd2,only_adhd3,only_adhd4,adhd12,adhd13,adhd14,adhd23,adhd24,adhd34,adhd123,adhd124,adhd134,adhd234,adhd1234)

for (df in groups_arms1){  
  if (nrow(df) > 0){
    first_row <- head(df,1)
    print(first_row)
    print(nrow(df))
  }
}

for (df in groups_arms1){  
  if (nrow(df) > 0){
    first_row <- head(df,1)
    print(first_row)
    print(nrow(df))
  }
}
# Create summary datasets for each ADHD label count
summary_data_arms1_ctrl_adhd4 <- ctrl_subjects %>%
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

summary_data_arms1_ctrl_adhd4 <- ctrl_subjects %>%
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

summary_data_arms1_combined <- bind_rows(summary_data_arms1_ctrl_adhd4, summary_data_arms1_ADHD1, summary_data_arms1_ADHD2, summary_data_arms1_ADHD3,summary_data_arms1_ADHD4)
summary_data_arms1_combined <- bind_rows(summary_data_arms1_ctrl_adhd4, summary_data_arms1_ADHD1, summary_data_arms1_ADHD2, summary_data_arms1_ADHD3,summary_data_arms1_ADHD4)


agg_data_arms1_test <- summary_data_arms1_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4,Ctrl) %>%
  summarise(count = sum(count))

agg_data_arms1_test <- summary_data_arms1_combined %>%
  group_by(community, ADHD1,ADHD2,ADHD3,ADHD4,Ctrl) %>%
  summarise(count = sum(count))

agg_data_arms1_tidy <- agg_data_arms1_test %>%
  pivot_longer(cols = c(ADHD1,ADHD2,ADHD3,ADHD4,Ctrl), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)

agg_data_arms1_tidy <- agg_data_arms1_test %>%
  pivot_longer(cols = c(ADHD1,ADHD2,ADHD3,ADHD4,Ctrl), names_to = "ADHD_label", values_to = "ADHD") %>%
  drop_na() %>%
  select(community, ADHD_label, count)

proportion_data_arms1 <- agg_data_arms1_tidy %>%
  group_by(community) %>%
  mutate(group_total = sum(count),
         proportion = count/group_total) %>%
  ungroup()

proportion_data_arms1 <- agg_data_arms1_tidy %>%
  group_by(community) %>%
  mutate(group_total = sum(count),
         proportion = count/group_total) %>%
  ungroup()

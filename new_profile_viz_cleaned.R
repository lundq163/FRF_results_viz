library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)


#read the CSV file
data_arms1 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS1_merged.csv")
#data_arms2 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS2_merged.csv")
data_arms2 <- read.csv("/home/faird/lundq163/projects/FRF_results_viz/GBM_work/arms2_cog_data_with_arms1_xgb_model.csv")

#filter the dataframe to include only the communities with more than 100 participants
communities_more_than_100_arms1 <- names(table(data_arms1$community))[table(data_arms1$community) > 100]
filtered_data_arms1 <- data_arms1[data_arms1$community %in% communities_more_than_100_arms1, ]

communities_more_than_100_arms2 <- names(table(data_arms2$community))[table(data_arms2$community) > 100]
filtered_data_arms2 <- data_arms2[data_arms2$community %in% communities_more_than_100_arms2, ]


#convert community to a factor to ensure proper ordering on the x-axis
filtered_data_arms1$community <- factor(filtered_data_arms1$community)
filtered_data_arms2$community <- factor(filtered_data_arms2$community)


#calculate mean and se by community for arms1 then arms2
mean_and_se_by_community_arms1 <- filtered_data_arms1 %>%
  group_by(community) %>%
  summarise(across(starts_with("nihtbx_"),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))

mean_and_se_by_community_arms2 <- filtered_data_arms2 %>%
  group_by(community) %>%
  summarise(across(starts_with("nihtbx_"),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))


#calculate mean and se for all ABCD participants (nihtbx and upps/bisbas)
arms1_test_full_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS1_test_FRF_UPPS_short_for_FRF_fixed.csv")
arms1_train_full_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS1_train_FRF_UPPS_short_for_FRF_fixed.csv")
arms2_test_full_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS2_test_FRF_UPPS_short_for_FRF_fixed.csv")
arms2_train_full_data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS2_train_FRF_UPPS_short_for_FRF_fixed.csv")

combined_full_data <- rbind(arms1_test_full_data, arms1_train_full_data, arms2_test_full_data, arms2_train_full_data)

#combined_full_data2 <- combined_full_data[!duplicated(combined_full_data$src_subject_id.baseline_year_1_arm_1.x), ]
#above not needed. all subject ids are unique

combined_full_data_rel_cols <- combined_full_data %>%
  select(matches('^(src|bis|upps|nihtbx).*'))

mean_and_se_all_data_nih <- combined_full_data_rel_cols %>%
  summarise(across(starts_with("nihtbx_"),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))

mean_and_se_all_data_nih$community <- "all"

mean_and_se_by_community_arms1_combined <- rbind(mean_and_se_all_data_nih, mean_and_se_by_community_arms1)
mean_and_se_by_community_arms2_combined <- rbind(mean_and_se_all_data_nih, mean_and_se_by_community_arms2)


#consolidate naming of metrics 
mean_and_se_by_community_renamed_arms1 <- mean_and_se_by_community_arms1_combined %>%
  rename_with(~ {
    new_names <- str_split(., "_")
    new_names <- lapply(new_names, function(parts) {
      if (length(parts) >= 3 && parts[1] == "nihtbx") {
        paste0(parts[2], "_", tail(parts, 1))
      } else {
        .
      }
    })
    unlist(new_names)
  }, starts_with("nihtbx"))

mean_and_se_by_community_renamed_arms2 <- mean_and_se_by_community_arms2_combined %>%
  rename_with(~ {
    new_names <- str_split(., "_")
    new_names <- lapply(new_names, function(parts) {
      if (length(parts) >= 3 && parts[1] == "nihtbx") {
        paste0(parts[2], "_", tail(parts, 1))
      } else {
        .
      }
    })
    unlist(new_names)
  }, starts_with("nihtbx"))


#remove community and prediction metric from prefixes to plot
unique_prefixes_arms1 <- mean_and_se_by_community_renamed_arms1 %>%
  names() %>%
  str_extract("^[^_]+") %>%
  unique() %>%
  .[!. %in% c("community", "fluidcomp")]

unique_prefixes_arms2 <- mean_and_se_by_community_renamed_arms2 %>%
  names() %>%
  str_extract("^[^_]+") %>%
  unique() %>%
  .[!. %in% c("community", "fluidcomp")]


#convert the data from wide to long format
mean_and_se_by_community_long_arms1 <- pivot_longer(mean_and_se_by_community_renamed_arms1,
                                                    cols = starts_with(unique_prefixes_arms1),
                                                    names_to = c("metric", ".value"),
                                                    names_sep = "_")

mean_and_se_by_community_long_arms2 <- pivot_longer(mean_and_se_by_community_renamed_arms2,
                                                    cols = starts_with(unique_prefixes_arms2),
                                                    names_to = c("metric", ".value"),
                                                    names_sep = "_")


#create a user input to select the desired communities from each arm
communities_more_than_100_arms1 <- c(communities_more_than_100_arms1, "all")
#communities_more_than_100_arms2 <- c(communities_more_than_100_arms2, "all")


#restart here if creating new plot
communities_to_plot_arms1 <- select.list(communities_more_than_100_arms1, multiple = TRUE, title = "Select communities to plot:")
communities_to_plot_arms2 <- select.list(communities_more_than_100_arms2, multiple = TRUE, title = "Select communities to plot:")


#filter and combine the reshaped data based on the selected communities
mean_and_se_by_community_selected_arms1 <- mean_and_se_by_community_long_arms1[mean_and_se_by_community_long_arms1$community %in% communities_to_plot_arms1, ]
mean_and_se_by_community_selected_arms2 <- mean_and_se_by_community_long_arms2[mean_and_se_by_community_long_arms2$community %in% communities_to_plot_arms2, ]

combined_tibble <- bind_rows(
  mean_and_se_by_community_selected_arms1 %>% mutate(arm = 1),
  mean_and_se_by_community_selected_arms2 %>% mutate(arm = 2)
)


#create a vector of colors with 'all' community as black
color_values <- c(
  'all' = 'black',
  setNames(
    c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
    unique(paste("ARMS", combined_tibble$arm[combined_tibble$community != 'all'], "comm", combined_tibble$community[combined_tibble$community != 'all']))
  )
)

#plot the og figure
original_plot <- ggplot(combined_tibble, aes(x = factor(metric), y = mean, color = paste("ARMS", arm, "comm", community), group = paste("ARMS", arm, "comm", community))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = paste("ARMS", arm, "comm", community)), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community across ARMS for List") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "Community by Arm", values = color_values) +
  scale_fill_manual(name = "Community by Arm", values = color_values)

#add the 'all' community as a separate layer
all_community <- combined_tibble[combined_tibble$community == 'all', ]
final_plot <- original_plot +
  geom_line(data = all_community, aes(x = factor(metric), y = mean, color = "all", group = paste("ARMS", arm)), linewidth = 1.2) +
  geom_ribbon(data = all_community, aes(x = factor(metric), ymin = mean - se, ymax = mean + se, fill = "all"), alpha = 0.2)

#create a new color mapping for the 'all' community
all_color_mapping <- c("all" = "black")

final_plot +
  scale_color_manual(
    name = "Community by Arm",
    values = c(all_color_mapping, color_values[names(color_values) != 'all']),
    labels = c("all" = "all", names(color_values)[-1])
  ) +
  scale_fill_manual(
    name = "Community by Arm",
    values = c(all_color_mapping, color_values[names(color_values) != 'all']),
    labels = c("all" = "all", names(color_values)[-1])
  )


############## doing the same thing for upps/bisbas ##############


#match columns that start with "bis_y" or "upps_y"
cols_to_select_arms1 <- names(data_arms1)[str_detect(names(data_arms1), "^bis_y|^upps_y")]
cols_to_select_arms2 <- names(data_arms2)[str_detect(names(data_arms2), "^bis_y|^upps_y")]


#calculate mean and se by community for arms1 then arms2
mean_and_se_by_community_arms1_bb <- filtered_data_arms1 %>%
  group_by(community) %>%
  summarise(across(all_of(cols_to_select_arms1),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))

mean_and_se_by_community_arms2_bb <- filtered_data_arms2 %>%
  group_by(community) %>%
  summarise(across(all_of(cols_to_select_arms2),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))


#calculate mean and se for all ABCD data
mean_and_se_all_data_bb <- combined_full_data_rel_cols %>%
  summarise(across(all_of(cols_to_select_arms1),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))

mean_and_se_all_data_bb$community <- "all"

mean_and_se_by_community_arms1_combined_bb <- rbind(mean_and_se_all_data_bb, mean_and_se_by_community_arms1_bb)
mean_and_se_by_community_arms2_combined_bb <- rbind(mean_and_se_all_data_bb, mean_and_se_by_community_arms2_bb)


#consolidate naming of metrics
mean_and_se_by_community_renamed_arms1_bb <- mean_and_se_by_community_arms1_combined_bb %>%
  rename_with(~ str_c(
    str_extract(., "(?<=ss_).*?(?=\\.baseline_year_1_arm_1)"),
    str_extract(., "(_mean|_se)?$"),
    sep = ""
  ), -community)

mean_and_se_by_community_renamed_arms2_bb <- mean_and_se_by_community_arms2_combined_bb %>%
  rename_with(~ str_c(
    str_extract(., "(?<=ss_).*?(?=\\.baseline_year_1_arm_1)"),
    str_extract(., "(_mean|_se)?$"),
    sep = ""
  ), -community)


#remove community and prediction metric from prefixes to plot
unique_prefixes_arms1_bb <- names(mean_and_se_by_community_renamed_arms1_bb) %>%
  str_remove("_mean|_se$") %>%  
  unique() %>%  
  .[!. %in% "community"] 

unique_prefixes_arms2_bb <- names(mean_and_se_by_community_renamed_arms2_bb) %>%
  str_remove("_mean|_se$") %>%  
  unique() %>%  
  .[!. %in% "community"] 


#convert the data from wide to long format
mean_and_se_by_community_long_arms1_bb <- pivot_longer(mean_and_se_by_community_renamed_arms1_bb,
                                                       cols = starts_with(unique_prefixes_arms1_bb),
                                                       names_to = c("metric", ".value"),
                                                       names_sep = "_(?=[^_]+$)")

mean_and_se_by_community_long_arms2_bb <- pivot_longer(mean_and_se_by_community_renamed_arms2_bb,
                                                       cols = starts_with(unique_prefixes_arms2_bb),
                                                       names_to = c("metric", ".value"),
                                                       names_sep = "_(?=[^_]+$)")


#create a user input to select the desired communities from each arm
communities_to_plot_arms1_bb <- select.list(communities_more_than_100_arms1, multiple = TRUE, title = "Select communities to plot:")
communities_to_plot_arms2_bb <- select.list(communities_more_than_100_arms2, multiple = TRUE, title = "Select communities to plot:")


#filter and combine the reshaped data based on the selected communities
mean_and_se_by_community_selected_arms1_bb <- mean_and_se_by_community_long_arms1_bb[mean_and_se_by_community_long_arms1_bb$community %in% communities_to_plot_arms1_bb, ]
mean_and_se_by_community_selected_arms2_bb <- mean_and_se_by_community_long_arms2_bb[mean_and_se_by_community_long_arms2_bb$community %in% communities_to_plot_arms2_bb, ]

combined_tibble_bb <- bind_rows(
  mean_and_se_by_community_selected_arms1_bb %>% mutate(arm = 1),
  mean_and_se_by_community_selected_arms2_bb %>% mutate(arm = 2)
)


#create a vector of colors with 'all' community as black
color_values <- c(
  'all' = 'black',
  setNames(
    c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3", "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"),
    unique(paste("ARMS", combined_tibble_bb$arm[combined_tibble_bb$community != 'all'], "comm", combined_tibble_bb$community[combined_tibble_bb$community != 'all']))
  )
)

#plot the og figure
original_plot <- ggplot(combined_tibble_bb, aes(x = factor(metric), y = mean, color = paste("ARMS", arm, "comm", community), group = paste("ARMS", arm, "comm", community))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = paste("ARMS", arm, "comm", community)), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "UPPS/BISBAS Metrics by Community across ARMS for Fluid") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(name = "Community by Arm", values = color_values) +
  scale_fill_manual(name = "Community by Arm", values = color_values)

#add the 'all' community as a separate layer
all_community <- combined_tibble_bb[combined_tibble_bb$community == 'all', ]
final_plot <- original_plot +
  geom_line(data = all_community, aes(x = factor(metric), y = mean, color = "all", group = paste("ARMS", arm)), linewidth = 1.2) +
  geom_ribbon(data = all_community, aes(x = factor(metric), ymin = mean - se, ymax = mean + se, fill = "all"), alpha = 0.2)

#create a new color mapping for the 'all' community
all_color_mapping <- c("all" = "black")

final_plot +
  scale_color_manual(
    name = "Community by Arm",
    values = c(all_color_mapping, color_values[names(color_values) != 'all']),
    labels = c("all" = "all", names(color_values)[-1])
  ) +
  scale_fill_manual(
    name = "Community by Arm",
    values = c(all_color_mapping, color_values[names(color_values) != 'all']),
    labels = c("all" = "all", names(color_values)[-1])
  )
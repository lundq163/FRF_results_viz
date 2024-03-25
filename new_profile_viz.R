library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)


#read the CSV file
data_arms1 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS1_merged.csv")
data_arms2 <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS2_merged.csv")


#filter the dataframe to include only the communities with more than 100 participants
communities_more_than_100_arms1 <- names(table(data_arms1$community))[table(data_arms1$community) > 100]
filtered_data_arms1 <- data_arms1[data_arms1$community %in% communities_more_than_100_arms1, ]

communities_more_than_100_arms2 <- names(table(data_arms2$community))[table(data_arms2$community) > 100]
filtered_data_arms2 <- data_arms2[data_arms2$community %in% communities_more_than_100_arms2, ]


#convert community to a factor to ensure proper ordering on the x-axis
filtered_data_arms1$community <- factor(filtered_data_arms1$community)
filtered_data_arms2$community <- factor(filtered_data_arms2$community)

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


mean_and_se_by_community_renamed_arms1 <- mean_and_se_by_community_arms1 %>%
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

mean_and_se_by_community_renamed_arms2 <- mean_and_se_by_community_arms2 %>%
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

# Convert the data from wide to long format
mean_and_se_by_community_long_arms1 <- pivot_longer(mean_and_se_by_community_renamed_arms1,
                                              cols = starts_with(unique_prefixes_arms1),
                                              names_to = c("metric", ".value"),
                                              names_sep = "_")

mean_and_se_by_community_long_arms2 <- pivot_longer(mean_and_se_by_community_renamed_arms2,
                                                    cols = starts_with(unique_prefixes_arms2),
                                                    names_to = c("metric", ".value"),
                                                    names_sep = "_")

# Plot the line chart with error bars
ggplot(mean_and_se_by_community_long_arms1, aes(x = factor(metric), y = mean, color = community, group = community)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = community), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community for fluid Arms1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(mean_and_se_by_community_long_arms2, aes(x = factor(metric), y = mean, color = community, group = community)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = community), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community for fluid Arms2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a user input to select the desired communities
communities_to_plot_arms1 <- select.list(communities_more_than_100_arms1, multiple = TRUE, title = "Select communities to plot:")
communities_to_plot_arms2 <- select.list(communities_more_than_100_arms2, multiple = TRUE, title = "Select communities to plot:")


# Select from both arms?????
# Filter the reshaped data based on the selected communities
mean_and_se_by_community_selected_arms1 <- mean_and_se_by_community_long_arms1[mean_and_se_by_community_long_arms1$community %in% communities_to_plot_arms1, ]
mean_and_se_by_community_selected_arms2 <- mean_and_se_by_community_long_arms2[mean_and_se_by_community_long_arms2$community %in% communities_to_plot_arms2, ]

combined_tibble <- bind_rows(
  mean_and_se_by_community_selected_arms1 %>% mutate(arm = 1),
  mean_and_se_by_community_selected_arms2 %>% mutate(arm = 2)
)

# Plot the line chart with error bars
ggplot(combined_tibble, aes(x = factor(metric), y = mean, color = community, group = community)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = community), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community for fluid Arms2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(combined_tibble, aes(x = factor(metric), y = mean, color = paste(community, "Arm", arm), group = paste(community, "Arm", arm))) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = paste(community, "Arm", arm)), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community for fluid Arms") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

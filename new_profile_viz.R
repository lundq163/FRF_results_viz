library(ggplot2)
library(tidyr)
library(stringr)
library(dplyr)


#read the CSV file
data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS2_merged.csv")


#filter the dataframe to include only the communities with more than 100 participants
communities_more_than_100 <- names(table(data$community))[table(data$community) > 100]
filtered_data <- data[data$community %in% communities_more_than_100, ]


#convert community to a factor to ensure proper ordering on the x-axis
filtered_data$community <- factor(filtered_data$community)

mean_and_se_by_community <- filtered_data %>%
  group_by(community) %>%
  summarise(across(starts_with("nihtbx_"),
                   list(mean = ~ mean(.x, na.rm = TRUE),
                        se = ~ sd(.x, na.rm = TRUE) / sqrt(n())),
                   .names = "{.col}_{.fn}"))

mean_and_se_by_community_renamed <- mean_and_se_by_community %>%
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

unique_prefixes <- mean_and_se_by_community_renamed %>%
  names() %>%
  str_extract("^[^_]+") %>%
  unique() %>%
  .[!. %in% c("community", "fluidcomp")]

# Convert the data from wide to long format
mean_and_se_by_community_long <- pivot_longer(mean_and_se_by_community_renamed,
                                              cols = starts_with(unique_prefixes),
                                              names_to = c("metric", ".value"),
                                              names_sep = "_")

# Plot the line chart with error bars
ggplot(mean_and_se_by_community_long, aes(x = factor(metric), y = mean, color = community, group = community)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = community), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community for fluid Arms2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Create a user input to select the desired communities
communities_to_plot <- select.list(communities_more_than_100, multiple = TRUE, title = "Select communities to plot:")

# Filter the reshaped data based on the selected communities
mean_and_se_by_community_selected <- mean_and_se_by_community_long[mean_and_se_by_community_long$community %in% communities_to_plot, ]

# Plot the line chart with error bars
ggplot(mean_and_se_by_community_selected, aes(x = factor(metric), y = mean, color = community, group = community)) +
  geom_line() +
  geom_ribbon(aes(ymin = mean - se, ymax = mean + se, fill = community), alpha = 0.2) +
  labs(x = "Metric", y = "Average", title = "NIH toolbox Metrics by Community for fluid Arms2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

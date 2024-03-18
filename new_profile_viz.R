library(ggplot2)
library(tidyr)
library(stringr)


#read the CSV file
data <- read.csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS1_merged.csv")


#filter the dataframe to include only the communities with more than 100 participants
communities_more_than_100 <- names(table(data$community))[table(data$community) > 100]
filtered_data <- data[data$community %in% communities_more_than_100, ]


#convert community to a factor to ensure proper ordering on the x-axis
filtered_data$community <- factor(filtered_data$community)


#reshape the data to have each y metric in its own column
reshaped_data <- filtered_data %>%
  pivot_longer(cols = starts_with("nihtbx_"), names_to = "metric", values_to = "value")

ggplot(reshaped_data, aes(x = metric, y = value, color = community, group = community)) +
  geom_line() +
  labs(x = "Metric", y = "Value", title = "NIH toolbox Metrics by Community for fluid Arms1")

#ggplot(reshaped_data, aes(x = factor(sub("^(.*?)_(.*)_.*$", "\\2", metric)), y = value, color = community, group = community)) +
#  geom_line() +
#  labs(x = "Metric Abbreviation", y = "Value", title = "NIH toolbox Metrics by Community for fluid Arms1") +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))


ggplot(reshaped_data, aes(x = factor(str_extract(metric, "(?<=nihtbx_)\\w+(?=_)")), y = value, color = community, group = community)) +
  geom_line() +
  labs(x = "Metric Abbreviation", y = "Value", title = "NIH toolbox Metrics by Community for fluid Arms1") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



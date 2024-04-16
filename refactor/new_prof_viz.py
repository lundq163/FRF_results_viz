import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Read CSV files
data_arms1 = pd.read_csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS1_merged.csv")
data_arms2 = pd.read_csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/code/jacob/Temp_fixed_fluid_ARMS2_merged.csv")

# Filter dataframes to include only communities with more than 100 participants
communities_more_than_100_arms1 = data_arms1['community'].value_counts()[data_arms1['community'].value_counts() > 100].index
filtered_data_arms1 = data_arms1[data_arms1['community'].isin(communities_more_than_100_arms1)]

communities_more_than_100_arms2 = data_arms2['community'].value_counts()[data_arms2['community'].value_counts() > 100].index
filtered_data_arms2 = data_arms2[data_arms2['community'].isin(communities_more_than_100_arms2)]

# Convert community to a categorical variable to ensure proper ordering on the x-axis
filtered_data_arms1['community'] = pd.Categorical(filtered_data_arms1['community'])
filtered_data_arms2['community'] = pd.Categorical(filtered_data_arms2['community'])

# Calculate mean and standard error by community for ARMS1 and ARMS2
mean_and_se_by_community_arms1 = filtered_data_arms1.groupby('community').agg(
    **{col: ('nihtbx_' + col, ['mean', 'std']) for col in filtered_data_arms1.filter(like='nihtbx_').columns}
)
mean_and_se_by_community_arms1.columns = ['_'.join(col).strip() for col in mean_and_se_by_community_arms1.columns.values]

mean_and_se_by_community_arms2 = filtered_data_arms2.groupby('community').agg(
    **{col: ('nihtbx_' + col, ['mean', 'std']) for col in filtered_data_arms2.filter(like='nihtbx_').columns}
)
mean_and_se_by_community_arms2.columns = ['_'.join(col).strip() for col in mean_and_se_by_community_arms2.columns.values]

# Calculate mean and standard error for all ABCD participants (nihtbx and upps/bisbas)
arms1_test_full_data = pd.read_csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS1_test_FRF_UPPS_short_for_FRF_fixed.csv")
arms1_train_full_data = pd.read_csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS1_train_FRF_UPPS_short_for_FRF_fixed.csv")
arms2_test_full_data = pd.read_csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS2_test_FRF_UPPS_short_for_FRF_fixed.csv")
arms2_train_full_data = pd.read_csv("/home/feczk001/shared/projects/FEZ_USERS/feczk001/UPPS_ABCD_FRF/data/ARMS2_train_FRF_UPPS_short_for_FRF_fixed.csv")

combined_full_data = pd.concat([arms1_test_full_data, arms1_train_full_data, arms2_test_full_data, arms2_train_full_data])

# Combine relevant columns for analysis
combined_full_data_rel_cols = combined_full_data.filter(regex='^(src|bis|upps|nihtbx).*')

# Group and aggregate by community for ARMS1 and ARMS2
mean_and_se_all_data_nih = combined_full_data_rel_cols.agg(
    **{col: ['mean', 'std'] for col in combined_full_data_rel_cols.filter(like='nihtbx_').columns}
)
mean_and_se_all_data_nih['community'] = 'all'

mean_and_se_by_community_arms1_combined = pd.concat([mean_and_se_all_data_nih, mean_and_se_by_community_arms1])
mean_and_se_by_community_arms2_combined = pd.concat([mean_and_se_all_data_nih, mean_and_se_by_community_arms2])

# Rename metrics
mean_and_se_by_community_renamed_arms1 = mean_and_se_by_community_arms1_combined.rename(
    columns=lambda x: x.split('_')[1] + '_' + x.split('_')[-1] if x.startswith('nihtbx_') else x)
mean_and_se_by_community_renamed_arms2 = mean_and_se_by_community_arms2_combined.rename(
    columns=lambda x: x.split('_')[1] + '_' + x.split('_')[-1] if x.startswith('nihtbx_') else x)

# Convert data from wide to long format
mean_and_se_by_community_long_arms1 = pd.melt(mean_and_se_by_community_renamed_arms1, id_vars=['community'], var_name='metric', value_name='value')
mean_and_se_by_community_long_arms2 = pd.melt(mean_and_se_by_community_renamed_arms2, id_vars=['community'], var_name='metric', value_name='value')

# User input to select desired communities from each arm
communities_to_plot_arms1 = input("Select communities to plot for ARMS1 (comma separated): ").split(',')
communities_to_plot_arms2 = input("Select communities to plot for ARMS2 (comma separated): ").split(',')

# Filter and combine data based on selected communities
mean_and_se_by_community_selected_arms1 = mean_and_se_by_community_long_arms1[mean_and_se_by_community_long_arms1['community'].isin(communities_to_plot_arms1)]
mean_and_se_by_community_selected_arms2 = mean_and_se_by_community_long_arms2[mean_and_se_by_community_long_arms2['community'].isin(communities_to_plot_arms2)]

combined_tibble = pd.concat([mean_and_se_by_community_selected_arms1.assign(arm=1), mean_and_se_by_community_selected_arms2.assign(arm=2)])

# Plotting
sns.set_style("whitegrid")
plt.figure(figsize=(10, 6))
sns.lineplot(data=combined_tibble, x='metric', y='value', hue='community', style='arm', markers=True, ci='sd', err_style='band')
plt.xticks(rotation=45, ha='right')
plt.xlabel('Metric')
plt.ylabel('Average')
plt.title('NIH toolbox Metrics by Community across ARMS for List')
plt.legend(title='Community by Arm', bbox_to_anchor=(1.05, 1), loc='upper left')
plt.tight_layout()
plt.show()

# load libraries
installed.packages("corrplot")
install.packages("Hmisc")

library(kableExtra)                     # for printing tables
library(cowplot)                        # for side by side plots
library(lubridate)                      # for dealing with dates
library(maps)                           # for creating maps
library(tidyverse)
library(corrplot)
library(Hmisc)


# read in the cleaned data
schools_data = read_tsv("/Users/rmtf1111/schools-project/data/clean/schools_data.tsv")

# calculate median scores
median_aggregate_score = schools_data %>%
  summarise(median(aggregate_score)) %>%
  pull()

# create histograms of scores
p_score = schools_data %>%
  ggplot(aes(x = aggregate_score)) + 
  geom_histogram() +
  geom_vline(xintercept = median_aggregate_score,
             linetype = "dashed") +
  labs(x = "Aggregate score range", 
       y = "Frequency") +
  theme_bw()

# save the histogram
ggsave(filename = "/Users/rmtf1111/schools-project/results/score_hist.png", 
       plot = p_score, 
       device = "png", 
       width = 5, 
       height = 3)

# find aggregate statistics per community
aggregate_rates_community <- schools_data %>% 
  group_by(community_area_name) %>%
  mutate(community_aggregate_score = mean(aggregate_score)) %>%
  mutate(community_safety_score = mean(safety_score)) %>%
  mutate(community_environment_score = mean(environment_score)) %>%
  mutate(community_instruction_score = mean(instruction_score)) %>%
  mutate(community_student_attendance = mean(average_student_attendance)) %>%
  mutate(community_misconduct = mean(rate_of_misconducts_per_100_students)) %>%
  mutate(community_teacher_attendance = mean(average_teacher_attendance)) %>%
  select(community_area_name, 
         community_aggregate_score,
         community_safety_score,
         community_environment_score,
         community_instruction_score,
         community_student_attendance,
         community_misconduct,
         community_teacher_attendance) %>%
  distinct() 

# print top 10 communities by aggregate score
community_rates <- inner_join(aggregate_rates_community, census_data, by = "community_area_name") 
community_rates %>%
  arrange(desc(community_aggregate_score)) %>%
  head(10) %>%
  select(c(community_area_name, community_aggregate_score, per_capita_income, hardship_index)) %>%
  write_tsv("/Users/rmtf1111/schools-project/results/top_10_communities_by_scores")


# scatterplots score against statistics
p1_score = community_rates %>%
  ggplot(aes(x = hardship_index, y=community_aggregate_score)) +
  geom_point() +
  labs(x = "Hardship",
       y = "Aggregate Score") +
  theme_bw()

p2_score = community_rates %>%
  ggplot(aes(x = per_capita_income, y=community_aggregate_score)) +
  geom_point() +
  labs(x = "Income",
       y = "Aggregate Score") +
  theme_bw()

p_score <- plot_grid(p1_score, p2_score)

# save the scatterplot
ggsave(filename = "/Users/rmtf1111/schools-project/results/score_vs_stats.png", 
       plot = p_score, 
       device = "png", 
       width = 5, 
       height = 3)

# create heat map

schools_numeric <- schools_data %>%
  select(c(percent_of_housing_crowded,
             percent_households_below_poverty,
             percent_aged_16_unemployed,
             percent_aged_25_without_high_school_diploma,
             percent_aged_under_18_or_over_64,
             per_capita_income,
             hardship_index,
             average_student_attendance,
             rate_of_misconducts_per_100_students,
             individualized_education_program_compliance_rate,
             average_teacher_attendance,
             prenatal_care_beginning_in_first_trimester,
             teen_birth_rate,
             assault_homicide,
             unemployment))
schools.cor = cor(schools_numeric)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = schools.cor, col = palette, symm = TRUE)

png(width = 12, 
    height = 8,
    res = 300,
    units = "in", 
    filename = "/Users/rmtf1111/schools-project/results/heatmap.png")
heatmap(x = schools.cor, col = palette, symm = TRUE)
dev.off()






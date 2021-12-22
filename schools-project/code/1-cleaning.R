# load libraries
install.packages("janitor")
library(lubridate)
library(tidyverse)
library(janitor)

# load raw census data
census_data_raw = read_tsv(file = "/Users/rmtf1111/schools-project/data/raw/census_data_raw.tsv")

# load raw school data
school_data_raw = read_tsv(file = "/Users/rmtf1111/schools-project/data/raw/school_data_raw.tsv")

# load health school data
health_data_raw = read_tsv(file = "/Users/rmtf1111/schools-project/data/raw/health_data_raw.tsv")

# choose health data
health_data <- health_data_raw %>%
  select(c(prenatal_care_beginning_in_first_trimester, 
           teen_birth_rate,
           assault_homicide,
           unemployment,
           community_area_name))

# clean case data
school_data = school_data_raw %>%
  na.omit() %>%     
  filter(elementary_middle_or_high_school == "ES") %>%
  select(c(name_of_school,
           safety_score,
           environment_score,
           instruction_score,
           average_student_attendance,
           rate_of_misconducts_per_100_students,
           average_teacher_attendance,
           community_area_number,
           individualized_education_program_compliance_rate,
           college_enrollment_number_of_students)) %>%
  mutate(aggregate_score = (safety_score + instruction_score + environment_score)/300)
# load raw county health data
# (omitted from this template)

# clean county health data
# (omitted from this template, reading from file instead)
census_data = read_tsv("/Users/rmtf1111/schools-project/data/raw/census_data_raw.tsv")

# join county health data with case data
interm_data = inner_join(health_data, census_data, by = "community_area_name")
schools_data = inner_join(school_data, interm_data, by = "community_area_number")

schools_data
# write cleaned data to file
write_tsv(schools_data, file = "/Users/rmtf1111/schools-project/data/clean/schools_data.tsv")
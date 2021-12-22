# load libraries
library(tidyverse)
library(janitor)

# download census case data from Chicago Data Portal
url_census_data = "https://data.cityofchicago.org/api/views/kn9c-c2s2/rows.csv?accessType=DOWNLOAD"
case_data_raw = read_csv(url_census_data)
case_data_raw <- case_data_raw %>% clean_names()

# download school case data from Chicago Data Portal
url_school_data = "https://data.cityofchicago.org/api/views/9xs2-f89t/rows.csv?accessType=DOWNLOAD"
school_data_raw = read_csv(url_school_data)
school_data_raw <- school_data_raw %>% clean_names()

# download school case data from Chicago Data Portal
url_health_data = "https://data.cityofchicago.org/api/views/iqnk-2tcu/rows.csv?accessType=DOWNLOAD"
health_data_raw = read_csv(url_health_data)
health_data_raw <- health_data_raw %>% clean_names()

# write raw datasets to files
write_tsv(x = case_data_raw, file = "/Users/rmtf1111/schools-project/data/raw/census_data_raw.tsv")
write_tsv(x = school_data_raw, file = "/Users/rmtf1111/schools-project/data/raw/school_data_raw.tsv")
write_tsv(x = health_data_raw, file = "/Users/rmtf1111/schools-project/data/raw/health_data_raw.tsv")



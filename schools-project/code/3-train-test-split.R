# read in the cleaned data
schools_data = read_tsv("/Users/rmtf1111/schools-project/data/clean/schools_data.tsv")

# split into train and test (set seed here if applicable)
test_communities = c("Gage Park", "Hermosa", "West Lawn", "Dunning",
                "Roseland", "Avondale", "Bridgeport", "West Town")
schools_train = schools_data %>% filter(!(community_area_name %in% test_communities))
schools_test = schools_data %>% filter(community_area_name %in% test_communities)

# save the train and test data
write_tsv(x = schools_train, file = "/Users/rmtf1111/schools-project/data/clean/schools_train.tsv")
write_tsv(x = schools_test, file = "/Users/rmtf1111/schools-project/data/clean/schools_test.tsv")

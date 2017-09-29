# scrub.R #

# Script to scrub PHI from SIS data to use with Shiny Apps 

# Make an ID key
mcaid_id <- unique(sub_sis$consumer_id)
sis_key <- data.frame(mcaid_id)
sis_key$fake_id <- sample(x = 100000001:999999999, 
                          size = length(sis_key$mcaid_id), 
                          replace = FALSE)
sis_key$mcaid_id <- as.character(sis_key$mcaid_id)
sis_key$fake_id <- as.character(sis_key$fake_id)
rm(mcaid_id)

# Make PHI-free dataset
scrub_sis <-
  sub_sis %>%
  mutate(mcaid_id = as.character(mcaid_id)) %>%
  left_join(sis_key, by = "mcaid_id") %>%
  select(-sis_id, -mcaid_id, -age, -gender, -address)

# Write SIS Key and Scrubbed data to local workspace
write.csv(sis_key,"data/sis_key.csv", row.names = F)
library(feather)
write_feather(scrub_sis,"data/scrub_sis.feather")
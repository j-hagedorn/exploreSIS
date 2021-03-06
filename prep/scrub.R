# scrub.R #

# Script to scrub PHI from SIS data to use with Shiny Apps 

# Make an ID key
mcaid_id <- unique(sub_sis$mcaid_id)
sis_key <- data.frame(mcaid_id)
sis_key$fake_id <- sample(x = 10000001:11000001, 
                          size = length(sis_key$mcaid_id), 
                          replace = FALSE)
sis_key$mcaid_id <- as.character(sis_key$mcaid_id)
sis_key$fake_id <- as.character(sis_key$fake_id)
rm(mcaid_id)

# Make a SIS ID key
sis_id <- unique(sub_sis$sis_id)
sis_id_key <- data.frame(sis_id)
sis_id_key$fake_sis_id <- sample(x = 10000001:11000001, 
                          size = length(sis_id_key$sis_id), 
                          replace = FALSE)
sis_id_key$sis_id <- as.character(sis_id_key$sis_id)
sis_id_key$fake_sis_id <- as.character(sis_id_key$fake_sis_id)
rm(sis_id)

# Make PHI-free dataset
scrub_sis <-
  sub_sis %>%
  mutate(mcaid_id = as.character(mcaid_id)) %>%
  left_join(sis_key, by = "mcaid_id") %>%
  left_join(sis_id_key, by = "sis_id") %>%
  select(-sis_id, -mcaid_id, -age, -gender, -race, -ethnic,-address,
         # Rm comment fields for 'Other' needs areas
         -ends_with("_Other")) %>%
  droplevels() %>%
  # Mutate factor variables (caused by reading in) to numeric
  mutate_at(
    .vars = vars(
      starts_with("scr_"),
      starts_with("Q1"),
      starts_with("Q2"),
      starts_with("Q3")
    ),
    .funs = funs(as.character)
  ) %>%
  mutate_at(
    .vars = vars(
      starts_with("scr_"),
      starts_with("Q1"),
      starts_with("Q2"),
      starts_with("Q3")
    ),
    .funs = funs(as.numeric)
  )

# Write SIS Key and Scrubbed data to local workspace
write.csv(sis_key,"data/sis_key.csv", row.names = F)
write.csv(sis_id_key,"data/sis_id_key.csv", row.names = F)
library(feather)
write_feather(scrub_sis,"data/scrub_sis.feather")

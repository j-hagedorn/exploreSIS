
################################################################################  
# TRANSFORM SECTIONS TO BREAK DOWN TYPE OF SERVICE  
# Process heavy computations up front to allow for cleaner performance
# Section 1

library(tidyverse)

q2_tos <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("tos")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q2")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q2"))
  ) %>%
  gather(item, type, Q2A1_TOS:Q2F8_TOS) %>%
  mutate(
    type_n = as.numeric(type),
    item = gsub("TOS","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,type) %>%
  summarize(
    n = n_distinct(id),
    type_n = sum(type_n)
  ) %>%
  ungroup() 

q2_fqy <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("Fqy")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q2")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q2"))
  ) %>%
  gather(item, frequency, Q2A1_Fqy:Q2F8_Fqy) %>%
  mutate(
    frequency_n = as.numeric(frequency),
    item = gsub("Fqy","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,frequency) %>%
  summarize(
    n = n_distinct(id),
    frequency_n = sum(frequency_n)
  ) %>%
  ungroup() 

q2_dst <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("dst")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q2")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q2"))
  ) %>%
  gather(item, DST, Q2A1_DST:Q2F8_DST) %>%
  mutate(
    DST_n = as.numeric(DST),
    item = gsub("DST","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,DST) %>%
  summarize(
    n = n_distinct(id),
    DST_n = sum(DST_n)
  ) %>%
  ungroup() 

q2_to <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("ImportantTo")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q2")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q2"))
  ) %>%
  gather(item, import_to, Q2A1_ImportantTo:Q2F8_ImportantTo) %>%
  mutate(
    import_to_n = as.numeric(import_to),
    item = gsub("ImportantTo","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,import_to) %>%
  summarize(
    n = n_distinct(id),
    import_to_n = sum(import_to_n)
  ) %>%
  ungroup() 

q2_for <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("ImportantFor")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q2")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q2"))
  ) %>%
  gather(item, import_for, Q2A1_ImportantFor:Q2F8_ImportantFor) %>%
  mutate(
    import_for_n = as.numeric(import_for),
    item = gsub("ImportantFor","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,import_for) %>%
  summarize(
    n = n_distinct(id),
    import_for_n = sum(import_for_n)
  ) %>%
  ungroup()

# Join intermediate tables
q2 <- 
  q2_tos %>% 
  inner_join(q2_fqy, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x,fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n) %>%
  inner_join(q2_dst, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x,fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n, DST, DST_n) %>%
  inner_join(q2_to, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x,fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n, DST, DST_n,
         import_to_n) %>%
  inner_join(q2_for, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x, fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n, DST, DST_n,
         import_to_n, import_for_n)

# Remove intermediate tables
rm(q2_dst); rm(q2_fqy); rm(q2_tos); rm(q2_to); rm(q2_for)

# Remove extra cols and recode vars  
q2 %<>%
  left_join(needs, by = "item") %>% # add item level desc and groups
  mutate(
    type = dplyr::recode(
      type,
      `0` = 'None',
      `1` = 'Monitoring',
      `2` = 'Coaching',
      `3` = 'Partial Physical Assistance',
      `4` = 'Full Physical Support'
    ),
    frequency = dplyr::recode(
      frequency,
      `0` = 'Minimal',
      `1` = 'Monthly',
      `2` = 'Weekly',
      `3` = 'Daily',
      `4` = 'Hourly'
    ),
    DST = dplyr::recode(
      DST,
      `0` = 'None',
      `1` = 'Under 30 min',
      `2` = 'Under 2 hrs',
      `3` = '2-4 hrs',
      `4` = 'Over 4 hrs'
    ),
    score = type_n + frequency_n + DST_n,
    import_to = as.logical(import_to_n),
    import_for = as.logical(import_for_n),
    importance = case_when(
      import_to == T & import_for == T ~ "To and For",
      import_to  == T                  ~ "To",
      import_for == T                  ~ "For",
      import_to == F & import_for == F ~ "Not Endorsed"
    )
  ) %>%
  select(-n) %>%
  droplevels()

# Process Section 3: Protection and Advocacy
q3_tos <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("tos")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q3")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q3"))
  ) %>%
  gather(item, type, Q3A1_TOS:Q3A8_TOS) %>%
  mutate(
    type_n = as.numeric(type),
    item = gsub("TOS","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,type) %>%
  summarize(
    n = n_distinct(id),
    type_n = sum(type_n)
  ) %>%
  ungroup() 

q3_fqy <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("Fqy")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q3")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q3"))
  ) %>%
  gather(item, frequency, Q3A1_Fqy:Q3A8_Fqy) %>%
  mutate(
    frequency_n = as.numeric(frequency),
    item = gsub("Fqy","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,frequency) %>%
  summarize(
    n = n_distinct(id),
    frequency_n = sum(frequency_n)
  ) %>%
  ungroup() 

q3_dst <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("dst")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q3")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q3"))
  ) %>%
  gather(item, DST, Q3A1_DST:Q3A8_DST) %>%
  mutate(
    DST_n = as.numeric(DST),
    item = gsub("DST","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,DST) %>%
  summarize(
    n = n_distinct(id),
    DST_n = sum(DST_n)
  ) %>%
  ungroup() 

q3_to <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("ImportantTo")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q3")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q3"))
  ) %>%
  gather(item, import_to, Q3A1_ImportantTo:Q3A8_ImportantTo) %>%
  mutate(
    import_to_n = as.numeric(import_to),
    item = gsub("ImportantTo","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,import_to) %>%
  summarize(
    n = n_distinct(id),
    import_to_n = sum(import_to_n)
  ) %>%
  ungroup() 

q3_for <-
  scrub_sis %>%
  # Remove empty randomized IDs
  filter(
    is.na(fake_id) == FALSE
    & is.na(fake_sis_id) == FALSE
  ) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,ends_with("ImportantFor")) %>% 
  select(fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,starts_with("Q3")) %>%
  mutate_at(
    .funs = funs(as.character), 
    .vars = vars(starts_with("Q3"))
  ) %>%
  gather(item, import_for, Q3A1_ImportantFor:Q3A8_ImportantFor) %>%
  mutate(
    import_for_n = as.numeric(import_for),
    item = gsub("ImportantFor","",item),
    id = as.factor(paste0(fake_sis_id,item))
  ) %>%
  group_by(id,fake_sis_id,fake_id,PIHP,agency,interviewer,sis_date,item,import_for) %>%
  summarize(
    n = n_distinct(id),
    import_for_n = sum(import_for_n)
  ) %>%
  ungroup()

# Join intermediate tables
q3 <- 
  q3_tos %>% 
  inner_join(q3_fqy, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x,fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n) %>%
  inner_join(q3_dst, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x,fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n, DST, DST_n) %>%
  inner_join(q3_to, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x,fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n, DST, DST_n,
         import_to_n) %>%
  inner_join(q3_for, by = "id") %>%
  select(id, fake_sis_id = fake_sis_id.x, fake_id = fake_id.x, 
         PIHP = PIHP.x, agency = agency.x, interviewer = interviewer.x,
         sis_date = sis_date.x, item = item.x, 
         type, type_n, frequency, frequency_n, DST, DST_n,
         import_to_n, import_for_n)

# Remove intermediate tables
rm(q3_dst); rm(q3_fqy); rm(q3_tos); rm(q3_to); rm(q3_for)

# Remove extra cols and recode vars  
q3 %<>%
  left_join(needs, by = "item") %>% # add item level desc and groups
  mutate(
    type = dplyr::recode(
      type,
      `0` = 'None',
      `1` = 'Monitoring',
      `2` = 'Coaching',
      `3` = 'Partial Physical Assistance',
      `4` = 'Full Physical Support'
    ),
    frequency = dplyr::recode(
      frequency,
      `0` = 'Minimal',
      `1` = 'Monthly',
      `2` = 'Weekly',
      `3` = 'Daily',
      `4` = 'Hourly'
    ),
    DST = dplyr::recode(
      DST,
      `0` = 'None',
      `1` = 'Under 30 min',
      `2` = 'Under 2 hrs',
      `3` = '2-4 hrs',
      `4` = 'Over 4 hrs'
    ),
    score = type_n + frequency_n + DST_n,
    import_to = as.logical(import_to_n),
    import_for = as.logical(import_for_n),
    importance = case_when(
      import_to == T & import_for == T ~ "To and For",
      import_to  == T                  ~ "To",
      import_for == T                  ~ "For",
      import_to == F & import_for == F ~ "Not Endorsed"
    )
  ) %>%
  select(-n) %>%
  droplevels()

library(feather)
write_feather(q2,"data/q2.feather")
write_feather(q3,"data/q3.feather")


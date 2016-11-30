library(dplyr)

## GET NEEDS ELEMENTS ##
##############################################################################
# Pull individual assessment elements from SIS data

sis_names <- data.frame(colnames(sis_full))
names(sis_names)[1] <- "field"

needs <-
  sis_names %>%
  mutate(section = ifelse(grepl("^Q[0-9]", field), 
                          yes = gsub("_.*$", "", field),
                          no = NA),
         section = substr(section, start=1, stop=3),
         item = ifelse(grepl("^Q[0-9]", field), 
                       yes = gsub("ExMedSupport|ExBehSupport|ImportantTo|ImportantFor|TOS|DST|Fqy|Notes|Other","",field),
                       no = NA),
         item_type = ifelse(grepl("^Q[0-9]", field), 
                            yes = gsub("^.*_","",field),
                            no = NA),
         section_desc = dplyr::recode(section,
                                    Q1A = "Medical Supports",
                                    Q1B = "Behavioral Supports",
                                    Q2A = "Home Living",
                                    Q2B = "Community Living",
                                    Q2C = "Lifelong Learning",
                                    Q2D = "Employment",
                                    Q2E = "Health and Safety",
                                    Q2F = "Social Activities",
                                    Q3A = "Protection and Advocacy",
                                    Q4A = "Supplemental Questions"),
         item_desc = dplyr::recode(item,
                                   Q1A1_ = "Oxygen therapy", 
                                   Q1A2_ = "Postural drainage", 
                                   Q1A3_ = "Chest PT", 
                                   Q1A4_ = "Suctioning", 
                                   Q1A5_ = "Oral Stimulation", 
                                   Q1A6_ = "Tube feeding", 
                                   Q1A7_ = "Parenteral feeding", 
                                   Q1A8_ = "Positioning", 
                                   Q1A9_ = "Wound dressing", 
                                   Q1A10_ = "Infection control", 
                                   Q1A11_ = "Seizure management",
                                   Q1A12_ = "Dialysis", 
                                   Q1A13_ = "Ostomy Care", 
                                   Q1A14_ = "Transferring", 
                                   Q1A15_ = "Therapy services", 
                                   Q1A16_ = "Hypertension",
                                   Q1A17_ = "Allergies",
                                   Q1A18_ = "Diabetes",
                                   Q1A19_ = "Other Medical (1st)",
                                   Q1A20_ = "Other Medical (2nd)",
                                   Q1A21_ = "Other Medical (3rd)",
                                   Q1B1_ = "Assault", 
                                   Q1B2_ = "Property destruction",
                                   Q1B3_ = "Stealing", 
                                   Q1B4_ = "Self-injury", 
                                   Q1B5_ = "Ingestion", 
                                   Q1B6_ = "Suicide attempts", 
                                   Q1B7_ = "Sexual aggression", 
                                   Q1B8_ = "Sexually inappropriate", 
                                   Q1B9_ = "Outbursts", 
                                   Q1B10_ = "Wandering",
                                   Q1B11_ = "Substance abuse",
                                   Q1B12_ = "Mental health",
                                   Q1B13_ = "Other Behavioral (1st)",
                                   Q1B14_ = "Other Behavioral (2nd)",
                                   Q1B15_ = "Other Behavioral (3rd)",
                                   Q2A1_ = "Home appliances", 
                                   Q2A2_ = "Personal hygiene", 
                                   Q2A3_ = "Toilet", 
                                   Q2A4_ = "Dressing", 
                                   Q2A5_ = "Preparing food", 
                                   Q2A6_ = "Eating Food", 
                                   Q2A7_ = "Laundry", 
                                   Q2A8_ = "Housekeeping", 
                                   Q2B1_ = "Transportation", 
                                   Q2B2_ = "Recreation", 
                                   Q2B3_ = "Preferred Activities", 
                                   Q2B4_ = "Accessing public settings", 
                                   Q2B5_ = "Using public services", 
                                   Q2B6_ = "Shopping", 
                                   Q2B7_ = "Community interaction", 
                                   Q2B8_ = "Visit Friends/Family", 
                                   Q2C1_ = "Learning problem-solving", 
                                   Q2C2_ = "Learning functional skills", 
                                   Q2C3_ = "Learning health skills", 
                                   Q2C4_ = "Learning self-determination", 
                                   Q2C5_ = "Learning self-management", 
                                   Q2C6_ = "Educational decisions", 
                                   Q2C7_ = "Accessing education", 
                                   Q2C8_ = "Interaction during learning", 
                                   Q2C9_ = "Learning with technology", 
                                   Q2D1_ = "Learning job skills", 
                                   Q2D2_ = "Accessing accommodations", 
                                   Q2D3_ = "Coworker interaction", 
                                   Q2D4_ = "Supervisor interaction", 
                                   Q2D5_ = "Work speed", 
                                   Q2D6_ = "Work quality", 
                                   Q2D7_ = "Changing job assignments", 
                                   Q2D8_ = "Seeking employer assistance", 
                                   Q2E1_ = "Taking medications", 
                                   Q2E2_ = "Moving about", 
                                   Q2E3_ = "Avoiding hazards", 
                                   Q2E4_ = "Obtaining health care", 
                                   Q2E5_ = "Accessing emergency svs", 
                                   Q2E6_ = "Nutrition and diet", 
                                   Q2E7_ = "Physical fitness", 
                                   Q2E8_ = "Emotional well-being", 
                                   Q2F1_ = "Social skills", 
                                   Q2F2_ = "Recreation/Leisure", 
                                   Q2F3_ = "Socializing outside", 
                                   Q2F4_ = "Friendship", 
                                   Q2F5_ = "Intimate relationships", 
                                   Q2F6_ = "Socializing at home", 
                                   Q2F7_ = "Communicating needs", 
                                   Q2F8_ = "Volunteering", 
                                   Q3A1_ = "Self-advocacy", 
                                   Q3A2_ = "Decision making", 
                                   Q3A3_ = "Avoiding exploitation", 
                                   Q3A4_ = "Civic responsibility", 
                                   Q3A5_ = "Advocacy participation", 
                                   Q3A6_ = "Legal services", 
                                   Q3A7_ = "Financial management", 
                                   Q3A8_ = "Advocating for others", 
                                   Q4A1v1 = "High medical staffing", 
                                   Q4A2v1 = "High behavioral with conviction",
                                   Q4A3v1 = "High behavioral w/o conviction", 
                                   Q4A4v1 = "High suicide risk"),
         item_long = dplyr::recode(item,
                                   Q1A1_ = "Inhalation or oxygen therapy", 
                                   Q1A2_ = "Postural drainage", 
                                   Q1A3_ = "Chest PT", 
                                   Q1A4_ = "Suctioning", 
                                   Q1A5_ = "Oral Stimulation or jaw positioning", 
                                   Q1A6_ = "Tube feeding (e.g. nasogastric)", 
                                   Q1A7_ = "Parenteral feeding (e.g. IV)", 
                                   Q1A8_ = "Turning or positioning", 
                                   Q1A9_ = "Dressing of open wound(s)", 
                                   Q1A10_ = "Protection from infectious diseases due to immune system", 
                                   Q1A11_ = "Seizure management",
                                   Q1A12_ = "Dialysis", 
                                   Q1A13_ = "Ostomy Care", 
                                   Q1A14_ = "Lifting and/or transferring", 
                                   Q1A15_ = "Therapy services", 
                                   Q1A16_ = "Hypertension",
                                   Q1A17_ = "Allergies",
                                   Q1A18_ = "Diabetes",
                                   Q1A19_ = "Other Medical (1st)",
                                   Q1A20_ = "Other Medical (2nd)",
                                   Q1A21_ = "Other Medical (3rd)",
                                   Q1B1_ = "Prevention of assaults or injuries to others", 
                                   Q1B2_ = "Prevention of property destruction (e.g. fire setting, breaking furniture)",
                                   Q1B3_ = "Prevention of stealing", 
                                   Q1B4_ = "Prevention of self-injury", 
                                   Q1B5_ = "Prevention of pica ingestion of inedible substances", 
                                   Q1B6_ = "Prevention of suicide attempts", 
                                   Q1B7_ = "Prevention of sexual aggression", 
                                   Q1B8_ = "Prevention of nonaggressive, but inapproppriate sexual behavior (e.g. exposes self in public, exhibitionism, inappropriate touching or gesturing)", 
                                   Q1B9_ = "Prevention of emotional outbursts", 
                                   Q1B10_ = "Prevention of wandering",
                                   Q1B11_ = "Prevention of substance abuse",
                                   Q1B12_ = "Maintaining mental health treatments",
                                   Q1B13_ = "Other Behavioral (1st)",
                                   Q1B14_ = "Other Behavioral (2nd)",
                                   Q1B15_ = "Other Behavioral (3rd)",
                                   Q2A1_ = "Operating home appliances/electronics", 
                                   Q2A2_ = "Bathing and taking care of personal hygiene and grooming needs", 
                                   Q2A3_ = "Using the toilet", 
                                   Q2A4_ = "Dressing", 
                                   Q2A5_ = "Preparing food", 
                                   Q2A6_ = "Eating Food", 
                                   Q2A7_ = "Taking care of clothes (includes laundering)", 
                                   Q2A8_ = "Housekeeping and cleaning", 
                                   Q2B1_ = "Getting from place to place throughout the community", 
                                   Q2B2_ = "Participating in recreation/leisure activities in the community settings", 
                                   Q2B3_ = "Participating in preferred community activities (church, volunteer, etc.)", 
                                   Q2B4_ = "Accessing public buildings and settings", 
                                   Q2B5_ = "Using public services in the community", 
                                   Q2B6_ = "Shopping and purchasing goods and services", 
                                   Q2B7_ = "Interacting with community members", 
                                   Q2B8_ = "Going to visit friends and family", 
                                   Q2C1_ = "Learning and using problem-solving strategies", 
                                   Q2C2_ = "Learning functional academics (reading signs, counting change, etc.)", 
                                   Q2C3_ = "Learning health and physical education skills", 
                                   Q2C4_ = "Learning self-determination skills", 
                                   Q2C5_ = "Learning self-management strategies", 
                                   Q2C6_ = "Participating in training/educational decisions", 
                                   Q2C7_ = "Accessing training/educational settings", 
                                   Q2C8_ = "Interacting with others in learning activities", 
                                   Q2C9_ = "Using technology for learning", 
                                   Q2D1_ = "Learning and using specific job skills", 
                                   Q2D2_ = "Accessing/receiving job/task accommodations", 
                                   Q2D3_ = "Interacting with coworkers", 
                                   Q2D4_ = "Interacting with supervisors/coaches", 
                                   Q2D5_ = "Completing work-related tasks with acceptable speed", 
                                   Q2D6_ = "Completing work-related tasks with acceptable quality", 
                                   Q2D7_ = "Changing job assignments", 
                                   Q2D8_ = "Seeking information and assistance from an employer", 
                                   Q2E1_ = "Taking medications", 
                                   Q2E2_ = "Ambulating and moving about", 
                                   Q2E3_ = "Avoiding health and safety hazards", 
                                   Q2E4_ = "Obtaining health care services", 
                                   Q2E5_ = "Learning how to access emergency services", 
                                   Q2E6_ = "Maintaining nutritious diet", 
                                   Q2E7_ = "Maintaining physical health and fitness", 
                                   Q2E8_ = "Maintaining emotional well-being", 
                                   Q2F1_ = "Using appropriate social skills", 
                                   Q2F2_ = "Participating in recreation/leisure activities with others", 
                                   Q2F3_ = "Socializing outside the household", 
                                   Q2F4_ = "Making and keeping friends", 
                                   Q2F5_ = "Engaging in loving and intimate relationships", 
                                   Q2F6_ = "Socializing within the household", 
                                   Q2F7_ = "Communicating with others about personal needs", 
                                   Q2F8_ = "Engaging in volunteer work", 
                                   Q3A1_ = "Advocating for self", 
                                   Q3A2_ = "Making choices and decisions", 
                                   Q3A3_ = "Protecting self from exploitation", 
                                   Q3A4_ = "Exercising legal/civic responsibilities", 
                                   Q3A5_ = "Belonging to and participating in self-advocacy/support organizations", 
                                   Q3A6_ = "Obtaining legal services", 
                                   Q3A7_ = "Managing money and personal finances", 
                                   Q3A8_ = "Advocating for others", 
                                   Q4A1v1 = "The Individual requires exceptionally high levels of staff support to address severe medical risks related to inhalation or oxygen therapy; postural drainage; chest PT, suctioning; oral stimulation and/or jaw positioning; tube feeding; parenteral feeding; skin care turning or positioning; skin care dressing of open wounds; protection from infectious diseases due to immune system impairment; seizure management; dialysis; ostomy care; medically-related lifting and/or transferring; therapy services, and/or other critical medical supports", 
                                   Q4A2v1 = "The Individual is currently a severe community safety risk to others related to actual or attempted assault and/or injury to others; property destruction due to fire setting and/or arson; and/or sexual aggression and has been convicted of a crime related to these risks?",
                                   Q4A3v1 = "The Indvidual is currently a severe community safety risk to others related to actual or attempted assault and/or injury to others; property destruction due to fire setting and/or arson; and/or sexual aggression and has not been convicted of a crime related to these risks?", 
                                   Q4A4v1 = "The Individual displays self-directed destructiveness related to self-injury; pica; and/or suicide attempts which seriously threatens their own health and/or safety?"),
         qol = dplyr::recode(item,
                             Q1A1_ = "Physical Wellbeing", 
                             Q1A2_ = "Physical Wellbeing", 
                             Q1A3_ = "Physical Wellbeing", 
                             Q1A4_ = "Physical Wellbeing", 
                             Q1A5_ = "Physical Wellbeing", 
                             Q1A6_ = "Physical Wellbeing", 
                             Q1A7_ = "Physical Wellbeing", 
                             Q1A8_ = "Physical Wellbeing", 
                             Q1A9_ = "Physical Wellbeing", 
                             Q1A10_ = "Physical Wellbeing", 
                             Q1A11_ = "Physical Wellbeing",
                             Q1A12_ = "Physical Wellbeing", 
                             Q1A13_ = "Physical Wellbeing", 
                             Q1A14_ = "Physical Wellbeing", 
                             Q1A15_ = "Physical Wellbeing", 
                             Q1A16_ = "Physical Wellbeing",
                             Q1A17_ = "Physical Wellbeing",
                             Q1A18_ = "Physical Wellbeing",
                             Q1A19_ = "Physical Wellbeing",
                             Q1A20_ = "Physical Wellbeing",
                             Q1A21_ = "Physical Wellbeing",
                             Q1B1_ = "Emotional Wellbeing", 
                             Q1B2_ = "Emotional Wellbeing",
                             Q1B3_ = "Emotional Wellbeing", 
                             Q1B4_ = "Emotional Wellbeing", 
                             Q1B5_ = "Emotional Wellbeing", 
                             Q1B6_ = "Emotional Wellbeing", 
                             Q1B7_ = "Emotional Wellbeing", 
                             Q1B8_ = "Emotional Wellbeing",
                             Q1B10_ = "Emotional Wellbeing",
                             Q1B11_ = "Emotional Wellbeing",
                             Q1B12_ = "Emotional Wellbeing",
                             Q1B13_ = "Emotional Wellbeing",
                             Q1B14_ = "Emotional Wellbeing",
                             Q1B15_ = "Emotional Wellbeing",
                             Q2A1_ = "Physical Wellbeing",
                             Q2A2_ = "Physical Wellbeing",
                             Q2A3_ = "Physical Wellbeing",
                             Q2A4_ = "Physical Wellbeing",
                             Q2A5_ = "Physical Wellbeing",
                             Q2A6_ = "Physical Wellbeing",
                             Q2A7_ = "Physical Wellbeing",
                             Q2A8_ = "Physical Wellbeing",
                             Q2B1_ = "Social Inclusion",
                             Q2B2_ = "Social Inclusion",
                             Q2B3_ = "Social Inclusion",
                             Q2B4_ = "Social Inclusion",
                             Q2B5_ = "Social Inclusion",
                             Q2B6_ = "Social Inclusion",
                             Q2B7_ = "Social Inclusion",
                             Q2B8_ = "Interpersonal Relations",
                             Q2C1_ = "Personal Development",
                             Q2C2_ = "Personal Development",
                             Q2C3_ = "Physical Wellbeing",
                             Q2C4_ = "Self Determination",
                             Q2C5_ = "Personal Development",
                             Q2C6_ = "Personal Development",
                             Q2C7_ = "Personal Development",
                             Q2C8_ = "Social Inclusion",
                             Q2C9_ = "Personal Development",
                             Q2D1_ = "Material Wellbeing",
                             Q2D2_ = "Material Wellbeing",
                             Q2D3_ = "Material Wellbeing",
                             Q2D4_ = "Material Wellbeing",
                             Q2D5_ = "Material Wellbeing",
                             Q2D6_ = "Material Wellbeing",
                             Q2D7_ = "Material Wellbeing",
                             Q2D8_ = "Material Wellbeing",
                             Q2E1_ = "Physical Wellbeing",
                             Q2E2_ = "Physical Wellbeing",
                             Q2E3_ = "Physical Wellbeing",
                             Q2E4_ = "Physical Wellbeing",
                             Q2E5_ = "Physical Wellbeing",
                             Q2E6_ = "Personal Development",
                             Q2E7_ = "Personal Development",
                             Q2E8_ = "Emotional Wellbeing",
                             Q2F1_ = "Interpersonal Relations",
                             Q2F2_ = "Interpersonal Relation",
                             Q2F3_ = "Interpersonal Relation",
                             Q2F4_ = "Interpersonal Relation",
                             Q2F5_ = "Interpersonal Relations",
                             Q2F6_ = "Interpersonal Relations",
                             Q2F7_ = "Self Determination",
                             Q2F8_ = "Social Inclusion",
                             Q3A1_ = "Rights",
                             Q3A2_ = "Self Determination",
                             Q3A3_ = "Rights",
                             Q3A4_ = "Rights",
                             Q3A5_ = "Social Inclusion",
                             Q3A6_ = "Rights",
                             Q3A7_ = "Self Determination",
                             Q3A8_ = "Rights",
                             Q4A1v1 = "Physical Wellbeing",
                             Q4A2v1 = "Emotional Wellbeing",
                             Q4A3v1 = "Emotional Wellbeing",
                             Q4A4v1 = "Emotional Wellbeing")
         ) %>%
  filter(is.na(item) == F) %>%
  group_by(section, section_desc, item, item_desc, qol) %>%
  summarize(n = n()) %>%
  filter(n > 1) %>% # Remove notes and other extraneous fields
  # Flag necessary services (i.e. high risk)
  mutate(need_svc = item %in% c("Q2A1_","Q2A2_","Q2A3_","Q2A4_",
                                "Q2A5_","Q2A6_","Q2A7_","Q2A8_",
                                "Q2B1_","Q2B4_","Q2B6_","Q2B7_",
                                "Q2E1_","Q2E2_","Q2E3_","Q2E4_","Q2E5_",
                                "Q1A1_","Q1A2_","Q1A3_","Q1A4_","Q1A5_", 
                                "Q1A6_","Q1A7_","Q1A8_","Q1A9_","Q1A10_", 
                                "Q1A11_","Q1A12_","Q1A13_","Q1A14_","Q1A15_", 
                                "Q1A16_","Q1A17_","Q1A18_",
                                "Q1B1_","Q1B2_","Q1B3_","Q1B4_","Q1B5_",
                                "Q1B6_","Q1B7_","Q1B12_","Q1B13_"),
         refer_ot = item %in% c("Q2A1_","Q2A2_","Q2A3_","Q2A4_",
                                "Q2A5_","Q2A6_","Q2A7_","Q2A8_",
                                "Q2B2_","Q2B7_"),
         refer_nurs = item %in% c("Q2E1_","Q1A1_","Q1A2_","Q1A3_","Q1A4_",
                                  "Q1A9_","Q1A10_","Q1A11_","Q1A12_","Q1A13_",
                                  "Q1A19_","Q1A16_","Q1A17_","Q1A18_"),
         refer_sp = item %in% c("Q1A5_","Q1A6_","Q1A7_"),
         refer_pt = item %in% c("Q1A2_","Q1A3_","Q2E7_","Q2B1_",
                                "Q2B4_","Q2E2_","Q1A14_","Q1A8_"),
         refer_diet = item %in% c("Q2A5_","Q2A6_","Q2E6_")
  ) 

# Write to file
write.csv(needs,"data/needs.csv", row.names = F)

## GET SERVICE CODES ##
##############################################################################
# Get standard cost data from 404 report
  open404 <- RCurl::getURL("https://raw.githubusercontent.com/j-hagedorn/open404/master/data/clean/Master.csv",
                    ssl.verifypeer = 0L, followlocation = 1L)
  writeLines(open404,'temp.csv')
  open404 <- read.csv('temp.csv')

# Get codes provided for DD population in most recent FY

  svs <-
  open404 %>%
    filter(Population == "DD" & FY == max(FY)) %>%
    filter(ServiceType != "Screening & Assessment") %>% # filter out scrn / assess
    filter(ServiceType != "Other") %>%
    group_by(Service, Description, Code, Code_Mod) %>%
    summarize(n = n(),
              pts = sum(SumOfCases, na.rm = T)) %>%
    filter(pts >= 50) %>%
    droplevels()

## COMBINE IN MATRIX ##
##############################################################################  
# combine Needs and Services in matrix

for (i in as.list(levels(as.factor(needs$item)))) {
    svs[i] <- NA
}
  
needs_matrix <- svs
rm(svs); rm(open404)

## MATCH NEEDS TO SERVICES ##
##############################################################################

library(magrittr)

# Match each need item to all possible services that could be used to address need

needs_matrix %<>%
  mutate(Q2A3_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Toilet
         Q2A7_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Clothes
         Q2A5_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Preparing food
         Q2A6_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020",
                              "92610","92507","92526"), # Eating food
         Q2A8_ = Code %in% c("H2015","H2016",
                              "T1020",
                              "S5165","E1399","T2028","T2029"), # Housekeeping
         Q2A4_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Dressing
         Q2A2_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020"), # Hygiene
         Q2A1_ = Code %in% c("H2015","H2016","H0043",
                              "H0045","S5150","S5151","T1005",
                              "T1020",
                              "H0043"), # Appliances
         Q2B1_ = Code %in% c("H2015","H2016","H0043",
                              "T1020",
                              "T5999"), # Getting Around
         Q2B2_ = Code %in% c("H2015","H2016","H0043"), # Recreation
         Q2B5_ = Code %in% c("H2015","H2016","H0043"), # Public Services
         Q2B8_ = Code %in% c("H2015","H2016","H0043"), # Visit Friends/Family
         Q2B3_ = Code %in% c("H2015","H2016","H0043"), # Preferred Activities
         Q2B6_ = Code %in% c("H2015","H2016","H0043"), # Shopping
         Q2B7_ = Code %in% c("H2015","H2016","H0043"), # Community interaction
         Q2B4_ = Code %in% c("H2015","H2016","H0043"), # Accessing Settings
         Q2C8_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111"), # Learning interaction
         Q2C6_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "T2025"), # Learning decisions
         Q2C1_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Problem solving
         Q2C9_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Using technology
         Q2C7_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Accessing training
         Q2C2_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Academics
         Q2C3_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "S9445","S9446"), # Learning health skills
         Q2C4_ = Code %in% c("T1016","T1017",
                              "T2025"), # Learning self-determination
         Q2C5_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017"), # Learning self-management
         Q2D2_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Job accomodations
         Q2D1_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Specific job skills
         Q2D3_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Co-worker interaction
         Q2D4_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Supervisor interaction
         Q2D5_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Work speed
         Q2D6_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Work quality
         Q2D7_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Changing assignments
         Q2D8_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "H2023","H2014"), # Seeking assistance
         Q2E1_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "T1999",
                              "99506","99211","96372"), # Taking medications
         Q2E3_ = Code %in% c("H2000","S5160","S5161"), # Avoiding hazards
         Q2E4_ = Code %in% c("H2015","H2016","H0043",
                              "T1016","T1017",
                              "S9445","S9446"), # Obtaining health care
         Q2E2_ = Code %in% c("T2028","T2029","S5199","E1399","T2039",
                              "97110","97112","97113","97116","97124",
                              "97140","97530","97532","97533","97535", 
                              "97537","97542","S8990","97750","97755",
                              "97760","97762","97150","97001","97002",
                              "97003","97004"), # Moving about
         Q2E5_ = Code %in% c("S5160","S5161"), # Accessing emergency svs
         Q2E6_ = Code %in% c("97802","97803","97804",
                              "S9470"), # Nutritional diet
         Q2E7_ = Code %in% c("97110","97112","97113","97116","97124",
                              "97140","97530","97532","97533","97535", 
                              "97537","97542","S8990","97750","97755",
                              "97760","97762","97150","97001","97002",
                              "97003","97004",
                              "T5999"), # Physical fitness
         Q2E8_ = Code %in% c("T1016","T1017",
                              "90837","90832","90834","90833","90836","90838"), # Emotional well-being
         Q2F6_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111",
                              "H2030",
                              "H0023","H0038","H0046",
                              "H2014"), # Socializing in home
         Q2F2_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111",
                              "H2030",
                              "H0023","H0038","H0046",
                              "H2014"), # Recreation with others
         Q2F3_ = Code %in% c("H2015","H2016","H0043",
                              "H2019","S5108","S5111",
                              "H2030",
                              "H0023","H0038","H0046",
                              "H2014"), # Socializing out of home
         Q2F4_ = Code %in% c(""), # Making friends
         Q2F7_ = Code %in% c("T2039"), # Communicating with helpers
         Q2F1_ = Code %in% c("H2015","H2016","H0043"), # Appropriate social skills
         Q2F5_ = Code %in% c(""), # Intimate relationships
         Q2F8_ = Code %in% c("H2015","H2016","H0043",
                              "H2014","T2015"), # Volunteer work
         Q3A1_ = Code %in% c("T1016","T1017",
                             "H0023","H0038","H0046"), # Self-advocacy
         Q3A7_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017",
                             "T2025"), # Money management
         Q3A3_ = Code %in% c("T1016","T1017"), # Exploited by others
         Q3A4_ = Code %in% c("T1016","T1017"), # Legal responsibility
         Q3A5_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017"), # Participation
         Q3A6_ = Code %in% c("T1016","T1017"), # Legal services
         Q3A2_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017",
                             "T2025"), # Decision making
         Q3A8_ = Code %in% c("H2015","H2016","H0043",
                             "T1016","T1017"), # Other advocacy
         Q1A1_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Oxygen therapy
         Q1A10_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "T1020"), # Prevent infection
         Q1A11_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Seizure mgmt
         Q1A12_ = Code %in% c("T1001","T1002","H0034",
                               "S9123","S9124","T1000"), # Dialysis
         Q1A13_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Ostomy care
         Q1A14_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Transfers
         Q1A15_ = Code %in% c("H2015","H2016","H0043",
                               "T1001","T1002","H0034",
                               "S9123","S9124","T1000",
                               "T1020"), # Therapy svs
         Q1A19_ = Code %in% c(""), # Other medical
         Q1A2_ = Code %in% c("T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Postural drainage
         Q1A3_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Chest PT
         Q1A4_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Suctioning
         Q1A5_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Oral stimulation
         Q1A6_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Tube feeding
         Q1A7_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Parental feeding
         Q1A8_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Positioning
         Q1A9_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Dressing wounds
         Q1A16_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Hypertension
         Q1A17_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020","S9470"), # Allergies
         Q1A18_ = Code %in% c("H2015","H2016","H0043",
                              "T1001","T1002","H0034",
                              "S9123","S9124","T1000",
                              "T1020"), # Diabetes
         Q1B2_ = Code %in% c("90837","90832","90834","90833","90836","90838",
                              "H2019","S5108","S5111",
                              "H2000",
                              "T1020"), # Assault
         Q1B11_ = Code %in% c("H2019","S5108","S5111"), # Wandering
         Q1B10_ = Code %in% c("T1016","T1017"), # Substance abuse
         Q1B12_ = Code %in% c("T1016","T1017"), # Mental health tx
         Q1B13_ = Code %in% c(""), # Other behavioral
         Q1B3_ = Code %in% c("H2019","S5108","S5111"), # Property destruction
         Q1B4_ = Code %in% c(""), # Stealing
         Q1B5_ = Code %in% c("H2019","S5108","S5111"), # Self injury
         Q1B7_ = Code %in% c(""), # Pica
         Q1B6_ = Code %in% c(""), # Suicide attempts
         Q1B9_ = Code %in% c("H2019","S5108","S5111"), # Sexual aggression
         Q1B8_ = Code %in% c("H2019","S5108","S5111"), # Inappropriate
         Q1B1_ = Code %in% c("") # Outbursts
         ) %>%
  ungroup() %>%
  select(Code, Q1A1_:Q3A8_) %>%
  distinct()

write.csv(needs_matrix, "data/needs_matrix.csv", row.names = F)


# tst
# # first remember the names
# n <- needs_matrix$Code
# 
# # transpose all but the first column (name)
# tst <- as.data.frame(t(needs_matrix[,-1]))
# colnames(tst) <- n
# tst$item <- row.names(tst)
# sis_desc <- needs %>% ungroup %>% select(item, item_desc)
# tst <-
# tst %>% 
#   left_join(sis_desc, by = "item") %>%
#   select(item, item_desc, H2019:H2023)
# 
# 
# tst %>%
#   mutate_each(funs(as.numeric)) %>%
#   d3heatmap(colors = "Blues", 
#             theme = "")         
         
  

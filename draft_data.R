library(baseballr)
library(dplyr)

draft2023 <- get_draft_mlb(2023)
draft2022 <- get_draft_mlb(2022)
draft2021 <- get_draft_mlb(2021)

drafts_22_23 <- full_join(draft2022, draft2023)

drafts_21_23 <- full_join(draft2021, drafts_22_23)

data <- drafts_21_23 %>%
  filter(!school_school_class %in% c('HS SR', 'HS JR', 'NS', 'No School'),
         complete.cases(signing_bonus)) %>%
  mutate(conference = case_when(school_school_class %in% c('JC J1', 'JC J2', 'JC J3') ~ 'JUCO',
                                school_name %in% c('Boston College', 'Clemson', 'Duke', 'Florida State', 'Georgia Tech', 'Louisville',
                                                   'Miami', 'North Carolina', 'NC State', 'Notre Dame', 'Pittsburgh', 'Virginia',
                                                   'Virginia Tech', 'Wake Forest') ~ 'ACC',
                                school_name %in% c('Illinois', 'Indiana', 'Iowa', 'Maryland', 'Michigan', 'Michigan State',
                                                   'Minnesota', 'Nebraska', 'Northwestern', 'Ohio State', 'Penn State',
                                                   'Purdue', 'Rutgers') ~ 'Big Ten',
                                school_name %in% c('Baylor', 'Kansas', 'Kansas State', 'Oklahoma', 'Oklahoma State', 'Texas',
                                                   'TCU', 'Texas Tech', 'West Virginia') ~ "Big 12",
                                school_name %in% c('Arizona', 'Arizona State', 'California', 'UCLA', 'Colorado', 'Oregon',
                                                   'Oregon State', 'USC', 'Stanford', 'Utah', 'Washington', 'Washington') ~ 'Pac 12',
                                school_name %in% c('Alabama', 'Arkansas', 'Auburn', 'Florida', 'Georgia', 'Kentucky', 'LSU',
                                                   'Mississippi State', 'Missouri', 'Ole Miss', 'South Carolina', 'Tennessee',
                                                   'Texas A&M', 'Vanderbilt') ~ 'SEC',
                                TRUE ~ 'Mid Major'))

data$signing_bonus <- as.numeric(data$signing_bonus)

data$signing_bonus <- as.numeric(data$signing_bonus)

data1 <- data %>%
  group_by(school_name) %>%
  summarise(draft_picks = n(),
            bonus_per_pick = sum(signing_bonus, na.rm = TRUE) / draft_picks)

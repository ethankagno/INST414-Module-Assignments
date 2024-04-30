library(baseballr)
library(dplyr)
library(ggplot2)
library(lubridate)

draft2023 <- get_draft_mlb(2023)

draft_subset <- subset(draft2023, select = c('person_first_last_name', 'pick_round', 'pick_number', 'person_birth_date', 'pick_value',
                                             'signing_bonus', 'school_school_class', 'person_primary_position_name'))
draft_subset$signing_bonus <- as.numeric(draft_subset$signing_bonus)
draft_subset$pick_value <- as.numeric(draft_subset$pick_value)
draft_subset$pick_value <- ifelse(draft_subset$pick_number >= 315, 150000, draft_subset$pick_value)

draft_subset <- draft_subset %>%
  rename(school_class = school_school_class) %>%
  filter(school_class %in% c('HS SR', '4YR SO', '4YR JR', '4YR SR', '4YR 5S', '4YR GR', '5S'),
         !(person_primary_position_name %in% c('Designated Hitter', 'Two-Way Player'))) %>%
  mutate(school_class = case_when(school_class == '5S' ~ '4YR SR',
                                  school_class == '4YR 5S' ~ '4YR GR',
                                  TRUE ~ school_class),
         person_primary_position_name = case_when(person_primary_position_name == 'Outfielder' ~ 'Outfield',
                                                  TRUE ~ person_primary_position_name),
         draft_date = case_when(pick_number <= 70 ~ '2023-07-09',
                                between(pick_number, 71, 314) ~ '2023-07-10',
                                pick_number >= 315 ~ '2023-07-11'),
         draft_date = ymd(`draft_date`),
         person_birth_date = ymd(`person_birth_date`),
         draft_age = as.numeric((draft_date - person_birth_date) / 365),
         percent_slot = signing_bonus / pick_value)

draft_subset$school_class <- factor(draft_subset$school_class,
                                    levels = c('HS SR', '4YR SO', '4YR JR', '4YR SR', '4YR GR'))

draft_subset$person_primary_position_name <- factor(draft_subset$person_primary_position_name,
                                                    levels = c('Pitcher', 'Catcher', 'First Base', 'Second Base',
                                                               'Third Base', 'Shortstop', 'Outfield'))

slot_by_class <- draft_subset %>%
  group_by(school_class) %>%
  summarise(percent_slot = sum(signing_bonus, na.rm = TRUE) / sum(pick_value, na.rm = TRUE))

slot_by_position <- draft_subset %>%
  group_by(person_primary_position_name) %>%
  summarise(percent_slot = sum(signing_bonus, na.rm = TRUE) / sum(pick_value, na.rm = TRUE))

slot_by_class_position <- draft_subset %>%
  group_by(school_class, person_primary_position_name) %>%
  summarise(percent_slot = sum(signing_bonus, na.rm = TRUE) / sum(pick_value, na.rm = TRUE))

post_10_slot <- draft_subset %>%
  filter(pick_round >= 11) %>%
  group_by(school_class) %>%
  summarise(percent_slot = sum(signing_bonus, na.rm = TRUE) / sum(pick_value, na.rm = TRUE))

bonus_plot <- ggplot(draft_subset, aes(draft_age, percent_slot)) + geom_point()

plot_by_class <- ggplot(slot_by_class, aes(school_class, percent_slot)) + geom_bar(stat = "identity") +
  labs(x = "Class", y = "Percent Slot") + geom_text(aes(label = paste(round(percent_slot, 3) * 100, '%', sep = "")), vjust = -0.2)

plot_by_position <- ggplot(slot_by_position, aes(person_primary_position_name, percent_slot)) + geom_bar(stat = "identity") +
  labs(x = "Position", y = "Percent Slot") + geom_text(aes(label = paste(round(percent_slot, 3) * 100, '%', sep = "")), vjust = -0.2)

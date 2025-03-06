

#'
#' Data processing script for "Evaluating convergence between two data visualization literacy assessments"
#'


# SETUP ----
rm(list=ls())
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

library(jsonlite) # used for `parse_json` call below
library(tidyverse)




# GLOBALS ----

DATA_DIR = '../data'
DATA_FILES = c(
  'gcb_test_data_4.csv', # N = 69: prolific (4) + sona (65)
  'gcb_test_data_5.csv', # N = 4: sona only
  'gcb_test_data_6.csv', # N = 57: sona only
  'gcb_test_data_7.csv', # N = 350: sona only
  'gcb_test_data_8.csv', # N = 305: sona only
  'gcb_test_data_9.csv', # N = 12: prolific only
  'gcb_test_data_10.csv' # N = 489: prolific only
)
SURVEY_FILES = c(
  'gcb_survey_data_4.csv',
  'gcb_survey_data_5.csv',
  'gcb_survey_data_6.csv',
  'gcb_survey_data_7.csv',
  'gcb_survey_data_8.csv',
  'gcb_survey_data_9.csv',
  'gcb_survey_data_10.csv'
)


default_plot_theme = theme(
  # text
  plot.title = element_text(size = 24, family = "Avenir", color = "black", margin = margin(b = 0.5, unit = "line")),
  axis.title.y = element_text(size = 24, family = "Avenir", color = "black", margin = margin(r = 0.5, unit = "line")),
  axis.title.x = element_text(size = 24, family = "Avenir", color = "black", margin = margin(t = 0.5, unit = "line")),
  axis.text.y = element_text(size = 18, family = "Avenir", color = "black"),
  axis.text.x = element_text(size = 18, family = "Avenir", color = "black"),
  legend.title = element_text(size = 20, family = "Avenir", color = "black"),
  legend.text = element_text(size = 14, family = "Avenir", color = "black"),
  # backgrounds, lines
  panel.background = element_blank(),
  strip.background = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.minor.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_blank(),
  axis.line = element_line(color = "black"),
  axis.ticks = element_line(color = "black")
)



# READ DATA ----
data_combined = data.frame()
for(i in 1:length(DATA_FILES)) {
  data_combined = data_combined %>%
    rbind(
      read_csv(paste(DATA_DIR, 'experiment', DATA_FILES[i], sep = '/')) %>% mutate(source_file = DATA_FILES[i])
    )
}


# Summarize
data_combined %>%
  group_by(source_file, recruitmentPlatform) %>%
  summarize(n = n_distinct(gameID))

data_combined %>%
  group_by(recruitmentPlatform) %>%
  summarize(n = n_distinct(gameID))



# CLEAN ----
data_combined$graphType[data_combined$graphType=='bubble chart'] = 'bubble_chart'
data_combined$graphType[data_combined$graphType=='stacked_bart_chart'] = 'stacked_bar_chart'
data_combined$origStudy_taskCategorization[data_combined$origStudy_taskCategorization=='determine_Range'] = 'determine_range'
data_combined = data_combined %>% rename(trialNum = "triamNum")


# CALCULATE ACCURACY ----
data_combined$corrAns_list = NA # supporting column for handling fill in the blank responses and improperly formatted multiple choice
data_combined$accuracy = NA


# > Fix improperly formatted multiple choice ----
# These questions have responses that are improperly formatted
# Code below goes through them one by one and (very manually) updates the response value
# Includes checks to make sure the values are expected after updating

item = 'item_39'
unique(data_combined$corrAns[data_combined$gcbID==item]) # sanity check
data_combined$corrAns_list[data_combined$gcbID==item] = list(c('70.5 kg')) # update correct answer
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the accuracy update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))

item = 'item_40'
unique(data_combined$corrAns[data_combined$gcbID==item]) # sanity check
data_combined$corrAns_list[data_combined$gcbID==item] = list(c('197.1 cm')) # update correct answer
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the accuracy update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))

# NB: this is changing previously correct answer (likely a bug in test code)
# See Fig. 2(g) at https://ieeexplore.ieee.org/stamp/stamp.jsp?arnumber=7539634
item = 'item_41'
unique(data_combined$corrAns[data_combined$gcbID==item]) # sanity check
data_combined$corrAns_list[data_combined$gcbID==item] = list(c('53.9 - 123.6 kg')) # update correct answer
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))

item = 'item_43'
unique(data_combined$corrAns[data_combined$gcbID==item]) # sanity check
data_combined$corrAns_list[data_combined$gcbID==item] = list(c('175.3 cm')) # update correct answer
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))

item = 'item_72'
unique(data_combined$corrAns[data_combined$gcbID==item]) # sanity check
data_combined$corrAns_list[data_combined$gcbID==item] = list(c('False')) # update correct answer
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))





# > Fill-in-the-blank questions ----
# This section scores participants' fill in the blank questions
# For each item we print the full set of unique responses,
# then assign scores based on the subset considered correct under each item

#' Scoring these items:
#' From Galesic & Garcia-Retamero (2011), pg. 449:
#' "When calculating participants’ results, we required exactly correct answers
#' to all questions except for question Q7, where we allowed as correct all answers
#' that fell between 23 and 25."


item = 'item_1'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns?
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give?
# choose valid responses from the above (variants of "35%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '35', '35%', '35 percent', '35%^', '35.0'
  ))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))


item = 'item_2'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns?
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give?
# choose valid responses from the above (variants of "15%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '15', '15%', '15 percent', '15.0',
  '15\\',
  '15% DIFFERENT',
  'Only 30% recovered after radiation and 45% recovered after radiation which is a 15% difference.',
  '15 points',
  'The difference is 15%'
  ))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))


item = 'item_3'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns?
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give?
# choose valid responses from the above (variants of "25%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '25', '25%', '25 percent',
  '0.25', '25.0',
  '~25%', 'aprox 25%', 'About 25%', 'about 25', 'Approx 25%',
  'In all the people die from cancer, 25% people die from lung cancer'
  ))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
# NB: number of people above who said e.g. "24.5", "24.9%", "~24%"


item = 'item_4'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns?
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give?
# choose valid responses from the above (variants of "25%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '25', '25%', '~25%', '25 percent', '25.0',
  '2 5',
  '1/4',
  'about 25',
  '.25', '0.25',
  'All together 25%',
  'With all combined approx 25%',
  '25% people who die from cancer die from colon cancer, breast cancer, and prostate cancer taken together'
  ))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
# NB: number of people above who said e.g. "25.5", "24", "24%", "26%", "~26%", "255" (25 typo?)


item = 'item_5'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give
# choose valid responses from the above (variants of "20%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '20', '20%',
  '20 %',
  '0.20', '20.0',
  'Approx 20%',
  'About 20% of people'
  ))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
# NB: number of people above who said e.g. "20.2%", "20.1", "20.5%"


# NB: range correct (inclusive 23-25%)
item = 'item_7'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give
# choose valid responses from the above (variants of "24%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '23%', '23',
  '23.5',
  '24%', '24',
  '24\\',
  '24.8%', '24.5%', '24.5',
  'about 24%',
  'About 24% of people',
  'About 24 or 25% maybe',
  '25', '0.25', '25.0', '25%', '25 %',
  '~25', '~25%',
  'Nearly 25%', 'A little below 25%', 'near 25%'
))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))


item = 'item_8'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give
# choose valid responses from the above (variants of "40%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '40', '40%',
  '4x10=40',
  '40 women and 60 men',
  '40 women',
  '40 patients',
  'out of 100 patients with disease X, only 40 were women.'
))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))


item = 'item_9'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give
# choose valid responses from the above (variants of "20")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '20',
  '20 more', '20 more men then women', '20 more men', 'about 20 more men'
))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))


item = 'item_13'
unique(data_combined$corrAns[data_combined$gcbID==item]) # what is currently corrAns
unique(data_combined$response[data_combined$gcbID==item]) # what are all the answers people give
# choose valid responses from the above (variants of "5%")
data_combined$corrAns_list[data_combined$gcbID==item] = list(c(
  '5', '5%',
  '%5',
  '0.05', '5.0'
))
# update accuracy
data_combined = data_combined %>% rowwise() %>% mutate(accuracy = ifelse(gcbID==item, as.numeric(response %in% corrAns_list), accuracy))
# sanity check the update (make sure no overlap between correct and incorrect)
data_combined %>% filter(gcbID==item, accuracy==1) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))
data_combined %>% filter(gcbID==item, accuracy==0) %>% group_by(response) %>% summarize(n()) %>% arrange(desc(`n()`)) %>% print(n=nrow(.))


# > All remaining questions (multiple choice) ----
# Here, we assign accuracy value to the multiple choice items, which have a correct answer already assigned to each item.

# Remaining questions not handled above: is the corrAns straightforward?
data_combined %>%
  filter(is.na(accuracy)) %>%
  group_by(gcbID, corrAns) %>%
  summarize(n()) %>% print(n=nrow(.))

# Example sanity check for a single item: can we set accuracy to be response == corrAns?
data_combined %>% filter(gcbID == 'item_10', response == corrAns) %>% group_by(response) %>% summarize(n())
data_combined %>% filter(gcbID == 'item_10', response != corrAns) %>% group_by(response) %>% summarize(n())

# Set accuracy to be response == corrAns
data_combined = data_combined %>%
  rowwise() %>%
  mutate(accuracy = ifelse(is.na(accuracy), as.numeric(response == corrAns), accuracy))

# sanity check
sum(is.na(data_combined$accuracy)) # did we miss any accuracy calcs?
glimpse(data_combined) # are all previous rows preserved?


# EXCLUSIONS ----

# add column: questions skipped, questions with no answer
data_combined = data_combined %>%
  group_by(gameID) %>%
  mutate(
    total_skip = sum(response == 'Skip'),
    total_responses = sum(!is.na(response)),
    prop_responses = total_responses / (max(data_combined$trialNum) + 1),
    prop_skip = total_skip / total_responses
  ) %>% ungroup()


# Summarize by subject ID to examine possible exclusions
data_summary = data_combined %>%
  group_by(gameID) %>%
  summarize(
    skip = unique(total_skip),
    total_resp = unique(total_responses),
    prop_skip = unique(prop_skip),
    prop_resp = unique(prop_responses)
  )

# How many people would be excluded by removing those with incomplete assessments?
sum(data_summary$total_resp < 66)
sum(data_summary$total_resp == 66)
sum(!is.na(data_summary$total_resp))


# Among those who didn't complete the full assessment, how many questions did they answer (`total_resp`)?
data_summary %>%
  filter(total_resp < 66) %>%
  arrange(
    total_resp
  ) %>% print(n=nrow(.))


# Figure: distribution of number of complete responses
data_summary %>%
  ggplot(aes(x = total_resp)) +
  geom_histogram() +
  scale_x_continuous(
    name = "Total responses (max. 66)"
  ) +
  default_plot_theme

# Figure: distribution of complete responses for those who didn't complete full assessment
data_summary %>%
  filter(total_resp < 66) %>%
  ggplot(aes(x = total_resp)) +
  geom_histogram() +
  geom_vline(aes(xintercept=66), color = 'red', linetype = 'dashed') +
  scale_x_continuous(
    name = 'Total responses below max.'
  ) +
  default_plot_theme


# Remove people for whom we had < 66 responses, check for technical issues
data_full_resp = data_combined %>%
  filter(total_responses == 66)

# sanity check
glimpse(data_full_resp)
length(unique(data_full_resp$gameID))

# Did any individual fill-in-the-blank responses show evidence of technical issues?
data_full_resp %>%
  filter(questionType == 'fill_in_blank') %>%
  group_by(gcbID, response) %>%
  summarize(n()) %>%
  print(n=nrow(.))

# From print-out above:
  # "Broken
  # "Broken image"
  # "Broken Image"
  # "IMAGE NOT SHOWN broken image"
  # "IMAGE IS NOT SHOWN"


# What subject IDs were associated with the technical issue responses identified above?
data_full_resp %>%
  filter(questionType == 'fill_in_blank',
         response %in% c(
           'Broken',
           'Broken image',
           'Broken Image',
           'IMAGE NOT SHOWN broken image',
           'IMAGE IS NOT SHOWN'
           )) %>%
  group_by(
    gameID, gcbID, response
  ) %>%
  summarize(n())


# Remove subjects identified as having technical issues
data_no_technical = data_full_resp %>%
  filter(gameID != '3089-3bb726da-27cd-40fa-b7c6-da544682b3a7')

# sanity checks
glimpse(data_no_technical)
length(unique(data_no_technical$gameID))




# SURVEY RESPONSES ----

# Read in survey response CSV files
survey_data_combined = data.frame()
for(i in 1:length(SURVEY_FILES)) {
  survey_data_combined = survey_data_combined %>%
    rbind(
      read_csv(paste(DATA_DIR, 'survey', SURVEY_FILES[i], sep = '/')) %>%
        select(gameID, iterationName, version, recruitmentPlatform,
               trial_type, trial_index, responses, response_json) %>%
        mutate(source_file_survey = SURVEY_FILES[i])
    )
}
glimpse(survey_data_combined)


# Handle multiple choice, multiple select, and free text survey response rows separately
survey_multiple_choice = survey_data_combined %>%
  filter(trial_type == 'survey-multi-choice') %>%
  rowwise() %>%
  mutate(x = list(parse_json(responses))) %>%
  unnest_wider(x)
glimpse(survey_multiple_choice)

survey_free_resp = survey_data_combined %>%
  filter(trial_type == 'survey-text') %>%
  rowwise() %>%
  mutate(x = list(parse_json(response_json))) %>%
  unnest_wider(x) %>%
  filter(is.na(Q0)) # remove the Q0 rows (otherwise redundant rows for each person where one is only email info contained in Q0)
glimpse(survey_free_resp)

survey_math_exp = survey_data_combined %>%
  filter(trial_type == 'survey-multi-select') %>%
  rowwise() %>%
  mutate(x = list(parse_json(responses))) %>%
  unnest_wider(x)
# convert list to string representation
survey_math_exp$participantEd_math = gsub(' ', '', survey_math_exp$participantEd_math, fixed = TRUE)
survey_math_exp$participantEd_math = gsub('list(', '', survey_math_exp$participantEd_math, fixed = TRUE)
survey_math_exp$participantEd_math = gsub(')', '', survey_math_exp$participantEd_math, fixed = TRUE)
survey_math_exp$participantEd_math = gsub('\"', '', survey_math_exp$participantEd_math, fixed = TRUE)
survey_math_exp$algebra = as.integer(grepl("Algebra", survey_math_exp$participantEd_math, fixed = TRUE))
survey_math_exp$statistics = as.integer(grepl("Statistics", survey_math_exp$participantEd_math, fixed = TRUE))
survey_math_exp$calculus = as.integer(grepl("Calculus", survey_math_exp$participantEd_math, fixed = TRUE))
survey_math_exp = survey_math_exp %>%
  rowwise() %>%
  mutate(math_sum = sum(algebra, statistics, calculus))
glimpse(survey_math_exp)


# Compare participants in data frames above
length(unique(survey_multiple_choice$gameID))
length(unique(survey_free_resp$gameID))
length(unique(survey_math_exp$gameID))
setdiff(unique(survey_multiple_choice$gameID), unique(survey_free_resp$gameID))
setdiff(unique(survey_math_exp$gameID), unique(survey_free_resp$gameID))
setdiff(unique(survey_multiple_choice$gameID), unique(survey_math_exp$gameID))


# Combine data frames, including the response DF processed above
data_combined = data_no_technical %>%
  left_join(
    survey_multiple_choice %>%
      rename(multiple_choice_trial_type = 'trial_type',
             multiple_choice_trial_index = 'trial_index',
             multiple_choice_source_file = 'source_file_survey') %>%
      select(
        gameID,
        multiple_choice_trial_type, multiple_choice_trial_index, multiple_choice_source_file,
        participantSex, participantEd, judgedDifficulty, participantEffort, technicalDifficultiesBinary),
    by = c('gameID')
  ) %>%
  left_join(
    survey_free_resp %>%
      filter(is.na(Q0)) %>%
      rename(free_resp_trial_type = 'trial_type',
             free_resp_trial_index = 'trial_index',
             free_resp_source_file = 'source_file_survey') %>%
      select(
        gameID,
        free_resp_trial_type, free_resp_trial_index, free_resp_source_file,
        TechnicalDifficultiesFreeResp, participantAge, participantYears, participantEd_field, participantComments),
    by = c('gameID')
  ) %>%
  left_join(
    survey_math_exp %>%
      rename(math_exp_trial_type = 'trial_type',
             math_exp_trial_index = 'trial_index',
             math_exp_source_file = 'source_file_survey') %>%
      select(
        gameID,
        algebra, statistics, calculus, math_sum
      ),
    by = c('gameID')
  )
# Sanity checks
glimpse(data_combined)
length(unique(data_no_technical$gameID))
length(unique(data_combined$gameID))


# Handle technical difficulties
# Print responses in the technical difficulties free resp, arranged by the technical difficulties binary
data_combined %>%
  filter(TechnicalDifficultiesFreeResp != '') %>%
  group_by(technicalDifficultiesBinary, TechnicalDifficultiesFreeResp, gameID) %>%
  summarize(n()) %>%
  print(n=nrow(.))

# Using the above, we manually curate responses below that clearly say there were no technical difficulties
data_combined$technical_issues_category = 'unprocessed'
no_tech_difficulties = c(
  '',
  'None', 'none', 'None.', 'none.', 'none!', 'None. Thank you!',
  'n/a', 'na', 'NA', 'N/A', 'N/a', 'N\\A', 'N?A',
  'no', 'No', 'No.', 'no ', 'nope', 'nay',
  'No issues', 'No issues.',
  'No issues at all.',
  'None, I was fine.',
  'no problems here',
  'No problems!',
  'I did not',
  'I did not encounter difficulties.',
  'I did not encounter technical difficulties',
  'I did not encounter technical difficulties ',
  'I did not encounter technical difficulties.',
  'I did not encounter technical difficulties. ',
  'I did not encounter any technical difficulties ',
  'I did not encounter any technical difficulties during the survey.',
  'I did not encounter any technical difficulties',
  'I did not encounter any technical difficulties.',
  'I did not encounter any technical difficulties. ',
  'I did not encounter an technical difficulties',
  'i did not encounter any technical difficulties',
  'I did not encounter  any technical difficulties',
  "I didn't encounter any technical difficulties during the study. Thank you.",
  'I did not encounter any technical difficulty ',
  'I did not encounter and technical difficulties',
  "I didn't have any technical difficulties.",
  'I did not have any technical difficulties',
  'I did not encounter any technical difficulties,',
  'I did not encounter any technological difficulties',
  'I did not encounter any difficulties',
  'I did  not encounter any difficulties',
  'I did not encounter any difficulties.',
  'I did not encounter any difficulties. ',
  'I did not encounter any difficulty.',
  'I did nt encounter any difficulties',
  'i did not encounter any difficulties',
  'i did not encounter any difficulties.',
  'did not have difficulties',
  'Did not encounter any difficulties.',
  'no difficulties',
  'No difficulties. :)',
  'No technical difficulties',
  'No technical diffuculties ',
  'no technical difficulties',
  'NO technical difficulties. ',
  'No tech difficulties ',
  'no tech difficulties ',
  'no technical issues',
  'There was no technical issues ',
  'no encounters',
  'I did not encounter any',
  'I did not encounter any.',
  'I did not encounter any ',
  'i did not encounter any',
  'I did  not encounter any, ',
  'Did not encounter any ',
  "I didn’t  encounter anything",
  'I did not encounter any issues',
  'I did not encounter any issues.',
  'i did not encounter any issues',
  'I did not encounter any issues',
  'I did not encounter any issues. ',
  'I did not encounter any problems. ',
  "I didn't encounter problems.",
  'Nope, survey worked well.',
  'Nothing!',
  'Nothing to report, thank you',
  'good',
  'All is well.'
)
# Categorize the responses above as clearly not having technical difficulties
data_combined = data_combined %>%
  rowwise() %>%
  mutate(technical_issues_category = ifelse(TechnicalDifficultiesFreeResp %in% no_tech_difficulties, 'no issues', technical_issues_category))


# Filter out the above and look at remaining responses to determine which ones provide clear evidence of technical difficulties
data_combined %>%
  filter(technical_issues_category != 'no issues') %>%
  group_by(technicalDifficultiesBinary, TechnicalDifficultiesFreeResp, gameID) %>%
  summarize(n()) %>%
  print(n=nrow(.))

# After filtering the above and looking at what's left, manually select clear technical difficulty exclusions
tech_difficulties = c(
  'HANNAH',
  "Two of the graphs didn't load. One was the one with the names of babies. I can't remember the topic of the other. ",
  'I believe for number 28 the picture did not show so I clicked skip. Other than that it all worked fine',
  'The graph of the data did not show on question 10, so my answer was random for that one. ',
  'the pictures did not show up on some of them',
  'I do not know if some images had words or not, I selected the skip button when I encountered graphs with no words at all',
  'I will explain this in a separate email, but I experienced a lot of technical difficulties.',
  'some images did not load',
  "I couldn't see 3 of the graphs, so I skipped those questions. ",
  'images did not load sometimes',
  'Some of the images would not load so I had to select skip for those.',
  'Image did not load',
  "Any question I selected 'skip' on was because the image did not load. I tried to right click -> reload the images, but it did not work.",
  "I have emailed Holly Huey all the issues. Link didn't work and the image didn't load. ",
  'A lot of images did not load(Had to skip questions)',
  'Image would not load',
  'The images in a couple of the questions did not load so I had to skip them',
  'For the graphs for the males and females, it did not load for me.',
  "Pictures weren't loading. May have been a wifi issue",
  'Two of the images did not show up properly, they were all pixelated and I only saw large blocks of colors displaying the graphs but I could not read or interpret them. These are the questions  I skipped. ',
  'The images were not loading properly for some reason.'
)
data_combined = data_combined %>%
  rowwise() %>%
  mutate(technical_issues_category = ifelse(TechnicalDifficultiesFreeResp %in% tech_difficulties, 'tech issues', technical_issues_category))

# handle remaining as 'uncategorized'
data_combined = data_combined %>%
  rowwise() %>%
  mutate(technical_issues_category = ifelse(!(technical_issues_category %in% c('no issues', 'tech issues')), 'uncategorized', technical_issues_category))

# sanity check
sum(is.na(data_combined$technical_issues_category))
table(data_combined$technical_issues_category)

# strong sanity check: any responses in the categories above that don't belong?
data_combined %>%
  filter(TechnicalDifficultiesFreeResp != '') %>%
  filter(technical_issues_category == 'no issues') %>%
  group_by(technicalDifficultiesBinary, TechnicalDifficultiesFreeResp, gameID) %>%
  summarize(n()) %>%
  print(n=nrow(.))

data_combined %>%
  filter(technical_issues_category == 'tech issues') %>%
  group_by(technicalDifficultiesBinary, TechnicalDifficultiesFreeResp, gameID) %>%
  summarize(n()) %>%
  print(n=nrow(.))

data_combined %>%
  filter(technical_issues_category == 'uncategorized') %>%
  group_by(technicalDifficultiesBinary, TechnicalDifficultiesFreeResp, gameID) %>%
  summarize(n()) %>%
  print(n=nrow(.))


# Check for additional removals
  # NA for technicalDifficulties binary
data_combined %>%
  filter(is.na(technicalDifficultiesBinary)) %>%
  group_by(gameID) %>% summarize(n())
  # NA for technical difficulties free response
data_combined %>%
  filter(is.na(TechnicalDifficultiesFreeResp)) %>%
  group_by(gameID) %>% summarize(n())
  # NA for math experience
data_combined %>%
  filter(is.na(math_sum)) %>%
  group_by(gameID, algebra, statistics, calculus) %>% summarize(n())


# Remove those identified above as having technical issues or NA values in the technical issues fields
data_combined_final = data_combined %>%
  filter(technical_issues_category != 'tech issues',
         !is.na(technicalDifficultiesBinary),
         !is.na(TechnicalDifficultiesFreeResp),
         !is.na(math_sum))


# Sanity check this process
glimpse(data_combined_final)
n_distinct(data_combined_final$gameID)
sum(is.na(data_combined_final$accuracy))
# check for NAs in survey response columns
sum(is.na(data_combined_final$participantSex))
sum(is.na(data_combined_final$participantEd))
sum(is.na(data_combined_final$judgedDifficulty))
sum(is.na(data_combined_final$participantEffort))
sum(is.na(data_combined_final$technicalDifficultiesBinary))
sum(is.na(data_combined_final$TechnicalDifficultiesFreeResp))
sum(is.na(data_combined_final$participantAge))
sum(is.na(data_combined_final$participantYears))
sum(is.na(data_combined_final$participantEd_field))
sum(is.na(data_combined_final$participantComments))



# WRITE TO CSV ----
write_csv(
  data_combined_final,
  file = paste(DATA_DIR, 'data_processed.csv', sep = '/')
)




# APPENDIX ANALYSES ----

# > APPENDIX: SKIP RESPONDING ----
# What kind of patterns do we see among people who responded "skip" on multiple choice questions?

# Summarize smaller exclusion dataframe by subject ID to examine patterns of skips
data_exclusion_summary = data_no_technical %>%
  group_by(gameID, recruitmentPlatform) %>%
  summarize(
    skip = unique(total_skip),
    prop_skip = unique(prop_skip)
  ) %>% ungroup()


# How many people skipped at least one question? How were they distributed across SONA and prolific?
data_exclusion_summary %>%
  filter(skip > 0) %>%
  group_by(skip, prop_skip, recruitmentPlatform) %>%
  summarize(n()) %>%
  print(n=nrow(.))

# Figure: plot skip distribution
data_exclusion_summary %>%
  ggplot(aes(x = skip, fill = recruitmentPlatform)) +
  geom_histogram(
    alpha = 0.5,
    position = 'identity'
  ) +
  scale_x_continuous(
    name = "Total questions skipped"
  ) +
  scale_fill_discrete(
    name = "Sample population"
  ) +
  default_plot_theme

# Figure: examine people at high end of skip count distribution
data_exclusion_summary %>%
  filter(skip > 10) %>%
  ggplot(aes(x = skip, fill = recruitmentPlatform)) +
  geom_histogram(
    alpha = 0.5,
    position = 'identity'
  ) +
  scale_x_continuous(
    name = "Total questions skipped"
  ) +
  scale_fill_discrete(
    name = "Sample population"
  ) +
  default_plot_theme


# > APPENDIX: SKIP RESPONDING + RESPONSE TIME ----
# Can we get any insight into skip behavior from looking at response times?
# (i.e., can we differentiate "thoughtful skipping" from rushing through the experiment?)

# Log transform response time, calculate subject average RT alongside skips
rt_summary = data_no_technical %>%
  # mutate(log_rt = log10(rt)) %>%
  mutate(log_rt = rt) %>%
  group_by(gameID) %>%
  summarize(
    skip = unique(total_skip),
    prop_skip = unique(prop_skip),
    mean_rt = mean(log_rt),
    n = n(),
    sd_rt = sd(log_rt),
    se_rt = sd_rt / sqrt(n),
    sum_rt = sum(log_rt)
  )


# What do skip counts look like among people with the lowest RTs?
rt_summary %>%
  arrange(desc(sum_rt))

data_no_technical %>%
  arrange(rt) %>% glimpse() %>% print(n=nrow(.))

# Figure: distribution of response time among those with very low RTs
data_no_technical %>%
  filter(rt < 10000) %>%
  ggplot(aes(x = rt)) +
  geom_histogram(
    alpha = 0.5
  ) +
  default_plot_theme


# What do RTs look like for people who skip the most?
rt_summary %>%
  arrange(desc(skip)) %>%
  print(n=nrow(.))


# Figure: distribution of mean RTs
rt_summary %>%
  ggplot(aes(x = log10(mean_rt))) +
  geom_histogram() +
  scale_x_continuous(
    name = "Subjects' log mean question response time"
  ) +
  default_plot_theme


# What does it look like at the low end?
rt_summary %>%
  # filter(mean_rt < 4.0) %>%
  filter(mean_rt < 20000) %>%
  ggplot(aes(x = mean_rt)) +
  geom_histogram() +
  scale_x_continuous(
    name = "Subjects' mean question response time"
  ) +
  default_plot_theme


# Figure: relationship between RT and proportion skipped
rt_summary %>%
  ggplot(aes(x = skip, y = mean_rt)) +
  geom_point() +
  scale_x_continuous(
    name = "Total questions skipped"
  ) +
  scale_y_continuous(
    name = "Subjects' mean question response time"
  ) +
  default_plot_theme

# People who skipped large numbers of questions are clearly low in RT,
# but it's not necessarily lower than others who skipped far fewer Qs


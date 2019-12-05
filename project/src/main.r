library(glue)
library(lubridate)
library(RColorBrewer)
library(scales)
library(tidyverse)

source('baseline.r')
source('metrics.r')
source('plots.r')

save_submission <- function(submission) {
    write.csv(submission, file = 'submission.csv', quote = FALSE, row.names = FALSE)
}

is_assessment_attempt <- function(title, event_code) {
    (title == 'Bird Measurer (Assessment)' & event_code == 4110) |
        (title != 'Bird Measurer (Assessment)' & title %in% unique_assessment_titles & event_code == 4100)
}

accuracy_group <- function(correct_attempts, total_attempts) {
    case_when(
        correct_attempts == 0 ~ 0,
        correct_attempts == 1 & total_attempts >= 3 ~ 1,
        correct_attempts == 1 & total_attempts == 2 ~ 2,
        correct_attempts == 1 & total_attempts == 1 ~ 3
    )
}

named_zeros_list <- function(names) {
    l <- as.list(integer(length(names)))
    names(l) <- names
    l
}

# Load data

train <- data.table::fread('../input/data-science-bowl-2019/train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('../input/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
test <- data.table::fread('../input/data-science-bowl-2019/test.csv', stringsAsFactors = F)
specs <- data.table::fread('../input/data-science-bowl-2019/specs.csv', stringsAsFactors = F)

# Take only those records which contain at leat one assessment

train <- train %>%
    filter(type == 'Assessment') %>% 
    distinct(installation_id) %>% 
    left_join(train, by = 'installation_id')

### Dataset visualisation

configure_plot_theme()
events_by_type_plot(train)
activity_time_plot(train)
accuracy_groups_plot(train_labels)

### Baseline model

baseline_predictions(train_labels, test) %>% save_submission

### Feature engineering

# Change timestamp to ymd date, weekday and hour
train <- train %>% 
    mutate(
        timestamp = timestamp %>% ymd_hms,
        date_stamp = gsub( ' .*$', '', timestamp),
        activity_wday = wday(timestamp, label = T),
        activity_hour = hour(timestamp)
    )

unique_event_codes <- unique(c(unique(train$event_code), unique(test$event_code)))
unique_event_ids <- unique(c(unique(train$event_id), unique(test$event_id)))
unique_worlds <- unique(c(unique(train$world), unique(test$world)))
unique_titles <- unique(c(unique(train$title), unique(test$title)))
unique_assessment_titles <- train %>% filter(type == 'Assessment') %>% pull(title) %>% unique

summarise_player <- function(df) {
    last_activity <- 0
    type_counts <- c('clip' = 0, 'activity' = 0, 'assessment' = 0, 'game' = 0)
    
    # time spent in each activity
    last_session_time_sec <- 0
    accuracy_groups <- c('0' = 0, '1' = 0, '2' = 0, '3' = 0)
    all_assessments <- c()
    accumulated_accuracy_group  <- 0
    accumulated_accuracy <- 0
    accumulated_correct_attempts <- 0 
    accumulated_uncorrect_attempts <- 0
    accumulated_actions <- 0
    counter <- 0
    
    time_of_first_activity <- df %>% select(timestamp) %>% first
    durations <- c()
    # last_accuracy_title
    event_code_counts <- named_zeros_list(unique_event_codes)
    event_id_counts <- named_zeros_list(unique_event_ids)
    title_counts <- named_zeros_list(unique_titles)
    
    df %>% group_by(game_session) %>% summarise(summary = summarise_game_session)
}

summarise_game_session <- function(df) {
    df %>% 
        filter(is_assessment_attempt(title, event_code)) %>%
        mutate(attempt = as.numeric(grepl('""correct"":true', event_data))) %>%
        select(game_session, event_id, event_code, attempt) %>%
        group_by(game_session) %>%
        summarize(
            correct_attempts = sum(attempt),
            incorrect_attempts = n() - sum(attempt),
            total_attempts = n(),
            accuracy = sum(attempt) / n()
        ) %>%
        mutate(accuracy_group = accuracy_group(correct_attempts, total_attempts)) %>%
        select(game_session, correct_attempts, incorrect_attempts, accuracy, accuracy_group)
}

### Evaluation

score_assessments <- function(df) {
    df %>% 
        filter(is_assessment_attempt(title, event_code)) %>%
        mutate(attempt = as.numeric(grepl('""correct"":true', event_data))) %>%
        select(game_session, event_id, event_code, attempt) %>%
        group_by(game_session) %>%
        summarize(
            correct_attempts = sum(attempt),
            incorrect_attempts = n() - sum(attempt),
            total_attempts = n(),
            accuracy = sum(attempt) / n()
        ) %>%
        mutate(accuracy_group = accuracy_group(correct_attempts, total_attempts)) %>%
        select(game_session, correct_attempts, incorrect_attempts, accuracy, accuracy_group)
}

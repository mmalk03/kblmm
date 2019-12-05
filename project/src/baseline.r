library(tidyverse)

source('plots.r')
source('submission.r')

# Load data

train <- data.table::fread('../input/data-science-bowl-2019/train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('../input/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
test <- data.table::fread('../input/data-science-bowl-2019/test.csv', stringsAsFactors = F)

# Take only those records which contain at leat one assessment

train <- train %>%
    filter(type == 'Assessment') %>% 
    distinct(installation_id) %>% 
    left_join(train, by = 'installation_id')

# Dataset visualisation

configure_plot_theme()
events_by_type_plot(train)
activity_time_plot(train)
accuracy_groups_plot(train_labels)

# Baseline model

median_accuracy_groups <- train_labels %>%
    group_by(title) %>%
    summarise(accuracy_group = median(accuracy_group, na.rm = T)) %>% ungroup()

last_assessments <- test %>%
    filter(type == 'Assessment') %>%
    arrange(installation_id, desc(timestamp)) %>%
    distinct(installation_id, .keep_all = T) %>%
    select(installation_id, title) %>% 
    left_join(median_accuracy_groups, by = 'title') %>% 
    select(-title) %>%
    save_submission

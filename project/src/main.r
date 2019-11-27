library(glue)
library(lubridate)
library(RColorBrewer)
library(scales)
library(tidyverse)

save_submission <- function(submission) {
    write.csv(submission, file = 'submission.csv', quote = FALSE, row.names = FALSE)
}

# Configure plotting options
colors <- brewer.pal(6, 'YlGnBu')
theme_set(
    theme_minimal() + theme(
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12)
    )
)

# Load data

train <- data.table::fread('../input/data-science-bowl-2019/train.csv', stringsAsFactors = F)
train_labels <- data.table::fread('../input/data-science-bowl-2019/train_labels.csv', stringsAsFactors = F)
test <- data.table::fread('../input/data-science-bowl-2019/test.csv', stringsAsFactors = F)

train <- train %>%
    filter(type == 'Assessment') %>% 
    distinct(installation_id) %>% 
    left_join(train, by = 'installation_id')

# Baseline model

last_assessments <- test %>%
    filter(type == 'Assessment') %>%
    arrange(installation_id, desc(timestamp)) %>%
    distinct(installation_id, .keep_all = T) %>%
    select(installation_id, title)

median_accuracy_groups <- train_labels %>%
    group_by(title) %>%
    summarise(accuracy_group = median(accuracy_group, na.rm = T)) %>% ungroup()

last_assessments %>% 
    left_join(median_accuracy_groups, by = 'title') %>% 
    select(-title) %>%
    save_submission

# Feature engineering

train <- train %>% 
    mutate(
        timestamp = timestamp %>% ymd_hms,
        date_stamp = gsub( ' .*$', '', timestamp),
        activity_wday = wday(timestamp, label = T),
        activity_hour = hour(timestamp)
    ) %>% 
    select(-timestamp)

# Dataset visualisation

# Number of events by type
train %>% 
    count(type) %>%
    ggplot(aes(x = reorder(type, n), y = n)) +
    geom_col(colour = 'black', fill = colors[4], alpha = 0.7) +
    scale_y_continuous(labels = comma) +
    labs(x = 'Event type', y = 'Number of Events') +
    coord_flip()

third_accuracy_group_counts <- train_labels %>% filter(accuracy_group == 3) %>% count(title) %>% arrange(n)
train_labels %>% 
    count(accuracy_group, title) %>% 
    ggplot(aes(x = factor(title, levels = third_accuracy_group_counts$title), y = n, fill = as.character(accuracy_group))) +
    geom_bar(stat = 'identity', position = 'fill', colour = 'black') +
    labs(x = 'Assessment', y = 'Count') +
    scale_fill_manual(values = colors, name = 'Accuracy group') +
    scale_y_continuous(labels = percent) +
    scale_x_discrete(labels = function(x) { sub(' \\(Assessment\\)', '', x) }) +
    coord_flip() +
    theme(legend.position = 'bottom')

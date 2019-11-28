baseline_predictions <- function(train_labels, test) {
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
        select(-title)
}

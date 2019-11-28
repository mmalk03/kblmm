configure_plot_theme <- function() {
    theme_set(
        theme_minimal() + theme(
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12)
        )
    )
}

colors <- brewer.pal(6, 'YlGnBu')

# Number of events by type
events_by_type_plot <- function(train) {
    train %>% 
        count(type) %>%
        ggplot(aes(x = reorder(type, n), y = n)) +
        geom_col(colour = 'black', fill = colors[4], alpha = 0.7) +
        scale_y_continuous(labels = comma) +
        labs(x = 'Event type', y = 'Number of Events') +
        coord_flip()
}

accuracy_groups_plot <- function(train_labels) {
    third_accuracy_group_counts <- train_labels %>%
        filter(accuracy_group == 3) %>%
        count(title) %>%
        arrange(n)

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
}

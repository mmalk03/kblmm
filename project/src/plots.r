library(ggplot2)
library(RColorBrewer)

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

events_by_type_plot <- function(train) {
    train %>% 
        count(type) %>%
        ggplot(aes(x = reorder(type, n), y = n)) +
        geom_col(colour = 'black', fill = colors[4], alpha = 0.7) +
        scale_y_continuous(labels = comma) +
        labs(x = 'Event type', y = 'Number of Events') +
        coord_flip()
}

activity_time_plot <- function(train) {
    train %>% 
        filter(type != 'Clip') %>% 
        ggplot(aes(x = log1p(game_time), fill = type)) +
        geom_density(colour = 'black', alpha = 0.5) +
        scale_fill_manual(values = c(colors[3], colors[1], colors[5]), name = 'Type') +
        theme(legend.position = 'bottom')
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

feature_importance_plot <- function(importance_matrix) {
    importance_matrix %>% 
        select(Feature, Gain) %>% 
        arrange(-Gain) %>% 
        head(n = 10) %>% 
        ggplot(aes(x = reorder(Feature, Gain), y = Gain)) +
        geom_col(colour = 'black', fill = colors[3], alpha = 0.7) +
        labs(x = 'Feature', y = 'Importance') +
        coord_flip()
}

xgboost_training_plot <- function(evaluation_log) {
    rbind(
        evaluation_log %>%
            as.data.frame %>%
            select(iter, train_qwk_mean, train_qwk_std) %>%
            setNames(c('iter', 'qwk_mean', 'qwk_std')) %>% 
            add_column(split = rep('train', nrow(evaluation_log))),
        evaluation_log %>%
            as.data.frame %>%
            select(iter, test_qwk_mean, test_qwk_std) %>%
            setNames(c('iter', 'qwk_mean', 'qwk_std')) %>% 
            add_column(split = rep('test', nrow(evaluation_log)))
    ) %>%
        ggplot(aes(x = iter, y = qwk_mean, color = split), colors = colors) +
        geom_line() +
        geom_point() +
        geom_errorbar(aes(ymin = qwk_mean - qwk_std, ymax = qwk_mean + qwk_std), width = .1) +
        labs(x = 'Iteration', y = 'Mean quadratic weighted kappa') +
        scale_color_manual(values = c(colors[3], colors[6])) +
        theme(legend.position = 'bottom')
}

svm_optimisation_plot <- function(tuning_result) {
    tuning_data <- generateHyperParsEffectData(tuning_result)
    min_kappa <- min(tuning_data$data$wkappa.test.mean)
    max_kappa <- max(tuning_data$data$wkappa.test.mean)
    midpoint_kappa <- mean(c(min_kappa, max_kappa))
    
    plotHyperParsEffect(
        tuning_data,
        x = 'C',
        y = 'sigma',
        z = 'wkappa.test.mean',
        plot.type = 'heatmap',
        interpolate = 'regr.earth',
        show.experiments = TRUE
    ) +
        scale_fill_gradient2(
            breaks = seq(min_kappa, max_kappa, length.out = 5) %>% round(4),
            low = colors[1],
            mid = colors[3],
            high = colors[6],
            midpoint = midpoint_kappa
        )
}

kappa_through_time <- function(tuning_result) {
    plotHyperParsEffect(
        generateHyperParsEffectData(tuning_result),
        x = 'iteration',
        y = 'wkappa.test.mean',
        plot.type = 'line'
    )
}

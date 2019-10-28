library(kernlab)
library(tidyr)
library(magrittr)
library(dplyr)
library(clusterSim)
library(ggplot2)
library(ggbiplot)

make_dataset <- function(shapes_producer) {
    shapes_producer() %>%
        as.data.frame %>%
        setNames(c('x', 'y', 'class')) %>%
        mutate(x = as.numeric(x)) %>%
        mutate(y = as.numeric(y)) %>%
        mutate(class = as.factor(class))
}

plot_points <- function(dataset) {
    p <- ggplot(dataset, aes(x = x, y = y, colour = class)) +
        geom_point() +
        theme_minimal()
    print(p)
}

plot_pca <- function(model, classes) {
    model$x %>%
        as.data.frame %>%
        setNames(c('x', 'y')) %>%
        mutate(class = classes) %>%
        plot_points
}

plot_1d_pca <- function(model, classes) {
    model$x %>%
        as.data.frame %>%
        setNames(c('x')) %>%
        mutate(y = x) %>%
        mutate(class = classes) %>%
        plot_points
}

plot_2d_pca <- function(model, classes) {
    model$x %>%
        as.data.frame %>%
        setNames(c('x', 'y')) %>%
        mutate(class = classes) %>%
        plot_points
}

plot_pca_biplot <- function(model, classes) {
    p <- model %>%
        ggbiplot(groups = classes) +
        ggtitle('Principal Component Analysis') +
        theme_minimal() +
        theme(legend.position = 'bottom')
    print(p)
}

plot_1d_kpca <- function(model, classes) {
    p <- model %>%
        rotated %>% 
        as.data.frame %>%
        ggplot(aes(x = V1, y = V1)) +
        geom_point(col = classes) +
        ggtitle('Kernel Principal Component Analysis') +
        theme_minimal() +
        theme(legend.position = 'bottom')
    print(p)
}

plot_2d_kpca <- function(model, classes) {
    p <- model %>%
        rotated %>% 
        as.data.frame %>%
        ggplot(aes(x = V1, y = V2)) +
        geom_point(col = classes) +
        ggtitle('Kernel Principal Component Analysis') +
        theme_minimal() +
        theme(legend.position = 'bottom')
    print(p)
}

train_pca <- function(dataset, features) {
    dataset %>%
        dplyr::select(x, y) %>%
        prcomp(center = TRUE, scale. = TRUE, rank. = 2) %T>%
        { print(summary(.)) }
}

train_kpca <- function(dataset, features) {
    dataset %>% 
        dplyr::select(x, y) %>% 
        kpca(~., data = ., features = features, kernel = 'rbfdot', kpar = list(sigma = 3))
}

# Following for loop will generate the same plots as seen in the report
# for (dataset_producer in c(shapes.bulls.eye, shapes.circles3)) {
#     dataset <- dataset_producer %>% make_dataset %T>% plot_points
#     dataset %>% train_pca(features = 2) %>% plot_2d_pca(dataset$class)
#     dataset %>% train_pca(features = 1) %>% plot_1d_pca(dataset$class)
#     dataset %>% train_kpca(features = 2) %>% plot_2d_kpca(dataset$class)
#     dataset %>% train_kpca(features = 1) %>% plot_1d_kpca(dataset$class)
# }

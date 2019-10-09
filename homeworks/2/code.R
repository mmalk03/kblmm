library(kernlab)
library(ggplot2)
library(patchwork)
library(tidyr)
library(dplyr)
library(tidyverse)
library(reshape2)
library(kernlab)
library(caret)
library(modelr)
library(broom)
library(GGally)

# Exploratory Data Analysis

yachts <- read.table('yacht_hydrodynamics.data')
head(yachts)

yachts %>%
    gather(key='var', value='value') %>%
    ggplot(aes(x = value)) + geom_histogram() + facet_wrap(~ var, scales = 'free')

yachts %>%
    gather(-V7, key='var', value='value') %>%
    ggplot(aes(x = value, y = V7)) + geom_point() + facet_wrap(~ var, scales = 'free')

ggplot(yachts, aes(x = V6, y = V7)) + geom_boxplot(aes(group=cut_number(V6, 14)))

ggpairs(yachts)

yachts %>%
    cor() %>%
    melt() %>%
    ggplot(aes(x = Var1, y = Var2, fill = value)) + geom_tile()

# Model training

train_indices <- yachts$V7 %>% createDataPartition(p = 0.8, list = FALSE)
yachts_train <- yachts[train_indices, ]
yachts_test <- yachts[-train_indices, ]

plot_regression <- function(model) {
    y_hat <- predict(model, yachts_test)
    ggplot(mapping = aes(x = yachts_test$V7, y_hat)) + geom_point() + geom_abline(color='red')
}

svm_regression_r2 <- function(kernel, c, epsilon) {
    
    k = 10
    folds <- cut(seq(1, nrow(yachts_train)), breaks=k, labels=FALSE)
    r2s <- rep(0, k)
    rmses <- rep(0, k)
    
    for(i in 1:10) {
        test_indices <- which(folds == i, arr.ind = TRUE)
        test_dataset <- yachts_train[test_indices, ]
        train_dataset <- yachts_train[-test_indices, ]
        
        model <- ksvm(V7 ~ ., train_dataset, type = 'eps-svr', kernel = kernel, C = c, epsilon = epsilon)
        predicted <- predict(model, test_dataset)
        r2s[i] <- R2(predicted, test_dataset$V7)
        rmses[i] <- RMSE(predicted, test_dataset$V7)
    }
    
    c(r2 = mean(r2s), rmse = mean(rmses))
}

grid_search <- list(
    kernel = c(rbfdot(), anovadot(), polydot(), polydot(degree = 2), polydot(degree = 3)),
    c = seq(0.5, 1.5, 0.25),
    epsilon = c(0.01, 0.05, 0.1, 0.20, 0.40)
) %>%
    cross_df() %>%
    mutate(metrics = pmap(., svm_regression_r2)) %>%
    unnest_wider(metrics)

best_row <- grid_search %>% arrange(rmse, -r2) %>% filter(row_number() == 1)
model <- ksvm(V7 ~ ., yachts_train, type = 'eps-svr', kernel = anovadot(), C = best_row$c, epsilon = best_row$epsilon)
plot_regression(model)

# Feature selection

predicted <- predict(model, yachts_test)
yachts_test %>%
    ggplot(aes(V6)) +
    geom_point(aes(y = V7, colour = 'V7 - true')) +
    geom_point(aes(y = predicted, colour = 'V7 - predicted'))

model <- ksvm(V7 ~ V6, yachts_train, type = 'eps-svr', kernel = anovadot(), C = best_row$c, epsilon = best_row$epsilon)
predicted <- predict(model, yachts_test)
r2 <- R2(predicted, yachts_test$V7)
rmse <- RMSE(predicted, yachts_test$V7)

library(data.table)
library(dplyr)
library(glue)
library(mltools)
library(pls)
library(ramify)

set.seed(1234)

# Load and sample data
train_df <- read.table('zip_train.dat') %>% sample_frac(0.05)
test_df <- read.table('zip_test.dat')

# Center training data and one-hot encode labels
x_train <- train_df %>% select(-V1) %>% scale %>% as.matrix
y_train <- train_df %>%
    select(V1) %>%
    mutate(V1 = as.factor(V1)) %>%
    as.data.table %>%
    one_hot %>%
    as.matrix

n <- nrow(x_train)
p <- ncol(x_train)
q <- ncol(y_train)

# Center test data with respect to training data and one-hot encode labels
x_test <- test_df %>%
    select(-V1) %>%
    scale(
        center = attr(x_train, 'scaled:center'),
        scale = attr(x_train, 'scaled:scale')
    ) %>% 
    as.matrix
y_test <- test_df %>%
    select(V1) %>%
    mutate(V1 = as.factor(V1)) %>%
    as.data.table %>%
    one_hot %>%
    as.matrix

# Partial Least Squares Regression
plsr_model <- plsr(y_train ~ x_train, ncomp = 25, validation = 'LOO')

summary(plsr_model)
nd <- 15

RMSEP(plsr_model)
plot(RMSEP(plsr_model), legendpos = 'topright')
abline(v = nd, col = 'red', lty = 'dotted')

R2(plsr_model)
plot(R2(plsr_model), legendpos = 'bottomright')
abline(v = nd, col = 'red', lty = 'dotted')

# Prediction on the test set
y_pred <- predict(plsr_model, ncomp = nd, newdata = x_test) %>% 
    argmax %>% 
    data.frame %>%
    setNames(c('V1')) %>%
    mutate(V1 = V1 - 1) %>%
    mutate(V1 = factor(V1, levels = seq(0, 9))) %>%
    as.data.table %>%
    one_hot %>%
    as.matrix

# Calculate mean R ^ 2
r2s <- sapply(seq(1, 10), function(i) { cor(y_pred[, i], y_test[, i]) ^ 2 })
print(glue("Mean R^2 on test dataset: {round(mean(r2s), 4)}"))  # 0.599

# Calculate mean error
error <- mean(argmax(y_pred) != argmax(y_test))
print(glue("Error on test dataset: {round(error, 4)}"))  # 0.1938

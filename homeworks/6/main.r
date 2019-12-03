library(data.table)
library(dplyr)
library(glue)
library(mltools)
library(plsdepot)
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

# We can't use interbat because the training matrix is not full rank
iba_model <- interbat(x_train, y_train)

# Perform Inter-Battery Analysis by hand
N <- diag(n) / n
R12 <- (t(x_train) %*% N %*% y_train) / n
A <- eigen(R12 %*% t(R12))$vectors
B <- eigen(t(R12) %*% R12)$vectors

# Analyse how many components to retain
gamma <- t(A) %*% R12 %*% B
nd <- sum(eigen(gamma %*% t(gamma))$values > 1e-8)

A <- A[, 1:nd]
Th <- x_train %*% A
B <- A %*% (t(Th) %*% N %*% Th) %*% t(Th) %*% N %*% y_train

# Predict values for the test set and one-hot encode the classification answers
y_pred <- x_test %*% B %>%
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
r2s[9] <- 0  # none of the classes were predicted as 9 so R2 for 9th class is NA
print(glue("Mean R^2 on test dataset: {round(mean(r2s), 4)}"))  # 0.1759

# Calculate mean error
error <- mean(argmax(y_pred) != argmax(y_test))
print(glue("Error on test dataset: {round(error, 4)}"))  # 0.5286

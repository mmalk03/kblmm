library(calibrate)
library(glue)
library(pls)
library(plsdepot)
library(readr)
library(tidyverse)

plot_individuals <- function(pc, nd = 0) {
    if (nd == 0) nd = ncol(pc)
    Psi = pc[, 1:nd]
    iden = rownames(Psi)
    plot(Psi[, 1], Psi[, 2], type = 'n', asp = 1, main = 'Plot of individuals')
    text(Psi[, 1], Psi[, 2], labels = iden)
    abline(h = 0, v = 0, col = 'cyan')
}

# Read data (ALL: 0, AML: 1)

train <- read_delim('data_set_ALL_AML_train.csv', delim = ';', escape_double = TRUE) %>%
    as.data.frame %>% 
    select(-starts_with('call')) %>% 
    select(-starts_with('Gene')) %>%
    t %>%
    as.data.frame %>% 
    rownames_to_column('y') %>%
    mutate(y = ifelse(as.numeric(y) <= 27, 0, 1))
train.x <- train %>% select(-y)
train.y <- train %>% select(y)

test <- read_delim('data_set_ALL_AML_independent.csv', delim = ';', escape_double = TRUE) %>%
    as.data.frame %>% 
    select(-starts_with('call')) %>% 
    select(-starts_with('Gene')) %>%
    t %>%
    as.data.frame %>% 
    rownames_to_column('y') %>%
    mutate(y = ifelse(as.numeric(y) %in% c(39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 55, 56, 59, 67, 68, 69, 70, 71, 72), 0, 1))
test.x <- test %>% select(-y)
test.y <- test %>% select(y)

# PLS1 regression

plsr_model <- plsr(y ~ ., ncomp = 10, data = train, validation = 'LOO')
summary(plsr_model)
nd <- 4

RMSEP(plsr_model)
plot(RMSEP(plsr_model), legendpos = 'topright')
abline(v = nd, col = 'red', lty = 'dotted')

R2(plsr_model)
plot(R2(plsr_model), legendpos = 'bottomright')
abline(v = nd, col = 'red', lty = 'dotted')

# Projecting test data as supplementary individuals

test.x <- test %>% 
    select(-y) %>%
    scale(center = apply(train.x, 2, mean), scale = F) %*%
    plsr_model$loadings[, 1:nd]

plot_individuals(plsr_model$scores)
text(test.x, labels = rownames(test), col = 'red')

# Logistic regression model

train.x <- plsr_model$scores[, 1:nd] %>% as.data.frame %>% setNames(c('c1', 'c2', 'c3', 'c4'))
train.y <- ifelse(train.y == 0, 'ALL', 'AML') %>% as.factor

test.x <- test.x %>% as.data.frame %>% setNames(c('c1', 'c2', 'c3', 'c4'))
test.y <- ifelse(test.y == 0, 'ALL', 'AML') %>% as.factor

glm_model <- glm(train.y ~ c1 + c2 + c3 + c4, data = train.x, family = binomial(link = 'logit'))
summary(glm_model)

y_pred <- predict(glm_model, newdata = test.x, type = 'response') %>% round
y_pred <- ifelse(y_pred == 0, 'ALL', 'AML') %>% as.factor
accuracy <- sum(diag(table(y_pred, test.y))) / sum(table(y_pred, test.y))
print(glue("Accuracy: {round(accuracy, 4)}"))

percent_with_aml <- sum(y_pred == 'AML') / length(y_pred)
print(glue("Probability of AML leukemia in the test sample: {round(percent_with_aml, 4)}"))

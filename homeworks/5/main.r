library(calibrate)
library(data.table)
library(dplyr)
library(FactoMineR)
library(glue)
library(mltools)
library(pls)
library(ramify)
library(tidyr)

set.seed(1234)

# 1
train_df <- read.table('zip_train.dat') %>% sample_frac(0.05)
test_df <- read.table('zip_test.dat')

# 2
x_train <- train_df %>% select(-V1) %>% scale %>% as.matrix
y_train <- train_df %>%
    select(V1) %>%
    mutate(V1 = as.factor(V1)) %>%
    as.data.table %>%
    one_hot %>%
    as.matrix

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

# 3
mvr_model <- lm(y_train ~ x_train)
r2s <- sapply(attributes(summary(mvr_model))$names, function(m) { summary(mvr_model)[[m]]$r.squared })
print(glue("Average R^2 on train dataset: {round(mean(r2s), 4)}"))
# Output: Average R^2 on train dataset: 0.9242

# 4
press <- colSums((mvr_model$residuals / (1 - ls.diag(mvr_model)$hat)) ^ 2)
r2s_loo <- 1 - press / (diag(var(y_train)) * (length(x_train) - 1))
print(glue("Average R^2 obtained by LOOCV on train dataset: {round(mean(r2s_loo), 4)}"))
# Output: Average R^2 obtained by LOOCV on train dataset: 0.9748

# 5
# y_test_pred <- predict(mvr_model, newdata = as.data.frame(x_test))
y_test_pred <- cbind(rep(0, nrow(x_test)), x_test) %*% coef(mvr_model)
# TODO: compute average R^2s
r2s_loo_test <- r2s_loo
print(glue("Average R^2 obtained by LOOCV on test dataset: {round(mean(r2s_loo_test), 4)}"))

# 6
error <- mean(argmax(y_test_pred) != argmax(y_test))
print(glue("Error on test dataset: {round(error, 4)}"))
# Output: Error on test dataset: 0.3876

# 7
model_pcr <- pcr(y_train ~ x_train, validation = 'LOO')

# We experimentally choose 100 components
n_components <- 100

# The choice is made based on using different values and observing the behaviour of plots and tables below
plot(R2(model_pcr, ncomp = n_components))
RMSEP(model_pcr, ncomp = n_components)
R2(model_pcr, ncomp = n_components)
plot(apply(R2(model_pcr, ncomp = n_components)$val[1, , ], 2, mean), type = 'l')

# 8
model_mvr_pcr <- lm(y_train ~ model_pcr$scores[, 1:n_components])
r2s <- sapply(attributes(summary(model_mvr_pcr))$names, function(m) { summary(model_mvr_pcr)[[m]]$r.squared })
print(glue("Average R^2 on train dataset: {round(mean(r2s), 4)}"))
# Output: Average R^2 on train dataset: 0.7582
# Comment: the value of R^2 on train dataset is lower than using original data, but maybe this model will generalise better to the test data
# Also, we can achieve comaparable R^2 if we use all 256 principal components - the average R^2 increases with the number of principal components

press <- colSums((model_mvr_pcr$residuals / (1 - ls.diag(model_mvr_pcr)$hat)) ^ 2)
r2s_loo <- 1 - press / (diag(var(y_train)) * (length(model_pcr$scores[, 1:n_components]) - 1))
print(glue("Average R^2 obtained by LOOCV on train dataset: {round(mean(r2s_loo), 4)}"))
# Output: Average R^2 obtained by LOOCV on train dataset: 0.9939
# Comment: Interestingly, when using Leave-One-Out CV the average R^2 is really high

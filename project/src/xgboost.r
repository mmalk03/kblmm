library(mlr)
library(parallel)
library(parallelMap)
library(tidyverse)
library(xgboost)

source('metrics.r')
source('plots.r')
source('submission.r')

configure_plot_theme()

# Load data

train_data <- data.table::fread('../input/data-science-bowl-2019/summarised_train.csv', stringsAsFactors = F)
test_data <- data.table::fread('../input/data-science-bowl-2019/summarised_test.csv', stringsAsFactors = F)

train.x <- train_data %>% select(-accuracy_group, -installation_id) %>% as.matrix
train.y <- train_data %>% select(accuracy_group) %>% as.matrix
test.x <- test_data %>% select(-accuracy_group, -installation_id) %>% as.matrix

dtrain <- xgb.DMatrix(data = train.x, label = train.y)

# Cross-validation with default parameters

xgboost_qudratic_weighted_kappa <- function(y_pred, dtrain) {
    list(metric = 'qwk', value = quadratic_kappa(getinfo(dtrain, 'label'), y_pred))
}

params <- list(objective = 'multi:softmax', num_class = 4)

xgb_cv_model <- xgb.cv(
    params = params,
    data = dtrain,
    nrounds = 100,
    nfold = 5,
    showsd = TRUE,
    stratified = TRUE,
    early_stopping_rounds = 5,
    feval = xgboost_qudratic_weighted_kappa,
    maximize = TRUE
)

xgboost_training_plot(xgb_cv_model$evaluation_log)

# Hyperparameter tuning

train_task <- train.x %>%
    data.frame %>%
    add_column(target = train.y) %>% 
    makeClassifTask(data = ., target = 'target')
xgboost_classification_learner <- makeLearner(
    'classif.xgboost',
    predict.type = 'response'
)
xgboost_classification_learner$par.vals <- list(
    objective = 'multi:softmax',
    num_class = 4,
    nround = 50,
    early_stopping_rounds = 5,
    feval = xgboost_qudratic_weighted_kappa,
    maximize = TRUE
)
parameters <- makeParamSet(
    makeIntegerParam('max_depth', lower = 2, upper = 6),
    makeNumericParam('min_child_weight', lower = 1, upper = 6),
    makeNumericParam('subsample', lower = .5, upper = 1),
    makeNumericParam('colsample_bytree', lower = .5, upper = 1)
)
stratified_cv_resampling <- makeResampleDesc('CV', stratify = TRUE, iters = 5)
random_search_strategy <- makeTuneControlRandom(maxit = 10)

parallelStart(mode = 'multicore', cpus = 6, level = 'mlr.tuneParams')
tuning_result <- tuneParams(
    learner = xgboost_classification_learner,
    task = train_task,
    resampling = stratified_cv_resampling,
    measures = wkappa,
    par.set = parameters,
    control = random_search_strategy,
    show.info = TRUE
)
parallelStop()
# [Tune] Result: max_depth=4; min_child_weight=4.51; subsample=0.975; colsample_bytree=0.728 : wkappa.test.mean=0.5411150
best_parameters <- list(max_depth = 4, min_child_weight = 4.514432, subsample = 0.9749555, colsample_bytree = 0.7277757)

# Prediction of best model

tuned_learner <- setHyperPars(xgboost_classification_learner, par.vals = tuning_result$x)
tuned_xgboost_model <- mlr::train(learner = tuned_learner, task = train_task)

importance_matrix <- getFeatureImportance(tuned_xgboost_model)$res %>%
    t %>%
    data.frame %>%
    rownames_to_column %>%
    setNames(c('Feature', 'Gain'))
feature_importance_plot(importance_matrix)
# importance_matrix <- xgb.importance(colnames(train.x), model = tuned_xgboost_model)

predict(tuned_xgboost_model, newdata = data.frame(test.x)) %>%
    as.data.frame %>%
    add_column(installation_id = test_data$installation_id, .before = 'response') %>%
    save_submission

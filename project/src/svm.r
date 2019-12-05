library(mlr)
library(parallel)
library(parallelMap)
library(tidyverse)

source('metrics.r')
source('plots.r')
source('submission.r')

configure_plot_theme()

# 50 best features selected by optimised XGBoost

best_features <- c(
    'session_title', 'accumulated_accuracy_group', 'accumulated_accuracy', '3afb49e6', 'acc_Bird Measurer (Assessment)',
    '0', '4070', '2000', 'acc_Chest Sorter (Assessment)', 'Clip',
    'duration_mean', '7372e1a5', 'Crystal Caves - Level 3', 'c58186bf', '04df9b66',
    'acc_Mushroom Sorter (Assessment)', '3020', '3121', 'acc_Cauldron Filler (Assessment)', '3110',
    '4035', '4030', '363c86c9', '3ee399c3', '4100',
    '4022', '392e14df', 'accumulated_actions', '0db6d71d', '3120',
    '2030', 'Tree Top City - Level 3', 'acc_Cart Balancer (Assessment)', '562cec5f', '84538528',
    'f6947f54', '907a054b', '3393b68b', '2010', '4020',
    '47026d5f', '4040', '4090', 'a8efe47b', 'a0faea5d',
    'Activity', 'accumulated_uncorrect_attempts', 'a7640a16', 'e5c9df6f', 'Cart Balancer (Assessment)'
)

# Load data

train_data <- data.table::fread('../input/data-science-bowl-2019/summarised_train.csv', stringsAsFactors = F)
test_data <- data.table::fread('../input/data-science-bowl-2019/summarised_test.csv', stringsAsFactors = F)
train.x <- train_data %>% select(-accuracy_group, -installation_id)
train.y <- train_data %>% select(accuracy_group)
test.x <- test_data %>% select(-accuracy_group, -installation_id)

small.train.x <- train.x %>% select(best_features)
small.test.x <- test.x %>% select(best_features)

# Hyperparameter tuning

train_task <- small.train.x %>%
    data.frame %>%
    add_column(target = train.y %>% as.matrix) %>% 
    makeClassifTask(data = ., target = 'target')
parameters <- makeParamSet(
    makeNumericParam('C', lower = 2 ^ -5, upper = 2 ^ 5),
    makeNumericParam('sigma', lower = 0, upper = 1)
)
# stratified_cv_resampling <- makeResampleDesc('CV', stratify = TRUE, iters = 5)
holdout_resampling <- makeResampleDesc('Holdout')
random_search_strategy <- makeTuneControlRandom(maxit = 25)
svm_classification_learner <- makeLearner(
    'classif.ksvm',
    kernel = 'rbfdot'
)

parallelStart(mode = 'multicore', cpus = 4, level = 'mlr.tuneParams')
tuning_result <- tuneParams(
    learner = svm_classification_learner,
    task = train_task,
    control = random_search_strategy,
    # resampling = stratified_cv_resampling,
    resampling = holdout_resampling,
    measures = wkappa,
    par.set = parameters,
    show.info = TRUE
)
parallelStop()
# [Tune] Result: C=17.6; sigma=2.7 : wkappa.test.mean=0.1173067
# [Tune] Result: C=10.9; sigma=0.375 : wkappa.test.mean=0.2975097
# [Tune] Result: C=4.35; sigma=0.0569 : wkappa.test.mean=0.3587876
best_parameters <- list(C = 4.35, sigma = 0.0569)

svm_optimisation_plot(tuning_result)
kappa_through_time(tuning_result)

# Evaluation

tuned_learner <- svm_classification_learner %>% setHyperPars(par.vals = tuning_result$x)
parameters <- makeParamSet(
    makeNumericParam('C', lower = 4.35, upper = 4.35),
    makeNumericParam('sigma', lower = 0.0569, upper = 0.0569)
)
tuning_result <- tuneParams(
    learner = tuned_learner,
    task = train_task,
    resampling = makeResampleDesc('CV', stratify = TRUE, iters = 5),
    control = makeTuneControlRandom(maxit = 1),
    measures = wkappa,
    par.set = parameters,
    show.info = TRUE
)
# [Tune] Result: C=4.35; sigma=0.0569 : wkappa.test.mean=0.3713058

# Prediction of best model

svm_classification_learner %>%
    setHyperPars(par.vals = tuning_result$x) %>% 
    mlr::train(train_task) %>% 
    predict(newdata = data.frame(test.x)) %>%
    as.data.frame %>%
    add_column(installation_id = test_data$installation_id, .before = 'response') %>%
    save_submission

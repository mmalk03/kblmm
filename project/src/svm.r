library(e1071)
library(tidyverse)

source('metrics.r')
source('plots.r')
source('submission.r')

configure_plot_theme()

# 50 best features selected by optimised XGBoost

best_features <- c(
    'session_title', 'accumulated_accuracy_group', 'accumulated_accuracy', 'X3afb49e6', 'acc_Bird.Measurer..Assessment.',
    'X0', 'X4070', 'X2000', 'acc_Chest.Sorter..Assessment.', 'Clip',
    'duration_mean', 'X7372e1a5', 'Crystal.Caves...Level.3', 'c58186bf', 'X04df9b66',
    'acc_Mushroom.Sorter..Assessment.', 'X3020', 'X3121', 'acc_Cauldron.Filler..Assessment.', 'X3110',
    'X4035', 'X4030', 'X363c86c9', 'X3ee399c3', 'X4100',
    'X4022', 'X392e14df', 'accumulated_actions', 'X0db6d71d', 'X3120',
    'X2030', 'Tree.Top.City...Level.3', 'acc_Cart.Balancer..Assessment.', 'X562cec5f', 'X84538528',
    'f6947f54', 'X907a054b', 'X3393b68b', 'X2010', 'X4020',
    'X47026d5f', 'X4040', 'X4090', 'a8efe47b', 'a0faea5d',
    'Activity', 'accumulated_uncorrect_attempts', 'a7640a16', 'e5c9df6f', 'Cart.Balancer..Assessment.'
)

# Load data

train_data <- data.table::fread('../input/data-science-bowl-2019/summarised_train.csv', stringsAsFactors = F)
test_data <- data.table::fread('../input/data-science-bowl-2019/summarised_test.csv', stringsAsFactors = F)
train.x <- train_data %>% select(-accuracy_group, -installation_id)
train.y <- train_data %>% select(accuracy_group)
test.x <- test_data %>% select(-accuracy_group, -installation_id)

small.train.x <- train.x %>% select(best_features)
small.test.x <- test.x %>% select(best_features)

# Train SVM model

svm_model <- svm(
    x = svm.train.x,
    y = train.y,
    type = 'C-classification'
)
y_pred <- predict(svm_model, svm.val.x)
quadratic_kappa(val.y, y_pred)

predict(svm_model, svm.test.x) %>%
    cbind(test_data$installation_id, .) %>%
    save_submission

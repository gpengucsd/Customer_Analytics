## Data Camp: XGBoost in R
churn_data <- readr::read_csv("data/churn_data.csv")

library(caret)
library(xgboost)
library(dplyr)

churn_data <- mutate(
  churn_data,
  month_5_still_here = factor(ifelse(month_5_still_here == 1, "yes", "no"), levels = c("yes", "no"))
)

set.seed(1234)
ind <- createDataPartition(churn_data$month_5_still_here, p = .8)[[1]]

training <- rep(0L, nrow(churn_data))
training[ind] <- 1L

X_train <- churn_data[ind, setdiff(colnames(churn_data), "month_5_still_here")]
y_train <- churn_data$month_5_still_here[ind]
X_test <- churn_data[-ind, setdiff(colnames(churn_data), "month_5_still_here")]
y_test <- churn_data$month_5_still_here[-ind]

## Custom function to evaluate model as ModelMetrics causes an R segfault
## on my mac
## for more on custom evaluation functions
## see http://topepo.github.io/caret/training.html#metrics
auc <- function(data, lev = NULL, model = NULL) {
  c(auc = radiant.model::auc(data$yes, data$obs, "yes"))
}

trControl = trainControl(
  method = 'cv',
  number = 2,
  classProbs = TRUE,
  # summaryFunction = twoClassSummary,
  summaryFunction = auc,
  verboseIter = TRUE
)

tuneGridXGB <- expand.grid(
  nrounds = c(350),
  max_depth = c(4, 6),
  eta = c(0.05, 0.1),
  gamma = c(0.01),
  colsample_bytree = c(0.75),
  subsample = c(0.50),
  min_child_weight = c(0)
)

# train the xgboost learner
xgbmod <- train(
  x = X_train,
  y = y_train,
  method = "xgbTree",
  # metric = "ROC",
  metric = "auc",
  trControl = trControl,
  tuneGrid = tuneGridXGB
)

## without caret
churn_data <- mutate(
  churn_data,
  month_5_still_here = as.integer(month_5_still_here) - 1
)

X_train <- churn_data[ind, setdiff(colnames(churn_data), "month_5_still_here")]
y_train <- churn_data$month_5_still_here[ind]
X_test <- churn_data[-ind, setdiff(colnames(churn_data), "month_5_still_here")]
y_test <- churn_data$month_5_still_here[-ind]

dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)

params <- list(
  objective = "binary:logistic",
  eta = 0.3,
  max_depth = 4
)

xgbcv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,
  nfold = 6,
  print_every_n = 10,
  eval_metric = "auc"
)

head(xgbcv$evaluation_log)
arrange(xgbcv$evaluation_log, desc(test_auc_mean))[1, ]

xgb <- xgb.train(
  params = params,
  data = dtrain,
  nrounds = 75,
  watchlist = list(val = dtest, train = dtrain),
  print_every_n = 10,
  eval_metric = "auc"
)

pred <- data.frame(
  month_5_still_here = y_test,
  pred = predict(xgb, dtest)
)

radiant.model::confusion(
  pred,
  pred = "pred",
  rvar = "month_5_still_here",
  lev = 1
) %>% summary()



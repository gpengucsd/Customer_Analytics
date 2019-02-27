library(ModelMetrics)
library(tidyverse)
library(radiant)
library(zipcode)
library(caret)
library(xgboost)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <- mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag, sincepurch,last,dollars,numords,version1, owntaxprod, upgraded),
                       .funs = as.numeric)%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  mutate(state =as.numeric(as.factor(intuit75k$state)))
train <- intuit75k %>%
  filter(training == 1)
validation  <- intuit75k %>%
  filter(training == 0)

set.seed(2)

X_train <- train[,c(3:12,16)]
y_train <- train$res1
X_test <- validation[,c(3:12,16)]
y_test <- validation$res1

dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
dtrain <- xgb.DMatrix(data =as.matrix(X_train), label = y_train)

params <- list(
  objective = "binary:logistic",
  eta = 0.1,
  max_depth = 10
)

xgbcv <- xgb.cv(
  params = params,
  data = dtrain,
  nrounds = 100,
  nfold = 10,
  print_every_n = 10,
)

head(xgbcv$evaluation_log)


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


validation_new <- data.frame(validation, pred)%>%
  select(id,pred)
intuit75k <- intuit75k %>%
  left_join(validation_new)
intuit75k[is.na(intuit75k)] <- 0
names(intuit75k) [19]="prediction_1"

cost = 1.41
margin = 60
Break_even = cost/margin



perform <- function(key){
  # Lift, Gain, Profit, ROME
  result <- evalbin(
    intuit75k,
    pred = key,
    rvar = "res1",
    lev = "1",
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )
  #plot(result, plots = c("lift", "gains", "profit", "rome"), custom = FALSE)

  # Confusion Matrix and profit&ROME
  cm <- confusion(
    intuit75k,
    pred = key,
    rvar = 'res1',
    lev = '1',
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )

  return(list(result$dataset[,12:13],cm$dataset))
}
perform('prediction_1')

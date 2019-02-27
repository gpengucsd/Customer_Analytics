library(nnet)
library(caret)
library(radiant)

## some formating options
options(
  width = 250,
  scipen = 100,
  max.print = 5000,
  stringsAsFactors = FALSE
)


intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))

cost = 1.41
margin = 60
Break_even = cost/margin


# change data type and add state
intuit75k$zip801 = intuit75k$zip== '00801'
intuit75k$zip804 = intuit75k$zip== '00804'
intuit75k <- mutate_at(intuit75k, .vars = vars(zip_bins, bizflag, version1, owntaxprod,
                                               upgraded), .funs = as.factor)


training_set = intuit75k%>%filter(training == 1)
test_set = intuit75k%>%filter(training != 1)


rvar <- "res1"
evar <- c("zip_bins","sex","bizflag","numords","dollars","last","sincepurch","version1","owntaxprod","upgraded","zip801","zip804")
lev <- "Yes"


result <- nn(training_set, rvar = rvar, evar = evar, lev = lev, size = 2, decay = 0.25, seed = 1234)

summary(result)

test_set$prob_nn <- predict(result, test_set)$Prediction



perform(test_set, "prob_nn")









perform <- function(df, key){
  # Lift, Gain, Profit, ROME
  result <- evalbin(
    df,
    pred = key,
    rvar = "res1",
    lev = "Yes",
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )
  #plot(result, plots = c("lift", "gains", "profit", "rome"), custom = FALSE)

  # Confusion Matrix and profit&ROME
  cm <- confusion(
    df,
    pred = key,
    rvar = 'res1',
    lev = 'Yes',
    cost = cost,
    margin = margin,
    train = 'Test',
    data_filter = 'training == 1'
  )

  return(list(result$dataset[,12:13],cm$dataset))
}



df_train_scaled <- scaledf(training_set, sf = 2)
str(df_train_scaled)

summarize_if(df_train_scaled, is.numeric, funs(mean, sd)) %>%
  gather() %>%
  format_df()


cv.nn(result, K = 5, size = 1:6, decay = seq(0.05, 0.15, 0.05))


size = 2
decay = 0.05


result_2 <- nn(training_set, rvar = rvar, evar = evar, lev = lev, size = 2, decay = 0.05, seed = 1234)

summary(result_2)

test_set$prob_nn_2 <- predict(result_2, test_set)$Prediction

perform(test_set, "prob_nn_2")

## using caret with nnet
## use the big grid ...
# grid <- expand.grid(size = 1:6, decay = seq(0, 1, 0.05))
## ... or the small grid as an example
grid <- expand.grid(size = 3, decay = seq(0.05, 0.15, 0.05))

ctrl <- trainControl(
  method = "cv",
  number = 5,
  classProbs = TRUE,
  summaryFunction = auc,
  verboseIter = TRUE
)


df_train_scaled <- scaledf(training_set, sf = 2)


## this can take quite some time, especially with a big grid and model ...
## comment out to avoid running is not needed
set.seed(1234)
result <- train(
  select(df_train_scaled, -1),
  df_train_scaled[[rvar]],
  method = "nnet",
  trControl = ctrl,
  tuneGrid = grid,
  metric = "auc",
  rang = .1,
  skip = FALSE,
  linout = FALSE,
  trace = FALSE,
  maxit = 10000
)

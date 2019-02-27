library(tidyverse)
library(keras)
library(recipes)
library(ModelMetrics)
library(tidyverse)
library(radiant)
library(zipcode)

pentathlon_nptb <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/pentathlon_nptb.rds"))

train <- pentathlon_nptb %>%
  filter(training== 1)
validation <- pentathlon_nptb %>%
  filter(training == 0 )
test <- pentathlon_nptb %>%
  filter( is.na(training))

dataTransRec <- recipe(total_os+buyer ~ message + age +gender +income+ education +children+ freq_endurance +freq_strength +freq_water + freq_team+ freq_backcountry
            +freq_winter + freq_racquet, data = train) %>%
  step_dummy(message,age,gender,one_hot = T) %>%
  prep(data = train)

trainTransF <- bake(dataTransRec, new_data = train)
testTransF <- bake(dataTransRec, new_data = validation)

Xtrain <- trainTransF %>% select(-total_os)%>%select(-buyer)
Xtest <- testTransF %>% select(-total_os)%>%select(-buyer)

dimX <- dim(Xtrain)[2]

Ytrain <- trainTransF %>% select(total_os,buyer)
Ytest <- testTransF %>% select(total_os,buyer)

#validation test can be skipped
perform <- function(key){
  dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                     total_os= Ytest$total_os)
  validation_new <- data.frame(validation, dlDF)

  intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
  data("zipcode")
  intuit75k <- intuit75k %>%
    left_join(zipcode)
  intuit75k[is.na(intuit75k)] <- 0
  intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
    mutate(state =as.factor(intuit75k$state))%>%
    mutate(res1 =ifelse(res1 == 'No',0,1))%>%
    select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


  intuit75k <- intuit75k %>%
    left_join(validation_new)
  intuit75k[is.na(intuit75k)] <- 0


  cost = 1.41
  margin = 60
  Break_even = cost/margin
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

  return(cm$dataset[14])
}

lista <-c()
set.seed(1234)
for (i in 1:10){for (j in 1:10){
  model <- keras_model_sequential() %>%
    layer_dense(units=i,activation = "relu", input_shape = dimX) %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units=j,activation = "relu") %>%
    layer_dropout(rate = 0.1) %>%
    layer_dense(units=1,activation= 'sigmoid')



  model%>%
    compile(loss='binary_crossentropy',
            optimizer_adam(lr = 1e-04),
            metrics=c('accuracy'))

  historyT <- model %>% fit(
    as.matrix(Xtrain),
    Ytrain$res1,
    epochs = 20,
    batch_size = 64,
    validation_split = 0.2
  )

  historyT <- model %>% fit(
    as.matrix(Xtrain),
    Ytrain$res1,
    epochs = 20,
    batch_size = 64
  )


  ## get test predictions
  profitnn <- perform('predDL')

  lista <- append(lista,profitnn$profit)}}

max <- max(lista)

#best model
set.seed(1235)
model <- keras_model_sequential() %>%
  layer_dense(units=8,activation = "relu", input_shape = dimX) %>%
  layer_dense(units=8,activation = "relu") %>%
  layer_dense(units=1,activation= 'linear')


model%>%
  compile(loss='mean_squared_error',
          optimizer="adam",
          metrics=c('mae'))

historyT <- model %>% fit(
  as.matrix(Xtrain),
  Ytrain$total_os,
  epochs = 20,
  batch_size = 64,
  validation_split = 0.2
)

historyT <- model %>% fit(
  as.matrix(Xtrain),
  Ytrain$res1,
  epochs = 20,
  batch_size = 64
)

dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                   res1= Ytest$res1)
validation_new <- data.frame(validation, dlDF)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


intuit75k <- intuit75k %>%
  left_join(validation_new)
intuit75k[is.na(intuit75k)] <- 0

intuit75k <- data1
cost = 1.41
margin = 60
Break_even = cost/margin
intuit75k_validation <- intuit75k%>%
  filter(training ==0)%>%
  mutate(mailto = predDL/2.2 > Break_even)
mail_to_rate = mean(intuit75k_validation$mailto)
intuit75k_validation <- intuit75k_validation%>%
  filter(mailto == TRUE)
response_rate <- mean(intuit75k_validation$res1)/2
cost1<- 1.41*763334*mail_to_rate
profit1 <- 60*763334*mail_to_rate*response_rate-cost1
ROME <-profit1/cost1

data1<- read_rds('prediction.rds')



######get prediction



validation_new <- data.frame(validation, dlDF)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


intuit75k <- intuit75k %>%
  left_join(validation_new)
intuit75k[is.na(intuit75k)] <- 0


cost = 1.41
margin = 60
Break_even = cost/margin




perform <- function(key){
  dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                     res1= Ytest$res1)
  validation_new <- data.frame(validation, dlDF)

  intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
  data("zipcode")
  intuit75k <- intuit75k %>%
    left_join(zipcode)
  intuit75k[is.na(intuit75k)] <- 0
  intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
    mutate(state =as.factor(intuit75k$state))%>%
    mutate(res1 =ifelse(res1 == 'No',0,1))%>%
    select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)


  intuit75k <- intuit75k %>%
    left_join(validation_new)
  intuit75k[is.na(intuit75k)] <- 0


  cost = 1.41
  margin = 60
  Break_even = cost/margin
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

  return(cm$dataset[14])
}
profitnn<- perform('predDL')

#best model

saveRDS(intuit75k,file ='prediction.rds')


dlDF <- data.frame(predDL=predict(model,as.matrix(Xtest)),
                   res1= Ytest$res1)
validation_new <- data.frame(validation, dlDF)

intuit75k <- readr::read_rds(file.path(radiant.data::find_dropbox(), "MGTA455-2019/data/intuit75k.rds"))
data("zipcode")
intuit75k <- intuit75k %>%
  left_join(zipcode)
intuit75k[is.na(intuit75k)] <- 0
intuit75k <-mutate_at(intuit75k, .vars = vars(zip_bins,sex,bizflag,version1, owntaxprod, upgraded),.funs = as.factor)%>%
  mutate(state =as.factor(intuit75k$state))%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))%>%
  select(id,zip_bins,sex,bizflag,numords,dollars,last,sincepurch,version1,owntaxprod,upgraded,state,res1,training)

test_set <-test_set%>%
  mutate(res1 =ifelse(res1 == 'No',0,1))
intuit75k <- intuit75k %>%
  left_join(test_set)
intuit75k[is.na(intuit75k)] <- 0

for (i in 1:40){
  cost = 1.41
  margin = 60
  Break_even = cost/margin
  intuit75k_validation <- intuit75k%>%
    filter(training ==0)%>%
    mutate(mailto = prob_nnk/(1.8+i*0.01) > Break_even)
  mail_to_rate = mean(intuit75k_validation$mailto)
  intuit75k_validation <- intuit75k_validation%>%
    filter(mailto == TRUE)
  response_rate <- mean(intuit75k_validation$res1)/2
  cost1<- 1.41*763334*mail_to_rate
  profit1 <- 60*763334*mail_to_rate*response_rate-cost1
  print(c(i,profit1))}
#so i = 26 reach maximum 455103.8




